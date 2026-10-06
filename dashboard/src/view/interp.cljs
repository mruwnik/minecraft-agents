(ns view.interp
  "Pose interpolation for the browser view (scene.mjs). Pure: time is passed in, there are no globals.
   Poses arrive a little late and unevenly; the camera plays them back `delay` ms behind the body clock, blended between
   the two poses around that moment. See docs/view-rendering-performance.md.

   Runs every frame, so it is written like engine.path.planner-tuned: the state is the mutable fields of one Interp and
   the poses, the buffer and the windows of skews and gaps are JS arrays and objects, mutated in place; no persistent data,
   no seqs, no keywords in sample. Poses are the JS objects of the /pose stream and a blended pose is a copy of the newer
   one with eye, pos, yaw, pitch and entities replaced.

     const interp = poseInterpolator({ minDelay, maxDelay, maxBuffer, teleport })   // all optional
     interp.push(pose, arrivalMs); interp.sample(nowMs) -> pose or null
     interp.delay(), interp.interval(), interp.offset(atMs?), interp.playhead(nowMs), interp.underruns()")

(set! *warn-on-infer* true)

(def ^:const TWO-PI (* 2 js/Math.PI))
(def ^:const OFFSET-RELAX 0.002) ; ms of offset per ms of browser time: the clock mapping follows skew without stepping
(def ^:const OFFSET-FALL 0.02) ; ms of offset per ms: a lower skew is followed over time, not in one step
(def ^:const JITTER-KEEP 20)
(def ^:const JITTER-QUANTILE 0.9)
(def ^:const DELAY-MARGIN 10)
;; the delay only moves when the target leaves [delay - BAND-BELOW, delay + BAND-ABOVE]: no wobble from the jitter estimate
(def ^:const BAND-BELOW 8)
(def ^:const BAND-ABOVE 20)
(def ^:const GAP-KEEP 60) ; about 6 s of writes: a one-in-ten longer step must still be in the window
(def ^:const MAX-INTERVAL-GAP 500)
(def ^:const DEFAULT-INTERVAL 100)
(def ^:const RISE-PER-SECOND 0.2) ; the delay rises slowly ...
(def ^:const FALL-PER-SECOND 1) ; ... and falls fast, so playback catches up when walking resumes
(def ^:const INTERVAL-QUANTILE 1) ; the longest recent step sets the delay: a body at a write-rate cap steps 1 or 2 ticks apart, at random (gaps over MAX-INTERVAL-GAP are pauses)

(defn clamp ^number [^number v ^number lo ^number hi] (min hi (max lo v)))
(defn lerp ^number [^number a ^number b ^number f] (+ a (* (- b a) f)))

(defn lerp3
  "A copy of b at the blend of a and b's x, y, z."
  [^js a ^js b f]
  (let [o (js/Object.assign #js {} b)]
    (set! (.-x o) (lerp (.-x a) (.-x b) f))
    (set! (.-y o) (lerp (.-y a) (.-y b) f))
    (set! (.-z o) (lerp (.-z a) (.-z b) f))
    o))

(defn dist3 ^number [^js a ^js b] (js/Math.hypot (- (.-x a) (.-x b)) (- (.-y a) (.-y b)) (- (.-z a) (.-z b))))

(defn ascending ^number [^number a ^number b] (- a b))

(defn quantile ^number [^js xs ^number q]
  (let [sorted (.sort (.slice xs) ascending)]
    (aget sorted (min (dec (alength sorted)) (js/Math.floor (* (alength sorted) q))))))

(defn push-window!
  "xs with x appended, keeping the last `keep` of them."
  [^js xs x keep]
  (.push xs x)
  (when (> (alength xs) keep) (.shift xs)))

(defn wrap-angle ^number [^number a] (mod a TWO-PI))

(defn lerp-yaw
  "Along the shortest arc, normalised to [0, 2π)."
  ^number [^number a ^number b ^number f]
  (let [d (- (mod (+ (- b a) js/Math.PI) TWO-PI) js/Math.PI)]
    (wrap-angle (+ a (* d f)))))

(defn blend-entity [^js a ^js b f teleport]
  (if (or (nil? a) (nil? (.-pos b)) (nil? (.-pos a)) (> (dist3 (.-pos a) (.-pos b)) teleport))
    b
    (let [o (js/Object.assign #js {} b)]
      (set! (.-pos o) (lerp3 (.-pos a) (.-pos b) f))
      (when (and (number? (.-yaw a)) (number? (.-yaw b)))
        (set! (.-yaw o) (lerp-yaw (.-yaw a) (.-yaw b) f)))
      o)))

(defn blend-entities
  "b's entities, each blended with the entity of the same id in a; one only in b stays where b has it."
  [^js a ^js b f teleport]
  (let [after (.-entities b)]
    (if (nil? after)
      after
      (let [before (js/Map.)
            earlier (.-entities a)]
        (when (some? earlier)
          (dotimes [i (alength earlier)]
            (let [^js e (aget earlier i)]
              (when (some? (.-id e)) (.set before (.-id e) e)))))
        (.map after (fn [^js e] (blend-entity (.get before (.-id e)) e f teleport)))))))

(defn blend [^js a ^js b f teleport]
  (let [o (js/Object.assign #js {} b)]
    (set! (.-eye o) (lerp3 (.-eye a) (.-eye b) f))
    (set! (.-pos o) (if (and (some? (.-pos a)) (some? (.-pos b))) (lerp3 (.-pos a) (.-pos b) f) (.-pos b)))
    (set! (.-yaw o) (lerp-yaw (.-yaw a) (.-yaw b) f))
    (set! (.-pitch o) (lerp (.-pitch a) (.-pitch b) f))
    (set! (.-entities o) (blend-entities a b f teleport))
    o))

(deftype Interp [^number min-delay ^number max-delay ^number max-buffer ^number teleport
                 ^js buffer ; poses, oldest first
                 ^js skews ; arrival - pose.t of the last arrivals
                 ^js gaps ; pose.t steps of the last arrivals
                 ^:mutable base ; running minimum of skew, relaxing upwards between arrivals; nil before the first pose
                 ^:mutable ^number base-at
                 ^:mutable ^number in-use ; the offset in use at base-at (it falls to `base` at OFFSET-FALL)
                 ^:mutable last-t ; body time last sampled, nil before
                 ^:mutable ^number held ; frames held at the newest pose since the last arrival
                 ^:mutable ^number underruns
                 ^:mutable applied ; the delay in use, nil before the first pose
                 ^:mutable last-now
                 ^:mutable ^boolean offline]
  Object
  (floor-at [_ at] (+ base (* OFFSET-RELAX (max 0 (- at base-at)))))

  (offset [this at]
    (let [at (if (nil? at) base-at at)]
      (if (nil? base) 0 (max (.floor-at this at) (- in-use (* OFFSET-FALL (max 0 (- at base-at))))))))

  (interval [_] (if (pos? (alength gaps)) (quantile gaps INTERVAL-QUANTILE) DEFAULT-INTERVAL))

  ;; never underrun: the interval plus the arrival jitter seen lately, plus a margin
  (target [this]
    (let [jitter (if (pos? (alength skews)) (- (quantile skews JITTER-QUANTILE) (.offset this nil)) 0)]
      (clamp (+ (.interval this) (max 0 jitter) DELAY-MARGIN) min-delay max-delay)))

  (delay [this] (if (nil? applied) (.target this) applied))

  (push [this ^js pose arrival]
    (set! offline (= "offline" (.-status pose)))
    (let [n (alength buffer)
          ^js newest (when (pos? n) (aget buffer (dec n)))
          t (.-t pose)]
      (when (and (some? (.-eye pose)) (or (nil? newest) (> t (.-t newest))))
        (let [gap (if (nil? newest) 0 (- t (.-t newest)))
              skew (- arrival t)
              current (if (nil? base) skew (.offset this arrival))]
          (when (and (pos? gap) (<= gap MAX-INTERVAL-GAP)) (push-window! gaps gap GAP-KEEP))
          (set! base (if (nil? base) skew (min (.floor-at this arrival) skew)))
          (set! base-at arrival)
          (set! in-use current)
          (push-window! skews skew JITTER-KEEP)
          ;; a stand-still hold is a long gap, not an underrun
          (when (and (some? last-t) (some? newest) (>= last-t (.-t newest)) (<= gap MAX-INTERVAL-GAP))
            (set! underruns (+ underruns held)))
          (set! held 0)
          (.push buffer pose)
          (when (> (alength buffer) max-buffer) (.splice buffer 0 (- (alength buffer) max-buffer)))
          (when (nil? applied) (set! applied (.target this)))))))

  (slew [this now]
    (let [goal (.target this)
          seconds (if (nil? last-now) 0 (/ (max 0 (- now last-now)) 1000))
          desired (clamp applied (- goal BAND-BELOW) (+ goal BAND-ABOVE))]
      (set! applied (+ applied (clamp (- desired applied) (- (* FALL-PER-SECOND goal seconds)) (* RISE-PER-SECOND goal seconds))))
      (set! last-now now)))

  (sample [this now]
    (if (zero? (alength buffer))
      nil
      (do
        (.slew this now)
        (let [^js newest (aget buffer (dec (alength buffer)))
              T (- now (.offset this now) applied)]
          (cond
            offline newest
            (>= T (.-t newest)) (do (set! last-t T) (set! held (inc held)) newest)
            (< T (.-t ^js (aget buffer 0))) (do (set! last-t T) (aget buffer 0))
            :else (do (set! last-t T) (.between this T)))))))

  ;; T is inside the buffer: drop the poses before the pair around T and blend that pair
  (between [this T]
    (let [i (loop [k 0] (if (> (.-t ^js (aget buffer (inc k))) T) k (recur (inc k))))]
      (.splice buffer 0 i)
      (let [^js a (aget buffer 0)
            ^js b (aget buffer 1)
            span (- (.-t b) (.-t a))
            iv (.interval this)
            start (if (> span (* 2 iv)) (max (.-t a) (- (.-t b) iv)) (.-t a))]
        (cond
          (> (dist3 (.-eye a) (.-eye b)) teleport) a
          (< T start) a
          :else (blend a b (/ (- T start) (- (.-t b) start)) teleport)))))

  ;; body time being shown at a browser time
  (playhead [this now] (- now (.offset this now) (.delay this))))

(defn option [^js options k fallback]
  (let [v (when (some? options) (unchecked-get options k))]
    (if (nil? v) fallback v)))

(defn pose-interpolator
  "The interpolator as a JS object of methods (see the namespace doc). options: minDelay 50, maxDelay 200, maxBuffer 32, teleport 8."
  ([] (pose-interpolator nil))
  ([^js options]
   (let [s (Interp. (option options "minDelay" 50) (option options "maxDelay" 200) (option options "maxBuffer" 32) (option options "teleport" 8)
                    #js [] #js [] #js [] nil 0 0 nil 0 0 nil nil false)]
     #js {:push (fn [pose arrival] (.push s pose arrival))
          :sample (fn [now] (.sample s now))
          :delay (fn [] (.delay s))
          :interval (fn [] (.interval s))
          :offset (fn [at] (.offset s at))
          :playhead (fn [now] (.playhead s now))
          :underruns (fn [] (.-underruns s))})))
