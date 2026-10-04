(ns engine.path.executor
  "The decision half of a plan executor, pure. A body follows a path found by engine.path.planner-tuned:
  every physics tick the JS side reports a pose and applies the controls returned here. Nothing in this
  namespace touches the body, a clock or a socket; steps-of is the one place a JS object is read.

  Step:  {:x :y :z :h :move kw :corner bool :px :pz}, plus :cx :cz (crossing point on the boundary the
         move came in by), :swim true, :opens [...] and :free [fx fz] (corner slides) when present.
         h is the stand height above the cell floor in 1/16 block; px/pz is the point to stand at.
  Pose:  {:x :y :z :vy :on-ground :on-climbable :in-water :collided}, feet position.
  State: {:steps :i :since :tick :yaw}; i is the index of the step walked to, since the tick at which it
         became current, tick the number of calls, yaw the last yaw sent while moving.
  Done:  {:status :arrived :at} | {:status :off-plan :at :step} | {:status :stuck :at :step :move :target :why}.")

(def policy
  "Every number the executor uses."
  {:max-replans 5          ; re-plans before giving up
   :no-progress-ticks 60   ; 3 s on one step without reaching it -> stuck
   :arrive-xz 0.35         ; final step: horizontal distance to px/pz
   :arrive-y 0.5           ; |feet y - stand-y| that counts as at a step's height
   :off-plan-xz 1.5        ; horizontal distance from the current leg that counts as off the plan
   :off-plan-below 1.5     ; feet this far below the leg's lower end: fell off
   :off-plan-above 2.5     ; feet this far above the leg's higher end: moved off (teleport)
   :lookahead 3            ; later steps checked for an overshoot
   :jump-xz 1.3            ; a rise is jumped once within this of the aim point
   :rise 0.6               ; a rise above this (vanilla step height) needs a jump
   :climb-over 0.2         ; keep climbing until feet are this far above a climb step's stand-y
   :crossing-xz 0.3        ; steer at a crossing point until this close to it
   :still-xz 0.1           ; closer than this to the aim: no forward, keep the yaw
   :gap-jump {1 {:from 0.2 :sprint false}  ; by gap width: jump once the feet are within :from of the takeoff
              2 {:from 0.4 :sprint true}   ; edge; sprint for the run and the flight
              3 {:from 0.1 :sprint true}}
   :gap-jump-down {2 {:from 0.0 :sprint false}}  ; one block down, by width, where it differs (live: a sprint
                                                  ; jump over 2 overshot a 1x1 landing 3 of 5 times)
   :gap-past 0.3           ; feet up to this far past the takeoff edge are still held by it (half the body's width)
   :gap-headroom 3         ; free blocks over the takeoff's stand height needed over takeoff and gap cells
   :sprint true})

(def move-names
  [:start :walk :diagonal :jump :drop :gap :corner :climb-up :climb-down :jump-climb :open :swim :swim-up
   :swim-down :exit])

(def supported-moves
  #{:start :walk :diagonal :corner :jump :drop :gap :climb-up :climb-down :jump-climb})

(def sprint-moves #{:walk :diagonal})

;; ---------------------------------------------------------------- steps

(defn step-of
  "One planner step (a JS object) as a map; optional fields only when present."
  [s]
  (cond-> {:x (.-x s) :y (.-y s) :z (.-z s) :h (.-h s) :move (nth move-names (.-move s))
           :corner (boolean (.-corner s)) :px (.-px s) :pz (.-pz s)}
    (some? (.-cx s)) (assoc :cx (.-cx s) :cz (.-cz s))
    (.-swim s) (assoc :swim true)
    (some? (.-opens s)) (assoc :opens (vec (js->clj (.-opens s) :keywordize-keys true)))))

(defn steps-of
  "A JS array of planner steps as a vector of step maps."
  [js-steps]
  (mapv step-of (array-seq js-steps)))

(defn stand-y [{:keys [y h]}] (+ y (/ h 16)))

(defn unsupported-kind
  "The kind of a step the executor cannot walk, or nil."
  [{:keys [move opens swim]}]
  (cond
    (not (contains? supported-moves move)) move
    (some? opens) :open
    swim :swim))

(def takeoff-blockers #{:climb-up :climb-down :jump-climb})

(defn gap-cells
  "[n dx dz] of a gap: the empty cells between takeoff and landing, and the step's direction; n is nil
  unless takeoff and landing lie on one cardinal line."
  [prev step]
  (let [dx (- (:x step) (:x prev)) dz (- (:z step) (:z prev))]
    [(when (= 1 (count (filter zero? [dx dz]))) (dec (+ (Math/abs dx) (Math/abs dz)))) dx dz]))

(defn gap-refused
  "The refusal for a :gap step walked from prev, or nil."
  [policy prev {:keys [x y z] :as step}]
  (let [at [x y z]
        refuse (fn [kind reason] {:status :refused :kind kind :at at :reason reason})
        [n] (gap-cells prev step)]
    (cond
      (contains? takeoff-blockers (:move prev))
      (refuse :gap-takeoff (str "gap jump at " (pr-str at) " from a ladder"))

      (nil? n) (refuse :gap-width (str "gap jump at " (pr-str at) " not in a straight line"))

      (not (contains? (:gap-jump policy) n))
      (refuse :gap-width (str "gap jump at " (pr-str at) " over " n " cells"))

      (> (stand-y step) (stand-y prev))
      (refuse :gap-up (str "gap jump up at " (pr-str at) " (not measured yet)"))

      (:low-ceiling step)
      (refuse :gap-low-ceiling (str "gap jump at " (pr-str at) " under a ceiling lower than "
                                    (:gap-headroom policy) " blocks")))))

(defn step-refusal
  "The refusal for step s (after prev), or nil: an unsupported step kind, else a :gap step that cannot
  be jumped from prev."
  [policy prev s]
  (let [{:keys [x y z]} s]
    (or (when-let [kind (unsupported-kind s)]
          {:status :refused :kind kind :at [x y z]
           :reason (str "unsupported step kind " kind " at " (pr-str [x y z]))})
        (when (and (= :gap (:move s)) (some? prev))
          (gap-refused policy prev s)))))

(defn refusal
  "nil when every step can be walked, else the refusal for the first one that cannot."
  [policy steps]
  (->> steps
       (map-indexed (fn [i s] (step-refusal policy (get steps (dec i)) s)))
       (some identity)))

(defn with-gap-ceilings
  "Add :low-ceiling to each gap step whose takeoff or gap cells have a solid block within :gap-headroom
  of the takeoff's stand height. solid? is a fn [x y z] -> bool."
  [policy steps solid?]
  (vec (map-indexed
        (fn [i s]
          (if-not (and (pos? i) (= :gap (:move s)))
            s
            (let [prev (nth steps (dec i))
                  [n dx dz] (gap-cells prev s)
                  cells (map (fn [k] [(+ (:x prev) (* k (Math/sign dx))) (+ (:z prev) (* k (Math/sign dz)))])
                             (range 0 (inc (or n 0))))
                  top (dec (Math/ceil (+ (stand-y prev) (:gap-headroom policy))))]
              (if (some (fn [[x z]] (some #(solid? x % z) (range (+ (:y prev) 2) (inc top)))) cells)
                (assoc s :low-ceiling true)
                s))))
        steps)))

;; ---------------------------------------------------------------- corner slides

(defn free-side
  "For a corner step, [fx fz] of the side cell with no solid block (feet and head height), nil when both
  or neither side is blocked. solid? is a fn [x y z] -> bool."
  [prev step solid?]
  (let [y (max (:y prev) (:y step))
        blocked? (fn [[x z]] (or (solid? x y z) (solid? x (inc y) z)))
        sides [[(:x step) (:z prev)] [(:x prev) (:z step)]]
        free (remove blocked? sides)]
    (when (= 1 (count free))
      (first free))))

(defn with-free-sides
  "Add :free to each corner step, computed against the step before it."
  [steps solid?]
  (vec (map-indexed
        (fn [i s]
          (if (and (pos? i) (:corner s))
            (if-let [f (free-side (nth steps (dec i)) s solid?)] (assoc s :free f) s)
            s))
        steps)))

;; ---------------------------------------------------------------- geometry

(defn dist-xz [x z ax az]
  (Math/hypot (- ax x) (- az z)))

(defn dist-to-segment
  "Horizontal distance from (x,z) to the segment (x1,z1)-(x2,z2)."
  [x z x1 z1 x2 z2]
  (let [dx (- x2 x1) dz (- z2 z1)
        len2 (+ (* dx dx) (* dz dz))
        t (if (zero? len2) 0 (-> (/ (+ (* (- x x1) dx) (* (- z z1) dz)) len2) (max 0) (min 1)))]
    (dist-xz x z (+ x1 (* t dx)) (+ z1 (* t dz)))))

(defn in-cell? [step {:keys [x z]}]
  (and (= (:x step) (Math/floor x)) (= (:z step) (Math/floor z))))

(defn past-edge
  "Signed distance of the feet past the takeoff cell's edge that faces the gap step's landing: -0.5 at the
  takeoff centre, 0 at the edge."
  [prev step {:keys [x z]}]
  (let [dx (Math/sign (- (:x step) (:x prev))) dz (Math/sign (- (:z step) (:z prev)))]
    (- (+ (* (- x (+ (:x prev) 0.5)) dx) (* (- z (+ (:z prev) 0.5)) dz)) 0.5)))

;; ---------------------------------------------------------------- reached

(defn reached?
  "The body is in the step's cell and at its height (climbs: at least that high / at most that high)."
  [policy step {:keys [y] :as pose}]
  (let [sy (stand-y step)]
    (and (in-cell? step pose)
         (or (not= :gap (:move step)) (:on-ground pose))
         (case (:move step)
           (:climb-up :jump-climb) (>= y (- sy 0.1))
           :climb-down (<= y (+ sy (:arrive-y policy)))
           (<= (Math/abs (- y sy)) (:arrive-y policy))))))

(defn advance
  "The index to walk to after this pose: past the last reached of the current step and the lookahead,
  never beyond the last step. In the air over a gap nothing is skipped."
  [policy {:keys [steps i]} {:keys [on-ground] :as pose}]
  (let [last-i (dec (count steps))
        hi (min last-i (+ i (:lookahead policy)))
        hit (last (filter #(reached? policy (nth steps %) pose) (range i (inc hi))))]
    (cond
      (and (= :gap (:move (nth steps i))) (not on-ground)) i
      (nil? hit) i
      :else (min last-i (inc hit)))))

;; ---------------------------------------------------------------- aim

(defn aim-point
  "Where to steer on this step: the midpoint towards a corner's free cell, the crossing point, or the
  stand point."
  [policy {:keys [px pz cx cz free] :as step} {:keys [x z] :as pose}]
  (let [[fx fz] free
        in-free? (and free (= fx (Math/floor x)) (= fz (Math/floor z)))
        in-step? (in-cell? step pose)]
    (cond
      (and free (not in-free?) (not in-step?)) [(/ (+ fx 0.5 px) 2) (/ (+ fz 0.5 pz) 2)]
      (and (some? cx) (not in-step?) (> (dist-xz x z cx cz) (:crossing-xz policy))) [cx cz]
      :else [px pz])))

;; ---------------------------------------------------------------- tick

(defn off-plan?
  "Too far from the leg (previous step's point to the aim), or too far below or above it."
  [policy prev step [ax az] {:keys [x y z]}]
  (let [y1 (stand-y prev) y2 (stand-y step)]
    (or (> (dist-to-segment x z (:px prev) (:pz prev) ax az) (:off-plan-xz policy))
        (< y (- (min y1 y2) (:off-plan-below policy)))
        (> y (+ (max y1 y2) (:off-plan-above policy))))))

(defn stuck-why [{:keys [i tick since steps]}]
  (let [s (nth steps i)]
    (str "no progress on step " i " (" (pr-str (:move s)) " to " (pr-str [(:x s) (:y s) (:z s)]) ") for "
         (.toFixed (/ (- tick since) 20) 1) " s")))

(defn jump? [policy {:keys [move] :as step} {:keys [y on-ground on-climbable]} dist]
  (let [sy (stand-y step)]
    (case move
      (:climb-up :jump-climb) (< y (+ sy (:climb-over policy)))
      :climb-down false
      (boolean (and (> (- sy y) (:rise policy))
                    (<= dist (:jump-xz policy))
                    (or on-ground on-climbable))))))

(defn gap-rule
  "The :gap-jump entry for the gap step i of steps; for a gap down, its :gap-jump-down entry when there is one."
  [policy steps i]
  (let [prev (nth steps (dec i))
        step (nth steps i)
        [n] (gap-cells prev step)]
    (or (when (< (stand-y step) (stand-y prev)) (get (:gap-jump-down policy) n))
        (get (:gap-jump policy) n))))

(defn gap-jump?
  "On the takeoff cell, close enough to its edge: jump. Otherwise the plain rule (a body that fell into a
  dip jumps up to the landing)."
  [policy steps i {:keys [y on-ground] :as pose} dist]
  (let [prev (nth steps (dec i))
        step (nth steps i)
        past (past-edge prev step pose)]
    (or (boolean (and on-ground
                      (<= (Math/abs (- y (stand-y prev))) (:arrive-y policy))
                      (<= (- (:from (gap-rule policy steps i))) past)
                      (< past (:gap-past policy))))
        (jump? policy step pose dist))))

(defn sprint? [policy steps i {:keys [on-ground]}]
  (let [window (take 3 (drop i steps))]
    (boolean (if (= :gap (:move (first window)))
               (and (:sprint policy) (:sprint (gap-rule policy steps i)))
               (and (:sprint policy)
                    on-ground
                    (= 3 (count window))
                    (every? #(contains? sprint-moves (:move %)) window)
                    (not-any? #(some? (:cx %)) window))))))

(defn yaw-to
  "Mineflayer's yaw: 0 faces -z, pi/2 faces -x."
  [x z ax az]
  (Math/atan2 (- (- ax x)) (- (- az z))))

(defn controls-for [policy {:keys [steps i yaw] :as state} {:keys [x z] :as pose}]
  (let [step (nth steps i)
        [ax az :as aim] (aim-point policy step pose)
        dist (dist-xz x z ax az)
        moving? (>= dist (:still-xz policy))
        yaw' (if moving? (yaw-to x z ax az) (or yaw 0))]
    {:state (cond-> state moving? (assoc :yaw yaw'))
     :controls {:forward moving? :back false :left false :right false
                :jump (if (= :gap (:move step))
                         (gap-jump? policy steps i pose dist)
                         (jump? policy step pose dist))
                :sneak false
                :sprint (sprint? policy steps i pose)}
     :yaw yaw' :pitch 0}))

(defn start
  "The state for walking steps; step 0 is where the plan starts."
  [steps _now-tick]
  {:steps steps :i (min 1 (max 0 (dec (count steps)))) :since 0 :tick 0 :yaw nil})

(defn arrived? [policy steps i {:keys [x z on-ground on-climbable] :as pose}]
  (let [final (nth steps i)]
    (and (= i (dec (count steps)))
         (reached? policy final pose)
         (<= (dist-xz x z (:px final) (:pz final)) (:arrive-xz policy))
         (boolean (or on-ground on-climbable)))))

(defn tick
  "One physics tick: the controls for this pose, or :done."
  [policy state {:keys [x y z] :as pose}]
  (let [n (inc (:tick state))
        i (advance policy state pose)
        state' (cond-> (assoc state :tick n)
                 (not= i (:i state)) (assoc :i i :since n))
        {:keys [steps since]} state'
        at [x y z]
        step (nth steps i)
        aim (aim-point policy step pose)]
    (cond
      (arrived? policy steps i pose)
      {:state state' :done {:status :arrived :at at}}

      (and (pos? i) (off-plan? policy (nth steps (dec i)) step aim pose))
      {:state state' :done {:status :off-plan :at at :step i}}

      (> (- n since) (:no-progress-ticks policy))
      {:state state'
       :done {:status :stuck :at at :step i :move (:move step) :target [(:x step) (:y step) (:z step)]
              :why (stuck-why state')}}

      :else (controls-for policy state' pose))))

;; ---------------------------------------------------------------- re-plan bookkeeping

(defn after-walk
  "What to do with a finished walk, given the re-plans so far: {:finish result} or {:replan n}."
  [policy replans {:keys [status at] :as done}]
  (case status
    :arrived {:finish {:status :arrived :at at :replans replans}}
    :off-plan (if (< replans (:max-replans policy))
                {:replan (inc replans)}
                {:finish {:status :gave-up :reason :replan-limit :replans replans :at at}})
    :stuck {:finish (assoc done :replans replans)}))
