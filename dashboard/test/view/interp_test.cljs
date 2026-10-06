(ns view.interp-test
  "Pose interpolation, as scene.mjs calls it: JS poses in, JS poses out (ported from test/view-web-interp.test.mjs)."
  (:require [clojure.test :refer [deftest are is testing]]
            [view.interp :as interp]))

(def T0 1000000)
(def SKEW 5000)
(def TWO-PI (* 2 js/Math.PI))
(def FRAME (/ 1000 60))
;; deterministic arrival jitter in ms (a 50 ms poll gives at most ~50); always contains 0 so the windowed minimum is stable
(def JITTER [0 30 10 50 20 0 40 15 35 5 45 25])
(defn jitter-at [k] (nth JITTER (mod k (count JITTER))))
(defn uniform-jitter [k] (mod (* k 7919) 51)) ; deterministic 0..50

(defn mk
  ([t x] (mk t x {}))
  ([t x extra]
   (clj->js (merge {:t t :status "online" :eye {:x x :y 65.62 :z 0} :pos {:x x :y 64 :z 0} :yaw 1 :pitch 0 :world "w"} extra))))

(defn at-skew [poses] (mapv (fn [p] {:pose p :arrival (+ (.-t p) SKEW)}) poses))

;; a walk along x at 4.3 blocks/s, poses every dt ms, from k = 0 to count - 1
(defn walk
  ([dt n] (walk dt n jitter-at))
  ([dt n jitter]
   (mapv (fn [k] {:pose (mk (+ T0 (* k dt)) (* (/ (* 0.43 dt) 100) k)) :arrival (+ T0 (* k dt) SKEW (jitter k))}) (range n))))

;; feeds arrivals in time order while sampling every frame at `step` ms; `f` sees the interpolator and the frame's now and
;; sample; returns what f returned for frames in [from, to)
(defn trace
  ([interp arrivals from to f] (trace interp arrivals from to FRAME f))
  ([interp arrivals from to step f]
   (loop [queue (sort-by :arrival arrivals) now from out []]
     (if (>= now to)
       out
       (let [[due later] (split-with #(<= (:arrival %) now) queue)]
         (doseq [{:keys [pose arrival]} due] (.push interp pose arrival))
         (let [shown (.sample interp now)]
           (recur later (+ now step) (conj out (f interp now shown)))))))))

(defn run
  ([interp arrivals from to] (run interp arrivals from to FRAME))
  ([interp arrivals from to step] (trace interp arrivals from to step (fn [_ now pose] {:now now :pose pose}))))

(defn eye-x [frame] (.. (:pose frame) -eye -x))
(defn steps [frames] (mapv - (map eye-x (rest frames)) (map eye-x frames)))
(defn mean [xs] (/ (reduce + xs) (count xs)))

(deftest steady-walk-even-steps-and-the-delay-settles
  (are [dt warm delay jitter]
       (let [it (interp/pose-interpolator)
             from (+ T0 SKEW warm)
             frames (filterv #(>= (:now %) from) (run it (walk dt (js/Math.ceil (/ 16000 dt)) jitter) (+ T0 SKEW 200) (+ from 3000)))
             s (steps frames)
             m (mean s)]
         (and (pos? m)
              (every? #(<= (abs (- % m)) (* 0.05 m)) s)
              (<= (abs (- (.delay it) delay)) 20)))
    100 4000 150 jitter-at
    50 9000 75 #(mod (jitter-at %) 20)))

(def slow-then-fast
  (let [fast-start (+ T0 10000)]
    (into (walk 100 100)
          (map (fn [k] {:pose (mk (+ fast-start (* k 50)) (+ (* 0.43 100) (* 0.215 k))) :arrival (+ fast-start (* k 50) SKEW (mod (jitter-at k) 20))}))
          (range 400))))

(deftest rate-change-10-to-20-hz-no-jerk-and-the-delay-slews-down
  (let [frames (run (interp/pose-interpolator) slow-then-fast (+ T0 SKEW 200) (+ T0 SKEW 25000))
        speed (/ 0.43 100)
        from (count (take-while #(< (:now %) (+ T0 SKEW 5500)) frames))
        s (drop from (steps frames))
        delays (trace (interp/pose-interpolator) slow-then-fast (+ T0 SKEW 200) (+ T0 SKEW 25000) (fn [it _ _] (.delay it)))
        slew (+ (/ (* 1.0 200 FRAME) 1000) 1e-6)]
    (is (every? #(<= (abs (- % (* speed FRAME))) (* 0.25 speed FRAME)) s))
    (is (every? #(<= (abs %) slew) (map - (rest delays) delays)))
    (is (< (peek delays) 100))))

(deftest yaw-across-the-wrap-takes-the-short-arc
  (let [poses [(mk T0 0 {:yaw 6.2}) (mk (+ T0 100) 0 {:yaw 0.1}) (mk (+ T0 200) 0 {:yaw 0.1})]
        yaws (map #(.-yaw (:pose %)) (run (interp/pose-interpolator) (at-skew poses) (+ T0 SKEW 150) (+ T0 SKEW 400) 4))]
    (is (every? #(and (>= % 0) (< % TWO-PI)) yaws))
    (is (every? #(or (> % (- 6.2 1e-9)) (< % (+ 0.1 1e-9))) yaws) "inside the short arc")
    (is (some #(or (> % (+ 6.2 0.01)) (< % (- 0.1 0.01))) yaws) "saw intermediate samples")))

(deftest teleport-no-sample-between-the-two-positions
  (let [poses [(mk T0 0) (mk (+ T0 100) 0.43) (mk (+ T0 200) 100.43) (mk (+ T0 300) 100.86)]
        xs (map eye-x (run (interp/pose-interpolator) (at-skew poses) (+ T0 SKEW 100) (+ T0 SKEW 600) 5))]
    (is (every? #(or (<= % (+ 0.43 1e-9)) (>= % (- 100.43 1e-9))) xs))
    (is (some #(>= % (- 100.43 1e-9)) xs))))

(deftest an-underrun-holds-the-last-pose-exactly
  (let [arrivals (walk 100 20)
        last-pose (:pose (peek arrivals))
        frames (run (interp/pose-interpolator) arrivals (+ T0 SKEW 3000) (+ T0 SKEW 3000 5000))
        tail (filter #(> (:now %) (+ T0 SKEW 2100 600)) frames)]
    (is (> (count tail) 100))
    (is (every? #(identical? last-pose (:pose %)) tail))))

(deftest heartbeat-gap-stays-put
  (let [it (interp/pose-interpolator)
        poses [(mk T0 0) (mk (+ T0 2000) 0) (mk (+ T0 2100) 0.43)]
        frames (run it (at-skew poses) (+ T0 SKEW 150) (+ T0 SKEW 2400) 5)
        before (filter #(< (- (:now %) SKEW (.delay it)) (+ T0 2000)) frames)]
    (is (> (count before) 100))
    (is (every? #(zero? (eye-x %)) before))))

(deftest stop-then-move-waits-then-covers-one-step-at-normal-speed
  (let [poses (conj (mapv #(mk (+ T0 (* % 100)) (* 0.43 %)) (range 11)) (mk (+ T0 2000) (* 0.43 11)))
        it (interp/pose-interpolator)
        frames (run it (at-skew poses) (+ T0 SKEW 1000) (+ T0 SKEW 3200) 5)
        body-t #(- (:now %) SKEW (.delay it))
        at-rest (filter #(< (+ T0 1000) (body-t %) (+ T0 1000 700)) frames)
        ramp (filterv #(and (>= (body-t %) (+ T0 1900)) (< (body-t %) (+ T0 2000))) frames)
        slope (/ (- (eye-x (peek ramp)) (eye-x (first ramp))) (- (body-t (peek ramp)) (body-t (first ramp))))]
    (is (> (count at-rest) 20))
    (is (every? #(< (abs (- (eye-x %) 4.3)) 1e-9) at-rest))
    (is (< (abs (- slope 0.0043)) 0.0004) (str "slope " slope))))

(deftest offline-freezes-at-the-newest-pose
  (let [poses [(mk T0 0) (mk (+ T0 100) 0.43) (mk (+ T0 150) 0.9 {:status "offline"})]
        frames (run (interp/pose-interpolator) (at-skew poses) (+ T0 SKEW 160) (+ T0 SKEW 600) 10)]
    (is (every? #(identical? (peek poses) (:pose %)) frames))))

(deftest an-offline-short-record-keeps-the-frozen-eye-pose
  (let [it (interp/pose-interpolator)
        last-pose (mk T0 3)]
    (.push it last-pose (+ T0 SKEW))
    (.push it #js {:t (+ T0 500) :status "offline" :world "w"} (+ T0 500 SKEW))
    (is (identical? last-pose (.sample it (+ T0 SKEW 900))))))

(deftest entities-both-blend-a-only-vanishes-b-only-appears-in-place
  (let [ent (fn [id x] {:id id :pos {:x x :y 64 :z 0} :yaw 0})
        a (mk T0 0 {:entities [(ent 1 0) (ent 2 5)]})
        b (mk (+ T0 100) 0.43 {:entities [(ent 1 1) (ent 3 9)]})
        c (mk (+ T0 200) 0.86 {:entities [(ent 1 2) (ent 3 9)]})
        frames (run (interp/pose-interpolator) (at-skew [a b c]) (+ T0 SKEW 150) (+ T0 SKEW 260) 5)
        during (filter #(< 0.001 (eye-x %) 0.429) frames)
        by-id (fn [frame] (into {} (map (fn [e] [(.-id e) e])) (.-entities (:pose frame))))]
    (is (> (count during) 3))
    (doseq [frame during
            :let [es (by-id frame)
                  e1 (.. (get es 1) -pos -x)]]
      (is (= #{1 3} (set (keys es))))
      (is (= 9 (.. (get es 3) -pos -x)))
      (is (< 0 e1 1))
      (is (< (abs (- e1 (/ (eye-x frame) 0.43))) 1e-9)))))

(deftest a-late-arrival-leaves-the-clock-mapping-alone
  (let [it (interp/pose-interpolator)]
    (doseq [{:keys [pose arrival]} (walk 100 40)] (.push it pose arrival))
    (let [shown (.sample it (+ T0 SKEW 4000))
          before (.offset it)]
      (is (<= (abs (- before SKEW)) 3) (str "offset " before))
      (.push it (mk (+ T0 4000) 17.2) (+ T0 4000 SKEW 300))
      (is (<= (abs (- (.offset it) before)) 1))
      (is (<= (abs (- (.. (.sample it (+ T0 SKEW 4000)) -eye -x) (.. shown -eye -x))) 0.01)))))

(deftest standing-still-then-walking-the-delay-stays-near-one-and-a-half-intervals
  (let [heartbeats (mapv (fn [k] {:pose (mk (+ T0 (* k 2000)) 0) :arrival (+ T0 (* k 2000) SKEW (jitter-at k))}) (range 6))
        H (+ T0 10000)
        stride (mapv (fn [k] (let [t (+ H (* (inc k) 100))] {:pose (mk t (* 0.43 (inc k))) :arrival (+ t SKEW (jitter-at (+ k 3)))})) (range 30))
        arrivals (into heartbeats stride)
        frames (run (interp/pose-interpolator) arrivals (+ T0 SKEW) (+ H SKEW 3000))
        delays (trace (interp/pose-interpolator) arrivals (+ T0 SKEW) (+ H SKEW 1000) (fn [it now _] [now (.delay it)]))
        delay-after-five (second (or (first (filter #(> (first %) (:arrival (nth stride 4))) delays)) (peek delays)))
        first-shown (first (filter #(and (> (:now %) (+ H SKEW)) (> (eye-x %) 1e-6)) frames))]
    (is (<= (abs (- delay-after-five 150)) 25) (str "delay " delay-after-five))
    (is (<= (:now first-shown) (+ (:arrival (first stride)) 200 FRAME)))))

(deftest slow-irregular-gaps-keep-the-delay-at-or-below-200
  (let [gaps [300 450 350 400 320 440]
        ts (reductions + T0 (map #(nth gaps (mod % (count gaps))) (range 39)))
        poses (map-indexed (fn [k t] {:pose (mk t (* 0.1 k)) :arrival (+ t SKEW (jitter-at k))}) ts)
        delays (trace (interp/pose-interpolator) poses (+ T0 SKEW) (+ T0 SKEW 14000) (fn [it _ _] (.delay it)))]
    (is (every? #(<= % 200) delays))))

(deftest at-20-hz-the-delay-is-near-75-within-2-s
  (let [it (interp/pose-interpolator)]
    (run it (walk 50 80 #(mod (jitter-at %) 20)) (+ T0 SKEW) (+ T0 SKEW 2000))
    (is (<= (abs (- (.delay it) 75)) 15) (str "delay " (.delay it)))))

(deftest a-server-hop-snaps-when-far-and-glides-when-near
  (are [jump snaps?]
       (let [poses [(mk T0 0) (mk (+ T0 100) 0.1) (mk (+ T0 200) (+ 0.1 jump)) (mk (+ T0 300) (+ 0.2 jump))]
             frames (run (interp/pose-interpolator) (at-skew poses) (+ T0 SKEW 100) (+ T0 SKEW 600) 5)]
         (= (not snaps?) (boolean (some #(< (+ 0.1 1e-9) (eye-x %) (- (+ 0.1 jump) 1e-9)) frames))))
    8.5 true
    12 true
    50 true
    5 false
    7 false))

;; per frame: the playhead, delay and underrun count after the sample
(defn playback [it arrivals from to]
  (trace it arrivals from to (fn [it now _] {:now now :playhead (.playhead it now) :delay (.delay it) :underruns (.underruns it)})))

(deftest the-playhead-advances-one-frame-per-frame
  (let [frames (filterv #(> (:now %) (+ T0 SKEW 5000)) (playback (interp/pose-interpolator) (walk 100 220 uniform-jitter) (+ T0 SKEW) (+ T0 SKEW 20000)))
        advances (map - (map :playhead (rest frames)) (map :playhead frames))]
    (is (every? #(<= (abs (- % FRAME)) 0.5) advances))))

(deftest a-late-arrival-never-moves-the-playhead-backwards
  (let [arrivals (map-indexed (fn [k a] (if (= k 120) (update a :arrival + 300) a)) (walk 100 220 uniform-jitter))
        frames (filterv #(> (:now %) (+ T0 SKEW 5000)) (playback (interp/pose-interpolator) arrivals (+ T0 SKEW) (+ T0 SKEW 20000)))]
    (is (every? true? (map #(>= (:playhead %2) (:playhead %1)) frames (rest frames))))))

(deftest delay-targets
  (let [after-warmup (fn [dt jitter] (filterv #(> (:now %) (+ T0 SKEW 8000))
                                              (playback (interp/pose-interpolator) (walk dt (js/Math.ceil (/ 30000 dt)) jitter) (+ T0 SKEW) (+ T0 SKEW 25000))))]
    (testing "20 Hz, 0-50 ms jitter: no underruns after warm-up"
      (let [frames (after-warmup 50 uniform-jitter)]
        (is (= 0 (- (:underruns (peek frames)) (:underruns (first frames)))))))
    (testing "10 Hz, 0-20 ms jitter: delay about 130 or less"
      (is (<= (:delay (peek (after-warmup 100 #(mod (uniform-jitter %) 21)))) 135)))))

(deftest holding-at-the-newest-pose-counts-as-an-underrun
  (let [it (interp/pose-interpolator)]
    (playback it (walk 100 10 (constantly 0)) (+ T0 SKEW) (+ T0 SKEW 1800))
    (.push it (mk (+ T0 1100) 5) (+ T0 1100 SKEW))
    (is (pos? (.underruns it)))))

(deftest options-are-read-from-a-js-object
  (let [it (interp/pose-interpolator #js {:minDelay 120 :maxDelay 130})]
    (.push it (mk T0 0) (+ T0 SKEW))
    (is (<= 120 (.delay it) 130))))

;; the body writes a pose when a 50 ms physics tick finds 100 ms passed since the last one: gaps of 100 or 150 ms
(defn capped-walk [n]
  (let [gaps [100 100 150 100 150 100 100 100 150 100 100 150]
        ts (reductions + T0 (map #(nth gaps (mod % (count gaps))) (range n)))]
    (vec (map-indexed (fn [k t] {:pose (mk t (* 0.43 (/ (- t T0) 100)))
                                 :arrival (+ t SKEW (mod (* k 7919) 31))}) ts))))

(deftest a-10-hz-walk-with-100-or-150-ms-gaps-does-not-underrun
  (let [frames (filterv #(> (:now %) (+ T0 SKEW 8000)) (playback (interp/pose-interpolator) (capped-walk 300) (+ T0 SKEW) (+ T0 SKEW 28000)))]
    (is (= 0 (- (:underruns (peek frames)) (:underruns (first frames)))))
    (is (<= (:delay (peek frames)) 180))))
