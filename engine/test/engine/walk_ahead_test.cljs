(ns engine.walk-ahead-test
  "The walk driver's look-ahead (engine.path.walk): the cells a plan's next steps stand on are watched while it is walked,
  a change the planner cares about re-plans at the next step boundary in the same round, a partial plan is re-planned every
  few seconds and kept unless the new one is clearly better, a body stuck behind a mob plans round it."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.path.walk :as walk]
            [engine.planner-fixture :as pf]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]))

;; ---------------------------------------------------------------- pure

(defn step [x y z move] {:x x :y y :z z :h 0 :move move :px (+ x 0.5) :pz (+ z 0.5)})

(deftest the-cells-of-a-straight-step-are-its-two-columns-from-the-floor-to-over-the-head
  (is (= #{[0 63 0] [0 64 0] [0 65 0] [0 66 0] [1 63 0] [1 64 0] [1 65 0] [1 66 0]}
         (set (walk/step-cells (step 0 64 0 :walk) (step 1 64 0 :walk))))))

(deftest the-cells-of-a-diagonal-step-include-both-side-columns
  (is (= #{[0 0] [1 0] [0 1] [1 1]}
         (set (map (fn [[x _ z]] [x z]) (walk/step-cells (step 0 64 0 :walk) (step 1 64 1 :diagonal)))))))

(deftest the-cells-of-a-step-down-span-both-heights
  (is (= [62 67] ((juxt first last) (sort (distinct (map second (walk/step-cells (step 0 65 0 :walk) (step 1 63 0 :drop)))))))))

(deftest the-window-covers-the-next-steps-and-skips-what-the-plan-opens
  (let [steps [(step 0 64 0 :start) (step 1 64 0 :walk) (assoc (step 2 64 0 :walk) :opens [{:x 2 :y 64 :z 0}])
               (step 3 64 0 :walk) (step 4 64 0 :walk)]
        cells (set (walk/window-cells steps 1 2))]
    (is (contains? cells [0 64 0]))
    (is (contains? cells [1 64 0]))
    (is (not (contains? cells [3 64 0])) "two steps from index 1: up to step 2")
    (is (not (contains? cells [2 64 0])) "the door the plan opens")
    (is (not (contains? cells [2 65 0])) "and its other half")))

(def table @pf/table)
(def fixture @pf/fixture)
(defn sid [name props] (.stateId fixture name (clj->js props)))

(deftest a-state-change-the-planner-ignores-is-no-change
  (is (walk/same-for-planner? table (sid "wheat" {:age 0}) (sid "wheat" {:age 7})))
  (is (walk/same-for-planner? table (sid "stone" {}) (sid "stone" {}))))

(deftest a-state-change-the-planner-reads-is-a-change
  (is (not (walk/same-for-planner? table (sid "air" {}) (sid "stone" {}))))
  (is (not (walk/same-for-planner? table (sid "oak_door" {:open true :half "lower" :facing "east"})
                                   (sid "oak_door" {:open false :half "lower" :facing "east"}))))
  (is (not (walk/same-for-planner? table (sid "air" {}) 0xFFFF)) "a cell that went unloaded"))

(def pose-on-ground {:x 1.5 :y 64 :z 0.5 :on-ground true :on-climbable false :in-water false})

(deftest a-step-boundary-is-an-advance-on-the-ground-to-a-plain-step-or-a-check-along-a-leg
  (let [steps [(step 0 64 0 :start) (step 1 64 0 :walk) (step 2 64 0 :walk) (step 3 64 0 :walk)]]
    (is (walk/boundary? steps 1 2 1 pose-on-ground))
    (is (not (walk/boundary? steps 2 2 1 pose-on-ground)) "no advance this tick")
    (is (walk/boundary? steps 2 2 5 pose-on-ground) "mid-way along a plain leg, every check-every ticks")
    (is (not (walk/boundary? steps 1 2 1 (assoc pose-on-ground :on-ground false))) "in the air")
    (is (not (walk/boundary? steps 1 2 1 (assoc pose-on-ground :in-water true))) "swimming")
    (is (not (walk/boundary? steps 1 2 1 (assoc pose-on-ground :on-climbable true))) "on a ladder")))

(deftest no-step-boundary-before-a-gap-a-climb-or-a-swim
  (doseq [move [:gap :climb-up :climb-down :jump-climb :swim :swim-up :swim-down :exit]]
    (let [steps [(step 0 64 0 :start) (step 1 64 0 :walk) (step 2 64 0 move) (step 3 64 0 :walk)]]
      (is (not (walk/boundary? steps 1 2 1 pose-on-ground)) (str move)))))

(deftest a-partial-plan-is-refreshed-after-its-interval-a-whole-one-never
  (is (walk/refresh-due? "partial" 80 80) "ticks walked, interval")
  (is (not (walk/refresh-due? "partial" 79 80)))
  (is (not (walk/refresh-due? "found" 10000 80))))

(deftest the-refresh-interval-grows-with-the-plan-time
  (is (= 80 (walk/refresh-ticks 3)))
  (is (= 200 (walk/refresh-ticks 500)) "a 500 ms plan: 10 s between refreshes (at most 5 % planning)"))

(deftest a-refreshed-plan-replaces-the-old-only-when-clearly-better
  (let [old [(step 0 64 0 :start) (step 10 64 0 :walk)]
        to [40 64 0]]
    (is (walk/take-refresh? old {:status "found" :steps [(step 0 64 0 :start) (step 40 64 0 :walk)]} to))
    (is (walk/take-refresh? old {:status "partial" :steps [(step 0 64 0 :start) (step 12 64 0 :walk)]} to))
    (is (not (walk/take-refresh? old {:status "partial" :steps [(step 0 64 0 :start) (step 11 64 0 :walk)]} to))
        "one block nearer is not worth a switch")
    (is (not (walk/take-refresh? old {:status "partial" :steps [(step 0 64 0 :start) (step 10 64 1 :walk)]} to))
        "an equal end elsewhere")))

;; ---------------------------------------------------------------- the driver over the fake

(defn on-steer
  "Override the fake's steer: f is called with the fake's state, the steer's tick (counted over every steer of the test) and
  the pose before each decide; it may change the world. decide-wrap, when given, maps (decide pose) to what the steer gets."
  ([p f] (on-steer p f nil))
  ([p f decide-wrap]
   (let [ticks (atom 0)
         s (.. p -world -state)]
     (.override (.-world p) "steer"
                (fn [token args impl]
                  (let [decide (.-decide args)
                        wrapped (fn [pose]
                                  (f s (swap! ticks inc) pose)
                                  (let [out (decide pose)]
                                    (if decide-wrap (decide-wrap s pose out) out)))
                        args' (doto (js-obj "timeoutS" (.-timeoutS args))
                                (js/Object.defineProperty "decide" #js {:value wrapped}))]
                    (impl token args'))))
     ticks)))

(defn set-block! [s [x y z] name] (.set (.-blocks s) (str x "," y "," z) name))

(defn ^:async walk-to
  "walk/walk-to! from pos to goal over blocks (range 0); prepare gets the fake primitives before the walk. [result
  announced-replans body-cell]."
  [world goal prepare]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        _ (prepare p)
        out (atom nil)
        announced (atom [])
        parent {:check (constantly true)
                :round (fn ^:async driving-round [c]
                         (reset! out (await (walk/walk-to! c {:to goal :range 0 :weight 1.2 :timeout-s 60
                                                              :announce! (fn [kind data] (swap! announced conj [kind data]))})))
                         :done)}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'driving-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(driving-parent) {})
    (loop [i 0]
      (when (and (< i 200) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [at (.-pos (.self p))]
      [(:result @out) (mapv second (filter #(= :replan (first %)) @announced)) [(.-x at) (.-y at) (.-z at)]])))

(def lane (floor -2 -3 24 3))

(deftest a-block-placed-ahead-mid-walk-replans-at-once-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result replans at] (await (walk-to {:self {:pos {:x 0 :y 64 :z 0}} :blocks lane} [20 64 0]
                                                  (fn [p] (on-steer p (fn [s n _] (when (= n 10) (set-block! s [8 64 0] "stone") (set-block! s [8 65 0] "stone")))))))]
          (is (= :arrived (:status result)))
          (is (= [20 64 0] at))
          (is (= [:changed] (mapv :why replans)) "one replan, for the change")
          (is (number? (:ms (first replans))) "the replan's planner ms"))))))

(deftest a-static-walk-never-replans
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; two equal ways round a pillar: nothing changes, the plan is walked as made
        (let [[result replans] (await (walk-to {:self {:pos {:x 0 :y 64 :z 0}} :blocks (merge lane (box 8 64 0 8 65 0 "stone"))}
                                               [20 64 0] (fn [_])))]
          (is (= :arrived (:status result)))
          (is (= [] replans)))))))

(deftest a-change-beside-the-way-that-the-plan-does-not-stand-on-is-ignored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result replans] (await (walk-to {:self {:pos {:x 0 :y 64 :z 0}} :blocks lane} [20 64 0]
                                               (fn [p] (on-steer p (fn [s n _] (when (= n 10) (set-block! s [10 64 3] "stone")))))))]
          (is (= :arrived (:status result)))
          (is (= [] replans)))))))

;; a floor that ends at x 15 (chunk 0); the goal lies in chunk 2, unloaded until the test lays the rest of the floor
(def near-floor (floor -40 -2 15 2))
(def far-floor (floor 16 -2 45 2))

(deftest a-partial-plan-is-refreshed-and-extends-once-the-land-loads
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result replans at] (await (walk-to {:self {:pos {:x -38 :y 64 :z 0}} :blocks near-floor} [40 64 0]
                                                  (fn [p] (on-steer p (fn [s n _] (when (= n 20) (doseq [[k v] far-floor] (.set (.-blocks s) k v))))))))]
          (is (= :arrived (:status result)))
          (is (= [40 64 0] at))
          (is (= [:refresh] (mapv :why replans)) "the refresh found the whole way before the partial end")
          (is (= [false] (mapv :kept replans))))))))

(deftest a-refresh-with-nothing-new-keeps-the-old-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result replans at] (await (walk-to {:self {:pos {:x -38 :y 64 :z 0}} :blocks near-floor} [40 64 0] (fn [_])))]
          (is (not= :arrived (:status result)))
          (is (<= 14 (first at) 15) "walked to the floor's end")
          (is (seq (filter #(= :refresh (:why %)) replans)))
          (is (every? :kept (filter #(= :refresh (:why %)) replans)) "every refresh kept the plan it had"))))))

;; a ladder up the east face of a 4-high tower at x 5; the walk goes up it and on along the tower top
(def tower
  (merge (floor -2 -2 4 2) (box 5 63 -2 12 67 2 "stone") (box 4 64 0 4 67 0 "ladder")))

(deftest a-change-while-climbing-replans-only-once-the-body-stands-on-top
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [placed (atom false)
              [result replans] (await (walk-to {:self {:pos {:x 0 :y 64 :z 0}} :blocks tower :states {"4,64,0" {:facing "west"} "4,65,0" {:facing "west"} "4,66,0" {:facing "west"} "4,67,0" {:facing "west"}}}
                                               [11 68 0]
                                               (fn [p] (on-steer p (fn [s _ pose]
                                                                     (when (and (not @placed) (.-onClimbable pose) (> (.-y pose) 65))
                                                                       (reset! placed true)
                                                                       (set-block! s [8 68 0] "stone") (set-block! s [8 69 0] "stone")))))))]
          (is @placed "the block was placed while climbing")
          (is (= :arrived (:status result)))
          (is (= [:changed] (mapv :why replans)))
          (is (every? #(>= (second (:at %)) 68) replans) "the replan waited for the top"))))))

(deftest replans-per-walk-are-bounded
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a ceiling at y 66 grows one cell ahead of the body every tick: every boundary sees a change
        (let [[result replans] (await (walk-to {:self {:pos {:x 0 :y 64 :z 0}} :blocks lane} [20 64 0]
                                               (fn [p] (on-steer p (fn [s _ pose] (set-block! s [(+ 2 (js/Math.floor (.-x pose))) 66 0] "stone"))))))]
          (is (= :arrived (:status result)))
          (is (= walk/max-watch-replans (count replans))))))))

;; ---------------------------------------------------------------- go-to

(defn ^:async go!
  "jobs.movement.go-to with args over world, prepare given the fake first; {:out :p :eng :seen}."
  [world args prepare]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        _ (prepare p)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid 'jobs.movement.go-to args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 40) (seq (:list (core/state eng))))
        (await (core/tick! eng))
        (recur (inc i))))
    {:out out :p p :eng eng :seen seen}))

(defn moved [eng] (mapv :data (mem/entries (mem/view (:store eng)) :moved)))
(defn replan-events [seen] (filterv #(= :replan (:kind %)) @seen))

;; a fence across the lane at x 5 with two open gates, at z 0 and z 2
(def two-gates
  {:self {:pos {:x 0 :y 64 :z 0}}
   :blocks (merge lane (box 5 64 -3 5 64 3 "oak_fence") {"5,64,0" "oak_fence_gate" "5,64,2" "oak_fence_gate"})
   :states {"5,64,0" {:open true :facing "east"} "5,64,2" {:open true :facing "east"}}})

(deftest go-to-a-gate-shut-ahead-replans-through-the-other-in-the-same-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng seen]} (await (go! two-gates {:pos [10 64 0] :range 0 :doors :never}
                                                   (fn [p] (on-steer p (fn [s n _] (when (= n 5) (.set (.-states s) "5,64,0" #js {:open false :facing "east"})))))))]
          (is (= {:arrived true} @out))
          (is (= 1 (count (moved eng))) "one round")
          (is (= [:changed] (mapv (comp :why) (replan-events seen))))
          (is (number? (:ms (first (replan-events seen)))))
          (is (= [10 64 0] (let [a (.-pos (.self p))] [(.-x a) (.-y a) (.-z a)]))))))))

(deftest go-to-a-far-goal-extends-as-the-land-loads-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng seen]} (await (go! {:self {:pos {:x -38 :y 64 :z 0}} :blocks near-floor} {:pos [40 64 0] :range 0}
                                                 (fn [p] (on-steer p (fn [s n _] (when (= n 20) (doseq [[k v] far-floor] (.set (.-blocks s) k v))))))))]
          (is (= {:arrived true} @out))
          (is (= ["arrived"] (mapv :status (moved eng))) "one round, no stop at the partial end")
          (is (= [:refresh] (mapv :why (replan-events seen)))))))))

;; two parallel 1-wide corridors (z 0 and z 2) from x 3 to x 12; a cow stands in the z 0 one at x 8
(def corridors
  {:self {:pos {:x 0 :y 64 :z 0}}
   :blocks (merge lane (box 3 64 -1 12 65 -1 "stone") (box 3 64 1 12 65 1 "stone") (box 3 64 3 12 65 3 "stone"))
   :entities [{:id 7 :name "cow" :kind "mob" :pos {:x 8.5 :y 64 :z 0.5}}]})

(defn blocked-by-the-cow
  "A decide wrap: a body within 1.2 blocks of the cow that heads toward it gets no forward control (the cow is in its
  way); walking away is free."
  [s pose out]
  (let [cow (some #(when (= "cow" (.-name %)) %) (.-entities s))
        dx (- (.. cow -pos -x) (.-x pose))
        dz (- (.. cow -pos -z) (.-z pose))
        yaw (.-yaw out)
        toward? (pos? (+ (* dx (- (js/Math.sin yaw))) (* dz (- (js/Math.cos yaw)))))
        near? (< (js/Math.hypot dx dz) 1.2)]
    (if (and near? toward? (.-controls out))
      #js {:controls #js {:forward false} :yaw (.-yaw out) :pitch 0}
      out)))

(deftest go-to-stuck-behind-a-mob-in-a-corridor-plans-round-it-in-the-same-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng seen]} (await (go! corridors {:pos [16 64 0] :range 0}
                                                 (fn [p] (on-steer p (fn [_ _ _]) blocked-by-the-cow))))]
          (is (= {:arrived true} @out))
          (is (= 1 (count (moved eng))) "one round")
          (is (= [:mob] (mapv :why (replan-events seen)))))))))
