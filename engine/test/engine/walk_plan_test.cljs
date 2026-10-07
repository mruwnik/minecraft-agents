(ns engine.walk-plan-test
  "jobs.debug.walk-plan against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.path.executor :as executor]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.walk.plan :as wplan]))

(def job 'jobs.debug.walk-plan)

(defn floor
  "Stone at y 63 for x in xs, z 0..2."
  [xs]
  (into {} (for [x xs z (range 3)] [(str x "," 63 "," z) "stone"])))

(defn setup
  "An engine over the fake world, the body at pos; a recording parent runs the job as its child and puts the child's
  result in :out when it finishes. prep is called with the primitives before the first tick."
  [blocks args prep pos]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {:blocks blocks :self {:pos pos}})
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (prep p)
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :seen seen :out out}))

(defn ^:async walk
  "Setup (the body at pos, default (0 64 1)), tick until the job is gone, return the setup map."
  [blocks args prep & [pos]]
  (let [{:keys [eng clock] :as s} (setup blocks args prep (or pos {:x 0 :y 64 :z 1}))]
    (loop [i 0]
      (when (and (< i 200) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    s))

(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))

(defn count-of [s kind] (count (events-of s kind)))

(def args {:to [10 64 1]})

(deftest a-flat-floor-is-walked-to-the-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (walk (floor (range 12)) args (fn [_]))) ]
          (is (= :arrived (:status @out)))
          (is (= 0 (:replans @out)))
          (let [ev (first (events-of s :walk-plan.result))]
            (is (pos? (:walked ev)))
            (is (pos? (:walk-ms ev)))
            (is (pos? (:blocks-per-s ev))))
          (is (= 1 (count-of s :walk-plan.plan)))
          (is (= 1 (count-of s :walk-plan.result))))))))

(defn box
  "Blocks named name filling x0..x1, y0..y1, z0..z1."
  [x0 y0 z0 x1 y1 z1 name]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(def ceiling (box 4 66 0 6 66 2 "stone"))
(def ledge (box 7 64 0 11 64 2 "stone"))

(defn ^:async no-path-within-abilities
  "Walk to over blocks: no path within the executor's abilities, kind named in the result and its event, no move."
  [blocks to kind]
  (let [{:keys [out p] :as s} (await (walk blocks {:to to} (fn [_])))
        ev (first (events-of s :walk-plan.result))
        pos (.-pos (.self p))]
    (is (= {:status :no-path :reason :abilities :kind kind} (select-keys @out [:status :reason :kind])))
    (is (= kind (:refused-kind ev)))
    (is (= [0 64 1] [(.-x pos) (.-y pos) (.-z pos)]) "the body did not move")
    (is (not (contains? ev :blocks-per-s)))))

(deftest a-gap-jump-under-a-low-ceiling-only-is-no-path-within-abilities
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (no-path-within-abilities (merge (floor (range 5)) (floor (range 7 12)) ceiling) [10 64 1] :gap-low-ceiling))))))

(deftest a-gap-jump-up-only-is-no-path-within-abilities
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (no-path-within-abilities (merge (floor (range 5)) ledge) [10 65 1] :gap-up))))))

(deftest a-low-ceiling-gap-with-a-way-round-walks-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the wall at the trench's end keeps the walk round off the trench's corner cells (the fake's walker is a point
        ;; that cannot cross a hole's corner the way a body does)
        (let [blocks (merge (box 0 63 -8 4 63 10 "stone") (box 7 63 -8 12 63 10 "stone") (box 5 63 9 6 63 10 "stone")
                            (box 4 66 -8 6 66 8 "stone") (box 5 64 8 6 65 8 "stone"))
              {:keys [out] :as s} (await (walk blocks {:to [10 64 1]} (fn [_])))]
          (is (= :arrived (:status @out)))
          (is (= 0 (count-of s :walk-plan.replan))))))))

(deftest the-executor-refusal-stays-a-backstop-and-names-its-kind
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a policy that cannot swim: the planner is told, but a start in water is no move it makes
        (let [policy executor/policy
              _ (set! executor/policy (update policy :moves disj :swim :swim-up :swim-down :exit))
              {:keys [out] :as s} (await (walk (merge (floor (range 12)) {"0,64,1" "water"}) args (fn [_])))]
          (set! executor/policy policy)
          (is (= {:status :refused :kind :swim} (select-keys @out [:status :kind])))
          (is (= :swim (:refused-kind (first (events-of s :walk-plan.result))))))))))

;; water x 5..8 at y 64 over stone at 63, floors at y 64 on both sides (feet 65): the only way is through the water
(def pond (merge (box 0 63 0 12 63 2 "stone") (box 0 64 0 4 64 2 "stone") (box 9 64 0 12 64 2 "stone")
                 (box 5 64 0 8 64 2 "water")))

(deftest a-pond-is-crossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (walk pond {:to [11 65 1]} (fn [_]) {:x 0 :y 65 :z 1}))
              plan (first (events-of s :walk-plan.plan))]
          (is (= :arrived (:status @out)))
          (is (= 0 (:replans @out)))
          (is (re-find #"(?i)water|swim" (pr-str (:summary plan)))))))))

(deftest a-body-in-the-water-swims-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (walk pond {:to [11 65 1]} (fn [_]) {:x 6 :y 64 :z 1}))]
          (is (= :arrived (:status @out)))
          (is (= 0 (:replans @out))))))))

;; the same pond 2 deep (y 63..64 over stone at 62), its far bank one block over the water (feet 66)
(def high-bank (merge (box 0 62 0 12 62 2 "stone") (box 0 63 0 4 64 2 "stone") (box 9 63 0 12 65 2 "stone")
                      (box 5 63 0 8 64 2 "water")))

(deftest a-partial-plan-ends-on-its-last-dry-step
  (let [s (fn [x swim?] (cond-> {:x x :move :walk} swim? (assoc :swim true)))]
    (are [steps kept] (= kept (mapv :x (wplan/dry-end steps)))
      [(s 0 false) (s 1 false) (s 2 true) (s 3 true)] [0 1]
      [(s 0 false) (s 1 true) (s 2 false) (s 3 true)] [0 1 2]
      [(s 0 true) (s 1 true)] []
      [(s 0 false) (s 1 false)] [0 1])))

(deftest a-bank-too-high-to-leave-is-no-path-and-the-body-stays-dry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (walk high-bank {:to [11 66 1]} (fn [_]) {:x 0 :y 65 :z 1}))
              pos (.-pos (.self p))]
          (is (= :no-path (:status @out)))
          (is (not= :abilities (:reason @out)))
          (is (<= (.-x pos) 4) "the body did not go into the water"))))))

(deftest a-goal-sealed-in-stone-is-no-path
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [[x z] [[9 1] [11 1] [10 0] [10 2]] y [64 65]] [(str x "," y "," z) "stone"]))
              roof {"10,66,1" "stone"}
              {:keys [out] :as s} (await (walk (merge (floor (range 12)) wall roof) args (fn [_])))]
          (is (= :no-path (:status @out)))
          (is (not (contains? (first (events-of s :walk-plan.result)) :blocks-per-s))))))))

(deftest a-body-pushed-off-the-plan-plans-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shifted (atom false)
              prep (fn [p]
                     (.override (.-world p) "steer"
                                (fn ^:async f [token a impl]
                                  (let [decide (.-decide a)
                                        n (atom 0)
                                        first? (not @shifted)]
                                    (reset! shifted true)
                                    (await (impl token
                                                 #js {:timeoutS (.-timeoutS a)
                                                      :decide (fn [pose]
                                                                (swap! n inc)
                                                                (when (and first? (> @n 10)) (set! (.-z pose) (+ (.-z pose) 3)))
                                                                (decide pose))}))))))
              {:keys [out] :as s} (await (walk (floor (range 12)) args prep))]
          (is (pos? (count-of s :walk-plan.replan)))
          (is (= :arrived (:status @out)))
          (is (<= 1 (:replans @out))))))))

(deftest missing-path-world-is-unsupported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (walk (floor (range 12)) args (fn [p] (js-delete p "pathWorld"))))]
          (is (= {:status :unsupported} @out)))))))

(deftest decide-is-not-an-enumerable-key-of-the-act-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (walk (floor (range 12)) args (fn [_])))
              started (filter #(and (= :action (:source %)) (= :started (:kind %)) (contains? (:args %) "timeoutS")) @seen)]
          (is (seq started))
          (is (every? #(not (contains? (:args %) "decide")) started)))))))

;; an island (stone at y 63, feet 64) of x 0..4 over a lower floor (stone at y 60, feet 61) of x 5..11; the goal stands on a
;; 3-high pillar of the lower floor, which no move reaches, so the plan is partial and its nearest end is down the drop
(def island (merge (box 0 63 0 4 63 2 "stone") (box 5 60 0 11 60 2 "stone")))
(def pillar-goal [10 64 1])
(def pillar (box 10 61 0 10 63 2 "stone"))

(defn ^:async stays-on-the-island
  "Walk to goal over blocks from the island, n times in a row, each from where the last ended: results, and the x of the body
  after each."
  [blocks goal n]
  (loop [i 0 at {:x 0 :y 64 :z 1} acc []]
    (if (= i n)
      acc
      (let [{:keys [out p]} (await (walk blocks {:to goal} (fn [_]) at))
            pos (.-pos (.self p))]
        (recur (inc i) {:x (.-x pos) :y (.-y pos) :z (.-z pos)} (conj acc [@out (.-x pos)]))))))

(deftest an-unreachable-goal-across-a-one-way-drop-leaves-the-body-on-the-island
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [rounds (await (stays-on-the-island (merge island pillar) pillar-goal 3))]
          (is (= [:no-path :no-path :no-path] (map (comp :status first) rounds)))
          (is (= [:one-way :one-way :one-way] (map (comp :reason first) rounds)))
          (is (= [:drop :drop :drop] (map (comp :kind :one-way first) rounds)))
          (is (every? #(<= (second %) 5) rounds) "the body never went down the drop"))))))

(deftest a-one-way-stop-is-one-event-with-where-and-how-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (walk (merge island pillar) {:to pillar-goal} (fn [_]) {:x 0 :y 64 :z 1}))
              ev (first (events-of s :walk-plan.result))]
          (is (= 1 (count (events-of s :walk-plan.result))))
          (is (= 0 (count-of s :walk-plan.replan)))
          (is (vector? (:at (:one-way @out))) "the cell of the step not taken")
          (is (number? (:near @out)) "how far from the goal the walk stopped")
          (is (= :one-way (:reason ev))))))))

(deftest a-reachable-goal-across-the-same-drop-is-walked-whole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (walk island {:to [10 61 1]} (fn [_]) {:x 0 :y 64 :z 1}))
              pos (.-pos (.self p))]
          (is (= :arrived (:status @out)))
          (is (> (.-x pos) 9)))))))

;; level floor, a wall 2 high across the whole width: nothing in the plan is one-way, the body goes as near as it can
(def walled (merge (box 0 63 0 11 63 2 "stone") (box 6 64 0 6 65 2 "stone")))

(deftest an-unreachable-goal-on-level-ground-goes-near-and-can-walk-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (walk walled {:to [10 64 1]} (fn [_]) {:x 0 :y 64 :z 1}))
              pos (.-pos (.self p))]
          (is (= :no-path (:status @out)))
          (is (not= :one-way (:reason @out)))
          (is (>= (.-x pos) 4) "it went near the wall")
          (is (< (.-x pos) 6)))))))
