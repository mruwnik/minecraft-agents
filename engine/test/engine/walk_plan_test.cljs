(ns engine.walk-plan-test
  "jobs.debug.walk-plan against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def job 'jobs.debug.walk-plan)

(defn floor
  "Stone at y 63 for x in xs, z 0..2."
  [xs]
  (into {} (for [x xs z (range 3)] [(str x "," 63 "," z) "stone"])))

(defn setup
  "An engine over the fake world; a recording parent runs the job as its child and puts the child's
  result in :out when it finishes. prep is called with the primitives before the first tick."
  [blocks args prep]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {:blocks blocks :self {:pos {:x 0 :y 64 :z 1}}})
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
  "Setup, tick until the job is gone, return the setup map."
  [blocks args prep]
  (let [{:keys [eng clock] :as s} (setup blocks args prep)]
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
        (let [{:keys [out] :as s} (await (walk (merge (floor (range 12)) {"0,64,1" "water"}) args (fn [_])))]
          (is (= {:status :refused :kind :swim} (select-keys @out [:status :kind])))
          (is (= :swim (:refused-kind (first (events-of s :walk-plan.result))))))))))

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
