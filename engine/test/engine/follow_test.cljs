(ns engine.follow-test
  "jobs.movement.follow against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def job 'jobs.movement.follow)

(defn steve
  "A player entity named Steve at x along the row the body stands on."
  [x]
  {:id 5 :name "Steve" :kind "player" :pos {:x x :y 64 :z 0}})

(defn setup
  "An engine over the fake world; a recording parent runs the job as its child
  and puts the child's result in :out when it finishes."
  [world args]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :seen seen :out out}))

(defn ^:async run-ticks
  "Tick n times, the clock moving step ms before each; before-tick, when given,
  is called with the tick index first (to change the world)."
  ([s n step] (run-ticks s n step (fn [_])))
  ([{:keys [eng clock]} n step before-tick]
   (loop [i 0]
     (when (and (< i n) (seq (:list (core/state eng))))
       (before-tick i)
       (swap! clock + step)
       (await (core/tick! eng))
       (recur (inc i))))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn move-targets [p] (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true) (calls p "moveTo")))
(defn self-x [p] (.-x (.-pos (.self p))))
(defn entities [p] (.-entities (.-state (.-world p))))
(defn has-event? [{:keys [seen]} kind level] (boolean (some #(and (= kind (:kind %)) (= level (:level %))) @seen)))

(defn ^:async follow
  "Setup, run n ticks step ms apart, return the setup map."
  ([world args n step] (follow world args n step (fn [_ _])))
  ([world args n step before-tick]
   (let [s (setup world args)]
     (await (run-ticks s n step #(before-tick s %)))
     s)))

(deftest follow-check-wants-a-player-name
  (are [args ok] (= ok ((:check (get registry/jobs job)) {:args args}))
    {:player "Steve"} true
    {:player nil} false
    {} false
    {:player 5} false))

(deftest follows-a-player-ten-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (follow {:entities [(steve 10)]} {:player "Steve"} 3 500))]
          (is (= 1 (count (move-targets p))))
          (is (= {:x 10 :y 64 :z 0} (first (move-targets p))))
          (is (<= (Math/abs (- 10 (self-x p))) 3))
          (is (= :not-done @out) "following goes on"))))))

(deftest a-player-within-range-is-only-looked-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (follow {:entities [(steve 2)]} {:player "Steve"} 4 500))]
          (is (empty? (calls p "moveTo")))
          (is (= 4 (count (calls p "look"))))
          (is (= 4 (count (calls p "wait"))))
          (is (= 65.6 (.-y (.-pos (.-args (first (calls p "look"))))))))))))

(deftest follows-again-when-the-player-moves-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [move (fn [s i] (when (= 2 i) (set! (.-x (.-pos (first (entities (:p s))))) 20)))
              {:keys [p]} (await (follow {:entities [(steve 2)]} {:player "Steve"} 4 500 move))]
          (is (= [{:x 20 :y 64 :z 0}] (move-targets p)))
          (is (= 20 (self-x p))))))))

(deftest a-player-who-disappears-is-walked-to-then-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [vanish (fn [s i] (when (= 1 i) (.splice (entities (:p s)) 0 1)))
              {:keys [p out] :as s} (await (follow {:entities [(steve 10)]} {:player "Steve" :lost-s 3} 20 1000 vanish))]
          (is (= {:reason "lost" :last-seen {:x 10 :y 64 :z 0}} @out))
          (is (= 1 (count (move-targets p))) "the last-seen cell is walked to once")
          (is (has-event? s :follow.lost :info)))))))

(deftest a-player-who-is-never-there-is-absent-after-two-seconds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [clock out] :as s} (await (follow {} {:player "Steve"} 10 500))]
          (is (= {:reason "absent"} @out))
          (is (<= 2000 (- @clock 1000000)))
          (is (has-event? s :follow.absent :info)))))))

(deftest not-yet-absent-within-the-grace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (follow {} {:player "Steve"} 2 500))]
          (is (= :not-done @out)))))))

(deftest three-blocked-walks-are-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (follow {:entities [(steve 10)] :unreachable ["10,64,0"]} {:player "Steve"} 10 500))]
          (is (= {:reason "unreachable"} @out))
          (is (= 3 (count (calls p "moveTo"))))
          (is (has-event? s :follow.unreachable :warn)))))))

(deftest timeout-ends-the-follow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (follow {:entities [(steve 2)]} {:player "Steve" :timeout-s 3} 20 1000))]
          (is (= {:reason "timeout"} @out)))))))

(deftest three-arrived-but-out-of-range-walks-are-out-of-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(steve 10)]} {:player "Steve"})
              {:keys [p out]} s]
          (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status "arrived"}))
          (await (run-ticks s 10 500))
          (is (= {:reason "out-of-range"} @out))
          (is (= 3 (count (calls p "moveTo"))))
          (is (has-event? s :follow.out-of-range :warn)))))))
