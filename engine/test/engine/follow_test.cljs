(ns engine.follow-test
  "jobs.movement.follow against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.movement.follow :as follow]))

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
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
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
(defn move-targets [eng] (tu/walked-to eng))
(defn self-x [p] (.-x (.-pos (.self p))))
(defn entities [p] (fake/entities p))
(defn has-event? [{:keys [seen]} kind] (boolean (some #(= kind (:kind %)) @seen)))

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
        (let [{:keys [p out eng]} (await (follow {:entities [(steve 10)]} {:player "Steve"} 3 500))]
          (is (= 1 (count (move-targets eng))))
          (is (= {:x 10 :y 64 :z 0} (first (move-targets eng))))
          (is (<= (Math/abs (- 10 (self-x p))) 3))
          (is (= :not-done @out) "following goes on"))))))

(deftest a-player-within-range-is-only-looked-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (follow {:entities [(steve 2)]} {:player "Steve"} 4 500))]
          (is (empty? (tu/walk-calls p)))
          (is (= 4 (count (calls p "look"))))
          (is (= 4 (count (calls p "wait"))))
          (is (= 65.6 (.-y (.-pos (.-args (first (calls p "look"))))))))))))

(deftest follows-again-when-the-player-moves-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [move (fn [s i] (when (= 2 i) (swap! (fake/state (:p s)) assoc-in [:entities 0 :pos 0] 20)))
              {:keys [p eng]} (await (follow {:entities [(steve 2)]} {:player "Steve"} 4 500 move))]
          (is (= [{:x 20 :y 64 :z 0}] (move-targets eng)))
          (is (<= (Math/abs (- 20 (self-x p))) 3)))))))

(deftest a-player-who-disappears-is-walked-to-then-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [vanish (fn [s i] (when (= 1 i) (swap! (fake/state (:p s)) update :entities subvec 1)))
              {:keys [p out eng] :as s} (await (follow {:entities [(steve 10)]} {:player "Steve" :lost-s 3} 20 1000 vanish))]
          (is (= {:reason "lost" :last-seen {:x 10 :y 64 :z 0}} @out))
          (is (every? #{{:x 10 :y 64 :z 0}} (move-targets eng)) "only the last-seen cell is walked to")
          (is (has-event? s :follow.lost)))))))

(deftest a-player-who-is-never-there-is-absent-after-two-seconds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [clock out] :as s} (await (follow {} {:player "Steve"} 10 500))]
          (is (= {:reason "absent"} @out))
          (is (<= 2000 (- @clock 1000000)))
          (is (has-event? s :follow.absent)))))))

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
          (is (pos? (count (tu/walked-to (:eng s)))))
          (is (has-event? s :follow.unreachable)))))))

(deftest the-blocked-count-is-per-round-three-walks-end-it-in-one-tick
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (follow {:entities [(steve 10)] :unreachable ["10,64,0"]} {:player "Steve"} 1 500))]
          (is (= {:reason "unreachable"} @out))
          (is (has-event? s :follow.unreachable)))))))

(deftest a-far-player-is-walked-to-within-the-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (follow {:entities [(steve 10)]} {:player "Steve"} 1 500))]
          (is (<= (Math/abs (- 10 (self-x p))) 3) "in range after the first round")
          (is (= :not-done @out)))))))

(deftest the-out-of-range-count-is-against-where-the-target-stands-after-the-walk
  (is (= 0 (follow/next-out 2 1 {:x 0 :y 64 :z 0} {:x 30 :y 64 :z 0} 3)) "blocked resets")
  (is (= 0 (follow/next-out 2 0 {:x 8 :y 64 :z 0} {:x 9 :y 64 :z 0} 3)) "in range of where the player is now")
  (is (= 3 (follow/next-out 2 0 {:x 0 :y 64 :z 0} {:x 9 :y 64 :z 0} 3))))

(deftest timeout-ends-the-follow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (follow {:entities [(steve 2)]} {:player "Steve" :timeout-s 3} 20 1000))]
          (is (= {:reason "timeout"} @out)))))))
