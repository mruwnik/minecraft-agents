(ns engine.follow-test
  "jobs.movement.follow against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.movement.follow :as follow]
            [jobs.movement.go-to :as go-to]
            [jobs.movement.go-to.tuning :as gts]))

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
        parent (tu/recording-parent job args out)
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

(deftest follow-condition-wants-a-player-name
  (are [args cause] (= cause (tu/kind-reason (follow/bad-args {:args args})))
    {:player "Steve"} [:ok :args]
    {:player nil} [:never :bad-args]
    {} [:never :bad-args]
    {:player 5} [:never :bad-args]))

(defn ended "The result fields a test reads of the follow's ending." [out]
  (select-keys out [:reason]))

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

(deftest a-player-who-disappears-is-walked-to-then-waited-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [vanish (fn [s i] (when (= 1 i) (swap! (fake/state (:p s)) update :entities subvec 1)))
              {:keys [out eng]} (await (follow {:entities [(steve 10)]} {:player "Steve"} 20 1000 vanish))]
          (is (= :not-done @out) "no lost clock: the call's limits end it")
          (is (every? #{{:x 10 :y 64 :z 0}} (move-targets eng)) "only the last-seen cell is walked to"))))))

(defn check-ctx
  "A ctx for the checks: the world's primitives and a job memory of mem."
  [p mem]
  {:primitives p :root "j1" :args {:player "Steve" :radius 64}
   :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data mem}]}}})})

(deftest a-player-never-seen-is-a-wait-not-an-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (follow {} {:player "Steve"} 10 500))
              c (check-ctx p {})]
          (is (= [:condition-failed :gone] [(:kind (:ending @out)) (:reason @out)]) "a called child that would wait ends")
          (is (= [:later :gone] (tu/kind-reason (follow/can-start? c))))
          (is (= [:later :gone] (tu/kind-reason (follow/can-run? c)))))))))

(deftest a-player-gone-since-was-seen-is-walked-to-not-waited-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {} {:player "Steve"})]
          (is (= [:ok :trail] (tu/kind-reason (follow/can-run? (check-ctx (:p s) {:last-seen {:x 10 :y 64 :z 0}}))))))))))

(deftest a-player-in-sight-lets-it-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(steve 2)]} {:player "Steve"})]
          (is (= [:ok :in-sight] (tu/kind-reason (follow/in-sight (check-ctx (:p s) {}))))))))))

(deftest one-blocked-go-to-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (follow {:entities [(steve 10)] :unreachable ["10,64,0"]} {:player "Steve"} 10 500))]
          (is (= {:reason :unreachable} (ended @out)))
          (is (= gts/max-blocked (count (tu/walked-to (:eng s)))) "one go-to call (its own tries), then give up"))))))

(deftest one-blocked-go-to-ends-it-in-one-tick
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (follow {:entities [(steve 10)] :unreachable ["10,64,0"]} {:player "Steve"} 1 500))]
          (is (= {:reason :unreachable} (ended @out)))
          (is (= gts/max-blocked (count (tu/walked-to (:eng s)))) "one go-to call (its own tries), then give up"))))))

(deftest a-far-player-is-walked-to-within-the-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (follow {:entities [(steve 10)]} {:player "Steve"} 1 500))]
          (is (<= (Math/abs (- 10 (self-x p))) 3) "in range after the first round")
          (is (= :not-done @out)))))))

(deftest a-leg-that-got-nearer-is-no-blocked-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (follow {:entities [(steve 150)] :floor [-30 -10 160 10]} {:player "Steve" :radius 200} 1 500))]
          (is (< 5 (self-x p)) (str "legs on toward the player, at " (self-x p))))))))

(deftest the-out-of-range-count-is-against-where-the-target-stands-after-the-walk
  (is (= 0 (follow/next-out 2 1 {:x 0 :y 64 :z 0} {:x 30 :y 64 :z 0} 3)) "blocked resets")
  (is (= 0 (follow/next-out 2 0 {:x 8 :y 64 :z 0} {:x 9 :y 64 :z 0} 3)) "in range of where the player is now")
  (is (= 3 (follow/next-out 2 0 {:x 0 :y 64 :z 0} {:x 9 :y 64 :z 0} 3))))

(deftest timeout-ends-the-follow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p clock out] :as s} (setup {:entities [(steve 2)]} {:player "Steve" :spend {:job-time 3}})
              _ (.override (.-world p) "look" (fn [token a impl] (swap! clock + 2000) (impl token a)))
              _ (await (run-ticks s 20 1000))]
          (is (has-event? s :limit_reached))
          (is (= {:met true} (select-keys @out [:met]))))))))

(deftest the-wait-beside-the-player-is-a-noop-pause-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pauses (atom [])
              stub {:round (fn ^:async stub-round [c] (swap! pauses conj (:args c)) (ctx/end c :done nil {}))}
              s (setup {:entities [(steve 1)]} {:player "Steve" :range 3 :radius 10})]
          (await (run-ticks (assoc-in s [:eng :jobs 'jobs.time.noop] stub) 3 500))
          (is (= [{:seconds 0.5 :why :following}] (distinct @pauses)))
          (is (empty? (calls (:p s) "wait")) "no bare wait act"))))))
