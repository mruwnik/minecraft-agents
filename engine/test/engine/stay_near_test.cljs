(ns engine.stay-near-test
  "jobs.movement.stay-near against the fake world: a player followed, a cell lingered near."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.game :as game]
            [engine.contract :as contract]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.movement.stay-near :as stay-near]
            [jobs.movement.go-to :as go-to]
            [engine.event-api :as event-api]))

(def job 'jobs.movement.stay-near)

(defn steve
  "A player entity named Steve at x along the row the body stands on."
  [x]
  {:id 5 :name "Steve" :kind "player" :pos {:x x :y 64 :z 0}})

(defn setup
  "An engine over the fake world; a recording parent runs the job as its child
  and puts the child's result in :out when it finishes."
  ([world args] (setup world args {}))
  ([world args stubs]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        out (atom :not-done)
        parent (tu/recording-parent job args out)
        eng (core/create {:primitives p :jobs (merge registry/jobs stubs {'recording-parent parent})
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :seen seen :out out})))

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

(defn ^:async stay
  "Setup, run n ticks step ms apart, return the setup map."
  ([world args n step] (stay world args n step (fn [_ _])))
  ([world args n step before-tick]
   (let [s (setup world args)]
     (await (run-ticks s n step #(before-tick s %)))
     s)))

(deftest a-player-needs-a-name
  (are [args cause] (= cause (tu/kind-reason (stay-near/bad-args {:args args})))
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
        (let [{:keys [p out eng]} (await (stay {:entities [(steve 10)]} {:player "Steve"} 3 500))]
          (is (= 1 (count (move-targets eng))))
          (is (= {:x 10 :y 64 :z 0} (first (move-targets eng))))
          (is (<= (Math/abs (- 10 (self-x p))) 3))
          (is (= :not-done @out) "following goes on"))))))

(deftest a-player-within-range-is-only-looked-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (stay {:entities [(steve 2)]} {:player "Steve"} 4 500))]
          (is (empty? (tu/walk-calls p)))
          (is (= 4 (count (calls p "look"))))
          (is (= 4 (count (calls p "wait"))))
          (is (= (+ 64 game/eye-height) (.-y (.-pos (.-args (first (calls p "look"))))))))))))

(deftest follows-again-when-the-player-moves-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [move (fn [s i] (when (= 2 i) (swap! (fake/state (:p s)) assoc-in [:entities 0 :pos 0] 20)))
              {:keys [p eng]} (await (stay {:entities [(steve 2)]} {:player "Steve"} 4 500 move))]
          (is (= [{:x 20 :y 64 :z 0}] (move-targets eng)))
          (is (<= (Math/abs (- 20 (self-x p))) 3)))))))

(deftest a-player-who-disappears-is-walked-to-then-waited-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [vanish (fn [s i] (when (= 1 i) (swap! (fake/state (:p s)) update :entities subvec 1)))
              {:keys [out eng]} (await (stay {:entities [(steve 10)]} {:player "Steve"} 20 1000 vanish))]
          (is (= :not-done @out) "no lost clock: the call's limits end it")
          (is (every? #{{:x 10 :y 64 :z 0}} (move-targets eng)) "only the last-seen cell is walked to"))))))

(deftest a-player-never-seen-is-waited-for-until-the-wall-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (stay {} {:player "Steve" :spend {:wall-time 3}} 10 1000))]
          (is (has-event? s :limit_reached) "a started call, so its wall time ends it")
          (is (= 0 (count (move-targets (:eng s)))) "nothing to walk to")
          (is (<= (Math/abs (self-x p)) 1)))))))

(deftest the-idle-wait-says-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [whys (atom [])
              stub {:round (fn ^:async stub-round [c] (swap! whys conj (:why (:args c))) (ctx/end c :done nil {}))}
              unseen (setup {} {:player "Steve"} {'jobs.time.noop stub})
              _ (await (run-ticks unseen 3 500))
              seen-one (setup {:entities [(steve 1)]} {:player "Steve"} {'jobs.time.noop stub})
              _ (await (run-ticks seen-one 3 500))]
          (is (= #{:player-unseen :following} (set @whys))))))))

(deftest one-blocked-leg-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (stay {:entities [(steve 10)] :unreachable ["10,64,0"]} {:player "Steve"} 10 500))]
          (is (= {:reason :unreachable} (ended @out)))
          (is (= 1 (count (tu/walked-to (:eng s)))) "one leg, one try, then give up"))))))

(deftest one-blocked-leg-ends-it-in-one-tick
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (stay {:entities [(steve 10)] :unreachable ["10,64,0"]} {:player "Steve"} 1 500))]
          (is (= {:reason :unreachable} (ended @out)))
          (is (= 1 (count (tu/walked-to (:eng s)))) "one leg, one try, then give up"))))))

(deftest a-far-player-is-walked-to-within-the-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (stay {:entities [(steve 10)]} {:player "Steve"} 1 500))]
          (is (<= (Math/abs (- 10 (self-x p))) 3) "in range after the first round")
          (is (= :not-done @out)))))))

(deftest a-leg-that-got-nearer-is-no-blocked-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (stay {:entities [(steve 150)] :floor [-30 -10 160 10]} {:player "Steve" :settings {:jobs.movement.stay-near/radius 200}} 1 500))]
          (is (< 5 (self-x p)) (str "legs on toward the player, at " (self-x p))))))))

(deftest the-out-of-range-count-is-against-where-the-target-stands-after-the-walk
  (is (= 0 (stay-near/next-out 2 1 {:x 0 :y 64 :z 0} {:x 30 :y 64 :z 0} 3)) "blocked resets")
  (is (= 0 (stay-near/next-out 2 0 {:x 8 :y 64 :z 0} {:x 9 :y 64 :z 0} 3)) "in range of where the player is now")
  (is (= 3 (stay-near/next-out 2 0 {:x 0 :y 64 :z 0} {:x 9 :y 64 :z 0} 3))))

(deftest timeout-ends-the-stay
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
              s (setup {:entities [(steve 1)]} {:player "Steve" :range 3 :settings {:jobs.movement.stay-near/radius 10}})]
          (await (run-ticks (assoc-in s [:eng :jobs 'jobs.time.noop] stub) 3 500))
          (is (= [{:seconds 0.5 :why :following}] (distinct @pauses)))
          (is (empty? (calls (:p s) "wait")) "no bare wait act"))))))

(deftest the-out-of-range-give-up-comes-after-the-set-number-of-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(steve 20)] :floor [-30 -10 400 10]}
                       {:player "Steve" :range 3
                        :settings {:jobs.movement.stay-near/max-out-walks 2 :jobs.movement.stay-near/radius 10000}})
              p (:p s)
              away (fn [token a impl]
                     (swap! (fake/state p) update-in [:entities 0 :pos 0] + 40)
                     (impl token a))
              _ (doseq [n ["moveTo" "steer"]] (.override (.-world p) n away))
              _ (await (run-ticks s 6 500))]
          (is (= {:reason :out-of-range} (ended @(:out s))))
          (is (= 2 (count (move-targets (:eng s))))))))))

;; ---- a cell to stay near (was linger-near)

(defn direct
  "An engine running the job itself (not as a child), so its own events reach the sink."
  [args extra-jobs]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        eng (core/create {:primitives (tu/fake-on-floor {}) :jobs (merge registry/jobs extra-jobs) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng (list job args) {})
    {:eng eng :clock clock :seen seen}))

(deftest a-cell-needs-a-positive-for-s-when-given-and-one-target
  (are [args cause] (= cause (tu/kind-reason (stay-near/bad-args {:args args})))
    {:pos {:x 1 :y 64 :z 0} :for-s 5} [:ok :args]
    {:pos {:x 1 :y 64 :z 0} :for-s 0} [:never :bad-args]
    {:pos {:x 1 :y 64 :z 0}} [:ok :args]
    {:pos {:x 1 :y 64 :z 0} :player "Steve"} [:never :bad-args]
    {:for-s 5} [:never :bad-args]))

(deftest within-range-it-holds-still-and-ends-after-the-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out eng]} (await (stay {} {:pos {:x 2 :y 64 :z 0} :range 3 :for-s 3} 12 500))]
          (is (empty? (tu/walk-calls p)))
          (is (= {:status :done :stayed true :reason "waited"} (select-keys @out [:status :stayed :reason])))
          (is (empty? (tu/walked-to eng))))))))

(deftest a-far-target-is-walked-to-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out eng]} (await (stay {} {:pos {:x 10 :y 64 :z 0} :range 3 :for-s 30} 4 500))]
          (is (= [{:x 10 :y 64 :z 0}] (tu/walked-to eng)))
          (is (<= (Math/abs (- 10 (self-x p))) 3))
          (is (= :not-done @out) "still waiting"))))))

(deftest an-unreachable-target-ends-stopped-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (stay {:unreachable ["10,64,0"]} {:pos {:x 10 :y 64 :z 0} :range 0 :for-s 30} 12 500))]
          (is (= :stopped (:status @out)))
          (is (= :unreachable (:reason @out)))
          (is (false? (:stayed @out))))))))

(defn scripted-go-to
  "A go-to stand-in that answers each walk with the next of results: :arrive moves the body to the target, :fail
  does not arrive."
  [results]
  (let [left (atom results)]
    {:target [] :ways #{:walk :dig} :may-dig #{:own}
     :round (fn ^:async stub-round [c]
              (let [r (first @left)]
                (swap! left rest)
                (when (= :arrive r)
                  (let [{:keys [x y z]} (:pos (:args c))]
                    (fake/swap-self! (:p (:s c)) assoc :pos [x y z])))
                (if (= :arrive r)
                  (ctx/end c :done nil {:arrived true})
                  (ctx/end c :nothing-possible :no_path {:arrived false}))))}))

(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))

(deftest in-range-the-lingering-hold-is-declared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (stay {} {:pos {:x 2 :y 64 :z 0} :range 3 :for-s 30} 4 500))]
          (is (seq (of-kind seen :holding)))
          (is (= :lingering (:reason (first (of-kind seen :holding))))))))))

(deftest pushed-out-it-walks-back-and-keeps-waiting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (stay {} {:pos {:x 2 :y 64 :z 0} :range 3 :for-s 30} 3 500))
              p (:p s)]
          (is (empty? (tu/walked-to (:eng s))))
          (fake/swap-self! p assoc :pos [20 64 0])
          (await (run-ticks s 6 500))
          (is (= [{:x 2 :y 64 :z 0}] (tu/walked-to (:eng s))))
          (is (<= (Math/abs (- 2 (self-x p))) 3))
          (is (= :not-done @(:out s))))))))

(deftest the-first-failed-walk-back-ends-stopped-with-go-tos-ending-as-the-because
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stub (scripted-go-to [:fail :arrive])
              s (direct {:pos {:x 10 :y 64 :z 0} :range 1 :for-s 1} {'jobs.movement.go-to stub})]
          (await (run-ticks s 1 2000))
          (let [e (first (of-kind (:seen s) :stopped))]
            (is (some? e) "one failure is enough: go-to retries itself")
            (is (false? (:stayed e)))
            (is (= :no_path (contract/cause-key (:because e))) "go-to's own ending is the because")))))))

(deftest a-pause-that-does-not-end-done-adds-no-waited-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stub {:round (fn ^:async stub-round [c] :continue)}
              s (setup {} {:pos {:x 2 :y 64 :z 0} :range 3 :for-s 1} {'jobs.time.noop stub})]
          (await (run-ticks s 10 500))
          (is (= :not-done @(:out s))))))))

(deftest the-done-event-says-waited
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (direct {:pos {:x 0 :y 64 :z 0} :range 3 :for-s 1} {})]
          (await (run-ticks s 12 500))
          (is (= "waited" (:reason (first (of-kind (:seen s) :completed)))) (pr-str (of-kind (:seen s) :completed))))))))

(deftest walking-time-does-not-count-towards-the-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stub (scripted-go-to [:arrive])
              holder (atom nil)
              s (setup {} {:pos {:x 10 :y 64 :z 0} :range 1 :for-s 2}
                       {'jobs.movement.go-to (assoc stub :round (fn [c] ((:round stub) (assoc c :s @holder))))})]
          (reset! holder s)
          (await (run-ticks s 1 5000))
          (is (= :not-done @(:out s)) "five seconds of walking and an arrival: nothing waited yet")
          (await (run-ticks s 2 5000))
          (is (= :not-done @(:out s)) "one second in range of the two asked for")
          (await (run-ticks s 6 500))
          (is (= {:status :done :stayed true :reason "waited"} (select-keys @(:out s) [:status :stayed :reason])) "two seconds held in range ends it"))))))

(deftest the-wait-in-range-is-a-noop-pause-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pauses (atom [])
              stub {:round (fn ^:async stub-round [c] (swap! pauses conj (:args c)) (ctx/end c :done nil {}))}
              s (setup {} {:pos {:x 2 :y 64 :z 0} :range 3 :for-s 30} {'jobs.time.noop stub})]
          (await (run-ticks s 4 500))
          (is (= [{:seconds 0.5 :why :lingering}] (distinct @pauses)) "each look is a pause child, not a bare wait act")
          (is (empty? (filter #(= "wait" (.-name %)) (.-calls (.-world (:p s))))) "no bare wait act"))))))

(deftest the-quote-adds-the-walk-back-when-out-of-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [least (fn ^:async least-of [pos] (let [{:keys [eng]} (setup {} {:pos pos :range 3 :for-s 30})]
                                        (let [q (:quote (await (event-api/quote-view eng (str job) (pr-str {:pos pos :range 3 :for-s 30}))))]
                                          (is (nil? (contract/quote-problem q)))
                                          (is (= (stay-near/budget-s) (last (get-in q [:uses :job-time]))) "most is the job-time budget")
                                          (first (get-in q [:uses :job-time])))))
              near (await (least {:x 2 :y 64 :z 0}))
              far (await (least {:x 10 :y 64 :z 0}))]
          (is (> far near)))))))
