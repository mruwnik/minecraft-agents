(ns engine.go-to-test
  "jobs.movement.go-to over the path planner and the executor, against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.walk :as walk]
            [engine.takeover :as takeover]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [jobs.movement.go-to :as go-to]))

(def start {:x 0 :y 64 :z 0})

(defn setup
  "A world key :light [sky block] makes the body see through perception, in that light everywhere."
  [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake (merge {:self {:pos start}} (dissoc world :light)))
        _ (when-let [l (:light world)] (swap! (fake/state raw) assoc :light-default l))
        p (if (:light world)
            (perception/wrap raw (perception/create (fake-raw/create raw) {:radius 16 :ray-deg 2 :now #(deref clock)}))
            raw)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn recording-parent
  "A parent that runs go-to with args as its child and keeps the child's result in out."
  [out args]
  {:check (constantly true)
   :round (fn ^:async recording-round [c]
            (let [r (await (ctx/call-child c :kid 'jobs.movement.go-to args))]
              (when (= :done r) (reset! out (ctx/child-result c :kid)))
              r))})

(defn ^:async tick-out!
  "Tick until the list is empty, at most n ticks; the ticks used."
  [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async go!
  "Run go-to with args as a child over world; {:eng :p :seen :out :ticks}, out the child's result."
  [world args]
  (let [{:keys [eng] :as s} (setup world)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out args)))]
    (core/submit! eng '(recording-parent) {})
    (assoc s :eng eng :out out :ticks (await (tick-out! eng 30)))))

(defn at [p] (let [pos (.-pos (.self p))] [(.-x pos) (.-y pos) (.-z pos)]))

(defn moved [eng] (mapv :data (mem/entries (mem/view (:store eng)) :moved)))

(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))

(def flat (floor -2 -3 40 3))

(deftest go-to-arrives-within-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [r [0 1 2]]
          (let [{:keys [out p]} (await (go! {:blocks flat} {:pos [10 64 0] :range r}))
                [x _ z] (at p)]
            (is (= {:arrived true} @out) (str "range " r))
            (is (<= (+ (* (- 10 x) (- 10 x)) (* z z)) (* r r)) (str "within range " r ", at " (at p)))))))))

(deftest go-to-from-the-edge-of-a-hole-arrives
  ;; the body's floored cell (0,64,0) has a hole under it; its hitbox (z .61-1.21) stands on the block under (0,64,1)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (go! {:blocks (dissoc flat "0,63,0") :self {:pos {:x 0.5 :y 64 :z 0.91}}}
                                                {:pos [10 64 0]}))]
          (is (= {:arrived true} @out) (str "events " (pr-str (map :data (events-of s :unreachable)))))
          (is (< (js/Math.hypot (- 10 (first (at p))) (last (at p))) 2) (str "at " (at p))))))))

(deftest go-to-already-within-range-does-not-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng p]} (await (go! {:blocks flat} {:pos [1 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [0 64 0] (at p)))
          (is (= [] (moved eng))))))))

(deftest go-to-writes-a-moved-entry-for-the-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (go! {:blocks flat} {:pos [10 64 0] :range 0}))
              [entry] (moved eng)]
          (is (= 1 (count (moved eng))))
          (is (= {:from start :status "arrived" :target {:x 10 :y 64 :z 0}} (select-keys entry [:from :status :target])))
          (is (= 10 (:x (:to entry))))
          (is (= {:cap 20 :ttl 600000} (mem/policy (mem/view (:store eng)) :moved))))))))

(def ledge (box 7 64 0 11 64 2 "stone"))
(def gap-up-world {:blocks (merge (box 0 63 0 4 63 2 "stone") ledge)})

(deftest go-to-gives-up-after-three-fruitless-rounds-and-carries-the-planner-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng p] :as s} (await (go! (assoc gap-up-world :self {:pos {:x 0 :y 64 :z 1}}) {:pos [10 65 1]}))]
          (is (= {:arrived false :reason :unreachable :why :abilities :kind :gap-up} @out))
          (is (= [0 64 1] (at p)) "the body did not move")
          (is (= ["blocked" "blocked" "blocked"] (mapv :status (moved eng))) "one :moved entry per round")
          (is (= [{:tries 3 :why :abilities :refused-kind :gap-up}]
                 (mapv #(select-keys % [:tries :why :refused-kind]) (events-of s :unreachable))))
          (is (= [{:arrived false :reason :unreachable :why :abilities :refused-kind :gap-up}]
                 (mapv #(select-keys % [:arrived :reason :why :refused-kind]) (events-of s :result)))))))))

(deftest go-to-reports-an-unreachable-fake-cell-as-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! {:blocks flat :unreachable ["10,64,0"]} {:pos [10 64 0]}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (keyword? (:why @out)) "the planner's reason")
          (is (= [0 64 0] (at p))))))))

(deftest go-to-keeps-walking-while-rounds-get-closer
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the floor ends at x 40 and the goal is in unloaded land: the first round walks to the edge, then nothing gets closer
        (let [{:keys [out eng p]} (await (go! {:blocks flat} {:pos [120 64 0]}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (> (first (at p)) 30) "the first round walked a long way")
          (is (= ["partial" "blocked" "blocked" "blocked"] (mapv :status (moved eng)))
              "a round that gets closer is progress and resets the count"))))))

(deftest go-to-goes-down-a-cliff-whose-foot-runs-into-unloaded-land
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the plateau (feet 64) ends at x 10 in a 3-drop to a floor (feet 61) that runs to the loaded edge at x 47; the goal is
        ;; far east in unloaded land
        (let [cliff (merge (floor -2 -3 10 3) (floor 60 11 -3 47 3))
              {:keys [p]} (await (go! {:blocks cliff} {:pos [120 61 0]}))]
          (is (= 61 (js/Math.floor (second (at p)))) "down the drop")
          (is (> (first (at p)) 40) "on to the loaded edge"))))))

(deftest go-to-never-drops-into-a-closed-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the same plateau, but below the drop is a pit (x 11..14) in front of a wall too high to climb out over
        (let [pit (merge (floor -2 -3 10 3) (floor 60 11 -3 14 3) (box 15 60 -3 16 64 3 "stone"))
              {:keys [p out]} (await (go! {:blocks pit} {:pos [120 64 0]}))]
          (is (= {:arrived false :reason :unreachable :why :one-way} (select-keys @out [:arrived :reason :why])))
          (is (= 64 (js/Math.floor (second (at p)))) "still on the plateau")
          (is (< (first (at p)) 11)))))))

(deftest go-to-accepts-doors
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [doors [:never :shut :leave-open]]
          (let [{:keys [out]} (await (go! {:blocks flat} {:pos [6 64 0] :doors doors}))]
            (is (= {:arrived true} @out) (str doors))))))))

(deftest go-to-without-path-sensing-says-unsupported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:blocks flat})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [6 64 0]})))]
          (set! (.-pathWorld p) nil)
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 10))
          (is (= {:arrived false :reason :unsupported} @out))
          (is (= [0 64 0] (at p)))
          (is (= [:unsupported] (mapv :reason (events-of s :refused)))))))))

(deftest a-cut-walk-leaves-no-controls_and_the-next-round-replans
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:blocks flat})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [10 64 0] :range 0})))
              world (.-world p)]
          (core/submit! eng '(recording-parent) {})
          (.hold world "steer")
          (let [running (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (takeover/take! eng {:who "claude" :why "cut"})
            (await running))
          (is (= {} (:controls @(fake/state p))) "no control is left pressed")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (tick-out! eng 10))
          (is (= {:arrived true} @out))
          (is (= [10 64 0] (at p)))
          (is (= 1 (count (moved eng))) "the cut walk wrote nothing; the replanned one wrote its entry"))))))

(deftest give-up-fields-explain-every-kind-of-fruitless-round
  (doseq [[result expected]
          [[{:status :no-path :reason :abilities :kind :gap-up} {:why :abilities :kind :gap-up}]
           [{:status :no-path :reason :door-stuck :cells [{:x 1 :y 64 :z 0}]} {:why :door-stuck :cells [{:x 1 :y 64 :z 0}]}]
           [{:status :no-path :reason :one-way :planner :exhausted :one-way {:x 3 :y 64 :z 0} :near 4.5}
            {:why :one-way :one-way {:x 3 :y 64 :z 0} :near 4.5}]
           [{:status :no-path :reason nil} {:why :no-path}]
           [{:status :no-path} {:why :no-path}]
           [{:status :stuck :step 3 :move :jump :target [4 65 0] :why "no progress on step 3 for 3.0 s"}
            {:why :stuck :kind :jump :detail "no progress on step 3 for 3.0 s"}]
           [{:status :stuck :why "walk timed out after 60 s"} {:why :stuck :detail "walk timed out after 60 s"}]
           [{:status :off-plan :at [1 64 0] :step 2} {:why :off-plan :detail "left the plan at step 2"}]
           [{:status :failed :reason "controls lost"} {:why :steer-failed :detail "controls lost"}]
           [{:status :failed} {:why :steer-failed}]
           [{:status :arrived :at [1 64 0]} {:why :no-progress}]]]
    (is (= expected (go-to/give-up-fields result)) (pr-str result))))

(defn ^:async go-prepped!
  "go! with (prepare! p) run on the fake primitives before the job starts."
  [world args prepare!]
  (let [{:keys [eng p] :as s} (setup world)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out args)))]
    (prepare! p)
    (core/submit! eng '(recording-parent) {})
    (assoc s :eng eng :out out :ticks (await (tick-out! eng 30)))))

(defn steer-through
  "A steer override that runs the walk's decide against pose-of (a fn of the tick count) for at most 200 ticks, as the real
  steer does, and ends done when decide does."
  [pose-of]
  (fn ^:async f [_ args _]
    (let [decide (.-decide args)]
      (loop [i 0]
        (if (>= i 200)
          #js {:status "timeout" :pose (pose-of i)}
          (let [out (decide (pose-of i))]
            (if (.-done out)
              #js {:status "done" :ticks i}
              (recur (inc i)))))))))

(defn pose [x z] #js {:x x :y 64 :z z :vy 0 :onGround true :onClimbable false :inWater false :collided false})

(defn override-steer! [p f] (.override (.-world p) "steer" f))

(deftest go-to-says-stuck-when-the-walk-never-progresses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng p] :as s} (await (go-prepped! {:blocks flat} {:pos [10 64 0] :range 0}
                                                            #(override-steer! % (steer-through (fn [_] (pose 0.5 0.5))))))
              result @out]
          (is (= {:arrived false :reason :unreachable :why :stuck}
                 (select-keys result [:arrived :reason :why])))
          (is (keyword? (:kind result)) "the move the walk was stuck on")
          (is (re-find #"^no progress on step \d+" (:detail result)))
          (is (= ["blocked" "blocked" "blocked"] (mapv :status (moved eng))))
          (is (= [{:tries 3 :why :stuck :refused-kind (:kind result)}]
                 (mapv #(select-keys % [:tries :why :refused-kind]) (events-of s :unreachable))))
          (is (= (:detail result) (:detail (first (events-of s :unreachable))))))))))

(deftest go-to-says-off-plan-when-the-walk-ends-off-its-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go-prepped! {:blocks flat} {:pos [10 64 0] :range 0}
                                                #(override-steer! % (steer-through (fn [_] (pose 0.5 30.5))))))]
          (is (= {:arrived false :reason :unreachable :why :off-plan}
                 (select-keys @out [:arrived :reason :why])))
          (is (re-find #"^left the plan at step" (:detail @out))))))))

(deftest go-to-says-steer-failed-when-the-steer-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go-prepped! {:blocks flat} {:pos [10 64 0] :range 0}
                                                #(override-steer! % (fn ^:async f [_ _ _] #js {:status "failed" :reason "no controls"}))))]
          (is (= {:arrived false :reason :unreachable :why :steer-failed :detail "no controls"} @out)))))))

(deftest go-to-says-no-progress-when-the-walk-ends-no-nearer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go-prepped! {:blocks flat} {:pos [10 64 0] :range 0}
                                                #(override-steer! % (fn ^:async f [_ _ _] #js {:status "done"}))))]
          (is (= {:arrived false :reason :unreachable :why :no-progress} @out)))))))

(deftest go-to-gives-up-at-once-on-a-walled-in-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a stone wall at x 5 cuts the floor: the goal's side is walled in
        (let [walled (merge (box -2 63 -2 10 63 4 "stone") (box 5 64 -2 5 65 4 "stone"))
              {:keys [out eng p]} (await (go! {:blocks walled} {:pos [8 64 1] :range 0 :escalate false}))]
          (is (= {:arrived false :reason :unreachable :why :goal-enclosed} @out))
          (is (= [0 64 0] (at p)) "the body did not walk to the wall")
          (is (= ["blocked"] (mapv :status (moved eng))) "one round"))))))

(deftest go-to-walks-to-where-the-loaded-land-runs-on-when-it-holds-no-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a walkway (feet 80) x 18..47 at z 8 over a floor x 0..47, z 0..15, with no way down; from x 48 on nothing is
        ;; loaded (the fake never loads more): the first round walks to the frontier, the rest find no new one
        (let [world {:blocks (merge (box 0 63 0 47 63 15 "stone") (box 18 79 8 47 79 8 "stone"))
                     :self {:pos {:x 18.5 :y 80 :z 8.5}}}
              {:keys [out eng p]} (await (go! world {:pos [10 64 8] :range 0}))]
          (is (= {:arrived false :reason :unreachable :why :exhausted} (select-keys @out [:arrived :reason :why])))
          (is (>= (first (at p)) 46) "walked to the frontier")
          (is (= ["partial" "blocked" "blocked" "blocked"] (mapv :status (moved eng)))
              "the walk to the frontier is progress"))))))

;; the same walkway with searches that take several rounds (live: soak j29/j30, a walled walkway at y 100 whose goal lies
;; below its middle). The walk to the frontier is refreshed on the way, and every search from the frontier is unfinished at
;; first: an unfinished search's nearest node (back on the walkway, over the goal) must not win over the frontier, or the
;; body swings between the two and gives up :off-plan
(deftest go-to-keeps-to-the-frontier-when-later-searches-take-several-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [budget walk/round-budget
              chunk walk/chunk-expansions
              world {:blocks (merge (box 0 63 0 47 63 15 "stone") (box 18 79 8 47 79 8 "stone"))
                     :self {:pos {:x 18.5 :y 80 :z 8.5}}}
              {:keys [eng p] :as s} (setup world)
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [10 64 8] :range 0})))]
          (reset! walk/searches {})
          (set! walk/round-budget 64)
          (set! walk/chunk-expansions 16)
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 600))
          (set! walk/round-budget budget)
          (set! walk/chunk-expansions chunk)
          (is (= {:arrived false :reason :unreachable :why :exhausted} (select-keys @out [:arrived :reason :why])))
          (is (>= (first (at p)) 46) "walked to the frontier and stayed there")
          (is (= ["partial" "blocked" "blocked" "blocked"] (mapv :status (moved eng)))
              "one walk to the frontier, then three rounds that find no new one")
          (is (= [] (filter #(= :refresh (:why %)) (remove :kept (events-of s :replan))))
              "no refresh swapped the walk to the frontier for an unfinished search's nearest node"))))))

;; a floor x 0..6, z 0..60 cut by a wall at x 3 (feet and head) open only at z 59..60: the way to x 6 runs 120 blocks
;; round, and no round's search gets nearer than the start. With a budget of 32 expansions a round, the rounds search on
;; (no walk, no :moved entry, no fruitless round) until the search ends, and go-to arrives
(deftest go-to-searches-on-over-rounds-until-its-search-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [budget walk/round-budget
              chunk walk/chunk-expansions
              joined (merge (box 0 63 0 6 63 60 "stone") (box 3 64 0 3 65 58 "stone"))
              {:keys [eng p] :as s} (setup {:blocks joined :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [6 64 0] :range 0})))]
          (reset! walk/searches {})
          (set! walk/round-budget 32)
          (set! walk/chunk-expansions 16)
          (core/submit! eng '(recording-parent) {})
          (let [plans (atom 0)
                plan-walk walk/plan-walk-budgeted!]
            (set! walk/plan-walk-budgeted! (fn [& args] (swap! plans inc) (apply plan-walk args)))
            (await (tick-out! eng 200))
            (set! walk/plan-walk-budgeted! plan-walk)
            (is (> @plans 3) "several rounds searched"))
          (set! walk/round-budget budget)
          (set! walk/chunk-expansions chunk)
          (is (= {:arrived true} @out))
          (is (= [] (events-of s :unreachable)))
          (is (= ["arrived"] (mapv :status (moved eng))) "one round walked: the rounds that searched wrote nothing"))))))

;; ------------------------------------------------------- looking round on arrival (card 9970c377)

(deftest go-to-looks-round-on-arrival-in-the-dark-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lit (await (go! {:blocks flat :light [15 0]} {:pos [10 64 0] :range 1}))
              dark (await (go! {:blocks flat :light [0 0]} {:pos [10 64 0] :range 1}))
              watched (fn [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :watched))]
          (is (= {:arrived true} @(:out dark)))
          (is (empty? (watched lit)))
          (is (seq (watched dark))))))))

(deftest go-to-range-0-from-a-block-edge-to-the-cell-it-stands-on-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go-prepped! {:self {:pos {:x 0.5 :y 64 :z -9.2}} :blocks {"0,63,-9" "stone"}}
                                                {:pos [0 64 -9] :range 0} identity))]
          (is (true? (:arrived @out)) (pr-str @out)))))))
