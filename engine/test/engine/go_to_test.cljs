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
            [jobs.lib.threats :as threats]
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
          (is (= {:status :stopped :arrived false :reason :unreachable :why :abilities :kind :gap-up} (dissoc @out :at :near :text)))
          (is (re-find #"^gave up walking to \[10 65 1\]: .*step up" (:text @out)) "the reason in words")
          (is (not (re-find #"[{}]" (:text @out))) "no map dump")
          (is (= [0 64 1] (at p)) "the body did not move")
          (is (= ["blocked" "blocked" "blocked"] (mapv :status (moved eng))) "one :moved entry per round")
          (is (= [{:tries 3 :why :abilities :refused-kind :gap-up}]
                 (mapv #(select-keys % [:tries :why :refused-kind]) (events-of s :unreachable))))
          (is (= [{:status :stopped :arrived false :reason :unreachable :why :abilities :refused-kind :gap-up}]
                 (mapv #(select-keys % [:status :arrived :reason :why :refused-kind]) (events-of s :result)))))))))

(deftest a-top-level-go-to-that-gives-up-or-refuses-is-stopped-not-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup (assoc gap-up-world :self {:pos {:x 0 :y 64 :z 1}}))
              _ (core/submit! eng '(jobs.movement.go-to {:pos [10 65 1]}) {})
              _ (await (tick-out! eng 30))
              ev (filter #(and (= :job (:source %)) (#{:stopped :completed} (:kind %))) @seen)]
          (is (= [:stopped] (mapv :kind ev)))
          (is (re-find #"^gave up walking to" (:text (first ev))))
          (is (re-find #"^gave up walking to \[10 65 1\]: it needs a step up" (:text (first (filter #(= :unreachable (:kind %)) @seen))))))))))

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
          (is (= {:status :stopped :arrived false :reason :unsupported} (select-keys @out [:status :arrived :reason])))
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
          (is (= {:arrived false :reason :unreachable :why :steer-failed :detail "no controls"} (dissoc @out :at :near :text :status))))))))

(deftest go-to-says-no-progress-when-the-walk-ends-no-nearer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go-prepped! {:blocks flat} {:pos [10 64 0] :range 0}
                                                #(override-steer! % (fn ^:async f [_ _ _] #js {:status "done"}))))]
          (is (= {:arrived false :reason :unreachable :why :no-progress} (dissoc @out :at :near :text :status))))))))

(deftest go-to-retry-false-ends-a-walk-that-got-no-nearer-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args want] [[{} 3] [{:retry false} 1]]]
          (let [steers (atom 0)
                {:keys [out]} (await (go-prepped! {:blocks flat} (merge {:pos [10 64 0] :range 0 :escalate false} args)
                                                  #(override-steer! % (fn ^:async f [_ _ _] (swap! steers inc) #js {:status "done"}))))]
            (is (= want @steers) (str "walks for " args))
            (is (= {:arrived false :reason :unreachable :why :no-progress} (dissoc @out :at :near :text :status :tries))
                (str "same give-up for " args))))))))

(deftest go-to-look-round-false-skips-the-arrival-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [watched (fn [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :watched))
              dark (await (go! {:blocks flat :light [0 0]} {:pos [10 64 0] :range 1 :look-round false}))]
          (is (= {:arrived true} @(:out dark)))
          (is (empty? (watched dark))))))))

(deftest go-to-gives-up-at-once-on-a-walled-in-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a stone wall at x 5 cuts the floor: the goal's side is walled in
        (let [walled (merge (box -2 63 -2 10 63 4 "stone") (box 5 64 -2 5 65 4 "stone"))
              {:keys [out eng p]} (await (go! {:blocks walled} {:pos [8 64 1] :range 0 :escalate false}))]
          (is (= {:arrived false :reason :unreachable :why :goal-cut-off} (dissoc @out :at :near :text :status)))
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

;; ------------------------------------------------------- one call is one whole attempt (docs/job-rounds-design.md)

(defn returns-parent
  "A parent that calls go-to with args as its child, keeps every call's return in returns and the child's result in out.
  seed, when given, is put in the child's memory before its first call (a stale hint left by an earlier round)."
  [out returns args seed]
  {:check (constantly true)
   :round (fn ^:async returns-round [c]
            (when (and seed (empty? @returns) (nil? (get-in (ctx/mem c) [:children :kid])))
              (ctx/update-mem! c assoc-in [:children :kid] (merge {:args args :children {}} seed)))
            (let [r (await (ctx/call-child c :kid 'jobs.movement.go-to args))]
              (swap! returns conj r)
              (when (= :done r) (reset! out (ctx/child-result c :kid)))
              r))})

(defn ^:async go-returns!
  "Run go-to with args under returns-parent over world; {:eng :p :seen :out :returns}."
  ([world args] (go-returns! world args nil))
  ([world args seed]
   (let [{:keys [eng] :as s} (setup world)
         out (atom :not-done)
         returns (atom [])
         eng (assoc eng :jobs (assoc (:jobs eng) 'returns-parent (returns-parent out returns args seed)))]
     (core/submit! eng '(returns-parent) {})
     (await (tick-out! eng 30))
     (assoc s :eng eng :out out :returns returns))))

(deftest one-go-to-call-walks-until-it-gives-up-with-no-continue-between
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out returns eng]} (await (go-returns! (assoc gap-up-world :self {:pos {:x 0 :y 64 :z 1}})
                                                            {:pos [10 65 1]}))]
          (is (= :unreachable (:reason @out)))
          (is (= ["blocked" "blocked" "blocked"] (mapv :status (moved eng))) "three fruitless walks")
          (is (= [:done] @returns) "all of them in one call"))))))

(deftest one-go-to-call-walks-to-the-frontier-and-on-with-no-continue-between
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks (merge (box 0 63 0 47 63 15 "stone") (box 18 79 8 47 79 8 "stone"))
                     :self {:pos {:x 18.5 :y 80 :z 8.5}}}
              {:keys [out returns eng]} (await (go-returns! world {:pos [10 64 8] :range 0}))]
          (is (= :exhausted (:why @out)))
          (is (= ["partial" "blocked" "blocked" "blocked"] (mapv :status (moved eng))))
          (is (= [:done] @returns)))))))

(def joined-world
  "A floor x 0..6, z 0..60 cut by a wall at x 3 open only at z 59..60: the way from x 0 to x 6 runs 120 blocks round."
  {:blocks (merge (box 0 63 0 6 63 60 "stone") (box 3 64 0 3 65 58 "stone")) :self {:pos {:x 0.5 :y 64 :z 0.5}}})

(defn ^:async with-small-budget!
  "Run (f) with a search budget of 32 expansions a slice and go-to's pace! counted in paces; restores both."
  [paces f]
  (let [budget walk/round-budget
        chunk walk/chunk-expansions
        pace go-to/pace!]
    (reset! walk/searches {})
    (set! walk/round-budget 32)
    (set! walk/chunk-expansions 16)
    (set! go-to/pace! (fn [] (swap! paces inc) (pace)))
    (try (await (f))
         (finally (set! walk/round-budget budget)
                  (set! walk/chunk-expansions chunk)
                  (set! go-to/pace! pace)))))

(deftest one-go-to-call-searches-on-paced-until-its-search-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [paces (atom 0)
              plans (atom 0)
              plan-walk walk/plan-walk-budgeted!]
          (set! walk/plan-walk-budgeted! (fn [& args] (swap! plans inc) (apply plan-walk args)))
          (let [{:keys [out returns]} (await (with-small-budget! paces #(go-returns! joined-world {:pos [6 64 0] :range 0})))]
            (set! walk/plan-walk-budgeted! plan-walk)
            (is (= {:arrived true} @out))
            (is (> @plans 3) "several search slices")
            (is (= [:done] @returns) "searched and walked in one call")
            (is (>= @paces (dec @plans)) "every slice that walked nowhere awaited the pace timer")))))))

(deftest a-cut-stops-the-search-loop-and-the-next-call-resumes-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [paces (atom 0)
              plans (atom 0)
              plan-walk walk/plan-walk-budgeted!]
          (set! walk/plan-walk-budgeted! (fn [& args] (swap! plans inc) (apply plan-walk args)))
          (await
           (with-small-budget! paces
             (fn ^:async cut-run []
               (let [{:keys [eng p] :as s} (setup joined-world)
                     out (atom :not-done)
                     returns (atom [])
                     eng (assoc eng :jobs (assoc (:jobs eng) 'returns-parent
                                                 (returns-parent out returns {:pos [6 64 0] :range 0} nil)))]
                 (core/submit! eng '(returns-parent) {})
                 (let [running (core/tick! eng)]
                   (loop [i 0]
                     (when (and (< @paces 2) (< i 400))
                       (await (js/Promise. (fn [r] (js/setTimeout r 5))))
                       (recur (inc i))))
                   (is (>= @paces 2) "the search loop paced")
                   (takeover/take! eng {:who "claude" :why "cut"})
                   (let [at-cut @plans]
                     (await running)
                     (is (<= @plans (inc at-cut)) "at most the slice in flight ran after the cut")
                     (is (= [] @returns) "the cut call returned nothing")
                     (is (= [] (moved eng)) "it walked nowhere")))
                 (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
                 (await (tick-out! eng 10))
                 (is (= {:arrived true} @out) (pr-str @out))
                 (is (= [6 64 0] (at p)))
                 (is (= [:done] @returns) "the resumed call arrived in one call")
                 (is (= [] (events-of s :unreachable)))))))
          (set! walk/plan-walk-budgeted! plan-walk))))))

(deftest a-call-reads-its-memory-as-a-hint
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; stale counters from an earlier, cut round: a search that went on 99 times and two fruitless walks
        (let [{:keys [out returns]} (await (go-returns! {:blocks flat} {:pos [10 64 0] :range 0}
                                                        {:searching 99 :blocked 2 :best 0}))]
          (is (= {:arrived true} @out) (pr-str @out))
          (is (= [:done] @returns)))
        ;; an escalation left in memory by a round a reflex cut, with the body now already at the goal: arrived, and no
        ;; escalation child is started
        (let [{:keys [out returns p eng]} (await (go-returns! {:blocks flat} {:pos [1 64 0] :range 1}
                                                              {:escalation {:step :stair :heading [1 0] :steps 3}
                                                               :escalations 1 :escalation-from [0 64 0] :planned []}))
              solid (fn [blocks] (count (remove #(= "air" (val %)) blocks)))]
          (is (= {:arrived true} @out) (pr-str @out))
          (is (= [:done] @returns))
          (is (= (solid flat) (solid (:blocks @(fake/state p)))) "no stair child dug")
          (is (= [] (mem/entries (mem/view (:store eng)) :tidy)) "nothing in the dig ledger"))))))

;; ------------------------------------------------------- a walker fault at one cell is routed round (card 679d3475)

(def east-yaw (- (/ js/Math.PI 2)))

(defn fault-at-east-edge!
  "Steer override: the body cannot get past x 4.9 on the z 0 row heading east (the walker's fault at the cell [5 64 0]); every
  pose is pushed to visited, an atom of [cell-x cell-z] pairs."
  [p visited]
  (.override (.-world p) "steer"
             (fn [token ^js a impl]
               (let [decide (.-decide a)
                     wrapped (fn [pose]
                               (swap! visited conj [(js/Math.floor (.-x pose)) (js/Math.floor (.-z pose))])
                               (let [^js out (decide pose)]
                                 (if (and (not (.-done out)) (<= 4.9 (.-x pose) 5.5) (= 0 (js/Math.floor (.-z pose)))
                                          (< (js/Math.abs (- (.-yaw out) east-yaw)) 0.3))
                                   #js {:controls #js {} :yaw (.-yaw out)}
                                   out)))]
                 (impl token (js/Object.assign #js {} a #js {:decide wrapped}))))))

(deftest a-frontier-walk-ended-at-its-partial-end-is-no-walker-fault
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (go! {:blocks flat} {:pos [120 64 0]}))]
          (is (= false (:arrived @out)))
          (is (= 0 (count (events-of s :walker-fault))) "the partial plan's end is a normal end, no warn"))))))

(deftest go-to-routes-round-a-cell-the-walker-got-stuck-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [visited (atom #{})
              {:keys [out] :as s} (await (go-prepped! {:blocks flat} {:pos [10 64 0] :range 0} #(fault-at-east-edge! % visited)))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= 1 (count (events-of s :walker-fault))) "the walker's fault stays visible as a warn")
          (is (every? (fn [[_ z]] (<= -1 z 1)) @visited) "a short detour, not a wide one"))))))

(deftest go-to-still-gives-up-stuck-when-the-only-way-runs-over-the-faulty-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go-prepped! {:blocks (floor -2 0 40 0)} {:pos [10 64 0] :range 0}
                                                #(fault-at-east-edge! % (atom #{}))))]
          (is (= {:arrived false :reason :unreachable :why :stuck}
                 (select-keys @out [:arrived :reason :why]))))))))

(deftest a-walk-plans-with-the-cells-to-avoid-and-keeps-them-in-its-search-key
  (let [p (tu/fake {:self {:pos start} :blocks flat})
        pw (walk/path-world p)
        avoided (walk/with-avoid pw #{[5 64 0] [6 64 1]})
        same (walk/with-avoid pw #{[6 64 1] [5 64 0]})
        other (walk/with-avoid pw #{[5 64 0]})]
    (is (= (walk/avoid-key avoided) (walk/avoid-key same)))
    (is (not= (walk/avoid-key avoided) (walk/avoid-key other)))
    (is (nil? (walk/avoid-key (walk/with-avoid pw #{}))) "no cells: the key of a plain search")
    (is (= (walk/avoid-key avoided) (walk/avoid-key (walk/with-walls avoided [[1 64 1]]))) "walls keep the avoided cells")
    (is (= 2 (.-size (.-cells (.-avoid (walk/plan-options avoided 1 nil nil))))))
    (is (nil? (.-avoid (walk/plan-options pw 1 nil nil))))))

(defn ^:async go-place!
  "Run go-to with args as a child over flat after writing the places in memory; {:out :p :seen}."
  [places args]
  (let [{:keys [eng] :as s} (setup {:blocks flat})
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out args)))]
    (doseq [[k pos] places] (mem/write! (:store eng) k {:pos pos} mem/place-policy))
    (core/submit! eng '(recording-parent) {})
    (await (tick-out! eng 30))
    (assoc s :out out)))

(deftest go-to-walks-to-a-named-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go-place! {:home {:x 10 :y 64 :z 0}} {:place :home :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [10 64 0] (at p))))))))

(deftest go-to-refuses-an-unknown-or-bad-place-name-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[place reason] [[:nowhere :unknown-place] ["Bad Name" :bad-name]]]
          (let [{:keys [out p seen]} (await (go-place! {:home {:x 10 :y 64 :z 0}} {:place place}))]
            (is (= reason (:reason @out)) (str place))
            (is (= :stopped (:status @out)))
            (is (re-find #"place" (:text @out)))
            (is (= [0 64 0] (at p)) "did not walk")
            (is (= 1 (count (events-of {:seen seen} :refused))))))))))

(deftest go-to-plans-with-dangers-unless-told-not-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args want] [[{} true] [{:dangers false} false]]]
          (let [planned (atom 0)]
            (with-redefs [threats/planner-dangers (fn [_] (swap! planned inc) nil)]
              (await (go! {:blocks flat} (assoc args :pos [10 64 0]))))
            (is (= want (pos? @planned)) (str "args " args))))))))

(deftest go-to-leg-s-walks-one-short-leg-and-ends-for-the-caller-to-re-aim
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! {:blocks flat} {:pos [35 64 0] :range 0 :leg-s 1}))]
          (is (= {:arrived false :leg true} (select-keys @out [:arrived :leg])) "a leg, not an arrival or a give-up")
          (is (< 0 (first (at p)) 35) (str "moved on toward the goal, at " (at p))))
        (let [{:keys [out p]} (await (go! {:blocks flat} {:pos [35 64 0] :range 0}))]
          (is (= {:arrived true} @out) "without :leg-s the whole way is walked")
          (is (= 35 (first (at p)))))))))
