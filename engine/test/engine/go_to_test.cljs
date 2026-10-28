(ns engine.go-to-test
  "jobs.movement.go-to over the path planner and the executor, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.go-to-support :refer [result-of]]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.fake :as fake]
            [engine.fake.steer :as steer]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.perception.rays :as rays]
            [jobs.lib.danger :as danger-q]
            [engine.fake.raw-world :as fake-raw]
            [engine.path.planner-tuned :as planner]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.cost :as cost]
            [jobs.lib.threats :as threats]
            ["fs" :as fs]
            ["path" :as path]
            [jobs.lib.world-files :as world-files]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [engine.way-on-test :as wt]
            [jobs.movement.go-to :as go-to]
            [jobs.movement.go-to.cut :as gcut]
            [jobs.movement.go-to.faults :as gfaults]
            [jobs.lib.pace :as pace]
            [jobs.movement.go-to.result :as end]))

(def start {:x 0 :y 64 :z 0})

(defn blocks-box
  "The box [x0 y0 z0 x1 y1 z1] round the cells of blocks, three more on every side: the world a body that has been all over
  it has seen."
  [blocks]
  (let [ks (map (fn [k] (if (string? k) (mapv js/parseInt (.split k ",")) k)) (keys blocks))
        [lo hi] (for [f [min max]] (mapv (fn [i] (apply f (map #(nth % i) ks))) [0 1 2]))]
    (vec (concat (map #(- % 3) lo) (map #(+ % 3) hi)))))

(def sight-steps-per-tick 20)

(defn setup
  "A world key :light [sky block] makes the body see through perception, in that light everywhere; :jitter (0.5) fixes
  where each pass's rays fall in their grid cells; :clock is the atom of the fake time (ms); :sight true gives :step, which
  runs the sight passes (sight-steps-per-tick of them; tick-out! calls it before each tick), else the body sees only what its jobs look at.
  :seen is :all (the box round the blocks) or boxes [x0 y0 z0 x1 y1 z1] whose cells the body has already seen (memory takes their true state), for the
  planner's :unknown default: a world with :light and no :seen is one the body has never looked at."
  ([world] (setup world nil))
  ([world shared]
  (let [clock (or (:clock world) (atom 1000000))
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake (merge {:self {:pos start}} (dissoc world :light :jitter :clock :ray-deg :jobs :sight :seen)))
        _ (when-let [l (:light world)] (swap! (fake/state raw) assoc :light-default l))
        per (when (:light world)
              (perception/create (fake-raw/create raw) {:radius 16 :ray-deg (:ray-deg world 2) :now #(deref clock)
                                                        :rand (constantly (:jitter world 0.5))}))
        boxes (if (= :all (:seen world)) [(blocks-box (:blocks world))] (:seen world))
        _ (doseq [[x0 y0 z0 x1 y1 z1] boxes x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))]
            (rays/touch! per [x y z]))
        p (if per (perception/wrap raw per) raw)
        eng (core/create (cond-> {:primitives p :jobs (merge registry/jobs (:jobs world)) :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                           shared (assoc :world shared)))]
    {:eng eng :p p :seen seen
     :step (when (:sight world) #(dotimes [_ sight-steps-per-tick] (rays/step! (aget p "perception"))))
     :stop (fn [])})))

(defn recording-parent
  "A parent that runs go-to with args as its child (ctx/call) and keeps the child's result (result-of) in out."
  [out args]
  {:round (fn ^:async recording-round [c]
            (let [e (await (ctx/call c :kid 'jobs.movement.go-to args))]
              (if (= :continue e)
                :continue
                (do (reset! out (result-of e)) (ctx/end c :done nil)))))})

(defn ^:async tick-out!
  "Tick until the list is empty, at most n ticks, running step (sight passes) before each; the ticks used."
  ([eng n] (tick-out! eng n nil))
  ([eng n step]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (when step (step))
          (await (core/tick! eng))
          (recur (inc i)))))))

(defn ^:async go!
  "Run go-to with args as a child over world; {:eng :p :seen :out :ticks}, out the child's result."
  [world args]
  (let [{:keys [eng stop step] :as s} (setup world)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out args)))]
    (core/submit! eng '(recording-parent) {})
    (let [ticks (await (tick-out! eng 30 step))]
      (stop)
      (assoc s :eng eng :out out :ticks ticks))))

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

(deftest go-to-a-feet-cell-over-a-short-floor-walks-onto-it
  ;; the planner stands in a farmland or path block's own cell (floor 15/16), so the feet cell above it (the goal a caller
  ;; names) is mapped to that cell. The fake stands a body on top of such a block (y 64), so arrival is not judged here.
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [n ["farmland" "dirt_path"]]
          (let [{:keys [out p]} (await (go! {:blocks (assoc flat "10,63,0" n)} {:pos [10 64 0] :range 0}))
                [x _ z] (at p)]
            (is (not= :goal-not-standable (:why @out)) n)
            (is (= [10 0] [(js/Math.floor x) (js/Math.floor z)]) (str n " walked onto the block, at " (at p)))))))))

(deftest go-to-from-the-edge-of-a-hole-arrives
  ;; the body's floored cell (0,64,0) has a hole under it; its hitbox (z .61-1.21) stands on the block under (0,64,1)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (go! {:blocks (dissoc flat "0,63,0") :self {:pos {:x 0.5 :y 64 :z 0.91}}}
                                                {:pos [10 64 0]}))]
          (is (= {:arrived true} @out) (pr-str @out))
          (is (< (js/Math.hypot (- 10 (first (at p))) (last (at p))) 2) (str "at " (at p))))))))

(deftest go-to-already-within-range-does-not-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng p] :as s} (await (go! {:blocks flat} {:pos [1 64 0] :range 1}))]
          (is (true? (:arrived @out)))
          (is (= [0 64 0] (at p)))
          (is (= [] (moved eng))))))))

(deftest go-to-plans-a-walk-once
  ;; a fresh call is quoted first only for a :least other than 1, and go-to's quote counts no units: it was a second plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as r} (await (go! {:blocks flat} {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= 1 (count (events-of r :planned)))))))))

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
        (let [{:keys [out eng p] :as s} (await (go! (assoc gap-up-world :self {:pos {:x 0 :y 64 :z 1}}) {:pos [10 65 1] :ways #{:walk}}))]
          (is (= {:status :stopped :arrived false :reason :unreachable :why :abilities :kind :gap-up :shut-in true} (dissoc @out :at :near :text)))
          (is (re-find #"^gave up walking to \[10 65 1\]: .*step up" (:text @out)) "the reason in words")
          (is (not (re-find #"[{}]" (:text @out))) "no map dump")
          (is (= [0 64 1] (at p)) "the body did not move")
          (is (= ["blocked" "blocked" "blocked"] (mapv :status (moved eng))) "one :moved entry per round"))))))

(deftest an-old-caller-reads-the-result-it-read-before
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[world args want] [[{:blocks flat} {:pos [10 64 0]} {:status :done :arrived true}]
                                   [(assoc gap-up-world :self {:pos {:x 0 :y 64 :z 1}}) {:pos [10 65 1]}
                                    {:status :stopped :arrived false :reason :unreachable :why :abilities :kind :gap-up}]]]
          (let [{:keys [eng]} (setup world)
                out (atom :not-done)
                parent (tu/recording-parent 'jobs.movement.go-to args out)
                eng (assoc eng :jobs (assoc (:jobs eng) 'old-parent parent))]
            (core/submit! eng '(old-parent) {})
            (await (tick-out! eng 30))
            (is (= want (select-keys @out (keys want))) (pr-str @out))))))))

(deftest a-top-level-go-to-that-gives-up-or-refuses-is-stopped-not-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup (assoc gap-up-world :self {:pos {:x 0 :y 64 :z 1}}))
              _ (core/submit! eng '(jobs.movement.go-to {:pos [10 65 1] :ways #{:walk}}) {})
              _ (await (tick-out! eng 30))
              ev (filter #(and (= :job (:source %)) (#{:stopped :completed} (:kind %))) @seen)]
          (is (= [:stopped] (mapv :kind ev)))
          (is (re-find #"^gave up walking to \[10 65 1\]: the body is shut in and needs a step up" (:text (first ev)))))))))

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

(deftest go-to-prefers-a-level-way-to-a-guessed-descent-toward-an-unloaded-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a level plateau runs east to the loaded edge; a slope of one-block steps leads south and down, nearer the far goal
        ;; in a straight line. The slope's foot has no known way back, so the first walk takes the plateau's end.
        (let [slope (apply merge (floor 63 -2 -3 47 3)
                           (for [i (range 5)] (floor (- 62 i) -2 (+ 4 i) 47 (+ 4 i))))
              {:keys [eng]} (await (go! {:blocks slope} {:pos [60 64 40]}))]
          (is (= 64 (:y (:to (first (moved eng))))) (pr-str (map (juxt :status :to) (moved eng)))))))))

(deftest go-to-never-drops-into-a-closed-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the same plateau, but below the drop is a pit (x 11..14) in front of a wall too high to climb out over.
        ;; :ways #{:walk}: the small plateau counts as shut in, and a shut-in body may escalate by dropping in and digging out
        (let [pit (merge (floor -2 -3 10 3) (floor 60 11 -3 14 3) (box 15 60 -3 16 64 3 "stone"))
              {:keys [p out]} (await (go! {:blocks pit} {:pos [120 64 0] :ways #{:walk}}))]
          (is (= {:arrived false :reason :unreachable :why :one-way} (select-keys @out [:arrived :reason :why])))
          (is (= 64 (js/Math.floor (second (at p)))) "still on the plateau")
          (is (< (first (at p)) 11)))))))

(deftest go-to-one-way-closed-takes-no-drop-it-cannot-climb-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cliff (merge (floor -2 -3 10 3) (floor 60 11 -3 47 3))
              {:keys [p]} (await (go! {:blocks cliff} {:pos [120 61 0] :one-way :closed :ways #{:walk :clear-seal}}))]
          (is (= 64 (js/Math.floor (second (at p)))) "still on the plateau"))))))

(deftest go-to-drop-cost-false-takes-no-drop-and-a-number-keeps-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cliff (merge (floor -2 -3 10 3) (floor 60 11 -3 47 3))
              no (await (go! {:blocks cliff} {:pos [120 61 0] :settings {:jobs.lib.cost.planner/drop-cost false} :ways #{:walk :clear-seal}}))
              dear (await (go! {:blocks cliff} {:pos [120 61 0] :settings {:jobs.lib.cost.planner/drop-cost 2} :ways #{:walk :clear-seal}}))]
          (is (= 64 (js/Math.floor (second (at (:p no))))) "no drop: still on the plateau")
          (is (= 61 (js/Math.floor (second (at (:p dear))))) "a dear drop is still the only way"))))))

(deftest go-to-takes-a-four-block-drop-within-the-damage-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cliff (merge (floor -2 -3 10 3) (floor 59 11 -3 47 3))
              plain (await (go! {:blocks cliff} {:pos [120 60 0] :ways #{:walk :clear-seal}}))
              none (await (go! {:blocks cliff} {:pos [120 60 0] :ways #{:walk :clear-seal} :spend {:health 0}}))
              boots (await (go! {:blocks cliff :equipment {:feet {:name "iron_boots" :enchants [{:name "feather_falling" :lvl 4}]}}}
                                {:pos [120 60 0] :ways #{:walk :clear-seal}}))]
          (is (= 60 (js/Math.floor (second (at (:p plain))))) "plain body: 1 hp is within the budget")
          (is (= 64 (js/Math.floor (second (at (:p none))))) ":spend {:health 0}: no 4-block drop")
          (is (= 60 (js/Math.floor (second (at (:p boots))))) "feather falling IV: it drops"))))))

(deftest go-to-drop-cost-false-still-takes-a-one-block-step-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [steps (merge (floor -2 -3 10 3) (floor 62 11 -3 47 3))
              {:keys [p]} (await (go! {:blocks steps} {:pos [120 63 0] :settings {:jobs.lib.cost.planner/drop-cost false} :ways #{:walk :clear-seal}}))]
          (is (= 63 (js/Math.floor (second (at p)))) "a 1-block step-down is no drop"))))))

(defn refusal [args] (expr/problem registry/jobs (list 'jobs.movement.go-to args)))

(deftest the-planner-price-settings-refuse-bad-values
  (doseq [[k bad] [[:jobs.lib.cost.planner/costs [{:swim-h -1} {:nope 1} [1]]]
                   [:jobs.lib.cost.planner/drop-cost [-1 "x" true]]
                   [:jobs.movement.go-to.tuning/goal-danger [:bogus]]
                   [:jobs.movement.go-to.tuning/y-slack [-1]]]
          v bad]
    (is (string? (a/setting-problem k v)) (pr-str k v)))
  (doseq [[k ok] [[:jobs.lib.cost.planner/costs {:swim-h 2}] [:jobs.lib.cost.planner/drop-cost 0] [:jobs.lib.cost.planner/drop-cost false]]]
    (is (nil? (a/setting-problem k ok)) (pr-str k ok))))

(deftest go-to-refuses-a-bad-floor-or-spend-at-submit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[k bad line] [[:floor "x" ":floor"] [:spend -1 ":spend"] [:spend "x" ":spend"]]]
          (is (re-find (re-pattern (str line " must be a map")) (refusal {:pos [6 64 0] k bad})) (pr-str [k bad])))
        (doseq [args [{:floor {:health 1}} {:floor {:health 20}} {:spend {:health 0}} {:spend {:health 5.5}}]]
          (let [{:keys [out]} (await (go! {:blocks flat} (assoc args :pos [6 64 0])))]
            (is (= {:arrived true} @out) (pr-str args))))))))

(deftest go-to-refuses-a-floor-or-leg-out-of-range-before-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{:floor {:health 21}} {:spend {:job-time 0}} {:spend {:job-time 0.05}}]]
          (let [{:keys [out p]} (await (go! {:blocks flat} (assoc args :pos [6 64 0])))]
            (is (= {:status :stopped :reason :bad-args} (select-keys @out [:status :reason])) (pr-str args))
            (is (= :impossible (:kind (:ending (meta @out)))) (pr-str args))
            (is (= [0 64 0] (at p)) (pr-str args))))
        (doseq [args [{:floor {:health 20}} {:spend {:job-time 0.1}} {:spend {:job-time 300}}]]
          (let [{:keys [out]} (await (go! {:blocks flat} (assoc args :pos [6 64 0])))]
            (is (not= :bad-args (:reason @out)) (pr-str args))))))))

(deftest go-to-accepts-doors
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [doors [:never :shut :leave-open]]
          (let [{:keys [out]} (await (go! {:blocks flat} {:pos [6 64 0] :doors doors}))]
            (is (= {:arrived true} @out) (str doors))))))))

(deftest go-to-refuses-malformed-tolls-at-submit
  (is (re-find #":tolls must be a list of \{:x :y :z :factor\}" (refusal {:pos [6 64 0] :tolls [{:x 3 :y 64 :z 0}]}))))

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
          (is (= :impossible (:kind (:ending (meta @out)))))
          (is (= [0 64 0] (at p))))))))

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
            (core/cut! eng (core/holder eng) :test nil)
            (await running))
          (is (= {} (:controls @(fake/state p))) "no control is left pressed")
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
           [{:status :off-plan :at [1 64 0] :step 17 :partial true} {:why :off-plan :detail "walked the nearest known way to its end at step 17, still short of the goal"}]
           [{:status :failed :reason "controls lost"} {:why :steer-failed :detail "controls lost"}]
           [{:status :failed} {:why :steer-failed}]
           [{:status :arrived :at [1 64 0]} {:why :no-progress}]]]
    (is (= expected (end/give-up-fields result)) (pr-str result))))

(defn ^:async go-prepped!
  "go! with (prepare! p) run on the fake primitives before the job starts."
  [world args prepare!]
  (let [{:keys [eng p stop] :as s} (setup world)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out args)))]
    (prepare! p)
    (core/submit! eng '(recording-parent) {})
    (let [ticks (await (tick-out! eng 30))]
      (stop)
      (assoc s :eng eng :out out :ticks ticks))))

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
          (is (= ["blocked" "blocked" "blocked"] (mapv :status (moved eng)))))))))

;; a steer held to the call's seconds that times out with most of them left is stuck, not out of time (that re-walked for ever)
(deftest go-to-whose-steer-times-out-at-once-gives-up-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go-prepped! {:blocks flat} {:pos [10 64 0] :range 0 :spend {:job-time 20}}
                                                #(override-steer! % (fn [_ _ _] (js/Promise.resolve #js {:status "timeout" :pose (pose 0.5 0.5)})))))]
          (is (= {:arrived false :why :stuck} (select-keys @out [:arrived :why])) (pr-str @out)))))))

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
          (is (= {:arrived false :reason :unreachable :why :steer-failed :detail "no controls" :escalation {:step :none :why :no-wall}}
                 (dissoc @out :at :near :text :status)) "flat land: no way to make either"))))))

(deftest go-to-says-no-progress-when-the-walk-ends-no-nearer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (go-prepped! {:blocks flat} {:pos [10 64 0] :range 0}
                                                #(override-steer! % (fn ^:async f [_ _ _] #js {:status "done"}))))]
          (is (= {:arrived false :reason :unreachable :why :no-progress :escalation {:step :none :why :no-wall}} (dissoc @out :at :near :text :status))))))))

(deftest go-to-spend-one-try-ends-a-walk-that-got-no-nearer-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args want] [[{} 3] [{:spend {:tries 2}} 2] [{:spend {:tries 1}} 1]]]
          (let [steers (atom 0)
                {:keys [out]} (await (go-prepped! {:blocks flat} (merge {:pos [10 64 0] :range 0 :ways #{:walk :clear-seal}} args)
                                                  #(override-steer! % (fn ^:async f [_ _ _] (swap! steers inc) #js {:status "done"}))))]
            (is (= want @steers) (str "walks for " args))
            (is (= {:arrived false :reason :unreachable :why :no-progress} (dissoc @out :at :near :text :status :tries))
                (str "same give-up for " args))))))))

(deftest go-to-gives-up-when-a-callers-tries-line-is-used-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [n [1 2]]
          (let [steers (atom 0)
                out (atom :not-done)
                parent (assoc (recording-parent out {:pos [10 64 0] :range 0 :ways #{:walk :clear-seal}}) :defaults {:spend {:tries n}})
                {:keys [eng p]} (setup {:blocks flat :jobs {'recording-parent parent}})]
            (override-steer! p (fn ^:async f [_ _ _] (swap! steers inc) #js {:status "done"}))
            (core/submit! eng '(recording-parent) {})
            (await (tick-out! eng 30))
            (is (= n @steers) (str "the caller's {:tries " n "} caps the walks"))
            (is (= :not-done @out) "go-to is cut off with its caller, which has no tries left")))))))

(deftest go-to-look-round-false-skips-the-arrival-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [watched (fn [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :watched))
              dark (await (go! {:blocks flat :light [0 0]} {:pos [10 64 0] :range 1 :settings {:jobs.movement.go-to.tuning/look-round false}}))]
          (is (= {:arrived true} @(:out dark)))
          (is (empty? (watched dark))))))))

(deftest go-to-walks-to-the-wall-of-a-walled-in-goal-and-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a stone wall at x 5 cuts the floor: the goal's side is walled in
        (let [walled (merge (box -2 63 -2 10 63 4 "stone") (box 5 64 -2 5 65 4 "stone"))
              {:keys [out eng p]} (await (go! {:blocks walled} {:pos [8 64 1] :range 0 :ways #{:walk :clear-seal}}))]
          (is (= {:arrived false :reason :unreachable :why :goal-cut-off} (dissoc @out :at :near :text :status :shut-in)))
          (is (= [3 64 0] (at p)) "the shortest walk among the cells within 1.5 of the nearest the goal it can reach")
          (is (= ["blocked" "arrived" "blocked"] (mapv :status (moved eng))) "no try again after the walk to the wall"))))))

(deftest go-to-walks-to-where-the-loaded-land-runs-on-when-it-holds-no-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a walkway (feet 80) x 18..47 at z 8 over a floor x 0..47, z 0..15, with no way down; from x 48 on nothing is
        ;; loaded (the fake never loads more): the first round walks to the frontier, the rest find no new one
        ;; (:spend {:health 7}: the walkway is a 16-block drop, which a body over its budget would take: go-to-health-test)
        (let [world {:blocks (merge (box 0 63 0 47 63 15 "stone") (box 18 79 8 47 79 8 "stone"))
                     :self {:pos {:x 18.5 :y 80 :z 8.5}}}
              {:keys [out eng p]} (await (go! world {:pos [10 64 8] :range 0 :spend {:health 7}}))]
          (is (= {:arrived false :reason :unreachable :why :exhausted} (select-keys @out [:arrived :reason :why])))
          (is (>= (first (at p)) 46) "walked to the frontier")
          (is (= ["partial" "blocked" "blocked" "blocked"] (mapv :status (moved eng)))
              "the walk to the frontier is progress"))))))

;; the same walkway, the goal in land never loaded: the far side was never seen, so that is the reason, not :exhausted
(deftest go-to-gives-up-goal-unloaded-when-the-loaded-land-is-exhausted
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a floor of 2x2 cells and nothing loaded from z 2 on, the goal in the unloaded land
        (let [world {:blocks (box 0 63 0 1 63 1 "stone")
                     :unloaded (keys (box 0 60 2 1 70 40 "x"))
                     :self {:pos {:x 0.5 :y 64 :z 0.5}}}
              {:keys [out]} (await (go! world {:pos [0 64 30] :range 0}))]
          (is (= {:arrived false :reason :unreachable :why :goal-unloaded} (select-keys @out [:arrived :reason :why]))))))))

;; the same walkway with searches that take several rounds (live: soak j29/j30, a walled walkway at y 100 whose goal lies
;; below its middle). The walk to the frontier is refreshed on the way, and every search from the frontier is unfinished at
;; first: an unfinished search's nearest node (back on the walkway, over the goal) must not win over the frontier, or the
;; body swings between the two and gives up :off-plan
(deftest go-to-keeps-to-the-frontier-when-later-searches-take-several-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [budget wsearch/round-budget
              world {:blocks (merge (box 0 63 0 47 63 15 "stone") (box 18 79 8 47 79 8 "stone"))
                     :self {:pos {:x 18.5 :y 80 :z 8.5}}}
              {:keys [eng p] :as s} (setup world)
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [10 64 8] :range 0 :spend {:health 7}})))]
          (reset! wsearch/searches {})
          (set! wsearch/round-budget 64)
          (core/submit! eng '(recording-parent) {})
          (await (tu/with-expansion-budget 16 #(tick-out! eng 600)))
          (set! wsearch/round-budget budget)
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
        (let [budget wsearch/round-budget
              joined (merge (box 0 63 0 6 63 60 "stone") (box 3 64 0 3 65 58 "stone"))
              {:keys [eng p] :as s} (setup {:blocks joined :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [6 64 0] :range 0})))]
          (reset! wsearch/searches {})
          (set! wsearch/round-budget 32)
          (core/submit! eng '(recording-parent) {})
          (let [plans (atom 0)
                plan-walk wsearch/plan-walk-budgeted!]
            (set! wsearch/plan-walk-budgeted! (fn [& args] (swap! plans inc) (apply plan-walk args)))
            (await (tu/with-expansion-budget 16 #(tick-out! eng 200)))
            (set! wsearch/plan-walk-budgeted! plan-walk)
            (is (> @plans 3) "several rounds searched"))
          (set! wsearch/round-budget budget)
          (is (= {:arrived true} @out))
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
  {:round (fn ^:async returns-round [c]
            (when (and seed (empty? @returns) (nil? (get-in (ctx/mem c) [:children :kid])))
              (ctx/update-mem! c assoc-in [:children :kid] (merge {:args args :children {}} seed)))
            (let [e (await (ctx/call c :kid 'jobs.movement.go-to args))
                  r (if (= :continue e) :continue :done)]
              (swap! returns conj r)
              (when (= :done r) (reset! out (result-of e)))
              (if (= :continue r) r (ctx/end c :done nil))))})

(defn ^:async go-returns!
  "Run go-to with args under returns-parent over world; {:eng :p :seen :out :returns}."
  ([world args] (go-returns! world args nil))
  ([world args seed] (go-returns! world args seed identity))
  ([world args seed prepare!]
   (let [{:keys [eng p stop] :as s} (setup world)
         out (atom :not-done)
         returns (atom [])
         eng (assoc eng :jobs (assoc (:jobs eng) 'returns-parent (returns-parent out returns args seed)))]
     (prepare! p)
     (core/submit! eng '(returns-parent) {})
     (await (tick-out! eng 30))
     (assoc s :eng eng :out out :returns returns))))

(deftest one-go-to-call-walks-until-it-gives-up-with-no-continue-between
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out returns eng]} (await (go-returns! (assoc gap-up-world :self {:pos {:x 0 :y 64 :z 1}})
                                                            {:pos [10 65 1] :ways #{:walk}}))]
          (is (= :unreachable (:reason @out)))
          (is (= ["blocked" "blocked" "blocked"] (mapv :status (moved eng))) "three fruitless walks")
          (is (= [:done] @returns) "all of them in one call"))))))

(deftest one-go-to-call-walks-to-the-frontier-and-on-with-no-continue-between
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks (merge (box 0 63 0 47 63 15 "stone") (box 18 79 8 47 79 8 "stone"))
                     :self {:pos {:x 18.5 :y 80 :z 8.5}}}
              {:keys [out returns eng]} (await (go-returns! world {:pos [10 64 8] :range 0 :spend {:health 7}}))]
          (is (= :exhausted (:why @out)))
          (is (= ["partial" "blocked" "blocked" "blocked"] (mapv :status (moved eng))))
          (is (= [:done] @returns)))))))

(def joined-world
  "A floor x 0..6, z 0..60 cut by a wall at x 3 open only at z 59..60: the way from x 0 to x 6 runs 120 blocks round."
  {:blocks (merge (box 0 63 0 6 63 60 "stone") (box 3 64 0 3 65 58 "stone")) :self {:pos {:x 0.5 :y 64 :z 0.5}}})

(defn ^:async with-small-budget!
  "Run (f) with a search budget of 32 expansions a slice and go-to's pace! counted in paces; restores both."
  [paces f]
  (let [budget wsearch/round-budget
        pace pace/timer]
    (reset! wsearch/searches {})
    (set! wsearch/round-budget 32)
    (set! pace/timer (fn [] (swap! paces inc) (pace)))
    (try (await (tu/with-expansion-budget 16 f))
         (finally (set! wsearch/round-budget budget)
                  (set! pace/timer pace)))))

(deftest one-go-to-call-searches-on-paced-until-its-search-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [paces (atom 0)
              plans (atom 0)
              plan-walk wsearch/plan-walk-budgeted!]
          (set! wsearch/plan-walk-budgeted! (fn [& args] (swap! plans inc) (apply plan-walk args)))
          (let [{:keys [out returns]} (await (with-small-budget! paces #(go-returns! joined-world {:pos [6 64 0] :range 0})))]
            (set! wsearch/plan-walk-budgeted! plan-walk)
            (is (= {:arrived true} @out))
            (is (> @plans 3) "several search slices")
            (is (= [:done] @returns) "searched and walked in one call")
            (is (>= @paces (- @plans 2)) "every walk slice after the first awaited the pace timer (the call's quote searches first, with none)")))))))

(deftest a-fresh-call-whose-quote-found-the-route-plans-it-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [searches (atom 0)
              create-plan planner/create-plan]
          (reset! wsearch/searches {})
          (set! planner/create-plan (fn [& args] (swap! searches inc) (apply create-plan args)))
          (let [{:keys [out]} (try (await (go-returns! {:blocks flat} {:pos [10 64 0] :range 0}))
                                   (finally (set! planner/create-plan create-plan)))]
            (is (= {:arrived true} @out))
            (is (= 1 @searches) "the walk took the plan the quote found")))))))

(deftest a-cut-stops-the-search-loop-and-the-next-call-resumes-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [paces (atom 0)
              plans (atom 0)
              plan-walk wsearch/plan-walk-budgeted!]
          (set! wsearch/plan-walk-budgeted! (fn [& args] (swap! plans inc) (apply plan-walk args)))
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
                   (core/cut! eng (core/holder eng) :test nil)
                   (let [at-cut @plans]
                     (await running)
                     (is (<= @plans (inc at-cut)) "at most the slice in flight ran after the cut")
                     (is (= [] @returns) "the cut call returned nothing")
                     (is (= [] (moved eng)) "it walked nowhere")))
                 (await (tick-out! eng 10))
                 (is (= {:arrived true} @out) (pr-str @out))
                 (is (= [6 64 0] (at p)))
                 (is (= [:done] @returns) "the resumed call arrived in one call")))))
          (set! wsearch/plan-walk-budgeted! plan-walk))))))

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
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [:done] @returns))
          (is (= (solid flat) (solid (:blocks @(fake/state p)))) "no stair child dug")
          (is (= [] (mem/entries (mem/view (:store eng)) :tidy)) "nothing in the dig ledger"))))))

;; ------------------------------------------------------- a goal a reflex keeps cutting the walk to (card 66863dc4)

(def way-zombie
  "A zombie standing still 7.2 blocks from goal [10 64 0], beside the way from the start, out of the hostile-near radius
  of a body cut at x 6."
  {:id 77 :name "zombie" :kind "hostile" :pos [14 64 6]})

(defn cut-seed
  "Memory of an earlier call of this session left open at goal [10 64 0] after cuts, cut 3 blocks from it."
  [m]
  (merge {:open (gcut/session-id) :goal {:x 10 :y 64 :z 0} :last-dist 3} m))

(defn sent-back-spots
  "n send-backs from [5 64 0] (:cut-walks n too), each undoing a share of the worth of a goal 10 blocks off."
  [n share]
  {:cut-walks n :sent-backs (vec (repeat n {:at [5 64 0] :lost-s (* share (cost/tour-worth 10 nil))}))})

(deftest a-new-session-forgets-the-earlier-runs-walks-and-open-calls
  (let [goal {:x 10 :y 64 :z 0}
        m (cut-seed (merge {:closest 3 :ref 3} (sent-back-spots 2 1)))
        before (gcut/session-id)]
    (swap! gcut/approaches assoc 1 {:last-dist 5})
    (gcut/new-session!)
    (is (= {} @gcut/approaches))
    (is (not= before (gcut/session-id)))
    (is (empty? (:sent-backs (gcut/cut-state m goal 10))))))

(deftest a-call-sent-back-for-more-walk-than-its-goal-is-worth-gives-up-cut-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the body is 10 from the goal; it got to 3 before and was sent back twice, each losing the goal's worth: the
        ;; chance of a third (3/4) times the walk lost passes it, and the zombie that sent it back still stands near
        (let [{:keys [out returns eng]} (await (go-returns! {:blocks flat :entities [way-zombie]} {:pos [10 64 0] :range 0}
                                                            (cut-seed (merge {:closest 3 :ref 3} (sent-back-spots 2 1)))))]
          (is (= {:arrived false :reason :unreachable :why :cut-again} (select-keys @out [:arrived :reason :why])) (pr-str @out))
          (is (= "zombie" (:nearest-hostile @out)) (pr-str @out))
          (is (= [14 64 6] (:nearest-hostile-at @out)) (pr-str @out))
          (is (= 3 (:sent-backs @out)) "the two and the one this restart found")
          (is (= [:done] @returns))
          (is (= [] (moved eng)) "it walked nowhere"))
        ;; send-backs that undo less than the worth together, however many: it walks on
        (doseq [[why seed] [["one send-back of the whole worth" (cut-seed (merge {:closest 3 :ref 3} (sent-back-spots 1 1)))]
                            ["four of a tenth" (cut-seed (merge {:closest 3 :ref 3} (sent-back-spots 4 0.1)))]
                            ["the closest approach improved 2 blocks since" (cut-seed (merge {:closest 1 :ref 3} (sent-back-spots 3 1)))]
                            ["another goal in the same slot" (cut-seed (merge {:goal {:x 30 :y 64 :z 0} :closest 3 :ref 3} (sent-back-spots 3 1)))]
                            ["left open by an earlier run of the engine" (cut-seed (merge {:open "old-run" :closest 3 :ref 3} (sent-back-spots 3 1)))]
                            ["the last call ended normally" (dissoc (cut-seed (merge {:closest 3 :ref 3} (sent-back-spots 3 1))) :open)]]]
          (let [{:keys [out returns]} (await (go-returns! {:blocks flat} {:pos [10 64 0] :range 0} seed))]
            (is (= {:arrived true} @out) (str why " " (pr-str @out)))
            (is (= [:done] @returns) why)))
        ;; the caller's worth counts: the same send-backs undo more than a goal worth little is worth
        (let [{:keys [out]} (await (go-returns! {:blocks flat} {:pos [10 64 0] :range 0 :worth 1}
                                                (cut-seed (merge {:closest 3 :ref 3} (sent-back-spots 1 0.1)))))]
          (is (= :cut-again (:why @out)) (pr-str @out)))))))

(defn record-cells!
  "Steer override: every pose's [cell-x cell-z] is pushed to visited, an atom of pairs."
  [p visited]
  (.override (.-world p) "steer"
             (fn [token ^js a impl]
               (let [decide (.-decide a)]
                 (impl token (js/Object.assign #js {} a #js {:decide (fn [pose]
                                                                       (swap! visited conj [(js/Math.floor (.-x pose)) (js/Math.floor (.-z pose))])
                                                                       (decide pose))}))))))

(deftest a-way-sent-back-from-is-dearer-and-the-next-walk-takes-a-fresh-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walked (fn ^:async f [seed]
                       (let [visited (atom #{})
                             {:keys [out]} (await (go-returns! {:blocks flat} {:pos [10 64 0] :range 0} seed #(record-cells! % visited)))]
                         (is (= {:arrived true} @out) (pr-str @out))
                         @visited))
              fresh (await (walked nil))
              sent (await (walked (cut-seed (merge {:closest 3 :ref 3} (sent-back-spots 1 0.5)))))]
          (is (every? (fn [[_ z]] (zero? z)) fresh) "the straight way crosses the cell")
          (is (some #{[5 0]} fresh) "the fresh walk crosses the cell")
          (is (not-any? #(contains? sent %) [[5 -1] [5 0] [5 1]]) "the cell a send-back left the walk at and those beside it cost more: the walk goes round, not one cell aside"))))))

(deftest a-way-sent-back-from-twice-costs-more-than-once-and-than-a-fresh-one
  (let [factor (fn [m] (let [c (gcut/send-back-tolls m)] (:factor (first c))))]
    (is (nil? (seq (gcut/send-back-tolls {}))))
    (is (< (factor (sent-back-spots 1 0.5)) (factor (sent-back-spots 2 0.5))) "the same place again adds its walk lost, and the chance rises")))

(deftest a-sidestep-of-the-way-that-was-cut-does-not-dodge-its-toll
  (let [trail (mapv #(vector % 64 0) (range 2 7))
        m {:cut-walks 1 :sent-backs [(gcut/send-back [6 64 0] 5 trail)]}
        tolls (gcut/send-back-tolls m)
        at (into {} (map (fn [{:keys [x y z factor]}] [[x y z] factor])) tolls)]
    (is (every? at (for [x (range 2 7) y [63 64 65] z [-1 0 1]] [x y z])) "each cell of the way and the cells a planner move from it: beside, one up, one down")
    (is (not-any? at [[4 66 0] [4 62 0] [0 64 0] [8 64 0] [4 64 2]]) "and no cell farther")
    (is (< (js/Math.abs (- (gcut/send-back-price m) (* (cost/walk-step-s) (reduce + (map #(at [% 64 0]) (range 2 7)))))) 1e-9)
        "crossing the whole way costs the send-back price once, not once a cell")))

(deftest a-walk-s-trail-keeps-the-last-cells-only
  (let [m (reduce (fn [m i] (gcut/approach-step m {:x (+ i 0.5) :y 64 :z 0.5} 1000)) {} (range (+ 50 gcut/trail-cells)))]
    (is (= gcut/trail-cells (count (:trail m))))
    (is (= [(+ 49 gcut/trail-cells) 64 0] (peek (:trail m))) "the newest cell last")
    (is (= [50 64 0] (first (:trail m))) "the oldest dropped")))

(deftest the-send-back-chance-is-the-counted-chance-of-sends-in-cut-walks
  (is (= (cost/counted-chance {:k 2 :n 5}) (gcut/send-back-chance {:cut-walks 5 :sent-backs [{} {}]}))))

(deftest cut-state-counts-only-a-restart-sent-back-from-where-the-cut-found-the-body
  (let [goal {:x 10 :y 64 :z 0}
        m {:open (gcut/session-id) :goal goal :closest 4 :ref 4 :last-dist 4 :cut-walks 1 :sent-backs [{:at [1 64 0] :lost-s 1}]
           :prev {:x 6.5 :y 64 :z 0.5} :trail (mapv #(vector % 64 0) (range 2 7))}]
    (doseq [[why m dist cuts] [["in place (a meal, a breath, a fight)" m 4 1]
                               ["knockback of half a block" m 4.5 1]
                               ["in place on a detour, far from the closest" (assoc m :last-dist 12) 12.3 1]
                               ["sent back 10 blocks" m 14 2]
                               ["sent back after the closest improved 3: the count starts again" (assoc m :closest 1 :last-dist 1) 11 1]
                               ["a new goal" (assoc m :goal {:x 30 :y 64 :z 0}) 14 0]
                               ["left open by another run of the engine" (assoc m :open "old-run") 14 0]
                               ["the last call returned" (dissoc m :open) 14 0]]]
      (is (= cuts (count (:sent-backs (gcut/cut-state m goal dist)))) why))
    (let [s (gcut/cut-state (assoc m :closest 1 :last-dist 1) goal 11)]
      (is (= [{:at [6 64 0] :lost-s (* 10 (cost/walk-step-s)) :cells (mapv #(vector % 64 0) (range 2 7))}] (:sent-backs s))
          "the cell left, the walk undone and the cells of the way (as many as blocks undone, the last of the trail), the cleared count aside")
      (is (= 1 (:cut-walks s))))
    (is (= {:goal goal :closest 14 :ref 14 :last-dist 14 :cut-walks 0 :sent-backs []} (gcut/cut-state (dissoc m :open) goal 14)))))

(deftest the-hostiles-near-the-way-are-the-known-ones-within-the-hostile-near-radius-of-it
  ;; the way runs from the start [0 64 0] to [20 64 0]; melee within 8 of it count, ranged within 16, nearest the way first
  (let [mob (fn [id name pos] {:id id :name name :kind "hostile" :pos pos})
        p (tu/fake {:self {:pos start} :blocks flat
                    :entities [(mob 1 "zombie" [10 64 7]) (mob 2 "zombie" [10 64 -9]) (mob 3 "zombie" [26 64 0])
                               (mob 4 "skeleton" [10 64 12]) (mob 5 "skeleton" [10 64 -17])]})]
    (is (= [3 1 4] (mapv #(.-id %) (danger-q/hostiles-near-way p {:x 20 :y 64 :z 0} 8 {:ranged-radius 16}))))))

(deftest air-and-budget-reasons-have-words
  (doseq [why [:air :air-lethal :budget]]
    (is (not (str/includes? (end/give-up-words [10 64 0] {:why why}) "no path (")) (str why)))
  (is (str/includes? (end/give-up-words [10 64 0] {:why :air-lethal}) "drown")))

(deftest a-budget-give-up-near-the-goal-says-walled-in-not-unseen-land
  (are [left part not-part] (let [text (end/give-up-words [10 64 0] {:why :budget :goal-left left})]
                              (and (str/includes? text part) (not (str/includes? text not-part))))
    2 "walled in" "land the body has not seen"
    40 "land the body has not seen" "walled in"))

(deftest a-moved-while-searching-give-up-says-when-the-body-is-in-water
  (is (str/includes? (end/give-up-words [10 64 0] {:why :moved-while-searching :in-water true}) "in water"))
  (is (not (str/includes? (end/give-up-words [10 64 0] {:why :moved-while-searching}) "water"))))

(deftest the-cut-again-text-names-the-nearest-hostile-as-a-guess
  (is (str/includes? (end/give-up-words [10 64 0] {:why :cut-again :nearest-hostile "zombie"}) "nearest hostile: zombie)"))
  (is (str/includes? (end/give-up-words [10 64 0] {:why :cut-again :nearest-hostile "zombie" :nearest-hostile-at [14 64 6]})
                     "nearest hostile: zombie at [14 64 6])"))
  (is (str/includes? (end/give-up-words [10 64 0] {:why :cut-again :nearest-hostile "skeleton" :nearest-hostile-at {:band :near :direction :south-west}})
                     "nearest hostile: skeleton heard near, south-west)"))
  (is (not (str/includes? (end/give-up-words [10 64 0] {:why :cut-again}) "hostile"))))

(defn ^:async cut-cycles!
  "Run go-to with args under returns-parent over world, cut once for each [pred after!] of cuts: the walk is cut at the
  first pose (js) pred holds for, then (after! p) moves the body (a reflex) and the engine is released; then ticked out.
  {:eng :p :seen :out :returns :mems}, mems the go-to's memory and walk approach after each cut."
  [world args cuts]
  (let [{:keys [eng p] :as s} (setup world)
        out (atom :not-done)
        returns (atom [])
        armed (atom nil)
        eng (assoc eng :jobs (assoc (:jobs eng) 'returns-parent (returns-parent out returns args nil)))
        id (atom nil)
        kid-mem (fn [] (merge (mem/job-mem (mem/view (:store eng)) @id [:kid]) (first (vals @gcut/approaches))))]
    (.override (.-world p) "steer"
               (fn [token ^js a impl]
                 (let [decide (.-decide a)
                       wrapped (fn [pose]
                                 (when-let [pred @armed]
                                   (when (pred pose)
                                     (reset! armed nil)
                                     (core/cut! eng (core/holder eng) :test nil)))
                                 (decide pose))]
                   (impl token (js/Object.assign #js {} a #js {:decide wrapped})))))
    (reset! id (core/submit! eng '(returns-parent) {}))
    (let [mems (loop [[[pred after!] & more] cuts mems []]
                 (if-not pred
                   mems
                   (do (reset! armed pred)
                       (await (core/tick! eng))
                       (after! p)
                       (let [m (kid-mem)]
                         (recur more (conj mems m))))))]
      (await (tick-out! eng 30))
      (assoc s :eng eng :out out :returns returns :mems mems))))

(defn send-back! [p] (swap! (fake/state p) assoc-in [:self :pos] [0 64 0]))

(deftest a-goal-a-reflex-keeps-sending-the-body-back-from-gives-up-cut-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the body walks to 4 blocks short of the goal, is cut there and sent back to the start each time (a stationary
        ;; zombie's flee): the walk records how near it got; the goal is worth little, so the second restart gives up
        (let [near-goal [#(>= (.-x %) 6) send-back!]
              {:keys [out returns mems]} (await (cut-cycles! {:blocks flat :entities [way-zombie]} {:pos [10 64 0] :range 0 :worth 1 :settings {:jobs.lib.walk.world/dangers false}} [near-goal near-goal]))]
          (is (<= (:closest (first mems)) 4.5) (str "the cut walk recorded its approach: " (pr-str (first mems))))
          (is (= {:arrived false :reason :unreachable :why :cut-again} (select-keys @out [:arrived :reason :why])) (pr-str @out))
          (is (str/includes? (:text @out) "sent back") (:text @out))
          (is (= [:done] @returns) "the cut calls returned nothing; the third gave up"))))))

(deftest a-goal-sent-back-once-with-walk-to-spare-is-walked-to-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [near-goal [#(>= (.-x %) 6) send-back!]
              {:keys [out returns p]} (await (cut-cycles! {:blocks flat :entities [way-zombie]} {:pos [10 64 0] :range 0} [near-goal]))]
          (is (= {:arrived true} @out) (pr-str @out))
          (is (= [10 64 0] (at p)))
          (is (= [:done] @returns) "the cut call returned nothing; the second arrived"))))))

(deftest a-long-bending-trip-cut-three-times-in-place-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the way to x 6 runs north to z 59 and back: every cut finds the body farther from the goal than its start, but
        ;; no reflex moved it, so none counts
        (let [stay (fn [_])
              {:keys [out p mems]} (await (cut-cycles! joined-world {:pos [6 64 0] :range 0}
                                                       [[#(>= (.-z %) 15) stay] [#(>= (.-z %) 30) stay] [#(>= (.-z %) 45) stay]]))]
          (is (= 3 (count mems)))
          (is (= {:arrived true} @out) (pr-str @out))
          (is (= [6 64 0] (at p))))))))

(deftest a-long-walk-writes-job-memory-a-few-times-not-per-half-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (go! {:blocks flat} {:pos [38 64 0] :range 0}))
              writes (events-of s :memory_written)]
          (is (= {:arrived true} @out) (pr-str @out))
          (is (<= (count writes) 12) (str (count writes) " memory writes on a 38 block walk")))))))

(deftest a-call-that-yields-continue-is-no-longer-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [step go-to/step!
              opens (atom [])
              {:keys [eng]} (setup {:blocks flat})
              eng (assoc eng :jobs (assoc (:jobs eng) 'open-parent
                                          {:round (fn ^:async open-round [c]
                                                    (let [r (await (ctx/call c :kid 'jobs.movement.go-to {:pos [10 64 0] :range 0}))]
                                                      (swap! opens conj (get-in (ctx/mem c) [:children :kid :open] :none))
                                                      (if (= :continue r) r (ctx/end c :done nil))))}))]
          (set! go-to/step! (fn [_ _] (js/Promise.resolve :continue)))
          (try
            (core/submit! eng '(open-parent) {})
            (await (core/tick! eng))
            (finally (set! go-to/step! step)))
          (is (= [:none] @opens) "a :continue return closes the call: the next start is no cut"))))))

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
                                          (some-> (steer/heading (js->clj (.-controls out) :keywordize-keys true) (.-yaw out))
                                                  (- east-yaw) js/Math.abs (< 0.3)))
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

(deftest an-off-plan-walk-that-fell-also-avoids-the-cells-above-where-it-left-the-plan
  (are [result hurt? expected] (= expected (gfaults/fault-cells result hurt?))
    {:status :off-plan :at [1 75 2]} false [[1 75 2]]
    {:status :off-plan :at [1 75 2]} true (mapv (fn [y] [1 y 2]) (range 75 82))
    {:status :off-plan :at [1 75 2] :partial true} true nil
    {:status :stuck :target [3 64 0]} true [[3 64 0]]))

(deftest go-to-routes-round-a-cell-the-walker-got-stuck-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [visited (atom #{})
              {:keys [out] :as s} (await (go-prepped! {:blocks flat} {:pos [10 64 0] :range 0} #(fault-at-east-edge! % visited)))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= 1 (count (events-of s :walker-fault))) "the walker's fault stays visible as a warn")
          (is (every? (fn [[_ z]] (<= -1 z 1)) @visited) "a short detour, not a wide one"))))))

(defn east-stall-at-start!
  "Steer override: heading east out of the cell [0 64 0] the body makes no headway (a fault of that cell's own way out);
  a diagonal or any other step goes."
  [p]
  (.override (.-world p) "steer"
             (fn [token ^js a impl]
               (let [decide (.-decide a)
                     wrapped (fn [pose]
                               (let [^js out (decide pose)]
                                 (if (and (not (.-done out)) (= [0 0] [(js/Math.floor (.-x pose)) (js/Math.floor (.-z pose))])
                                          (some-> (steer/heading (js->clj (.-controls out) :keywordize-keys true) (.-yaw out))
                                                  (- east-yaw) js/Math.abs (< 0.3)))
                                   #js {:controls #js {} :yaw (.-yaw out)}
                                   out)))]
                 (impl token (js/Object.assign #js {} a #js {:decide wrapped}))))))

(deftest a-later-go-to-takes-another-way-out-of-a-start-cell-whose-cheap-step-stalled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:blocks flat})
              home (get-in @(fake/state p) [:self :pos])
              go (fn ^:async go []
                   (let [out (atom :not-done)
                         eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [6 64 0] :range 0})))]
                     (core/submit! eng '(recording-parent) {})
                     (await (tick-out! eng 30))
                     @out))]
          (east-stall-at-start! p)
          (is (true? (:arrived (await (go)))))
          (is (= 1 (count (events-of s :walker-fault))) "the first call stalls once on the walk east")
          (swap! (fake/state p) assoc-in [:self :pos] home)
          (is (true? (:arrived (await (go)))))
          (is (= 1 (count (events-of s :walker-fault))) "the second call remembers the stall: it leaves by the dearer diagonal"))))))

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
        pw (wworld/path-world p)
        avoided (wworld/with-avoid pw #{[5 64 0] [6 64 1]})
        same (wworld/with-avoid pw #{[6 64 1] [5 64 0]})
        other (wworld/with-avoid pw #{[5 64 0]})]
    (is (= (wworld/avoid-key avoided) (wworld/avoid-key same)))
    (is (not= (wworld/avoid-key avoided) (wworld/avoid-key other)))
    (is (nil? (wworld/avoid-key (wworld/with-avoid pw #{}))) "no cells: the key of a plain search")
    (is (= (wworld/avoid-key avoided) (wworld/avoid-key (wworld/with-walls avoided [[1 64 1]]))) "walls keep the avoided cells")
    (is (= 2 (.-size (.-cells (.-avoid (wplan/plan-options avoided 1 nil nil))))))
    (is (nil? (.-avoid (wplan/plan-options pw 1 nil nil))))))

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
        (is (re-find #":place must be a keyword or a string, got 5" (refusal {:place 5})))
        (doseq [[place reason] [[:nowhere :unknown-place] ["Bad Name" :unknown-place] ["" :bad-name]
                                      [(apply str (repeat 101 "a")) :bad-name]]]
          (let [{:keys [out p]} (await (go-place! {:home {:x 10 :y 64 :z 0}} {:place place}))]
            (is (= reason (:reason @out)) (str place))
            (is (= :stopped (:status @out)))
            (is (re-find #"place" (:text @out)))
            (is (= [0 64 0] (at p)) "did not walk")))))))

(deftest go-to-plans-with-dangers-unless-told-not-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args want] [[{} true] [{:settings {:jobs.lib.walk.world/dangers false}} false]]]
          (let [planned (atom 0)]
            (with-redefs [threats/planner-dangers (fn [_] (swap! planned inc) nil)]
              (await (go! {:blocks flat} (assoc args :pos [10 64 0]))))
            (is (= want (pos? @planned)) (str "args " args))))))))

;; a go-to walk reads the flee ways waiting for their cells, whatever job asked for the walk
(deftest go-to-reads-a-waiting-flee-way-once-its-cells-are-in-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as s} (setup {:blocks flat :light [15 0] :seen :all})
              walk-to! (fn ^:async walk-to! [x known]
                         (let [e (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent (atom nil) {:pos [x 64 0]})))]
                           (with-redefs [wworld/memory-sections (constantly known)]
                             (core/submit! e '(recording-parent) {})
                             (await (tick-out! e 30 nil)))))
              rock-east (wt/known-of (fn [[x y z]] (not (and (= z 0) (#{64 65} y) (<= -12 x 8)))) {} [-12 60 -8 12 68 8])]
          (mem/write! (:store eng) :way-pending {:way [8 64 0 0]} {:cap 64 :ttl :forever})
          (await (walk-to! 3 wt/open-corridor))
          (is (= [] (events-of s :way-on.seen)) "the way's cells are not seen yet")
          (await (walk-to! 6 rock-east))
          (is (= [{:way [8 64 0 0] :on false}] (mapv #(select-keys % [:way :on]) (events-of s :way-on.seen)))
              "seen as rock: read by the walk"))))))

(deftest go-to-walks-the-whole-way-whatever-the-callers-legs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! {:blocks flat} {:pos [35 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= 35 (first (at p)))))))))

;; a hostile first sensed once the walk is under way (the body faced away at the start, or it came out of the dark): when it
;; lies within its danger radius of the way still ahead the walk plans again once, so the plan is costed with it
(deftest go-to-plans-again-once-for-a-hostile-first-sensed-near-the-way-ahead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [mob {:key "z1" :name "zombie" :pos {:x 20 :y 64 :z 1}}
              s (with-redefs [threats/sensed-mobs (fn [p _] (if (> (first (at p)) 5) [mob] []))]
                  (await (go! {:blocks flat} {:pos [35 64 0] :range 0})))]
          (is (= {:arrived true} @(:out s)))
          (is (= [:danger] (mapv :why (events-of s :replan))) "one replan, for the danger"))))))

(deftest go-to-does-not-plan-again-for-a-hostile-far-from-the-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [mob {:key "z1" :name "zombie" :pos {:x 20 :y 64 :z 30}}
              s (with-redefs [threats/sensed-mobs (fn [p _] (if (> (first (at p)) 5) [mob] []))]
                  (await (go! {:blocks flat} {:pos [35 64 0] :range 0})))]
          (is (= {:arrived true} @(:out s)))
          (is (= [] (events-of s :replan))))))))

(deftest go-to-plans-again-for-each-newly-sensed-hostile-only-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [mob (fn [x] {:key "z1" :name "zombie" :pos {:x x :y 64 :z 1}})
              s (with-redefs [threats/sensed-mobs (fn [p _] (let [bx (first (at p))]
                                                            (cond (> bx 20) [] (> bx 5) [(mob (+ 10 (mod (js/Math.floor bx) 3)))] :else [])))]
                  (await (go! {:blocks flat} {:pos [35 64 0] :range 0})))]
          (is (= {:arrived true} @(:out s)))
          (is (= [:danger] (mapv :why (events-of s :replan))) "a wandering, flickering mob is one mob"))))))

(defn gap-strip
  "Stone strips along x at y 63, z 0: x 0-4 and x (5+n)-(9+n), an n-wide gap; a floor 5 below."
  [n]
  (merge (box 0 63 0 4 63 0 "stone") (box (+ 5 n) 63 0 (+ 9 n) 63 0 "stone") (box -2 58 -3 20 58 3 "stone")))

(deftest go-to-does-not-jump-a-gap-at-food-6-or-less
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[n food] [[2 0] [2 6] [3 0] [3 6]]]
          (let [{:keys [out p]} (await (go! {:blocks (gap-strip n) :self {:pos {:x 0.5 :y 64 :z 0.5} :food food}}
                                            {:pos [(+ 7 n) 64 0] :range 0 :ways #{:walk :clear-seal}}))]
            (is (false? (:arrived @out)) (str "gap " n " food " food " " (pr-str @out)))
            ;; (the jump over the 5-deep floor may miss for 2 hp: a starving body's budget is 0, so the planner offers no jump)
            (is (= :unreachable (:reason @out)) (str "gap " n " food " food " " (pr-str @out)))
            (is (< (first (at p)) 5) (str "gap " n " food " food " at " (at p)))))))))

(deftest go-to-does-not-jump-a-gap-over-a-4-deep-pit-at-food-0
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [n [2 3]]
          (let [blocks (merge (box 3 59 -6 26 59 6 "stone") (box 4 63 0 8 63 0 "stone") (box (+ 9 n) 63 0 (+ 13 n) 63 0 "stone"))
                {:keys [out p]} (await (go! {:blocks blocks :self {:pos {:x 6.5 :y 64 :z 0.5} :food 0}}
                                            {:pos [(+ 10 n) 64 0] :range 0 :ways #{:walk :clear-seal}}))]
            (is (false? (:arrived @out)) (str "gap " n " " (pr-str @out)))
            (is (< (first (at p)) 9) (str "gap " n " at " (at p)))))))))

(deftest the-planned-event-says-whether-the-plan-may-sprint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[food sprint] [[0 false] [6 false] [7 true] [20 true]]]
          (let [s (await (go! {:blocks flat :self {:pos {:x 0.5 :y 64 :z 0.5} :food food}} {:pos [5 64 0] :range 0}))]
            (is (= [sprint] (distinct (map :sprint (events-of s :planned)))) (str "food " food))))))))

(defn ^:async go-marker!
  "Run go-to over flat with the places in memory and a shared places.json holding markers; {:out :p :seen}."
  [places markers args]
  (let [dir (tu/tmp-dir)
        file (path/join dir "places.json")
        _ (fs/writeFileSync file (js/JSON.stringify (clj->js markers)))
        w (world-files/open {:plans-dir (path/join dir "plans") :blueprint-dir (path/join dir "bps")
                             :zones-file (path/join dir "zones.edn")
                             :emit (fn [_])})
        {:keys [eng] :as s} (setup {:blocks flat} w)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out args)))]
    (doseq [[k pos] places] (mem/write! (:store eng) k {:pos pos} mem/place-policy))
    (core/submit! eng '(recording-parent) {})
    (await (tick-out! eng 30))
    (assoc s :out out)))

(def hut {:name "hut" :kind "base" :x 10 :y 64 :z 0})

(deftest go-to-walks-to-a-shared-marker-by-name
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go-marker! {} [hut] {:place :hut :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [10 64 0] (at p))))))))

(deftest go-to-prefers-the-bodys-own-place-over-a-shared-marker
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go-marker! {:hut {:x 5 :y 64 :z 0}} [hut] {:place :hut :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [5 64 0] (at p))))))))

(deftest go-to-refuses-a-name-neither-memory-nor-markers-know
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go-marker! {} [hut] {:place :mine}))]
          (is (= :unknown-place (:reason @out)))
          (is (re-find #"shared" (:text @out)))
          (is (= [0 64 0] (at p))))))))

(def north-farm {:name "North Farm" :kind "farm" :x 10 :y 64 :z 0})

(deftest go-to-reaches-a-shared-marker-with-free-text-name
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [place ["North Farm" "north farm" "NORTH FARM"]]
          (let [{:keys [out p]} (await (go-marker! {} [north-farm] {:place place :range 0}))]
            (is (= {:arrived true} @out) place)
            (is (= [10 64 0] (at p)))))))))

(deftest go-to-matches-a-shared-marker-name-exactly-before-any-case
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ms [{:name "hut" :x 5 :y 64 :z 0} {:name "Hut" :x 10 :y 64 :z 0}]
              {:keys [out p]} (await (go-marker! {} ms {:place "Hut" :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [10 64 0] (at p))))))))

;; a goal inside the full-cost zone of a known mob the body would flee: :goal-danger :wait (default) walks on and says so
;; once; :end gives up at once with :why :goal-dangerous
(def goal-zombie {:key "z1" :name "zombie" :pos {:x 11 :y 64 :z 1}})

(deftest goal-danger-end-gives-up-at-once-naming-the-mob
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (with-redefs [threats/sensed-mobs (constantly [goal-zombie])]
                  (await (go! {:blocks flat} {:pos [10 64 0] :range 0 :settings {:jobs.movement.go-to.tuning/goal-danger :end}})))]
          (is (= {:arrived false :reason :unreachable :why :goal-dangerous} (select-keys @(:out s) [:arrived :reason :why])) (pr-str @(:out s)))
          (is (= "zombie" (:mob @(:out s))))
          (is (str/includes? (:text @(:out s)) "zombie") (:text @(:out s)))
          (is (= [0 64 0] (at (:p s))) "the body did not walk"))))))

(deftest goal-danger-wait-is-the-default-and-walks-on-with-an-info
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{:pos [10 64 0] :range 0} {:pos [10 64 0] :range 0 :settings {:jobs.movement.go-to.tuning/goal-danger :wait}}]]
          (let [s (with-redefs [threats/sensed-mobs (constantly [goal-zombie])]
                    (await (go! {:blocks flat} args)))]
            (is (= {:arrived true} @(:out s)) (pr-str args))
            (is (= 1 (count (events-of s :goal-in-danger))))))))))

(deftest goal-danger-end-ignores-a-faded-sighting-and-keeps-a-fresh-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[weight arrived] [[1 false] [1e-5 true]]]
          (let [s (with-redefs [threats/known-threats (constantly [(assoc goal-zombie :mob "zombie" :weight weight :remembered true :sighted true)])]
                    (await (go! {:blocks flat} {:pos [10 64 0] :range 0 :settings {:jobs.movement.go-to.tuning/goal-danger :end}})))]
            (is (= arrived (:arrived @(:out s))) (str "weight " weight (pr-str @(:out s))))))))))

;; the goal's danger is a price: the toll over the stay against what giving up loses. A goal worth more than a fresh
;; zombie's toll over the stay is walked to; a toll under 1 s a second over an hour's stay costs more than a tour's worth.
;; The old 1 s a second cut gave the first up and walked the second.
(deftest goal-danger-end-weighs-the-toll-over-the-stay-against-the-goal-s-worth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [light (/ 0.5 (cost/threat-factor {:health 20 :equipment nil :weapon nil} "zombie" 1))]
          (doseq [[weight args arrived] [[1 {:worth 1e9} true] [light {:stay-s 3600} false] [light {} true] [1 {} false] [1 {:stay-s 0} true]]]
            (let [s (with-redefs [threats/known-threats (constantly [(assoc goal-zombie :mob "zombie" :weight weight)])]
                      (await (go! {:blocks flat} (merge {:pos [10 64 0] :range 0 :settings {:jobs.movement.go-to.tuning/goal-danger :end}} args))))]
              (is (= arrived (:arrived @(:out s))) (str "weight " weight " " (pr-str args) (pr-str @(:out s)))))))))))

(deftest goal-danger-reads-a-remembered-creeper-spot-only-while-the-client-lists-the-creeper
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[entities arrived] [[[] true]
                                    [[{:id 9 :uuid "u9" :name "creeper" :kind "hostile" :visible false :pos {:x 11 :y 64 :z 1}}] false]]]
          (let [{:keys [eng] :as s} (setup {:blocks flat :entities entities})
                out (atom :not-done)
                eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [10 64 0] :range 0 :settings {:jobs.movement.go-to.tuning/goal-danger :end}})))
                _ (mem/write! (:store eng) :threat {:mob "creeper" :id 9 :uuid "u9" :key "u9" :pos {:x 11 :y 64 :z 1} :ended 1} threats/threat-policy)]
            (core/submit! eng '(recording-parent) {})
            (await (tick-out! eng 30))
            (is (= arrived (:arrived @out)) (pr-str entities @out))))))))

(deftest goal-danger-end-ignores-a-mob-far-from-the-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (assoc goal-zombie :pos {:x 11 :y 64 :z 30})
              s (with-redefs [threats/sensed-mobs (constantly [far])]
                  (await (go! {:blocks flat} {:pos [10 64 0] :range 0 :settings {:jobs.movement.go-to.tuning/goal-danger :end}})))]
          (is (= {:arrived true} @(:out s))))))))

(deftest approach-step-notes-a-teleport-between-two-ticks-not-a-walk-or-a-knockback
  (let [at (fn [x y z] {:x x :y y :z z})
        step (fn [m pose] (gcut/approach-step m pose 10))]
    (is (not (:jumped (-> {} (step (at 0 64 0)) (step (at 0.3 64 0)) (step (at 2 64 0.5))))) "a sprint or knockback")
    (is (not (:jumped (-> {} (step (at 0 64 0)) (step (at 0 58 0))))) "a fall of 6")
    (is (not (:jumped (-> {} (step (at 0 64 0)) (step (assoc (at 0 50 0) :vy -3.5))))) "a fall of 14 in a lagged sample")
    (is (:jumped (-> {} (step (at 0 64 0)) (step (assoc (at 0 50 0) :vy 0)))) "a drop of 14 at rest is a teleport")
    (is (:jumped (-> {} (step (at 0 64 0)) (step (assoc (at 51 50 0) :vy -3.5)))) "a fall does not carry 51 blocks along")
    (is (:jumped (-> {} (step (at 0 64 0)) (step (at 51 64 0)))) "a teleport")
    (is (:jumped (-> {} (step (at 0 64 0)) (step (at 51 64 0)) (step (at 51.2 64 0)))) "kept until the call restarts")))

(deftest a-body-teleported-mid-walk-ends-the-walk-at-once-and-the-call-starts-again-from-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:blocks flat})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [38 64 0] :range 0})))
              jumped (atom false)
              after (atom 0)
              booked (atom [])]
          ;; the fake steer keeps its own body, so the jump is shown to decide as an offset of this steer's poses, and set in the
          ;; world for the steers after it
          (.override (.-world p) "steer"
                     (fn [token ^js a impl]
                       (let [decide (.-decide a)
                             off (atom 0)
                             jump! (fn []
                                     (reset! jumped true)
                                     (reset! off 14)
                                     (swap! (fake/state p) assoc-in [:self :pos] [20 64 0]))
                             wrapped (fn [pose]
                                       (swap! after + (quot @off 14))
                                       ((get {[true false] jump!} [(>= (.-x pose) 6) @jumped] identity))
                                       (decide (js/Object.assign #js {} pose #js {:x (+ (.-x pose) @off)})))]
                       (impl token (js/Object.assign #js {} a #js {:decide wrapped})))))
          (with-redefs [ctx/note-walk! (fn [_ status _] (swap! booked conj status))]
            (core/submit! eng '(recording-parent) {})
            (await (tick-out! eng 30 (:step s))))
          ((:stop s))
          (is (not-any? #{"blocked" "partial"} @booked) (str "the jumped walk is no walk to book: " @booked))
          (is (= 1 (count (events-of s :go-to.jumped))) "the restart is announced once")
          (is (zero? @after) (str "the walk ended on the jump pose itself; poses after it: " @after))
          (is (= {:arrived true} @out) (pr-str @out))
          (is (= [38 64 0] (at p))))))))

(deftest a-call-left-walking-writes-nothing-into-a-new-call-under-its-job-id
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stale (setup {:blocks flat :self {:pos [38 64 0]}})
              stale-eng (assoc (:eng stale) :jobs (assoc (:jobs (:eng stale)) 'recording-parent (recording-parent (atom nil) {:pos [0 64 0] :range 0})))
              {:keys [eng p] :as s} (setup {:blocks flat})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [30 64 0] :range 0})))]
          ;; the left round's walk goes on 30 blocks off, under the same job id j1/kid
          (core/submit! stale-eng '(recording-parent) {})
          (let [left (core/tick! stale-eng)]
            (core/submit! eng '(recording-parent) {})
            (await (tick-out! eng 30))
            (await left))
          (is (empty? (events-of s :go-to.jumped)) "the other call's poses are no jump of this body")
          (is (= {:arrived true} @out) (pr-str @out))
          (is (= [30 64 0] (at p))))))))

(deftest a-body-moved-far-on-every-walk-from-the-same-place-gives-up-once-the-moves-cost-more-than-the-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:blocks flat :self {:pos [20 64 0]}})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [38 64 0] :range 0})))]
          ;; every walk's second pose lies 14 blocks off its first, and the body stays where it was
          (.override (.-world p) "steer"
                     (fn [token ^js a impl]
                       (let [decide (.-decide a)
                             poses (atom 0)
                             wrapped (fn [pose]
                                       (decide (js/Object.assign #js {} pose #js {:x (+ (.-x pose) (if (< 1 (swap! poses inc)) 14 0))})))]
                         (impl token (js/Object.assign #js {} a #js {:decide wrapped})))))
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 30 (:step s)))
          ((:stop s))
          (is (false? (:arrived @out)) (pr-str @out))
          (is (= :cut-again (:why @out)) (pr-str @out))
          (let [lost (* 14 (cost/walk-step-s))
                worth (cost/tour-worth 18 nil)
                price (fn [n] (gcut/send-back-price {:cut-walks n :sent-backs (vec (repeat (dec n) {:lost-s lost}))}))
                expected (first (filter #(> (price %) worth) (iterate inc 2)))]
            (is (= expected (count (events-of s :go-to.jumped))) "one restart per jump, the last the first whose repeats cost more than the goal is worth")))))))
