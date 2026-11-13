(ns engine.go-to-escalate-c-test
  "jobs.movement.go-to escalation (pillar, stair, dig route, a walk to a wall) and jobs.lib.escape, against the fake
  world."
  (:require [clojure.string :as str]
            [jobs.lib.access.rules :as rules]
            [jobs.movement.go-to.search-bound :as search-bound]
            [engine.contract :as contract]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-test :as gt]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.rays :as rays]
            [engine.perception.store :as store]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.value :as value]
            [jobs.lib.threat-field :as threat-field]
            [jobs.lib.need :as need]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.blocks :as b]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.seen-sections :as seen-sections]
            [jobs.lib.util :as u]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.way :as escape-way]
            [jobs.lib.look :as look]
            [jobs.lib.tools :as tools]
            [jobs.lib.near :as near]
            [jobs.movement.go-to.climb-route :as climb-route]
            [jobs.movement.go-to.cut :as gcut]
            [jobs.lib.ledger :as ledger]
            [plan.shape :as shape]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.restore :as restore]
            [jobs.movement.go-to.result :as gend]
            [jobs.lib.tidy :as tidy]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [triggers.access.scaffold-left :as scaffold-left]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]
            [engine.go-to-escalate-support :refer [setup recording-parent tick-out! run-job! feet block events-of calls pit-cells pit stone-pit in-pit room in-room hollow ba ledge-room on-ledge wide-pit plank-yard holes dug-cells foreign trespass deep-pit stone-room run-in-shelter! ledger restores-after-the-last-stair? deep-shaft own-pit-wall cobble-room room-and-deck pocket free-by-pocket high-pocket moat dark-at gap-tunnel long-loop loop-tolls stair-below-surface cave below-pocket dig-route-run!]]))

(deftest go-to-escalates-on-a-door-that-doors-never-refuses
  (let [ok? (fn [result doors] (choose/escalate-reason? result doors))]
    (is (true? (ok? {:reason :exhausted} :shut)))
    (is (true? (ok? {:reason :start-enclosed} :shut)) "a pen sealed in the loaded world, goal far and unloaded (card 5a822a99)")
    (is (true? (ok? {:reason :goal-unloaded} :shut)) "a shut-in body's search that ended at a loaded edge beyond its walls (escalate? also requires it shut in)")
    (is (true? (ok? {:reason :abilities :kind :open} :never)) "the planner's way through a door, refused as a wall")
    (is (false? (ok? {:reason :abilities :kind :open} :shut)))
    (is (false? (ok? {:reason :abilities :kind :break} :never)))
    (is (false? (ok? {:reason :stuck} :never)))))

(deftest a-one-way-end-at-a-refused-drop-is-dug-down-but-a-closed-door-one-is-not
  (is (true? (boolean (choose/refused-drop? {:reason :one-way :one-way {:kind :drop :at [0 70 0]}}))))
  (is (true? (choose/damage-refused? {:status :no-path :reason :damage-budget :hp-cost 10 :hp-budget 7})))
  (is (false? (choose/damage-refused? {:status :no-path :reason :needs-health})))
  (is (false? (boolean (choose/refused-drop? {:reason :one-way :one-way {:kind :door}}))))
  (is (false? (boolean (choose/refused-drop? {:reason :exhausted :one-way {:kind :drop}})))))

;; a free body (a wide floor) across a 5-wide chasm from a level goal: the only walk drops 13 (10 hp, over the 7 a
;; body at full health may spend) and climbs the far wall's stair; the dig down the near face is priced against the call's worth
(def level-chasm
  (merge (tu/floor -40 -20 0 20) (box -6 50 -20 0 62 20 "stone") (box 1 50 -20 5 50 20 "stone") (box 6 50 -20 20 63 20 "stone")
         (into {} (for [k (range 1 14)] [(str "5," (+ 50 k) "," k) "stone"]))))

(deftest a-level-goal-across-a-damaging-drop-is-dug-to-by-price-never-dropped-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:pos {:x -0.5 :y 64 :z 0.5} :health 20 :food 20} :blocks level-chasm :inventory [{:name "stone_pickaxe" :count 1}]}
              {rich :out rp :p rseen :seen} (await (run-job! world {:pos [10 64 0] :range 0}))
              {poor :out pp :p seen :seen} (await (run-job! world {:pos [10 64 0] :range 0 :worth 5}))]
          (is (true? (:arrived @rich)) (pr-str @rich))
          (is (= 20 (.-health (.self rp))) "no drop hurt the body")
          (is (= [[:dig-route :damage-budget]] (map (juxt :step :why) (events-of rseen :go-to.escalated))) "the walk ended :damage-budget: a stair is dug down the near face")
          (is (false? (:arrived @poor)) (pr-str @poor))
          (is (= [0 64] (pop (feet pp))) "a way dearer than the goal is worth is neither dug nor dropped")
          (is (= 20 (.-health (.self pp))))
          (is (empty? (events-of seen :go-to.escalated)) (pr-str @poor)))))))

(deftest an-exhausted-search-above-a-goal-deeper-than-a-survivable-fall-is-dug-down
  (let [p (tu/fake {})
        ex {:reason :exhausted}
        pos {:x 6 :y 0 :z 0}
        stranded? (fn [max-damage feet result] (choose/drop-stranded? {:primitives p :args {:max-damage max-damage}} feet pos result))]
    (is (true? (stranded? 0 [0 13 0] ex)) "the fall to the goal costs hp the call allows none of")
    (is (true? (stranded? 0 [0 13 0] {:reason :unseen-floors})) "every floor felt for at the edges turned out not there")
    (is (false? (stranded? 20 [0 6 0] ex)) "a fall the walk's damage budget pays is no stranding")
    (is (true? (stranded? 20 [0 13 0] ex)) "a fall the body survives but the walk's damage budget (min-health) refuses")
    (is (false? (stranded? 0 [0 13 0] {:reason :budget})))
    (is (false? (stranded? 0 [0 -5 0] ex)) "a goal above is no stair down")))

(deftest a-goal-deeper-than-the-planners-max-drop-is-stranded-whatever-the-floor
  (settings/with-settings {:jobs.lib.cost.health/margin 0}
    #(let [c {:primitives (tu/fake {:self {:health 10 :food 20}}) :args {:min-health 0}}]
       (is (= 12 (:max-drop (wworld/body-policy c))) "the planner searches no fall past 12 at 10 hp")
       (is (true? (choose/drop-stranded? c [0 13 0] {:x 6 :y 0 :z 0} {:reason :exhausted})) "a 13 fall it never searched"))))

(deftest escape-enclosed-counts-doors-as-walls-under-doors-never
  (let [wide (merge room (box -20 63 -20 30 63 20 "stone"))
        p (tu/fake (merge in-room {:blocks (assoc wide "3,64,0" "oak_door" "3,65,0" "oak_door")}))]
    (is (false? (escape/enclosed? p :shut :ground)) "a wooden door is a way out")
    (is (true? (escape/enclosed? p :never :ground)) "doors :never: the room is shut")))

;; ------------------------------------------------------------------ other headings (card 4ad29ed7)
(deftest escape-choose-skips-only-the-refused-stair-heading
  (let [p (tu/fake (merge in-pit {:blocks pit}))
        first-heading (:heading (escape-way/choose p [0 61 0] [10 64 0]))
        again (escape-way/choose p [0 61 0] [10 64 0] (constantly true) {:skip #{[:stair first-heading]}})]
    (is (= :east first-heading))
    (is (= :stair (:step again)))
    (is (not= first-heading (:heading again)))))

(deftest go-to-reads-a-room-shut-only-over-walls-it-has-seen
  (let [wide (merge room (box -20 63 -20 30 63 20 "stone"))
        p (tu/fake (merge in-room {:blocks (assoc wide "3,64,0" "oak_door" "3,65,0" "oak_door")}))
        door? #{[3 64 0] [3 65 0]}
        shut? (fn [args] (escape/walk-enclosed? {:primitives p :args (assoc args :doors :never)}))]
    (aset p "sensedAt" (fn [pos] (if (door? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    (is (false? (shut? {})) "a door cell never seen may be a gap: not shut in")
    (is (true? (shut? {:unknown false})) "a walk planned over the world as it is reads the door as it is")))

(deftest go-to-climbs-out-of-a-pit-by-another-heading-when-the-first-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lava-pit (assoc pit "2,63,1" "lava")
              {:keys [out seen]} (await (run-job! (merge in-pit {:blocks lava-pit}) {:pos [10 64 0] :range 1}))]
          (is (true? (:arrived @out)))
          (is (< 1 (count (events-of seen :go-to.escalated))) "the first heading was refused, another was tried"))))))

(deftest go-to-escalates-from-the-free-part-of-a-shut-iron-door-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [iron (assoc room "3,64,0" "iron_door" "3,65,0" "iron_door")
              states {"3,64,0" {:open false :half "lower" :facing "west"} "3,65,0" {:open false :half "upper" :facing "west"}}
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 3.3 :y 64 :z 0.5}} :blocks iron :states states
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [8 64 0] :range 1}))]
          (is (seq (events-of seen :go-to.escalated)) "a body shut in by the panel escalates")
          (is (= {:arrived true} @out))
          (is (< 6 (first (feet p)))))))))

;; ------------------------------------------------------------------ rounds follow-ups (card be8f7a88)
(deftest a-put-back-whose-place-fails-its-condition-is-skipped-and-stays-in-the-ledger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [place (get registry/jobs 'jobs.blocks.place)
              flaky (assoc place :can-start? (constantly (contract/later :flaky "flaky")))
              {:keys [out p seen eng]} (with-redefs [registry/jobs (assoc registry/jobs 'jobs.blocks.place flaky)]
                                         (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})
                                                          'jobs.movement.go-to (merge {:pos [10 64 0] :range 1 :ways #{:walk :stair}} trespass) foreign)))
              skipped (:cells (first (events-of seen :go-to.restore-skipped)))]
          (is (= {:arrived true} @out))
          (is (= #{:flaky} (set (map :why skipped))) "a skip with the place's cause")
          (is (= [[1 62 0] [1 63 0] [2 63 0]] (sort (mapv :cell skipped))) "every hole is left for restore-broken")
          (is (= "air" (block p [1 62 0])) "nothing was put back"))))))

(deftest go-to-escalates-on-a-goal-cut-off-as-on-a-walled-in-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge {:self {:pos {:x 5 :y 64 :z 0}}} {:blocks room-and-deck})
              {:keys [out seen]} (await (run-job! world {:pos [8 71 0] :range 0}))
              {off :out} (await (run-job! world {:pos [8 71 0] :range 0 :ways #{:walk :clear-seal} :spend {:health 0}}))]
          (is (= :goal-cut-off (:why @off)) "without escalation the planner's verdict is cut-off")
          (is (seq (events-of seen :go-to.escalated)) "shut in: it escalates")
          (is (= :unreachable (:reason @out))))))))

(deftest a-free-body-digs-into-a-walled-in-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! free-by-pocket {:pos [8 64 0] :range 0}))]
          (is (true? (:arrived @out)) "not shut in, but the goal is: it digs to it")
          (is (= [8 64 0] (feet p)))
          (is (some #{:dig-route} (map :step (events-of seen :go-to.escalated)))))))))

(deftest a-free-body-digs-down-to-a-walled-in-goal-below-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [below (apply dissoc (box -20 55 -20 20 63 20 "stone") ["3,60,0" "3,61,0"])
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks below
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [3 60 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [3 60 0] (feet p)))
          (is (= [:dig-route] (map :step (events-of seen :go-to.escalated))) "no door or stair leads down: the fewest cells are dug"))))))

(deftest a-wall-nearby-does-not-lead-into-a-goal-below
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (assoc below-pocket "5,64,3" "stone")
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [3 60 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [:dig-route] (map :step (events-of seen :go-to.escalated))) "a walk to a wall cuts no way down"))))))

(deftest a-body-on-a-plateau-it-cannot-walk-off-digs-down-to-a-walled-in-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; 29x29 top 6 above a floor: fewer cells than reach-room/room-cells, so the body counts as shut in
        (let [blocks (apply dissoc (merge (box -40 57 -40 40 57 40 "stone") (box -14 58 -14 14 63 14 "stone")) ["3,60,0" "3,61,0"])
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [3 60 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [3 60 0] (feet p)))
          (is (= [:dig-route] (map :step (events-of seen :go-to.escalated))) "the goal is a dig away: no wall is walked to")
          (is (empty? (events-of seen :go-to.wall-walk)) "no wall is walked to"))))))

(deftest a-body-on-a-plateau-digs-toward-a-far-walled-in-goal-leg-by-leg
  ;; the plateau of the test above, the goal sealed in a block 30 off on the floor below: no dig search runs to the
  ;; goal from farther than a leg (a whole-way search to a far goal searches a ball of rock round the body)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [goal [30 58 0]
              blocks (apply dissoc (merge (box -40 57 -40 40 57 40 "stone") (box -14 58 -14 14 63 14 "stone") (box 26 58 -3 34 62 3 "stone"))
                            ["30,58,0" "30,59,0"])
              searches (atom []) search dig-route/search
              {:keys [out p]} (with-redefs [dig-route/search (fn [block-at from to opts] (swap! searches conj [from to]) (search block-at from to opts))]
                                (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks
                                                  :inventory [{:name "stone_pickaxe" :count 1}]}
                                                 {:pos goal :range 0})))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= goal (feet p)))
          (is (not-any? (fn [[from to]] (and (= goal (vec to)) (> (u/dist (zipmap [:x :y :z] from) (zipmap [:x :y :z] to)) choose/leg-blocks))) @searches)
              (pr-str @searches)))))))

(deftest a-body-by-a-blocked-door-digs-toward-a-far-goal-leg-by-leg
  ;; a door with rock behind it (no room to open it into) and a goal 30 off in a pocket of air: no dig search runs to
  ;; the goal from farther than a leg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [goal [30 64 0]
              blocks (apply dissoc (merge room (box -6 63 -6 40 63 6 "stone") (box 4 64 -3 34 66 3 "stone") (box 26 64 -3 34 66 3 "stone")
                                          {"3,64,0" "oak_door" "3,65,0" "oak_door"})
                            ["30,64,0" "30,65,0"])
              searches (atom []) search dig-route/search
              {:keys [out]} (with-redefs [dig-route/search (fn [block-at from to opts] (swap! searches conj [from to]) (search block-at from to opts))]
                              (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks
                                                :inventory [{:name "stone_pickaxe" :count 1}]}
                                               {:pos goal :range 0 :unknown false})))]
          (is (seq @searches) "a dig route was searched")
          (is (not-any? (fn [[from to]] (and (= goal (vec to)) (> (u/dist (zipmap [:x :y :z] from) (zipmap [:x :y :z] to)) choose/leg-blocks))) @searches)
              (pr-str @out @searches)))))))

(deftest a-walled-in-goal-is-not-dug-to-in-another-s-zone-or-under-walk-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[what args zones] [[:zone {} [{:name "vault" :owner "Miles" :min [6 60 -2] :max [10 70 2]}]]
                                   [:walk-only {:ways #{:walk}} nil]]]
          (let [{:keys [out p seen]} (await (run-job! free-by-pocket 'jobs.movement.go-to (merge {:pos [8 64 0] :range 0} args) zones))]
            (is (false? (:arrived @out)) what)
            (is (= :goal-enclosed (:why @out)) what)
            (is (re-find #"the goal is walled in" (:text @out)) what)
            (is (empty? (calls p "dig")) what)
            (is (every? #{:approach} (map :step (events-of seen :go-to.escalated))) "at most a walk to the nearest cell, to look")))))))

(deftest a-goal-sealed-in-seen-bedrock-with-no-worth-ends-unreachable-as-no-way-exists
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [bedrock-pocket (into {} (for [[k v] pocket] [k (if (= "stone" v) (if (<= 6 (first (fake/parse-cell k)) 10) "bedrock" v) v)]))
              {:keys [out]} (await (run-job! (assoc free-by-pocket :blocks bedrock-pocket) {:pos [8 64 0] :range 0}))]
          (is (false? (:arrived @out)))
          (is (= :unreachable (:reason @out)) (pr-str @out))
          (is (= :goal-enclosed (:why @out)) "no search is kept for a way no dig can open"))))))

(deftest a-walk-only-call-walks-to-the-wall-of-a-walled-in-goal-when-its-budget-allows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spend from at] [[nil {:x 0 :z 0} [4 64 0]] [{:wall-time 0.5} {:x 0 :z 0} [0 64 0]] [nil {:x 3 :z 2} [4 64 2]]]]
          (let [{:keys [out p]} (await (run-job! (update-in free-by-pocket [:self :pos] merge from) (cond-> {:pos [8 64 0] :range 0 :ways #{:walk}} spend (assoc :spend spend))))]
            (is (= :goal-enclosed (:why @out)) (pr-str spend @out))
            (is (= at (feet p)) (str spend ": the nearest cell to the goal, when the walk there fits the budget"))
            (is (empty? (calls p "dig")))))))))

(deftest a-way-chosen-where-no-walk-was-tried-walks-before-it-gives-up
  ;; card 2b2da873: an approach or a climb leaves the body on open land; no way is chosen there, but its kept give-up was
  ;; a walk from another cell, so it walks again rather than give that up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (gt/setup {:blocks (box -10 63 -10 10 63 10 "stone")})
              given (atom 0)
              pos {:x 6 :y 64 :z 0}
              escalate (fn ^:async escalate-from [at]
                         (let [c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:pos pos :worth tu/order-worth} :round 1})]
                           (.setOwner (:primitives eng) "tx")
                           (ctx/update-mem! c assoc :give-up {:tries 3 :status :no-path :at at :result {:status :no-path :reason :exhausted}})
                           [(await (esc/escalate! c pos nil)) (contains? (ctx/mem c) :give-up)]))]
          (with-redefs [escape-way/choose (fn ([_ _ _] {:step :none :why :no-wall}) ([_ _ _ _] {:step :none :why :no-wall}) ([_ _ _ _ _] {:step :none :why :no-wall}))
                        escape-way/way-spot (constantly nil)
                        gend/give-up! (fn ([_ _ _] (swap! given inc) :gave-up) ([_ _ _ _] (swap! given inc) :gave-up))]
            (let [away (await (escalate [9 64 9]))
                  _ (is (zero? @given))
                  here (await (escalate [0 64 0]))]
              (is (= [:again false] away) "kept from another cell: walked again, the give-up dropped")
              (is (= :gave-up (first here)) "kept from the feet: given up")
              (is (= 1 @given)))))))))

(deftest a-wall-walk-cut-short-is-walked-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [real near/walk-round!
              cuts (atom 0)
              cut-once (fn [c to & args]
                         (if (and (= to {:x 4 :y 64 :z 0}) (< (swap! cuts inc) 2))
                           (js/Promise.resolve {:status "partial"})
                           (apply real c to args)))
              {:keys [out p seen]} (with-redefs [near/walk-round! cut-once]
                                     (await (run-job! free-by-pocket {:pos [8 64 0] :range 0 :ways #{:walk}})))]
          (is (= :goal-enclosed (:why @out)) (pr-str @out))
          (is (= 2 (count (events-of seen :go-to.wall-walk))) "a walk reported partial is made again")
          (is (= [4 64 0] (feet p))))))))

(deftest a-wall-walk-past-the-flood-s-reach-walks-on-to-the-nearest-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the 1024-cell flood from x -10 on a 25-wide floor stops near x 20, short of the pocket's wall at x 27
        (let [far-pocket (apply dissoc (merge (box -40 63 -12 40 63 12 "stone") (box 28 64 -2 32 66 2 "stone")) ["30,64,0" "30,65,0"])
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x -10 :y 64 :z 0}} :blocks far-pocket} {:pos [30 64 0] :range 0 :ways #{:walk}}))]
          (is (= :goal-enclosed (:why @out)) (pr-str @out))
          (is (<= (js/Math.hypot (- (first (feet p)) 30) (last (feet p))) 4.5) (str (feet p) ": the nearest cell, 3 off, within the wall-walk slack"))
          (is (= 2 (count (events-of seen :go-to.wall-walk)))))))))

(deftest a-wall-walk-that-ended-in-a-teleport-is-not-marked-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [real near/walk-round!
              real-restart gcut/restart-after-jump!
              jumps (atom 0)
              marks (atom [])
              teleport (fn [c to & args]
                         (if (and (= to {:x 4 :y 64 :z 0}) (zero? @jumps))
                           (do (doseq [x [1 90]] ((:on-pose c) {:x x :y 64 :z 0}))
                               (js/Promise.resolve {:result {:status :jumped} :status "blocked"}))
                           (apply real c to args)))
              {:keys [seen]} (with-redefs [near/walk-round! teleport
                                           gcut/restart-after-jump! (fn [c pos]
                                                                      (swap! jumps inc)
                                                                      (swap! marks conj (:wall-walked (ctx/mem c)))
                                                                      (real-restart c pos))]
                               (await (run-job! free-by-pocket {:pos [8 64 0] :range 0 :ways #{:walk}})))]
          (is (= [nil] @marks) "the restart finds no :wall-walked from the jumped walk")
          (is (= 2 (count (events-of seen :go-to.wall-walk)))))))))

(deftest a-wall-walk-takes-the-short-walk-among-near-equal-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; west face one block thicker than the north face: the cell north of the goal is 1 nearer, but 8 blocks of walking farther
        (let [blocks (apply dissoc (merge (box -20 63 -20 20 63 20 "stone") (box 5 64 -2 10 66 2 "stone")) ["8,64,0" "8,65,0"])
              {:keys [out p]} (await (run-job! {:self {:pos {:x 0 :y 64 :z 0}} :blocks blocks}
                                               {:pos [8 64 0] :range 0 :ways #{:walk}}))]
          (is (= :goal-enclosed (:why @out)) (pr-str @out))
          (is (= [4 64 0] (feet p))))))))

(deftest a-free-body-far-from-a-walled-in-goal-walks-to-the-nearest-cell-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (assoc-in free-by-pocket [:self :pos] {:x -15 :y 64 :z 0}) {:pos [8 64 0] :range 0}))]
          (is (true? (:arrived @out)))
          (is (= [8 64 0] (feet p)))
          (is (= [:approach :dig-route] (take 2 (map :step (events-of seen :go-to.escalated)))))
          (is (every? #(<= 5 (first %) 8) (dug-cells p)) "nothing dug on the open land of the walk there"))))))

(deftest a-free-body-stairs-or-pillars-up-into-a-higher-walled-in-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:pos {:x 0 :y 64 :z 0}} :blocks high-pocket
                     :inventory [{:name "stone_pickaxe" :count 1} {:name "cobblestone" :count 16}]}
              {:keys [out p seen]} (await (run-job! world {:pos [8 67 0] :range 0}))]
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text])))
          (is (= [8 67 0] (feet p)))
          (is (some #{:stair :pillar} (map :step (events-of seen :go-to.escalated)))))))))

(deftest a-goal-behind-lava-is-not-dug-toward-and-one-behind-water-is-swum-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lava (await (run-job! (assoc free-by-pocket :blocks (moat "lava")) {:pos [8 64 0] :range 0}))
              water (await (run-job! (assoc free-by-pocket :blocks (moat "water")) {:pos [8 64 0] :range 0}))]
          (is (false? (:arrived @(:out lava))))
          (is (not= :goal-enclosed (:why @(:out lava))) (pr-str (select-keys @(:out lava) [:why :text])))
          (is (empty? (events-of (:seen lava) :go-to.escalated)) "lava: no way in is made")
          (is (empty? (calls (:p lava) "dig")) "lava: nothing dug")
          (is (true? (:arrived @(:out water))) (pr-str (select-keys @(:out water) [:why :text]))))))))

(deftest a-goal-in-or-over-or-ringed-by-lava-is-refused-whatever-is-dug
  (let [p (tu/fake {:self {:pos {:x 0 :y 64 :z 0}}
                    :blocks (merge (box -6 63 -6 6 63 6 "stone")
                                   {"3,63,3" "lava" "5,64,0" "lava"}
                                   {"-1,63,-4" "lava" "1,63,-4" "lava" "0,63,-5" "lava" "0,63,-3" "lava"})})
        lava? #(choose/goal-lava? p %)]
    (is (true? (lava? [3 64 3])) "over lava")
    (is (true? (lava? [5 64 0])) "in lava")
    (is (true? (lava? [0 64 -4])) "lava on all four sides of the floor cell")
    (is (false? (lava? [1 64 1])) "on stone")))

(deftest go-to-the-cut-off-deck-with-the-default-budget-is-still-cut-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (run-job! {:self {:pos {:x 5 :y 64 :z 0}} :blocks room-and-deck}
                                             {:pos [8 71 0] :range 0 :ways #{:walk :clear-seal}}))]
          (is (= {:why :goal-cut-off :reason :unreachable :arrived false} (select-keys @out [:why :reason :arrived]))
              "the default budget lets drops of 10 in, and still no way leads up onto the deck"))))))

(deftest drop-edges-and-nearest-stand-read-only-cells-the-body-has-seen
  (let [slab (box -3 63 -3 3 63 3 "stone")
        outside (for [x (range -6 7) z (range -6 7) y (range 58 68) :when (or (> (js/Math.abs x) 3) (> (js/Math.abs z) 3))] [x y z])
        seen (tu/fake {:blocks slab})
        dark (tu/fake {:blocks slab})
        _ (dark-at dark outside)]
    (is (seq (choose/drop-edges seen [[0 64 0]] [0 64 0])) "every edge cell seen: a floor cell to fill is offered")
    (is (empty? (choose/drop-edges dark [[0 64 0]] [0 64 0])) "the cells beyond the edge never seen: not offered, not read through the true world")
    (is (= [0 64 0] (choose/nearest-stand dark [0 64 0])))))

(deftest wall-spot-and-door-scan-flood-only-through-cells-the-body-has-seen
  (let [yard (merge (box -3 63 -3 3 63 3 "stone") (box 3 64 -3 3 65 3 "stone"))
        seen (tu/fake {:blocks yard :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        dark (tu/fake {:blocks yard :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        _ (dark-at dark [[2 64 0]])
        hall (merge (box -3 63 -3 5 63 3 "stone") (box -3 66 -3 5 66 3 "stone") (box 3 64 -3 3 65 3 "stone") (box -3 64 -3 5 65 -3 "stone") (box -3 64 3 5 65 3 "stone") (box -3 64 -3 -3 65 3 "stone")
                    {"3,64,0" "oak_door" "3,65,0" "oak_door"})
        door-seen (tu/fake {:blocks hall :self {:pos {:x 0.5 :y 64 :z 0.5}} :states {"3,64,0" {:open false :half "lower" :facing "east"} "3,65,0" {:open false :half "upper" :facing "east"}}})
        door-dark (tu/fake {:blocks hall :self {:pos {:x 0.5 :y 64 :z 0.5}} :states {"3,64,0" {:open false :half "lower" :facing "east"} "3,65,0" {:open false :half "upper" :facing "east"}}})
        _ (dark-at door-dark [[2 64 0]])]
    (is (= [2 64 0] (escape-way/wall-spot seen [0 64 0] [9 64 0] #{} nil)))
    (is (not= [2 64 0] (escape-way/wall-spot dark [0 64 0] [9 64 0] #{} nil)) "the cell it would stand in never seen: not offered")
    (is (true? (escape/open-door-beside? door-seen [0 64 0])))
    (is (false? (escape/open-door-beside? door-dark [0 64 0])) "the only way to the door runs through a cell never seen")))

(deftest wall-spot-is-the-cheapest-wall-toward-the-goal-not-the-nearest
  (let [yard (merge (box -12 63 -8 12 63 8 "stone") (box -2 64 2 -2 65 2 "stone") (box 7 64 0 7 65 0 "stone"))
        p (tu/fake {:blocks yard :self {:pos {:x 0.5 :y 64 :z 0.5}}})]
    (is (= [6 64 0] (escape-way/wall-spot p [0 64 0] [10 64 0] #{} nil)) "a wall 6 steps off on the way beats one 5 steps off behind the body")))

(deftest priced-spot-is-bounded-by-the-price-of-its-cells-not-by-a-count-or-radius
  (let [p (tu/fake {:blocks (box -3 63 -3 45 63 3 "stone") :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        steps (escape/level-steps p false)
        far (fn [cell _ _] (= 40 (first cell)))]
    (is (= [40 64 0] (escape-way/priced-spot p [0 64 0] [45 64 0] steps far #{} nil)) "dozens of rejected cells and 40 blocks away: still found, no cap or radius ends it")
    (is (= [40 64 0] (escape-way/priced-spot p [0 64 0] [45 64 0] steps far #{} 3600)))
    (is (nil? (escape-way/priced-spot p [0 64 0] [45 64 0] steps far #{} 3)) "its price, the walk there and on, is more than the bound affords: the search ends")))

(deftest go-to-fills-a-one-cell-gap-under-a-low-ceiling-with-a-carried-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x 1.5 :y 64 :z 0.5}} :blocks gap-tunnel :inventory [{:name "dirt" :count 5}]}
                                                    {:pos [9 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [:fill-gap] (mapv :step (events-of seen :go-to.escalated))))
          (is (= "dirt" (block p [5 63 0])) "the block went into the gap")
          (is (empty? (calls p "dig"))))))))

(def stone-mass
  "Solid stone x -1..12, y 60..80, z -2..2 with the body's cell [0 64 0] and the cell over it open."
  (dissoc (box -1 60 -2 12 80 2 "stone") "0,64,0" "0,65,0"))

(deftest a-climb-through-rock-it-barely-sees-has-a-route-in-hand-at-the-search-ceiling
  ;; Q141: seeing 2 blocks, the unseeded search read 142k cells (2.8 s) over 29 calls before its first route; searched
  ;; round its planned stairs first it holds one at once and answers once its CPU passes dig-route/answer-s (a stair lane
  ;; alone was up to 3x dearer). Prices move with every prior, so the test pins the route, the ceiling and the order
  (let [answer (fn [goal]
                 (let [p (tu/seeing-within (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]}) 2)
                       id "climb-ceiling"]
                   (try (loop [i 0]
                          (let [a (choose/route-answer p [0 64 0] goal (constantly true) (constantly false) false false
                                                       {:read-s (cost/search-read-s) :slice (choose/route-slice id)})]
                            (if (and (= :searching a) (< i 3)) (recur (inc i)) a)))
                        (finally (choose/end-searches! id)))))
        near (answer [8 72 0])
        far (answer [8 76 0])]
    (doseq [[goal a] [[[8 72 0] near] [[8 76 0] far]]]
      (is (seq (:steps a)) (str goal " " (pr-str (dissoc a :steps :dig))))
      (is (= goal (:to (last (:steps a)))) (str goal " reached"))
      (is (< (:searched-s a) (* 2 dig-route/answer-s)) (str goal " " (:searched-s a) " s of CPU")))
    (is (< (:seconds near) (:seconds far)) "four blocks higher is dearer")))

(deftest a-dig-route-and-its-straight-dig-pay-the-calls-tolls
  ;; a level tunnel x 0..10 in stone, tolled factor 60 but for its ends: the straight dig walks it, so priced without the
  ;; tolls it bounds the search below every way round; priced with them the route digs round
  (let [blocks (apply dissoc (box -1 63 -3 11 67 3 "stone") (for [x (range 0 11) y [64 65]] (str x "," y ",0")))
        p (tu/fake {:blocks blocks :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]})
        tolls (for [x (range 1 10)] {:x x :y 64 :z 0 :factor 60})
        id "tolled-tunnel"
        a (try (loop [i 0]
                 (let [a (choose/route-answer p [0 64 0] [10 64 0] (constantly true) (constantly false) false false
                                              {:tolls tolls :read-s (cost/search-read-s) :slice (choose/route-slice id)})]
                   (if (and (= :searching a) (< i 3)) (recur (inc i)) a)))
               (finally (choose/end-searches! id)))]
    (is (seq (:steps a)) (pr-str a))
    (is (not-any? (set (map (juxt :x :y :z) tolls)) (map :to (:steps a))) (pr-str (map :to (:steps a))))))

(deftest a-route-refused-cell-by-cell-with-no-bound-is-searched-at-most-twice
  ;; no :bound-s or :worth-s: only the zone's refusals end the rounds. The first route is refused; the next search asks
  ;; may-dig? of the cells it weighs, so the route it finds is never refused again
  (let [blocks (-> stone-mass (assoc "5,64,2" "air" "5,65,2" "air"))
        p (tu/fake {:blocks blocks :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]})
        refused? (fn [[x _ z]] (and (<= 1 x 4) (< z 2)))
        rounds (atom 0) route dig-route/route
        answer (with-redefs [dig-route/route (fn [& args] (swap! rounds inc) (apply route args))]
                 (choose/route-answer p [0 64 0] [5 64 2] #(not (refused? %)) (constantly false) false false {:worth-s tu/order-worth}))]
    (is (seq (:dig answer)))
    (is (not-any? refused? (:dig answer)))
    (is (<= @rounds 2) (str @rounds " searches"))))

(deftest a-stair-is-priced-as-the-climb-route-prices-it-with-the-tool-carried
  (are [inv] (let [p (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory inv})
                   route (choose/route-answer p [0 64 0] [3 67 0] (constantly true) (constantly false) false false {})]
               (and (= [:walk] (distinct (map :move (:steps route))))
                    (= 3 (count (:steps route)))
                    (< 0 (:seconds route))
                    (= (:seconds route) (escape-way/price p [0 64 0] {:step :stair :heading :east :steps 3}))))
    []
    [{:name "stone_pickaxe" :count 1}]
    [{:name "iron_pickaxe" :count 1}]))

(deftest a-route-searched-for-a-pickaxe-fetch-digs-at-the-fetched-pickaxes-price
  (let [route-s (fn [inv fetch] (:seconds (choose/route-answer (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory inv})
                                                               [0 64 0] [3 67 0] (constantly true) (constantly false) false fetch {})))
        wooden (route-s [{:name "wooden_pickaxe" :count 1}] false)
        bare (route-s [] false)]
    (is (= (+ wooden 1) (route-s [] (delay 1))) "the fetch is paid once, on top of the cuts at its pickaxe's price")
    (is (< wooden bare) "bare-handed the same cuts cost more")
    (is (= bare (route-s [] (delay 1e6))) "a fetch dearer than what it saves is not made: the cuts by hand")))

(deftest route-tool-spends-a-worn-picks-uses-across-the-whole-route
  (let [cells (mapv #(vector % 64 0) (range 10))
        price (fn [inv] (:s (choose/route-tool (tu/fake {:blocks stone-mass :inventory inv}) (constantly "stone") cells nil nil (constantly 1))))
        pick (fn [left] [{:name "stone_pickaxe" :count 1 :durability left :max-durability 131}])
        fresh (price (pick 100))
        worn (price (pick 3))
        bare (price [])]
    (is (< fresh worn bare) "3 uses cut 3 of the 10 cells at the pick's rate, the other 7 by hand")))

(def open-stair-mass
  "stone-mass with the cells of a 3-step stair east from [0 64 0] already open."
  (apply dissoc stone-mass (map #(str (clojure.string/join "," %)) (escape/stair-cuts [0 64 0] [1 0] 3))))

(deftest a-stair-through-open-cells-is-priced-as-the-climb-route-prices-it
  (let [p (tu/fake {:blocks open-stair-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]})
        route (choose/route-answer p [0 64 0] [3 67 0] (constantly true) (constantly false) false false {})]
    (is (= 3 (count (:steps route))))
    (is (= (:seconds route) (escape-way/price p [0 64 0] {:step :stair :heading :east :steps 3})))
    (is (< (:seconds route) (cw/stair-cost 3)) "a cut-free step is no dearer than a walk step")))

(defn at-chance
  "f run with every unseen cell solid at chance."
  [chance f]
  (with-redefs [look/unseen-solid-at-fn (fn [_] (constantly chance))] (f)))

(deftest a-new-stair-step-is-priced-as-a-step-cut-through-stone-and-dearer-unseen
  (let [inv [{:name "stone_pickaxe" :count 1}]
        body #(tu/seeing-within (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory inv}) %)
        seen (body 50)
        step {:step :stair :heading :east :steps 1}]
    (is (= (at-chance 1 #(escape-way/price (body 1) [0 64 0] step)) (escape-way/new-stair-step-s seen)) "the price of one unseen stone step, the cuts sure to be solid")
    (is (< (cw/stair-step-s) (escape-way/new-stair-step-s seen)) "the cuts are paid on top of the stair's step")
    (is (< (escape-way/price seen [0 64 0] step) (escape-way/new-stair-step-s seen)) "seen cells cost less")))

(deftest an-unseen-stair-and-pillar-are-priced-as-the-climb-route-prices-them
  (let [inv [{:name "iron_pickaxe" :count 1}]
        body #(tu/seeing-within (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory inv}) %)
        p (body 4.2)
        unseen-s (cost/unseen-route-s (wworld/unseen-share p) (constantly :ground) [{:x 0 :y 0 :z 0}])
        stair {:step :stair :heading :east :steps 3}
        route-s #(:seconds (choose/route-answer p [0 64 0] [3 67 0] (constantly true) (constantly false) false false {}))]
    (is (= (at-chance 1 route-s) (at-chance 1 #(escape-way/price p [0 64 0] stair))) "unseen rock sure to be solid")
    (is (< (escape-way/price (body 50) [0 64 0] stair) (at-chance 1 #(escape-way/price p [0 64 0] stair))) "unseen cells cost more")
    (is (< (escape-way/price p [0 64 0] stair) (at-chance 1 #(escape-way/price p [0 64 0] stair))) "an unseen cut is dug at the chance it is solid")
    (is (< (route-s) (at-chance 1 route-s)) "the climb route digs an unseen cell at the chance it is solid")
    (are [r n] (let [q (body r)
                     roof-s (* n ((escape/dig-share-fn q) [0 67 0]) (/ (tools/clear-ms q "stone") 1000))
                     ;; the unseen [0 68 0] over the roof [0 67 0] (solid at its chance) and [0 67 0] over the seen roof [0 66 0]
                     share (value/roof-fall-share nil)
                     land-s (cost/landing-cost-s {:health 20 :absorption 0} (cost/death-s q nil))
                     landing-s (* n (+ (* share land-s) (* ((escape/dig-share-fn q) [0 67 0]) share land-s)))]
                 (= (+ (cw/pillar-cost 2) (* n unseen-s) (escape/pillar-knock q [0 64 0] 2) roof-s landing-s) (escape-way/price q [0 64 0] {:step :pillar :height 2})))
      50 0
      2.9 1)))

(deftest a-climb-dearer-as-rock-than-its-bound-is-found-at-the-chance-its-cells-are-solid
  (let [p (tu/seeing-within (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]}) 2)
        ;; searched over slices as go-to's rounds search it, each question under its own owner
        climb (fn [bound] (let [id (gensym)]
                            (try (loop [] (let [e (climb-route/climb-way p [0 64 0] [3 67 0] (constantly true) {:skip #{} :bound-s bound :slice (choose/route-slice id)})]
                                            (if (= :searching (:step e)) (recur) e)))
                                 (finally (choose/end-searches! id)))))
        rock-s (at-chance 1 #(:route-s (climb nil)))
        share-s (:route-s (climb nil))
        bound (/ (+ rock-s share-s) 2)]
    (is (< share-s rock-s))
    (is (nil? (at-chance 1 #(climb bound))) "as rock it costs past the bound")
    (is (= [:stair share-s] ((juxt :step :route-s) (climb bound))) "its search is not cut by a bound its share price is under")))

(deftest a-pillar-roof-over-seen-loose-ground-is-priced-as-the-measured-mix-of-loose-block-and-stone
  (let [inv [{:name "iron_pickaxe" :count 1}]
        p (tu/seeing-within (tu/fake {:blocks (assoc stone-mass "0,65,0" "gravel" "0,66,0" "gravel") :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory inv}) 2.9)
        price-at (fn [share] (settings/with-settings {:jobs.lib.cost.value/loose-over-loose-counts (zipmap [:h1 :h2 :h3 :h4 :h5] (repeat {:loose share :other (- 100 share)}))}
                               #(escape-way/price p [0 64 0] {:step :pillar :height 2})))
        gravel-s (/ (tools/clear-ms p "gravel") 1000)
        stone-s (* ((escape/dig-share-fn p) [0 67 0]) (/ (tools/clear-ms p "stone") 1000))]
    (is (< (js/Math.abs (- (- (price-at 100) (price-at 0)) (- gravel-s stone-s))) 1e-6) "an unseen cell over the gravel is gravel at the chance, else the stone guess")
    (is (not= (price-at 100) (price-at 0)))))

(deftest a-seen-loose-cell-over-an-unseen-gap-is-certain-and-the-run-above-reads-its-height
  (let [column {[0 64 0] "gravel" [0 66 0] "gravel" [0 63 0] "stone"}
        counts {:h1 {:loose 1 :other 1} :h2 {:loose 1 :other 1} :h3 {:loose 1 :other 3} :h4 {:loose 1 :other 3} :h5 {:loose 1 :other 3}}]
    (with-redefs [escape/block-at-of (constantly #(get column %))
                  escape/unseen-of (constantly #(nil? (get column %)))]
      (settings/with-settings {:jobs.lib.cost.value/loose-over-loose-counts counts}
        #(is (= [[[0 65 0] 0.5 "gravel"] [[0 66 0] 1 "gravel"] [[0 67 0] 0.25 "gravel"]]
                (vec (take 3 (escape-way/loose-run nil [0 64 0])))))))))

(def open-ground
  "Air x -1..12, y 64..80, z -2..2 over a stone floor at y 63."
  (merge (box -1 64 -2 12 80 2 "air") (box -1 63 -2 12 63 2 "stone")))

(deftest a-pillar-way-prices-the-knock-off-fall-at-the-chance
  (let [p (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]})
        price #(escape-way/price p [0 64 0] {:step :pillar :height 8})
        at (fn [knock] (settings/with-settings {:jobs.lib.cost.value/pillar-knock-counts {:knock knock :kept 1}} price))]
    (is (< (at 0) (at 1)) "a dearer chance of a knock-off dears the pillar")))

(deftest the-climb-routes-pillar-blocks-cost-by-their-place-in-a-run-and-the-open-sides
  (let [open (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        walled (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        block-s (fn [p] (climb-route/pillar-block-s p))]
    (is (= (cw/pillar-cost 1) ((block-s open) 1 0 64 0)) "the first block falls no hurt")
    (is (= (+ (cw/pillar-cost 1) (cost/pillar-knock-of open [0 0 0 0 0 1])) ((block-s open) 6 0 64 0)) "the sixth, on open sides, falls six")
    (is (< ((block-s open) 3 0 64 0) ((block-s open) 6 0 64 0)) "a taller run falls farther")
    (is (= (cw/pillar-cost 1) ((block-s walled) 6 0 64 0)) "a walled column is never knocked off")))

(deftest a-climb-does-not-pillar-up-under-a-seen-falling-block
  ;; live (Tamsin S7): a climb's pillar dug its roof under sand that landed on the body (4 suffocation hits). The sand
  ;; over the roof is cheaper to dig than stone, so unpriced the column would be pillared as the stone one is.
  (let [climb (fn [over]
                (let [p (tu/seeing-within (tu/fake {:blocks (assoc stone-mass "0,67,0" over "0,68,0" over) :self {:pos {:x 0.5 :y 64 :z 0.5}}
                                                    :inventory [{:name "iron_pickaxe" :count 1} {:name "dirt" :count 16}]}) 20)
                      id (gensym)
                      e (try (loop [] (let [e (climb-route/climb-way p [0 64 0] [0 70 0] (constantly true) {:skip #{} :pillar-item {:item "dirt" :count 16} :slice (choose/route-slice id)})]
                                        (if (= :searching (:step e)) (recur) e)))
                             (finally (choose/end-searches! id)))]
                  (set (keep #(when (= :pillar (:step %)) (:from %)) (cons e (:then e))))))]
    (is (contains? (climb "stone") [0 64 0]) "under stone the climb pillars straight up from the feet")
    (is (not-any? (fn [[x y z]] (and (= 0 x z) (< y 67))) (climb "sand")) "under sand no pillar digs up into it from below")))

(deftest a-climb-does-not-pillar-up-under-seen-water-over-unseen-rock
  ;; live (Tamsin S7): a climb pillared up under a lake bed it had not seen; the sand over its roof landed on the body.
  ;; Seen water a few cells over unseen roofs makes a falling block over them likely (value/roof-fall-shares).
  (let [climb (fn [over]
                (let [p (tu/seeing-within (tu/fake {:blocks (assoc stone-mass "0,72,0" over "0,73,0" over) :self {:pos {:x 0.5 :y 64 :z 0.5}}
                                                    :inventory [{:name "iron_pickaxe" :count 1} {:name "dirt" :count 16}]}) 4.5)
                      sensed (aget p "sensedAt")
                      _ (tu/set-sensed! p (fn [pos] (if (and (= 0 (.-x pos) (.-z pos)) (<= 72 (.-y pos) 73)) (.blockAt (tu/oracle p) pos) (sensed pos))))
                      id (gensym)
                      e (try (loop [] (let [e (climb-route/climb-way p [0 64 0] [0 70 0] (constantly true) {:skip #{} :pillar-item {:item "dirt" :count 16} :slice (choose/route-slice id)})]
                                        (if (= :searching (:step e)) (recur) e)))
                             (finally (choose/end-searches! id)))]
                  ;; the roofs its pillars dig in the column under the water, 2 to 4 cells under it
                  (set (for [{[x y z] :from h :height} (cons e (:then e)) :when (= 0 x z) roof (range (+ y 2) (+ y h 2)) :when (<= 68 roof 70)] roof))))]
    (is (= #{68 69 70} (climb "stone")) "under stone the climb pillars straight up from the feet")
    (is (= #{} (climb "water")) "under the water it digs those roofs from beside, not from below")))

(deftest climb-way-gives-the-search-the-pillar-block-price-at-the-measured-chance
  (let [p (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        block-6 (fn [knock]
                  (settings/with-settings {:jobs.lib.cost.value/pillar-knock-counts {:knock knock :kept 1}}
                    #(let [seen (atom nil)]
                       (with-redefs [choose/route-answer (fn [_ _ _ _ _ _ _ opts] (reset! seen (:pillar-s opts)) nil)]
                         (climb-route/climb-way p [0 64 0] [0 70 0] (constantly true) {:skip #{} :pillar-item {:item "dirt" :count 8}}))
                       (@seen 6 0 64 0))))]
    (is (< (block-6 0) (block-6 1)) "a dearer chance of a knock-off dears the search's pillar block")))

(deftest an-open-pillar-pays-the-knock-off-chance-at-every-block-and-a-walled-one-none
  (let [open (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        walled (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}}})]
    (is (= [4 4 4] (escape/pillar-open-sides open [0 64 0] 3)))
    (is (= [0 0 0] (escape/pillar-open-sides walled [0 64 0] 3)))
    (is (= 0 (escape/pillar-knock walled [0 64 0] 3)) "a shaft prices no knock-off")
    (is (= (cost/pillar-knock-of open [1 1 1]) (escape/pillar-knock open [0 64 0] 3)) "open ground pays the full rate")))

(deftest a-pillars-unseen-side-cells-are-held-at-the-measured-chance-they-are-solid
  (let [shaft (merge stone-mass (box 0 64 0 0 70 0 "air"))
        sides (escape/pillar-open-sides (tu/seeing-within (tu/fake {:blocks shaft :self {:pos {:x 0.5 :y 64 :z 0.5}}}) 2.6) [0 64 0] 6)]
    (is (= [0 0 0] (subvec sides 0 3)) "the seen walls hold")
    (is (every? #(< 0 % 4) (subvec sides 3)) "unseen walls are neither held nor open")))

(deftest a-tall-pillar-costs-more-in-the-open-than-in-a-shaft
  (let [mass (box -1 40 -2 30 90 2 "stone")
        col (fn [walls] (merge (if walls mass (box -1 40 -2 30 90 2 "air")) (box 0 64 0 0 90 0 "air") (box -1 40 -2 30 63 2 "stone")))
        inv [{:name "dirt" :count 32} {:name "iron_pickaxe" :count 1}]
        price (fn [walls step] (escape-way/price (tu/fake {:blocks (col walls) :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory inv}) [0 64 0] step))
        pillar {:step :pillar :height 18}]
    (is (< (price true pillar) (price false pillar)) "open sides add the knock-off falls")))

(deftest a-fill-competes-by-price-with-the-stair-and-the-pillar
  (let [p (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]})
        fill #(hash-map :step :fill-gap :cell [5 63 0] :item "dirt" :count %)
        stair #(hash-map :step :stair :heading :east :steps %)
        three (stair 3)]
    (is (< (cw/fill-cost 9) (escape-way/price p [0 64 0] three)))
    (are [way carried expected] (= expected (:step (choose/cheaper-fill p [0 64 0] way (fill carried))))
      three 9 :fill-gap
      {:step :pillar :height 1} 9 :pillar
      {:step :pillar :height 2} 9 :fill-gap
      {:step :none :why :no-dig} 9 :fill-gap
      {:step :leave-shelter} 9 :fill-gap)
    (is (< (cw/fill-cost 9) (cw/fill-cost 2)) "the last blocks of the pack cost a fetch")))

(def doorway-hole
  "Floor y 63 with a 1-cell hole at x 5 (floor under it at y 62), and a floor 1 higher from x 6: the doorway of a hut whose sill was dug."
  (-> (merge (box -2 62 -2 4 63 4 "stone") (box 5 62 -2 5 62 4 "stone") (box 6 62 -2 12 64 4 "stone"))))

(deftest go-to-fills-a-one-cell-hole-before-a-step-up-with-a-carried-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x 1.5 :y 64 :z 1.5}} :blocks doorway-hole :inventory [{:name "dirt" :count 5}]}
                                                    {:pos [10 65 1] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [:fill-gap] (mapv :step (events-of seen :go-to.escalated))))
          (is (= "dirt" (block p [5 63 1])) "the block went into the hole at the takeoff's floor height"))))))

(def hut-on-slope
  "A stone hut (interior x -2..2, z -2..2, feet y 64) open at a doorway x 0, z 3 (y 64..65) whose sill (y 63) is dug out to
  y 62, on a stone floor; the slope outside is solid to y 66 from z 4 on, 3 up from the doorway."
  (apply dissoc
         (merge (box -6 62 -6 12 63 12 "stone") (box -3 64 -3 3 66 3 "stone") (box -6 64 4 12 66 12 "stone"))
         "0,63,3" (concat (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)) ["0,64,3" "0,65,3"])))

(def hut-plan
  (shape/with-author {:id "hut" :parts [{:id "w" :cells (vec (for [x (range -3 4) y (range 63 68) z (range -3 4)] [x y z])) :want "stone"}]} "Fake"))

(deftest go-to-walks-to-the-dug-doorway-to-cut-a-way-out-when-the-wall-beside-it-is-its-own-build
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 1.5 :y 64 :z 1.5}} :blocks hut-on-slope}
                                                  'jobs.movement.go-to {:pos [0 67 8] :range 0} {:plans {"hut" hut-plan}}))]
          (is (= {:arrived true} @out))
          (is (= [:approach :stair] (distinct (mapv :step (events-of seen :go-to.escalated))))
              "a stair cut from the doorway, not through the hut wall"))))))

(deftest go-to-digs-round-a-gap-under-a-low-ceiling-with-nothing-to-fill-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 1.5 :y 64 :z 0.5}} :blocks gap-tunnel :inventory [{:name "stone_pickaxe" :count 1}]}
                                                  {:pos [9 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (not-any? #{:fill-gap} (map :step (events-of seen :go-to.escalated)))))))))

(def step-up-tunnel
  "A 4-high tunnel (x 0..14, z 0) in stone with a 1-cell hole at x 5 (floor y 62, stone under it), then a raised floor (stone y 63) from x 6 and air y 64..67 to x 14."
  (apply dissoc (merge (box -2 60 -2 16 69 2 "stone"))
         (concat (for [x (range 0 6) y (range 63 67)] (str x "," y ",0"))
                 (for [x (range 6 15) y (range 64 68)] (str x "," y ",0"))
                 ["5,62,0"])))

(deftest go-to-cuts-the-riser-beyond-a-hole-before-a-step-up-with-nothing-to-fill-it
  "The far tunnel is not seen from here (dark), as from a sealed tunnel: no dig route reaches the goal, the riser is the way."
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inner? (fn [x y z] (every? #(contains? step-up-tunnel (str (+ x (% 0)) "," (+ y (% 1)) "," (+ z (% 2))))
                                         [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))
              {:keys [eng p seen]} (setup {:self {:pos {:x 1.5 :y 63 :z 0.5}} :blocks step-up-tunnel})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [12 64 0] :range 0})))]
          (dark-at p (for [x (range -2 17) y (range 60 70) z (range -2 3) :when (or (>= x 7) (and (contains? step-up-tunnel (str x "," y "," z)) (inner? x y z)))] [x y z]))
          (core/submit! eng (tu/order '(recording-parent {})) {})
          (await (tick-out! eng 1000))
          (is (= :dig-route (:step (first (events-of seen :go-to.escalated)))) "the riser is cut before any stair")
          (is (= "air" (block p [6 63 0])) "the step up beyond the gap is cut level with the takeoff"))))))

(deftest go-to-digs-its-way-up-instead-of-a-long-priced-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0 :y 50 :z 0}} :blocks long-loop :inventory [{:name "iron_pickaxe" :count 1}]}
                                              {:pos [10 60 2] :range 1 :tolls loop-tolls}))]
          (is (= {:arrived true} @out))
          (let [es (events-of seen :go-to.escalated)]
            ;; the dig route prices the tunnel's tolls (factor 60): it leaves the tunnel at once and stairs up, not the loop
            ;; (unpriced, a 21.30 s route walked all 21 tolled cells)
            (is (= [:dig-route :approach :stair :stair] (mapv :step es)) (pr-str (map #(select-keys % [:step :at :why]) es)))
            (is (= [2 50 1] (:at (nth es 2))) "the first stair begins off the tunnel, 2 blocks along")))))))

(deftest go-to-walks-the-long-loop-when-escalating-is-off-or-the-goal-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:pos {:x 0 :y 50 :z 0}} :blocks long-loop :inventory [{:name "iron_pickaxe" :count 1}]}
              off (await (run-job! world {:pos [10 60 2] :range 1 :tolls loop-tolls :ways #{:walk :clear-seal}}))
              near (await (run-job! world {:pos [24 52 0] :range 1 :tolls loop-tolls}))]
          (is (true? (:arrived @(:out off))))
          (is (empty? (events-of (:seen off) :go-to.escalated)))
          (is (true? (:arrived @(:out near))))
          (is (empty? (events-of (:seen near) :go-to.escalated))))))))

(deftest go-to-digs-the-last-steps-of-its-stair-out-instead-of-calling-the-way-one-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x -8.5 :y 19 :z 0.5}} :blocks stair-below-surface
                                                  :inventory [{:name "iron_pickaxe" :count 1}]}
                                                 {:pos [-40 41 0] :range 1}))]
          (is (= [:stair] (take 1 (mapv :step (events-of seen :go-to.escalated)))))
          (is (= {:arrived true} @out)))))))

(deftest a-go-to-sealed-in-a-cave-without-escalate-does-not-plan-the-same-search-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 3.5 :y 64 :z 0.5}} :blocks cave
                                                   :inventory [{:name "iron_pickaxe" :count 1}]}
                                                  {:pos [30 64 0] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= [false :start-enclosed] ((juxt :arrived :why) @out)) (str "a sealed body is the problem, as the looped rounds it replaces ended: " (pr-str @out)))
          (is (empty? (events-of seen :go-to.retrying)) "an exhausted search from the same cell over the same memory answers the same"))))))

(deftest an-approach-walk-may-take-back-its-own-seal
  (is (= #{:walk :clear-seal} (:ways (second (esc/escalation-job {:step :approach :pos [1 64 0]} nil false false false))))))


(deftest walker-faults-past-the-retries-feed-the-escalation-ladder
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (merge (box -20 63 -40 20 63 40 "stone") (box 3 64 -12 3 65 12 "dirt"))
              {:keys [eng p seen]} (setup {:self {:pos {:x 2.5 :y 64 :z 0.5}} :blocks wall})
              steers (atom 0)
              stuck (gt/steer-through (fn [_] (gt/pose 2.5 0.5)))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [8 64 0] :range 0})))]
          (gt/override-steer! p (fn [token a impl] (if (<= (swap! steers inc) 3) (stuck token a impl) (impl token a))))
          (core/submit! eng (tu/order '(recording-parent {})) {})
          (await (tick-out! eng 1000))
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [:dig-route] (mapv :step (events-of seen :go-to.escalated))) "three stuck walks round the wall, then a dig through it"))))))

(deftest one-stuck-walk-under-one-try-gives-up-without-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (merge (box -20 63 -40 20 63 40 "stone") (box 3 64 -12 3 65 12 "dirt"))
              {:keys [eng p seen]} (setup {:self {:pos {:x 2.5 :y 64 :z 0.5}} :blocks wall})
              steers (atom 0)
              stuck (gt/steer-through (fn [_] (gt/pose 2.5 0.5)))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [8 64 0] :range 0 :spend {:tries 1}})))]
          (gt/override-steer! p (fn [token a impl] (if (<= (swap! steers inc) 1) (stuck token a impl) (impl token a))))
          (core/submit! eng (tu/order '(recording-parent {})) {})
          (await (tick-out! eng 1000))
          (is (false? (:arrived @out)) (pr-str @out))
          (is (empty? (events-of seen :go-to.escalated)) "a caller that re-aims itself gets the stuck walk back, no door dug")
          (is (empty? (calls p "dig"))))))))

(deftest a-free-body-with-one-pickaxe-or-none-lets-its-stair-fetch-a-tool
  (let [flat {:blocks (box -20 63 -20 20 63 20 "stone") :self {:pos {:x 0 :y 64 :z 0}}}
        fetch? #(choose/fetch-tool? {:primitives (tu/fake %) :args {}})
        picks #(vec (repeat % {:name "stone_pickaxe" :count 1}))]
    (is (= [true true false] (map #(fetch? (assoc flat :inventory (picks %))) [0 1 2])))
    (is (false? (fetch? (merge {:blocks room :inventory (picks 1)} in-room))) "shut in: no walk to a tool")
    (is (= [#{:carried :chest :craft :gather} #{:carried :craft}] (map #(:ways (second (esc/escalation-job {:step :stair :heading :east :steps 2} nil false false %))) [true false])))))

(deftest a-stair-through-its-own-pen-picks-up-its-drops
  (is (= [true false] (map #(:collect (second (esc/escalation-job {:step :stair :heading :east :steps 2 :own-cut %} nil false false false))) [true nil]))))

(deftest a-dig-route-s-digs-fetch-a-pickaxe-only-when-its-price-chose-one
  ;; the route's three stones cost ~22 s by hand, ~4 s with a wooden pickaxe: got in 5 s it pays, in 600 s it does not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pick {:name "stone_pickaxe" :count 1}
              priced (fn ^:async priced-run [s inventory] (with-redefs [choose/pickaxe-fetch (fn [_ fetch?] (when fetch? (delay s)))]
                                                 (await (dig-route-run! below-pocket inventory nil))))
              one (await (priced 5 [pick]))
              two (await (priced 5 [pick pick]))
              dear (await (priced 600 []))
              cheap (await (priced 5 []))
              fetching (fn [run] (-> (first @(:digs run)) (update :ways (comp boolean :chest)) (select-keys [:ways :need-drop])))]
          (is (= [true true true] (map (comp :arrived deref :out) [one two dear])))
          (is (= [{:ways false :need-drop false} {:ways false :need-drop false} {:ways false :need-drop false} {:ways true :need-drop true}]
                 (map fetching [one two dear cheap]))
              "a carried pickaxe digs quicker than a wooden one got; a pickaxe dear to get is not fetched, the rock dug by hand; a cheap one is fetched and needed")
          (is (= #{false} (set (map :collect @(:digs one)))) "rock nobody else owns is left as dug: nothing is put back")
          (is (= #{#{:lava-adjacent :fluid-adjacent}} (set (map :accept @(:digs one)))) "seen lava beside a route cell is sealed by the dig, water beside was priced as its flood: neither is a refusal that ends the route"))))))

(deftest a-dig-route-through-another-s-zone-picks-up-its-drops-for-the-put-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen digs]} (await (dig-route-run! below-pocket [{:name "stone_pickaxe" :count 1}] nil [{:name "vault" :owner "Miles" :min [-60 0 -60] :max [60 120 60]}]))]
          (is (= #{true} (set (map :collect @digs))))
          (is (seq (mapcat :cells (events-of seen :go-to.restored))) "the dug blocks are carried, so the put-back places some (old code left every drop on the ground)"))))))

(deftest a-dig-route-cell-a-look-leaves-unseen-is-dug-and-the-lava-behind-it-sealed
  ;; [3 62 0] stays unseen until dug whatever the body looks at (occluded, too old); lava at [4 62 0] beside it shows only
  ;; once that cell is dug: a player mining digs into unknown rock, the dig child seals what it opens
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (assoc below-pocket "4,62,0" "lava")
              sense (fn [p]
                      (let [sensed (.-sensedAt p)]
                        (aset p "sensedAt" (fn [q] (let [cell [(.-x q) (.-y q) (.-z q)]]
                                                     (if (and (#{[3 62 0] [4 62 0]} cell) (not= "air" (block p [3 62 0])))
                                                       #js {:unknown true}
                                                       (sensed q)))))))
              {:keys [out p seen]} (await (dig-route-run! blocks [{:name "stone_pickaxe" :count 1} {:name "cobblestone" :count 4}] sense))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [3 60 0] (feet p)))
          (is (= "air" (block p [3 62 0])) "dug as guessed rock")
          (is (= "cobblestone" (block p [4 62 0])) "the lava it laid open is sealed")
          (is (seq (events-of seen :blocks.dig.sealed))))))))

(deftest a-dig-route-cell-whose-floor-is-unseen-is-not-dug-on-top-while-a-side-stand-exists
  ;; a floor shows only once its cell is dug; with a cell to stand on beside, a player steps aside and looks first (a 1-wide shaft is the live case sealed-pocket-below-is-dug-down-to)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hide (fn [p]
                     (let [sensed (.-sensedAt p)]
                       (aset p "sensedAt" (fn [q] (if (and (= [3 61 0] [(.-x q) (.-y q) (.-z q)]) (not= "air" (block p [3 62 0])))
                                                    #js {:unknown true}
                                                    (sensed q))))))
              pick [{:name "stone_pickaxe" :count 1}]
              seen-floors (await (dig-route-run! below-pocket pick nil))
              unseen-floor (await (dig-route-run! below-pocket pick hide))
              under-feet #(set (map (fn [d] (contains? (:accept d) :under-feet)) @(:digs %)))]
          (is (= #{false} (under-feet seen-floors)))
          (is (= #{false} (under-feet unseen-floor)) "the bend leaves a cell to step aside to and look from"))))))

(defn hiding-with-pickaxe [blocks hidden?]
  (let [p (tu/fake {:blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]})]
    (aset p "sensedAt" (fn [pos] (if (hidden? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    p))

(deftest a-dig-route-does-not-step-on-a-floor-the-body-has-not-seen-under-seen-air
  (let [world (apply dissoc (box 0 60 -1 6 70 1 "stone") (for [x (range 3) y [64 65]] (str x "," y ",0")))
        p (hiding-with-pickaxe world #(= [2 63 0] %))
        stands (map :to (:steps (choose/route-answer p [0 64 0] [3 64 0] (constantly true) (constantly false) false false nil)))]
    ;; it may dig down into the unseen cell, onto the seen rock under it
    (is (seq stands))
    (is (not-any? #{[2 64 0]} stands) "no step stands on the unseen floor under the seen-open cell")))

(deftest a-far-walled-in-goal-is-searched-for-a-slice-a-call-and-reached
  ;; a body in a walled yard on a stone mass, the goal a sealed pocket 80 across and 12 down: each escalate! call's dig
  ;; route searches expand at most the dig-route ::max-nodes setting, a search that needs more goes on in the next
  ;; calls, and the approaches toward the goal do not wander round the yard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box -10 50 -6 90 63 6 "stone") (box -4 64 -4 4 66 4 "stone") (box -3 64 -3 3 66 3 "air")
                            {"80,52,0" "air" "80,53,0" "air"})
              nodes (atom 0) per-call (atom [])
              search dig-route/search escalate! esc/escalate!
              {:keys [out p seen]}
              (with-redefs [dig-route/search (fn [& args] (let [run (apply search args)] (fn [n] (let [[_ used :as r] (run n)] (swap! nodes + used) r))))
                            esc/escalate! (fn [c pos failed]
                                            (let [before @nodes]
                                              (.then (escalate! c pos failed) (fn [r] (swap! per-call conj (- @nodes before)) r))))]
                (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]}
                                 {:pos [80 52 0] :range 0})))]
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text])))
          (is (= [80 52 0] (feet p)))
          (is (every? #(<= % (settings/get dig-route/settings ::dig-route/max-nodes)) @per-call) (pr-str @per-call))
          (is (some #(> % (quot (settings/get dig-route/settings ::dig-route/max-nodes) 2)) @per-call) "a search spent most of a call's slice and went on")
          (is (< (count (events-of seen :go-to.escalated)) 8) (pr-str (map (juxt :step :at) (events-of seen :go-to.escalated)))))))))

(deftest a-body-shut-in-on-a-plateau-digs-down-to-the-seen-plain-toward-a-goal-too-far-for-one-route
  ;; seeing 25 round it, the pocket 40 off reads as through rock (no dig route); the plain below the edge is seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box 0 50 0 49 54 39 "stone") (box 0 55 0 20 60 20 "stone") {"40,51,30" "air" "40,52,30" "air"})
              {:keys [out p seen]} (await (run-job! {:sight 25 :self {:pos {:x 10.5 :y 61 :z 10.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [40 51 30] :range 0}))]
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text :escalation])))
          (is (= [40 51 30] (feet p)))
          (is (= {:step :dig-route :at [10 61 10]} (select-keys (first (events-of seen :go-to.escalated)) [:step :at])) "the first way is the dig off the edge"))))))

(deftest a-dig-route-planned-again-over-the-slice-still-reaches-a-far-goal
  ;; the stone over the pocket is dirt, and the cell 4 down shows only once the cell above is dug (a guessed cell seen
  ;; otherwise): with a slice of 30 nodes the route from there is planned on over several rounds, not read as no route
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box -10 50 -6 90 63 6 "stone") (box -4 64 -4 4 66 4 "stone") (box -3 64 -3 3 66 3 "air")
                            (box 78 54 -2 82 63 2 "dirt") {"80,52,0" "air" "80,53,0" "air"})
              {:keys [eng p]} (setup {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]})
              sensed (.-sensedAt p)
              _ (aset p "sensedAt" (fn [q] (if (and (= 60 (.-y q)) (= "dirt" (block p [(.-x q) (.-y q) (.-z q)])) (not= "air" (block p [(.-x q) (inc (.-y q)) (.-z q)])))
                                             #js {:unknown true}
                                             (sensed q))))
              out (atom :not-done)
              slice 30
              nodes (atom 0) search dig-route/search replans (atom 0) replan esc/replan-route!
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [80 52 0] :range 0})))]
          (with-redefs [dig-route/search (fn [& args] (let [run (apply search args)] (fn [n] (let [[_ used :as r] (run n)] (swap! nodes + used) r))))
                        esc/replan-route! (fn [& args] (swap! replans inc) (apply replan args))]
            (core/submit! eng (tu/order '(recording-parent {})) {})
            (await (settings/with-settings {::dig-route/max-nodes slice} #(tick-out! eng 3000))))
          (is (> @replans 1) "the dirt sent the route to be planned again, and a plan went on in a later round")
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text])))
          (is (= [80 52 0] (feet p))))))))

(deftest a-free-body-walks-to-the-cell-nearest-a-walled-in-goal-when-it-sees-no-way-from-there
  ;; out of a walled yard on a wide floor, the goal a pocket in a stone block 40 across: seeing 30 blocks round it, the
  ;; land by the block reads as rock and no dig route shows from the nearest cell; it walks there and looks, not round the yard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box 0 60 0 63 62 39 "stone") (box 2 63 2 10 65 10 "stone") (box 3 63 3 9 65 9 "air")
                            (box 44 63 26 50 65 32 "stone") {"47,63,29" "air" "47,64,29" "air"})
              {:keys [out p seen]} (await (run-job! {:sight 30 :self {:pos {:x 6.5 :y 63 :z 6.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [47 63 29] :range 0}))]
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text])))
          (is (= [47 63 29] (feet p)))
          (is (<= (count (filter #{:approach} (map :step (events-of seen :go-to.escalated)))) 4) "it walks to the nearest cell and looks, not round the yard"))))))

(deftest a-dig-route-over-guesses-is-priced-once-in-the-search-at-the-chance-each-breaks-it
  ;; a guess not diggable breaks the route where it lies: at its class's risk, once, the look that shows it and the replan
  ;; round it (two digs of the rock beside it and a step), whatever its distance from the route's start (here 9 and 10 blocks off);
  ;; :share-s leaves those misses out. A guess beside seen undiggable rock (a chest) is of class :placed
  (let [world (assoc (box 0 60 -1 13 70 1 "stone") "0,64,0" "air" "0,65,0" "air" "12,64,0" "air" "12,65,0" "air" "10,66,0" "chest")
        hidden #{[9 64 0] [9 65 0] [10 64 0] [10 65 0]}
        p (hiding-with-pickaxe world hidden)
        answer #(choose/route-answer p [0 64 0] [12 64 0] (constantly true) (constantly false) false false %)
        risk {:placed 0.2 :apart 0.2}
        plain (answer {})
        {:keys [seconds share-s dig]} (answer {:guess-risk risk :worth-s tu/order-worth})
        stone-s (/ (tools/clear-ms p "stone") 1000)
        misses (reduce + (map #(* 0.2 (+ (cost/look-s 1) (* 2 stone-s) (cost/walk-step-s))) hidden))
        priced (fn [worth] (some? (answer {:guess-risk risk :worth-s worth})))]
    (is (= 22 (count (:dig plain))))
    (is (= (set dig) (set (:dig plain))) "the straight tunnel, under the light, at this risk still the cheapest")
    (is (< (js/Math.abs (- seconds (:seconds plain) misses)) 1e-6) "each guess pays its risk of a look and a replan, once")
    (is (< (js/Math.abs (- share-s (:seconds plain))) 1e-6) ":share-s is the route without its misses")
    (is (priced (* 1.01 seconds)) "priced once: no second factor on the route")
    (is (not (priced (* 0.99 seconds))))
    (is (not-any? hidden (:dig (answer {:guess-risk {:placed 0.9 :apart 0.9} :worth-s tu/order-worth}))) "guesses that nearly always fail are dug round through seen rock")
    (is (= [[10 65 0]] (map :cell (filter (comp #{:placed} :class) (choose/route-cells p [0 64 0] [12 64 0] (constantly true) (constantly false))))))))

(deftest a-dig-route-search-ends-once-the-cheapest-open-cell-passes-its-worth
  ;; the same walled pocket 10 off through stone: a worth over the dig's price finds the route; a worth under it ends the
  ;; search as no route, after fewer cells than the one that finds it (no radius margin bounds either)
  (let [world (merge (box -2 60 -6 14 70 6 "stone") {"0,64,0" "air" "0,65,0" "air" "10,64,0" "air" "10,65,0" "air"})
        p (tu/fake {:blocks world :inventory [{:name "stone_pickaxe" :count 1}]})
        nodes (atom 0)
        search dig-route/search
        answer (fn [worth] (reset! nodes 0)
                 (with-redefs [dig-route/search (fn [& args] (let [run (apply search args)] (fn [n] (let [[_ used :as r] (run n)] (swap! nodes + used) r))))]
                   (let [a (choose/route-answer p [0 64 0] [10 64 0] (constantly true) (constantly false) false false {:bound-s worth})]
                     [a @nodes])))
        [found found-nodes] (answer ##Inf)
        [dear dear-nodes] (answer 5)]
    (is (map? found))
    (is (nil? dear))
    (is (< dear-nodes found-nodes))))

(deftest a-stair-over-guesses-is-priced-over-the-chance-they-all-hold-and-at-zero-risk-by-its-price
  ;; the stair east from [0 64 0] two steps cuts [0 66 0] [1 66 0] [1 65 0] [1 67 0] [2 67 0] [2 66 0]; three are hidden,
  ;; [2 66 0] under a seen light (class :placed): 10 s holds with 0.8 * 0.8 * 0.5 = 0.32
  (let [p (hiding-with-pickaxe (assoc (box 0 60 -1 9 70 1 "stone") "0,64,0" "air" "0,65,0" "air" "2,67,0" "chest")
                               #{[1 65 0] [1 66 0] [2 66 0]})
        guesses (choose/way-guesses p {:step :stair :heading :east :steps 2} [0 64 0])
        risk {:placed 0.5 :apart 0.2}]
    (is (= {[1 65 0] :apart [1 66 0] :apart [2 66 0] :placed} (into {} (map (juxt :cell :class)) guesses)))
    (is (choose/past-worth? 10 guesses risk 31))
    (is (not (choose/past-worth? 10 guesses risk 32)))
    (is (choose/past-worth? 41 guesses {:placed 0 :apart 0} 38.8) "no look has failed: a stair dearer than the worth is still none")
    (is (not (choose/past-worth? 10 guesses {:placed 0 :apart 0} 10)))
    (is (choose/past-worth? 10 guesses {:placed 1 :apart 0} 1e9) "a guess sure to fail: past any worth")))

(deftest the-guesses-a-stair-reached-count-as-looks-the-cell-it-could-not-dig-a-bad-one
  (let [{:keys [eng]} (gt/setup {:blocks {}})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
        e {:step :stair :guesses [{:cell [1 65 0] :class :apart} {:cell [2 66 0] :class :placed} {:cell [3 67 0] :class :apart}]}]
    (.setOwner (:primitives eng) "tx")
    (esc/note-guesses! c e {:kind :stopped :cause {:reason :not-diggable :cell [2 66 0] :block "chest"}
                            :data {:dug [{:cell [1 65 0] :block "stone"}]}})
    (is (= {:placed {:all 1 :bad 1} :apart {:all 1 :bad 0}} (:guess-looks (ctx/mem c))) "the cell past the stop is no look")))

(deftest a-cut-dig-route-through-the-bodys-own-build-is-walked-before-its-holes-are-put-back
  (let [{:keys [eng]} (gt/setup {:blocks {}})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
        run! (fn [e & [done?]] (ctx/update-mem! c #(-> % (dissoc :restore-now :restore-pending) (assoc :escalation e)))
               (esc/escalated! c [20 2 10] e (not (false? done?)))
               (select-keys (ctx/mem c) [:restore-now :restore-pending]))]
    (.setOwner (:primitives eng) "tx")
    (is (= {:restore-pending true} (run! {:step :dig-route :own-cut true})) "walk through first")
    (is (= {:restore-now true :restore-pending true} (run! {:step :dig-route :own-cut true} false)) "a route stopped in a dead end is closed behind at once")
    (is (= {:restore-now true :restore-pending true} (run! {:step :stair :own-cut true})) "a stair's cut is closed behind at once")))

;; card 22c43e75: a claim 17 wide across the line is searched round, each re-search refusing the cells the last route dug in it; a
;; count of three gave up there
(deftest a-claim-wider-than-three-searches-is-dug-round
  (let [p (tu/fake {:blocks (assoc (box -2 62 -7 12 67 7 "stone") "0,64,0" "air" "0,65,0" "air" "10,64,0" "air" "10,65,0" "air") :inventory [{:name "stone_pickaxe" :count 1}]})
        claim? (fn [[x y z]] (and (<= 4 x 5) (<= -3 z 3)))
        cells (choose/route-cells p [0 64 0] [10 64 0] #(not (claim? %)) (constantly false) false false {:bound-s 200})]
    (is (seq cells))
    (is (not-any? (comp claim? :cell) cells))))

(deftest the-put-back-holds-dug-route-cells-the-walk-has-still-to-enter
  ;; a U-shaped dug route: the body, part way along, has a cell still to enter that lies farther from the goal than the feet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (gt/setup {:self {:pos {:x 0.5 :y 64 :z 4.5}} :blocks (box -2 60 -2 12 70 6 "stone")})
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:range 0} :round 1})
              goal {:x 10 :y 64 :z 0}
              route [[0 64 2] [0 64 3] [0 64 4] [1 64 4] [2 64 4]]
              hole (fn [cell] {:cell cell :action :dig :was "stone" :zone "vault" :escalation true :go-to "j9"})]
          (.setOwner (:primitives eng) "tx")
          (doseq [cell route] (tidy/record! c (hole cell) 0))
          (ctx/update-mem! c assoc :route-ahead route)
          (is (true? (restore/ahead-cell? c goal [2 64 4])) "a route cell ahead is held though it is no nearer the goal than the feet")
          (is (true? (restore/ahead-cell? c goal [1 64 4])) "so is the next one")
          (is (false? (restore/ahead-cell? c goal [0 64 3])) "a cell behind the body is put back")
          (ctx/update-mem! c assoc :restore-now true :restore-seen (into #{} (take 2) route))
          (is (= :again (await (restore/restore-round! c goal))))
          (is (= :again (await (restore/restore-round! c goal))))
          (is (true? (:restore-held (ctx/mem c))) "the route cell ahead is held, not filled")
          (is (contains? (into #{} (map :cell) (tidy/entries c)) [2 64 4]) "it stays in the ledger for the arrival"))))))

(deftest the-dug-routes-of-a-climb-and-of-earlier-ways-are-all-held-ahead
  (let [{:keys [eng]} (gt/setup {:blocks {}})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
        cells (fn [& xs] (mapv (fn [x] {:cell [x 64 0]}) xs))
        run! (fn [e] (ctx/update-mem! c assoc :escalation e) (esc/escalated! c [20 2 10] e true) (:route-ahead (ctx/mem c)))]
    (.setOwner (:primitives eng) "tx")
    (is (= [[1 64 0] [2 64 0]] (run! {:step :stair :then [{:step :dig-route :cells (cells 1 2)}]})) "a later way's route is held")
    (is (= [[1 64 0] [2 64 0] [3 64 0]] (run! {:step :dig-route :cells (cells 3)})) "a later route adds to the earlier route's unentered cells")))

(deftest a-dig-route-whose-guessed-floor-is-seen-to-be-air-digs-no-more-and-is-planned-again
  ;; a dig of the stand's head and feet over the floor it guessed rock, which a look into a hole dug before showed is air
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (gt/setup {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                                       :blocks (assoc (box -1 62 -1 4 65 1 "stone") "0,64,0" "air" "0,65,0" "air" "1,63,0" "air")})
              p (:primitives eng)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})]
          (.setOwner p "tx")
          (ctx/update-mem! c assoc
                           :replan-budget {:since (ctx/now c) :cells 0 :fallback-s 1000}
                           :escalation {:step :dig-route :from [0 64 0] :then [{:step :approach :pos [3 64 0] :from [0 64 0]}]
                                        :cells [{:cell [1 65 0] :block "stone" :guess false}
                                                {:cell [1 64 0] :block "stone" :guess false :floor-guess [1 63 0]}]})
          (is (= :again (await (esc/dig-route-round! c [3 64 0]))))
          (is (= ["stone" "stone"] [(tu/true-block-name p 1 65 0) (tu/true-block-name p 1 64 0)]) "nothing more of the route is dug")
          (is (nil? (:escalation (ctx/mem c))) "the climb is searched again from what the body sees now"))))))

(deftest a-dig-route-that-digs-this-call-s-pillar-block-rules-the-pillar-out
  ;; run4 F14b: a pillar out of a pocket, then a dig route in whose first cell is that pillar block, ten times over
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (gt/setup {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                                            :blocks (assoc (box -1 61 -1 1 62 1 "stone") "0,63,0" "cobblestone")})
              p (:primitives eng)
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})]
          (.setOwner p "tx")
          (esc/note-pillar! c {:kind :done :data {:status :done :cells [[0 63 0]] :built 1 :height 1}})
          (ctx/update-mem! c assoc :escalation {:step :dig-route :cells [{:cell [0 63 0] :block "cobblestone" :guess false}]})
          (with-redefs [b/dig-cell-ending! (fn [_ _ _] (swap! (fake/state p) assoc-in [:blocks "0,63,0"] "air") (js/Promise.resolve {:kind :done :data {:dug true}}))]
            (is (= :again (await (esc/dig-route-round! c [0 50 0])))))
          (is (contains? (choose/skips c) :pillar) "the next way is not a pillar")
          (is (= [[0 63 0]] (map :cell (events-of seen :go-to.pillar-undone)))))))))

(deftest a-climb-s-next-stair-over-a-guessed-floor-seen-to-be-air-is-not-started-and-the-ways-are-chosen-again
  (let [{:keys [eng seen]} (gt/setup {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (assoc (box -1 62 -1 4 63 1 "stone") "2,63,0" "air")})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
        run! (fn [floor]
               (ctx/update-mem! c #(-> % (dissoc :escalate-now) (assoc :escalation {:step :approach :then [{:step :stair :heading :east :steps 1 :from [0 64 0] :guessed [floor]}]})))
               (reset! seen [])
               (esc/escalated! c [0 64 0] (:escalation (ctx/mem c)) true)
               (ctx/mem c))]
    (.setOwner (:primitives eng) "tx")
    (let [m (run! [2 63 0])]
      (is (= [[2 63 0]] (map :floor (events-of seen :go-to.climb-replanned))) "the guessed floor is seen to be air")
      (is (empty? (events-of seen :go-to.escalated)) "the stair is not started")
      (is (nil? (:escalation m)) "the rest of the climb is dropped")
      (is (true? (:escalate-now m)) "the ways are chosen again at once"))
    (let [m (run! [1 63 0])]
      (is (empty? (events-of seen :go-to.climb-replanned)) "a floor that held: the climb goes on")
      (is (= :stair (:step (:escalation m))))
      (is (nil? (:escalate-now m))))))

(deftest a-dig-route-goes-round-cells-may-dig-refuses
  (let [world (assoc (box 0 60 -1 9 70 1 "stone") "0,64,0" "air" "0,65,0" "air" "4,64,0" "air" "4,65,0" "air")
        p (tu/fake {:blocks world :inventory [{:name "stone_pickaxe" :count 1}]})
        straight (set (map :cell (choose/route-cells p [0 64 0] [4 64 0] (constantly true) (constantly false))))
        round (choose/route-cells p [0 64 0] [4 64 0] #(not (contains? straight %)) (constantly false))]
    (is (= #{[1 64 0] [1 65 0] [2 64 0] [2 65 0] [3 64 0] [3 65 0]} straight))
    (is (seq round) "the refused tunnel is dug round")
    (is (not-any? (comp straight :cell) round))))

(deftest a-dig-beside-water-is-priced-as-its-flood-and-a-goal-ringed-by-water-is-dug-to
  ;; KitOak94: the only way to the ore went beside water; a refused dig there left the ore unreachable
  (let [hp-s (cost/hp-seconds)
        p (tu/fake {:blocks {"0,65,0" "water"} :inventory [{:name "stone_pickaxe" :count 1}]})
        flood-s (partial dig-route/flood-s (:flood (dig-route/body-prices p (constantly false))))
        open-s (cost/flood-dig-s {} 20 hp-s)
        ;; a one-wide row under three water cells, the rock over them too high to dig round: the row is the only way
        world (assoc (box 0 63 0 5 90 0 "stone") "0,64,0" "air" "0,65,0" "air" "4,64,0" "air" "4,65,0" "air"
                     "1,66,0" "water" "2,66,0" "water" "3,66,0" "water")
        route (choose/route-cells (tu/fake {:blocks world :inventory [{:name "stone_pickaxe" :count 1}]}) [0 64 0] [4 64 0] (constantly true) (constantly false))]
    (is (= open-s (flood-s [[:dig [0 64 0] "stone"]])) "the cell under the water costs its flood over a dry one")
    (is (= open-s (flood-s [[:dig [1 65 0] "stone"]])) "water beside floods it too")
    (is (zero? (flood-s [[:dig [0 66 0] "stone"]])) "water under it does not rise into it")
    (is (< (js/Math.abs (- (flood-s [[:dig [0 64 0] "stone"] [:dig [1 65 0] "stone"]])
                           open-s (cost/wet-dig-s {} 20 hp-s (cost/flood-under-s {}) (cost/dig-cost "stone"))))
           1e-9)
        "a second cut beside the pool, once its water is in, is a wet dig: the flood is paid once")
    (is (pos? (cost/flood-dig-s {} 20 hp-s)) "the flood is swum, not walked")
    (is (< (cost/flood-dig-s {} 20 hp-s) (cost/flood-dig-s {:air-used 14.5} 20 hp-s)) "with its air nearly spent the body drowns some hp")
    (is (= ##Inf (cost/flood-dig-s {:air-used 14.5} 2 hp-s)) "a flood the body does not live through")
    (is (< (js/Math.abs (- (cost/wet-dig-s {} 20 hp-s 0 1) 4)) 1e-9) "a dig inside the flood is made under water, 5 times as long")
    (is (zero? (cost/wet-dig-s {:dig-pace 1} 20 hp-s 0 1)) "Aqua Affinity digs under water as fast as dry, with air to spare")
    (is (< 8 (cost/wet-dig-s {:air-used 10} 20 hp-s 0 2)) "a slow dig under water outlasts the air left and drowns")
    (is (= [5 1] [(cost/wet-dig-pace nil) (cost/wet-dig-pace {:head {:name "iron_helmet" :enchants [{:name "aqua_affinity" :level 1}]}})]))
    (is (some #{[2 65 0]} (map :cell route)) "the goal is dug to under the water")))

(deftest every-dig-of-a-stair-down-inside-the-water-it-let-in-is-priced-wet
  ;; card 9a274838: a pool over the first cut pours down the stair; the flood priced one wet dig after it, the rest dry
  (let [rock (assoc (box 0 59 0 3 70 0 "stone") "0,64,0" "air" "0,65,0" "air")
        route-s (fn [world]
                  (let [p (tu/fake {:blocks world :inventory [{:name "stone_pickaxe" :count 1}]})
                        prices (dig-route/body-prices p (constantly false))]
                    [(:seconds (dig-route/route (fn [[x y z]] (u/seen-name p {:x x :y y :z z})) [0 64 0] [3 61 0] (assoc prices :margin 0)))
                     (:flood prices)]))
        [dry-s] (route-s rock)
        [wet-s flood] (route-s (assoc rock "1,66,0" "water"))
        open (dig-route/flood-s flood [[:dig [1 65 0] "stone"]])]
    (is (pos? open) "the first cut lets the pool in")
    (is (<= (+ open (* 4 4 (cost/dig-cost "stone"))) (- wet-s dry-s))
        "the stair's next two columns, at least four digs, are dug under water at 5 times their dry seconds")))

(deftest a-flooded-way-swims-its-steps-under-the-water-and-breathes-where-its-head-is-over-it
  ;; card 8b609f09: a walked step inside the flood was priced dry and the time under water never fell
  (let [hp-s (cost/hp-seconds)
        flood-s (partial dig-route/flood-s (:flood (dig-route/body-prices (tu/fake {:blocks {"1,66,0" "water"}}) (constantly false))))
        open [:dig [1 65 0] "stone"]
        swum [:step [1 64 0]]
        ;; enough swum steps to outlast the air and 20 hp drowned at 2 a second
        n (js/Math.ceil (/ (+ (cost/air-left-s {}) 10) (cost/wet-move-s {} :walk)))
        half (repeat (js/Math.ceil (/ n 2)) swum)]
    (is (pos? (cost/wet-step-s {} 20 hp-s 0)) "a step under water is swum, slower than walked")
    (is (< (js/Math.abs (- (flood-s [open swum]) (flood-s [open]) (cost/wet-step-s {} 20 hp-s (cost/flood-under-s {})))) 1e-9)
        "a step whose head is under the pool's level is swum")
    (is (= ##Inf (flood-s (cons open (repeat n swum)))) "a way that never surfaces drowns")
    (is (< (flood-s (concat [open] half [[:step [1 66 0]]] half)) ##Inf) "a step with the head over the water breathes")))

(deftest water-let-in-floods-only-as-far-as-it-flows
  ;; card f9ca065d: every dig at or under the pool's level was wet, however far: a way round far from the water was priced
  ;; as if under it
  (let [flood-s (partial dig-route/flood-s (:flood (dig-route/body-prices (tu/fake {:blocks {"1,66,0" "water"}}) (constantly false))))
        open [:dig [1 65 0] "stone"]
        dry (flood-s [open])]
    (is (< dry (flood-s [open [:dig [8 65 0] "stone"]])) "as far as water flows on level ground, the dig is wet")
    (is (= dry (flood-s [open [:dig [9 65 0] "stone"]])) "a block past that it is dry")
    (is (< dry (flood-s [open [:dig [15 64 0] "stone"]])) "a block lower it flows as far again")
    (is (= dry (flood-s [open [:dig [16 64 0] "stone"]])))
    (is (= dry (flood-s [open [:step [20 64 0]]])) "a step past its reach is walked")))

(deftest a-second-opening-within-the-reach-widens-the-flood
  ;; the flood kept its first opening as origin: a cell only the second opening's water reaches was priced dry
  (let [flood (:flood (dig-route/body-prices (tu/fake {:blocks {"1,66,0" "water" "8,66,0" "water"}}) (constantly false)))
        dig (fn [[_ level under origin] cell] (dig-route/flood-dig flood level under origin cell "stone" 66))
        [_ level _ origin] (-> (dig-route/flood-dig flood nil 0 nil [1 65 0] "stone" 66) (dig [8 65 0]))
        reaches? (partial cost/flood-reaches? origin level)]
    (is (reaches? [15 65 0]) "7 across from the second opening is wet")
    (is (reaches? [-6 65 0]) "7 across from the first, still wet")
    (is (not (reaches? [16 65 0])) "past both reaches it is dry")))

(deftest an-origin-is-within-another-when-its-water-reaches-no-cell-the-others-does-not
  ;; a label whose flood is within another's is no worse on it: dominance by origin keeps the joined origins from multiplying labels
  (is (cost/flood-origin-within? [0 66 0] [0 66 0]))
  (is (cost/flood-origin-within? [0 66 0] [0 66 0 3]) "the same opening with less reach")
  (is (not (cost/flood-origin-within? [0 66 0 3] [0 66 0])))
  (is (cost/flood-origin-within? [2 66 0] [0 66 0 2]) "an opening 2 across lies inside a reach 2 wider")
  (is (not (cost/flood-origin-within? [3 66 0] [0 66 0 2])))
  (is (cost/flood-origin-within? [0 65 0] [0 66 0 0]) "a lower opening reaches 7 less across at each height, so it fits 7 under"
      )
  (is (not (cost/flood-origin-within? [0 67 0] [0 66 0 0])) "a higher opening reaches farther"))

(deftest a-fast-swimmer-pays-no-negative-price-for-time-under-water
  ;; wet-s lost its floor: swimming faster than the dry walk it replaces priced the flood below nothing
  (let [hp-s (cost/hp-seconds)]
    (is (zero? (cost/wet-s {} 20 hp-s 0 10 1)) "a wet leg quicker than the dry one adds nothing")
    (is (not (neg? (cost/flood-dig-s {} 20 hp-s))))))

(deftest a-dug-legs-cells-are-priced-with-the-flood-the-first-lets-in
  ;; the dig leg is booked from these prices: the opening's seconds and each later dig inside the water count, not :dig-s alone
  (let [p (tu/fake {:blocks {"1,66,0" "water"} :inventory [{:name "stone_pickaxe" :count 1}]})
        prices (dig-route/body-prices p (constantly false))
        cells [[1 65 0] [1 64 0] [1 63 0]]
        priced (dig-route/cells-s prices (constantly "stone") cells)
        dry (map #((:dig-s prices) "stone" %) cells)]
    (is (< (apply + dry) (apply + priced)))
    (is (< (js/Math.abs (- (first priced) (first dry) (dig-route/flood-s (:flood prices) [[:dig (first cells) "stone"]]))) 1e-9)
        "the first cell carries the opening")
    (is (< (js/Math.abs (- (apply + priced) (apply + dry) (dig-route/flood-s (:flood prices) (map #(vector :dig % "stone") cells)))) 1e-9)
        "the sum over the cells is the route's flood")))

(deftest a-dig-route-dearer-than-its-worth-is-not-planned
  (let [world (assoc (box 0 60 -1 9 70 1 "stone") "0,64,0" "air" "0,65,0" "air")
        route (fn [inventory] (choose/route-cells (tu/fake {:blocks world :inventory inventory}) [0 64 0] [8 64 0] (constantly true) (constantly false) false false {:worth-s 30}))]
    (is (nil? (route [])) "8 stones by hand take minutes")
    (is (seq (route [{:name "stone_pickaxe" :count 1}])) "the same take seconds with a pickaxe")))
(deftest a-dig-route-through-the-bodys-own-scaffold-is-not-held-to-the-worth
  (let [world (assoc (box 0 60 -1 9 70 1 "cobblestone") "0,64,0" "air" "0,65,0" "air")
        route (fn [own?] (choose/route-cells (tu/fake {:blocks world :inventory []}) [0 64 0] [8 64 0] (constantly true) own? false false {:worth-s 30}))]
    (is (nil? (route (constantly false))) "8 foreign blocks by hand take minutes")
    (is (seq (route (constantly true))) "its own pillar left behind is dug out however long it takes")))

(deftest a-route-through-the-bodys-own-blocks-reports-their-dig-time
  (let [world (assoc (box 0 60 -1 9 70 1 "cobblestone") "0,64,0" "air" "0,65,0" "air")
        p (tu/fake {:blocks world :inventory []})
        seconds (fn [own?] (:seconds (choose/route-answer p [0 64 0] [8 64 0] (constantly true) own? false false {:worth-s 30})))
        mine (seconds (constantly true))]
    (is (nil? (seconds (constantly false))) "foreign blocks are held to the worth")
    (is (> mine (* 7 (/ (tools/clear-ms p "cobblestone") 1000))) "the planks it dug its way out through are in the price, as stairs pay theirs")))

(deftest a-dig-route-is-priced-with-the-pickaxe-the-body-would-fetch
  (let [world (assoc (box 0 60 -1 40 70 1 "stone") "0,64,0" "air" "0,65,0" "air")
        route (fn [inventory fetch?] (choose/route-cells (tu/fake {:blocks world :inventory inventory}) [0 64 0] [8 64 0] (constantly true) (constantly false) false fetch? {:worth-s 30}))]
    (is (nil? (route [] nil)) "shut in: bare hands cannot tunnel 12 stones")
    (is (seq (route [] (delay 5))) "a free body crafts a pickaxe and tunnels")
    (is (nil? (route [] (delay 60))) "a pickaxe dearer to get than the route is worth is no way to tunnel")
    (is (seq (route [{:name "wooden_pickaxe" :count 1}] nil)) "a wooden pickaxe tunnels 12 blocks")
    (is (seq (route [{:name "stone_pickaxe" :count 1}] nil)) "a stone pickaxe tunnels 12 blocks")))

(deftest the-pickaxe-fetch-price-is-asked-once-while-the-body-and-its-world-hold
  (let [asked (atom 0)
        ctx-of (fn [inventory]
                 (let [{:keys [eng p]} (gt/setup {:blocks (box 0 60 -1 4 70 1 "stone") :inventory inventory})]
                   [p (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})]))
        [p c] (ctx-of [])
        price (fn [c] @(choose/pickaxe-fetch c true))]
    (with-redefs [need/item-price (fn [& _] (swap! asked inc) 7)]
      (is (= [7 7] [(price c) (price c)]))
      (is (= 1 @asked) "a second quote and round read the price the first made")
      (price (second (ctx-of [{:name "oak_log" :count 3}])))
      (is (= 2 @asked) "another body asks again")
      (swap! (fake/state p) update :inventory fake/add-to "oak_log" 3)
      (price c)
      (is (= 3 @asked) "carrying logs now changes what a pickaxe costs")
      (swap! (fake/state p) assoc-in [:self :health] 6)
      (price c)
      (is (= 4 @asked) "a hit changes what the walk there risks"))))

(deftest a-pocket-floor-seen-only-from-inside-is-no-bar-when-floors-unseen-are-ok
  (let [world (apply dissoc (box 0 60 -1 6 70 1 "stone") (for [x (range 3) y [64 65]] (str x "," y ",0")))
        p (hiding-with-pickaxe world #(= [2 63 0] %))
        stands #(map :to (:steps (choose/route-answer p [0 64 0] [3 64 0] (constantly true) (constantly false) % false nil)))]
    (is (not-any? #{[2 64 0]} (stands false)))
    (is (some #{[2 64 0]} (stands true)) "it goes straight over the unseen floor")))

(deftest a-dig-route-walks-on-into-a-pocket-whose-floor-is-seen-only-from-inside
  ;; the pocket [3 60 0] [3 61 0] reads as rock until [3 62 0] is dug, then shows open; its floor [3 59 0] never shows: the
  ;; replan finds no strict route, so it plans with floors unseen ok and walks on, counted as a replan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sense (fn [p]
                      (let [sensed (.-sensedAt p)]
                        (aset p "sensedAt" (fn [q] (let [cell [(.-x q) (.-y q) (.-z q)]]
                                                     (if (or (= [3 59 0] cell) (and (#{[3 60 0] [3 61 0]} cell) (not= "air" (block p [3 62 0]))))
                                                       #js {:unknown true}
                                                       (sensed q)))))))
              pick [{:name "stone_pickaxe" :count 1}]
              run #(dig-route-run! below-pocket pick sense)
              walked (await (run))
              capped (with-redefs [search-bound/fallback-s (constantly 0)] (await (run)))]
          (is (true? (:arrived @(:out walked))) (pr-str @(:out walked)))
          (is (= [3 60 0] (feet (:p walked))))
          (is (empty? (events-of (:seen walked) :go-to.escalation-stopped)))
          (is (some #(= [:dig-route :replans-spent] ((juxt :step :reason) %)) (events-of (:seen capped) :go-to.escalation-stopped))
              "the walk on is a replan, so replans that cost what the other way does end it"))))))

(deftest a-dig-route-walk-on-never-stands-in-a-pocket-whose-unseen-floor-is-void-or-lava
  ;; as the test above, but the floor [3 59 0] is a void (air over lava, or lava): the body never ends in the pocket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [floor ["air" "lava"]]
          (let [blocks (cond-> (dissoc below-pocket "3,59,0") (= "lava" floor) (assoc "3,59,0" "lava") (= "air" floor) (assoc "3,58,0" "lava"))
                sense (fn [p]
                        (let [sensed (.-sensedAt p)]
                          (aset p "sensedAt" (fn [q] (let [cell [(.-x q) (.-y q) (.-z q)]]
                                                       (if (or (= [3 59 0] cell) (and (#{[3 60 0] [3 61 0]} cell) (not= "air" (block p [3 62 0]))))
                                                         #js {:unknown true}
                                                         (sensed q)))))))
                {:keys [out p]} (await (dig-route-run! blocks [{:name "stone_pickaxe" :count 1}] sense))]
            (is (not (true? (:arrived @out))) (str floor " " (pr-str @out)))
            (is (not= [3 60 0] (feet p)) floor)
            (is (> (second (feet p)) 59) (str floor " feet " (feet p)))))))))

(deftest every-escalation-child-runs-hurt
  (doseq [e [{:step :pillar :height 2} {:step :stair :heading :east :steps 2}]]
    (is (= '(always) (get-in (second (esc/escalation-job e nil false false false)) [:checks :fit?])) (str (:step e))))
  (is (nil? (:checks (second (esc/escalation-job {:step :approach :pos [1 64 0]} nil false false false)))) "go-to's own fit is ready/ok"))

;; ------------------------------------------------------------------ ore walls (card f14e177c)
(def ore-pit
  "A 2-deep 1x1 pit (feet at y 62) whose walls and floor are coal ore, in stone x -3..3, y 58..63, z -3..3, beside a natural cave (x 2..5, y 60..62) its ore touches; stone ground east of it (x 4..14) topped at the rim (feet y 64)."
  (let [pit-air #{"0,62,0" "0,63,0"}
        cave (for [x (range 2 6) y [60 61 62] z (range -2 3)] (str x "," y "," z))]
    (apply dissoc
           (merge (tu/box 4 58 -3 14 63 3 "stone") (tu/box -3 58 -3 3 63 3 "stone") (tu/box -1 59 -1 2 63 1 "coal_ore"))
           (concat pit-air cave))))

(deftest go-to-climbs-out-of-a-two-deep-pit-in-an-ore-vein
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-job! {:self {:pos {:x 0.5 :y 62 :z 0.5}} :blocks ore-pit :seeing-all true
                                                      :inventory [{:name "iron_pickaxe" :count 1}]}
                                                    {:pos [8 64 0] :range 1}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (< 6 (first (feet p)))))))))

(def hill-stair
  "A stone plain (top y 71) with a dirt hill x 94..110 (y 72..75) and the body's own stair cut up inside them from
  [86 58 0] to its dead end [100 72 0], under 3 blocks of the hill."
  (let [carve (for [k (range 15) y [(+ 58 k) (+ 59 k) (+ 60 k)]] [(+ 86 k) y 0])]
    (apply dissoc (merge (box -60 40 -10 110 71 10 "stone") (box 94 72 -10 110 75 10 "dirt"))
           (map (fn [[x y z]] (str x "," y "," z)) carve))))

(deftest go-to-from-the-top-of-its-own-stair-under-a-hill-digs-out-instead-of-walking-down-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos {:x 100.5 :y 72 :z 0.5}} :blocks hill-stair
                                           :inventory [{:name "iron_pickaxe" :count 1} {:name "cobblestone" :count 4}]})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [40 72 0] :range 1})))
              _ (mem/write! (:store eng) :stair-way {:dim "overworld" :floors (vec (for [k (range 15)] [(+ 86 k) (+ 57 k) 0]))})
              _ (core/submit! eng (tu/order '(recording-parent {})) {})
              _ (await (tick-out! eng 1000))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (<= 72 (second (feet p))))
          (is (= [:stair] (take 1 (mapv :step (events-of seen :go-to.escalated)))) "a pillar under unseen dirt is not first")
          (is (empty? (events-of seen :go-to.retrying)) "the partial walk down its own stair is not taken"))))))

(deftest a-budget-give-up-close-to-the-goal-reads-as-a-walled-in-goal
  (let [c {:primitives (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (box -20 63 -20 20 63 20 "stone")})}
        walled? #(choose/goal-walled? c %)]
    (is (true? (walled? {:status :no-path :reason :budget :goal-left 3})) "three blocks off")
    (is (false? (walled? {:status :no-path :reason :budget :goal-left 40})) "far off: the unseen land is the problem")
    (is (false? (walled? {:status :no-path :reason :budget})) "no distance known")))

(deftest a-goal-cut-off-by-a-drop-reads-as-walled-in-for-a-free-body
  (let [c {:primitives (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (box -20 63 -20 20 63 20 "stone")})}]
    (is (true? (choose/goal-walled? c {:status :no-path :reason :goal-cut-off})))
    (is (false? (choose/goal-walled? c {:status :no-path :reason :one-way})))))

(def trench-room
  "The sealed room opened at its east door (x 3, z 0) onto a 3-deep trench across the whole dirt yard (x 4, z -6..6, feet y 61); the yard beyond is level with the room floor."
  (let [trench (for [z (range -6 7) y [61 62 63]] (str "4," y "," z))]
    (apply dissoc (merge room (box 4 60 -6 14 63 6 "dirt")) "3,64,0" "3,65,0" trench)))

(deftest go-to-shut-in-by-a-trench-it-cannot-climb-digs-out-with-bare-hands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-job! (merge in-room {:blocks trench-room}) {:pos [8 64 0] :range 1}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (<= 6 (first (feet p)))))))))

(def oriel-house
  "Oriel's house (planks floor y 68, cobble walls y 69-70 x 58..64 z 6..10, door gap x 61, planks roof y 71) beside a farm field with a pit 3 deep outside the door."
  (let [ground (box 40 60 0 80 67 30 "dirt")
        floor (box 58 68 6 64 68 10 "oak_planks")
        walls (apply dissoc (box 58 69 6 64 70 10 "cobblestone")
                     (for [x (range 59 64) z (range 7 10) y [69 70]] (str x "," y "," z)))
        door ["61,69,10" "61,70,10"]
        roof (box 58 71 6 64 71 10 "oak_planks")
        field (merge (box 58 68 12 64 68 18 "farmland") (box 58 69 12 64 69 18 "wheat"))
        pit (for [y [66 67 68]] (str "61," y ",11"))]
    (apply dissoc (merge ground floor walls roof field) (concat door pit))))

(def protected-house
  "oriel-house built of blocks nothing digs (spawners)."
  (into {} (for [[k v] oriel-house] [k (if (#{"oak_planks" "cobblestone"} v) "spawner" v)])))

(deftest go-to-a-house-whose-planks-and-cobble-cannot-be-dug-drops-into-the-pit-outside-its-door-and-digs-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-job! {:self {:pos {:x 61.5 :y 69 :z 9.5}} :blocks oriel-house :light [0 0]} {:pos [63 69 12] :range 1}))]
          (is (true? (:arrived @out)) (pr-str @out)))))))

(deftest go-to-one-way-closed-never-drops-into-the-pit-outside-the-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-job! {:self {:pos {:x 61.5 :y 69 :z 9.5}} :blocks protected-house :light [0 0]} {:pos [63 69 12] :range 1 :one-way :closed}))]
          (is (false? (:arrived @out)) (pr-str @out))
          (is (= 69 (second (feet p))) "the caller's :closed forbids the drop"))))))

(deftest go-to-a-house-above-an-undiggable-pit-stays-in-instead-of-dropping-into-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge protected-house (into {} (for [[k v] oriel-house :when (= "dirt" v)] [k "bedrock"])))
              {:keys [out p]} (await (run-job! {:self {:pos {:x 61.5 :y 69 :z 9.5}} :blocks blocks :light [0 0]} {:pos [63 69 12] :range 1}))]
          (is (false? (:arrived @out)) (pr-str @out))
          (is (= 69 (second (feet p))) "the body did not drop into the pit"))))))

(deftest a-dig-leg-whose-search-spends-its-slice-goes-on-in-the-next
  (let [p (tu/fake {:blocks (box -2 60 -2 20 70 2 "stone") :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly 1200)}
        slice (atom nil)
        leg #(let [s (choose/route-slice "leg-resume")] (reset! slice s) (choose/dig-leg c [0 64 0] [12 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice s}))]
    (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {})]
      (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
        #(let [first-leg (leg)
               legs (loop [n 0] (let [e (leg)] (if (and (= :searching (:step e)) (< n 300)) (recur (inc n)) e)))]
           (is (= {:step :searching} first-leg))
           (is (= :dig-route (:step legs)) (pr-str (:step legs))))))))

;; a bedrock slab three wide across the line between the body and both leg ends: no route within leg-margin of the
;; line reaches them; the leg goes round it, searched in slices over later rounds like the first pass
(deftest a-dig-leg-goes-round-a-slab-wider-than-its-margin-across-the-line-over-several-slices
  (let [p (tu/fake {:blocks (merge (box -2 60 -4 20 70 4 "stone") (box 4 60 -1 5 70 1 "bedrock")) :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly 1200)}
        leg #(choose/dig-leg c [3 64 0] [10 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice (choose/route-slice "leg-round-slab")})]
    (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {})]
      (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
        #(let [legs (doall (take 300 (repeatedly leg)))]
           (is (= {:step :searching} (first legs)))
           (is (= :dig-route (:step (first (remove #{{:step :searching}} legs)))) (pr-str (last legs))))))))

;; a slab 21 wide across the line: the box of the leg's own length round its ends is cut off by it, a wider one reaches round
(deftest a-dig-leg-widens-its-box-round-a-slab-wider-than-its-length
  (let [p (tu/fake {:blocks (merge (box -2 50 -14 14 80 14 "stone") (box 4 50 -10 5 80 10 "bedrock")) :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly tu/order-worth)}
        leg #(choose/dig-leg c [3 64 0] [10 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice (choose/route-slice "leg-round-wide-slab")})]
    (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {})]
      (let [e (first (remove #{{:step :searching}} (take 300 (repeatedly leg))))]
        (is (= :dig-route (:step e)) (pr-str e))
        (is (some #(< 10 (js/Math.abs (nth (:cell %) 2))) (:cells e)) "it digs round the slab's end")))))

;; a goal sealed in bedrock: the back search from it ends at its shell, inside the box, so no wider box is searched
(deftest a-dig-leg-to-a-goal-sealed-in-bedrock-ends-with-no-leg
  (let [p (tu/fake {:blocks (merge (box -2 50 -14 14 80 14 "stone") (box 9 63 -1 11 66 1 "bedrock") {"10,64,0" "air" "10,65,0" "air"})
                    :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly 1200)}
        leg #(choose/dig-leg c [6 64 0] [10 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice (choose/route-slice "leg-sealed")})]
    (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {})]
      (is (= [nil] (take 1 (remove #{{:step :searching}} (take 300 (repeatedly leg)))))))))

;; the leg's far end hangs over a seen pit one block deep: the leg ends in the pit under it
(deftest a-dig-leg-end-over-a-pit-ends-in-the-pit
  (let [p (tu/fake {:blocks (merge (box -2 50 -14 14 80 14 "stone") {"9,63,0" "air"}) :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly 1200)}
        leg #(choose/dig-leg c [3 64 0] [12 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice (choose/route-slice "leg-over-pit")})]
    (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {})]
      (is (= [9 64 0] (:cell (last (:cells (first (remove #{{:step :searching}} (take 300 (repeatedly leg)))))))) "its last cut is the pit's head cell"))))

;; the goal hangs over open air with no block carried: no box holds a way to it, and each wider one cuts the search off again.
;; The widening ends with no leg when a search spends the 100 ms it may (dig-route/answer-s) before its bound, or :past-worth
;; once the boxed searches have spent the way without a dig route (memory :route-search :fallback-s); only that price differs here
(deftest a-dig-leg-widening-round-an-end-no-box-reaches-ends-at-the-cut-or-the-fallback-price
  (let [p (tu/fake {:blocks (box -2 50 -14 7 80 14 "stone") :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly 1200)}
        ends (fn [fallback-s]
               (let [slice (choose/route-slice (str "leg-over-void-" fallback-s))
                     leg #(choose/dig-leg c [3 64 0] [9 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice slice})]
                 (with-redefs [ctx/mem (constantly {:route-search {:fallback-s fallback-s}}) ctx/left (constantly {}) cost/search-read-s (constantly 0.001)]
                   (let [r (first (remove #{{:step :searching}} (take 300 (repeatedly leg))))
                         ;; the answers of the widest box's searches (its :margin in the question's route-opts), the last searched
                         asked @(:asked slice)
                         widest (apply max -1 (map #(:margin (% 3) 0) (keys asked)))]
                     {:r r :widest (set (map #(select-keys % [:problem :cut :boxed]) (for [[k a] asked :when (= widest (:margin (k 3) 0))] a)))}))))
        cut (ends 60)]
    (is (nil? (:r cut)) "no leg")
    (is (= #{{:problem :too-big :cut :answer-s}} (:widest cut)) "every search of the widest box was cut at answer-s: not boxed (covers?), not a full heap, not no ends")
    (is (= {:step :none :why :past-worth} (:r (ends 1))) "the boxed searches spent the fallback price")))

;; card 29cd6b4d: lava 5 long on the line and 15 wide holds both leg ends (3 and 6 on): each is moved to the stand nearest
;; it with no lava beside, and the leg is dug there
(deftest a-dig-leg-whose-ends-lie-in-lava-digs-to-the-stands-nearest-them
  (let [p (tu/fake {:blocks (merge (box -2 58 -10 16 72 10 "stone") (box 3 64 -7 7 65 7 "lava")) :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly 1000)}
        leg #(choose/dig-leg c [0 64 0] [12 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice (choose/route-slice "leg-ends-in-lava")})
        r (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {})]
            (first (drop-while #{{:step :searching}} (take 300 (repeatedly leg)))))]
    (is (= :dig-route (:step r)) (str r))
    (is (= [8 62 0] (:cell (peek (:cells r)))) "the far end's nearest stand: under the lava's edge, its head clear of it")
    (is (not-any? #(= "lava" (:block %)) (:cells r)))))

;; card 7771243f: a bedrock slab 38 long, 17 wide and 13 high holds both leg ends (at most 6 on) with its edge beyond the leg's
;; length from them: the stand nearest an end lies 7 or more off, so a cube of the leg's length found none
(deftest a-dig-leg-whose-ends-lie-in-a-slab-wider-than-the-leg-moves-them-to-the-stands-beyond-it
  (let [p (tu/fake {:blocks (merge (box -2 50 -15 50 80 15 "stone") (box 3 58 -8 40 70 8 "bedrock")) :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly 1000)}
        leg #(choose/dig-leg c [2 64 0] [44 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice (choose/route-slice "leg-ends-in-wide-slab")})
        r (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {})]
            (first (drop-while #{{:step :searching}} (take 300 (repeatedly leg)))))]
    (is (= :dig-route (:step r)) (str r))
    (is (not-any? #(= "bedrock" (:block %)) (:cells r)))))

;; card ee78bde4: an end's stand search reads cells at the price of a route search's nodes against what the leg is worth, whatever
;; the rock round the end: with the worth at 20 cells' time not one ring (27 cells) is read, where the geometric reach alone read hundreds of thousands
(deftest a-dig-leg-end-in-rock-with-no-stand-reads-cells-only-as-far-as-the-leg-is-worth
  (doseq [[worth cap] [[(* 20 (cost/search-read-s)) 100]]]
    (let [p (tu/fake {:blocks (box -2 50 -15 50 80 15 "bedrock") :inventory [{:name "stone_pickaxe" :count 1}]})
          reads (atom 0)
          real-block-at (escape/block-at-of p)
          c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly worth)}]
      (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {}) escape/block-at-of (constantly (fn [pos] (swap! reads inc) (real-block-at pos)))]
        (choose/dig-leg c [2 64 0] [44 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice (choose/route-slice "leg-end-scan-priced")}))
      (is (<= @reads cap) (str "the ends are looked at, no more rings than the route slice's nodes buy, worth " worth)))))

;; card e252d55b: the goal floats in seen air with no fallback way and no bound but the root's default worth; the body remembers only the
;; cells round it. Once a box holds them and what a step reads past them, a wider one adds only guessed rock: no leg
(deftest a-dig-leg-widening-at-no-price-ends-once-its-box-s-faces-are-unremembered-though-the-body-remembers-far-away
  (let [f (tu/fake {:blocks (box 0 60 -3 5 68 3 "stone") :inventory [{:name "stone_pickaxe" :count 1}]})
        raw (fake-raw/create f)
        per (perception/create raw {:now (constantly 1000000)})
        ^js st (:st per)
        seen [0 60 -3 8 68 3]
        in? (fn [[x y z]] (every? true? (map #(<= %1 %2 %3) (take 3 seen) [x y z] (drop 3 seen))))
        ;; the body remembers the stone round it and, 1000 blocks off, a second place
        _ (doseq [x (range 0 9) y (range 60 69) z (range -3 4)] (store/record! st (store/store-of st (.-dim st)) x y z (.stateAt raw x y z) 1000000))
        _ (store/record! st (store/store-of st (.-dim st)) 1000 64 0 (.stateAt raw 1000 64 0) 1000000)
        p (perception/wrap f per)
        _ (tu/set-sensed! p (fn [pos] (if (in? (mapv #(js/Math.floor (aget pos %)) ["x" "y" "z"])) (.blockAt f pos) #js {:unknown true :pos pos})))
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly 1200)}
        margins (atom [])
        route-cells choose/route-cells
        leg #(choose/dig-leg c [2 64 0] [7 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice (choose/route-slice "leg-widens-past-memory")})]
    ;; a second arity gives the stand-in the arity-8 entry dig-leg calls
    (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {})
                  choose/route-cells (fn ([p from goal may-dig? own?] (route-cells p from goal may-dig? own?))
                                         ([p from goal may-dig? own? unseen-ok? fetch? opts]
                                           (some->> (:margin opts) (swap! margins conj))
                                           (route-cells p from goal may-dig? own? unseen-ok? fetch? opts)))]
      (is (= [nil] (take 1 (drop-while #{{:step :searching}} (take 300 (repeatedly leg))))))
      ;; the box round the leg of 5 doubled once holds the stone and 3 past it; old code widened till it held the far place
      (is (<= (apply max @margins) 10) (str "widest margin " (apply max @margins))))))

;; the detour leg digs longer than the call has left (memory :dug-s of its worth): out of dig budget, not past its worth
(deftest a-detour-leg-past-the-calls-dig-time-is-out-of-dig-budget
  (let [p (tu/fake {:blocks (merge (box -2 60 -4 20 70 4 "stone") (box 4 60 -1 5 70 1 "bedrock")) :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {} :slots [] :view (fn [] {:now 0 :data {}}) :worth (constantly 100)}
        leg #(choose/dig-leg c [3 64 0] [10 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice (choose/route-slice "leg-past-budget")})]
    (with-redefs [ctx/mem (constantly {:dug-s 99.9}) ctx/left (constantly {})]
      (is (= :dig-budget (:why (first (remove #{{:step :searching}} (take 300 (repeatedly leg))))))))))

(deftest a-flood-is-charged-to-the-slice-at-its-measured-cpu-against-a-search-node
  (let [p (tu/fake {:blocks (box -20 63 -20 20 63 20 "stone")})
        slice (assoc (choose/route-slice "nearest-flood") :floor 0)
        before @(:left slice)
        per-node (/ (cost/search-cell-s) (cost/flood-cell-s))]
    (choose/nearest-stand p [10 64 10] 0 slice)
    (let [charged (- before @(:left slice))]
      (is (<= (/ 1024 per-node) charged (/ 1100 per-node)) "a flood is up to room-cells cells, each a flood cell's seconds over a node's"))))

(deftest frontier-way-charges-its-flood-to-the-slice
  (let [p (tu/fake {:blocks (merge (box -3 63 -3 3 63 3 "stone") (box 5 40 -3 9 40 3 "stone"))})
        slice (choose/route-slice "frontier-flood")
        before @(:left slice)
        opts {:own? (constantly false) :skip (constantly false) :slice slice}]
    (choose/frontier-way p [0 64 0] [40 40 0] (constantly true) opts)
    (is (< @(:left slice) before) "the flood of the land's cells came off the slice")))

(deftest a-fresh-pillar-fn-each-call-resumes-the-sliced-search-rather-than-restarting-it
  (let [p (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        slice (choose/route-slice "pillar-fn-resume")
        ask #(choose/route-answer p [0 64 0] [10 70 0] (constantly true) (constantly false) false false
                                  {:slice slice :pillar-s (fn [_ _ _ _] 1.5) :place-s 1})]
    (ask)
    (is (= 1 (count (get @choose/kept-routes (:id slice)))) "the search is kept under one key")
    (vreset! (:asked slice) {})
    (ask)
    (is (= 1 (count (get @choose/kept-routes (:id slice)))) "a new fn is the same search")))

(deftest an-ended-sliced-search-keeps-its-answer-not-its-cell-maps
  (let [slice (choose/route-slice "ended-frees")
        k [[0 64 0] [9 64 0] false {} {} #{}]
        starts (atom 0)
        start #(do (swap! starts inc) (fn [_] [{:problem :too-big} 5 (fn [] false)]))
        a (choose/sliced-answer slice k start nil)
        kept (get-in @choose/kept-routes [(:id slice) k])]
    (is (= {:problem :too-big} a))
    (is (= 0 (second ((:run kept) 100))) "what is kept is the answer, not the search that holds its cell maps")
    (vreset! (:asked slice) {})
    (is (= {:problem :too-big} (choose/sliced-answer slice k start nil)))
    (is (= 1 @starts) "the ended search is not begun again")))

(deftest an-ended-route-answers-a-smaller-bound-while-the-world-holds
  (let [slice (choose/route-slice "ended-bound")
        k #(vector [0 64 0] [9 64 0] false {:bound-s %} {} #{})
        route {:dig [[1 64 0]] :seconds 7}
        starts (atom 0)
        start #(do (swap! starts inc) (fn [_ _] [route 5 (fn [] false)]))
        ask (fn [bound epoch] (vreset! (:asked slice) {}) (choose/sliced-answer slice (k bound) start bound epoch))]
    (is (= route (ask 30 1)))
    (is (= route (ask 20 1)) "a smaller bound the route fits")
    (is (= {:problem :dearer} (ask 5 1)) "a bound under the cheapest route: none under it")
    (is (= 1 @starts) "the ended search is not begun again")
    (ask 5 2)
    (is (= 2 @starts) "the world changed: searched again")
    (choose/forget-routes! (:id slice))))

(deftest an-ended-search-with-no-route-answers-no-larger-bound-and-no-other-memory
  (let [slice (choose/route-slice "ended-none")
        k #(vector [0 64 0] [9 64 0] false {:bound-s %} {} #{})
        starts (atom 0)
        start #(do (swap! starts inc) (fn [_ _] [{:problem :no-route} 5 (fn [] false)]))
        ask (fn [bound epoch] (vreset! (:asked slice) {}) (choose/sliced-answer slice (k bound) start bound epoch))]
    (is (= {:problem :no-route} (ask 20 1)))
    (ask 10 1)
    (ask 15 1)
    (is (= 1 @starts) "a bound no larger than the one it ran under finds no route it did not")
    (ask 25 1)
    (is (= 2 @starts) "a larger bound: searched again")
    (ask 25 2)
    (is (= 3 @starts) "the memory changed: searched again")
    (choose/forget-routes! (:id slice))))

(defn counted-search
  "dig-route/search counting each search begun in starts: route-within's re-price of a kept route over its own cells is none."
  [starts]
  (let [search dig-route/search]
    (fn [& args] (when-not (:only (nth args 3)) (swap! starts inc)) (apply search args))))

(defn health-run
  "route-answer's seconds (to 1e-6; else its answer, :searching say) from feet cell from to goal at each health of hps: {:kept under one go-to call, :fresh
  under a call of its own at each, :starts the searches the kept call began}."
  [p from goal hps]
  (let [ask (fn [id] (let [a (choose/route-answer p from goal (constantly true) (constantly false) false false {:slice (choose/route-slice id)})]
                      (if (:seconds a) (js/Math.round (* 1e6 (:seconds a))) a)))
        at (fn [id hp] (swap! (fake/state p) assoc-in [:self :health] hp) (ask id))
        starts (atom 0)
        kept (with-redefs [dig-route/search (counted-search starts)]
               (mapv #(at "hp-kept" %) hps))
        fresh (mapv #(at (str "hp-fresh-" %) %) hps)]
    (choose/forget-routes! "hp-kept")
    (doseq [hp hps] (choose/forget-routes! (str "hp-fresh-" hp)))
    {:kept kept :fresh fresh :starts @starts}))

(def ledge
  "A body at x 0 on a bedrock ledge 6 over the open ground: its way to the floor is the fall off it."
  {:blocks (merge open-ground (box 0 64 -2 1 68 2 "bedrock")) :self {:pos {:x 0.5 :y 69 :z 0.5}}})

(def flooded-row
  "A one-wide row of rock under a pool, the rock over it too high to dig round: the row dug under water is the only way,
  one the body drowns on at 10 hp."
  {:blocks (merge (box 0 63 0 4 90 0 "stone") (box 1 66 0 3 66 0 "water") (box 0 64 0 0 65 0 "air") (box 4 64 0 4 65 0 "air"))
   :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "stone_pickaxe" :count 1}]})

(deftest an-ended-route-is-re-priced-as-the-health-rises-not-searched-again
  (let [{:keys [kept fresh starts]} (health-run (tu/fake ledge) [0 69 0] [8 64 0] (range 12 21))]
    (is (= 1 starts) "the health is no new question: the memory holds")
    (is (= 1 (count (distinct kept))) "a fall the body lives through costs its hp at the one price an hp, at any health")
    (is (= fresh kept) "the kept route's price is a fresh search's at each health")))

(deftest a-kept-route-follows-the-health-down-and-a-no-route-a-deeper-drop-up
  (are [scene from goal hps starts] (let [{:keys [kept fresh] :as run} (health-run (tu/fake scene) from goal hps)]
                                      (and (= fresh kept) (= starts (:starts run))))
    ;; searched again once the fall passes the deepest drop survived
    ledge [0 69 0] [8 64 0] (range 20 0 -1) 2
    ;; no route at 1 hp, asked again at each deeper drop till the fall is one
    ledge [0 69 0] [8 64 0] (range 1 21) 3
    ;; the flood's drowning re-priced: searched again once it kills
    flooded-row [0 64 0] [4 64 0] (range 20 0 -2) 2
    ;; a route found at ##Inf (the flood kills) is re-priced as the health rises, not searched again
    flooded-row [0 64 0] [4 64 0] (range 2 21 2) 1))

(deftest a-search-gone-on-over-calls-prices-its-route-at-the-health-it-ends-at
  ;; 5 nodes a call: the search begun at 20 hp ends calls later, at a health its way does not survive
  (are [scene from goal nodes calls hp] (let [{:keys [kept fresh]} (settings/with-settings {:jobs.lib.dig-route/max-nodes nodes}
                                                         #(health-run (tu/fake scene) from goal (concat (repeat calls 20) (repeat 12 hp))))]
                              (and (= [:searching] (distinct (take calls kept)))
                                   (= [(last fresh)] (distinct (remove #{:searching} kept)))))
    ;; the fall off the ledge passes the deepest drop
    ledge [0 69 0] [8 64 0] 5 3 2
    ;; the flood drowns it
    flooded-row [0 64 0] [4 64 0] 2 2 8))

(deftest a-dig-route-past-the-count-needs-an-end-more-than-a-block-nearer-than-any-way-before
  (are [end best dive? ok?] (= ok? (esc/dig-progress? {:cells [{:cell [0 60 0]} {:cell end}]} [10 60 0] best dive?))
    [8 60 0] 12 false true
    [8 60 0] 4 false true
    [8 60 0] 3 false false
    [3 60 0] 4 false false
    [10 60 0] 2 false true
    ;; a dive-climb's best is a height to gain: the end's horizontal distance does not count
    [0 58 0] 4 true true
    [0 58 0] 3 true false
    [10 58 0] 4 true true
    [0 59 0] 2 true false))

(deftest a-plug-with-an-unseen-neighbour-is-dug-at-its-flood-price
  ;; card f043195e: the unseen neighbour of a plug may be the pool; it is priced as water, not banned
  (let [hp-s (cost/hp-seconds)
        plug [10 64 0]
        stone (into {} (map (fn [d] [(apply str (interpose "," (map + plug d))) "stone"])) rules/neighbour-deltas)
        p (tu/seeing-all (tu/fake {:blocks (assoc stone "10,64,0" "stone") :unloaded ["11,64,0"] :inventory [{:name "stone_pickaxe" :count 1}]}))
        flood (:flood (dig-route/body-prices p (constantly false) {:plugs #{plug}}))
        dig-s (:dig-s (dig-route/body-prices p (constantly false) {:plugs #{plug}}))]
    (is (= (cost/flood-dig-s {} 20 hp-s) (dig-route/flood-s flood [[:dig plug "stone"]])) "the flood price over a dry dig")
    (is (< (dig-s "stone" plug) ##Inf) "still dug: a price, not a ban")
    (is (zero? (dig-route/flood-s flood [[:dig [20 64 0] "stone"]])) "a cell that is no plug is priced dry")))

(deftest a-dig-route-and-a-way-price-each-dug-cell-by-what-it-destroys
  ;; another's planks are put back, the stone round them costs nothing; farmland under a growing crop costs its
  ;; replacing and the crop's growth; the body's own build costs its own price (:own-s) in place of that
  (let [p (tu/fake {:blocks {"1,64,0" "oak_planks" "2,64,0" "stone" "3,63,0" "farmland" "3,64,0" "wheat"}
                    :ages {"3,64,0" 3}})
        dig-s (:dig-s (dig-route/body-prices p (constantly false) {:own-s #(if (= [2 64 0] %) 5 0)}))
        hand-s #(/ (tools/clear-ms p %) 1000)
        planks [{:cell [1 64 0]}]]
    (is (= (+ (hand-s "oak_planks") (cost/put-back-s "oak_planks")) (dig-s "oak_planks" [1 64 0])))
    (is (= (+ (hand-s "stone") 5) (dig-s "stone" [2 64 0])))
    (is (= (+ (hand-s "farmland") (cost/destroyed-s "farmland" "wheat" 3 false)) (dig-s "farmland" [3 63 0]))
        "a route cutting farmland under a standing crop pays the crop with the farmland")
    (let [farm (escape-way/price p [3 65 0] {:step :dig-route :cells [{:cell [3 63 0]}]})
          shaft (escape-way/price p [3 65 0] {:step :dig-route :cells [{:cell [3 64 0]} {:cell [3 63 0]}]})]
      (is (< (- shaft farm) (cost/destroyed-s "wheat" "air" 3 false))
          "a shaft through the crop then its farmland pays the crop once, with the farmland (the crop is an open cell)"))
    (is (= (+ (escape-way/price p [0 64 0] {:step :dig-route :cells planks} (constantly 1)) (cost/put-back-s "oak_planks"))
           (escape-way/price p [0 64 0] {:step :dig-route :cells planks}))
        "a way's price holds the put-back of a cell that is not the body's own; the caller prices its own")))

(def tunnel-over-room
  "A 1-wide 2-tall tunnel (x 0, y 64..65, z 0..3) in solid stone, closed at both ends, whose floor (y 63) roofs a room
  below (x -3..3, y 60..62, z -3..3); a work row (x -3..3, y 64..65, z 6..8) lies beyond a wall two thick (z 4..5)."
  (apply dissoc
         (box -6 58 -6 6 68 12 "stone")
         (concat (for [y [64 65] z (range 0 4)] (str "0," y "," z))
                 (for [x (range -3 4) y [60 61 62] z (range -3 4)] (str x "," y "," z))
                 (for [x (range -3 4) y [64 65] z (range 6 9)] (str x "," y "," z)))))

(deftest go-to-from-a-closed-tunnel-digs-the-wall-to-the-goal-not-the-floor-over-the-room-below
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-job! {:blocks tunnel-over-room :self {:pos {:x 0.5 :y 64 :z 1.5}} :inventory [{:name "stone_pickaxe" :count 1}]}
                                               {:pos [0 64 7] :range 0}))
              dug (map #(js->clj (.-args %) :keywordize-keys true) (calls p "dig"))]
          (is (true? (:arrived @out)))
          (is (seq dug))
          (is (every? #(>= (get-in % [:pos :y]) 64) dug) "no cell under the tunnel's floor is dug"))))))

(deftest a-dig-quote-of-no-running-call-keeps-no-search
  (let [{:keys [eng]} (gt/setup {:blocks (box -2 60 -2 40 70 2 "stone")})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth tu/order-worth} :round 1})]
    (.setOwner (:primitives eng) "tx")
    (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
      (fn []
        (is (= :budget (choose/dig-price c [38 64 0] nil)) "a slice spent: not known, no estimate to wait on")
        (is (= :budget (choose/dig-price c [38 64 0] nil)))
        (is (empty? @choose/kept-routes) "nothing is kept: asked at no time cost, a kept search would grow with every quote")))))

(deftest a-dig-quote-bounded-by-the-walk-on-ends-nil-when-every-dig-costs-more
  (let [{:keys [eng]} (gt/setup {:blocks (box -2 60 -2 40 70 2 "stone")})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth tu/order-worth} :round 1})]
    (.setOwner (:primitives eng) "tx")
    (is (nil? (choose/dig-price c [38 64 0] 0.1)) "dearer than walking on: priced and dropped, not :budget")))

(deftest a-dig-quote-whose-walk-on-alone-passes-the-worth-searches-nothing
  ;; run4 F2: a quote to a bed 17.5 km off searched a dig route no worth could pay for, and its setup ran the body out of heap
  (let [{:keys [eng]} (gt/setup {:blocks (box -2 60 -2 40 70 2 "stone")})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth tu/order-worth} :round 1})
        searched (atom 0)
        real choose/dig-way]
    (.setOwner (:primitives eng) "tx")
    (with-redefs [choose/dig-way (fn [& args] (swap! searched inc) (apply real args))]
      (is (nil? (choose/dig-price c [20000 64 0] nil)))
      (is (zero? @searched)))
    (let [priced (atom 0)
          real choose/route-s]
      (with-redefs [choose/route-s (fn ([p cells own? fetch] (real p cells own? fetch))
                                         ([p block-at cells own? fetch shares] (swap! priced + (count cells)) (real p block-at cells own? fetch shares)))]
        (is (< 1000 (choose/rest-dig-s c [0 64 0] [20000 64 0])) "the far rock is priced")
        (is (<= @priced (+ 2 (/ dig-route/answer-s (cost/search-read-ceiling-s)))) (str @priced " cells priced one by one"))))))

(deftest a-dig-quote-cheaper-than-walking-on-is-found-under-the-bound
  (let [{:keys [eng]} (gt/setup {:blocks (box -2 60 -2 6 70 2 "stone")})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth tu/order-worth} :round 1})
        _ (.setOwner (:primitives eng) "tx")
        dug (choose/dig-price c [4 64 0] nil)]
    (is (number? dug) "the dig is found with no bound")
    (is (= dug (choose/dig-price c [4 64 0] (* 1.5 dug))) "a walk-on dearer than the dig leaves the dig found")
    (is (nil? (choose/dig-price c [4 64 0] (* 0.75 dug))) "a walk-on cheaper than the dig ends the search")))

(deftest a-dig-quote-of-no-running-call-that-ended-keeps-no-search
  (let [{:keys [eng]} (gt/setup {:blocks (box -2 60 -2 6 70 2 "stone")})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth tu/order-worth} :round 1})]
    (.setOwner (:primitives eng) "tx")
    (is (not= :budget (choose/dig-price c [4 64 0] nil)) "the search ended within its slice")
    (is (empty? @choose/kept-routes) "an ended quote's answer is not kept under the job's quote key")))

(deftest a-dig-quote-of-a-running-call-goes-on-searching
  (let [{:keys [eng]} (gt/setup {:blocks (box -2 60 -2 40 70 2 "stone")})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth tu/order-worth} :round 1})]
    (.setOwner (:primitives eng) "tx")
    (ctx/update-mem! c assoc :route-owner "j9")
    (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
      (fn []
        (is (= :searching (choose/dig-price c [38 64 0] nil)) "the call's search goes on at its next quote")
        (is (seq (get @choose/kept-routes "j9")))
        (choose/forget-routes! "j9")))))

(deftest a-dig-quote-searches-with-the-opts-the-escalation-digs-with
  (let [{:keys [eng]} (gt/setup {:blocks (box -2 60 -2 12 70 2 "stone")})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth tu/order-worth} :round 1})
        seen (atom nil)
        real choose/dig-way]
    (.setOwner (:primitives eng) "tx")
    (ctx/update-mem! c assoc :guess-looks {:placed {:all 3 :bad 3}} :spent-routes #{[1 2]})
    (with-redefs [choose/dig-way (fn [p from goal may-dig? opts] (reset! seen opts) (real p from goal may-dig? opts))]
      (choose/dig-price c [10 64 0] nil))
    (let [feet (vec (map #(js/Math.floor %) [0 64 0]))
          way (choose/way-opts c feet false nil)]
      (is (every? #(contains? @seen %) [:spent :own-s :risk-s :guess-risk :worth-s]) (pr-str (keys @seen)))
      (is (= (:guess-risk way) (:guess-risk @seen)))
      (is (pos? (:placed (:guess-risk @seen))) "the looks this call made price the quote's route too")
      (is (= #{[1 2]} (:spent @seen))))))

;; a choice after a failed escalation searches under the replans' bound over rounds: once the :searching branch has
;; dropped the failed escalation from memory, the next round asks the same search, which goes on rather than begins again
(deftest a-choice-after-a-failed-escalation-keeps-its-search-over-rounds
  (let [{:keys [eng]} (gt/setup {:blocks (box -2 60 -2 14 70 2 "stone") :inventory [{:name "stone_pickaxe" :count 1}]})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth tu/order-worth} :round 1})
        feet [0 64 0]
        goal [10 64 0]
        starts (atom 0)
        choice (fn []
                 (let [slice (choose/route-slice (choose/route-owner c))
                       o (choose/way-opts c feet false slice)
                       e (choose/dig-way (:primitives c) feet goal (constantly true) o)]
                   (esc/search-begun! c feet goal o)
                   (ctx/update-mem! c dissoc :escalation)
                   (choose/keep-routes! slice)
                   [(:worth-s o) (:step e)]))]
    (.setOwner (:primitives eng) "tx")
    (ctx/update-mem! c assoc :escalation {:step :pillar} :replan-budget {:since 0 :fallback-s 30})
    (with-redefs [dig-route/search (counted-search starts)]
      (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
        (fn []
          (let [first-round (choice)
                _ (doall (take-while true? (repeatedly 300 #(choose/routes-searching! (choose/route-owner c) (choose/route-epoch (:primitives c))))))
                last-round (choice)]
            (is (= [30 :searching] first-round))
            (is (= [30 :dig-route] last-round) "the kept search's answer, under the same bound")
            (is (= 1 @starts) "one search, gone on over the rounds")
            (choose/forget-routes! (choose/route-owner c))))))))

;; the search reads the body's memory only: a world change it has not seen leaves its ended route standing
(deftest an-ended-route-holds-while-the-memory-does-though-the-world-changes
  (let [{:keys [eng]} (gt/setup {:blocks (box -2 60 -2 14 70 2 "stone") :inventory [{:name "stone_pickaxe" :count 1}] :light [15 15] :seen :all})
        c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {:worth tu/order-worth} :round 1})
        p (:primitives c)
        starts (atom 0)
        choice (fn []
                 (let [slice (choose/route-slice (choose/route-owner c))
                       o (choose/way-opts c [0 64 0] false slice)
                       e (choose/dig-way p [0 64 0] [10 64 0] (constantly true) o)]
                   (choose/keep-routes! slice)
                   (:step e)))
        ended (fn [] (doall (take-while true? (repeatedly 300 #(choose/routes-searching! (choose/route-owner c) (choose/route-epoch p))))))]
    (.setOwner p "tx")
    (with-redefs [dig-route/search (counted-search starts)]
      (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
        (fn []
          (is (= :searching (choice)))
          (ended)
          (is (= :dig-route (choice)))
          (let [world (threat-field/world-epoch p)
                memory (choose/route-epoch p)]
            (fake/set-block! p [40 64 40] "dirt")
            (is (not= world (threat-field/world-epoch p)) "the world changed")
            (is (= memory (choose/route-epoch p)) "out of the body's sight")
            (is (= :dig-route (choice)))
            (is (= 1 @starts) "the ended route answers: nothing the search reads changed"))
          ;; the new search counts once its lane and band have run
          (fake/remove-block! p [5 66 0])
          (rays/touch! (aget p "perception") [5 66 0])
          (choice)
          (ended)
          (is (= 2 @starts) "the memory changed: searched again")
          (choose/forget-routes! (choose/route-owner c)))))))

(defn ^:async priced-search-run!
  "run-job! with every dig-route search counted: {:out :seen :searched-s (cells read at each search's :read-s) :max-ss
  (each search's bound, in order) :start-s (the bound of a search from the start to the goal before the run: the
  straight dig)}."
  [spec args]
  (let [search dig-route/search
        spent (atom 0)
        max-ss (atom [])
        from (mapv #(js/Math.floor (get-in spec [:self :pos %])) [:x :y :z])]
    (set! dig-route/search (fn [block-at from goal opts]
                             (swap! max-ss conj (:max-s opts))
                             (let [run (search block-at from goal opts)]
                               (fn [n & [m]]
                                 (let [[_ _ _ read :as r] (run n m)]
                                   (swap! spent + (* read (:read-s opts)))
                                   r)))))
    (try (let [_ (choose/route-answer (:p (setup spec)) from (:pos args) (constantly true) (constantly false) false false {})
               start-s (first @max-ss)
               _ (reset! max-ss [])
               _ (reset! spent 0)
               {:keys [out seen]} (await (run-job! spec args))]
           {:out @out :seen seen :searched-s @spent :max-ss @max-ss :start-s start-s})
         (finally (set! dig-route/search search)))))

(deftest a-search-toward-a-pocket-seen-through-rock-costs-no-more-than-the-straight-dig-from-the-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[sight mass [x y z :as pocket]] [[20 (merge (box 0 50 0 49 54 39 "stone") (box 0 55 0 20 60 20 "stone")) [40 51 30]]
                                                 [20 (merge (box 0 50 0 49 54 39 "stone") (box 0 55 0 20 60 20 "stone")) [40 52 30]]]]
          (let [blocks (merge mass {(str x "," y "," z) "air" (str x "," (inc y) "," z) "air"})
                {:keys [out searched-s max-ss start-s]} (await (priced-search-run! {:self {:pos {:x 10.5 :y 61 :z 10.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}] :sight sight}
                                                                                   {:pos pocket :range 0}))]
            (is (true? (:arrived out)) (pr-str pocket (select-keys out [:why :text :escalation])))
            (is (js/Number.isFinite start-s) "the straight dig from the start has a price")
            (is (<= (apply max max-ss) start-s) (pr-str pocket "a search bounded past the straight dig from the start" start-s max-ss))
            (is (<= searched-s start-s) (pr-str pocket "the searches cost more than the straight dig from the start" searched-s start-s))))))))

(defn absent-at
  "f run with ::unseen-floor-counts holding each kind of kinds absent at chance p, every other kind at an even chance."
  [kinds p f]
  (settings/with-settings {:jobs.lib.cost.value/unseen-floor-counts
                           (merge (zipmap [:level :up :down :drop :drop-4 :drop-8] (repeat {:absent 1 :present 1})) (zipmap kinds (repeat {:absent p :present (- 1 p)})))}
    f))

(deftest a-goal-over-an-unseen-floor-is-priced-over-the-chance-the-floor-is-none
  (let [world (assoc (box 0 60 -1 9 70 1 "stone") "0,64,0" "air" "0,65,0" "air" "4,64,0" "air" "4,65,0" "air")
        p (hiding-with-pickaxe world #{[4 63 0]})
        seconds (fn [absent] (absent-at [:level] absent #(:seconds (choose/route-answer p [0 64 0] [4 64 0] (constantly true) (constantly false) false false {:worth-s tu/order-worth}))))]
    (is (< (seconds 0.01) (seconds 0.5) (seconds 0.9)) "the likelier a level step's floor is a void, the dearer the way")))

(deftest a-walk-over-an-unseen-floor-of-a-loosened-pass-is-priced-over-the-chance-the-floor-is-none
  (let [world (assoc (box 0 60 -1 9 70 1 "stone") "0,64,0" "air" "0,65,0" "air" "1,64,0" "air" "1,65,0" "air" "2,64,0" "air" "2,65,0" "air" "3,64,0" "air" "3,65,0" "air")
        p (hiding-with-pickaxe world #{[1 63 0] [2 63 0]})
        seconds (fn [absent] (absent-at [:level] absent #(:seconds (choose/route-answer p [0 64 0] [3 64 0] (constantly true) (constantly false) true false {:worth-s tu/order-worth}))))]
    (is (< (seconds 0.01) (seconds 0.5)) "the likelier a level step's unseen floor is a void, the dearer the walk over it")))

(deftest a-route-that-ends-in-range-short-of-the-goal-is-not-priced-over-the-goals-unseen-floor
  (let [world (assoc (box 0 60 -1 9 70 1 "stone") "0,64,0" "air" "0,65,0" "air" "4,64,0" "air" "4,65,0" "air")
        p (hiding-with-pickaxe world #{[4 63 0]})
        seconds (fn [absent] (absent-at [:level :up :down :drop :drop-4 :drop-8] absent #(:seconds (choose/route-answer p [0 64 0] [4 64 0] (constantly true) (constantly false) false false {:worth-s tu/order-worth :range 2 :near [4 64 0]}))))]
    (is (= (seconds 0.01) (seconds 0.9)) "its end stands on a seen floor")))

(def drop-world
  "A floor at y 63 and a wide stone platform at y 79 under feet [0 80 0], its edge far off: the goal [0 64 0] lies a 16-block drop below."
  (merge (box -3 63 -3 3 63 3 "stone") (box -30 79 -30 30 79 30 "stone")))

(defn straight-dig
  "route-answer's route when the straight dig is the answer (a :read-s that prices every search past it) and what the body-prices say of the fall."
  [p goal floors-unseen-ok? route-opts]
  (choose/route-answer p [0 80 0] goal (constantly true) (constantly false) floors-unseen-ok? false (merge {:read-s 1 :worth-s tu/order-worth} route-opts)))

(deftest the-straight-dig-over-a-drop-is-priced-at-the-falls-hp-cost
  (let [p (tu/fake {:blocks drop-world :self {:pos {:x 0.5 :y 80 :z 0.5}} :inventory [{:name "stone_pickaxe" :count 1}]})
        {:keys [dig seconds]} (straight-dig p [0 64 0] false {})
        fall-s ((:fall-s (dig-route/body-prices p (constantly false))) 16)]
    (is (= [[0 79 0]] dig) "the platform under the feet is dug")
    (is (> seconds fall-s) "the 16-block drop it opens costs its hp, not a stair of one-block steps")))

(deftest the-straight-dig-takes-no-fall-the-calls-health-spend-refuses
  (let [p (tu/fake {:blocks drop-world :self {:pos {:x 0.5 :y 80 :z 0.5}} :inventory [{:name "stone_pickaxe" :count 1}]})]
    (is (some? (straight-dig p [0 64 0] false {})) "the body's own limit allows the drop")
    (is (nil? (straight-dig p [0 64 0] false {:max-damage 7})) "a call that spends 7 hp takes no 16-block drop")))

(deftest a-landing-part-way-down-the-straight-dig-on-an-unseen-floor-is-priced-over-the-chance-it-is-none
  (let [world (merge (box -2 60 -2 2 80 2 "stone") (zipmap (map #(str "0," % ",0") (range 71 80)) (repeat "air")) {"0,80,0" "air" "0,81,0" "air" "0,65,0" "air" "0,64,0" "air"})
        p (hiding-with-pickaxe world #{[0 70 0]})
        seconds (fn [absent] (absent-at [:drop-8] absent #(:seconds (straight-dig p [0 64 0] true {}))))]
    (is (< (seconds 0.01) (seconds 0.9)) "the likelier the floor a 9-block fall lands on is a void (a :drop-8's chance), the dearer the way")
    (let [self (.self p)
          fall {:equipment (cost/equipment-of (.-equipment self)) :max-drop (:max-fall (dig-route/body-prices p (constantly false))) :carried 0 :share (wworld/unseen-share p)}
          beyond (- (cost/unseen-fall-s :drop-8 9 fall) ((:fall-s (dig-route/body-prices p (constantly false))) 9))]
      (is (< 0 beyond) "a void under the 9-block drop falls on")
      (is (< (Math/abs (- (- (seconds 0.9) (seconds 0.01)) (* 0.89 beyond))) 1e-6)
          "the toll is the chance times the fall the void adds past the 9 blocks the step already falls"))))

(deftest water-seen-only-as-the-dig-route-reaches-it-is-dug-past
  ;; KitOak94 and the live fixture: the plan read the pool beside the shaft's top as unseen rock (it shows once the digging starts);
  ;; the dig there was refused :fluid-adjacent and the route ended :refused. A dig beside water is dug, its flood priced.
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> (box 2 0 2 30 5 30 "stone") (assoc "19,2,16" "air" "19,3,16" "air" "20,5,16" "water"))
              {:keys [eng p seen]} (setup {:self {:pos {:x 16.5 :y 6 :z 16.5}} :blocks world :inventory [{:name "stone_pickaxe" :count 1}]})
              sensed (.-sensedAt p)
              _ (aset p "sensedAt" (fn [q] (if (and (= [20 5 16] [(.-x q) (.-y q) (.-z q)]) (empty? (calls p "dig"))) #js {:unknown true} (sensed q))))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [19 2 16] :range 0 :unknown false})))]
          (is (some #{[19 5 16]} (map :cell (choose/route-cells p [16 6 16] [19 2 16] (constantly true) (constantly false) false false {:range 0 :near [19 2 16]})))
              "planned before the water shows, the route digs beside it")
          (core/submit! eng (tu/order '(recording-parent {})) {})
          (await (tick-out! eng 1000))
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text])))
          (is (= [19 2 16] (feet p)))
          (is (not-any? #(= :refused (:reason %)) (events-of seen :go-to.escalation-stopped)))
          (is (some #{{"x" 19 "y" 5 "z" 16}} (map #(js->clj (.-pos (.-args %))) (calls p "dig"))) "the cell beside the water is dug"))))))

(deftest a-route-dig-refused-for-a-falling-block-ends-the-route
  ;; the planner does not know the block that would fall: a route planned again would dig the same cell, refused again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [refused {:kind :condition-failed :cause {:reason :hazard :hazards [:falling-block]} :data {:dug false}}
              {:keys [seen]} (with-redefs [b/dig-cell-ending! (fn [_ _ _] (js/Promise.resolve refused))]
                               (await (dig-route-run! below-pocket [{:name "stone_pickaxe" :count 1}] nil)))]
          (is (some #(= [:dig-route :refused] [(:step %) (:reason %)]) (events-of seen :go-to.escalation-stopped))
              (pr-str (map #(select-keys % [:step :reason]) (events-of seen :go-to.escalation-stopped)))))))))

;; rock-dig? takes a stair or pillar over the dig leg only when its way-total is no more than the leg's
(deftest a-stair-or-pillar-beats-the-dig-leg-only-by-its-whole-way
  (let [leg {:step :dig-route}
        totals (fn [stair-s leg-s] {{:step :stair} stair-s leg leg-s {:step :pillar} stair-s {:step :approach} 0})
        beats (fn [e stair-s leg-s] (esc/beats-leg? e leg (totals stair-s leg-s)))]
    (is (true? (beats {:step :stair} 10 20)) "a stair that is cheaper in all wins")
    (is (true? (beats {:step :pillar} 20 20)) "a tie goes to the stair or pillar")
    (is (false? (beats {:step :stair} 21 20)) "a stair whose own cuts are short but whose walk on is long loses")
    (is (false? (beats {:step :approach} 0 20)) "only a stair or pillar may displace the leg")
    (is (false? (beats nil 0 20)))))

(defn remembering-within
  "A fake body at feet cell [0 64 0] in blocks carrying inv that remembers, and senses, only the cells within r of it."
  [blocks inv r]
  (let [f (tu/fake {:blocks blocks :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory inv})
        raw (fake-raw/create f)
        per (perception/create raw {:now (constantly 1000000)})
        ^js st (:st per)
        n (js/Math.ceil r)]
    (doseq [x (range (- n) (inc n)) y (range (- 64 n) (+ 65 n)) z (range (- n) (inc n))
            :when (<= (js/Math.hypot (- (+ x 0.5) 0.5) (- y 64) (- (+ z 0.5) 0.5)) r)]
      (store/record! st (store/store-of st (.-dim st)) x y z (.stateAt raw x y z) 1000000))
    (tu/seeing-within (perception/wrap f per) r)))

(defn climb-searched
  "route-answer's climb from [0 64 0] to [8 72 0] through stone-mass for a body carrying inv that remembers the cells within
  2 of it, the memory's open heights read (open?) or not: [seconds feet-cells cells-expanded]."
  [inv open?]
  (let [p (remembering-within stone-mass inv 2)
        n (atom 0)
        search dig-route/search
        a (settings/with-settings {:jobs.lib.dig-route/max-nodes 100000}
            #(with-redefs [dig-route/search (fn [& args] (let [run (apply search args)] (fn [& r] (let [out (apply run r)] (swap! n + (second out)) out))))
                           seen-sections/open-heights (if open? seen-sections/open-heights (constantly nil))]
               (choose/route-answer p [0 64 0] [8 72 0] (constantly true) (constantly false) false false {})))]
    [(:seconds a) (map :to (:steps a)) @n]))

(deftest a-climb-through-rock-with-no-open-cell-remembered-at-its-heights-is-searched-narrow-whatever-the-pickaxe
  ;; each rise there is a stair's step (3 s), not a walk's (0.3 s): priced so in the bounds, a quicker pickaxe no longer
  ;; buys a wider ball of rock searched (without: bare 743 cells, iron 6060, diamond 7467)
  (let [bare (nth (climb-searched [] true) 2)]
    (is (< bare 600) (str bare " cells bare-handed"))
    (doseq [inv (cons [] (map #(vector {:name % :count 1}) ["wooden_pickaxe" "stone_pickaxe" "iron_pickaxe" "diamond_pickaxe"]))
            :let [[s steps cells] (climb-searched inv true)
                  [s0 steps0] (climb-searched inv false)]]
      (is (= [s0 steps0] [s steps]) (str inv ": the same route at the same price"))
      (is (<= cells bare) (str inv ": " cells " cells")))))
