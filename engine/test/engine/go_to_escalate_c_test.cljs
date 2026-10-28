(ns engine.go-to-escalate-c-test
  "jobs.movement.go-to escalation (pillar, stair, clear-path, a walk to a wall) and jobs.lib.escape, against the fake
  world."
  (:require [clojure.string :as str]
            [jobs.movement.go-to.search-bound :as search-bound]
            [engine.contract :as contract]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-test :as gt]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.escape :as escape]
            [jobs.lib.near :as near]
            [jobs.movement.go-to.cut :as gcut]
            [jobs.lib.ledger :as ledger]
            [plan.shape :as shape]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.movement.go-to.escalation :as esc]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [triggers.access.scaffold-left :as scaffold-left]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]
            [engine.go-to-escalate-support :refer [setup recording-parent tick-out! run-job! feet block events-of calls pit-cells pit stone-pit in-pit room in-room hollow ba ledge-room on-ledge wide-pit plank-yard holes dug-cells foreign trespass deep-pit stone-room run-in-shelter! ledger restores-after-the-last-stair? deep-shaft own-pit-wall cobble-room room-and-deck pocket free-by-pocket high-pocket moat door-cell dark-at gap-tunnel long-loop loop-tolls stair-below-surface cave below-pocket dig-route-run!]]))

(deftest escape-choose-walks-along-the-wall-when-the-wall-in-line-is-an-iron-door
  (let [iron (-> room (assoc "3,64,0" "iron_door" "3,65,0" "iron_door"))
        c (escape/choose (tu/fake (merge in-room {:blocks iron :self {:pos {:x 2 :y 64 :z 0}}})) [2 64 0] [8 64 0])]
    (is (= :approach (:step c)))
    (is (not= [2 64 0] (:pos c)))))

(deftest escape-choose-searches-for-a-door-spot-once
  (let [iron (-> room (assoc "3,64,0" "iron_door" "3,65,0" "iron_door"))
        calls (atom 0)
        real escape/door-spot]
    (with-redefs [escape/door-spot (fn [& args] (swap! calls inc) (apply real args))]
      (escape/choose (tu/fake (merge in-room {:blocks iron :self {:pos {:x 2 :y 64 :z 0}}})) [2 64 0] [8 64 0]))
    (is (= 1 @calls))))

(deftest go-to-escalates-on-a-door-that-doors-never-refuses
  (let [ok? (fn [result doors] (choose/escalate-reason? result doors))]
    (is (true? (ok? {:reason :exhausted} :shut)))
    (is (true? (ok? {:reason :start-enclosed} :shut)) "a pen sealed in the loaded world, goal far and unloaded (card 5a822a99)")
    (is (true? (ok? {:reason :goal-unloaded} :shut)) "a shut-in body's search that ended at a loaded edge beyond its walls (escalate? also requires it shut in)")
    (is (true? (ok? {:reason :abilities :kind :open} :never)) "the planner's way through a door, refused as a wall")
    (is (false? (ok? {:reason :abilities :kind :open} :shut)))
    (is (false? (ok? {:reason :abilities :kind :break} :never)))
    (is (false? (ok? {:reason :stuck} :never)))))

(deftest escape-enclosed-counts-doors-as-walls-under-doors-never
  (let [wide (merge room (box -20 63 -20 30 63 20 "stone"))
        p (tu/fake (merge in-room {:blocks (assoc wide "3,64,0" "oak_door" "3,65,0" "oak_door")}))]
    (is (false? (escape/enclosed? p :shut :ground)) "a wooden door is a way out")
    (is (true? (escape/enclosed? p :never :ground)) "doors :never: the room is shut")))

;; ------------------------------------------------------------------ other headings (card 4ad29ed7)
(deftest escape-choose-skips-only-the-refused-stair-heading
  (let [p (tu/fake (merge in-pit {:blocks pit}))
        first-heading (:heading (escape/choose p [0 61 0] [10 64 0]))
        again (escape/choose p [0 61 0] [10 64 0] (constantly true) {:skip #{[:stair first-heading]}})]
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
          (is (some #{:clear-path} (map :step (events-of seen :go-to.escalated)))))))))

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

(deftest a-walk-only-call-walks-to-the-wall-of-a-walled-in-goal-when-its-budget-allows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spend from at] [[nil {:x 0 :z 0} [4 64 0]] [{:wall-time 0.5} {:x 0 :z 0} [0 64 0]] [nil {:x 3 :z 2} [4 64 2]]]]
          (let [{:keys [out p]} (await (run-job! (update-in free-by-pocket [:self :pos] merge from) (cond-> {:pos [8 64 0] :range 0 :ways #{:walk}} spend (assoc :spend spend))))]
            (is (= :goal-enclosed (:why @out)) (pr-str spend @out))
            (is (= at (feet p)) (str spend ": the nearest cell to the goal, when the walk there fits the budget"))
            (is (empty? (calls p "dig")))))))))

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
                                           gcut/restart-after-jump! (fn [c]
                                                                      (swap! jumps inc)
                                                                      (swap! marks conj (:wall-walked (ctx/mem c)))
                                                                      (real-restart c))]
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
          (is (= [:approach :clear-path] (take 2 (map :step (events-of seen :go-to.escalated)))))
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

(deftest door-back-is-a-seen-standable-cell-or-one-not-seen-yet
  (let [back (escape/door-back (tu/fake door-cell) [0 64 0])
        [bx by bz] back
        walled (update door-cell :blocks assoc (str bx "," by "," bz) "stone")
        seen-wall (tu/fake walled)
        dark-wall (tu/fake walled)
        _ (dark-at dark-wall [back])]
    (is (some? back))
    (is (nil? (escape/door-back seen-wall [0 64 0])) "a wall it sees")
    (is (= back (escape/door-back dark-wall [0 64 0])) "a wall it has not seen: go and look, not an x-ray read")
    (let [dark-door (tu/fake door-cell)]
      (dark-at dark-door [[0 64 0]])
      (is (nil? (escape/door-back dark-door [0 64 0])) "a door cell it has not sensed is no known door: not read through"))))

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
    (is (= [2 64 0] (escape/wall-spot seen [0 64 0] [9 64 0])))
    (is (not= [2 64 0] (escape/wall-spot dark [0 64 0] [9 64 0])) "the cell it would stand in never seen: not offered")
    (is (true? (escape/open-door-beside? door-seen [0 64 0])))
    (is (false? (escape/open-door-beside? door-dark [0 64 0])) "the only way to the door runs through a cell never seen")))

(deftest door-spot-is-the-cheapest-cell-whose-door-leaves-the-body-cheaper-off-toward-the-goal
  (let [yard (merge (box -12 63 -8 12 63 8 "stone") (box 5 64 -8 5 65 8 "stone") (box -5 64 -8 -5 65 8 "stone"))
        p (tu/fake {:blocks yard :self {:pos {:x 0.5 :y 64 :z 0.5}}})]
    (is (= [4 64 0] (escape/door-spot p [0 64 0] [10 64 0] (constantly true) #{})) "the wall ahead is the door; the one behind is not")
    (is (nil? (escape/door-spot p [0 64 0] [10 64 0] (constantly false) #{})) "a wall that may not be dug is no door")
    (is (not= [4 64 0] (escape/door-spot p [0 64 0] [10 64 0] (constantly true) #{[4 64 0]})) "a cell walked to already is not offered again")))

(deftest wall-spot-is-the-cheapest-wall-toward-the-goal-not-the-nearest
  (let [yard (merge (box -12 63 -8 12 63 8 "stone") (box -2 64 2 -2 65 2 "stone") (box 7 64 0 7 65 0 "stone"))
        p (tu/fake {:blocks yard :self {:pos {:x 0.5 :y 64 :z 0.5}}})]
    (is (= [6 64 0] (escape/wall-spot p [0 64 0] [10 64 0])) "a wall 6 steps off on the way beats one 5 steps off behind the body")))

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

(deftest a-stair-is-priced-as-the-climb-route-prices-it
  (let [p (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]})
        route (choose/route-answer p [0 64 0] [3 67 0] (constantly true) (constantly false) false false {})]
    (is (= [:walk] (distinct (map :move (:steps route)))))
    (is (= 3 (count (:steps route))))
    (is (< 0 (:seconds route)))
    (is (= (:seconds route) (choose/price p [0 64 0] {:step :stair :heading :east :steps 3})))))

(def open-stair-mass
  "stone-mass with the cells of a 3-step stair east from [0 64 0] already open."
  (apply dissoc stone-mass (map #(str (clojure.string/join "," %)) (escape/stair-cuts [0 64 0] [1 0] 3))))

(deftest a-stair-through-open-cells-is-priced-as-the-climb-route-prices-it
  (let [p (tu/fake {:blocks open-stair-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]})
        route (choose/route-answer p [0 64 0] [3 67 0] (constantly true) (constantly false) false false {})]
    (is (= 3 (count (:steps route))))
    (is (= (:seconds route) (choose/price p [0 64 0] {:step :stair :heading :east :steps 3})))
    (is (< (:seconds route) (cw/stair-cost 3)) "a cut-free step is no dearer than a walk step")))

(deftest a-new-stair-step-is-priced-as-a-step-cut-through-stone-and-dearer-unseen
  (let [inv [{:name "iron_pickaxe" :count 1}]
        body #(tu/seeing-within (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory inv}) %)
        seen (body 50)
        step {:step :stair :heading :east :steps 1}]
    (is (= (choose/price (body 1) [0 64 0] step) (choose/new-stair-step-s seen)) "the climb route's price of one unseen stone step")
    (is (< (cw/stair-step-s) (choose/new-stair-step-s seen)) "the cuts are paid on top of the stair's step")
    (is (< (choose/price seen [0 64 0] step) (choose/new-stair-step-s seen)) "seen cells cost less")))

(deftest an-unseen-stair-and-pillar-are-priced-as-the-climb-route-prices-them
  (let [inv [{:name "iron_pickaxe" :count 1}]
        body #(tu/seeing-within (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory inv}) %)
        p (body 4.2)
        route (choose/route-answer p [0 64 0] [3 67 0] (constantly true) (constantly false) false false {})
        stair {:step :stair :heading :east :steps 3}
        unseen-s (cost/unseen-route-s (wworld/unseen-share p) (constantly :ground) [{:x 0 :y 0 :z 0}])]
    (is (= (:seconds route) (choose/price p [0 64 0] stair)))
    (is (< (choose/price (body 50) [0 64 0] stair) (choose/price p [0 64 0] stair)) "unseen cells cost more")
    (are [r n] (= (+ (cw/pillar-cost 2) (* n unseen-s)) (choose/price (body r) [0 64 0] {:step :pillar :height 2}))
      50 0
      2.9 1)))

(deftest a-fill-competes-by-price-with-the-stair-and-the-pillar
  (let [p (tu/fake {:blocks stone-mass :self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory [{:name "iron_pickaxe" :count 1}]})
        fill #(hash-map :step :fill-gap :cell [5 63 0] :item "dirt" :count %)
        stair #(hash-map :step :stair :heading :east :steps %)
        three (stair 3)]
    (is (< (cw/fill-cost 9) (choose/price p [0 64 0] three)))
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
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (= :dig-route (:step (first (events-of seen :go-to.escalated)))) "the riser is cut before any stair")
          (is (= "air" (block p [6 63 0])) "the step up beyond the gap is cut level with the takeoff"))))))

(deftest go-to-digs-a-stair-up-instead-of-a-long-priced-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0 :y 50 :z 0}} :blocks long-loop :inventory [{:name "iron_pickaxe" :count 1}]}
                                              {:pos [10 60 2] :range 1 :tolls loop-tolls}))]
          (is (= {:arrived true} @out))
          (is (= [:approach :stair] (distinct (map :step (events-of seen :go-to.escalated)))) "a climb of stairs, not the loop"))))))

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

(deftest a-go-to-sealed-in-a-cave-without-escalate-says-why-each-time-it-plans-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 3.5 :y 64 :z 0.5}} :blocks cave
                                                   :inventory [{:name "iron_pickaxe" :count 1}]}
                                                  {:pos [30 64 0] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= [false :exhausted] ((juxt :arrived :why) @out)) (pr-str @out))
          (is (= [[:exhausted 1] [:exhausted 2]] (mapv (juxt :reason :try) (events-of seen :go-to.retrying)))
              (pr-str (events-of seen :go-to.retrying))))))))

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
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= [:clear-path] (mapv :step (events-of seen :go-to.escalated))) "three stuck walks round the wall, then a door through it"))))))

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
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (false? (:arrived @out)) (pr-str @out))
          (is (empty? (events-of seen :go-to.escalated)) "a caller that re-aims itself gets the stuck walk back, no door dug")
          (is (empty? (calls p "dig"))))))))

(deftest a-dig-route-never-digs-a-block-built-there-after-it-was-planned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [below (apply dissoc (box -20 55 -20 20 63 20 "stone") ["3,60,0" "3,61,0"])
              around [[2 60 0] [4 60 0] [3 60 1] [3 60 -1] [3 59 0] [2 61 0] [4 61 0] [3 61 1] [3 61 -1] [3 62 0]]
              {:keys [eng p seen]} (setup {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks below
                                           :inventory [{:name "stone_pickaxe" :count 1}]})
              walled? (atom false)
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [3 60 0] :range 0})))]
          (.override (.-world p) "dig"
                     (fn [token a impl]
                       (when-not @walled?
                         (reset! walled? true)
                         (swap! (fake/state p) update :blocks merge (zipmap around (repeat "bricks"))))
                       (impl token a)))
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (false? (:arrived @out)) (pr-str @out))
          (is (= (repeat (count around) "bricks") (map #(block p %) around))
              "a brick wall put round the goal after the route was planned is someone's build")
          (is (some #(= [:dig-route :not-diggable] ((juxt :step :reason) %)) (events-of seen :go-to.escalation-stopped))
              (pr-str (events-of seen :go-to.escalation-stopped))))))))

(deftest a-free-body-with-one-pickaxe-or-none-lets-its-stair-fetch-a-tool
  (let [flat {:blocks (box -20 63 -20 20 63 20 "stone") :self {:pos {:x 0 :y 64 :z 0}}}
        fetch? #(choose/fetch-tool? {:primitives (tu/fake %) :args {}})
        picks #(vec (repeat % {:name "stone_pickaxe" :count 1}))]
    (is (= [true true false] (map #(fetch? (assoc flat :inventory (picks %))) [0 1 2])))
    (is (false? (fetch? (merge {:blocks room :inventory (picks 1)} in-room))) "shut in: no walk to a tool")
    (is (= [(esc/ways-for true) #{:carried}] (map #(:ways (second (esc/escalation-job {:step :stair :heading :east :steps 2} nil false false %))) [true false])))))

(deftest a-stair-through-its-own-pen-picks-up-its-drops
  (is (= [true false] (map #(:collect (second (esc/escalation-job {:step :stair :heading :east :steps 2 :own-cut %} nil false false false))) [true nil]))))

(deftest a-door-through-its-own-pen-shuts-its-mouth-behind-the-body
  (is (= [true nil] (map #(:close-behind (second (esc/escalation-job {:step :clear-path :heading :east :own-cut %} nil false false false))) [true nil]))))

(deftest a-dig-route-s-digs-fetch-a-pickaxe-as-fetch-tool-says
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pick {:name "stone_pickaxe" :count 1}
              one (await (dig-route-run! below-pocket [pick] nil))
              two (await (dig-route-run! below-pocket [pick pick] nil))]
          (is (= [true true] (map (comp :arrived deref :out) [one two])))
          (is (= #{true} (set (map (comp boolean :chest :ways) @(:digs one)))) "one pickaxe: its breaking is no stop")
          (is (= #{false} (set (map (comp boolean :chest :ways) @(:digs two)))) "a spare pickaxe: no fetch")
          (is (= #{true} (set (map :need-drop @(:digs one)))) "a fetching dig needs a tool, else it punches the rock by hand and the route was priced for a tool it never gets")
          (is (= #{false} (set (map :need-drop @(:digs two)))))
          (is (= #{#{:lava-adjacent}} (set (map :accept @(:digs one)))) "seen lava beside a route cell is sealed by the dig, not a refusal that ends the route"))))))

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
        cells (choose/route-cells p [0 64 0] [3 64 0] (constantly true) (constantly false))]
    (is (some #(= 1 (nth (:cell %) 2)) cells) "it digs round the seen-open cell whose floor is unseen")))

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
  ;; seeing 30 round it, the pocket 70 off reads as through rock (no dig route); the plain below the edge is seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (box 0 50 0 99 54 63 "stone") (box 0 55 0 20 60 20 "stone") {"80,51,56" "air" "80,52,56" "air"})
              {:keys [out p seen]} (await (run-job! {:sight 30 :self {:pos {:x 10.5 :y 61 :z 10.5}} :blocks blocks :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [80 51 56] :range 0}))]
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text :escalation])))
          (is (= [80 51 56] (feet p)))
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
            (core/submit! eng '(recording-parent) {})
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

(deftest a-dig-route-over-guesses-is-priced-once-over-the-chance-they-all-hold
  ;; each guess holds with 1 - its class's risk: the route's seconds over the product, against :worth-s; a guess beside
  ;; seen undiggable rock (a light) is of class :beside
  (let [world (assoc (box 0 60 -1 9 70 1 "stone") "0,64,0" "air" "0,65,0" "air" "4,64,0" "air" "4,65,0" "air" "2,66,0" "glowstone")
        p (hiding-with-pickaxe world #{[1 64 0] [1 65 0] [2 64 0] [2 65 0] [3 64 0] [3 65 0]})
        answer #(choose/route-answer p [0 64 0] [4 64 0] (constantly true) (constantly false) false false %)
        {:keys [seconds dig]} (answer {})
        hold (fn [beside apart] (reduce * (map #(if (= % [2 65 0]) (- 1 beside) (- 1 apart)) dig)))
        priced (fn [risk worth] (some? (answer {:guess-risk risk :worth-s worth})))]
    (is (some #{[2 65 0]} dig) "the straight tunnel, under the light")
    (is (= seconds (:seconds (answer {:guess-risk {:beside 0.5 :apart 0.2} :worth-s ##Inf}))) "the risk leaves the search alone")
    (is (priced {:beside 0.5 :apart 0.2} (* 1.01 (/ seconds (hold 0.5 0.2)))) "priced once: no second factor on the route")
    (is (not (priced {:beside 0.5 :apart 0.2} (* 0.99 (/ seconds (hold 0.5 0.2))))))
    (is (= [[2 65 0]] (map :cell (filter :beside (choose/route-cells p [0 64 0] [4 64 0] (constantly true) (constantly false))))))))

(deftest a-dig-route-goes-round-cells-may-dig-refuses
  (let [world (assoc (box 0 60 -1 9 70 1 "stone") "0,64,0" "air" "0,65,0" "air" "4,64,0" "air" "4,65,0" "air")
        p (tu/fake {:blocks world :inventory [{:name "stone_pickaxe" :count 1}]})
        straight (set (map :cell (choose/route-cells p [0 64 0] [4 64 0] (constantly true) (constantly false))))
        round (choose/route-cells p [0 64 0] [4 64 0] #(not (contains? straight %)) (constantly false))]
    (is (= #{[1 64 0] [1 65 0] [2 64 0] [2 65 0] [3 64 0] [3 65 0]} straight))
    (is (seq round) "the refused tunnel is dug round")
    (is (not-any? (comp straight :cell) round))))

(deftest a-dig-route-that-takes-longer-than-the-body-would-dig-is-not-planned
  (let [world (assoc (box 0 60 -1 9 70 1 "stone") "0,64,0" "air" "0,65,0" "air")
        route (fn [inventory] (choose/route-cells (tu/fake {:blocks world :inventory inventory}) [0 64 0] [8 64 0] (constantly true) (constantly false)))]
    (is (nil? (route [])) "8 stones by hand take minutes")
    (is (seq (route [{:name "stone_pickaxe" :count 1}])) "the same take seconds with a pickaxe")))
(deftest a-dig-route-through-the-bodys-own-scaffold-is-not-held-to-the-time-cap
  (let [world (assoc (box 0 60 -1 9 70 1 "cobblestone") "0,64,0" "air" "0,65,0" "air")
        route (fn [own?] (choose/route-cells (tu/fake {:blocks world :inventory []}) [0 64 0] [8 64 0] (constantly true) own?))]
    (is (nil? (route (constantly false))) "8 foreign blocks by hand take minutes")
    (is (seq (route (constantly true))) "its own pillar left behind is dug out however long it takes")))

(deftest a-dig-route-is-priced-with-the-pickaxe-the-body-would-fetch
  (let [world (assoc (box 0 60 -1 40 70 1 "stone") "0,64,0" "air" "0,65,0" "air")
        route (fn [inventory fetch?] (choose/route-cells (tu/fake {:blocks world :inventory inventory}) [0 64 0] [8 64 0] (constantly true) (constantly false) false fetch?))]
    (is (nil? (route [] false)) "shut in: bare hands cannot tunnel 12 stones")
    (is (seq (route [] true)) "a free body crafts a pickaxe and tunnels")
    (is (seq (route [{:name "wooden_pickaxe" :count 1}] false)) "a wooden pickaxe tunnels 12 blocks")
    (is (seq (route [{:name "stone_pickaxe" :count 1}] false)) "a stone pickaxe tunnels 12 blocks")))

(deftest a-pocket-floor-seen-only-from-inside-is-no-bar-when-floors-unseen-are-ok
  (let [world (apply dissoc (box 0 60 -1 6 70 1 "stone") (for [x (range 3) y [64 65]] (str x "," y ",0")))
        p (hiding-with-pickaxe world #(= [2 63 0] %))
        cells #(choose/route-cells p [0 64 0] [3 64 0] (constantly true) (constantly false) %)]
    (is (some #(= 1 (nth (:cell %) 2)) (cells false)))
    (is (not-any? #(= 1 (nth (:cell %) 2)) (cells true)) "it goes straight over the unseen floor")))

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
  (doseq [e [{:step :pillar :height 2} {:step :stair :heading :east :steps 2} {:step :clear-path :heading :east} {:step :approach :pos [1 64 0]}]]
    (is (= '(always) (get-in (second (esc/escalation-job e nil false false false)) [:checks :fit?])) (str (:step e)))))

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
              _ (core/submit! eng '(recording-parent) {})
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
        (let [{:keys [out p]} (await (run-job! {:self {:pos {:x 61.5 :y 69 :z 9.5}} :blocks oriel-house :light [0 0]} {:pos [63 69 12] :range 1 :one-way :closed}))]
          (is (false? (:arrived @out)) (pr-str @out))
          (is (= 69 (second (feet p))) "the caller's :closed forbids the drop"))))))

(deftest go-to-a-house-above-an-undiggable-pit-stays-in-instead-of-dropping-into-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge oriel-house (into {} (for [[k v] oriel-house :when (= "dirt" v)] [k "bedrock"])))
              {:keys [out p]} (await (run-job! {:self {:pos {:x 61.5 :y 69 :z 9.5}} :blocks blocks :light [0 0]} {:pos [63 69 12] :range 1}))]
          (is (false? (:arrived @out)) (pr-str @out))
          (is (= 69 (second (feet p))) "the body did not drop into the pit"))))))

(deftest a-dig-leg-whose-search-spends-its-slice-goes-on-in-the-next
  (let [p (tu/fake {:blocks (box -2 60 -2 20 70 2 "stone") :inventory [{:name "stone_pickaxe" :count 1}]})
        c {:primitives p :args {}}
        slice (atom nil)
        leg #(let [s (choose/route-slice "leg-resume")] (reset! slice s) (choose/dig-leg c [0 64 0] [12 64 0] (constantly true) {:own? (constantly false) :skip (constantly false) :slice s}))]
    (with-redefs [ctx/mem (constantly {}) ctx/left (constantly {})]
      (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
        #(let [first-leg (leg)
               legs (loop [n 0] (let [e (leg)] (if (and (= :searching (:step e)) (< n 300)) (recur (inc n)) e)))]
           (is (= {:step :searching} first-leg))
           (is (= :dig-route (:step legs)) (pr-str (:step legs))))))))

(deftest frontier-way-charges-its-flood-to-the-slice
  (let [p (tu/fake {:blocks (merge (box -3 63 -3 3 63 3 "stone") (box 5 40 -3 9 40 3 "stone"))})
        slice (choose/route-slice "frontier-flood")
        before @(:left slice)
        opts {:own? (constantly false) :skip (constantly false) :slice slice}]
    (choose/frontier-way p [0 64 0] [40 40 0] (constantly true) opts)
    (is (< @(:left slice) before) "the flood of the land's cells came off the slice")))

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
