(ns engine.go-to-escalate-a-test
  "jobs.movement.go-to escalation (pillar, stair, clear-path, a walk to a wall) and jobs.lib.escape, against the fake
  world."
  (:require [engine.contract :as contract]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-test :as gt]
            [engine.go-to-support :refer [hiding]]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [jobs.lib.escape :as escape]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.ledger :as ledger]
            [plan.shape :as shape]
            [jobs.movement.go-to.escalation :as esc]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [triggers.access.scaffold-left :as scaffold-left]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]
            [engine.go-to-escalate-support :refer [setup recording-parent tick-out! run-job! feet block events-of calls pit-cells pit stone-pit in-pit room in-room hollow ba ledge-room on-ledge wide-pit plank-yard holes dug-cells foreign trespass deep-pit stone-room run-in-shelter! ledger restores-after-the-last-stair? deep-shaft own-pit-wall cobble-room room-and-deck pocket free-by-pocket high-pocket moat door-cell dark-at gap-tunnel long-loop loop-tolls stair-below-surface cave below-pocket dig-route-run!]]))

(deftest escape-pit-depth-counts-the-walled-levels
  (is (= 3 (escape/pit-depth (ba {:blocks pit}) [0 61 0])))
  (is (= 0 (escape/pit-depth (ba {:blocks room}) [0 64 0])) "the middle of a room is no pit"))

(deftest escape-door-is-the-solid-cells-of-a-thin-wall-with-floor-beyond
  (is (= {:cells [[3 65 0] [3 64 0]] :through [4 64 0]} (escape/door (ba {:blocks room}) [2 64 0] [1 0] 3)))
  (is (nil? (escape/door (ba {:blocks room}) [1 64 0] [1 0] 3)) "no wall right in front")
  (is (nil? (escape/door (ba {:blocks pit}) [0 61 0] [1 0] 3)) "no floor beyond the pit's wall"))

(deftest escape-door-never-goes-through-a-door-a-bed-or-a-container
  (doseq [b ["iron_door" "oak_door" "spruce_fence_gate" "red_bed" "chest" "barrel" "bedrock"]]
    (is (nil? (escape/door (ba {:blocks (assoc room "3,64,0" b)}) [2 64 0] [1 0] 3)) b)))

(deftest escape-door-is-at-most-max-thick
  (let [thick (merge room (box 3 64 -3 6 65 3 "dirt"))]
    (is (nil? (escape/door (ba {:blocks thick}) [2 64 0] [1 0] 3)) "4 thick")
    (is (= [7 64 0] (:through (escape/door (ba {:blocks thick}) [2 64 0] [1 0] 4))))))

(deftest escape-choose-walks-to-a-wall-first-from-the-middle-of-a-room
  (is (= {:step :approach :pos [2 64 0]} (escape/choose (tu/fake (merge in-room {:blocks room})) [0 64 0] [8 64 0]))
      "the nearest cell with the wall toward the goal beside it"))

(deftest escape-choose-stairs-on-from-a-ledge-when-it-carries-a-block-for-the-floor
  (is (= {:step :stair :heading :east :steps 4}
         (escape/choose (tu/fake (assoc on-ledge :inventory [{:name "cobblestone" :count 8}])) [0 66 0] [8 70 0])))
  (is (= {:step :none :why :no-wall} (escape/choose (tu/fake on-ledge) [0 66 0] [8 70 0]))))

(deftest escape-choose-pillars-only-with-enough-blocks
  (is (= {:step :pillar :height 2 :item "dirt"}
         (escape/choose (tu/fake (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})) [0 61 0] [10 64 0]))
      "a 3-deep pit: two blocks up, then a step onto the rim")
  (is (= {:step :stair :heading :east :steps 3}
         (escape/choose (tu/fake (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 1}]})) [0 61 0] [10 64 0]))))

(deftest escape-choose-pillars-beside-the-wall-of-a-wide-pit-it-may-not-dig
  (let [at-wall (fn [n] (tu/fake {:self {:pos {:x 2 :y 61 :z 0}} :blocks wide-pit :inventory [{:name "dirt" :count n}]}))]
    (is (= 0 (escape/pit-depth (ba {:blocks wide-pit}) [2 61 0])) "no level of a wide pit is walled on three sides")
    (is (= {:step :pillar :height 2 :item "dirt"} (escape/choose (at-wall 2) [2 61 0] [10 64 0] (constantly false)))
        "two blocks up, then a step onto the rim")
    (is (= {:step :none :why :no-dig} (escape/choose (at-wall 1) [2 61 0] [10 64 0] (constantly false))) "too few blocks")
    (is (= {:step :none :why :no-dig}
           (escape/choose (tu/fake {:self {:pos {:x 2 :y 61 :z 0}} :blocks (merge wide-pit (box -3 64 -3 3 64 3 "oak_planks"))
                                    :inventory [{:name "dirt" :count 9}]})
                          [2 61 0] [10 64 0] (constantly false)))
        "a roofed pit has no rim to step onto")))

(deftest escape-choose-rim-pillars-though-the-cells-over-the-rim-are-not-seen-from-the-pit
  ;; sweep 4410 wide-pit-no-dig-pillars-out: at the wall the rim hides the cells over it
  (let [p (tu/fake {:self {:pos {:x 2 :y 61 :z 0}} :blocks wide-pit :inventory [{:name "dirt" :count 8}]})
        over-rim? (fn [[x y _]] (and (= 3 x) (>= y 64)))]
    (aset p "sensedAt" (fn [pos] (if (over-rim? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    (is (= {:step :pillar :height 2 :item "dirt"} (escape/choose p [2 61 0] [10 64 0] (constantly false))))))

(deftest escape-choose-rim-pillars-on-the-side-toward-the-goal
  (let [low-south (apply dissoc wide-pit (for [x (range -3 4)] (str x ",63,3")))
        p (tu/fake {:self {:pos {:x 2 :y 61 :z 2}} :blocks low-south :inventory [{:name "dirt" :count 9}]})]
    (is (= {:step :pillar :height 2 :item "dirt"} (escape/choose p [2 61 2] [10 64 2] (constantly false)))
        "the east wall, toward the goal, though the south one is a block lower")
    (is (= {:step :pillar :height 1 :item "dirt"} (escape/choose p [2 61 2] [2 64 10] (constantly false)))
        "the south wall when the goal is south")))

(deftest escape-choose-walks-to-a-door-it-may-dig-before-it-rim-pillars
  (let [p (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :blocks plank-yard :inventory [{:name "dirt" :count 9}]})]
    (is (= :approach (:step (escape/choose p [2 64 0] [8 64 0]))) "the dirt beside the planks")
    (is (= {:step :pillar :height 1 :item "dirt"} (escape/choose p [2 64 0] [8 64 0] (constantly false)))
        "no dig allowed: over the wall")))

;; ------------------------------------------------------------------ go-to escalation
(deftest go-to-pillars-out-of-a-pit-when-it-carries-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]})
                                                {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [:pillar] (mapv :step (events-of seen :go-to.escalated))))
          (is (= 2 (count (calls p "jumpPlace"))) "two blocks up, then a step onto the rim")
          (is (empty? (calls p "dig"))))))))

(deftest go-to-at-low-food-escalates-instead-of-giving-up-on-a-sprint-only-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks (merge (box 0 63 0 0 63 0 "stone") (box 1 64 0 1 65 0 "stone") (box 1 64 1 1 64 1 "stone"))
                     :self {:pos {:x 0.5 :y 64 :z 0.5} :food 0}
                     :inventory [{:name "dirt" :count 5}]}
              {:keys [out seen]} (await (run-job! world {:pos [1 65 1] :range 0}))]
          (is (seq (events-of seen :go-to.escalated)) (pr-str @out)))))))

(deftest go-to-s-pillar-on-open-land-is-taken-back-once-it-ended
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out p]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5} {:name "iron_sword" :count 1}]})
                                                   'jobs.movement.go-to {:pos [10 64 0] :range 1} []))
              column [[0 61 0] [0 62 0]]]
          (is (= {:arrived true} @out))
          (is (every? #(= "dirt" (block p %)) column) "the pillar stands")
          (is (true? (scaffold-left/scaffold-left p (mem/view (:store eng)) {} (ew/of-data {} {} [])))
              "the scaffold-left trigger offers it once go-to ended")
          (core/submit! eng '(jobs.access.cleanup) {})
          (await (tick-out! eng 600))
          (is (every? #(= "air" (block p %)) column) "cleanup took every pillar block back"))))))

(deftest go-to-leaves-the-holes-it-dug-on-open-land
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [zones [nil [] [{:name "home" :owner "fake" :min [-60 0 -60] :max [60 120 60]}]]]
          (let [{:keys [eng out p seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})
                                                      'jobs.movement.go-to {:pos [10 64 0] :range 1 :ways #{:walk :stair}} zones))]
            (is (= {:arrived true} @out) (pr-str zones))
            (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))) (pr-str zones))
            (is (= #{} (holes eng)) "own diggings are not noted for refilling")
            (is (empty? (events-of seen :go-to.restored)) (pr-str zones))
            (is (every? #(= "air" (block p %)) (dug-cells p)) "the stair stays dug")))))))

(deftest go-to-stairs-out-of-a-pit-and-puts-back-what-it-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})
                                                'jobs.movement.go-to (merge {:pos [10 64 0] :range 1 :ways #{:walk :stair}} trespass) foreign))
              restored (first (events-of seen :go-to.restored))
              skipped (first (events-of seen :go-to.restore-skipped))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))))
          (is (= [[1 62 0] [1 63 0]] (:cells restored)) "the two blocks carried go back, lowest first")
          (is (= "dirt" (block p [1 62 0])))
          (is (= "dirt" (block p [1 63 0])))
          (is (= [[2 63 0]] (mapv :cell (:cells skipped))) "nothing left to put back the third"))))))

(deftest go-to-does-not-walk-back-down-its-stair-to-put-it-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x 0 :y 59 :z 0}} :blocks deep-pit
                                                     :inventory [{:name "dirt" :count 2}]}
                                                'jobs.movement.go-to (merge {:pos [10 64 0] :range 1} trespass) foreign))
              skipped (:cells (first (events-of seen :go-to.restore-skipped)))
              low (set (mapv :cell skipped))]
          (is (= {:arrived true} @out))
          (is (>= (second (feet p)) 64) "the body stays up on the rim")
          (is (seq low) "the cells deep below the rim are left for restore-broken")
          (is (some #(= :away (:why %)) skipped) "cells the body would have to go down the shaft for are left")
          (is (every? #(not= :away (:why %)) (remove #(> (- 64 (second (:cell %))) 2) skipped)) "only deep ones")
          (is (every? #(= "air" (block p %)) low)))))))

(deftest go-to-stair-ending-on-the-goal-cell-is-no-duplicate-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0 :y 59 :z 0}} :blocks deep-pit
                                                  :inventory [{:name "dirt" :count 2} {:name "iron_pickaxe" :count 1}]}
                                             'jobs.movement.go-to {:pos [5 64 0] :range 1} []))]
          (is (= {:arrived true} @out))
          (is (empty? (events-of seen :go-to.escalation-stopped)))
          (is (empty? (events-of seen :stair.stopped))))))))

(deftest go-to-clears-a-door-out-of-a-sealed-room-and-shuts-it-behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-room {:blocks room :inventory [{:name "dirt" :count 2}]})
                                                'jobs.movement.go-to (merge {:pos [8 64 0] :range 1} trespass) foreign))]
          (is (= {:arrived true} @out))
          (is (= [:clear-path] (mapv :step (events-of seen :go-to.escalated))) "the failed walks took it to the east wall")
          (is (empty? (events-of seen :go-to.restore-skipped)))
          (is (= [[3 64 0] [3 65 0]] (:cells (first (events-of seen :go-to.restored)))))
          (is (= "dirt" (block p [3 64 0])) "the door is shut again")
          (is (= "dirt" (block p [3 65 0])))
          (is (< 6 (first (feet p)))))))))

(deftest go-to-walks-to-a-wall-and-stairs-out-of-a-wide-hollow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 12 :y 65 :z 0}} :blocks hollow}
                                              {:pos [24 67 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))) "the failed walks took it to the east wall"))))))

(deftest go-to-escalates-and-arrives-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[what world pos] [[:pillar (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]}) [10 64 0]]
                                  [:stair (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 1}]}) [10 64 0]]
                                  [:clear-path (merge in-room {:blocks room}) [8 64 0]]]]
          (let [{:keys [out seen ticks]} (await (run-job! world {:pos pos :range 1}))]
            (is (= {:arrived true} @out) what)
            (is (= [what] (mapv :step (events-of seen :go-to.escalated))) what)
            (is (= 1 ticks) (str what ": one go-to call, the escalation and put-back inside it"))))))))

(deftest go-to-with-escalate-false-gives-up-as-before
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]})
                                                {:pos [10 64 0] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= false (:arrived @out)))
          (is (= :unreachable (:reason @out)))
          (is (empty? (events-of seen :go-to.escalated)))
          (is (empty? (calls p "jumpPlace")))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-with-no-pickaxe-stairs-out-of-a-stone-pit-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks stone-pit}) {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out) "stone walls, no pickaxe, no blocks: the stair is dug by hand")
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))))
          (is (= [:stair] (mapv :step (events-of seen :go-to.by-hand))) "said once")
          (is (seq (calls p "dig"))))))))

(deftest go-to-stairs-out-of-a-pit-under-gravel-beside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; gravel over the stair's next column falls into the cut, not onto the body: the stair digs on
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "1,64,0" "gravel")}) {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))))
          (is (contains? (dug-cells p) [1 63 0])))))))

(deftest go-to-with-no-pickaxe-clears-a-door-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-room {:blocks stone-room}) {:pos [8 64 0] :range 1}))]
          (is (= {:arrived true} @out) "the dig child would wait for a pickaxe: the door is dug by hand")
          (is (= [:clear-path] (mapv :step (events-of seen :go-to.escalated))))
          (is (= 1 (count (events-of seen :go-to.by-hand))))
          (is (seq (calls p "dig"))))))))

(deftest go-to-on-open-ground-never-escalates
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [tower (merge (box -20 63 -20 20 63 20 "stone") (box 8 64 0 8 68 0 "stone"))
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0 :y 64 :z 0}} :blocks tower
                                                 :inventory [{:name "dirt" :count 9}]}
                                                {:pos [8 69 0] :range 0}))]
          (is (= false (:arrived @out)) "the top of a 5-high stone column cannot be walked to")
          (is (empty? (events-of seen :go-to.escalated)) "not shut in: go-to does not change the world")
          (is (empty? (calls p "jumpPlace"))))))))

(deftest a-shut-in-body-that-may-not-escalate-says-it-is-shut-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! (merge in-pit {:blocks pit}) {:pos [10 64 0] :range 1 :ways #{:walk :clear-seal}}))]
          (is (= false (:arrived @out)))
          (is (true? (:shut-in @out)) "the result says the body is shut in")
          (is (re-find #"shut in" (:text @out)) "and so does the text, not 'no walkable way'")
          (is (empty? (events-of seen :go-to.escalated))))))))

(deftest go-to-escalates-only-by-the-ways-its-call-allows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]})
                                                  {:pos [10 64 0] :range 1 :ways #{:walk :stair}}))]
          (is (true? (:arrived @out)))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))) "no pillar, though it carries the blocks"))
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 1}]})
                                                    {:pos [10 64 0] :range 1 :ways #{:walk :pillar}}))]
          (is (= false (:arrived @out)))
          (is (re-find #"shut in" (:text @out)) "too few blocks to pillar and no stair allowed")
          (is (empty? (calls p "dig")))
          (is (empty? (events-of seen :go-to.escalated))))))))

(deftest a-walk-on-open-ground-is-not-called-shut-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [tower (merge (box -20 63 -20 20 63 20 "stone") (box 8 64 0 8 68 0 "stone"))
              {:keys [out]} (await (run-job! {:self {:pos {:x 0 :y 64 :z 0}} :blocks tower}
                                             {:pos [8 69 0] :range 0 :ways #{:walk :clear-seal}}))]
          (is (false? (:arrived @out)))
          (is (nil? (:shut-in @out))))))))

(deftest a-dig-s-walk-does-not-escalate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "10,64,0" "dirt")
                                                               :inventory [{:name "dirt" :count 5}]})
                                                'jobs.blocks.dig {:pos [10 64 0]}))]
          (is (= {:status :stopped :reason :unreachable} (select-keys @out [:status :reason])) "the dig cannot reach the cell")
          (is (empty? (events-of seen :go-to.escalated)) "its go-to child runs with :ways #{:walk :clear-seal}")
          (is (empty? (calls p "jumpPlace"))))))))

(deftest go-to-sealed-in-its-own-shelter-by-night-stays-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-in-shelter! 14000))]
          (is (not= {:arrived true} @out))
          (is (empty? (events-of seen :go-to.escalated)) "no refuge exit at night")
          (is (empty? (calls p "dig"))))))))

(deftest go-to-sealed-in-its-own-shelter-by-day-leaves-it-and-walks-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-in-shelter! 1000))]
          (is (= [:leave-shelter] (mapv :step (events-of seen :go-to.escalated))))
          (is (seq (calls p "dig")) "it dug out of the shelter")
          (is (= {:arrived true} @out)))))))

;; live (Juniper, card ba754e77): 16 under its goal, the stairs climbed 8, then 4 (the depth of the first stair's own walled
;; cut), then a partial walk led back down and the third way out was spent 9 under the goal. Each stair climbs as far as
;; the goal is above (at most max-depth), and an escalation that got the body nearer than any before does not count
;; toward the 3.
(deftest go-to-climbs-a-deep-shaft-stair-by-stair-while-each-gets-nearer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out p seen]} (await (run-job! {:self {:pos {:x 0 :y 34 :z 0}} :blocks deep-shaft}
                                                    'jobs.movement.go-to (merge {:pos [30 64 0] :range 1} trespass) foreign))
              escalated (events-of seen :go-to.escalated)]
          (is (= {:arrived true} @out))
          (is (= [:stair :stair :stair :stair] (map :step escalated)) "30 up is 4 stairs of at most 8, one after another")
          (is (= [1 1 1 1] (map :n escalated)) "each began nearer than any before")
          (is (restores-after-the-last-stair? seen))
          (is (= (dug-cells p) (holes eng)) "every dug cell is in the ledger"))))))

;; the walk from the last stair's top to a goal back over the cut passes the corner of the cut's open column
(deftest go-to-climbs-a-deep-shaft-to-a-goal-back-over-its-stairs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out p seen]} (await (run-job! {:self {:pos {:x 0 :y 34 :z 0}} :blocks deep-shaft}
                                                    'jobs.movement.go-to (merge {:pos [10 64 0] :range 1} trespass) foreign))]
          (is (= {:arrived true} @out))
          (is (= [:stair :stair :stair :stair] (map :step (events-of seen :go-to.escalated))))
          (is (seq (events-of seen :go-to.restore-skipped)) "the put-back ran once the walk got on")
          (is (restores-after-the-last-stair? seen))
          (is (= (dug-cells p) (holes eng)) "every dug cell is in the ledger"))))))

(deftest go-to-pillars-up-a-deep-shaft-while-each-pillar-gets-nearer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0 :y 34 :z 0}} :blocks deep-shaft
                                                   :inventory [{:name "dirt" :count 64}]}
                                              {:pos [10 64 0] :range 1}))
              escalated (events-of seen :go-to.escalated)]
          (is (= {:arrived true} @out))
          (is (= [:pillar :pillar :pillar :pillar] (map :step escalated)) "30 up is 4 pillars of at most 8")
          (is (= [1 1 1 1] (map :n escalated))))))))

(deftest escalations-count-only-while-none-gets-nearer
  (are [m d n] (= n (:fruitless (esc/count-escalation m d)))
    {} 20 1
    {:fruitless 1 :escalation-best 20} 19.5 2
    {:fruitless 2 :escalation-best 20} 12 1
    {:fruitless 3 :escalation-best 12} 12 4)
  (is (= 12 (:escalation-best (esc/count-escalation {:fruitless 2 :escalation-best 20} 12))))
  (is (= 20 (:escalation-best (esc/count-escalation {:fruitless 1 :escalation-best 20} 30))) "the best so far stays"))

(deftest escape-choose-stair-climbs-as-far-as-the-goal-is-above
  (is (= {:step :stair :heading :east :steps 8}
         (escape/choose (tu/fake (merge in-pit {:blocks pit})) [0 61 0] [10 75 0]))
      "a 3-deep pit under a goal 14 up: the stair climbs max-depth, not the pit's 3")
  (is (= {:step :stair :heading :east :steps 3}
         (escape/choose (tu/fake (merge in-pit {:blocks pit})) [0 61 0] [10 62 0]))
      "a goal no higher than the rim: the pit's depth"))

;; ------------------------------------------------------------------ review fixes (card 30615854, be7e5bb6)
(deftest rules-natural-is-a-whitelist-of-terrain
  (doseq [n ["stone" "dirt" "grass_block" "deepslate" "gravel" "sand" "netherrack" "iron_ore" "deepslate_diamond_ore"
             "red_terracotta" "andesite" "tuff" "oak_leaves" "mangrove_roots" "ice" "packed_ice" "blue_ice" "powder_snow"
             "magma_block" "pointed_dripstone" "amethyst_block" "suspicious_sand" "crimson_nylium" "warped_wart_block"
             "ancient_debris" "sculk" "infested_stone" "moss_block" "moss_carpet" "pale_moss_block" "pale_moss_carpet" "azalea"
             "flowering_azalea" "cave_vines" "cave_vines_plant" "big_dripleaf" "big_dripleaf_stem" "small_dripleaf"
             "spore_blossom" "sculk_vein" "cobweb" "poppy" "brown_mushroom" "brown_mushroom_block" "seagrass" "kelp_plant"
             "sweet_berry_bush" "sugar_cane" "bamboo" "cactus" "pumpkin" "melon" "dripstone_block" "mud" "clay"]]
    (is (rules/natural? n) n))
  (doseq [n ["oak_planks" "bricks" "stone_bricks" "glass" "white_wool" "cobblestone" "oak_door" "iron_door" "oak_log" "glowstone" "cobbled_deepslate" "dirt_path"
             "oak_fence" "spruce_fence_gate" "white_glazed_terracotta" "smooth_stone" "polished_andesite" nil]]
    (is (not (rules/natural? n)) (pr-str n))))
