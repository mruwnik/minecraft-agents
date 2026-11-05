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
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.way :as escape-way]
            [jobs.lib.look :as look]
            [jobs.movement.go-to.escalate-choose :as ec]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.tools :as tools]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tidy :as tidy]
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
  (is (= {:cells [[3 65 0] [3 64 0]] :through [4 64 0]} (escape/door (ba {:blocks room}) (constantly false) [2 64 0] [1 0] 3)))
  (is (nil? (escape/door (ba {:blocks room}) (constantly false) [1 64 0] [1 0] 3)) "no wall right in front")
  (is (nil? (escape/door (ba {:blocks pit}) (constantly false) [0 61 0] [1 0] 3)) "no floor beyond the pit's wall"))

(deftest escape-door-never-goes-through-a-door-a-bed-or-a-container
  (doseq [b ["iron_door" "oak_door" "spruce_fence_gate" "red_bed" "chest" "barrel" "bedrock"]]
    (is (nil? (escape/door (ba {:blocks (assoc room "3,64,0" b)}) (constantly false) [2 64 0] [1 0] 3)) b)))

(deftest escape-door-through-a-seen-wall-is-the-whole-wall-however-thick
  ;; how thick a wall is worth digging is the digger's price, not a count of rows
  (let [thick (merge room (box 3 64 -3 8 65 3 "dirt"))]
    (is (= [9 64 0] (:through (escape/door (ba {:blocks thick}) (constantly false) [2 64 0] [1 0] 3))))
    (is (= 12 (count (:cells (escape/door (ba {:blocks thick}) (constantly false) [2 64 0] [1 0] 3)))))))

(deftest escape-choose-skips-only-the-door-that-failed
  (let [p (tu/fake (merge in-room {:blocks room}))
        east {:step :clear-path :heading :east}]
    (is (= east (escape-way/choose p [2 64 0] [8 64 0])))
    (is (= east (escape-way/choose p [2 64 0] [8 64 0] (constantly true) {:skip #{(escape/door-key [2 64 1] :east)}}))
        "a door from another cell rules out only that one")
    (is (not= :clear-path (:step (escape-way/choose p [2 64 0] [8 64 0] (constantly true) {:skip #{(escape/door-key [2 64 0] :east)}})))
        "the door that failed is not dug again")
    (let [in-line? #{[3 64 0] [3 65 0]}
          may-dig? (complement in-line?)
          spot (escape-way/choose p [2 64 0] [8 64 0] may-dig? {})
          other (escape-way/choose p [2 64 0] [8 64 0] may-dig? {:skip #{(escape/door-key (:pos spot) :east)}})]
      (is (= :approach (:step spot) (:step other)) "the wall in line may not be dug: a walk to a door beside it")
      (is (not= (:pos spot) (:pos other)) "no walk to a cell whose door failed"))
    (is (= [:clear-path [2 64 0] :east] (esc/skip-key east [2 64 0])) "go-to keys a failed door by where it began")))

(deftest escape-choose-walks-to-a-wall-first-from-the-middle-of-a-room
  (is (= {:step :approach :pos [2 64 0]} (escape-way/choose (tu/fake (merge in-room {:blocks room})) [0 64 0] [8 64 0]))
      "the nearest cell with the wall toward the goal beside it"))

(deftest escape-choose-stairs-on-from-a-ledge-when-it-carries-a-block-for-the-floor
  (is (= {:step :stair :heading :east :steps 4}
         (escape-way/choose (tu/fake (assoc on-ledge :inventory [{:name "cobblestone" :count 8}])) [0 66 0] [8 70 0])))
  (is (= {:step :none :why :no-wall} (escape-way/choose (tu/fake on-ledge) [0 66 0] [8 70 0]))))

(deftest escape-choose-ledge-stair-skips-a-heading-whose-first-floor-is-a-stand-of-the-bodys-own-stair
  (let [p (tu/fake (assoc on-ledge :inventory [{:name "cobblestone" :count 8}]))
        step (escape-way/choose p [0 66 0] [8 70 0] (constantly true) {:stands #{[1 66 0]}})]
    (is (= :stair (:step step)))
    (is (not= :east (:heading step)) "the stair job refuses :undercuts-way on that cell")))

(deftest escape-choose-pillars-only-with-enough-blocks
  (is (= {:step :pillar :height 2 :item "dirt"}
         (escape-way/choose (tu/fake (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})) [0 61 0] [10 64 0]))
      "a 3-deep pit: two blocks up, then a step onto the rim")
  (is (= {:step :stair :heading :east :steps 3}
         (escape-way/choose (tu/fake (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 1}]})) [0 61 0] [10 64 0]))))

(deftest escape-choose-pillars-beside-the-wall-of-a-wide-pit-it-may-not-dig
  (let [at-wall (fn [n] (tu/fake {:self {:pos {:x 2 :y 61 :z 0}} :blocks wide-pit :inventory [{:name "dirt" :count n}]}))]
    (is (= 0 (escape/pit-depth (ba {:blocks wide-pit}) [2 61 0])) "no level of a wide pit is walled on three sides")
    (is (= {:step :pillar :height 2 :item "dirt"} (escape-way/choose (at-wall 2) [2 61 0] [10 64 0] (constantly false)))
        "two blocks up, then a step onto the rim")
    (is (= {:step :none :why :no-dig} (escape-way/choose (at-wall 1) [2 61 0] [10 64 0] (constantly false))) "too few blocks")
    (is (= {:step :none :why :no-dig}
           (escape-way/choose (tu/fake {:self {:pos {:x 2 :y 61 :z 0}} :blocks (merge wide-pit (box -3 64 -3 3 64 3 "oak_planks"))
                                    :inventory [{:name "dirt" :count 9}]})
                          [2 61 0] [10 64 0] (constantly false)))
        "a roofed pit has no rim to step onto")))

(deftest escape-choose-rim-pillars-though-the-cells-over-the-rim-are-not-seen-from-the-pit
  ;; sweep 4410 wide-pit-no-dig-pillars-out: at the wall the rim hides the cells over it
  (let [p (tu/fake {:self {:pos {:x 2 :y 61 :z 0}} :blocks wide-pit :inventory [{:name "dirt" :count 8}]})
        over-rim? (fn [[x y _]] (and (= 3 x) (>= y 64)))]
    (aset p "sensedAt" (fn [pos] (if (over-rim? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    (is (= {:step :pillar :height 2 :item "dirt"} (escape-way/choose p [2 61 0] [10 64 0] (constantly false))))))

(deftest escape-choose-rim-pillars-on-the-side-toward-the-goal
  (let [low-south (apply dissoc wide-pit (for [x (range -3 4)] (str x ",63,3")))
        p (tu/fake {:self {:pos {:x 2 :y 61 :z 2}} :blocks low-south :inventory [{:name "dirt" :count 9}]})]
    (is (= {:step :pillar :height 2 :item "dirt"} (escape-way/choose p [2 61 2] [10 64 2] (constantly false)))
        "the east wall, toward the goal, though the south one is a block lower")
    (is (= {:step :pillar :height 1 :item "dirt"} (escape-way/choose p [2 61 2] [2 64 10] (constantly false)))
        "the south wall when the goal is south")))

(def column-yard
  "A stone floor with a 6-high cobblestone column east of the body at [0 64 0]."
  (merge (box -6 63 -6 6 63 6 "stone") (box 1 64 0 1 69 0 "cobblestone")))

(deftest escape-choose-pillars-beside-a-column-to-a-goal-on-top-as-it-is-cheaper-than-a-stair
  ;; a rise-6 stair ends 6 blocks aside, a dig through the column at its foot: both leave the climb to do
  (let [p (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :blocks column-yard
                    :inventory [{:name "cobblestone" :count 16} {:name "stone_pickaxe" :count 1}]})
        priced {:rest-s (escape-way/rest-s-fn p [1 70 0]) :item "cobblestone" :count 16}
        s #(escape-way/way-s p [0 64 0] [1 70 0] % priced)]
    (is (= {:step :pillar :height 5 :item "cobblestone"} (escape-way/choose p [0 64 0] [1 70 0])))
    (is (< (* 2 (s {:step :pillar :height 5})) (s {:step :stair :heading :east :steps 6})) "no knife edge: the stair costs twice the pillar and more")
    (is (= {:step :clear-path :heading :east}
           (escape-way/choose p [0 64 0] [1 70 0] (constantly true) {:risk-s (fn [cells _ _] (if (some #{[0 66 0]} cells) 1000 0))}))
        "a danger on the pillar's column turns it: through the column's foot and up its far side")))

(deftest escape-choose-prices-a-lone-rim-pillar-against-the-door-through-its-wall
  ;; no stair: the door through the 3-high wall leaves the climb onto it to do, the rim pillar steps onto the goal
  (let [p (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :blocks (merge (box -6 63 -6 6 63 6 "stone") (box 1 64 -3 1 66 3 "cobblestone"))
                    :inventory [{:name "cobblestone" :count 16}]})]
    (is (= :clear-path (:step (escape-way/choose p [0 64 0] [1 67 0] (constantly true) {:skip #{:stair :pillar}}))) "the door is a way here")
    (is (= {:step :pillar :height 2 :item "cobblestone"} (escape-way/choose p [0 64 0] [1 67 0] (constantly true) {:skip #{:stair}})))))

(deftest escape-choose-leaves-the-door-whose-climb-left-is-rock-it-may-not-cut-for-the-rim-pillar
  ;; a danger on the pillar's column turns it to the door; rock over the door's far side then makes that climb none
  (let [p (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :blocks (merge (box -6 63 -6 6 63 6 "stone") (box 1 64 -3 1 66 3 "cobblestone")
                                                               (box 2 66 0 2 68 0 "stone"))
                    :inventory [{:name "cobblestone" :count 16}]})
        risk-s (fn [cells _ _] (if (some #{[0 65 0]} cells) 1000 0))
        far-side? (fn [[x y z]] (and (= x 2) (= z 0) (> y 65)))]
    (is (= :clear-path (:step (escape-way/choose p [0 64 0] [1 67 0] (constantly true) {:skip #{:stair} :risk-s risk-s}))))
    (is (= :pillar (:step (escape-way/choose p [0 64 0] [1 67 0] (complement far-side?) {:skip #{:stair} :risk-s risk-s}))))))

(deftest escape-the-climb-left-after-a-way-is-a-pillar-of-the-blocks-left-under-open-sky-else-new-stair-steps
  (let [p (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :blocks (merge (box -6 63 -6 6 63 6 "stone") {"0,67,0" "stone"})
                    :inventory [{:name "cobblestone" :count 16}]})
        share (wworld/unseen-share p)]
    (is (= (+ (cost/pillar-s share 3 0) (escape/pillar-knock p [1 64 0] 3)) (escape-way/rise-left-s p [1 64 0] [1 68 0] 3)) "3 blocks left for a rise of 3")
    (is (= (+ (cost/pillar-s share 3 0 2) (escape/pillar-knock p [1 64 0] 3)) (escape-way/rise-left-s p [1 64 0] [1 68 0] 1)) "2 of them fetched")
    (is (< (cost/pillar-s share 3 0) (cost/pillar-s share 3 0 2)))
    (is (= (* 3 (escape-way/new-stair-step-s p)) (escape-way/rise-left-s p [0 64 0] [0 68 0] 3)) "rock over the head: a pillar cannot climb")
    (is (= ##Inf (escape-way/rise-left-s p [0 64 0] [0 68 0] 3 (constantly false))) "rock over the head that may not be cut: no climb")
    (is (zero? (escape-way/rise-left-s p [0 64 0] [0 65 0] 0)) "a step up is a walk")))

(deftest escape-choose-cuts-the-planks-door-nobody-claims-else-rim-pillars
  (let [p (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :blocks plank-yard :inventory [{:name "dirt" :count 9}]})]
    (is (= :clear-path (:step (escape-way/choose (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :blocks plank-yard}) [2 64 0] [8 64 0])))
        "the planks are nobody's claim")
    (is (= {:step :pillar :height 1 :item "dirt"} (escape-way/choose p [2 64 0] [8 64 0])) "a block up and over the wall costs less than the two planks' cut and put-back")
    (is (= {:step :pillar :height 1 :item "dirt"} (escape-way/choose p [2 64 0] [8 64 0] (constantly false)))
        "no dig allowed: over the wall")))

(deftest goal-way-pillars-over-a-wall-to-a-level-goal-when-the-top-walks-on-to-it
  (let [yard (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :blocks plank-yard :inventory [{:name "dirt" :count 9}]})
        moat (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :blocks (merge plank-yard (box 5 63 -6 5 63 6 "air")) :inventory [{:name "dirt" :count 9}]})
        way #(:step (ec/goal-way {:primitives %} [2 64 0] [8 64 0] (constantly false) {:skip #{:approach}}))]
    (is (= :pillar (way yard)) "no dig allowed: over the wall and down to the goal")
    (is (= :none (way moat)) "a drop with no floor beyond the wall is no way in")
    (is (= :pillar (:step (ec/goal-way {:primitives yard} [2 64 0] [8 64 0] (constantly true) {:skip #{:approach}})))
        "the hop that walks on to the goal is the cheaper way, priced against the door")
    (is (= :clear-path (:step (ec/goal-way {:primitives moat} [2 64 0] [8 64 0] (constantly true) {:skip #{:approach}})))
        "a hop that lands nowhere is not weighed against the door")))

(deftest goal-way-walks-to-the-wall-for-a-level-goal-after-the-door-failed
  ;; a step from the wall: the nearest stand is no nearer by more than a block, so the way is asked from the feet
  (let [p (tu/fake {:self {:pos {:x 1 :y 64 :z 0}} :blocks room})]
    (is (= {:step :approach :pos [2 64 0]} (ec/goal-way {:primitives p} [1 64 0] [8 64 0] (constantly true) {:skip #{:clear-path}}))
        "the failed door and the dropped pillar and stair leave the walk to the wall, not a dig route")))

(deftest goal-way-never-pillars-out-of-a-pit-to-a-level-goal-the-rim-does-not-walk-on-to
  ;; the pit's rim pillar steps onto ground the goal, sealed in its stone, cannot be walked to from
  (let [p (tu/fake (merge in-pit {:blocks (dissoc pit "10,61,0" "10,62,0") :inventory [{:name "dirt" :count 5}]}))]
    (is (= {:step :none :why :no-dig} (ec/goal-way {:primitives p} [0 61 0] [10 61 0] (constantly false) {:skip #{}})))))

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

(deftest go-to-s-pillar-on-open-land-is-taken-back-once-its-time-is-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out p clock]} (await (settings/with-settings {:jobs.lib.ledger/ttl-s 1}
                                           #(run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5} {:name "iron_sword" :count 1}]})
                                                      'jobs.movement.go-to {:pos [10 64 0] :range 1} [])))
              column [[0 61 0] [0 62 0]]]
          (is (= {:arrived true} @out))
          (is (every? #(= "dirt" (block p %)) column) "the pillar stands")
          (swap! clock + 1000)
          (is (true? (scaffold-left/scaffold-left p (mem/view (:store eng)) {} (ew/of-data {} {} [])))
              "the scaffold-left trigger offers it once its time is out")
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

(deftest go-to-steps-off-a-dug-cell-it-stands-in-once-and-then-puts-it-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stood (atom #{})
              stepped (atom [])
              in-body? (fn [_ cell] (when-not (@stood cell) (swap! stood conj cell) true))
              {:keys [out p seen]} (with-redefs [tidy/in-body? in-body?
                                                 step-off/step-off! (fn [_ cell _] (swap! stepped conj cell) (js/Promise.resolve :arrived))]
                                     (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})
                                                      'jobs.movement.go-to (merge {:pos [10 64 0] :range 1 :ways #{:walk :stair}} trespass) foreign)))]
          (is (= {:arrived true} @out))
          (is (= [[1 62 0] [1 63 0]] (sort (:cells (first (events-of seen :go-to.restored))))) "the cells the body stood in are put back")
          (is (= (sort @stood) (sort @stepped)) "each is stepped off once")
          (is (= "dirt" (block p [1 62 0]))))))))

(deftest go-to-keeps-stepping-off-a-dug-cell-after-the-step-off-waited-and-avoids-the-other-holes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [moved (atom #{})
              waited (atom #{})
              avoided (atom [])
              in-body? (fn [_ cell] (not (@moved cell)))
              step-off! (fn [_ cell opts]
                          (swap! avoided conj (:avoid opts))
                          (js/Promise.resolve
                           (if (@waited cell)
                             (do (swap! moved conj cell) :arrived)
                             (do (swap! waited conj cell) :continue))))
              {:keys [out p seen]} (with-redefs [tidy/in-body? in-body?
                                                 step-off/step-off! step-off!]
                                     (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})
                                                      'jobs.movement.go-to (merge {:pos [10 64 0] :range 1 :ways #{:walk :stair}} trespass) foreign)))]
          (is (= {:arrived true} @out))
          (is (= [[1 62 0] [1 63 0]] (sort (:cells (first (events-of seen :go-to.restored))))) "a waiting step-off does not use the cell up")
          (is (every? (first @avoided) [[1 62 0] [1 63 0]]) "the dug cells are not stepped onto")
          (is (= "dirt" (block p [1 62 0]))))))))

(deftest go-to-stairs-out-of-a-sealed-stone-pit-with-a-full-pack-leaving-the-drops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pack (into [{:name "stone_pickaxe" :count 1}] (repeat 35 {:name "rotten_flesh" :count 64}))
              {:keys [out p]} (await (run-job! (merge in-pit {:blocks stone-pit :inventory pack}) {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out) "a full pack does not stop the dig: the stone is left lying")
          (is (empty? (calls p "toss"))))))))

(def lake-pocket
  "A dry 3-deep pocket (feet y 60) in stone x -2..70, y 56..62; the only walk out is a 60-block water tunnel (y 60..61) east to a surface cell, longer than a breath."
  (merge (apply dissoc (box -2 56 -3 70 62 3 "stone") ["0,60,0" "0,61,0" "0,62,0"]) (box 1 60 0 60 61 0 "water") {"60,62,0" "water"}))

(deftest go-to-stairs-out-of-a-dry-pocket-whose-only-walk-is-a-swim-past-its-air
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0 :y 60 :z 0}} :blocks lake-pocket}
                                                  'jobs.movement.go-to {:pos [66 63 0] :range 1 :ways #{:walk :stair}}))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated)))))))))

(deftest go-to-giving-up-in-a-dry-pockets-tunnel-lands-and-stairs-out
  ;; a partial plan led the body into the tunnel before it learned the swim fits no breath (live: or leads nowhere
  ;; loaded): a give-up under water cannot read the pocket as shut in, so it lands first and is judged there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 2.5 :y 60 :z 0.5}} :blocks lake-pocket}
                                                  'jobs.movement.go-to {:pos [66 63 0] :range 1 :ways #{:walk :stair}}))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated)))))))))

(deftest dry-pocket-escalates-when-the-goal-lies-in-unloaded-land
  ;; the search ends :goal-unloaded, not :air, when the swim and the goal are not loaded: the pocket is still shut in but for the swim
  (let [c {:primitives (tu/fake {:self {:pos {:x 0 :y 60 :z 0}} :blocks lake-pocket})}]
    (is (ec/air-pocket? c {:status :no-path :reason :goal-unloaded}))
    (is (not (ec/air-pocket? c {:status :no-path :reason :goal-enclosed})))))

(deftest go-to-does-not-walk-back-down-its-stair-to-put-it-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; no blocks: the climb is one stair from the bottom (with blocks it pillars up and cuts nothing deep)
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x 0 :y 59 :z 0}} :blocks deep-pit
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
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
          (is (empty? (events-of seen :go-to.escalation-stopped))))))))

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

;; The sealed room, then a 2-cell pocket (x 4..5) shut by a second dirt wall at x 6: two doors in a row to the goal.
(def two-walls (merge room (apply dissoc (box 4 64 -1 6 66 1 "dirt") ["4,64,0" "4,65,0" "5,64,0" "5,65,0"])))

(deftest each-door-of-a-go-to-may-spend-only-what-the-doors-before-it-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup (merge in-room {:blocks two-walls}))
              budgets (atom [])
              door (get-in registry/jobs ['jobs.access.clear-path :round])
              out (atom :not-done)
              eng (assoc eng :jobs (-> (:jobs eng)
                                       (assoc-in ['jobs.access.clear-path :round]
                                                 (fn ^:async spied-door [c]
                                                   (when-not (:through (ctx/mem c)) (swap! budgets conj (:max-dig-s (:args c))))
                                                   (await (door c))))
                                       (assoc 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [9 64 0] :range 1 :worth 600}))))
              door-s (cost/dig-cells-s (repeat 2 (tools/clear-ms p "dirt")))]
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (= {:arrived true} @out))
          (is (= 2 (count @budgets)) (pr-str @budgets))
          (is (= 600 (first @budgets)))
          (is (< (js/Math.abs (- (second @budgets) (- 600 door-s))) 1e-6) "the second door gets what the first left"))))))

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
        ;; one dirt: a block's pillar and a two-step stair cost less than a three-step stair (cost.ways)
        (doseq [[what world pos] [[[:pillar] (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]}) [10 64 0]]
                                  [[:pillar :stair] (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 1}]}) [10 64 0]]
                                  [[:clear-path] (merge in-room {:blocks room}) [8 64 0]]]]
          (let [{:keys [out seen ticks]} (await (run-job! world {:pos pos :range 1}))]
            (is (= {:arrived true} @out) what)
            (is (= what (mapv :step (events-of seen :go-to.escalated))) what)
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

(deftest go-to-with-no-pickaxe-digs-the-stair-by-hand-though-the-other-walls-are-another-stone
  ;; andesite wants the pickaxe too: it is no heading that needs no tool, so the first stair is run again by hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walls (merge stone-pit (box -2 60 -2 -1 63 2 "andesite") (box -2 60 -2 2 63 -1 "andesite"))
              {:keys [out p seen]} (await (run-job! (merge in-pit {:blocks walls}) {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.by-hand))))
          (is (contains? (dug-cells p) [1 63 0]) "the heading toward the goal, not a detour round it"))))))

(deftest go-to-stairs-out-of-a-pit-under-gravel-beside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the climb's route digs no cell under gravel: a step south, then east
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "1,64,0" "gravel")}) 'jobs.movement.go-to {:pos [10 64 0] :range 1} nil true))]
          (is (= {:arrived true} @out))
          (is (= [:stair :stair] (mapv :step (events-of seen :go-to.escalated))))
          (is (not (contains? (dug-cells p) [1 63 0])) "the cell under the gravel stays"))))))

(deftest go-to-stairs-through-gravel-over-the-next-column
  ;; a stair is the only way allowed: its cut through the gravel is dug again as it falls, never priced past an arrival
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shaft (-> (apply dissoc (box -6 60 -6 6 63 6 "stone") pit-cells) (assoc "1,64,0" "gravel"))
              {:keys [out seen]} (await (run-job! (merge in-pit {:blocks shaft :inventory [{:name "stone_pickaxe" :count 1}]})
                                                  'jobs.movement.go-to {:pos [4 64 0] :range 1 :ways #{:walk :stair}} nil true))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))))))))
  (let [shaft (-> (apply dissoc (box -6 60 -6 6 63 6 "stone") pit-cells) (assoc "1,64,0" "gravel"))
        price #(escape-way/price (tu/fake (merge in-pit {:blocks % :inventory [{:name "stone_pickaxe" :count 1}]})) [0 61 0] {:step :stair :heading :east :steps 3})]
    (is (< (price shaft) (price (merge shaft (box 1 65 0 1 68 0 "gravel")))) "each gravel stacked over the cut falls in and is dug")))

(deftest stair-price-digs-the-unseen-loose-ground-over-a-cut-at-the-chance-it-is-solid
  (let [shaft (-> (apply dissoc (box -6 60 -6 6 63 6 "stone") pit-cells) (assoc "1,64,0" "gravel"))
        p (tu/seeing-within (tu/fake (merge in-pit {:blocks shaft :inventory [{:name "stone_pickaxe" :count 1}]})) 3.5)
        price-at (fn [chance] (with-redefs [look/unseen-solid-at-fn (fn [_] (constantly chance))]
                                (escape-way/price p [0 61 0] {:step :stair :heading :east :steps 3})))]
    (is (< (price-at 0) (price-at 0.5)) "the cells over the seen gravel read as gravel, priced at the chance they are there")))

(deftest go-to-with-no-pickaxe-clears-a-door-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-room {:blocks stone-room}) {:pos [8 64 0] :range 1}))]
          (is (= {:arrived true} @out) "the dig child would wait for a pickaxe: the door is dug by hand")
          (is (= [:clear-path] (mapv :step (events-of seen :go-to.escalated))))
          (is (= 1 (count (events-of seen :go-to.by-hand))))
          (is (seq (calls p "dig"))))))))

(deftest go-to-with-the-goods-for-a-pickaxe-crafts-it-instead-of-digging-a-door-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-room {:blocks stone-room :inventory [{:name "cobblestone" :count 6} {:name "stick" :count 2} {:name "crafting_table" :count 1}]})
                                                    {:pos [8 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (empty? (events-of seen :go-to.by-hand)))
          (is (seq (calls p "craft"))))))))

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

(deftest go-to-sealed-in-its-own-shelter-by-night-does-not-dig-out-for-a-goal-worth-less-than-the-dark-outside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-in-shelter! 14000 [8 64 0] 1 {:args {:worth 0}}))]
          (is (empty? (events-of seen :go-to.escalated)) "the dark outside is waited out, the first dig never happens")
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (re-find #"costs more hp" (:text @out)) "the refusal names the night's price, not a closed land")
          (is (empty? (calls p "dig"))))))))

(deftest go-to-sealed-in-bedrock-by-night-keeps-the-plain-shut-in-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [bedrock (dissoc (box -1 64 -1 1 66 1 "bedrock") "0,64,0" "0,65,0")
              {:keys [out]} (await (run-in-shelter! 14000 [8 64 0] 1 {:args {:worth 0} :blocks bedrock}))]
          (is (= false (:arrived @out)))
          (is (not (re-find #"costs more hp" (:text @out))) "no way out to dig: a higher worth would not help, so the price is not named")
          (is (re-find #"no walkable way" (:text @out))))))))

(deftest go-to-sealed-in-its-own-shelter-by-night-digs-out-for-a-goal-that-must-be-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-in-shelter! 14000))]
          (is (seq (events-of seen :go-to.escalated)))
          (is (true? (:arrived @out))))))))

(def stay-danger-night 14000)

(def second-cell
  "A second shelter cell at z 1, walled again at z 2."
  (merge (box -1 64 2 1 66 2 "dirt") {"0,64,1" "air" "0,65,1" "air"}))

(deftest go-to-leaves-a-night-shelter-with-fire-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-in-shelter! stay-danger-night [8 64 0] 1
                                                      {:blocks (assoc second-cell "0,64,1" "fire")}))]
          (is (seq (events-of seen :go-to.escalated))))))))

(deftest go-to-leaves-a-night-shelter-with-lava-beside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-in-shelter! stay-danger-night [8 64 0] 1
                                                      {:blocks (assoc second-cell "0,64,1" "lava")}))]
          (is (seq (events-of seen :go-to.escalated))))))))

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
        (let [{:keys [eng out p seen]} (await (run-job! {:self {:pos {:x 0 :y 46 :z 0}} :blocks deep-shaft}
                                                    'jobs.movement.go-to (merge {:pos [30 64 0] :range 1 :worth 30} trespass) foreign))
              escalated (events-of seen :go-to.escalated)]
          (is (= {:arrived true} @out))
          ;; a worth of 30 s makes the 18 digs of a climb route too dear: stairs, each reaching nearer than any before
          (is (every? #{:stair} (map :step escalated)))
          (is (<= 2 (count escalated)) "one stair does not reach: the climb is stairs")
          (is (every? #{1} (map :n escalated)) "each began nearer than any before")
          (is (restores-after-the-last-stair? seen))
          (is (= (dug-cells p) (holes eng)) "every dug cell is in the ledger"))))))

;; the walk from the last stair's top to a goal back over the cut passes the corner of the cut's open column
(deftest go-to-climbs-a-deep-shaft-to-a-goal-back-over-its-stairs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out p seen]} (await (run-job! {:self {:pos {:x 0 :y 46 :z 0}} :blocks deep-shaft}
                                                    'jobs.movement.go-to (merge {:pos [10 64 0] :range 1} trespass) foreign))]
          (is (= {:arrived true} @out))
          (is (some #{:stair} (map :step (events-of seen :go-to.escalated))) "the climb is stairs")
          (is (every? #{:stair :dig-route :approach} (map :step (events-of seen :go-to.escalated))) "no pillar, no drop")
          (is (= 20 (.-health (.self p))) "no drop hurt")
          (is (seq (events-of seen :go-to.restore-skipped)) "the put-back ran once the walk got on")
          (is (restores-after-the-last-stair? seen))
          (is (= (dug-cells p) (into (holes eng) (mapcat :cells (events-of seen :go-to.restored)))) "every dug cell is in the ledger or was put back"))))))

(deftest go-to-pillars-up-a-deep-shaft-in-one-pillar
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0 :y 46 :z 0}} :blocks deep-shaft
                                       :inventory [{:name "dirt" :count 64}]}
                                      {:pos [10 64 0] :range 1}))
              escalated (events-of seen :go-to.escalated)]
          (is (= {:arrived true} @out))
          (is (= [:pillar] (map :step escalated)) "the climb is one pillar of 18")
          (is (= [1] (map :n escalated))))))))

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
         (escape-way/choose (tu/fake (merge in-pit {:blocks pit})) [0 61 0] [10 75 0]))
      "a 3-deep pit under a goal 14 up: the stair climbs max-depth, not the pit's 3")
  (is (= {:step :stair :heading :east :steps 3}
         (escape-way/choose (tu/fake (merge in-pit {:blocks pit})) [0 61 0] [10 62 0]))
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

(deftest escape-choose-walks-to-an-unseen-part-of-a-build-wall-once-then-finds-no-dig
  ;; unseen wall cells are guessed as rock, so each looks like a door: the body walks to look, and a cell it walked to
  ;; (:approached) is not chosen again; the chain ends in :no-dig. The seen planks are a claim's (may-dig? refuses them).
  (let [planks (into {} (map (fn [[k v]] [k (if (= v "dirt") "oak_planks" v)])) room)
        p (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :blocks planks})
        hide? (fn [[x _ z]] (and (= 3 x) (not= 0 z)))]
    (aset p "sensedAt" (fn [pos] (if (hide? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    (let [steps (loop [seen #{} out []]
                  (let [e (escape-way/choose p [2 64 0] [8 64 0] hide? {:approached seen})]
                    (if (or (not= :approach (:step e)) (> (count out) 8))
                      (conj out e)
                      (recur (conj seen (:pos e)) (conj out e)))))]
      (is (= {:step :none :why :no-dig} (last steps)))
      (is (apply distinct? (map :pos (butlast steps)))))))

(deftest a-dig-route-that-meets-buried-bedrock-is-planned-again-till-it-takes-the-glass-it-went-round
  ;; another's glass shaft is the one way into a pocket walled in bedrock whose top is seen and whose sides, buried in
  ;; the stone, are not (guessed rock). A pillar out of the route's own hole leads no nearer a walled-in goal, so it
  ;; sets no bound on the replans: they go on till one goes in.
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pocket #{[19 60 16] [19 61 16]}
              faces (fn [q] (map #(mapv + q %) [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))
              blocks (merge (box 2 58 2 30 63 30 "stone") (box 17 59 14 21 63 18 "bedrock")
                            {"19,60,16" "air" "19,61,16" "air" "19,62,16" "stone" "19,63,16" "glass"})
              {:keys [eng p seen]} (setup {:self {:pos {:x 16.5 :y 64 :z 16.5}} :blocks blocks
                                           :inventory [{:name "stone_pickaxe" :count 1} {:name "cobblestone" :count 4}]})
              ;; the body sees a cell open to the air it stands in, and the faces round one; the pocket's air only once opened
              open? (fn [q] (and (= "air" (block p q)) (or (not (pocket q)) (some #(and (not (pocket %)) (= "air" (block p %))) (faces q)))))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [19 60 16] :range 0 :unknown false})))]
          (tu/set-sensed! p (fn [q] (let [cell (mapv js/Math.floor [(.-x q) (.-y q) (.-z q)])]
                                      (if (some open? (cons cell (faces cell))) (.blockAt p q) #js {:unknown true :pos q}))))
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 3000))
          (is (true? (:arrived @out)) (pr-str (select-keys @out [:why :text :escalation])))
          (is (= [19 60 16] (feet p)))
          (is (apply distinct? (map :cell (events-of seen :go-to.guess-look))) "a replan searching over rounds counts its guess one look")
          (is (= "air" (block p [19 63 16])) "the glass is dug once the routes round it prove walled"))))))

(deftest way-s-prices-the-climb-left-past-a-doors-far-side-on-its-column-cells
  (let [p (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :blocks column-yard
                    :inventory [{:name "cobblestone" :count 16} {:name "stone_pickaxe" :count 1}]})
        seen (atom nil)
        priced {:rest-s (escape-way/rest-s-fn p [1 70 0]) :item "cobblestone" :count 16
                :risk-s (fn [cells _ _] (reset! seen cells) 0)}]
    (escape-way/way-s p [0 64 0] [1 70 0] {:step :clear-path :heading :east} priced)
    (is (every? (set @seen) [[2 65 0] [2 66 0] [2 67 0] [2 68 0]]) "every cell of the climb left from the door's far side is a priced cell")))

(def unseen-shaft
  "A 6-deep 1x1 shaft (feet y 58) in stone x -6..8, y 57..63; the goal on top 6 east."
  (apply dissoc (box -6 57 -6 8 63 6 "stone") (for [y (range 58 64)] (str "0," y ",0"))))

(deftest escape-choose-prices-the-pit-pillar-under-a-column-it-has-not-seen
  ;; a dark shaft shows the body only the cells beside it: the pillar is priced against the door, not left for when nothing else is
  (let [p (tu/seeing-within (tu/fake {:self {:pos {:x 0.5 :y 58 :z 0.5}} :blocks unseen-shaft :inventory [{:name "dirt" :count 8}]}) 1.5)]
    (is (= :pillar (:step (escape-way/choose p [0 58 0] [6 64 0]))))))

(deftest escape-choose-does-not-pillar-up-an-unseen-column-it-may-not-dig
  (let [p (tu/seeing-within (tu/fake {:self {:pos {:x 0.5 :y 58 :z 0.5}} :blocks unseen-shaft :inventory [{:name "dirt" :count 8}]}) 1.5)]
    (is (= :pillar (:step (escape-way/choose p [0 58 0] [6 64 0] (constantly true)))))
    (is (not= :pillar (:step (escape-way/choose p [0 58 0] [6 64 0] (fn [[x _ z]] (not= [0 0] [x z]))))) "a zoned roof cell unseen is the worse case: solid and not dug")))

(deftest escape-choose-prices-the-unseen-roof-over-a-pit-by-the-chance-it-is-solid
  (let [p (tu/seeing-within (tu/fake {:self {:pos {:x 0.5 :y 58 :z 0.5}} :blocks unseen-shaft :inventory [{:name "dirt" :count 8}]}) 1.5)
        at (fn [chance f] (with-redefs [look/unseen-solid-at-fn (fn [_] (constantly chance))] (f)))
        pillar-s #(escape-way/price p [0 58 0] {:step :pillar :height 6})]
    (is (< (at 0 pillar-s) (at 0.5 pillar-s)))
    (is (not= :pillar (at 1 #(:step (escape-way/choose p [0 58 0] [6 64 0])))) "a roof sure to be there is dug by hand: a way through the side is cheaper")))

(deftest go-to-escalates-out-of-a-shaft-whose-surrounding-rock-it-cannot-see
  ;; the planner proves the start sealed (:start-enclosed) though the room flood reads the unseen walls as open: it escalates
  ;; (a pillar; the roof it digs is found air and remembered as air, as perception's touching does) and arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0.5 :y 58 :z 0.5}} :blocks unseen-shaft :sight 1.5
                                                   :inventory [{:name "dirt" :count 8}]}
                                                  {:pos [6 64 0] :range 1}))]
          (is (= [:pillar] (take 1 (mapv :step (events-of seen :go-to.escalated)))))
          (is (true? (:arrived @out)) (pr-str @out)))))))
