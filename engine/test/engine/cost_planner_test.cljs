(ns engine.cost-planner-test
  "jobs.lib.cost.planner: go-to's :costs overrides of the planner's per-move prices, and how they reach the planner."
  (:require [cljs.test :refer [deftest is are]]
            [engine.planner-fixture :as pf :refer [near world run]]
            [jobs.lib.cost :as cost]
            [jobs.lib.walk.plan :as wplan]))

(deftest costs-are-checked-by-name-and-value
  (are [costs ok?] (= ok? (nil? (cost/planner-costs-problem costs)))
    nil true
    {} true
    {:swim-h 1.5 :climb-up 0} true
    {:swim-h -1} false
    {:swim-h 0.1} false
    {:exit 0.2} false
    {:swim-h 0.24 :exit 0.24} true
    {:open-redstone 0 :open-lever 0 :open-plate 0 :beside-magma-column 0 :max-water-drop 8 :dripleaf 0 :dripleaf-risk 0} true
    {:air-supply 15} false
    {:swim-h "slow"} false
    {:swim-h js/Infinity} false
    {:flying 1} false
    [[:swim-h 1]] false))

(deftest costs-reach-the-planner-under-its-own-names
  (is (= {"swimH" 2 "climbUp" 0.1} (js->clj (cost/planner-costs {:swim-h 2 :climb-up 0.1})))))

(deftest with-drops-merges-the-policys-costs-with-the-drop-factor
  (let [o (wplan/with-drops #js {} {:costs {:swim-h 2} :drop-cost 3})]
    (is (= [2 3] [(.-swimH (.-costs o)) (.-dropFactor (.-costs o))])))
  (let [o (wplan/with-drops #js {} {:costs {:swim-h 2}})]
    (is (= 2 (.-swimH (.-costs o)))))
  (is (nil? (.-costs (wplan/with-drops #js {} {}))) "no costs: the planner's defaults"))

(deftest every-name-is-a-planner-cost
  (is (every? #(contains? pf/default-costs (keyword %)) (vals cost/planner-names))))

;; a wall 5 high across the whole world, a ladder in it at z 5 and a stair up it at z 20
(def ladder-wall
  (world {:fill [[10 64 -2 10 68 40 "stone"]
                 [9 64 5 9 68 5 "ladder" {:facing "west"}]
                 [5 64 20 5 64 20 "stone"] [6 64 20 6 65 20 "stone"] [7 64 20 7 66 20 "stone"]
                 [8 64 20 8 67 20 "stone"] [9 64 20 9 68 20 "stone"]]}))

(defn climbs? [costs]
  (let [r (run ladder-wall (near 10 69 5) {:goalFlood 0 :costs (cost/planner-costs costs)})]
    (assert (= "found" (str (:status r))) (pr-str (:status r) (:reason r)))
    (contains? (set (pf/moves r)) (:climb-up pf/MOVE))))

(deftest a-climb-price-flips-ladder-for-stair
  (is (true? (climbs? {})))
  (is (false? (climbs? {:climb-up 5})) "dear ladder: the long stair"))

(deftest landing-is-checked-by-block-name-and-factor
  (are [landing ok?] (= ok? (nil? (cost/landing-problem landing)))
    nil true
    {} true
    {"hay_block" 0.2 "slime_block" -1 "water" 0} true
    {"no_such_block" 1} false
    {"hay_block" "soft"} false
    {"hay_block" js/Infinity} false
    {:hay_block 0.2} false
    [[:hay_block 1]] false))

(deftest landing-maps-every-state-of-a-block-to-its-factor
  (let [hay (.stateAt (world {:blocks [[0 70 0 "hay_block"]]}) 0 70 0)
        slime (.stateAt (world {:blocks [[0 70 0 "slime_block"]]}) 0 70 0)
        honey (.stateAt (world {:blocks [[0 70 0 "honey_block"]]}) 0 70 0)
        stone (.stateAt (world {:blocks [[0 70 0 "stone"]]}) 0 70 0)
        m (cost/planner-landing nil)]
    (is (= [0.2 -1 0.2 nil] [(.get m hay) (.get m slime) (.get m honey) (.get m stone)]) "hay, slime and honey by default")
    (is (= [1 nil] [(.get (cost/planner-landing {"hay_block" 1}) hay) (.get (cost/planner-landing {"hay_block" 1}) stone)]))
    (is (= 0 (.get (cost/planner-landing {"stone" 0}) stone)))))

(deftest with-drops-sets-the-landing-of-the-policy
  (let [hay (.stateAt (world {:blocks [[0 70 0 "hay_block"]]}) 0 70 0)]
    (is (= 0.2 (.get (.-landing (wplan/with-drops #js {} {})) hay)) "the default")
    (is (= 0.5 (.get (.-landing (wplan/with-drops #js {} {:landing {"hay_block" 0.5}})) hay)))))

(deftest air-profile-follows-the-helmet
  (are [equipment expected] (= expected (cost/air-profile equipment))
    nil {}
    {:head {:name "iron_helmet"}} {}
    {:head {:name "diving_helmet" :enchants [{:name "respiration" :level 3}]}} {:air-drain 0.25}
    {:head {:name "turtle_helmet"}} {:air-grace 10}
    {:head {:name "turtle_helmet" :enchants [{:name "respiration" :level 1}]}} {:air-drain 0.5 :air-grace 10}))

(deftest with-drops-sets-the-air-costs-under-the-callers-costs
  (let [o (wplan/with-drops #js {} {:air-drain 0.5 :air-grace 10})]
    (is (= 0.5 (.. o -costs -airDrain)))
    (is (= 10 (.. o -costs -airGrace))))
  (let [o (wplan/with-drops #js {} {:air-drain 0.5 :costs {:swim-h 2}})]
    (is (= 0.5 (.. o -costs -airDrain)))
    (is (= 2 (.. o -costs -swimH))))
  (is (nil? (.-costs (wplan/with-drops #js {} {})))))

(deftest with-drops-prices-walking-for-a-body-that-cannot-sprint
  (let [walk-s (.-sprintS (.-costs (wplan/with-drops #js {} {:gait :walk})))]
    (is (= walk-s (.-sprintS (.-costs (wplan/with-drops #js {} {:sprint false})))) "sprint false: sprintS = walkS")
    (is (nil? (.-costs (wplan/with-drops #js {} {:sprint true}))) "can sprint: the planner's defaults")))
