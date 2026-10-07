(ns engine.cost-planner-test
  "jobs.lib.cost.planner: go-to's :costs overrides of the planner's per-move prices, and how they reach the planner."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.cost :as cost]
            [jobs.lib.walk.plan :as wplan]))

(deftest costs-are-checked-by-name-and-value
  (are [costs ok?] (= ok? (nil? (cost/planner-costs-problem costs)))
    nil true
    {} true
    {:swim-h 1.5 :climb-up 0} true
    {:swim-h -1} false
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
