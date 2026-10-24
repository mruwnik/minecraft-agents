(ns engine.route-ways-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.movement.go-to.route-ways :as ways]))

(def route
  "A walk across, two steps up east digging, two pillar blocks with a roof cell dug, a step up north laying its floor, a
  step across digging and a walk on."
  [{:to [1 0 0] :move :walk :dig []}
   {:to [2 1 0] :move :walk :dig [[1 2 0] [2 1 0]]}
   {:to [3 2 0] :move :walk :dig [[3 2 0]]}
   {:to [3 3 0] :move :pillar :dig [] :place [3 2 0]}
   {:to [3 4 0] :move :pillar :dig [[3 6 0]] :place [3 3 0]}
   {:to [3 5 -1] :move :laid :dig [] :place [3 4 -1]}
   {:to [3 5 -2] :move :walk :dig [[3 6 -2] [3 5 -2]]}
   {:to [3 5 -3] :move :walk :dig []}])

(deftest a-route-runs-as-a-stair-a-pillar-and-a-dig-with-walks-between-and-none-at-the-end
  (is (= [{:step :approach :pos [1 0 0] :from [0 0 0]}
          {:step :stair :heading :east :steps 2 :from [1 0 0]}
          {:step :pillar :height 2 :roof 1 :from [3 2 0]}
          {:step :stair :heading :north :steps 1 :from [3 4 0]}
          {:step :dig-route :cells [{:cell [3 6 -2] :block "stone" :guess false} {:cell [3 5 -2] :block "stone" :guess false}] :from [3 5 -1]}]
         (ways/segments (constantly "stone") (constantly false) [0 0 0] route))))

(deftest the-next-way-starts-where-the-last-one-left-the-body
  (let [e {:step :stair :then [{:step :pillar :from [3 2 0]} {:step :approach :pos [9 9 9] :from [3 4 0]}]}]
    (is (= {:step :pillar :from [3 2 0] :then [{:step :approach :pos [9 9 9] :from [3 4 0]}]} (ways/next-way e [3 2 0])))
    (is (nil? (ways/next-way e [3 1 0])) "a stair that stopped short: the climb is searched again")
    (is (= :approach (:step (ways/next-way {:then [{:step :approach :pos [9 9 9] :from [3 4 0]}]} [5 5 5]))) "a walk starts anywhere")
    (is (nil? (ways/next-way {:step :pillar} [3 2 0])))))
