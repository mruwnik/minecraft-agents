(ns dashboard.view-info-test
  (:require [cljs.test :refer [deftest is testing]]
            [dashboard.view-info :as vi]))

(def pose {:v 1 :t 5 :world "w" :status "online" :dimension "overworld" :pos {:x 1 :y 2 :z 3} :entities [{:id 1}] :yaw 0
           :villagers [{:id 9 :x 4 :y 5 :z 6}]})
(def hud {:v 1 :health 10 :food 4 :held {:name "dirt" :count 3}})

(deftest summarize-cases
  (doseq [[title args expected]
          [["both" [pose hud 1234.5] {:poseMtimeMs 1234.5 :poseT 5 :status "online" :pos {:x 1 :y 2 :z 3} :dimension "overworld" :hud hud
                                 :villagers [{:id 9 :x 4 :y 5 :z 6}]}]
           ["no hud" [pose nil 7] {:poseMtimeMs 7 :poseT 5 :status "online" :pos {:x 1 :y 2 :z 3} :dimension "overworld" :hud nil
                        :villagers [{:id 9 :x 4 :y 5 :z 6}]}]
           ["hud only" [nil hud nil] {:poseMtimeMs nil :poseT nil :status nil :pos nil :dimension nil :hud hud :villagers nil}]
           ["neither" [nil nil nil] nil]]]
    (testing title
      (is (= expected (apply vi/summarize args))))))

(deftest villagers-keeps-only-villagers-with-a-position
  (is (= [{:id 3 :x 1.5 :y 64 :z -2}
          {:id 4 :x 7 :y 65 :z 8}]
         (vi/villagers [{:id 3 :name "villager" :pos {:x 1.5 :y 64 :z -2}}
                        {:id 4 :name "villager" :pos {:x 7 :y 65 :z 8} :health nil}
                        {:id 5 :name "cow" :pos {:x 0 :y 0 :z 0}}
                        {:id 6 :name "wandering_trader" :pos {:x 0 :y 0 :z 0}}
                        {:id 7 :name "villager"}
                        {:id 8 :name "player" :username "Ann" :pos {:x 0 :y 0 :z 0}}]))))

(deftest villagers-of-nothing
  (is (= [] (vi/villagers nil)))
  (is (= [] (vi/villagers []))))
