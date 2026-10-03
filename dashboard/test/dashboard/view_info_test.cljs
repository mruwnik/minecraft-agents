(ns dashboard.view-info-test
  (:require [cljs.test :refer [deftest is testing]]
            [dashboard.view-info :as vi]))

(def pose {:v 1 :t 5 :world "w" :status "online" :dimension "overworld" :pos {:x 1 :y 2 :z 3} :entities [{:id 1}] :yaw 0})
(def hud {:v 1 :health 10 :food 4 :held {:name "dirt" :count 3}})

(deftest summarize-cases
  (doseq [[title args expected]
          [["both" [pose hud 1234.5] {:poseMtimeMs 1234.5 :status "online" :pos {:x 1 :y 2 :z 3} :dimension "overworld" :hud hud}]
           ["no hud" [pose nil 7] {:poseMtimeMs 7 :status "online" :pos {:x 1 :y 2 :z 3} :dimension "overworld" :hud nil}]
           ["hud only" [nil hud nil] {:poseMtimeMs nil :status nil :pos nil :dimension nil :hud hud}]
           ["neither" [nil nil nil] nil]]]
    (testing title
      (is (= expected (apply vi/summarize args))))))
