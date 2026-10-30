(ns world-test.food-gets-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.food-gets :as t]))

(defn ended [job how wall yields]
  (pr-str {:generation-id "g" :kind :ended :context {:job-id job :reflex-id :hungry} :message (str "hungry → jobs.survival.get-food: " how)
           :data {:report (cond-> {:uses {:job-time 0 :wall-time wall}} yields (assoc :yields {:items yields}))}}))

(defn took [job] (pr-str {:generation-id "g" :kind :food.took-from-chest :context {:job-id (str job "/food")} :data {:item "bread" :count 3}}))

(deftest counts-the-field-food-of-hungry-get-food-calls-that-took-nothing-from-a-chest
  (let [rows (keep t/scan-line [(ended "j1" "done" 60 {"chicken" 2 "cooked_chicken" 2 "wheat_seeds" 3})
                                (ended "j2" "dropped" 30 nil)
                                (took "j3") (ended "j3" "done" 5 {"bread" 3})
                                (ended "j4" "done" 10 {"beef" 1})])]
    (is (= {:n 3 :s 100 :points 15 :point-s 6.67} (t/report rows))
        "two chickens cooked count once at 6 each, a beef at 3, the chest call left out, the cut call's seconds kept")
    (is (nil? (t/report [])))))
