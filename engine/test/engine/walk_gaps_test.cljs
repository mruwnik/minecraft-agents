(ns engine.walk-gaps-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.walk.gaps :as gaps]))

(def steps
  [{:x 0 :y 64 :z 0 :move :start}
   {:x 0 :y 64 :z 1 :move :walk}
   {:x 0 :y 64 :z 4 :move :gap}
   {:x 0 :y 64 :z 5 :move :walk}])

(defn run [ticks]
  (:events (reduce (fn [{:keys [tracked events]} [i controls done]]
                     (let [[tracked' data] (gaps/tick steps tracked i controls done)]
                       {:tracked tracked' :events (cond-> events data (conj data))}))
                   {} ticks)))

(def landed {:width 2 :dy 0 :sprint true :from [0 64 1] :to [0 64 4] :out :landed})

(deftest a-gap-step-that-the-body-passes-is-landed-with-the-sprint-it-ran
  (is (= [landed] (run [[1 {:sprint false} nil] [2 {:sprint true} nil] [2 {:sprint false} nil] [3 {} nil]]))))

(deftest a-gap-step-the-walk-ends-on-is-landed-when-it-arrives
  (is (= [landed] (run [[2 {:sprint true} nil] [2 nil {:status :arrived}]]))
      "the last step of a plan is the gap's landing"))

(deftest a-gap-step-ended-off-plan-or-stuck-is-fell-or-stuck
  (is (= [(assoc landed :out :fell)] (run [[2 {:sprint true} nil] [2 nil {:status :off-plan :step 2}]])))
  (is (= [(assoc landed :out :stuck)] (run [[2 {:sprint true} nil] [2 nil {:status :stuck :step 2}]]))))

(deftest a-walk-that-replans-in-the-air-or-never-reaches-the-gap-emits-nothing
  (is (empty? (run [[2 {:sprint true} nil] [2 nil {:status :replan :why :changed}]])))
  (is (empty? (run [[1 {} nil] [1 nil {:status :off-plan :step 1}]]))))
