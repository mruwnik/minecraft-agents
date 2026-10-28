(ns engine.walk-gaps-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.walk.gaps :as gaps]))

(def steps
  [{:x 0 :y 64 :z 0 :move :start}
   {:x 0 :y 64 :z 1 :move :walk}
   {:x 0 :y 64 :z 4 :move :gap}
   {:x 0 :y 64 :z 5 :move :walk}])

(defn run
  "Events of ticks [i controls done y], y the feet height (default 64: on the platform)."
  [ticks]
  (:events (reduce (fn [{:keys [tracked events]} [i controls done y]]
                     (let [[tracked' data] (gaps/tick steps tracked i controls done (or y 64))]
                       {:tracked tracked' :events (cond-> events data (conj data))}))
                   {} ticks)))

(def landed {:width 2 :run 2 :dy 0 :sprint true :from [0 64 1] :to [0 64 4] :out :landed})

(deftest a-gap-step-that-the-body-passes-is-landed-with-the-sprint-it-ran
  (is (= [landed] (run [[1 {:sprint false} nil] [2 {:sprint true} nil] [2 {:sprint false} nil] [3 {} nil]]))))

(deftest a-gap-step-the-walk-ends-on-is-landed-when-it-arrives
  (is (= [landed] (run [[2 {:sprint true} nil] [2 nil {:status :arrived}]]))
      "the last step of a plan is the gap's landing"))

(deftest a-body-ending-below-the-gap-fell-whether-the-walk-ends-off-plan-or-replans
  (is (= [(assoc landed :out :fell)] (run [[2 {:sprint true} nil] [2 nil {:status :off-plan :step 2} 60.5]])))
  (is (= [(assoc landed :out :fell)] (run [[2 {:sprint true} nil] [2 nil {:status :replan :why :changed} 60.5]]))
      "a miss that ends in a replan from the hole is a miss"))

(deftest a-gap-step-stuck-is-stuck
  (is (= [(assoc landed :out :stuck)] (run [[2 {:sprint true} nil] [2 nil {:status :stuck :step 2}]]))))

(deftest a-walk-ending-on-the-platform-before-the-jump-is-no-jump
  (is (empty? (run [[2 {:sprint true} nil] [2 nil {:status :off-plan :step 2}]])) "knocked off the line in the run-up, still level")
  (is (empty? (run [[2 {:sprint true} nil] [2 nil {:status :replan :why :changed}]])))
  (is (empty? (run [[1 {} nil] [1 nil {:status :off-plan :step 1}]]))))

(deftest a-takeoff-entered-by-a-straight-walk-has-a-run-up-of-2-else-1
  (is (= [2 1 1 1]
         [(gaps/run-up steps 2)
          (gaps/run-up (assoc-in steps [1 :move] :jump) 2)
          (gaps/run-up [(first steps) {:x 0 :y 64 :z 3 :move :gap}] 1)
          (gaps/run-up [{:x 3 :y 64 :z 0 :move :start} {:x 3 :y 64 :z 1 :move :walk} {:x 0 :y 64 :z 1 :move :gap}] 2)])
      "a jump up into the takeoff, the start cell as takeoff, a walk not in the jump's direction"))
