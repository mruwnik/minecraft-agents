(ns engine.within-test
  "u/within?: the measure moveTo's arrival uses."
  (:require [cljs.test :refer [deftest is are]]
            [engine.jobs.util :as u]))

(def player {:x -4989.5 :y 101 :z 5000.5})

(deftest within-uses-floored-cells
  (are [body range ok] (= ok (u/within? body player range))
    {:x -4991.1 :y 101 :z 5000.9} 2 true
    {:x -4991.9 :y 101 :z 5000.1} 2 true
    {:x -4991.9 :y 101 :z 5000.1} 1 false
    {:x -4992.5 :y 101 :z 5000.5} 2 false
    {:x -4991.5 :y 103 :z 5000.5} 2 false
    {:x -4990.5 :y 101 :z 5000.5} 1 true
    {:x -4989.5 :y 101 :z 5000.5} 0 true))
