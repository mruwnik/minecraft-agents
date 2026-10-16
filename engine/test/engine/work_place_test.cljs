(ns engine.work-place-test
  "jobs.lib.work-place/spot: the cell a farm or build job's safe? judges."
  (:require [cljs.test :refer [deftest are]]
            [jobs.lib.work-place :as work-place]))

(defn spot [args] (work-place/spot {:args args}))

(deftest spot-is-the-middle-of-the-place-the-args-name
  (are [args expected] (= expected (spot args))
    {:at [3 64 5]} {:x 3 :y 64 :z 5}
    {:pos {:x 1 :y 2 :z 3}} {:x 1 :y 2 :z 3}
    {:box {:min {:x 0 :y 63 :z 0} :max {:x 4 :y 64 :z 6}}} {:x 2 :y 63 :z 3}
    {:from [4 70 8] :to [0 64 0]} {:x 2 :y 67 :z 4}
    {:center [9 64 9] :radius 3} {:x 9 :y 64 :z 9}
    {:at [3 64 5] :center [9 64 9]} {:x 3 :y 64 :z 5}
    {} nil))
