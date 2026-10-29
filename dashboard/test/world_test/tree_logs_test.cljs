(ns world-test.tree-logs-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.tree-logs :as t]))

(defn ended [job how n]
  (pr-str {:kind :ended :message (str "x → " job ": " how) :data {:base {:x 1} :logs n :kind (keyword how)}}))

(deftest counts-the-logs-of-fell-tree-done-endings-only
  (let [logs (keep t/scan-line [(ended "jobs.forestry.fell-tree" "done" 4) (ended "jobs.forestry.fell-tree" "done" 9)
                                (ended "jobs.forestry.fell-tree" "done" 5) (ended "jobs.forestry.fell-tree" "condition-failed" 2)
                                (ended "jobs.gather.mine" "done" 7)])]
    (is (= [4 9 5] logs))
    (is (= {:n 3 :mean 6 :median 5} (t/report logs)) "the mean, not the median, is the prior")
    (is (nil? (t/report [])))))
