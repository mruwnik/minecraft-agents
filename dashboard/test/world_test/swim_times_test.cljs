(ns world-test.swim-times-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.swim-times :as t]))

(defn act [kind id job t & [status]]
  (pr-str {:source :action :kind kind :time-ms t :context {:job-id job :action-id id}
           :data (cond-> {:name "swim"} status (assoc :status status))}))

(defn unstuck [job t status] (pr-str {:source :job :kind :go-to.unstuck :time-ms t :context {:job-id job} :data {:status status}}))

(def lines
  [(act :started "a" "j1/walk" 1000) (act :done "a" "j1/walk" 2500 "landed") (unstuck "j1/walk" 2510 "landed")
   (act :started "b" "j1/walk" 3000) (act :done "b" "j1/walk" 5501 "timeout") (unstuck "j1/walk" 5502 "timeout")
   (act :started "c" "j2" 100) (act :done "c" "j2" 900 "surfaced")])

(deftest swims-are-the-swim-actions-go-to-reported-in-any-line-order
  (is (= [{:status "landed" :ms 1500} {:status "timeout" :ms 2501}] (t/swims lines)))
  (is (= (set (t/swims lines)) (set (t/swims (reverse lines))))))

(deftest a-quantile-past-the-cap-is-a-lower-bound
  (let [rows (concat (repeat 3 {:status "landed" :ms 1000}) [{:status "timeout" :ms 2500}])]
    (is (= {:n 4 :landed 3 :capped 1 :other 0 :q 0.5 :quantile 1000 :lower-bound? false} (t/report rows 0.5)))
    (is (= {:n 4 :landed 3 :capped 1 :other 0 :q 0.95 :quantile 2500 :lower-bound? true} (t/report rows 0.95))))
  (is (nil? (t/report [] 0.5))))
