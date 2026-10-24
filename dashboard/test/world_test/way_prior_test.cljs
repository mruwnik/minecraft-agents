(ns world-test.way-prior-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.way-prior :as w]))

(defn priced [t set k n ways] {:kind :way-on.priced :time-ms t :seq t :data {:set set :k k :n n :ways ways}})
(defn seen [t way on] {:kind :way-on.seen :time-ms t :seq t :data {:way way :on on}})

(deftest a-way-belongs-to-the-place-that-first-listed-it-and-is-read-once
  (let [{:keys [places unread]}
        (w/places [(priced 1 :dark 1 3 [[0 64 0 0] [5 64 0 1]])
                   (priced 5 :lit 4 4 [[5 64 0 1] [9 64 0 2]])
                   (seen 9 [5 64 0 1] true)
                   (seen 10 [5 64 0 1] false)
                   (seen 11 [0 64 0 0] false)])]
    (is (= [{:set :dark :k 1 :n 3 :ways 2 :seen 2 :on 1}
            {:set :lit :k 4 :n 4 :ways 1 :seen 0 :on 0}]
           places))
    (is (= 1 unread) "the way listed last was never seen")))

(deftest the-posterior-follows-the-counts-the-ways-were-priced-with
  (let [places (fn [k y] (vec (repeat 30 {:set :lit :k k :n 3 :seen 2 :on y})))
        agree (w/fit (places 3 2))
        shut (w/fit (places 0 0))
        none (w/fit [{:k 3 :n 3 :seen 0 :on 0}])]
    (is (> (get-in agree [:mean :mean]) 0.9))
    (is (< (get-in shut [:mean :mean]) 0.1))
    (is (< (get-in agree [:mean :lo]) (get-in agree [:mean :mean]) (get-in agree [:mean :hi])))
    (is (nil? none) "a place with no way read tells nothing")))

(deftest the-report-splits-by-setting
  (let [r (w/report (concat (repeat 5 {:set :lit :k 2 :n 3 :seen 1 :on 1}) (repeat 5 {:set :dark :k 0 :n 3 :seen 1 :on 0})) 7)]
    (is (= [:dark :lit] (keys (:by-setting r))))
    (is (= 10 (get-in r [:all :places])))
    (is (= 7 (:unread r)))))
