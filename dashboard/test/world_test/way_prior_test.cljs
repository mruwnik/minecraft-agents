(ns world-test.way-prior-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.way-prior :as w]))

(defn priced [t set k n ways] {:kind :way-on.priced :time-ms t :seq t :data {:set set :k k :n n :ways ways}})
(defn seen [t way on] {:kind :way-on.seen :time-ms t :seq t :data {:way way :on on}})
(defn evicted [t way] {:kind :way-on.censored :time-ms t :seq t :data {:way way :why :evicted}})

(deftest a-way-belongs-to-the-place-that-first-listed-it-and-is-read-once
  (let [{:keys [places evicted]}
        (w/places [(priced 1 :dark 1 3 [[0 64 0 0] [5 64 0 1]])
                   (priced 5 :lit 4 4 [[5 64 0 1] [9 64 0 2] [7 64 0 3]])
                   (seen 9 [5 64 0 1] true)
                   (seen 10 [5 64 0 1] false)
                   (seen 11 [0 64 0 0] false)
                   (evicted 12 [9 64 0 2])])]
    (is (= [{:set :dark :k 1 :n 3 :ways 2 :seen 2 :on 1 :unread 0 :waits [8 10] :unread-ages []}
            {:set :lit :k 4 :n 4 :ways 2 :seen 0 :on 0 :unread 2 :waits [] :unread-ages [7 7]}]
           places))
    (is (= 1 evicted) "one of the two unread ways was dropped by way-log, the other still waits")))

(deftest the-posterior-follows-the-counts-the-ways-were-priced-with
  (let [places (fn [k y] (vec (repeat 30 {:set :lit :k k :n 3 :ways 2 :seen 2 :on y :unread 0})))
        agree (:read (w/fit (places 3 2)))
        shut (:read (w/fit (places 0 0)))]
    (is (> (get-in agree [:mean :mean]) 0.9))
    (is (< (get-in shut [:mean :mean]) 0.1))
    (is (< (get-in agree [:mean :lo]) (get-in agree [:mean :mean]) (get-in agree [:mean :hi])))
    (is (nil? (w/fit [])))))

(deftest unread-ways-bound-the-answer-and-a-read-chance-per-outcome-explains-them
  ;; 3 of 10 stands lead out, yet only ways that go on were read: rock ones went unread
  (let [f (w/fit (vec (repeat 40 {:set :dark :k 3 :n 10 :ways 5 :seen 2 :on 2 :unread 3})))
        mean #(get-in f [% :mean :mean])]
    (is (< (mean :shut) (mean :read) (mean :on)) "every unread way rock, read ways alone, every unread way on")
    (is (< (mean :shut) (mean :censored) (mean :read)) "the censored fit lies inside the bounds, below the missing-at-random one")
    (is (> (get-in f [:censored :r-on :mean]) (get-in f [:censored :r-shut :mean])) "ways that go on are read more often")))

(deftest the-report-splits-by-setting
  (let [r (w/report (concat (repeat 5 {:set :lit :k 2 :n 3 :ways 2 :seen 1 :on 1 :unread 1 :waits [] :unread-ages [100]})
                            (repeat 5 {:set :dark :k 0 :n 3 :ways 1 :seen 1 :on 0 :unread 0}))
                    2)]
    (is (= [:dark :lit] (keys (:by-setting r))))
    (is (= 10 (get-in r [:all :places])))
    (is (= 5 (:unread r)))
    (is (= 2 (:evicted r)))))

(deftest ways-younger-than-the-median-wait-of-the-read-ones-are-not-counted-unread
  (let [p {:set :lit :k 1 :n 2 :ways 4 :seen 2 :on 1 :unread 2 :waits [100 300] :unread-ages [50 400]}]
    (is (= [1] (map :unread (w/mature [p]))) "the median wait is 200 ms: the way waiting 50 is left out, the one waiting 400 stays")
    (is (= [2] (map :unread (w/mature [(assoc p :waits [])]))) "no way read: every unread way counts")))
