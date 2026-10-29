(ns world-test.dark-reach-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.dark-reach :as w]))

(defn world
  "at of stone everywhere but the open cells (a set of [x y z]), dark but where lit (a set), unrecorded past x 60."
  [open lit]
  (fn [x y z] (when (<= x 60) {:name (if (open [x y z]) "air" "stone") :block (if (lit [x y z]) 15 0) :sky 0})))

(defn tunnel [x0 x1] (set (for [x (range x0 (inc x1)) y [0 1]] [x y 0])))

(deftest a-dark-stand-is-far-only-when-dark-floor-past-24-walks-to-it
  (is (= :near (w/reach (world (tunnel 0 20) #{}) [0 0 0])) "a short dead-end tunnel")
  (is (= :far (w/reach (world (tunnel 0 40) #{}) [0 0 0])) "a long dark one")
  (is (= :near (w/reach (world (tunnel 0 40) (tunnel 25 40)) [0 0 0])) "a long one lit past 24")
  (is (nil? (w/reach (world (tunnel 0 70) (tunnel 25 70)) [0 0 0])) "a walk into land never recorded is left out"))

(deftest a-fall-down-an-open-column-walks-to-the-stand
  (let [ledge (into (tunnel 0 10) (for [x (range 11 40) y [3 4]] [x y 0]))]
    (is (= :near (w/reach (world ledge #{}) [0 0 0])) "a ledge 3 up with no room to drop through is walled off")
    (is (= :far (w/reach (world (into ledge #{[10 2 0] [10 3 0] [10 4 0]}) #{}) [0 0 0])) "an open shaft down from it")))

(deftest the-prior-is-fitted-over-chunks
  (let [r (w/report [{:roofed {:far 8 :near 2}} {:roofed {:far 4 :near 6}}] 1)]
    (is (= {:far 12 :near 8 :chunks 2 :p-far 0.6} (select-keys (:roofed r) [:far :near :chunks :p-far])))
    (is (> 1e-9 (js/Math.abs (- 0.6 (let [[a b] (:prior (:roofed r))] (/ a (+ a b)))))) "the fit keeps the mean")))
