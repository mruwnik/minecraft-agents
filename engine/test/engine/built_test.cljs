(ns engine.built-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.built :as built]))

(defn booked [data] (into #{} cat (vals (:columns data))))

(deftest a-big-pen-keeps-its-cells-however-many-are-placed-after-it
  (let [pen (for [x (range 16) z (range 16) y (range 20)] [x y z])
        others (for [i (range 1 (inc (dec built/max-columns)))] [(* 16 i) 64 0])
        data (reduce built/add-cell {} (concat pen others))]
    (is (= 5120 (count pen)))
    (is (every? (booked data) pen))))

(deftest the-column-touched-longest-ago-goes-whole-past-the-cap
  (let [data (reduce built/add-cell {} (for [i (range (inc built/max-columns))] [(* 16 i) 64 0]))]
    (is (not (contains? (booked data) [0 64 0])))
    (is (= built/max-columns (count (:columns data))))))

(deftest a-dug-cell-leaves-the-book-and-an-emptied-column-with-it
  (let [data (reduce built/add-cell {} [[1 64 1] [2 64 1] [40 64 1]])
        less (built/drop-cell data [1 64 1])]
    (is (= #{[2 64 1] [40 64 1]} (booked less)))
    (is (= [[0 0]] (:order (built/drop-cell less [40 64 1]))))))
