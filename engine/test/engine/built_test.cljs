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

(deftest a-pen-in-use-is-not-evicted-by-later-placements
  (let [others (fn [from n] (for [i (range from (+ from n))] [(* 16 i) 64 0]))
        pen [[0 64 0]]
        placed (reduce built/add-cell {} (concat pen (others 1 (dec built/max-columns))))
        rebooked (built/remember-cell placed [0 64 0])
        used (built/refresh placed pen)
        later #(booked (reduce built/add-cell % (others 1000 3)))]
    (is (not (contains? (later placed) [0 64 0])) "unused: aged out")
    (is (contains? (later rebooked) [0 64 0]) "a re-booked cell touches its column")
    (is (contains? (later used) [0 64 0]) "a pen read as own touches its column")
    (is (= placed (built/refresh placed [[5000 64 0]])) "cells never booked change nothing")))
