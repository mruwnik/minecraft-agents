(ns world-test.dark-meets-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.dark-meets :as d]))

;; one column at x 0: stone below y 30, dark air (block 0, sky 0) up to 60, lit air (block 15) at 61 and over
(defn cave [x y z]
  (when (and (= 0 x) (= 0 z))
    {:name "air" :block (if (> y 60) 15 0) :sky 0}))

(defn rec [t k & {:as more}] (merge {:t t :k k :gen "g" :job "j1"} more))
(defn pos [t y] (rec t :pos :x 0.5 :y y :z 0.5))

(deftest exposure-counts-the-seconds-between-close-positions-in-dark-cells-by-band
  (is (= {:under-40 5} (d/exposure cave [(pos 0 10) (pos 5000 10)])))
  (is (= {:40-55 3} (d/exposure cave [(pos 0 50) (pos 3000 70)])) "the first cell is the one counted")
  (is (= {} (d/exposure cave [(pos 0 70) (pos 3000 10)])) "a lit cell")
  (is (= {} (d/exposure cave [(pos 0 10) (pos 20000 10)])) "a gap hides where the body was"))

(deftest a-meet-is-the-first-run-about-a-spawnable-mob-and-counts-to-the-dark-cell-it-met-it-in
  (is (= [:under-40 nil]
         (d/meets cave [(pos 0 10) (rec 100 :decided :mob "zombie" :job "j1")
                        (rec 200 :decided :mob "zombie" :job "j2") ; the same meet going on
                        (pos 99000 70) (rec 99100 :decided :mob "creeper" :job "j3")
                        (rec 99200 :decided :mob "cow" :job "j4")]))))

(deftest hurt-sums-the-hp-of-spawnable-mobs-dealt-at-dark-cells
  (is (= {:under-40 {:hp 4 :hits 2}}
         (d/hurt cave [(rec 0 :hurt :amount 2.5 :causes #{"zombie"} :at {:x 0.5 :y 10 :z 0.5})
                       (rec 1 :hurt :amount 1.5 :causes #{"arrow"} :at {:x 0.5 :y 10 :z 0.5})
                       (rec 2 :hurt :amount 3 :causes #{"fall"} :at {:x 0.5 :y 10 :z 0.5})
                       (rec 3 :hurt :amount 3 :causes #{"zombie"} :at {:x 0.5 :y 70 :z 0.5})]))))

(deftest the-rate-is-k-plus-one-over-the-dark-seconds
  (let [r (d/report [{:under-40 100} {:under-40 100}] [[:under-40] [:under-40 nil]] [{:under-40 {:hp 10 :hits 3}}])]
    (is (= {:n-s 200 :k 2 :per-s 0.015 :hp 10 :hp-per-s 0.05} (get-in r [:bands :under-40])))
    (is (= 200 (get-in r [:all :n-s])))))
