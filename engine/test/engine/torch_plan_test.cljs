(ns engine.torch-plan-test
  "jobs.lib.torch-plan: the fewest torch spots that leave no dark cell."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.torch-plan :as tp]))

(defn open-in [cells] (let [s (set cells)] #(contains? s %)))

(defn solve [cells & {:keys [dark? sources spots]}]
  (tp/plan {:cells cells :dark? (or dark? (constantly true)) :open? (open-in cells)
            :spots (or spots cells) :sources sources}))

(defn lit-after [cells result]
  (let [open? (open-in cells)]
    (into #{} (mapcat #(tp/lit-by open? %)) (:spots result))))

(defn line [n] (mapv (fn [x] [x 0 0]) (range n)))

(defn l-corridor [a b] (into (mapv (fn [x] [x 0 0]) (range a)) (map (fn [z] [(dec a) 0 z]) (range 1 b))))

(defn room [n] (vec (for [x (range n) z (range n)] [x 0 z])))

(deftest a-straight-tunnel-gets-a-torch-per-twice-the-spread
  (are [n torches] (= torches (count (:spots (solve (line n)))))
    1 1
    14 1
    27 1
    28 2
    40 2
    54 2
    55 3))

(deftest every-dark-cell-is-lit-and-nothing-is-left-over
  (doseq [cells [(line 40) (l-corridor 30 20) (room 9) (room 30)]
          :let [r (solve cells)]]
    (is (empty? (:uncovered r)))
    (is (every? (lit-after cells r) cells))))

(deftest a-nine-by-nine-room-takes-one-torch-and-an-l-corridor-two
  (is (= 1 (count (:spots (solve (room 9))))))
  (is (= 2 (count (:spots (solve (l-corridor 30 20)))))))

(deftest a-torch-on-the-line-spares-the-cells-it-lights
  (let [cells (line 40)]
    (is (= 1 (count (:spots (solve cells :sources [[0 0 0]])))) "cells 14..39 are left for one more torch")
    (is (= 0 (count (:spots (solve cells :sources [[13 0 0] [37 0 0]])))))))

(deftest lit-cells-are-left-alone
  (is (empty? (:spots (solve (line 40) :dark? (constantly false)))))
  (is (= 1 (count (:spots (solve (line 40) :dark? #(>= (first %) 30)))))))

(deftest light-does-not-pass-a-wall
  (let [open? (open-in [[0 0 0] [1 0 0] [5 0 0] [6 0 0]])
        r (tp/plan {:cells [[0 0 0] [6 0 0]] :dark? (constantly true) :open? open? :spots [[0 0 0] [6 0 0]]})]
    (is (= [[0 0 0] [6 0 0]] (:spots r)))))

(deftest cells-no-spot-reaches-are-reported
  (let [r (tp/plan {:cells (line 3) :dark? (constantly true) :open? (open-in (line 3)) :spots []})]
    (is (= (line 3) (:uncovered r)))
    (is (empty? (:spots r)))))

(deftest the-spot-lighting-most-is-chosen-and-a-tie-goes-to-the-earlier-listed
  (is (= [[13 0 0]] (:spots (solve (line 27) :spots (line 27)))) "reach limit 13 each way")
  (is (= [[5 0 0]] (:spots (solve (line 14) :spots [[5 0 0] [4 0 0]])))))

(deftest a-shorter-spread-takes-more-torches
  (let [cells (line 27)
        spots (fn [spread] (count (:spots (tp/plan {:cells cells :dark? (constantly true) :open? (open-in cells) :spots cells :spread spread}))))]
    (is (= 3 (spots 4)) "a torch lights 4 cells each way: 9 cells")
    (is (= 1 (spots 13)))))
