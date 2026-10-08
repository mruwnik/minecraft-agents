(ns engine.spent-test
  "jobs.lib.spent: what a job placed (item counts and cells), bounded, read back for its result and done event."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.spent :as spent]))

(defn logged [placements] (reduce (fn [log [item cell]] (spent/add log item cell)) nil placements))

(deftest text-names-counts-and-cells
  (let [log (logged [["oak_log" [1 64 2]] ["oak_log" [1 65 2]] ["dirt" [3 64 2]]])]
    (is (= {:spent {"oak_log" 2 "dirt" 1} :spent-at [[1 64 2] [1 65 2] [3 64 2]]} (spent/summary log)))
    (is (= "used 2 oak_log, 1 dirt (shelter walls at [1 64 2] [1 65 2] [3 64 2])" (spent/text log "shelter walls")))
    (is (nil? (spent/text nil "shelter walls")))
    (is (= "used 1 crafting_table (table at [1 64 2])" (spent/text (logged [["crafting_table" {:x 1 :y 64 :z 2}]]) "table")))))

(deftest text-shortens-a-long-cell-list
  (let [log (logged (for [x (range 12)] ["dirt" [x 64 0]]))]
    (is (= "used 12 dirt (walls at [0 64 0] [1 64 0] [2 64 0] [3 64 0] ... [11 64 0])" (spent/text log "walls")))))

(deftest five-blocks-name-all-five-cells
  (let [log (logged (for [x (range 5)] ["dirt" [x 64 0]]))]
    (is (= (mapv (fn [x] [x 64 0]) (range 5)) (:spent-at (spent/summary log))))))

(deftest the-log-stays-bounded-for-a-thousand-places
  (let [log (logged (for [x (range 1000)] ["dirt" [x 64 0]]))]
    (is (= {"dirt" 1000} (:spent (spent/summary log))))
    (is (= [[0 64 0] [1 64 0] [2 64 0] [3 64 0] [999 64 0]] (:spent-at (spent/summary log))))
    (is (<= (count (pr-str log)) 200))))

(deftest absorb-adds-a-childs-spent-to-the-parents-log
  (let [parent (logged [["torch" [0 64 0]]])
        child (spent/summary (logged [["dirt" [5 64 0]] ["dirt" [6 64 0]]]))
        merged (spent/absorb parent child)]
    (is (= {"torch" 1 "dirt" 2} (:spent (spent/summary merged))))
    (is (= [[0 64 0] [5 64 0] [6 64 0]] (:spent-at (spent/summary merged))))
    (is (= parent (spent/absorb parent {})) "a child that placed nothing adds nothing")
    (is (= "used 2 dirt (niche at [5 64 0] [6 64 0])" (spent/text (spent/absorb nil child) "niche")))))
