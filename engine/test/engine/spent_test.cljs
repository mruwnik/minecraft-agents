(ns engine.spent-test
  "jobs.lib.spent: what a job placed (item counts and cells), read back for its result and done event."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.test-util :as tu]
            [jobs.lib.spent :as spent]
            [jobs.lib.tidy :as tidy]))

(deftest text-names-counts-and-cells
  (let [log [{:item "oak_log" :cell [1 64 2]} {:item "oak_log" :cell [1 65 2]} {:item "dirt" :cell [3 64 2]}]]
    (is (= {:spent {"oak_log" 2 "dirt" 1} :spent-at [[1 64 2] [1 65 2] [3 64 2]]} (spent/summary log)))
    (is (= "used 2 oak_log, 1 dirt (shelter walls at [1 64 2] [1 65 2] [3 64 2])" (spent/text log "shelter walls")))
    (is (nil? (spent/text [] "shelter walls")))
    (is (= "used 1 crafting_table (table at [1 64 2])" (spent/text [{:item "crafting_table" :cell [1 64 2]}] "table")))))

(deftest text-shortens-a-long-cell-list
  (let [log (for [x (range 12)] {:item "dirt" :cell [x 64 0]})]
    (is (= "used 12 dirt (walls at [0 64 0] [1 64 0] [2 64 0] [3 64 0] ... [11 64 0])" (spent/text log "walls")))))
