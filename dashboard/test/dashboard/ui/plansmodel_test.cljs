(ns dashboard.ui.plansmodel-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.ui.plansmodel :as pm]))

(deftest sort-plans-by-name
  (is (= ["a" "b" "c"] (map :name (pm/sort-plans [{:name "c"} {:name "a"} {:name "b"}])))))

(deftest bar-widths
  (are [summary expected] (= expected (pm/bar-widths summary))
    {:total 10 :match 5 :wrong 2 :missing 1 :unknown 2} {:match 50 :wrong 20 :missing 10 :unknown 20}
    {:total 4 :match 4 :wrong 0 :missing 0 :unknown 0} {:match 100 :wrong 0 :missing 0 :unknown 0}
    {:total 0 :match 0 :wrong 0 :missing 0 :unknown 0} {:match 0 :wrong 0 :missing 0 :unknown 100}))

(deftest completion-colors
  (are [summary expected] (= expected (pm/completion-color summary))
    {:total 10 :match 10 :wrong 0 :missing 0 :unknown 0} pm/green
    {:total 10 :match 6 :wrong 0 :missing 4 :unknown 0} pm/amber
    {:total 10 :match 1 :wrong 5 :missing 4 :unknown 0} pm/red
    {:total 10 :match 0 :wrong 0 :missing 0 :unknown 10} pm/grey
    {:total 10 :match 3 :wrong 0 :missing 0 :unknown 7} pm/grey
    nil pm/grey))

(deftest default-layer
  (are [layers expected] (= expected (pm/default-layer-y layers))
    [{:y 0 :rows [[{:status "match"} {:status "free"}]]} {:y 1 :rows [[{:status "match"} {:status "wrong"}]]}] 1
    [{:y 0 :rows [[{:status "match"}]]} {:y 1 :rows [[{:status "match"}]]}] 0
    [] nil))

(deftest bill-rows-sorted
  (is (= [["wheat_seeds" 40] ["oak_fence" 1] ["water_bucket" 1]]
         (pm/bill-rows {:oak_fence 1 :wheat_seeds 40 :water_bucket 1}))))

(deftest cell-text
  (are [cell expected] (= expected (pm/cell-text cell))
    {:status "free"} "not part of the plan"
    {:status "match" :expected "wheat" :actual "wheat"} "match: wheat"
    {:status "missing" :expected "wheat" :actual "air"} "missing: wanted wheat, found air"
    {:status "wrong" :expected "wheat" :actual "carrots"} "wrong: wanted wheat, found carrots"
    {:status "unknown" :expected "wheat" :actual nil} "unknown: wanted wheat, chunk not dumped"))

(deftest cell-size
  (are [cols rows w h expected] (= expected (pm/cell-size cols rows w h))
    10 10 1000 1000 40
    30 30 600 600 20
    200 10 600 600 6
    0 0 100 100 40))

(deftest cell-at
  (are [x y expected] (= expected (pm/cell-at 10 4 3 x y))
    0 0 [0 0]
    25 14 [2 1]
    29 29 [2 2]
    40 0 nil
    -1 0 nil
    0 30 nil))

(deftest layer-world-y
  (are [place-y layer-y expected] (= expected (pm/world-y place-y layer-y))
    71 0 71
    71 1 72))

(deftest legend-char
  (are [ch expected] (= expected (pm/shown-char ch))
    "w" "w"
    "_" ""
    "" "."))
