(ns dashboard.ui.plansmodel-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.ui.plansmodel :as pm]))

(deftest sort-plans-by-name
  (is (= ["a" "b" "c"] (map :name (pm/sort-plans [{:name "c"} {:name "a"} {:name "b"}])))))

(def plans
  [{:id "village" :name "Village" :children ["farm" "ghost"]}
   {:id "farm" :name "Farm" :children ["melons"]}
   {:id "melons" :name "Melons" :children []}
   {:id "clear" :name "Clearing" :children []}
   {:id "x" :name "X" :children ["y"]}
   {:id "y" :name "Y" :children ["x"]}])

(deftest tree-is-roots-with-their-children-below
  (is (= [["clear" 0] ["village" 0] ["farm" 1] ["melons" 2] ["x" 0] ["y" 1]]
         (map (fn [{:keys [plan depth]}] [(:id plan) depth]) (pm/plan-tree plans))))
  (is (= [] (pm/plan-tree []))))

(deftest tree-lists-a-twice-nested-plan-once
  (is (= ["a" "c" "b"]
         (map (comp :id :plan) (pm/plan-tree [{:id "a" :name "A" :children ["c"]} {:id "b" :name "B" :children ["c"]} {:id "c" :name "C"}])))))

(deftest bar-widths
  (are [counts expected] (= expected (pm/bar-widths counts))
    {:total 10 :match 5 :wrong 2 :missing 1 :extra 0 :unknown 2} {:match 50 :wrong 20 :missing 10 :extra 0 :unknown 20}
    {:total 4 :match 4 :wrong 0 :missing 0 :extra 0 :unknown 0} {:match 100 :wrong 0 :missing 0 :extra 0 :unknown 0}
    {:total 4 :match 1 :extra 3} {:match 25 :wrong 0 :missing 0 :extra 75 :unknown 0}
    {:total 0 :match 0 :wrong 0 :missing 0 :extra 0 :unknown 0} {:match 0 :wrong 0 :missing 0 :extra 0 :unknown 100}))

(deftest completion-colors
  (are [counts expected] (= expected (pm/completion-color counts))
    {:total 10 :match 10 :wrong 0 :missing 0 :unknown 0} pm/green
    {:total 10 :match 9 :wrong 0 :missing 1 :unknown 0} pm/green
    {:total 10 :match 6 :wrong 0 :missing 4 :unknown 0} pm/amber
    {:total 10 :match 1 :wrong 5 :missing 4 :unknown 0} pm/red
    {:total 10 :match 0 :wrong 0 :missing 0 :unknown 10} pm/grey
    {:total 10 :match 3 :wrong 0 :missing 0 :unknown 7} pm/grey
    nil pm/grey))

(deftest default-layer
  (are [layers expected] (= expected (pm/default-layer-y layers))
    [{:y 62 :rows [[{:s "match"} nil]]} {:y 63 :rows [[{:s "match"} {:s "wrong"}]]}] 63
    [{:y 62 :rows [[{:s "match"}]]} {:y 63 :rows [[{:s "match"}]]}] 62
    [] nil))

(deftest texts
  (are [actual expected] (= expected actual)
    (pm/percent-text {:percent 42}) "42%"
    (pm/percent-text nil) "0%"
    (pm/region-text {:min [1 2 3] :max [4 5 6]}) "1, 2, 3  to  4, 5, 6"
    (pm/region-size {:min [1 2 3] :max [4 5 6]}) "4 x 4 x 4"
    (pm/counts-text {:match 1 :wrong 2 :missing 3 :extra 4 :unknown 5 :total 15}) "1/15 match · 3 missing · 2 wrong · 4 extra · 5 unknown"))

(deftest cell-world-positions
  (are [cell expected] (= expected (pm/world-pos {:min-x -17 :min-z -97} 63 cell))
    [0 0] [-17 63 -97]
    [3 2] [-14 63 -95]))

(deftest cell-text
  (are [cell expected] (= expected (pm/cell-text [1 63 -2] cell))
    nil "1 63 -2: not part of the plan"
    {:s "match" :e "wheat" :a "wheat" :el "plot"} "1 63 -2: match, wheat (plot)"
    {:s "missing" :e "wheat" :a "air" :el "plot"} "1 63 -2: missing, wanted wheat, found air (plot)"
    {:s "wrong" :e "wheat" :a "carrots" :el "plot"} "1 63 -2: wrong, wanted wheat, found carrots (plot)"
    {:s "extra" :e "air" :a "oak_leaves" :el "clear"} "1 63 -2: extra, wanted air, found oak_leaves (clear)"
    {:s "unknown" :e "wheat" :a nil :el "plot"} "1 63 -2: unknown, wanted wheat, chunk not dumped (plot)"))

(deftest cell-size-fits-the-box
  (are [cols rows w h expected] (= expected (pm/cell-size cols rows w h))
    10 10 200 760 20
    100 100 200 760 3
    2 2 800 760 40
    0 5 800 760 40))

(deftest cell-at-pixel
  (are [x y expected] (= expected (pm/cell-at 10 3 2 x y))
    0 0 [0 0]
    29 19 [2 1]
    30 5 nil
    5 20 nil
    -1 5 nil))

(def grid-3x3 {:min-x 10 :min-z 20 :cols 3 :rows 3})
(def rows-3x3 [[:a :b :c] [:d :e :f] [:g :h :i]])

(deftest cropping-the-grid-to-bounds
  (are [bounds expected-rows expected-grid] (= {:rows expected-rows :grid expected-grid} (pm/crop-grid rows-3x3 grid-3x3 bounds))
    nil rows-3x3 grid-3x3
    {:min [11 0 21] :max [12 0 22]} [[:e :f] [:h :i]] {:min-x 11 :min-z 21 :cols 2 :rows 2}
    {:min [10 0 20] :max [10 0 20]} [[:a]] {:min-x 10 :min-z 20 :cols 1 :rows 1}
    {:min [0 0 0] :max [100 0 100]} rows-3x3 grid-3x3
    {:min [50 0 50] :max [60 0 60]} [] grid-3x3))

(deftest dimming
  (are [selected cell expected] (= expected (pm/dimmed? selected cell))
    nil {:el "a"} false
    "a" {:el "a"} false
    "b" {:el "a"} true
    "b" nil false))

(deftest element-rows-label-what-an-element-is
  (is (= [{:id "w" :kind "structure" :bounds nil :content "blueprint well" :counts {:total 3} :cells 3 :error nil :ref nil :where "1, 2, 3"}
          {:id "m" :kind "plan" :bounds {:min [0 0 0] :max [1 1 1]} :content "plan melons" :counts nil :cells 0 :error "unknown plan" :ref "melons" :where nil}]
         (pm/element-rows {:elements [{:id "w" :kind "structure" :content "blueprint well" :counts {:total 3} :count 3 :at [1 2 3]}
                                      {:id "m" :kind "plan" :bounds {:min [0 0 0] :max [1 1 1]} :content "plan melons" :count 0 :error "unknown plan" :ref "melons"}]}))))
