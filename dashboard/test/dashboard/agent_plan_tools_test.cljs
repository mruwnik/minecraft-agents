(ns dashboard.agent-plan-tools-test
  (:require [cljs.test :refer [deftest is]]
            [clojure.string :as str]
            [dashboard.agent-plan-tools :as tools]))

(def plan-text "{:id \"score\" :parts [{:id \"base\" :cells [[0 64 0] [1 64 0] [2 64 0]] :want \"stone\"}]}")

(defn cells-text [n] (str/join " " (map #(str "[" % " 64 0]") (range n))))

(defn prepare [text id blueprints plans]
  (tools/prepare-native text id blueprints plans nil []))

(deftest score-counts-and-materials-follow-the-block-lookup
  (let [prepared (prepare plan-text "score" [] [])
        blocks {[0 64 0] {:name "stone"} [1 64 0] {:name "dirt"} [2 64 0] {:name "air"}}
        with-inventory (tools/score-native (:expansion prepared) blocks (constantly nil) {"stone" 1} true 0 10)
        unknown (tools/score-native (:expansion prepared) (constantly nil) (constantly nil) {} false 0 10)]
    (is (:ok prepared))
    (is (= {:match 1 :wrong 1 :missing 1} (select-keys (:counts with-inventory) [:match :wrong :missing])))
    (is (= 3 (get-in with-inventory [:materials :required "stone"])))
    (is (= 1 (get-in with-inventory [:materials :remaining "stone" :shortage])))
    (is (= 3 (get-in unknown [:counts :unknown])))
    (is (= :unknown (get-in unknown [:materials :availability])))))

(deftest candidate-checks-report-conflicts-with-every-stored-plan
  (let [prepared (prepare "{:id \"candidate\" :parts [{:id \"a\" :cells [[4 64 9]] :want \"dirt\"}]}" "candidate" []
                          [{:id "active" :text "{:id \"active\" :parts [{:id \"b\" :cells [[4 64 9]] :want \"stone\"}]}"}])]
    (is (:ok prepared))
    (is (= [{:with "active" :count 1}] (mapv #(select-keys % [:with :count]) (:conflicts prepared))))))

(deftest the-aggregate-cell-budget-counts-every-stored-plan
  (let [big (fn [id] {:id id :text (str "{:id \"" id "\" :parts [{:id \"all\" :cells [" (cells-text 60000) "] :want \"stone\"}]}")})
        prepared (prepare "{:id \"candidate\" :parts [{:id \"a\" :cells [[0 70 0]] :want \"dirt\"}]}" "candidate" [] [(big "one") (big "two")])]
    (is (false? (:ok prepared)))
    (is (re-find #"exceeds 100000" (str/join " " (:errors prepared))))))

(deftest oversized-plan-geometry-is-refused-before-expanding-cells
  (let [prepared (prepare (str "{:id \"large\" :parts [{:id \"all\" :cells [" (cells-text 100001) "] :want \"stone\"}]}") "large" [] [])]
    (is (false? (:ok prepared)))
    (is (re-find #"aggregate plan conflict index exceeds 100000" (str/join " " (:errors prepared))))
    (is (empty? (:cells prepared)))))

(deftest wide-shallow-blueprint-placements-are-estimated-by-width
  (let [blueprint (str "{:id \"wide\" :front :north :key {\"S\" \"stone\"} :layers [[\"" (apply str (repeat 100001 "S")) "\"]]}")
        prepared (prepare "{:id \"wide-plan\" :parts [{:id \"p\" :blueprint \"wide\" :at [0 64 0]}]}" "wide-plan"
                          [{:id "wide" :text blueprint}] [])]
    (is (false? (:ok prepared)))
    (is (re-find #"exceeds 100000" (str/join " " (:errors prepared))))
    (is (empty? (:cells prepared)))))

(deftest a-stair-part-is-estimated-by-its-steps
  (let [prepared (prepare "{:id \"deep\" :parts [{:id \"s\" :stair {:from [0 50000 0] :heading :east :to-y 5000} :want :clear}]}" "deep" [] [])]
    (is (false? (:ok prepared)))
    (is (re-find #"exceeds 100000" (str/join " " (:errors prepared))))
    (is (empty? (:cells prepared)))))

(deftest score-lists-the-wrong-cells-with-wanted-and-found
  (let [prepared (prepare plan-text "score" [] [])
        blocks {[0 64 0] {:name "stone"} [1 64 0] {:name "dirt"} [2 64 0] {:name "air"}}
        score (tools/score-native (:expansion prepared) blocks (constantly nil) {} false 0 10)]
    (is (= [{:pos [1 64 0] :part "base" :want "stone" :found "dirt"}] (:wrong-cells score)))))

(deftest score-lists-the-missing-cells-with-wanted-and-says-how-many
  (let [text (str "{:id \"gaps\" :parts [{:id \"base\" :cells [" (cells-text 14) "] :want [:any \"oak_fence\" \"birch_fence\"]}]}")
        prepared (prepare text "gaps" [] [])
        score (tools/score-native (:expansion prepared) (constantly {:name "air"}) (constantly nil) {} false 0 10)]
    (is (= {:pos [0 64 0] :part "base" :want "oak_fence | birch_fence" :found "air"} (first (:missing-cells score))))
    (is (= [10 14] [(count (:missing-cells score)) (:missing-total score)]))))

(deftest score-caps-the-wrong-cell-list-and-says-so
  (let [text (str "{:id \"many\" :parts [{:id \"base\" :cells [" (cells-text 14) "] :want \"stone\"}]}")
        prepared (prepare text "many" [] [])
        score (tools/score-native (:expansion prepared) (constantly {:name "dirt"}) (constantly nil) {} false 0 10)]
    (is (= 10 (count (:wrong-cells score))))
    (is (= 14 (:wrong-total score)))))
