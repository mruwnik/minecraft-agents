(ns plan.conflicts-test
  (:require [clojure.test :refer [deftest are is]]
            [plan.conflicts :as conflicts]
            [plan.shape :as shape]))

(defn expansion [& cells]
  {:cells (mapv (fn [[pos want]] {:pos pos :want want :part "p"}) cells)})

(defn plan [id status & parts] {:id id :status status :parts (vec parts)})
(defn area [id [a b] want] {:id id :box [a b] :want want})

(defn pairs [result] (mapv :plans result))

(deftest plans-that-do-not-touch-conflict-with-nothing
  (is (= [] (conflicts/conflicts {"a" (expansion [[0 64 0] "stone"]) "b" (expansion [[1 64 0] "dirt"])})))
  (is (= [] (conflicts/conflicts {})))
  (is (= [] (conflicts/conflicts {"a" (expansion [[0 64 0] "stone"])}))))

(deftest the-same-want-on-a-shared-cell_is_no_conflict
  (are [wa wb] (= [] (conflicts/conflicts {"a" (expansion [[0 64 0] wa]) "b" (expansion [[0 64 0] wb])}))
    "stone" "stone"
    "stone" {:block "stone"}
    {:crop "wheat"} {:crop "wheat"}
    :clear :clear
    [:any "stone" "dirt"] "dirt"
    [:any "stone" "dirt"] [:any "dirt" "sand"]))

(deftest different-wants-on-a-shared-cell_conflict
  (are [wa wb] (= [["a" "b"]] (pairs (conflicts/conflicts {"a" (expansion [[0 64 0] wa]) "b" (expansion [[0 64 0] wb])})))
    "stone" "dirt"
    "stone" :clear
    {:crop "wheat"} "wheat"
    {:block "oak_door" :facing :north} {:block "oak_door" :facing :south}
    {:block "oak_door" :facing :north} "oak_door"
    [:any "stone" "dirt"] "sand"))

(deftest a-pair-reports-count-box-cells-and-agreeing-cells
  (let [a (expansion [[0 64 0] "stone"] [[5 64 2] "stone"] [[9 64 9] "stone"] [[3 64 3] "stone"])
        b (expansion [[0 64 0] "dirt"] [[5 64 2] "dirt"] [[9 64 9] "stone"] [[7 64 7] "dirt"])]
    (is (= [{:plans ["a" "b"] :count 2 :same 1
             :box {:min [0 64 0] :max [5 64 2]}
             :cells [[0 64 0] [5 64 2]]}]
           (conflicts/conflicts {"b" b "a" a})))))

(deftest three-plans-on-one-cell-give-a-pair-each
  (let [at (fn [want] (expansion [[0 64 0] want]))]
    (is (= [["a" "b"] ["a" "c"] ["b" "c"]]
           (pairs (conflicts/conflicts {"a" (at "stone") "b" (at "dirt") "c" (at "sand")}))))
    (is (= [["a" "b"] ["a" "c"]]
           (pairs (conflicts/conflicts {"a" (at "stone") "b" (at "dirt") "c" (at "dirt")}))))))

(deftest the-worst-pair-comes-first
  (let [big (expansion [[0 64 0] "stone"] [[1 64 0] "stone"] [[2 64 0] "stone"])
        other (expansion [[0 64 0] "dirt"] [[1 64 0] "dirt"] [[2 64 0] "dirt"])
        one (expansion [[0 64 0] "sand"])]
    (is (= [["a" "b"] ["a" "c"] ["b" "c"]] (pairs (conflicts/conflicts {"a" big "b" other "c" one}))))
    (is (= [3 1 1] (map :count (conflicts/conflicts {"a" big "b" other "c" one}))))))

(def stone-field (area "field" [[0 64 0] [3 64 3]] "stone"))
(def dirt-field (area "field" [[2 64 2] [5 64 5]] "dirt"))

(deftest only-active-plans-count
  (let [conflicting (fn [status-b] (conflicts/active-conflicts {"a" (plan "a" :active stone-field) "b" (plan "b" status-b dirt-field)} {}))]
    (is (= [["a" "b"]] (pairs (conflicting :active))))
    (is (= 4 (:count (first (conflicting :active)))))
    (is (= [] (conflicting :proposed)))
    (is (= [] (conflicting :retired)))))

(deftest a-part-of-a-plan-holds-only-the-cells-it-won
  (let [a (plan "a" :active (area "base" [[0 64 0] [3 64 0]] "stone") (area "patch" [[0 64 0] [1 64 0]] "dirt"))
        b (plan "b" :active (area "row" [[0 64 0] [3 64 0]] "dirt"))]
    ;; the later part of a wins the first two cells: they agree with b, the other two are stone against dirt
    (is (= [{:plans ["a" "b"] :count 2 :same 2 :box {:min [2 64 0] :max [3 64 0]} :cells [[2 64 0] [3 64 0]]}]
           (conflicts/active-conflicts {"a" a "b" b} {})))))

(deftest a-blueprint-part-conflicts-through-its-placed-cells
  (let [hut {:id "hut" :front :south :key {"S" "cobblestone"} :layers [["SS"]]}
        a (plan "a" :active {:id "h" :blueprint "hut" :at [0 64 0]})
        b (plan "b" :active (area "yard" [[1 64 0] [1 64 0]] :clear))]
    (is (= [["a" "b"]] (pairs (conflicts/active-conflicts {"a" a "b" b} {"hut" hut}))))))

(deftest tens-of-thousands-of-cells-stay-cheap
  (let [result (conflicts/conflicts {"a" (shape/expand (plan "a" :active (area "x" [[0 64 0] [199 64 199]] "stone")) {})
                                     "b" (shape/expand (plan "b" :active (area "x" [[100 64 100] [299 64 299]] "dirt")) {})})]
    (is (= 10000 (:count (first result))))
    (is (= {:min [100 64 100] :max [199 64 199]} (:box (first result))))))

(deftest the-plan-view-lists-each-side-with-the-other-plan
  (let [result (conflicts/conflicts {"a" (expansion [[0 64 0] "stone"] [[0 64 1] "stone"])
                                     "b" (expansion [[0 64 0] "dirt"] [[0 64 1] "dirt"])})]
    (is (= {"a" [{:with "b" :count 2 :box {:min [0 64 0] :max [0 64 1]}}]
            "b" [{:with "a" :count 2 :box {:min [0 64 0] :max [0 64 1]}}]}
           (conflicts/per-plan result)))))
