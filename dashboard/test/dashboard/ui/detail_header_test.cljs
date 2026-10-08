(ns dashboard.ui.detail-header-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.ui.detail :as detail]))

(defn walk [h] (tree-seq #(or (vector? %) (seq? %)) seq h))
(defn labels [h] (vec (for [n (walk h) :when (and (vector? n) (keyword? (first n)) (re-find #"\.lbl" (str (first n))))] (second n))))

(def model {:name "Ann" :status :working :goal "test a › b" :goal-wait "waiting: reflex.ended" :goal-age "2s ago"
            :job "get-food" :pos-text "1, 2, 3" :dimension "overworld" :world "claude"})

(deftest the-header-labels-each-fact
  (is (= ["Goal" "Job" "Position" "Dimension" "World"] (labels (detail/header model)))))

(deftest the-header-dims-the-wait-and-keeps-the-goal-text-whole
  (let [h (detail/header model)]
    (is (some #(= "test a › b" %) (walk h)))
    (is (some #(and (vector? %) (re-find #"\.dim" (str (first %))) (= "waiting: reflex.ended" (second %))) (walk h)))))

(deftest the-header-without-a-goal-has-no-goal-label
  (is (= ["Job" "Position" "Dimension" "World"] (labels (detail/header (dissoc model :goal :goal-wait :goal-age))))))
