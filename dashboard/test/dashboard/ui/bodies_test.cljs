(ns dashboard.ui.bodies-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.bodies :as bodies]))

(defn button-of [hiccup] (second (nth hiccup 2)))

(deftest show-on-map-is-enabled-with-a-position
  (let [props (button-of (bodies/show-on-map "Ann" {:x 1 :z 2} "claude"))]
    (is (false? (:disabled props)))
    (is (= "show Ann on the map" (:title props)))))

(deftest show-on-map-is-disabled-with-a-reason-without-a-position
  (let [props (button-of (bodies/show-on-map "Ann" nil "claude"))]
    (is (true? (:disabled props)))
    (is (= "no known position yet" (:title props)))))

(defn walk [h] (tree-seq #(or (vector? %) (seq? %)) seq h))
(defn labels [h] (vec (for [n (walk h) :when (and (vector? n) (keyword? (first n)) (re-find #"\.lbl" (str (first n))))] (second n))))
(defn text-of [h] (apply str (filter string? (walk h))))

(deftest the-card-goal-line-labels-the-goal-and-dims-the-wait
  (let [h (bodies/goal-line {:goal "test a › b" :goal-wait "waiting: reflex.ended" :goal-age "2s ago"})]
    (is (= ["Goal"] (labels h)))
    (is (some #(= "test a › b" %) (walk h)))
    (is (some #(and (vector? %) (re-find #"\.dim" (str (first %))) (= "waiting: reflex.ended" (second %))) (walk h))))
  (is (nil? (bodies/goal-line {}))))

(deftest the-card-job-line-labels-the-job
  (is (= ["Job"] (labels (bodies/job-line {:job "get-food"}))))
  (is (some #(= "no job" %) (walk (bodies/job-line {})))))
