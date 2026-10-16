(ns engine.heap-test
  (:require [cljs.test :refer [deftest is]]
            [engine.heap :as heap]
            [jobs.lib.walk.search :as wsearch]))

(deftest the-sample-reads-the-heap-and-the-planner-stores
  (reset! wsearch/known-land {:a {:cells (js/Set. #js [1 2 3]) :edges (js/Set. #js [1])} :b {:cells (js/Set. #js [4]) :edges (js/Set.)}})
  (reset! wsearch/goal-floods {:a {}})
  (reset! wsearch/searches {:a {} :b {}})
  (let [s (heap/sample)]
    (is (every? number? (map s [:heap-used :heap-limit :rss :external :array-buffers])))
    (is (< (:heap-used s) (:heap-limit s)))
    (is (= {:known-cells 4 :known-edges 1 :goal-floods 1 :searches 2} (select-keys s [:known-cells :known-edges :goal-floods :searches])))))
