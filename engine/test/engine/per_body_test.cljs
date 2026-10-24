(ns engine.per-body-test
  (:require [cljs.test :refer [deftest is]]
            [engine.core.base :as base]
            [jobs.lib.walk.search :as wsearch]))

(deftest a-warm-start-drops-what-is-kept-per-body
  (reset! wsearch/known-land {:a {:cells (js/Set. #js [1])}})
  (reset! wsearch/goal-floods {:a {}})
  (reset! wsearch/searches {:a {:k {}}})
  (base/warm-start!)
  (is (= [{} {} {}] [@wsearch/known-land @wsearch/goal-floods @wsearch/searches])))
