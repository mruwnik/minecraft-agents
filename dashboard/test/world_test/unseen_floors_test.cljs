(ns world-test.unseen-floors-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.unseen-floors :as u]))

(defn line [data] (pr-str {:kind :walk.unseen-floor :data data}))

(deftest an-unseen-floor-looked-at-counts-present-or-absent-by-its-kind-a-missed-one-absent
  (is (= [:down :missed] (u/scan-line (line {:floor :missed :step-kind :down :cell [0 63 0]}))))
  (is (nil? (u/scan-line (pr-str {:kind :walk.gates :data {:floor :present}}))))
  (is (= {:level {:absent 2 :present 1} :down {:absent 0 :present 1}}
         (u/report [[:level :present] [:level :absent] [:level :missed] [:down :present]]))))
