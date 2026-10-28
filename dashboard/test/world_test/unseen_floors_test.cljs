(ns world-test.unseen-floors-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.unseen-floors :as u]))

(defn line [data] (pr-str {:kind :walk.unseen-floor :data data}))

(deftest an-unseen-floor-looked-at-counts-present-or-absent-a-missed-one-absent
  (is (= :missed (u/scan-line (line {:floor :missed :cell [0 63 0]}))))
  (is (nil? (u/scan-line (pr-str {:kind :walk.gates :data {:floor :present}}))))
  (is (= {:absent 2 :present 1} (u/report [:present :absent :missed]))))
