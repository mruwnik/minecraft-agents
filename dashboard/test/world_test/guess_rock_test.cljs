(ns world-test.guess-rock-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.guess-rock :as w]))

(defn world
  "A reader of a world of stone with bedrock at y 0 and air above y 3."
  [x y z]
  (cond (< y 0) nil
        (zero? y) "bedrock"
        (> y 3) "air"
        :else "stone"))

(deftest a-solid-cell-counts-by-whether-rock-never-dug-touches-it
  (is (= {:beside {:all 18 :bad 9} :apart {:all 9 :bad 0}}
         (w/tally world 0 2 0 2 0 2))
      "the bedrock row and the stone on it (3x3 each) are beside bedrock, and only the first is rock never dug; stone two up is not beside"))

(deftest the-prior-is-fitted-over-sections-and-pooled-without-spread
  (let [r (w/report [{:apart {:all 100 :bad 1}} {:apart {:all 100 :bad 3}}] 1)]
    (is (= {:bad 4 :all 200 :sections 2} (select-keys (:apart r) [:bad :all :sections])))
    (is (> 1e-9 (js/Math.abs (- 0.02 (let [[a b] (:prior (:apart r))] (/ a (+ a b)))))) "the fit keeps the mean"))
  (is (= [4 196] (:prior (:apart (w/report [{:apart {:all 100 :bad 2}} {:apart {:all 100 :bad 2}}] 1)))) "sections that agree: pooled counts"))
