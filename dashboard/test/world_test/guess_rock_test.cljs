(ns world-test.guess-rock-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.guess-rock :as w]))

(defn world
  "A reader of stone with bedrock at y -64, air above y -61 up to a chest at [0 10 0] on stone at 9."
  [x y z]
  (cond (< y -64) nil
        (= y -64) "bedrock"
        (> y 10) "air"
        (= y 10) (when (= 0 x z) "chest")
        (> y -61) (if (< y 9) nil "stone")
        :else "stone"))

(deftest a-solid-cell-counts-by-its-guess-class
  (is (= {:layer-beside {:all 18 :bad 9} :layer-apart {:all 9 :bad 0}}
         (w/tally world 0 2 -64 -62 0 2))
      "bedrock at -64 and the stone on it (3x3 each) have bedrock beside, and only the first is rock never dug; stone two up has none")
  (is (= {:placed {:all 1 :bad 0} :apart {:all 1 :bad 1}}
         (w/tally world 0 0 9 10 0 0))
      "above the layer: the stone under the chest is :placed; the chest, with only stone beside, is :apart and itself rock never dug"))

(deftest the-prior-is-fitted-over-sections-and-pooled-without-spread
  (let [r (w/report [{:apart {:all 100 :bad 1}} {:apart {:all 100 :bad 3}}] 1)]
    (is (= {:bad 4 :all 200 :sections 2} (select-keys (:apart r) [:bad :all :sections])))
    (is (> 1e-9 (js/Math.abs (- 0.02 (let [[a b] (:prior (:apart r))] (/ a (+ a b)))))) "the fit keeps the mean"))
  (is (= [4 196] (:prior (:apart (w/report [{:apart {:all 100 :bad 2}} {:apart {:all 100 :bad 2}}] 1)))) "sections that agree: pooled counts"))
