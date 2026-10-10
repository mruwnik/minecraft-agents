(ns engine.go-to-depth-test
  "go-to's :y-margin: blocks past the lowest and highest of body and goal that a walk's search may reach, so a route that
  dips far below both ends (through deep caves) is refused instead of walked; absent: the walks' wide box."
  (:require [cljs.test :refer [deftest is async]]
            [engine.path-preview-test :as pt]
            [engine.test-util :as tu :refer [floor]]))

;; level ground x 0..6 (feet 64), a valley stepping down one block a column to feet 52 and up again, level ground to x 40
(def valley
  (let [heights (concat (repeat 7 63) (range 62 50 -1) (range 52 64) (repeat 17 63))]
    (apply merge (map-indexed (fn [x y] (floor y x -3 x 3)) heights))))

(def goal [40 64 1])

(deftest a-narrow-margin-refuses-the-route-through-the-valley
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (false? (:found (:out (await (pt/preview valley {:pos goal :y-margin 5}))))))))))

(deftest a-margin-that-holds-the-valley-or-none-walks-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (:found (:out (await (pt/preview valley {:pos goal :y-margin 14}))))))
        (is (true? (:found (:out (await (pt/preview valley {:pos goal}))))))))))
