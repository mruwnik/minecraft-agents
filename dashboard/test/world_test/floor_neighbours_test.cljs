(ns world-test.floor-neighbours-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.floor-neighbours :as w]))

(defn column
  "A column, y -> name: stone up to top, air over it, water from wet-from up to top when given."
  ([top] (column top nil))
  ([top wet-from] (fn [y] (cond (> y top) "air" (and wet-from (>= y wet-from)) "water" :else "stone"))))

(def lit (constantly 15))

(deftest a-step-onto-a-neighbour-counts-by-its-kind-shallowest-first
  (is (= {[:level :lit] {:present 1}} (w/tally (column 63) lit [(column 63)])) "the next floor at the stand's own height")
  (is (= {[:up :lit] {:present 1}} (w/tally (column 63) lit [(column 64)])))
  (is (= {[:level :lit] {:absent 1} [:down :lit] {:present 1}} (w/tally (column 63) lit [(column 62)]))
      "a floor one lower is a level step's floor not there and a down step's floor there")
  (is (= {[:level :lit] {:absent 1} [:down :lit] {:absent 1} [:drop :lit] {:absent 2}} (w/tally (column 63) lit [(column 40)]))
      "a void: no floor in any step's reach")
  (is (= {[:level :lit] {:absent 1} [:down :lit] {:absent 1}} (w/tally (column 63) lit [(column 62 62)])) "water is no floor"))

(deftest a-stand-under-no-sky-counts-dark-and-one-with-no-floor-or-room-counts-nothing
  (is (= {[:level :dark] {:present 1}} (w/tally (column 63) (constantly 0) [(column 63)])))
  (is (= {} (w/tally (column 63 63) lit [(column 63)])) "a stand on water")
  (is (= {} (w/tally (column 63) lit [(column 66)])) "a wall: no step"))

(deftest the-prior-is-fitted-over-chunks-and-pooled-without-spread
  (let [r (w/report [{[:level :lit] {:absent 1 :present 9}} {[:level :lit] {:absent 3 :present 7}}] 1)]
    (is (= {:absent 4 :present 16 :chunks 2 :p-absent 0.2} (select-keys (get r [:level :lit]) [:absent :present :chunks :p-absent])))
    (is (> 1e-9 (js/Math.abs (- 0.2 (let [[a b] (:prior (get r [:level :lit]))] (/ a (+ a b)))))) "the fit keeps the mean"))
  (is (= [4 16] (:prior (get (w/report [{[:up :dark] {:absent 2 :present 8}} {[:up :dark] {:absent 2 :present 8}}] 1) [:up :dark])))
      "chunks that agree: their pooled counts"))
