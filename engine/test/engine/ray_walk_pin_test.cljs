(ns engine.ray-walk-pin-test
  "reach/ray-clear? (kind-at) cell walk."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.reach :as reach]))

(def cases
  [[[0.5 64.5 0.5] [5.5 64.5 0.5] #{} #{} true]
   [[0.5 64.5 0.5] [5.5 64.5 0.5] #{[3 64 0]} #{} false]
   [[0.5 64.5 0.5] [5.5 64.5 0.5] #{[3 64 1]} #{} true]
   [[0.5 64.5 0.5] [5.5 64.5 0.5] #{[0 64 0] [5 64 0]} #{} true]
   [[5.5 64.5 0.5] [0.5 64.5 0.5] #{[2 64 0]} #{} false]
   [[0.5 66.4 0.5] [6.5 64.4 0.5] #{[1 66 0]} #{} false]
   ;; exact diagonal: the walk steps x before z at a corner
   [[0.5 64.5 0.5] [4.5 64.5 4.5] #{[1 64 0]} #{} false]
   [[0.5 64.5 0.5] [4.5 64.5 4.5] #{[0 64 1]} #{} true]
   [[0.5 64.5 0.5] [4.5 64.5 4.5] #{[2 64 2]} #{} false]])

(deftest ray-clear-over-kind-at
  (doseq [[from to solid _ clear] cases]
    (is (= clear (reach/ray-clear? (fn [x y z] (if (contains? solid [x y z]) :solid :open)) from to)) (str solid))))
