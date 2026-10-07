(ns engine.ray-walk-pin-test
  "perception/line-clear? (sight table, unloaded cells) and reach/ray-clear? (kind-at) share one cell walk."
  (:require [cljs.test :refer [deftest is are]]
            [engine.perception :as perception]
            [jobs.lib.reach :as reach]))

(defn raw-with
  "A raw world over a sight table where state 1 blocks and 0 does not; cells in `solid` are 1, in `unloaded` -1."
  [solid unloaded]
  #js {:stateAt (fn [x y z] (cond (contains? unloaded [x y z]) -1 (contains? solid [x y z]) 1 :else 0))
       :lightAt (fn [_ _ _] 15)})

(def table #js [0 1])

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

(deftest line-clear-over-a-sight-table
  (doseq [[[ox oy oz] [tx ty tz] solid _ clear] cases]
    (is (= clear (perception/line-clear? (raw-with solid #{}) table ox oy oz tx ty tz)) (str solid))))

(deftest line-clear-unloaded-cell-blocks
  (is (false? (perception/line-clear? (raw-with #{} #{[3 64 0]}) table 0.5 64.5 0.5 5.5 64.5 0.5)))
  (is (true? (perception/line-clear? (raw-with #{} #{[5 64 0]}) table 0.5 64.5 0.5 5.5 64.5 0.5))))

(deftest ray-clear-over-kind-at
  (doseq [[from to solid _ clear] cases]
    (is (= clear (reach/ray-clear? (fn [x y z] (if (contains? solid [x y z]) :solid :open)) from to)) (str solid))))
