(ns engine.path.planner.dark
  "Search methods: the cost of walking in the dark (options.dark, see engine.path.planner-tuned)."
  (:require [engine.path.planner.base :refer [TABLE]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(extend-type Search
  Object

  ;; 1 when the caller's test (options.dark.at) calls the cell x,y,z dark, else 0. The test runs about once per cell:
  ;; its answer is kept in a direct-mapped cache (the key stored +1 so zeroed memory reads as empty).
  (darkOf [s x y z]
    (let [key (inc (.keyOf s x y z 0))
          slot (bit-and (.hashOf s x y z 0) (dec TABLE))]
      (if (== (aget (.-dark-keys s) slot) key)
        (- (aget (.-dark-flags s) slot) 1)
        (let [dark (if (== (.call ^js (.-dark-at s) nil x y z) 1) 1 0)]
          (aset (.-dark-keys s) slot key)
          (aset (.-dark-flags s) slot (inc dark))
          dark)))))
