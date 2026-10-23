(ns engine.path.planner.dark
  "Search methods: the cost of walking in the dark (options.dark, see engine.path.planner-tuned)."
  (:require [engine.path.planner.base :refer [TABLE]]
            [engine.path.planner.search :refer [Search]]))

(set! *warn-on-infer* true)

(extend-type Search
  Object

  ;; The extra cost of the cell x,y,z as a share of its own seconds: that of the class the caller's test
  ;; (options.dark.at) gives it (0 lit; one it does not price counts lit). The test runs about once per cell: its class
  ;; is kept in a direct-mapped cache (stored +1 so zeroed memory reads as empty).
  (darkOf [s x y z]
    (let [key (inc (.keyOf s x y z 0))
          slot (bit-and (.hashOf s x y z 0) (dec TABLE))
          ^js shares (.-dark-shares s)]
      (if (== (aget (.-dark-keys s) slot) key)
        (aget shares (- (aget (.-dark-flags s) slot) 1))
        (let [c (.call ^js (.-dark-at s) nil x y z)
              c (if (and (>= c 0) (< c (.-length shares))) c 0)]
          (aset (.-dark-keys s) slot key)
          (aset (.-dark-flags s) slot (inc c))
          (aget shares c))))))
