(ns engine.approach-test
  "jobs.lib.access.approach: where to stand, or pillar, to reach target cells. Pure, over tiny snapshots."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.util]
            [jobs.lib.access.approach :as approach]))

(defn world
  "Stone up to and including y 63 (and up to ground-y at the given columns), air above, plus extra cells
  (alternating cell and name)."
  [columns & cells]
  (let [m (apply hash-map cells)]
    (fn [[x y z :as pos]]
      (or (get m pos) (if (<= y (get columns [x z] 63)) "stone" "air")))))

(def foreign-zone {:name "z" :owner "other" :allow #{} :min [-20 0 -20] :max [20 100 20]})

(defn plan [& {:as in}]
  (approach/plan (merge {:targets #{[0 72 0]} :feet [3 64 0] :block-at (world {}) :reach 4.2
                         :zones [] :footprints #{} :self "me"}
                        in)))

(deftest a-target-in-reach-gets-stand-cells-no-pillar
  (let [r (plan :targets #{[0 66 0]})]
    (is (nil? (:pillar r)))
    (is (= [3 64 0] (first (:stand r))))
    (is (every? #(<= (jobs.lib.util/eye-dist (zipmap [:x :y :z] (map + % [0.5 0 0.5])) [0 66 0]) 4.2) (:stand r)))))

(deftest stand-cells-are-ranked-by-3d-distance-from-the-feet
  (let [feet [0 64 3]
        r (plan :targets #{[0 66 0]} :feet feet :block-at (world {[0 -1] 65}))
        d (fn [c] (jobs.lib.util/dist (zipmap [:x :y :z] c) (zipmap [:x :y :z] feet)))]
    (is (= (:stand r) (sort-by d (:stand r))))
    (is (some #{[0 66 -1]} (:stand r)))))

(deftest a-high-log-gets-the-nearest-column-and-the-minimal-height
  (is (= {:base [1 64 0] :height 3 :stand [1 67 0]} (:pillar (plan)))))

(deftest the-height-is-from-the-highest-target
  (is (= 4 (:height (:pillar (plan :targets #{[0 72 0] [0 73 0]}))))))

(deftest the-pillar-never-stands-in-the-tree-column
  (let [logs (into #{} (map (fn [y] [0 y 0])) (range 64 73))]
    (is (not= [0 64 0] (:base (:pillar (plan :targets logs :feet [0 64 0] :block-at (world {} [0 64 0] "oak_log"))))))))

(deftest a-zoned-base-gives-way-to-the-next
  (let [zone {:name "z" :owner "other" :allow #{} :min [1 0 -5] :max [1 100 5]}
        p (:pillar (plan :zones [zone]))]
    (is (some? p))
    (is (not= 1 (first (:base p))))))

(deftest a-footprint-cell-in-the-column-refuses-that-base
  (let [p (:pillar (plan :footprints #{[1 65 0]}))]
    (is (not= [1 64 0] (:base p)))))

(are [in reason] (= {:reason reason} (apply plan (mapcat identity in)))
  {:zones [foreign-zone]} :zone
  {:block-at (constantly "air")} :no-base
  {:block-at (fn [p] (when-not (= p [0 72 0]) "air"))} :not-loaded
  {:max-height 2} :no-stand)
