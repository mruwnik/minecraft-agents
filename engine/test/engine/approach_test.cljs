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

(deftest avoided-cells-are-neither-stand-nor-base
  (let [r (plan :targets #{[0 66 0]})
        r2 (plan :targets #{[0 66 0]} :avoid #{(first (:stand r))})
        p (plan :avoid #{[1 64 0]})]
    (is (not-any? #{(first (:stand r))} (:stand r2)))
    (is (seq (:stand r2)))
    (is (not= [1 64 0] (:base (:pillar p))))))

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

(def inner-zone {:name "z" :owner "other" :allow #{} :min [-3 0 -3] :max [3 100 3]})

(deftest a-zone-over-radius-3-widens-to-a-free-base
  (let [p (:pillar (plan :zones [inner-zone]))]
    (is (some? p))
    (is (= 4 (apply max (map #(js/Math.abs %) [(first (:base p)) (nth (:base p) 2)]))))))

(deftest a-zone-over-radius-3-widens-to-a-stand-at-4
  (is (= [[4 68 0]] (:stand (plan :targets #{[0 70 0]} :feet [4 68 0] :zones [inner-zone]
                                  :block-at (world {[4 0] 67}))))))

(deftest a-claim-refuses-like-a-zone
  (let [claim {:id "c" :owner "other" :status :active :until 100 :min [-20 0 -20] :max [20 100 20]}]
    (is (= {:reason :zone} (plan :claims [claim] :now 0)))))

(deftest a-claim-over-radius-3-widens-to-a-free-base
  (let [claim {:id "c" :owner "other" :status :active :until 100 :min [-3 0 -3] :max [3 100 3]}]
    (is (some? (:pillar (plan :claims [claim] :now 0))))))

(deftest an-unseen-floor-is-no-stand
  (let [guessed (world {[1 0] 66})
        seen (fn [[_ y _ :as p]] (when (<= y 63) (guessed p)))
        r (plan :block-at guessed :floor-at seen)]
    (is (nil? (:stand r)))
    (is (some? (:pillar r)))))

(deftest a-leaf-is-no-floor-to-stand-on
  (let [r (plan :targets #{[0 72 0]} :feet [1 70 0]
                :block-at (world {} [1 70 0] "oak_leaves" [1 71 0] "air"))]
    (is (not-any? #(= [1 71 0] %) (:stand r)))
    (is (not= [1 71 0] (get-in r [:pillar :stand])))
    (is (= 64 (second (get-in r [:pillar :base]))))))

(def gap
  "Ground at y 63 under the feet cell only; the row east at z 0 is over a pit down to y 62."
  (world (into {} (for [x (range 1 60)] [[x 0] 62]))))

(defn bridge [& {:as in}]
  (approach/bridge (merge {:targets #{[8 66 0]} :feet [0 64 0] :block-at gap :reach 4.2
                           :zones [] :footprints #{} :self "me"}
                          in)
                   nil))

(deftest a-bridge-row-is-the-shortest-that-brings-the-target-in-reach
  (let [r (bridge)]
    (is (= :east (:heading r)))
    (is (<= 1 (:length r) 5))
    (is (= [(:length r) 64 0] (:stand r)))))

(deftest a-bridge-row-is-refused-over-a-footprint
  (is (nil? (bridge :footprints #{[2 63 0]}))))

(deftest a-bridge-row-longer-than-max-row-is-not-planned
  (is (nil? (bridge :targets #{[40 66 0]} :max-row 5))))

(deftest a-wall-top-the-zone-bars-digging-is-no-stand-and-no-base
  (let [wall (for [x [1 2] y [64 65 66]] [[x y 0] "oak_log"])
        zone {:name "yard" :owner "other" :allow #{:place} :min [1 64 0] :max [2 66 0]}
        r (plan :targets (set (for [y (range 68 74)] [0 y 0])) :feet [1 67 0] :zones [zone]
                :block-at (apply world {} (mapcat identity (concat wall (for [y (range 64 74)] [[0 y 0] "oak_log"])))))]
    (is (nil? (:stand r)))
    (is (not-any? #{[1 67 0] [2 67 0]} [(:base (:pillar r))]))))
