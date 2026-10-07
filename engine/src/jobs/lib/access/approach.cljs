(ns jobs.lib.access.approach
  "Pure planner: where does the body stand to work on target cells (logs, wall cells), or where does it pillar up?
  No sensing, no side effects; never plans a bridge or a gap (the walker finds the way to a stand).

  Input, one map: the rules input of jobs.lib.access.rules (:block-at :zones :footprints :claims :self :now
  :ignore-zones?) plus
    :targets    set of [x y z] cells that must all be within reach of the eye
    :feet       the body's feet cell
    :reach      eye-to-centre distance a target is worked from (the caller's constant, e.g. dig-reach)
    :radius     columns searched around the targets, default 3; widened to max-radius whenever nothing usable is found
    :max-height tallest pillar considered, default 10

  Output, the first that applies:
    {:stand [cell ...]}   cells to walk to (go-to goal set), nearest to the feet first, 3D (a cell 5 up is 5 away)
    {:pillar {:base cell :height n :stand cell}}  base: a ground cell outside zones and footprints; n blocks are placed
                          from the base up (jobs.access.pillar), :stand is the feet cell on top
    {:reason :not-loaded} a target is not loaded
    {:reason :no-base}    no ground to pillar from within the radius
    {:reason :zone}       every base up to max-radius is refused by zones, claims or footprints
    {:reason :no-stand}   ground and permission exist but no column gives a stand (headroom, too tall)

  A :stand cell is not zone-checked: walking there is go-to's business.

  Ranking: walking a block costs 1, placing a block pillar-cost. A stand needs no placement, so any stand wins over a
  pillar."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::default-radius {:default 3 :doc "Blocks round a target an approach cell is searched." :spec (a/int-in 1 nil)}
   ::max-radius {:default 5 :doc "The most that radius may be." :spec (a/int-in 1 nil)}
   ::default-max-height {:default 10 :doc "How high above the body an approach may climb." :spec (a/int-in 1 nil)}})

(defn default-radius [] (settings/get settings ::default-radius))

(defn max-radius [] (settings/get settings ::max-radius))

(defn default-max-height [] (settings/get settings ::default-max-height))

(def pillar-cost
  "Walking cost of placing one block: it takes a jump and a placement, about two steps."
  2)

(defn passable?
  "A known cell the body stands in: air or a plant/snow it overwrites, never a fluid."
  [block-at pos]
  (let [n (block-at pos)]
    (boolean (and n (rules/replaceable n) (not (rules/fluids n))))))

(defn standing-cell?
  "Feet and head cells passable over a solid floor."
  [block-at [x y z]]
  (and (passable? block-at [x y z]) (passable? block-at [x (inc y) z])
       (rules/solid-floor? block-at [x (dec y) z])))

(defn in-reach?
  "Every target within reach of the eye of a body standing at the middle of the feet cell."
  [reach targets [x y z]]
  (let [here {:x (+ x 0.5) :y y :z (+ z 0.5)}]
    (every? #(<= (u/eye-dist here %) reach) targets)))

(defn dist3 [[ax ay az] [bx by bz]]
  (js/Math.hypot (- ax bx) (- ay by) (- az bz)))

(defn columns
  "[x z] columns within radius of the bounding box of targets."
  [targets radius]
  (let [xs (map first targets) zs (map #(nth % 2) targets)]
    (for [x (range (- (apply min xs) radius) (+ (apply max xs) radius 1))
          z (range (- (apply min zs) radius) (+ (apply max zs) radius 1))]
      [x z])))

(defn y-range
  "Feet heights worth looking at: a pillar of up to max-height rises from the lowest of them."
  [targets reach max-height]
  (let [ys (map second targets) r (js/Math.ceil reach)]
    (range (- (apply min ys) r 2 max-height) (+ (apply max ys) 2))))

(defn stands
  "Standing cells with every target in reach, nearest to feet first."
  [{:keys [block-at targets reach feet]} cols ys]
  (->> (for [[x z] cols y ys :let [c [x y z]]
             :when (and (not (contains? targets c)) (standing-cell? block-at c) (in-reach? reach targets c))]
         c)
       (sort-by (juxt #(dist3 feet %) identity))))

(defn column-verdict
  "Where a pillar of height n over base is refused: nil when fine, else the first refusal map. The body's own cells
  are no refusal: the pillar rises under it."
  [in [bx by bz] n]
  (let [block-at (:block-at in)]
    (or (some (fn [y] (let [v (rules/may-place? (assoc in :cell [bx y bz] :feet [bx by bz]))]
                        (when-not (or (:ok v) (= :own-body (:reason v))) v)))
              (range by (+ by n)))
        (when-not (and (passable? block-at [bx (+ by n) bz]) (passable? block-at [bx (+ by n 1) bz]))
          {:ok false :reason :headroom}))))

(defn base-plans
  "Every pillar over a ground cell of cols, as {:base :height :stand :cost :refusal}. A base with no workable height
  carries the refusal of its last try."
  [{:keys [block-at targets reach feet max-height] :as in} cols ys]
  (for [[x z] cols y ys :let [base [x y z]]
        :when (and (not (contains? targets base)) (standing-cell? block-at base))
        :let [tries (for [n (range 1 (inc max-height))
                          :let [stand [x (+ y n) z]]
                          :when (in-reach? reach targets stand)]
                      [n stand (column-verdict in base n)])
              ok (first (filter (comp nil? #(nth % 2)) tries))
              last-try (last tries)]]
    (if ok
      {:base base :height (first ok) :stand (second ok)
       :cost (+ (dist3 feet base) (* pillar-cost (first ok)))}
      {:base base :refusal (some-> last-try (nth 2))})))

(defn plan-at-radius [in radius]
  (let [cols (columns (:targets in) radius)
        ys (y-range (:targets in) (:reach in) (:max-height in))
        found (stands in cols ys)]
    (if (seq found)
      {:stand (vec found)}
      (let [plans (base-plans in cols ys)
            workable (filter :stand plans)]
        (cond
          (seq workable) {:pillar (-> (first (sort-by (juxt :cost :base) workable))
                                      (select-keys [:base :height :stand]))}
          (empty? plans) {:reason :no-base}
          (some #(#{:zone :footprint :claim :no-zones} (:reason (:refusal %))) plans) {:reason :zone}
          :else {:reason :no-stand})))))

(defn plan
  "See the namespace docstring."
  [{:keys [targets block-at radius] :as in}]
  (let [in (assoc in :max-height (or (:max-height in) (default-max-height)))]
    (if (some #(nil? (block-at %)) targets)
      {:reason :not-loaded}
      (loop [r (or radius (default-radius))]
        (let [res (plan-at-radius in r)]
          (if (and (:reason res) (< r (max-radius)))
            (recur (inc r))
            res))))))
