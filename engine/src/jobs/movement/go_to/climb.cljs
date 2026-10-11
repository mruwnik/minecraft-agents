(ns jobs.movement.go-to.climb
  "go-to's way up for a body under ground whose goal lies far above it: the stairs it cut itself (jobs.lib.ledger/ways-of)
  before any other way, and no way down through caves."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.escape :as escape]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::cave-dip {:default 3 :doc "Blocks below where it began that a go-to of a body under ground to a goal far above (jobs.movement.go-to/climb-rise) walks over cells that are no stair of its own: a plan that goes deeper is not walked while a stair up can be cut instead." :spec (a/num-in 0 nil)}
   ::own-way {:default true :doc "Whether a go-to of a body under ground to a goal far above (jobs.movement.go-to/climb-rise) first climbs the stairs the body cut itself, and cuts a blocked one again." :spec boolean?}
   ::own-way-reach {:default 14 :doc "Blocks within which a floor of a stair the body cut is a way up for its go-to (::own-way)." :spec (a/num-in 1 nil)}})
(defn cave-dip [] (settings/get settings ::cave-dip))
(defn own-way [] (settings/get settings ::own-way))
(defn own-way-reach [] (settings/get settings ::own-way-reach))

(defn dives?
  "Whether a plan's steps ({:x :y :z} feet) go below floor on a cell that stands on none of ways (stair floors [x y z])."
  [steps floor ways]
  (boolean (some (fn [{:keys [x y z]}]
                   (let [[x y z] (map #(js/Math.floor %) [x y z])]
                     (and (< y floor) (not (contains? ways [x (dec y) z])))))
                 steps)))

(defn way-up
  "The feet cell, a map, of the highest of ways (stair floors [x y z]) above from (the body) and no higher than top whose
  centre is within reach blocks, the nearest of those; nil when none."
  [from ways reach top]
  (->> (for [[x y z] ways
             :let [centre {:x (+ x 0.5) :y (inc y) :z (+ z 0.5)}]
             :when (and (> (inc y) (+ (:y from) 0.5)) (<= (inc y) top) (<= (u/dist from centre) reach))]
         [{:x x :y (inc y) :z z} (u/dist from centre)])
       (sort-by (juxt (comp - :y first) second))
       ffirst))

(defn recut
  "For a body at feet (a cell) with floors of ways (stair floors [x y z]) running up from it along a heading, one up and
  one along each: {:step :stair :heading kw :steps n :own true}, n the floors of the run; else nil."
  [[fx fy fz] ways]
  (some (fn [[dx dz :as d]]
          (let [n (count (take-while #(contains? ways [(+ fx (* % dx)) (+ (dec fy) %) (+ fz (* % dz))]) (iterate inc 1)))]
            (when (pos? n) {:step :stair :heading (escape/heading-names d) :steps n :own true})))
        escape/cardinals))
