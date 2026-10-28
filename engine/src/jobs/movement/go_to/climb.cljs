(ns jobs.movement.go-to.climb
  "go-to's way up for a body under ground whose goal lies far above it: the stairs it cut itself (jobs.lib.ledger/ways-of)
  that lead to the goal's level or the surface before any other way, and no way down through caves."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.util :as u]
            [jobs.lib.column-kinds :as kinds]
            [jobs.lib.places :as places]
            [jobs.lib.cost :as cost]
            [jobs.movement.go-to.column :as column]))

(a/defargs settings
  {::cave-dip {:default 3 :doc "Blocks below where it began that a go-to of a body under ground to a goal far above (jobs.movement.go-to.tuning/climb-rise) walks over cells that are no stair of its own: a plan that goes deeper is not walked while a stair up can be cut instead." :spec (a/num-in 0 nil)}
   ::own-way {:default true :doc "Whether a go-to of a body under ground to a goal far above (jobs.movement.go-to.tuning/climb-rise) first climbs the stairs the body cut itself, and cuts a blocked one again." :spec boolean?}
   ::own-way-reach {:default 14 :doc "Blocks of its own way up (::own-way) a go-to walks in one hop." :spec (a/num-in 1 nil)}
   ::own-way-digs {:default 2 :doc "Cells not seen open (rock, or never seen) on the straight line to a stand of its own stair within ::own-way-reach past which a go-to does not take that stand as the start of its own way." :spec (a/int-in 0 nil)}
   ::own-way-search {:default 4000 :doc "Cells a go-to looks over for a way up its own stairs (::own-way): its stairs and tunnels and the standable cells it has seen." :spec (a/int-in 1 nil)}})
(defn cave-dip [] (settings/get settings ::cave-dip))
(defn own-way [] (settings/get settings ::own-way))
(defn own-way-reach [] (settings/get settings ::own-way-reach))
(defn own-way-search [] (settings/get settings ::own-way-search))
(defn own-way-digs [] (settings/get settings ::own-way-digs))

(defn dives?
  "Whether a plan's steps ({:x :y :z} feet) go below floor on a cell that stands on none of ways (stair floors [x y z])."
  [steps floor ways]
  (boolean (some (fn [{:keys [x y z]}]
                   (let [[x y z] (map #(js/Math.floor %) [x y z])]
                     (and (< y floor) (not (contains? ways [x (dec y) z])))))
                 steps)))

(defn returns?
  "Whether a plan's steps ({:x :y :z} feet) go below floor on a stand of the body's own stairs (stands, feet cells)."
  [steps floor stands]
  (boolean (some (fn [{:keys [x y z]}]
                   (let [cell (mapv #(js/Math.floor %) [x y z])]
                     (and (< (second cell) floor) (contains? stands cell))))
                 steps)))

(defn route
  "The shortest walk from start (a feet cell [x y z]) to the nearest cell end? holds of, by steps of one block along and
  at most one up or down over cells pass? holds of, or first straight to one of seeds: the cells, start first; nil when
  none is met within limit cells."
  [start seeds pass? end? limit]
  (loop [queue (into (conj cljs.core/PersistentQueue.EMPTY start) seeds)
         from (into {start nil} (map #(vector % start)) (remove #{start} seeds))]
    (when-let [[x y z :as cell] (peek queue)]
      (cond
        (end? cell) (vec (reverse (take-while some? (iterate from cell))))
        (> (count from) limit) nil
        :else
        (let [nexts (for [[dx dz] escape/cardinals dy [1 0 -1]
                          :let [n [(+ x dx) (+ y dy) (+ z dz)]]
                          :when (and (not (contains? from n)) (pass? n))]
                      n)]
          (recur (into (pop queue) nexts) (into from (map #(vector % cell)) nexts)))))))

(defn rock-between
  "The cells on the straight line from feet cell a to feet cell b (the feet and head cell at each half block) that
  kind-at does not read free (kinds/free?): rock, or never seen, a walk straight there digs."
  [kind-at [ax ay az] [bx by bz]]
  (let [n (* 2 (max (abs (- bx ax)) (abs (- by ay)) (abs (- bz az))))
        at (fn [a b i] (js/Math.floor (+ a 0.5 (* (/ i (max n 1)) (- b a)))))]
    (count (into #{} (comp (mapcat (fn [i] (let [x (at ax bx i) y (js/Math.floor (+ ay (* (/ i (max n 1)) (- by ay)))) z (at az bz i)]
                                             [[x y z] [x (inc y) z]])))
                           (remove (fn [[x y z]] (kinds/free? (kind-at x y z)))))
                 (range (inc n))))))

(defn own-route
  "The walk (route) from the body's feet up its own ways to goal height top, or nil: over the stands of its stairs and
  tunnels (jobs.lib.ledger; open or not, a blocked stair is cut again) and the standable cells it has seen no deeper than
  ::cave-dip below the feet, or straight to a stand of a stair of its own within ::own-way-reach with no more than
  ::own-way-digs cells not seen open on the line there (rock-between), to the nearest stand of a stair of its own at top (or one under it), or under the open sky where
  its stair goes no higher (a step part way up one is no end: what shuts the steps above it is on the way). A stair that
  ends in rock below that is no way up."
  [c feet top]
  (let [p (:primitives c)
        stairs (ledger/stair-stands c)
        own (into stairs (ledger/tunnel-stands c))
        kind-at (kinds/seen-kind-at p)
        surface-at (kinds/surface-kind-at p)
        min-y (places/min-y p)
        low (- (second feet) (cave-dip))
        goes-on? (fn [[x y z]] (some (fn [[dx dz]] (contains? stairs [(+ x dx) (inc y) (+ z dz)])) escape/cardinals))]
    (when (seq stairs)
      (route feet
             (filter (fn [[x y z :as cell]] (and (<= (u/dist (u/self-pos c) {:x (+ x 0.5) :y y :z (+ z 0.5)}) (own-way-reach))
                                                 (<= (rock-between kind-at feet cell) (own-way-digs))))
                     stairs)
             (fn [[x y z :as cell]] (or (contains? own cell) (and (>= y low) (column/stands? kind-at x y z))))
             (fn [[x y z :as cell]]
               (and (contains? stairs cell)
                    (or (>= y (dec top))
                        (and (not (goes-on? cell)) (keyword-identical? :open (kind-at x (+ y 2) z)) (= y (kinds/top surface-at x z min-y kinds/top-y))))))
             (own-way-search)))))

(defn hop
  "The next cell of route (cells, the body's first) to walk to from from, a map with :floor, the lowest y on the way
  there: the last of its leading cells whose centre is within reach blocks, short of the first that blocked? holds of
  (that one itself when it is the next cell). nil for a route of one cell."
  [route from reach blocked?]
  (let [near (take-while (fn [[x y z]] (<= (u/dist from {:x (+ x 0.5) :y y :z (+ z 0.5)}) reach)) (rest route))
        walk (or (seq (take-while (complement blocked?) near)) (take 1 (rest route)))]
    (when-let [[x y z] (last walk)]
      {:x x :y y :z z :floor (apply min (map second (cons (first route) walk)))})))

(defn weigh
  "Walking route (cells) against cutting a new stair of rise steps at step-s seconds each: {:take :to :blocks :walk-s
  :cut-steps :cut-s :text}, :take whether the walk is no slower."
  [route rise step-s]
  (let [blocks (dec (count route))
        walk-s (js/Math.round (/ blocks cost/walk-speed))
        cut-s (* rise step-s)
        take (<= walk-s cut-s)]
    {:take take :to (peek route) :blocks blocks :walk-s walk-s :cut-steps rise :cut-s cut-s
     :text (str "its own way up is " blocks " blocks (about " walk-s " s) to its stair at " (pr-str (peek route))
                "; a new stair is " rise " steps (about " cut-s " s): " (if take "walking its own way" "cutting a new stair"))}))

(defn recut
  "For a body at feet (a cell) with floors of ways (stair floors [x y z]) running up from it along a heading, one up and
  one along each: {:step :stair :heading kw :steps n :own true}, n the floors of the run; else nil."
  [[fx fy fz] ways]
  (some (fn [[dx dz :as d]]
          (let [n (count (take-while #(contains? ways [(+ fx (* % dx)) (+ (dec fy) %) (+ fz (* % dz))]) (iterate inc 1)))]
            (when (pos? n) {:step :stair :heading (escape/heading-names d) :steps n :own true})))
        escape/cardinals))
