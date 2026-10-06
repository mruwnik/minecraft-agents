(ns jobs.lib.cost.danger
  "Danger, in two forms: route-danger, the expected damage of walking a route past mobs (recover-drops' fetch cost), and
  danger-rate / danger-list, the hp a second go-to's planner costs near a known danger (its options.dangers).

  The mobs are an input: what the body has seen or remembers ({:name :pos} per mob; JS entities are read too), e.g.
  jobs.lib.reach/known-hostiles. Nothing here reads the world's entities, so an unseen creeper never counts. The
  world is read only through kind-at (jobs.lib.reach/lookup), the caller's.

  route-danger, per mob:
    hostile?  jobs.lib.cost.threat/hostile?; anything else is no danger unless an override names it. Provoked-only mobs
              (endermen, zombified piglins) x0.1.
    threat    jobs.lib.cost.threat/mob-hurt over 3 s of exposure (a creeper: one blast), after armour.
    weight    by the closest route cell: 1 within 2 blocks, falling to 0 at :radius (16).
    reach     a melee mob (creepers too) needs a walkable way to that cell (jobs.lib.reach). A ranged mob needs a
              line of fire to one of the route cells near it. A mob behind walls is no danger.
  danger = hurt x weight, when the mob can reach the route; else 0.
  Overrides {mob-name number-or-{:times n}}: a number is the mob's threat before armour, {:times n} multiplies it.

  danger-rate: mob-hurt over 1 s, times the stance factor (what the hostile reflex would do: :flee x4, :fight x0.1),
  times 20 / health (at most 4), at most max-rate. Body {:health :equipment :weapon :pos}."
  (:require [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [engine.game :as game]))

;; ---------------------------------------------------------------- reading inputs

(defn mob-of
  "{:name :pos {:x :y :z} :kind} of a mob given as a cljs map or a JS entity."
  [m]
  (if (map? m)
    (select-keys m [:name :pos :kind :id])
    (let [pos (.-pos m)]
      {:name (.-name m) :kind (.-kind m) :id (.-id m) :pos (when pos {:x (.-x pos) :y (.-y pos) :z (.-z pos)})})))

(defn override-fn [o]
  (cond (number? o) (constantly o)
        (map? o) (let [t (or (:times o) (get o "times"))] (if (number? t) #(* t %) identity))
        :else identity))

;; ---------------------------------------------------------------- reach

(def max-fire-checks "Route cells a ranged mob's line of fire is tried to, nearest first." 8)

(defn dist [a b] (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn centre [{:keys [x y z]}] {:x (+ (js/Math.floor x) 0.5) :y (js/Math.floor y) :z (+ (js/Math.floor z) 0.5)})

(defn line-clear?
  "Whether no :solid cell of kind-at lies on the segment from a to b ({:x :y :z}), sampled every 0.25 block."
  [kind-at a b]
  (let [d (dist a b)
        n (max 1 (js/Math.ceil (/ d 0.25)))]
    (loop [i 1]
      (if (>= i n)
        true
        (let [t (/ i n)
              x (js/Math.floor (+ (:x a) (* t (- (:x b) (:x a)))))
              y (js/Math.floor (+ (:y a) (* t (- (:y b) (:y a)))))
              z (js/Math.floor (+ (:z a) (* t (- (:z b) (:z a)))))]
          (if (keyword-identical? :solid (kind-at x y z)) false (recur (inc i))))))))

(defn reaches?
  "Whether mob (at pos) can hurt a body on the route: a line of fire to one of the nearest route cells within radius
  for a ranged mob, a walkable way to the nearest cell for any other."
  [kind-at {:keys [name pos]} near-cells]
  (if (= :projectile (threat/damage-type name))
    (let [eye (update pos :y + 1.6)]
      (boolean (some #(line-clear? kind-at eye (update (centre %) :y + 1.5)) (take max-fire-checks near-cells))))
    (let [{:keys [x y z]} (first near-cells)]
      (reach/way? kind-at (reach/cell-of pos) [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))))

;; ---------------------------------------------------------------- route danger

(def default-radius 16)
(def close 2)
(def exposure-s 3)

(defn route-danger
  "{:danger total :mobs [{:name :pos :distance :weight :threat :danger} ...]} of walking route ([{:x :y :z}] cells)
  past mobs (cljs maps {:name :pos} or JS entities) for a body wearing equipment. Lists only mobs that add danger, the
  worst first. kind-at: jobs.lib.reach/lookup. Options: :overrides, :radius (16), :version (minecraft-data, default
  the body's). See the ns doc."
  [kind-at route mobs equipment & {:keys [overrides radius version]}]
  (let [version (or version @game/version)
        radius (or radius default-radius)
        overrides (into {} (map (fn [[k o]] [(if (keyword? k) (name k) (str k)) o])) overrides)
        stats (armour/armour-stats equipment)
        route (vec route)
        rows (when (seq route)
               (for [m (map mob-of mobs)
                     :when (and (string? (:name m)) (:pos m)
                                (or (contains? overrides (:name m)) (threat/hostile? version m)))
                     :let [cells (->> route
                                      (map (fn [c] [(dist (:pos m) (centre c)) c]))
                                      (filter #(<= (first %) radius))
                                      (sort-by first))
                           d (ffirst cells)]
                     :when d
                     :let [weight (if (<= d close) 1 (/ (- radius d) (- radius close)))
                           share (if (contains? threat/provoked-only (:name m)) threat/provoked-share 1)
                           threat-fn #(* share ((override-fn (get overrides (:name m))) %))
                           threat (threat-fn (:threat (threat/base-threat (:name m) exposure-s)))]
                     :when (and (pos? weight) (pos? threat) (reaches? kind-at m (map second cells)))
                     :let [hurt (threat/mob-hurt stats (:name m) exposure-s threat-fn)]]
                 {:name (:name m) :pos (:pos m) :distance d :weight weight :threat threat :danger (* weight hurt)}))
        rows (vec (sort-by :danger > rows))]
    {:danger (reduce + 0 (map :danger rows)) :mobs rows}))

(def snap-span "How far up or down a straight route looks for ground in each column." 4)

(defn straight-route
  "The cells [{:x :y :z}] one a block from the cell of from to the cell of to on a straight line, each put on the
  ground of kind-at nearest the cell before it (within 4 up or down: feet and head free, something under), else at
  that cell's height. A cheap stand-in for a planned route when only the danger along the way is wanted."
  [kind-at from to]
  (let [[fx fy fz] (map js/Math.floor [(:x from) (:y from) (:z from)])
        [tx _ tz] (map js/Math.floor [(:x to) (:y to) (:z to)])
        n (max (js/Math.abs (- tx fx)) (js/Math.abs (- tz fz)))
        ground (fn [x prev z]
                 (or (some #(when (reach/standable? kind-at x % z) %)
                           (cons prev (mapcat (fn [k] [(+ prev k) (- prev k)]) (range 1 (inc snap-span)))))
                     prev))]
    (if (zero? n)
      [{:x fx :y fy :z fz}]
      (loop [i 0 prev fy out []]
        (if (> i n)
          out
          (let [x (+ fx (js/Math.round (/ (* i (- tx fx)) n)))
                z (+ fz (js/Math.round (/ (* i (- tz fz)) n)))
                y (ground x prev z)]
            (recur (inc i) y (conj out {:x x :y y :z z}))))))))

;; ---------------------------------------------------------------- the planner's danger rate

(def max-dangers 8)
(def danger-shape
  "close and radius of a danger's cost (planner options.dangers): full within close blocks, 0 past radius. A remembered
  spot is wider: the mob has moved on from it."
  {:sensed {:close 3 :radius 12} :remembered {:close 3 :radius 16} :creeper {:close 4 :radius 8}})
(def stances "Rate factor: :flee from a mob costs the body far more than one it would :fight." {:flee 4 :fight 0.1})
(def max-rate "hp a second one danger costs at most (the planner caps all of them together at its dangerCap, 4)." 4)
(def reserve "Health a fight must leave (respond-to-hostile's default :reserve)." 4)

(defn stance
  "What the hostile reflex would do about mob-name alone (fight/decide): :fight or :flee."
  [{:keys [health equipment weapon]} mob-name]
  (fight/decide {:health health :reserve reserve :creeper? (= "creeper" mob-name)
                 :damage (fight/fight-damage {:weapon weapon :equipment equipment :mobs [{:name mob-name :distance 0}]})}))

(defn danger-rate
  "hp a second near mob-name costs body {:health :equipment :weapon} (see the ns doc)."
  [{:keys [health equipment] :as body} mob-name]
  (min max-rate
       (* (threat/mob-hurt (armour/armour-stats equipment) mob-name 1)
          (get stances (stance body mob-name))
          (min 4 (/ 20 (max 1 health))))))

(defn danger-of [body kind {:keys [name pos]}]
  (merge {:x (:x pos) :y (:y pos) :z (:z pos) :rate (danger-rate body name) :mob name}
         (get danger-shape (if (= "creeper" name) :creeper kind))))

(defn danger-list
  "The dangers ({:x :y :z :close :radius :rate :mob}) of the sensed mobs [{:key :name :pos}] (nearest first) and the
  remembered :threat entries' data [{:key :mob :pos}] (nearest the body first; one whose :key is sensed now is left out:
  the sensed place wins), at most max-dangers. body {:health :equipment :weapon :pos}."
  [body sensed remembered]
  (let [now (set (map :key sensed))
        spots (->> remembered
                   (remove #(contains? now (:key %)))
                   (filter :pos)
                   (sort-by #(u/dist (:pos body) (:pos %))))]
    (vec (take max-dangers (concat (map #(danger-of body :sensed %) sensed)
                                   (map #(danger-of body :remembered {:name (:mob %) :pos (:pos %)}) spots))))))
