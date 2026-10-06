(ns engine.jobs.danger
  "The danger of walking a route past mobs (route-danger), in expected points of damage after the body's armour, and
  a straight ground route between two points (straight-route) for a caller with no planned one.

  The mobs are an input: what the body has seen or remembers ({:name :pos} per mob; JS entities are read too), e.g.
  engine.jobs.reach/known-hostiles. route-danger never reads the world's entities itself, so an unseen creeper
  never counts.

  Per mob:
    hostile?  minecraft-data's entity type \"hostile\" (or category \"Hostile mobs\"). Anything else is no danger
              unless an override names it. Endermen and zombified piglins only fight when provoked: x0.1.
    threat    damage before armour: a creeper's blast is 43 (one hit); any other mob is engine.jobs.combat/mob-dps
              (3 if unknown) x 3 s of exposure.
    weight    by the closest route cell: 1 within 2 blocks, falling to 0 at :radius (16).
    reach     a melee mob (creepers too) needs a walkable way to that cell (engine.jobs.reach). A ranged mob needs a
              line of fire to one of the route cells near it. A mob behind walls is no danger.
    armour    vanilla's formula per hit: the armour-points and toughness reduction, then the enchantment reduction
              (EPF: protection 1 a level, blast 2 for explosions, projectile 2 for arrows). Toughness is 2 a diamond
              piece and 3 a netherite piece.
  danger = threat after armour x weight, when the mob can reach the route; else 0.
  Overrides {mob-name number-or-{:times n}}: a number is the mob's threat before armour, {:times n} multiplies it.
  Equipment: {part {:name :enchants [{:name :lvl}]}} for head torso legs feet, default what the body wears."
  (:require [engine.jobs.combat :as combat]
            [engine.jobs.reach :as reach]
            ["minecraft-data" :as minecraft-data]
            [engine.game :as game]))

(def default-radius 16)
(def close 2)
(def exposure-s 3)
(def creeper-blast 43)
(def provoked-only #{"enderman" "zombified_piglin"})
(def provoked-share 0.1)
(def max-fire-checks "Route cells a ranged mob's line of fire is tried to, nearest first." 8)
(def toughness-by-material {"diamond" 2 "netherite" 3})
(def epf-per-level {"protection" {:all 1} "blast_protection" {:explosion 2} "projectile_protection" {:projectile 2}
                    "fire_protection" {:fire 2}})

;; ---------------------------------------------------------------- reading inputs

(defn mob-of
  "{:name :pos {:x :y :z} :kind} of a mob given as a cljs map or a JS entity."
  [m]
  (if (map? m)
    (select-keys m [:name :pos :kind :id])
    (let [pos (.-pos m)]
      {:name (.-name m) :kind (.-kind m) :id (.-id m) :pos (when pos {:x (.-x pos) :y (.-y pos) :z (.-z pos)})})))

(defn piece-of [x]
  (cond
    (nil? x) nil
    (map? x) x
    :else {:name (.-name x) :enchants (some-> (.-enchants x) (js->clj :keywordize-keys true))}))

(defn equipment-of
  "{part piece} for head torso legs feet of equipment given as cljs (keyword or string keys) or JS."
  [eq]
  (into {} (for [part ["head" "torso" "legs" "feet"]
                 :let [x (if (map? eq) (or (get eq (keyword part)) (get eq part)) (some-> eq (aget part)))]
                 :when x]
             [part (piece-of x)])))

(defn body-equipment [p] (some-> p .self .-equipment))

;; ---------------------------------------------------------------- mob threat

(defn hostile? [version {:keys [name kind]}]
  (let [e (some-> (minecraft-data version) .-entitiesByName (aget name))]
    (if e
      (or (= "hostile" (.-type e)) (= "Hostile mobs" (.-category e)))
      (= "hostile" kind))))

(defn damage-type [name]
  (cond (= "creeper" name) :explosion
        (contains? combat/ranged-mobs name) :projectile
        :else :melee))

(defn base-threat
  "{:threat :hits} of mob name before armour."
  [name]
  (if (= "creeper" name)
    {:threat creeper-blast :hits 1}
    {:threat (* exposure-s (get combat/mob-dps name 3)) :hits exposure-s}))

(defn override-fn [o]
  (cond (number? o) (constantly o)
        (map? o) (let [t (or (:times o) (get o "times"))] (if (number? t) #(* t %) identity))
        :else identity))

;; ---------------------------------------------------------------- armour

(defn armour-stats [equipment]
  (reduce (fn [acc [_ {:keys [name enchants]}]]
            (let [material (first (.split (str name) "_"))]
              (-> acc
                  (update :points + (combat/piece-points name))
                  (update :toughness + (get toughness-by-material material 0))
                  (update :enchants into enchants))))
          {:points 0 :toughness 0 :enchants []} equipment))

(defn epf [enchants dtype]
  (min 20 (reduce + 0 (map (fn [{:keys [name id lvl level]}]
                             (let [per (get epf-per-level (str (or name id)))]
                               (* (or lvl level 1) (+ (get per :all 0) (get per dtype 0)))))
                           enchants))))

(defn after-armour
  "The threat (spread over hits) after armour stats and enchantments, for damage type dtype."
  [{:keys [points toughness enchants]} dtype threat hits]
  (let [hit (/ threat (max 1 hits))
        armour (min 20 (max (/ points 5) (- points (/ (* 4 hit) (+ toughness 8)))))
        after (* hit (- 1 (/ armour 25)) (- 1 (/ (epf enchants dtype) 25)))]
    (* after (max 1 hits))))

;; ---------------------------------------------------------------- reach

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
  (if (= :projectile (damage-type name))
    (let [eye (update pos :y + 1.6)]
      (boolean (some #(line-clear? kind-at eye (update (centre %) :y + 1.5)) (take max-fire-checks near-cells))))
    (let [{:keys [x y z]} (first near-cells)]
      (reach/way? kind-at (reach/cell-of pos) [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))))

;; ---------------------------------------------------------------- the danger

(defn route-danger
  "{:danger total :mobs [{:name :pos :distance :weight :threat :danger} ...]} of walking route ([{:x :y :z}] cells)
  past mobs (cljs maps {:name :pos} or JS entities). Lists only mobs that add danger, the worst first.
  Options: :overrides, :equipment (default what the body wears), :radius (16), :version (minecraft-data, default the
  body's). See the ns doc."
  [p route mobs & {:keys [overrides equipment radius version]}]
  (let [version (or version @game/version)
        radius (or radius default-radius)
        overrides (into {} (map (fn [[k o]] [(if (keyword? k) (name k) (str k)) o])) overrides)
        stats (armour-stats (equipment-of (if (some? equipment) equipment (body-equipment p))))
        kind-at (reach/lookup p)
        route (vec route)
        rows (when (seq route)
               (for [m (map mob-of mobs)
                     :when (and (string? (:name m)) (:pos m)
                                (or (contains? overrides (:name m)) (hostile? version m)))
                     :let [cells (->> route
                                      (map (fn [c] [(dist (:pos m) (centre c)) c]))
                                      (filter #(<= (first %) radius))
                                      (sort-by first))
                           d (ffirst cells)]
                     :when d
                     :let [weight (if (<= d close) 1 (/ (- radius d) (- radius close)))
                           {:keys [threat hits]} (base-threat (:name m))
                           threat ((override-fn (get overrides (:name m))) threat)
                           threat (if (contains? provoked-only (:name m)) (* provoked-share threat) threat)]
                     :when (and (pos? weight) (pos? threat) (reaches? kind-at m (map second cells)))
                     :let [hurt (after-armour stats (damage-type (:name m)) threat hits)]]
                 {:name (:name m) :pos (:pos m) :distance d :weight weight :threat threat :danger (* weight hurt)}))
        rows (vec (sort-by :danger > rows))]
    {:danger (reduce + 0 (map :danger rows)) :mobs rows}))

;; ---------------------------------------------------------------- a route without a plan

(def snap-span "How far up or down a straight route looks for ground in each column." 4)

(defn straight-route
  "The cells [{:x :y :z}] one a block from the cell of from to the cell of to on a straight line, each put on the
  ground nearest the cell before it (within 4 up or down: feet and head free, something under), else at that cell's
  height. A cheap stand-in for a planned route when only the danger along the way is wanted."
  [p from to]
  (let [kind-at (reach/lookup p)
        [fx fy fz] (map js/Math.floor [(:x from) (:y from) (:z from)])
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
