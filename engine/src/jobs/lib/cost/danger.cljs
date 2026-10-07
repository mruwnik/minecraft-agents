(ns jobs.lib.cost.danger
  "Danger of mobs the body knows (an input, e.g. jobs.lib.danger/known-hostiles; no world entity reads): route-danger, the
  expected damage of walking a route past them (recover-drops' fetch cost), and danger-rate / danger-list, the hp a
  go-to planner costs near a known danger (its options.dangers).

  route-danger per mob: hostile (jobs.lib.cost.threat; provoked-only mobs x0.1) or named in overrides; mob-hurt over 3 s
  after armour; weight 1 within 2 blocks of the route, falling to 0 at :radius; counts only if a melee mob can walk to
  the route or a ranged mob has a line of fire to it. Overrides {mob-name number-or-{:times n}}: threat before armour.

  danger-rate: mob-hurt over 1 s x stance factor (:flee x4, :fight x0.1), at most max-rate: hp a second (the factors, the cap and
  danger-shape are go-to options, danger-opts), which the planner prices at go-to's hp price (:hp-seconds x health-scale) like a drop's damage."
  (:require [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

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

(defn centre [{:keys [x y z]}] {:x (+ (js/Math.floor x) 0.5) :y (js/Math.floor y) :z (+ (js/Math.floor z) 0.5)})

(defn line-clear?
  "Whether no :solid cell of kind-at lies on the segment from a to b ({:x :y :z}): reach/ray-clear?."
  [kind-at a b]
  (reach/ray-clear? kind-at [(:x a) (:y a) (:z a)] [(:x b) (:y b) (:z b)]))

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
  worst first. version: the minecraft-data version; kind-at: jobs.lib.reach/lookup. Options: :overrides, :radius (16).
  See the ns doc."
  [version kind-at route mobs equipment & {:keys [overrides radius]}]
  (let [radius (or radius default-radius)
        overrides (into {} (map (fn [[k o]] [(if (keyword? k) (name k) (str k)) o])) overrides)
        stats (armour/armour-stats equipment)
        route (vec route)
        rows (when (seq route)
               (for [m (map mob-of mobs)
                     :when (and (string? (:name m)) (:pos m)
                                (or (contains? overrides (:name m)) (threat/hostile? version m)))
                     :let [cells (->> route
                                      (map (fn [c] [(u/dist (:pos m) (centre c)) c]))
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
(def max-rate "hp a second one danger costs at most." 4)
(def danger-cap "hp a second all known dangers together cost at most (the planner's dangerCap); a larger :danger-max-rate raises it." 4)

(defn danger-opts
  "The danger options of go-to's args {:flee-factor :fight-factor :danger-max-rate :danger-shape}, as the {:stances :max-rate
  :shape} that danger-rate and danger-list take; only the ones given (the rest are the defaults above)."
  [{:keys [flee-factor fight-factor danger-max-rate danger-shape]}]
  (let [stances (cond-> {} (some? flee-factor) (assoc :flee flee-factor) (some? fight-factor) (assoc :fight fight-factor))]
    (cond-> {}
      (seq stances) (assoc :stances stances)
      (some? danger-max-rate) (assoc :max-rate danger-max-rate)
      (some? danger-shape) (assoc :shape danger-shape))))

(defn danger-shape-problem
  "Why shape (go-to's :danger-shape) is not usable, else nil: a map of :sensed, :remembered or :creeper to {:close :radius}
  numbers >= 0, close under radius (against what the default has for the term not given)."
  [shape]
  (when (some? shape)
    (or (when-not (map? shape) (str ":danger-shape must be a map, got " (pr-str shape)))
        (some (fn [[k v]]
                (let [m (merge (get danger-shape k) v)
                      ok? #(and (number? %) (js/isFinite %) (>= % 0))]
                  (cond (not (contains? danger-shape k)) (str ":danger-shape has no " (pr-str k) ", only :sensed :remembered :creeper")
                        (not (and (map? v) (every? #{:close :radius} (keys v)) (ok? (:close m)) (ok? (:radius m)) (< (:close m) (:radius m))))
                        (str ":danger-shape " (pr-str k) " must be {:close :radius} numbers >= 0 with :close under :radius, got " (pr-str v)))))
              shape))))

(defn stance
  "What the hostile reflex would do about mob-name alone (fight/decide): :fight or :flee."
  [{:keys [health equipment weapon]} mob-name]
  (fight/decide {:health health :reserve fight/default-reserve :creeper? (= "creeper" mob-name)
                 :damage (fight/fight-damage {:weapon weapon :equipment equipment :mobs [{:name mob-name :distance 0}]})}))

(defn danger-rate
  "hp a second near mob-name costs body {:health :equipment :weapon} (see the ns doc). opts: danger-opts (default none)."
  ([body mob-name] (danger-rate body mob-name nil))
  ([{:keys [equipment] :as body} mob-name opts]
   (min (get opts :max-rate max-rate)
        (* (threat/mob-hurt (armour/armour-stats equipment) mob-name 1)
           (get (merge stances (:stances opts)) (stance body mob-name))))))

(defn danger-of [body kind {:keys [name pos]} {:keys [shape] :as opts}]
  (let [k (if (= "creeper" name) :creeper kind)]
    (merge {:x (:x pos) :y (:y pos) :z (:z pos) :rate (danger-rate body name opts) :mob name}
           (get danger-shape k) (get shape k))))

(defn danger-list
  "The dangers ({:x :y :z :close :radius :rate :mob}) of the sensed mobs [{:key :name :pos}] (nearest first) and the
  remembered :threat entries' data [{:key :mob :pos}] (nearest the body first; one whose :key is sensed now is left out:
  the sensed place wins), at most max-dangers. body {:health :equipment :weapon :pos}. opts: danger-opts (default none)."
  ([body sensed remembered] (danger-list body sensed remembered nil))
  ([body sensed remembered opts]
  (let [now (set (map :key sensed))
        spots (->> remembered
                   (remove #(contains? now (:key %)))
                   (filter :pos)
                   (sort-by #(u/dist (:pos body) (:pos %))))]
    (vec (take max-dangers (concat (map #(danger-of body :sensed % opts) sensed)
                                   (map #(danger-of body :remembered {:name (:mob %) :pos (:pos %)} opts) spots)))))))
