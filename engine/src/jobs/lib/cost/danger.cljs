(ns jobs.lib.cost.danger
  "Danger of mobs the body knows (an input, e.g. jobs.lib.danger/known-hostiles; no world entity reads): route-danger, the
  expected damage of walking a route past them (recover-drops' fetch cost), and danger-rate / danger-list, the hp a
  go-to planner costs near a known danger (its options.dangers).

  route-danger per mob: hostile (jobs.lib.cost.threat; provoked-only mobs x0.1) or named in overrides; mob-hurt over 3 s
  after armour, dearer for a body that cannot afford to fight it (no-answer-factor; not for a provoked-only mob, which is never fought, and a mob with :angry false adds none);
  weight 1 within 2 blocks of the route, falling to 0 at :radius; counts only if a melee mob can walk to
  the route or a ranged mob has a line of fire to it. Overrides {mob-name number-or-{:times n}}: threat before armour.

  exposure-price: the seconds a second exposed to mobs that reach the body costs (hurt-rate, the hp a second, at the hp price).

  danger-rate: mob-hurt over 1 s x stance factor (:flee x4, :fight x0.1), at most max-rate: hp a second (the factors, the cap and
  shape are settings, danger-opts), which the planner prices at go-to's hp price (:hp-seconds x health-scale) like a drop's damage."
  (:require [engine.args :as a]
            [engine.constants :as constants]
            [engine.settings :as settings]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

(declare danger-shape-problem)

(a/defargs settings
  {::max-fire-checks {:default 8 :doc "Route cells a ranged mob's line of fire is tried to, nearest first." :spec (a/int-in 1 nil)}
   ::default-radius {:default 16 :doc "Blocks out to which a sensed or remembered mob costs a route." :spec (a/int-in 1 nil)}
   ::close {:default 2 :doc "Blocks within which a mob is close." :spec (a/int-in 0 nil)}
   ::exposure-s {:default 3 :doc "Seconds a body is exposed to a mob on a route." :spec (a/int-in 0 nil)}
   ::snap-span {:default 4 :doc "How far up or down a straight route looks for ground in each column." :spec (a/int-in 0 nil)}
   ::sprint-speed {:default 5.6 :doc "Blocks a second a sprinting body covers (reach-first?)." :spec (a/num-in 0.1 nil)}
   ::walk-speed {:default 4.3 :doc "Blocks a second a walking body covers, one too hungry to sprint (reach-first?)." :spec (a/num-in 0.1 nil)}
   ::flee-factor {:default 4 :doc "Danger price: a known mob the body would flee costs this times its hp a second." :spec (a/num-in 0 nil)}
   ::fight-factor {:default 0.1 :doc "Danger price: a known mob the body would fight costs this times its hp a second." :spec (a/num-in 0 nil)}
   ::max-rate {:default 4 :doc "Danger price: hp a second one known danger costs at most; above 4 it also raises the total of all dangers a second (the planner's dangerCap)." :spec a/pos-num?}
   ::shape {:default false :doc "Danger price: {:sensed|:remembered|:creeper {:close :radius}} blocks, full within :close, none past :radius; only the terms given replace the defaults (sensed 3 12, remembered 3 16, creeper 4 8). false: the defaults."
            :spec (a/or-of false? (a/valid-by "a map of :sensed, :remembered or :creeper to {:close :radius} numbers >= 0, :close under :radius" #(danger-shape-problem %)))}
   ::sprint-food {:default 6 :doc "Food at or under which a body cannot sprint (reach-first?)." :spec (a/int-in 0 20)}
   ::chase-speed {:default 3 :doc "Blocks a second a chasing melee mob covers, a little over a zombie's (reach-first?)." :spec (a/num-in 0.1 nil)}
   ::baby-chase-speed {:default 5.1 :doc "Blocks a second a baby zombie covers chasing (chase-speed of mob-kind \"baby\")." :spec (a/num-in 0.1 nil)}
   ::outrun-factor {:default 1.5 :doc "Times a mob's chase speed the body must run to get away from it on foot (outruns?): a flight goes in steps and round obstacles, the mob comes straight on." :spec (a/num-in 0 nil)}
   ::no-answer-scale {:default 60 :doc "How much dearer a mob's route danger is for each time over what the body can afford to lose in a fight with it (no-answer-factor)." :spec (a/num-in 0 nil)}
   ::race-margin-s {:default 0.5 :doc "Seconds the body must win a race to a refuge by (reach-first?)." :spec (a/num-in 0 nil)}
   ::melee-reach {:default 1.4 :doc "Blocks from the body within which a melee mob's blow lands, vanilla's for a zombie (catches?)." :spec (a/num-in 0 nil)}})


;; ---------------------------------------------------------------- reading inputs

(defn mob-of
  "{:name :pos {:x :y :z} :kind} of a mob given as a cljs map or a JS entity."
  [m]
  (if (map? m)
    (select-keys m [:name :pos :kind :id :angry])
    (let [pos (.-pos m)]
      {:name (.-name m) :kind (.-kind m) :id (.-id m) :angry (.-angry m) :pos (when pos {:x (.-x pos) :y (.-y pos) :z (.-z pos)})})))

(defn override-fn [o]
  (cond (number? o) (constantly o)
        (map? o) (let [t (or (:times o) (get o "times"))] (if (number? t) #(* t %) identity))
        :else identity))

;; ---------------------------------------------------------------- reach

(defn max-fire-checks [] (settings/get settings ::max-fire-checks))

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
      (boolean (some #(line-clear? kind-at eye (update (centre %) :y + 1.5)) (take (max-fire-checks) near-cells))))
    (let [{:keys [x y z]} (first near-cells)]
      (reach/way? kind-at (reach/cell-of pos) [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))))

;; ---------------------------------------------------------------- route danger

(defn default-radius [] (settings/get settings ::default-radius))
(defn close [] (settings/get settings ::close))
(defn exposure-s [] (settings/get settings ::exposure-s))

(defn no-answer-scale [] (settings/get settings ::no-answer-scale))

(defn no-answer-factor
  "How much dearer a mob's danger is for a body with weapon (item name, nil a fist) and health that cannot afford to
  fight it: 1 when fight-damage (alone, at distance) leaves the fight reserve, else 1 + no-answer-scale x (damage over
  the health above the reserve, less 1). Nil weapon and health count as a fist at 20."
  [equipment weapon health name distance]
  (let [damage (fight/fight-damage {:weapon weapon :equipment equipment :mobs [{:name name :distance distance}]})
        affordable (max 1 (- (if (number? health) health 20) fight/default-reserve))
        ratio (/ damage affordable)]
    (if (> ratio 1) (+ 1 (* (no-answer-scale) (- ratio 1))) 1)))

(defn route-danger
  "{:danger total :mobs [{:name :pos :distance :weight :threat :danger} ...]} of walking route ([{:x :y :z}] cells)
  past mobs (cljs maps {:name :pos} or JS entities) for a body wearing equipment. Lists only mobs that add danger, the
  worst first. version: the minecraft-data version; kind-at: a jobs.lib.reach lookup. Options: :overrides, :radius (16),
  :weapon (the item held or to hold, default a fist) and :health (default 20): see no-answer-factor. See the ns doc."
  [version kind-at route mobs equipment & {:keys [overrides radius weapon health]}]
  (let [radius (or radius (default-radius))
        overrides (into {} (map (fn [[k o]] [(if (keyword? k) (name k) (str k)) o])) overrides)
        stats (armour/armour-stats equipment)
        route (vec route)
        rows (when (seq route)
               (for [m (map mob-of mobs)
                     :when (and (string? (:name m)) (:pos m)
                                (not (false? (:angry m)))
                                (or (contains? overrides (:name m)) (threat/hostile? version m)))
                     :let [cells (->> route
                                      (map (fn [c] [(u/dist (:pos m) (centre c)) c]))
                                      (filter #(<= (first %) radius))
                                      (sort-by first))
                           d (ffirst cells)]
                     :when d
                     :let [weight (if (<= d (close)) 1 (/ (- radius d) (- radius (close))))
                           share (if (contains? threat/provoked-only (:name m)) (threat/provoked-share) 1)
                           threat-fn #(* share ((override-fn (get overrides (:name m))) %))
                           threat (threat-fn (:threat (threat/base-threat (:name m) (exposure-s))))]
                     :when (and (pos? weight) (pos? threat) (reaches? kind-at m (map second cells)))
                     :let [hurt (threat/mob-hurt stats (:name m) (exposure-s) threat-fn)
                           scale (if (contains? threat/provoked-only (:name m)) 1 (no-answer-factor equipment weapon health (:name m) d))]]
                 {:name (:name m) :pos (:pos m) :distance d :weight weight :threat threat :danger (* weight hurt scale)}))
        rows (vec (sort-by :danger > rows))]
    {:danger (reduce + 0 (map :danger rows)) :mobs rows}))

(defn snap-span [] (settings/get settings ::snap-span))

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
                           (cons prev (mapcat (fn [k] [(+ prev k) (- prev k)]) (range 1 (inc (snap-span))))))
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
(def danger-cap "hp a second all known dangers together cost at most (the planner's dangerCap); a larger ::max-rate raises it." 4)

(defn danger-opts
  "The danger options as the {:stances :max-rate :shape} that danger-rate and danger-list take, from the settings
  ::flee-factor, ::fight-factor, ::max-rate and ::shape."
  []
  (let [shape (settings/get settings ::shape)]
    (cond-> {:stances {:flee (settings/get settings ::flee-factor) :fight (settings/get settings ::fight-factor)}
             :max-rate (settings/get settings ::max-rate)}
      shape (assoc :shape shape))))

(defn danger-shape-problem
  "Why shape (the setting ::shape) is not usable, else nil: a map of :sensed, :remembered or :creeper to {:close :radius}
  numbers >= 0, close under radius (against what the default has for the term not given)."
  [shape]
  (when (some? shape)
    (or (when-not (map? shape) (str "the danger shape must be a map, got " (pr-str shape)))
        (some (fn [[k v]]
                (let [m (merge (get danger-shape k) v)
                      ok? #(and (number? %) (js/isFinite %) (>= % 0))]
                  (cond (not (contains? danger-shape k)) (str "the danger shape has no " (pr-str k) ", only :sensed :remembered :creeper")
                        (not (and (map? v) (every? #{:close :radius} (keys v)) (ok? (:close m)) (ok? (:radius m)) (< (:close m) (:radius m))))
                        (str "the danger shape " (pr-str k) " must be {:close :radius} numbers >= 0 with :close under :radius, got " (pr-str v)))))
              shape))))

(defn stance
  "What the hostile reflex would do about mob-name alone (fight/decide): :fight or :flee."
  [{:keys [health equipment weapon]} mob-name]
  (fight/decide {:health health :reserve fight/default-reserve :creeper? (= "creeper" mob-name)
                 :damage (fight/fight-damage {:weapon weapon :equipment equipment :mobs [{:name mob-name :distance 0}]})}))

(defn hurt-rate
  "hp a second mob-name takes off a body wearing equipment while it can reach it."
  [equipment mob-name]
  (threat/mob-hurt (armour/armour-stats equipment) mob-name 1))

(defn danger-rate
  "hp a second near mob-name costs body {:health :equipment :weapon} (see the ns doc). opts: danger-opts (default none)."
  ([body mob-name] (danger-rate body mob-name nil))
  ([{:keys [equipment] :as body} mob-name opts]
   (min (get opts :max-rate max-rate)
        (* (hurt-rate equipment mob-name)
           (get (merge stances (:stances opts)) (stance body mob-name))))))

(defn exposure-price
  "Seconds each second spent exposed costs body {:health :equipment} to mob-names that can reach it: their hurt-rates
  summed, at health/hp-price. 0 with none."
  [{:keys [health equipment]} mob-names]
  (* (reduce + 0 (map #(hurt-rate equipment %) mob-names)) (health/hp-price (if (number? health) health 20))))

(defn habitat-factor
  "Seconds more each second in the water a mob-name lives in costs body {:health :equipment :weapon}: its danger-rate x
  weight (a fading memory, 0-1) at the hp price of the body's health (the planner's options.tolls factor)."
  [{:keys [health] :as body} mob-name weight]
  (* weight (danger-rate body mob-name (danger-opts)) (health/hp-price (if (number? health) health 20))))

(defn danger-of [body kind {:keys [name pos]} {:keys [shape] :as opts}]
  (let [k (if (= "creeper" name) :creeper kind)]
    (merge {:x (:x pos) :y (:y pos) :z (:z pos) :rate (danger-rate body name opts) :mob name :block true}
           (get danger-shape k) (get shape k))))

(defn danger-list
  "The dangers ({:x :y :z :close :radius :rate :mob :block}; :block true: a known mob's cell is no way) of the sensed mobs [{:key :name :pos}] (nearest first) and the
  remembered :threat entries' data [{:key :mob :pos}, with a :weight 0-1 scaling the rate for a fading memory, :block false for a spot that costs but may be crossed, :terrain? true for a death by terrain (death/terrain-danger)] (nearest the body first; one whose :key is sensed now is left out:
  the sensed place wins), at most max-dangers. body {:health :equipment :weapon :pos}. opts: danger-opts (default none)."
  ([body sensed remembered] (danger-list body sensed remembered nil))
  ([body sensed remembered opts]
  (let [now (set (map :key sensed))
        spots (->> remembered
                   (remove #(contains? now (:key %)))
                   (filter :pos)
                   (sort-by #(u/dist (:pos body) (:pos %))))]
    (vec (take max-dangers (concat (map #(danger-of body :sensed % opts) sensed)
                                   (map #(if (:terrain? %)
                                           (death/terrain-danger {:pos (:pos %) :cause (:mob %) :weight (:weight %)})
                                           (cond-> (danger-of body :remembered {:name (:mob %) :pos (:pos %)} opts)
                                             (:weight %) (update :rate * (:weight %))
                                             (false? (:block %)) (assoc :block false)))
                                        spots)))))))

;; ---------------------------------------------------------------- the race to a refuge

(defn body-speed
  "Blocks a second a body with food food covers running: ::sprint-speed, ::walk-speed at ::sprint-food or under."
  [food]
  (settings/get settings (if (> food (settings/get settings ::sprint-food)) ::sprint-speed ::walk-speed)))

(defn chase-speed
  "Blocks a second mob-name covers chasing the body: engine.constants/mob-speeds, ::baby-chase-speed for \"baby\" (mob-kind), ::chase-speed for a mob not listed there."
  [mob-name]
  (if (= mob-name "baby")
    (settings/get settings ::baby-chase-speed)
    (get constants/mob-speeds mob-name (settings/get settings ::chase-speed))))

(defn mob-kind
  "The key of engine.constants/mob-speeds for hostile entity e: \"baby\" for a baby, else its name."
  [e]
  (if (true? (.-baby e)) "baby" (.-name e)))

(defn reach-first?
  "Whether a body with food food (body-speed), body-dist blocks from a refuge, gets there ::race-margin-s before every
  chaser, each (mob-dists) that many blocks from it at ::chase-speed, or its own chase-speed when that is more
  (mob-names, in the order of mob-dists). True with no chaser."
  ([body-dist mob-dists food] (reach-first? body-dist mob-dists food (repeat nil)))
  ([body-dist mob-dists food mob-names]
   (let [body-s (+ (/ body-dist (body-speed food)) (settings/get settings ::race-margin-s))
         floor (settings/get settings ::chase-speed)]
     (every? true? (map #(< body-s (/ %1 (max floor (chase-speed %2)))) mob-dists mob-names)))))

(defn outruns?
  "Whether a body with food food gets away from every one of mob-names on foot: its body-speed is at least
  ::outrun-factor times each one's chase-speed. A spider is faster at any food, a zombie never."
  [food mob-names]
  (let [factor (settings/get settings ::outrun-factor)]
    (every? #(>= (body-speed food) (* factor (chase-speed %))) mob-names)))

(defn outruns-at?
  "Whether a body with food food (body-speed) running degrees off straight away from a chaser still gains on it (at
  ::chase-speed)."
  [food degrees]
  (> (* (body-speed food) (js/Math.cos (* degrees (/ js/Math.PI 180)))) (settings/get settings ::chase-speed)))

(defn catches?
  "Whether a melee chaser (mob-name, chaser its mob-kind) at pos lands a blow on a body with food food walking the straight
  way from `from` to `to` ({:x :y :z}) before it is past: at some point of the way, every step blocks from the start,
  the mob gets within ::melee-reach no later than the body (reach-first? lost: the body's start counts
  ::race-margin-s). A mob at the heels or ahead by the way catches; one the walk leaves behind does not."
  [{:keys [chaser pos from to food step]}]
  (let [len (u/dist from to)
        n (js/Math.ceil (/ len step))
        reach (settings/get settings ::melee-reach)]
    (boolean (some (fn [i] (let [t (if (zero? n) 0 (/ i n))
                                 pt (merge-with #(+ %1 (* t (- %2 %1))) (select-keys from [:x :y :z]) (select-keys to [:x :y :z]))]
                             (not (reach-first? (* t len) [(max 0 (- (u/dist pos pt) reach))] food [chaser]))))
                   (range (inc n))))))
