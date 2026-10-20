(ns jobs.lib.cost.planner
  "The prices go-to's :costs may override, in seconds: the planner's per-move costs (engine.path.planner.base/DEFAULT-COSTS
  holds the defaults, which apply to every price left out). A drop's price is go-to's :drop-cost, a hurt hp's :hp-seconds.
  :lethal-air is no price: 1 lets a swim to air drown the body to death when no other way exists (breathe's), 0 refuses it (:air-lethal); :drown-slack is the seconds of margin in its count of 2 hp hits (ceil(seconds + slack)). Left out: airSupply, airLimit and airUsed (the body's breath, a fact, not a price), airDrain and airGrace (its helmet, see air-profile) and dropFactor (:drop-cost)."
  (:require [engine.args :as a]
            [engine.path.blocks :as blocks]
            [engine.settings :as settings]
            [clojure.string :as str]
            [jobs.lib.cost.armour :as armour]
            [engine.path.planner.base :as base]))

(def walk-floor
  "Seconds a block of walking costs: the planner's heuristic counts this per horizontal block, so a swim-h or exit price below it
  would make the search inadmissible (the spec refusal names it)."
  base/WALK-S)

(def floored
  "The :costs keys that stand in for a block of horizontal walking and may not go under walk-floor."
  #{:swim-h :swim-sub-h :exit})

(def planner-names
  "The :costs keys and the planner cost each sets."
  {:climb-up "climbUp" :climb-down "climbDown" :jump-climb "jumpClimb" :open "open" :swim-h "swimH" :swim-sub-h "swimSubH" :swim-up "swimUp"
   :swim-down "swimDown" :exit "exit" :current "current" :bubble-up "bubbleUp" :bubble-down "bubbleDown"
   :open-redstone "openRedstone" :open-lever "openLever" :open-plate "openPlate" :beside-magma-column "besideMagmaColumn"
   :max-water-drop "maxWaterDrop" :dripleaf "dripleaf" :dripleaf-risk "dripleafRisk" :lethal-air "lethalAir"
   :drown-slack "drownSlack"})

(defn water-breathing-s
  "Seconds of Water Breathing left in self's :effects (their :left ticks), 0 when none or infinite."
  [effects]
  (/ (->> effects (filter #(= "water_breathing" (.-name %))) (map #(max 0 (or (.-left %) 0))) (reduce max 0)) 20))

(defn air-profile
  "{:air-drain :air-grace} of the body's helmet, a key only where it differs from no gear. equipment: self's :equipment
  (cljs or JS). Respiration n drains 1/(n+1) a second (the expected value); a turtle helmet gives each dive 10 s in which
  the air does not drain. Water Breathing adds its seconds left (an effect's :left ticks, from self's :effects) to the
  grace: one pool counted from the dive start, so the walk to the dive is not taken off it. Conduit Power is left out (it
  lasts ~13 s once out of the conduit's range)."
  [equipment effects]
  (let [head (get (armour/equipment-of equipment) "head")
        pool (water-breathing-s effects)
        respiration (->> (:enchants head) (some #(when (= "respiration" (:name %)) (:level %))))]
    (cond-> {}
      (pos? (or respiration 0)) (assoc :air-drain (/ 1 (inc respiration)))
      (or (= "turtle_helmet" (:name head)) (pos? pool))
      (assoc :air-grace (+ (if (= "turtle_helmet" (:name head)) 10 0) pool)))))

(defn- swim-s
  "Seconds a block of swimming at the surface takes with Depth Strider level (vanilla water travel: inertia 0.8 and
  acceleration 0.02 move toward 0.546 and the walking speed 0.1 by level/3, max 3, half that off the ground)."
  [level]
  (let [e (/ (min level 3) 6)
        inertia (+ 0.8 (* (- 0.546 0.8) e))
        accel (+ 0.02 (* (- 0.1 0.02) e))]
    (/ (- 1 inertia) (* 0.98 20 accel))))

(defn swim-profile
  "{:swim-h :swim-sub-h} for Depth Strider on the boots (none: {}): the planner's swimH and swimSubH (the sprint swim with the head under water) scaled as the surface swim speeds up, about
  0.73 at level 1, 0.61 at 2 and 0.55 at 3, never under walk-floor. equipment: self's :equipment (cljs or JS)."
  [equipment]
  (let [level (->> (:enchants (get (armour/equipment-of equipment) "feet"))
                   (some #(when (= "depth_strider" (:name %)) (:level %))))]
    (if (pos? (or level 0))
      (let [scale (/ (swim-s level) (swim-s 0))]
        {:swim-h (max walk-floor (* (unchecked-get base/DEFAULT-COSTS "swimH") scale))
         :swim-sub-h (max walk-floor (* (unchecked-get base/DEFAULT-COSTS "swimSubH") scale))})
      {})))

(defn swim-factor
  "How many blocks of walking a block of swimming at the surface costs the body (the planner's swimH over walk-floor,
  Depth Strider on the boots counted: swim-profile): about 2.2 with none."
  [equipment]
  (/ (or (:swim-h (swim-profile equipment)) (unchecked-get base/DEFAULT-COSTS "swimH")) walk-floor))

(defn air-used
  "The seconds of air a body with oxygen (0-20 bubbles, 15 s; unknown: full) has used; mineflayer rounds the air to a
  bubble, so 0.375 s (half a bubble) more."
  [oxygen]
  (if (number? oxygen) (+ (* (- 20 oxygen) 0.75) 0.375) 0))

(defn air-costs
  "The planner's options.costs airDrain (with airLimit, the supply less the default margin in seconds at that drain), airGrace, airUsed and swimH (a JS object) for a policy's :air-drain,
  :air-grace, :air-used and :swim-h, those it has."
  [policy]
  (let [o #js {}]
    (when-some [h (:swim-h policy)] (unchecked-set o "swimH" h))
    (when-some [h (:swim-sub-h policy)] (unchecked-set o "swimSubH" h))
    (when-some [d (:air-drain policy)]
      (unchecked-set o "airDrain" d)
      ;; the planner's 3 s margin under the supply is seconds of time, so Respiration shrinks it with the drain
      (let [supply (unchecked-get base/DEFAULT-COSTS "airSupply")
            margin (- supply (unchecked-get base/DEFAULT-COSTS "airLimit"))]
        (unchecked-set o "airLimit" (- supply (* margin d)))))
    (when-some [g (:air-grace policy)] (unchecked-set o "airGrace" g))
    (when-some [u (:air-used policy)] (unchecked-set o "airUsed" u))
    o))

(defn planner-costs-problem
  "Why costs is not a usable go-to :costs (nil, or a map of planner-names keys to finite numbers >= 0), else nil."
  [costs]
  (cond
    (nil? costs) nil
    (not (map? costs)) (str ":costs must be a map of price name to seconds, got " (pr-str costs))
    :else (some (fn [[k v]]
                  (cond
                    (not (contains? planner-names k)) (str ":costs has no price " (pr-str k) ", the prices are " (pr-str (vec (keys planner-names))))
                    (not (and (number? v) (js/isFinite v) (>= v 0))) (str ":costs " k " must be a number >= 0, got " (pr-str v))
                    (and (floored k) (< v walk-floor)) (str ":costs " k " must be at least " walk-floor " s (the cost of walking a block: the planner's search assumes no move is quicker), got " v)))
                costs)))

(defn planner-costs
  "The planner's options.costs (a JS object) for the go-to :costs map."
  [costs]
  (let [o #js {}]
    (doseq [[k v] costs] (unchecked-set o (planner-names k) v))
    o))

(def gaits
  "go-to's :gait values: :auto (sprints where the executor does), :walk (never sprints), :sneak (sneaks on level steps, never sprints)."
  #{:auto :walk :sneak})

(defn gait-problem
  "Why gait is not a usable go-to :gait (nil, or one of gaits), else nil."
  [gait]
  (when-not (or (nil? gait) (contains? gaits gait))
    (str ":gait must be one of " (pr-str (sort gaits)) ", got " (pr-str gait))))

(defn gait-costs
  "The planner's options.costs walkS and sprintS (seconds per block walked and per block of a gap jump) for gait: a walking
  body runs a gap jump at walking speed, a sneaking one walks at sneak speed; :auto: the planner's defaults."
  [gait]
  (case gait
    :walk #js {:sprintS base/WALK-S :swimSubH js/Infinity}
    :sneak #js {:walkS base/SNEAK-S :sprintS base/SNEAK-S :swimSubH js/Infinity}
    #js {}))

(def default-landing
  "Block name -> share of a fall's damage a body takes landing on it (go-to's :landing, which overrides entry by entry): hay
  and honey take 80% off; a negative factor (slime) is a bounce: no damage, but the body takes seconds to settle."
  {"hay_block" 0.2 "honey_block" 0.2 "slime_block" -1})

(defn landing-problem
  "Why landing is not a usable go-to :landing (nil, or a map of block name to a finite number), else nil."
  [landing]
  (let [table (blocks/default-state-table)]
    (cond
      (nil? landing) nil
      (not (map? landing)) (str ":landing must be a map of block name to damage factor, got " (pr-str landing))
      :else (some (fn [[k v]]
                    (cond
                      (not (and (string? k) (seq (blocks/state-ids table k)))) (str ":landing has no block " (pr-str k))
                      (not (and (number? v) (js/isFinite v))) (str ":landing " k " must be a number, got " (pr-str v))))
                  landing))))

(defn planner-landing
  "The planner's options.landing (a Map of state id to damage factor) for the go-to :landing map over default-landing. Under
  the :sneak gait a negative factor is 1: a sneaking body does not bounce, it takes the full fall."
  ([landing] (planner-landing landing nil))
  ([landing gait]
   (let [table (blocks/default-state-table)
         m (js/Map.)]
     (doseq [[k v] (merge default-landing landing)
             id (blocks/state-ids table k)]
       (.set m id (if (and (= :sneak gait) (neg? v)) 1 v)))
     m)))

(def unknown-prices
  "The planner's prices of cells the body has not seen (options.unknown), seconds: :cell a share of a move's own seconds per
  unseen feet or head cell, :floor the same for a stand on an unseen floor, :peek-s a drop onto one (a stop at the edge to
  look), :aim-s a level or up step onto one (a look down at the takeoff); and the depths, in blocks: :see-lit-depth the deepest
  drop a lit landing is seen over from the walk's worst peek stop, :light-depth the deepest a torch at the takeoff lights."
  {:cell 0.1 :floor 0.3 :peek-s 0.7 :aim-s 0.1 :see-lit-depth 5 :light-depth 5})

(a/defargs settings
  {::drop-cost {:default 1 :doc "Scales the cost of a drop (fall seconds and damage; 1 as is, 0 free, 5 dear); false: no drop of 2 or 3 at all. :one-way :closed instead refuses only a drop the body cannot climb back." :spec (a/or-of (a/num-in 0 nil) false?)}
   ::costs {:default false :doc (str "Map of price name to seconds, overriding the planner's price of a move (" (str/join " " (map name (keys planner-names))) "); a price left out keeps the planner's default. false: none.")
            :spec (a/or-of false? (a/valid-by "a map of planner price name to a number of seconds >= 0" #(planner-costs-problem %)))}
   ::lethal-air {:default false :doc "true lets a plan drown the body when no other way to air exists (breathe's); false: such a plan is refused (:why :air-lethal)." :spec boolean?}
   ::landing {:default false :doc "Map of block name to the share of a fall's damage a landing on it takes (0.2: 80% off), negative: a bounce, no damage but settle time (only onto a pad in view, bouncing blocks or a wall all round, not under :gait :sneak); entries override the defaults hay_block 0.2, honey_block 0.2 and slime_block -1."
              :spec (a/or-of false? (a/valid-by "a map of block name to a number" #(landing-problem %)))}
   ::unknown-prices {:default false :doc "Map of :cell :floor :peek-s :aim-s :see-lit-depth :light-depth to numbers overriding the prices of cells a walk over :unknown has not seen (unknown-prices); a go-to :unknown map wins over it." :spec (a/or-of false? (a/valid-by "a map of unknown-price name to a number >= 0" (fn [m] (when-not (or (nil? m) (and (map? m) (every? #(and (contains? unknown-prices (key %)) (number? (val %)) (>= (val %) 0)) m))) "not a map of unknown-price names to numbers >= 0"))))}})

(defn walk-costs
  "The planner price overrides every walk keeps: the setting ::costs, with :lethal-air 1 when ::lethal-air."
  []
  (let [m (settings/get settings ::costs)]
    (cond-> (or m nil) (settings/get settings ::lethal-air) (assoc :lethal-air 1))))

(defn walk-landing "The setting ::landing, nil when none." [] (or (settings/get settings ::landing) nil))
(defn walk-drop-cost "The setting ::drop-cost." [] (settings/get settings ::drop-cost))
(defn walk-unknown-prices "The unknown-prices as set: the defaults over the setting ::unknown-prices." []
  (merge unknown-prices (or (settings/get settings ::unknown-prices) nil)))
