(ns jobs.lib.cost.planner
  "The prices go-to's :costs may override, in seconds: the planner's per-move costs (engine.path.planner.base/DEFAULT-COSTS
  holds the defaults, which apply to every price left out). A drop's price is go-to's :drop-cost, a hurt hp's :hp-seconds.
  :lethal-air is no price: 1 lets a swim to air drown the body to death when no other way exists (breathe's), 0 refuses it (:air-lethal). Left out: airSupply, airLimit and airUsed (the body's breath, a fact, not a price), airDrain and airGrace (its helmet, see air-profile) and dropFactor (:drop-cost)."
  (:require [engine.path.blocks :as blocks]
            [jobs.lib.cost.armour :as armour]
            [engine.path.planner.base :as base]))

(def walk-floor
  "Seconds a block of walking costs: the planner's heuristic counts this per horizontal block, so a swim-h or exit price below it
  would make the search inadmissible (the :bad-costs refusal names it)."
  base/WALK-S)

(def floored
  "The :costs keys that stand in for a block of horizontal walking and may not go under walk-floor."
  #{:swim-h :exit})

(def planner-names
  "The :costs keys and the planner cost each sets."
  {:climb-up "climbUp" :climb-down "climbDown" :jump-climb "jumpClimb" :open "open" :swim-h "swimH" :swim-up "swimUp"
   :swim-down "swimDown" :exit "exit" :current "current" :bubble-up "bubbleUp" :bubble-down "bubbleDown"
   :open-redstone "openRedstone" :open-lever "openLever" :open-plate "openPlate" :beside-magma-column "besideMagmaColumn"
   :max-water-drop "maxWaterDrop" :dripleaf "dripleaf" :dripleaf-risk "dripleafRisk" :lethal-air "lethalAir"})

(defn air-profile
  "{:air-drain :air-grace} of the body's helmet, a key only where it differs from no gear. equipment: self's :equipment
  (cljs or JS). Respiration n drains 1/(n+1) a second (the expected value); a turtle helmet gives each dive 10 s in which
  the air does not drain. Effects are left out: a sensed duration never counts down, and Water Breathing is one pool the
  walk to a dive uses up (Conduit Power lasts ~13 s once out of the conduit's range)."
  [equipment]
  (let [head (get (armour/equipment-of equipment) "head")
        respiration (->> (:enchants head) (some #(when (= "respiration" (:name %)) (:level %))))]
    (cond-> {}
      (pos? (or respiration 0)) (assoc :air-drain (/ 1 (inc respiration)))
      (= "turtle_helmet" (:name head)) (assoc :air-grace 10))))

(defn air-used
  "The seconds of air a body with oxygen (0-20 bubbles, 15 s; unknown: full) has used; mineflayer rounds the air to a
  bubble, so 0.375 s (half a bubble) more."
  [oxygen]
  (if (number? oxygen) (+ (* (- 20 oxygen) 0.75) 0.375) 0))

(defn air-costs
  "The planner's options.costs airDrain, airGrace and airUsed (a JS object) for a policy's :air-drain, :air-grace and
  :air-used, those it has."
  [policy]
  (let [o #js {}]
    (when-some [d (:air-drain policy)] (unchecked-set o "airDrain" d))
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
    :walk #js {:sprintS base/WALK-S}
    :sneak #js {:walkS base/SNEAK-S :sprintS base/SNEAK-S}
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
