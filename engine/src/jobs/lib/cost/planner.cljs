(ns jobs.lib.cost.planner
  "The prices go-to's :costs may override, in seconds: the planner's per-move costs (engine.path.planner.base/DEFAULT-COSTS
  holds the defaults, which apply to every price left out). A drop's price is go-to's :drop-cost, a hurt hp's :hp-seconds.
  Left out: airSupply and airLimit (the body's breath, a fact, not a price) and dropFactor (:drop-cost)."
  (:require [engine.path.blocks :as blocks]
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
   :max-water-drop "maxWaterDrop" :dripleaf "dripleaf" :dripleaf-risk "dripleafRisk"})

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

(def default-landing
  "Block name -> share of a fall's damage a body takes landing on it (go-to's :landing, which overrides entry by entry): hay
  takes 80% off; a negative factor (slime) takes no drop over 3 onto the block at all, the body bounces off it."
  {"hay_block" 0.2 "slime_block" -1})

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
  "The planner's options.landing (a Map of state id to damage factor) for the go-to :landing map over default-landing."
  [landing]
  (let [table (blocks/default-state-table)
        m (js/Map.)]
    (doseq [[k v] (merge default-landing landing)
            id (blocks/state-ids table k)]
      (.set m id v))
    m))
