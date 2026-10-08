(ns jobs.build.wood
  "Wood species of a build (jobs.build.from-plan helper, not a job): a plan's wood blocks (planks, logs, stairs, slabs,
  doors, fences, trapdoors ...) are one family, so a species the body carries stands in for the planned one, with the
  same shape and state. Pinned: an any-of want ([:any \"oak_planks\"]) and a plan with :metadata {:exact-wood true}
  keep their species. The choice is made once per species of the plan (booked in the job memory by the caller)."
  (:require [clojure.string :as str]))

(def species ["dark_oak" "pale_oak" "oak" "spruce" "birch" "jungle" "acacia" "mangrove" "cherry"])
(def kinds ["planks" "log" "wood" "stairs" "slab" "door" "fence_gate" "fence" "trapdoor" "button" "pressure_plate"])

(def wood-re
  (re-pattern (str "^(stripped_)?(" (str/join "|" species) ")_(" (str/join "|" kinds) ")$")))

(defn parse
  "[prefix species kind] of a wood block or item name, else nil."
  [name]
  (when-let [[_ pre sp kind] (and (string? name) (re-matches wood-re name))]
    [pre sp kind]))

(defn species-of [name] (second (parse name)))

(defn swap
  "The name in species to, or the name when it is no wood."
  [name to]
  (if-let [[pre _ kind] (parse name)]
    (str pre to "_" kind)
    name))

(defn rewrite
  "The want with its wood in the species of to-of ({from to}); an any-of, a crop, a tree and a non-wood want stay."
  [want to-of]
  (cond
    (string? want) (if-let [to (to-of (species-of want))] (swap want to) want)
    (and (map? want) (string? (:block want))) (if-let [to (to-of (species-of (:block want)))] (update want :block swap to) want)
    :else want))

(defn weight
  "How much of species the inventory ({name n}) holds, in planks: a log or wood block counts four."
  [carried sp]
  (reduce-kv (fn [w name n]
               (let [[_ s kind] (parse name)]
                 (if (= s sp) (+ w (* n (if (#{"log" "wood"} kind) 4 1))) w)))
             0 carried))

(defn choose
  "{planned-species chosen-species} for the wants of a plan's cells and the inventory {name n}. A species keeps itself
  when enough of it is carried to place its cells; else the species carried most stands in; else it keeps itself."
  [wants carried]
  (let [need (frequencies (keep #(species-of (if (map? %) (:block %) (when (string? %) %))) wants))
        best (->> species (map (fn [sp] [sp (weight carried sp)])) (filter (comp pos? second)) (sort-by (comp - second)) ffirst)]
    (into {} (map (fn [[sp n]] [sp (cond (>= (weight carried sp) n) sp best best :else sp)])) need)))
