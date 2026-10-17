(ns jobs.lib.build.wood
  "Generic wood names of a plan (plan helper): a want of `planks`, `logs`, `wooden_stairs`,
  `wooden_slabs`, `wooden_doors`, `wooden_fences`, `fence_gates`, `wooden_trapdoors`, `wooden_buttons` or
  `wooden_pressure_plates` (a name or a block with state) is satisfied by any species, as Minecraft's tags are. A species
  name (oak_planks) stays exact. One species is chosen for the plan: the one carried most, else the nearest log seen,
  else oak; it is listed first among the choices (the rest follow, so a block of another species already standing counts).
  Not wood: bamboo, crimson and warped."
  (:require [clojure.string :as str]))

(def species ["oak" "spruce" "birch" "jungle" "acacia" "dark_oak" "mangrove" "cherry" "pale_oak"])

(def generic
  "Generic name -> the kind of block after the species (oak_ + kind)."
  {"planks" "planks" "logs" "log" "wooden_stairs" "stairs" "wooden_slabs" "slab" "wooden_doors" "door"
   "wooden_fences" "fence" "fence_gates" "fence_gate" "wooden_trapdoors" "trapdoor" "wooden_buttons" "button"
   "wooden_pressure_plates" "pressure_plate"})

(def kinds (set (vals generic)))

(def wood-re
  (re-pattern (str "^(stripped_)?(" (str/join "|" (sort-by (comp - count) species)) ")_(" (str/join "|" kinds) ")$")))

(defn species-of
  "The species of a wood block or item name (oak_planks -> \"oak\"), else nil."
  [name]
  (when-let [[_ _ sp _] (and (string? name) (re-matches wood-re name))] sp))

(defn kind-of
  "The kind a generic name stands for, else nil."
  [name]
  (get generic name))

(defn want-name [want] (cond (string? want) want (map? want) (:block want)))

(defn generic-want?
  [want]
  (boolean (kind-of (want-name want))))

(defn generic-wants?
  "Whether any of the wants is a generic wood want."
  [wants]
  (boolean (some generic-want? wants)))

(defn weight
  "How much of species the inventory ({name n}) holds, in planks: a log or wood block counts four."
  [carried sp]
  (reduce-kv (fn [w name n]
               (if (= sp (species-of name))
                 (+ w (* n (if (str/ends-with? name "_log") 4 1)))
                 w))
             0 carried))

(defn choose
  "The species for the plan's generic wood: the one carried most, else nearest-log's species (a log name or nil), else oak."
  [carried nearest-log]
  (let [best (->> species (map (fn [sp] [sp (weight carried sp)])) (filter (comp pos? second)) (sort-by (comp - second)) ffirst)]
    (or best (some-> nearest-log (str/replace #"_log$" "")) "oak")))

(defn resolve-want
  "The want with a generic wood name made [:any chosen-first ...] over the species (state kept); other wants stay."
  [want sp]
  (let [kind (kind-of (want-name want))]
    (if-not kind
      want
      (let [ordered (cons sp (remove #{sp} species))
            state (when (map? want) (dissoc want :block))
            one #(let [name (str % "_" kind)] (if (seq state) (assoc state :block name) name))]
        (into [:any] (map one) ordered)))))
