(ns jobs.items.obtain.plan
  "The craft chain and the gather plan for the names wanted."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.items.obtain.base :refer [craft-radius plan-allowed? seen-radius way?]]
            [jobs.items.obtain.seen :refer [seen-of sheep-in-range shears-source wool-keep]]
            [jobs.items.obtain.table :refer [gather-needs grass-seeds hand-dug leaves-of material-blocks need-way source-opts stalks wool-color wool?]]
            [jobs.items.recipes :as recipes]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]
            [jobs.lib.trees :as trees]
            [jobs.lib.util :as u]))

(defn furnace-free
  "Of plans in order of preference, the first with no smelt step (a furnace needs fuel and time), else the first."
  [plans]
  (or (first (remove (fn [pl] (some #(= :smelt (:op %)) (:steps pl))) plans)) (first plans)))

(defn craft-plan
  "{:item name :steps [..]} for the first of names a craft chain of allowed steps (a smelt step needs the :smelt way,
  any other :craft) makes n more of from what is carried, else nil."
  [c names n]
  (let [p (:primitives c)
        have (storage/usable-totals (u/inventory p))
        mem (ctx/mem c)
        table? (and (not (get-in mem [:craft :table-unreachable]))
                    (boolean (or (:table mem) (crafting-table/nearest p (craft-radius)))))
        version (game/version-of p)]
    (furnace-free
     (keep (fn [name]
             (when-let [pl (recipes/plan version have name n (assoc (source-opts) :table? table?))]
               (when (plan-allowed? c pl) (assoc pl :item name))))
           names))))

(defn pick-plan
  "Of the gather plans (one per wanted name) the one that gathers the least wool, so the colour the body carries
  most of; a plan whose sheep are not in range comes after one whose are. {:wool-fallback {:wanted :taking}} on
  the plan when the colour the body carries most of has no sheep in range."
  [c plans]
  (let [wool-of (fn [pl] (into {} (filter (comp wool? key)) (:gather pl)))]
    (if-not (some (comp seq wool-of) plans)
      (furnace-free plans)
      (let [sorted (sort-by #(reduce + 0 (vals (wool-of %))) plans)
            keep-n (if (shears-source c) 0 wool-keep)
            colors (set (keep (fn [[k n]] (when (> n keep-n) k)) (frequencies (keep #(.-color %) (sheep-in-range c)))))
            colors-of (fn [pl] (map wool-color (keys (wool-of pl))))
            ok? (fn [pl] (every? colors (colors-of pl)))
            chosen (or (first (filter ok? sorted)) (first sorted))
            wanted (first sorted)]
        (cond-> chosen
          (and (not= chosen wanted) (seq (wool-of chosen)) (seq (wool-of wanted)))
          (assoc :wool-fallback {:wanted (first (colors-of wanted)) :taking (first (colors-of chosen))}))))))

(defn raw-seen?
  "Whether every raw need of a gather plan (but wool, left to pick-plan) has a block seen: of several names, the plans
  whose blocks are seen come before one that would have to look for them (coal unseen, logs seen: charcoal)."
  [c plan]
  (every? #(or (:sheep %) (seen-of c %)) (some-> plan :gather (gather-needs (game/version-of (:primitives c))))))

(defn any-log
  "The gather map with the logs any species would do as \"log\" (summed); a log only its own species makes (oak_log
  for oak_planks) keeps its name: it stays the plan's even with plenty of another species carried."
  [gather version have name n opts]
  (let [free? (fn [k] (let [other (first (remove #{k} trees/log-names))
                            pl (recipes/plan version (assoc have other 1000) name n opts)]
                        (not (pos? (get-in pl [:gather k] 0)))))
        free (filter (fn [[k _]] (and (trees/log-name? k) (free? k))) gather)]
    (cond-> (apply dissoc gather (map key free))
      (seq free) (update "log" (fnil + 0) (reduce + 0 (vals free))))))

(defn any-of-logs
  "The picked plan with its logs as \"log\" when the plans of the other names gather a log of another species: any
  species then does (planks of any wood), not only the first name's."
  [pl plans]
  (let [log-keys (fn [p] (filter trees/log-name? (keys (:gather p))))]
    (if (< 1 (count (set (mapcat log-keys plans))))
      (update pl :gather (fn [g] (-> (apply dissoc g (log-keys pl))
                                     (update "log" (fnil + 0) (reduce + 0 (map #(get g %) (log-keys pl)))))))
      pl)))

(defn gather-plan
  "The craft chain for the first of names that has one once the raw items it lacks are gathered, with :gather
  {name n} when it lacks any, else nil. A sapling has no recipe: its plan is just :gather {name n}, when its leaves are seen (or its child is running).
  Of several names that lack wool, the one gathering the least (pick-plan)."
  [c names n]
  (let [p (:primitives c)
        have (storage/usable-totals (u/inventory p))
        version (game/version-of p)
        seen? #(seq (look/seen-blocks p {:names (vec (material-blocks version %)) :radius (seen-radius) :max 1}))
        seen (set (filter seen? (recipes/stone-materials version)))
        materials (if (seq seen) seen (recipes/stone-materials version))
        allowed (fn [pl] (when (and pl (every? #(way? c (need-way %)) (some-> pl :gather (gather-needs version)))) pl))
        plan-of (fn [name]
                  (if-let [seen-of-name (cond (hand-dug name) (hand-dug name)
                                              (leaves-of name) #{(leaves-of name)}
                                              (grass-seeds name) (grass-seeds name)
                                              (stalks name) #{name}
                                              (= "water_bucket" name) #{"water"})]
                    (when (or (= "water_bucket" name)
                              (= name (get-in (ctx/mem c) [:gather :item]))
                              (seq (look/seen-blocks p {:names (vec seen-of-name) :radius (seen-radius) :max 1})))
                      {:steps [] :gather {name n} :item name})
                    (let [opts (merge (source-opts)
                                      {:table? (boolean (or (:table (ctx/mem c)) (crafting-table/nearest p (craft-radius))))
                                       :gather? true :materials materials})]
                      (when-let [pl (recipes/plan version have name n opts)]
                        (when (plan-allowed? c pl)
                          (-> pl
                              (update :gather any-log version have name n opts)
                              (assoc :item name)))))))]
    (if (= 1 (count names))
      (allowed (plan-of (first names)))
      (let [plans (keep (comp allowed plan-of) names)]
        (some-> (pick-plan c (or (not-empty (filter #(raw-seen? c %) plans)) plans)) (any-of-logs plans))))))
