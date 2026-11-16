(ns jobs.items.obtain.chain
  "The craft chain from what is carried and the seconds its steps take."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.items.craft :as craft]
            [jobs.items.obtain.base :refer [plan-allowed?]]
            [jobs.lib.cost :as cost]
            [jobs.lib.foods :as foods]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.lib.recipes :as recipes]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.sources :refer [source-opts]]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(defn furnace-free
  "Of plans in order of preference, the first with no smelt step (a furnace needs fuel and time), else the first."
  [plans]
  (or (first (remove (fn [pl] (some #(= :smelt (:op %)) (:steps pl))) plans)) (first plans)))

(defn spendable
  "{name total} the craft children may use: the carried items the reserve register leaves free (reserve/free) to
  jobs.items.craft's chooser, worn-out tools left out. A spare tool's craft, which may take held units once nothing
  else can be got, asks as :craft. Carried raw meat and fish are always spendable: the furnace turns them into more food points (potatoes stay planting stock)."
  [c]
  (let [free (storage/usable-totals (reserve/free c (if (= :torches (craft/slot-of c)) :craft-held :craft)))
        raw (select-keys (storage/usable-totals (u/inventory (:primitives c))) (remove #{"potato"} foods/raw-meals))]
    (merge free raw)))

(defn craft-plan
  "{:item name :steps [..]} for the first of names a craft chain of allowed steps (a smelt step needs the :smelt way,
  any other :craft) makes n more of from what is carried (spendable) less the units of names themselves (turning one
  of the wanted items into another, cobblestone into stone, nets nothing), else nil."
  [c names n]
  (let [p (:primitives c)
        have (apply dissoc (spendable c) names)
        mem (ctx/mem c)
        table? (and (not (get-in mem [:craft :table-unreachable]))
                    (crafting-table/at-hand? p mem))
        version (game/version-of p)]
    (furnace-free
     (keep (fn [name]
             (when-let [pl (recipes/plan version have name n (assoc (source-opts) :table? table?))]
               (when (plan-allowed? c pl) (assoc pl :item name))))
           names))))

(defn smelt-s
  "Seconds n smelted items take, fuel included (the plan holds no fuel fetch: the smelt child gets its own)."
  [c n]
  (* n (cost/smelt-price :version (game/version-of (:primitives c)))))

(defn steps-s
  "Seconds the craft and smelt steps of a plan take."
  [c pl]
  (let [{smelts true crafts false} (group-by #(= :smelt (:op %)) (:steps pl))]
    (+ (* (cost/craft-step-s) (count crafts)) (smelt-s c (reduce + 0 (map :count smelts))))))
