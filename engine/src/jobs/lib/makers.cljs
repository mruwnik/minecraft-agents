(ns jobs.lib.makers
  "Which listed job will make an item: a reflex that would fetch one itself waits for it instead (needs-bed, needs-weapon)."
  (:require [engine.memory :as mem]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.world-files :as world]))

(def iron-kit
  "jobs.items.get-kit's default :items."
  ["iron_pickaxe" "iron_sword" "iron_axe" "iron_shovel" "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"])

(defn makes
  "The item names a listed leaf job {:job label :args} brings: an obtain's :item, :any-of and :bill, a get-kit's :items,
  the blocks of a from-plan's plan (its :part only, when named; plans from world-files kn)."
  [kn {:keys [job args]}]
  (case (str job)
    "jobs.items.obtain" (concat (some-> (:item args) vector) (:any-of args) (keys (:bill args)))
    "jobs.items.get-kit" (or (:items args) iron-kit)
    "jobs.build.from-plan" (->> (:cells (world/plan kn (:plan args)))
                                (filter #(or (nil? (:part args)) (= (:part args) (:part %))))
                                (mapcat (comp pen-read/want-blocks :want)))
    nil))

(defn listed-maker
  "The id of a job in live (the listed ids) that brings an item `wanted?` holds for, else nil."
  [view kn live wanted?]
  (some (fn [id] (when (some wanted? (makes kn (:data (mem/latest view (mem/job-kind id))))) id)) live))
