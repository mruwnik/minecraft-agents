(ns jobs.lib.storage
  "What the body carries and where its chest is, shared by the storage and item jobs (deposit, withdraw, kit,
  make-room, craft, give, obtain, get-seeds)."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [jobs.lib.places :as places]))

(def gear-suffixes ["_pickaxe" "_axe" "_shovel" "_hoe" "_sword" "_helmet" "_chestplate" "_leggings" "_boots"])
(def gear-names #{"shears" "bow" "crossbow" "fishing_rod" "flint_and_steel" "shield" "trident"})

(defn tool? [n]
  (or (contains? gear-names n)
      (some #(str/ends-with? n %) gear-suffixes)))

(defn chest-of
  "The chest position: args :chest as {:x :y :z} (read from [x y z] or {:x :y :z}; nil when unreadable), else the
  known :chest place."
  [view args]
  (if (some? (:chest args))
    (:pos (places/parse-pos (:chest args)))
    (mem/place view :chest)))

(def group-kind :storage/chests)

(a/defargs settings
  {::group-cap {:default 16 :spec (a/int-in 1 nil) :doc "Most storage chests (memory :storage/chests) the body keeps."}})
(defn group-cap [] (settings/get settings ::group-cap))

(defn cell-of [pos] (let [{:keys [x y z]} (if (vector? pos) {:x (pos 0) :y (pos 1) :z (pos 2)} pos)] [x y z]))

(defn group
  "The body's storage chests as [x y z] cells: the :chest place first, then the chests it placed or marked
  (memory :storage/chests), each once."
  [view]
  (let [main (mem/place view :chest)]
    (vec (distinct (map cell-of (concat (when main [main]) (map (comp :pos :data) (mem/entries view group-kind))))))))

(defn add-to-group!
  "Remember the chest at pos as storage, once."
  [c pos]
  (when-not (some #{(cell-of pos)} (group (ctx/view c)))
    (ctx/remember! c group-kind {:pos (zipmap [:x :y :z] (cell-of pos))} {:cap (group-cap) :ttl :forever})))

(defn drop-from-group!
  "Forget the chest at pos as storage (it is gone)."
  [c pos]
  (ctx/forget-where! c group-kind #(= (cell-of pos) (cell-of (:pos %)))))

(defn carried
  "Total carried of name over all stacks."
  [items name]
  (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 items))

(defn carried-totals
  "{name total} of the carried items."
  [items]
  (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} items))
