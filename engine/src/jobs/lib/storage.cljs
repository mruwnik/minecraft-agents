(ns jobs.lib.storage
  "What the body carries and where its chest is, shared by the storage and item jobs (deposit, withdraw, kit,
  make-room, bake, craft, give, obtain, get-seeds)."
  (:require [clojure.string :as str]
            [engine.memory :as mem]
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

(defn carried
  "Total carried of name over all stacks."
  [items name]
  (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 items))
