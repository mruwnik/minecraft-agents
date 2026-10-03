(ns jobs.storage.deposit
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.memory :as mem]))

(def doc
  "Walk to the chest and deposit one stack per round: the named items, or
  everything but tools and armour (saplings included).")

(def args
  {:chest {:doc "chest position; the known :chest place when nil" :default nil}
   :items {:doc "item names to put away; everything but tools and armour when nil" :default nil}})

(def gear-suffixes ["_pickaxe" "_axe" "_shovel" "_hoe" "_sword" "_helmet" "_chestplate" "_leggings" "_boots"])
(def gear-names #{"shears" "bow" "crossbow" "fishing_rod" "flint_and_steel" "shield" "trident"})

(defn tool? [n]
  (or (contains? gear-names n)
      (some #(str/ends-with? n %) gear-suffixes)))

(defn chest-of
  "The chest position: args :chest, else the known :chest place."
  [view args]
  (or (:chest args) (mem/place view :chest)))

(defn to-deposit
  "The first carried stack to put away: of the wanted names when given,
  otherwise anything that is not a tool or armour."
  [items wanted]
  (let [wanted (some-> wanted set)]
    (first (filter #(if wanted (wanted (:name %)) (not (tool? (:name %)))) items))))

(defn check
  "A chest is known."
  [c]
  (boolean (chest-of (ctx/view c) (:args c))))

(defn ^:async round
  "Done when nothing is left to put away. Three failed transfers (full,
  missing, unreachable) give up with a chest_unusable warn."
  [c]
  (let [chest (chest-of (ctx/view c) (:args c))
        stack (to-deposit (u/inventory (:primitives c)) (:items (:args c)))]
    (cond
      (nil? stack) :done
      (nil? chest) :continue
      :else
      (let [w (await (u/walk-near! c chest 3))]
        (case w
          :partial :continue
          :blocked (u/fail! c :chest_unusable "cannot reach the chest")
          (let [r (await (ctx/act c :transfer (clj->js {:pos chest :direction "deposit"
                                                         :item (:name stack) :count (:count stack)})))]
            (if (= "ok" (.-status r))
              :continue
              (u/fail! c :chest_unusable (str "chest not usable: " (.-status r))))))))))
