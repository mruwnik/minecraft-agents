(ns engine.jobs.storage
  "Putting things away. Contracts in README.md, section Job library."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.memory :as mem]))

(def gear-suffixes ["_pickaxe" "_axe" "_shovel" "_hoe" "_sword" "_helmet" "_chestplate" "_leggings" "_boots"])
(def gear-names #{"shears" "bow" "crossbow" "fishing_rod" "flint_and_steel" "shield" "trident"})

(defn tool? [n]
  (or (contains? gear-names n)
      (some #(str/ends-with? n %) gear-suffixes)))

(defn chest-of
  "The chest position: args :chest, else the first known :chest place."
  [memory args]
  (or (:chest args) (:pos (first (mem/places memory :chest)))))

(defn to-deposit
  "The first carried stack to put away: of the wanted names when given,
  otherwise anything that is not a tool or armour."
  [items wanted]
  (let [wanted (some-> wanted set)]
    (first (filter #(if wanted (wanted (:name %)) (not (tool? (:name %)))) items))))

(defn deposit-check
  "A chest is known."
  [c]
  (boolean (chest-of {:common (ctx/mem c :common)} (:args c))))

(defn ^:async deposit-round
  "args {:chest pos-or-nil :items names-or-nil}. Walks to the chest (args, or
  the first :chest in common places) and deposits one stack per round: the
  named items, or everything but tools and armour. Done when nothing is left
  to put away. Three failed transfers (full, missing, unreachable) give up
  with a chest_unusable warn."
  [c]
  (let [chest (chest-of {:common (ctx/mem c :common)} (:args c))
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

(def deposit {:name :deposit :check deposit-check :round deposit-round})
