(ns jobs.survival.eat
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Eat the best food carried, one item per round, until food reaches :until
  or nothing edible is left. Rotten flesh, spider eyes, pufferfish and
  poisonous potatoes are not food unless :allow-bad is set. Writes a :fed
  entry (item, food after) for each meal.")

(def args
  {:item {:doc "the food to eat; the best carried when nil" :default nil}
   :until {:doc "keep eating while food is below this (of 20)" :default 18}
   :allow-bad {:doc "also eat the harmful foods when nothing else is carried" :default false}})

(def food-points
  "Hunger points restored, by item name."
  {"cooked_beef" 8 "cooked_porkchop" 8 "pumpkin_pie" 8 "rabbit_stew" 10
   "cooked_mutton" 6 "cooked_chicken" 6 "cooked_salmon" 6 "mushroom_stew" 6
   "beetroot_soup" 6 "golden_carrot" 6 "cooked_rabbit" 5 "cooked_cod" 5
   "bread" 5 "baked_potato" 5 "apple" 4 "carrot" 3 "beef" 3 "porkchop" 3
   "rabbit" 3 "mutton" 2 "chicken" 2 "cod" 2 "salmon" 2 "melon_slice" 2
   "sweet_berries" 2 "glow_berries" 2 "cookie" 2 "beetroot" 1 "potato" 1
   "dried_kelp" 1})

(def bad-food-points
  "Harmful foods, eaten only with :allow-bad."
  {"rotten_flesh" 4 "spider_eye" 2 "pufferfish" 1 "poisonous_potato" 2})

(def edible (set (keys food-points)))

(def fed-policy {:cap 20 :ttl (* 6 60 60 1000)})

(defn points
  "Hunger points of item, or nil when it is not eaten under allow-bad."
  [allow-bad item]
  (or (food-points item) (when allow-bad (bad-food-points item))))

(defn best-food
  "The name of the carried item with the most hunger points (ties by
  carrying order), or nil. Only item when given."
  [inventory allow-bad item]
  (->> inventory
       (map :name)
       (filter #(or (nil? item) (= item %)))
       (filter #(points allow-bad %))
       (sort-by #(- (points allow-bad %)))
       first))

(defn carried-best [c]
  (let [{:keys [item allow-bad]} (:args c)]
    (best-food (u/inventory (:primitives c)) allow-bad item)))

(defn check [c]
  (and (< (.-food (.self (:primitives c))) (:until (:args c)))
       (some? (carried-best c))))

(defn ^:async round [c]
  (let [{:keys [until]} (:args c)
        best (carried-best c)]
    (if (or (nil? best) (>= (.-food (.self (:primitives c))) until))
      :done
      (let [_ (await (ctx/act c :equip #js {:item best}))
            r (await (ctx/act c :eat #js {:item best}))]
        (if (not= "ate" (.-status r))
          :done
          (do (ctx/remember! c :fed {:item best :food (.-food r)} fed-policy)
              (if (or (>= (.-food r) until) (nil? (carried-best c)))
                :done
                :continue)))))))
