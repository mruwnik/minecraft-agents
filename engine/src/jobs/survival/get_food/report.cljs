(ns jobs.survival.get-food.report
  "The words get-food says when it found nothing."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.foods :as foods]
            [jobs.lib.util :as u]
            [jobs.survival.get-food.cook :as cook]
            [jobs.survival.get-food.sources :as sources]))

(defn nearest-known-source [c]
  (let [here (u/self-pos c)]
    (->> (ctx/entries c :food-source)
         (map :data)
         (sort-by #(u/dist here (:pos %)))
         first)))

(defn no-bake-reason
  "The reason of the live :no-bake entry, or nil."
  [c]
  (when (pos? (ctx/count-in c :no-bake cook/no-bake-ms))
    (:reason (:data (ctx/latest c :no-bake)))))

(defn harmful-carried-text
  "\"; carrying 2 chicken, 5 rotten_flesh, skipped on purpose (...)\" for the harmful foods in the inventory, or nil."
  [inventory]
  (let [bad (->> inventory (filter #(contains? foods/harmful (:name %))) (map (juxt :name :count)))]
    (when (seq bad)
      (str "; carrying " (str/join ", " (map (fn [[n k]] (str k " " n)) bad))
           ", skipped on purpose (raw or rotten food hurts; eat with :allow-bad via the eat job)"))))

(defn none-text
  "Why nothing was found: what was searched within radius, the animals passed over, the mobs in range when none
  qualified (around) and the nearest known source."
  [c {:keys [radius passed around explored]}]
  (let [near (nearest-known-source c)
        wheat (sources/carried-count c "wheat")
        reason (no-bake-reason c)]
    (str "no food: carried none" (harmful-carried-text (u/inventory (:primitives c))) ", searched for animals and ripe crops within " radius " blocks"
         (when explored (str ", explored: " explored))
         (when (and (>= wheat 3) reason)
           (str "; carrying " wheat " wheat but cannot bake (" reason ")"))
         (when (seq passed)
           (str "; passed over " (count passed) " animal" (when (> (count passed) 1) "s") " ("
                (str/join ", " (map (fn [a] (str (:name a) ": " (if (= :kept (:why a)) "kept for breeding" (name (:why a)))
                                         (when (:pos a) (str " at " (pr-str (:pos a)))))) passed)) ")"))
         (when (and (empty? passed) (seq around))
           (str "; nothing hunted; in range: " (str/join ", " (map (fn [a] (str (:name a) " (" (:kind a) ")" (when (:pos a) (str " at " (pr-str (:pos a)))))) around))))
         (if near
           (str "; nearest known source is a " (name (:kind near)) " at " (pr-str (:pos near))
                ", " (js/Math.round (u/dist (u/self-pos c) (:pos near))) " blocks away")
           "; no food source is known"))))
