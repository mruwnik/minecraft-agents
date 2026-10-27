(ns jobs.survival.get-food.report
  "The words get-food says when it found nothing."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.foods :as foods]
            [jobs.lib.util :as u]
            [jobs.survival.get-food.sources :as sources]))

(defn nearest-known-source [c]
  (let [here (u/self-pos c)]
    (->> (ctx/entries c :food-source)
         (map :data)
         (sort-by #(u/dist here (:pos %)))
         first)))

(defn no-bake-reason
  "Why the bake failed in this job, or nil."
  [c]
  (:no-bake (ctx/mem c)))

(defn harmful-carried-text
  "\"; carrying 2 chicken, 5 rotten_flesh, skipped on purpose (...)\" for the harmful foods in the inventory, or nil."
  [inventory]
  (let [bad (->> inventory (filter #(contains? foods/harmful (:name %))) (map (juxt :name :count)))]
    (when (seq bad)
      (str "; carrying " (str/join ", " (map (fn [[n k]] (str k " " n)) bad))
           ", skipped on purpose (raw or rotten food hurts; eat with :allow-bad via the eat job)"))))

(defn explored-text
  "What the exploring search covered: \"2 found (cow at [1 64 2], wheat at [3 64 4]), 3 legs, time\"; the finds are named so a
  find the hunt then could not use is visible. A search cut before its first look or leg says so: it searched nothing."
  [res]
  (let [found (:found res)
        {:keys [legs scans]} (:coverage res)]
    (if (and (empty? found) (zero? (or legs 0)) (zero? (or scans 0)))
      (str "not searched: the search ended" (when (:why res) (str " (" (name (:why res)) ")")) " before its first look")
      (str (count found) " found"
           (when (seq found)
             (str " (" (str/join ", " (map (fn [{:keys [what pos]}] (str (name (or what "?")) (when pos (str " at " (pr-str pos))))) (take 4 found))) ")"))
           ", " (or legs 0) " legs"
           (when (:why res) (str ", " (name (:why res))))))))

(defn animals-text
  "The animals passed over (and why), or with none the mobs in range (around), as a text starting \"; \"; nil when neither."
  [passed around]
  (cond
    (seq passed)
    (str "; passed over " (count passed) " animal" (when (> (count passed) 1) "s") " ("
         (str/join ", " (map (fn [a] (str (:name a) ": " (if (= :kept (:why a)) "kept for breeding" (name (:why a)))
                                          (when (:by a) (str " (" (:by a) ")"))
                                          (when (:pos a) (str " at " (pr-str (:pos a)))))) passed)) ")")
    (seq around)
    (str "; nothing hunted; in range: " (str/join ", " (map (fn [a] (str (:name a) " (" (:kind a) ")" (when (:pos a) (str " at " (pr-str (:pos a)))))) around)))))

(defn none-text
  "Why nothing was found: what was searched within radius, the animals passed over, the mobs in range when none
  qualified (around) and the nearest known source."
  [c {:keys [radius passed around explored]}]
  (let [near (nearest-known-source c)
        wheat (sources/carried-count c "wheat")
        reason (no-bake-reason c)]
    (str "no food: carried none" (harmful-carried-text (u/inventory (:primitives c))) ", searched for animals and ripe crops within " (js/Math.round radius) " blocks"
         (when explored (str ", explored: " explored))
         (when (and (>= wheat 3) reason)
           (str "; carrying " wheat " wheat but cannot bake (" reason ")"))
         (when-let [f (seq (vals (:foreign-food (ctx/mem c))))]
           (str "; left alone food in another's area (only a caller's :ignore-zones? takes it): " (str/join ", " f)))
         (animals-text passed around)
         (when-let [{:keys [pos why]} (:noted-lost (ctx/mem c))]
           (str "; an animal noted at " (pr-str pos) " was not reached: " why))
         (if near
           (str "; nearest known source is a " (name (:kind near)) " at " (pr-str (:pos near))
                ", " (js/Math.round (u/dist (u/self-pos c) (:pos near))) " blocks away")
           (when-not (:noted-lost (ctx/mem c)) "; no food source is known")))))
