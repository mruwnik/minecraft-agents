(ns engine.jobs.combat
  "Helpers the survival jobs share: reading hostiles off sensing and ranking
  the weapons carried."
  (:require [clojure.string :as str]
            [engine.jobs.util :as u]))

(def default-weapons
  "Item name substrings that count as weapons; _axe does not match pickaxes."
  ["_sword" "_axe"])

(def ranged-mobs
  "Hostiles that shoot or throw from a distance, so count from further away."
  #{"skeleton" "stray" "bogged" "pillager" "witch"})

(defn ranged? [e] (contains? ranged-mobs (.-name e)))

(defn hostiles
  "The hostile mobs within radius of the body, nearest first, as JS entities.
  opts: {:sight mode} :only keeps the ones the body can see (the `visible`
  field of sensing), :prefer lists the visible ones first, each group nearest
  first; without it sight is ignored. {:ranged-radius r} counts ranged mobs
  (ranged-mobs) out to r instead of radius."
  ([p radius] (hostiles p radius {}))
  ([p radius {:keys [sight ranged-radius]}]
   (let [rr (or ranged-radius radius)
         all (->> (array-seq (.entities p #js {:radius (max radius rr) :kind "hostile" :max 16}))
                  (filter #(<= (.-distance %) (if (ranged? %) rr radius))))]
     (case sight
       :only (filterv #(.-visible %) all)
       :prefer (into (filterv #(.-visible %) all) (remove #(.-visible %)) all)
       (vec all)))))

(defn creeper? [e]
  (or (true? (.-creeper e)) (= "creeper" (.-name e))))

(def material-rank
  {"netherite" 5 "diamond" 4 "iron" 3 "stone" 2 "golden" 1 "wooden" 0})

(defn weapon-score
  "Higher is better: the material, and swords over axes of the same material."
  [item-name]
  (+ (get material-rank (first (str/split item-name #"_")) 0)
     (if (str/ends-with? item-name "_sword") 0.5 0)))

(defn weapon? [weapons item-name]
  (boolean (some #(str/includes? item-name %) weapons)))

(defn best-weapon
  "The name of the best carried item matching one of the weapons substrings,
  or nil."
  [p weapons]
  (->> (u/inventory p)
       (map :name)
       (filter #(weapon? weapons %))
       (sort-by weapon-score >)
       first))
