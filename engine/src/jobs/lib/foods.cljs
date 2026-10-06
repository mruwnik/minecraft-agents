(ns jobs.lib.foods
  "The food table, read from minecraft-data's foods for the version the body is connected with (hunger points and saturation),
  and the three short judgement lists on top of it: harmful, precious and named-only foods. The data names
  mob and fish buckets as foods (they are not eaten); those are left out."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [engine.game :as game]))

(def table-for
  "{item name {:points :saturation}} for every food minecraft-data lists for a version (memoised)."
  (memoize
   (fn [version]
     (->> (array-seq (.-foodsArray (minecraft-data version)))
          (remove #(str/ends-with? (.-name %) "_bucket"))
          (map (fn [f] [(.-name f) {:points (.-foodPoints f) :saturation (.-saturation f)}]))
          (into {})))))

(defn table [] (table-for @game/version))

(def harmful
  "Foods that hurt or only half feed (hunger, poison, a rotten stomach): eaten only with :allow-bad, last."
  #{"rotten_flesh" "spider_eye" "pufferfish" "poisonous_potato" "chicken"})

(def precious
  "Foods kept for an emergency: eaten only when named or when health is low."
  #{"golden_apple" "enchanted_golden_apple"})

(def named-only
  "Foods with an effect worse than hunger (a teleport, random effects): eaten only when named."
  #{"chorus_fruit" "suspicious_stew"})

(def low-health "Below this health (of 20) precious food is eaten unnamed." 10)

(defn food? [item] (contains? (table) item))

(defn points [item] (get-in (table) [item :points]))

(defn saturation [item] (get-in (table) [item :saturation] 0))

(defn edible?
  "A food worth carrying as food: any food that is not harmful."
  [item]
  (and (food? item) (not (contains? harmful item))))
