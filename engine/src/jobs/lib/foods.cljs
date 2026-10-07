(ns jobs.lib.foods
  "The food table, read from minecraft-data's foods for the version the body is connected with (hunger points and saturation),
  and the three short judgement lists on top of it: harmful, precious and named-only foods. The data names
  mob and fish buckets as foods (they are not eaten); those are left out."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [engine.game :as game]))

(a/defargs settings
  {::low-health {:default 10 :doc "Below this health (of 20) precious food is eaten unnamed." :spec (a/int-in 0 nil)}})

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

(defn low-health [] (settings/get settings ::low-health))

(defn food? [item] (contains? (table) item))

(defn points [item] (get-in (table) [item :points]))

(defn saturation [item] (get-in (table) [item :saturation] 0))

(defn edible?
  "A food worth carrying as food: any food that is not harmful."
  [item]
  (and (food? item) (not (contains? harmful item))))

(def default-food "Hungry below this much food (of 20), plus one per missing hp." 6)

(def top-up-food
  "Natural regeneration needs food of at least this much (of 20)."
  18)

(defn hungry?
  "Food is below :food (default 6) plus one per missing hp, at most top-up-food: a hurt body below 18 food does not
  heal, so the more hurt it is the sooner it looks for food. The hungry trigger's line."
  [food health args]
  (< food (min top-up-food (+ (:food args default-food) (- 20 health)))))

;; ------------------------------------------------------------------ eating from the pack (the hungry trigger and get-food)

(def default-health
  "Below this health (of 20) carried food is eaten up to a full bar: saturation heals fastest on a full bar."
  7)

(def rare-food
  "Foods kept for emergencies; never eaten just to top up."
  precious)

(defn top-up?
  "Health is below full, food is below top-up-food and a common food is
  carried (names: the carried item names): eat to regenerate like a player."
  [food health carried-names]
  (boolean (and (< health 20) (< food top-up-food)
                (some #(and (edible? %) (not (rare-food %))) carried-names))))

(defn carried-names [self]
  (map #(.-name %) (array-seq (.-inventory self))))

(defn eaten-unnamed?
  "Whether a meal that names no item would eat item at health: food, not harmful, not named-only, and precious only
  at low health (the rules of jobs.survival.eat)."
  [health item]
  (and (edible? item)
       (not (contains? named-only item))
       (or (not (contains? precious item)) (< health (low-health)))))

(defn carries-food?
  "Whether the sensed self carries something a meal would eat."
  [self]
  (boolean (some #(eaten-unnamed? (.-health self) %) (carried-names self))))

(defn eat-now?
  "Health below :health (default 7), food below 20 and something a meal would eat carried."
  [self args]
  (boolean (and (< (.-health self) (:health args default-health)) (< (.-food self) 20) (carries-food? self))))
