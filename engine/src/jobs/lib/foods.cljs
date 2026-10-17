(ns jobs.lib.foods
  "The food table, read from minecraft-data's foods for the version the body is connected with (hunger points and saturation),
  and the three short judgement lists on top of it: harmful, precious and named-only foods. The data names
  mob and fish buckets as foods (they are not eaten); those are left out."
  (:require [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [engine.game :as game]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::starving-rest-ms {:default 60000 :doc "The rest after a fruitless food search while starving (food 0, or hurt and hungry), for get-food's ask cooldown and the hungry trigger." :spec (a/num-in 0 nil)}
   ::reserve-eat-below {:default 3 :doc "At this much food or less bait reserved by a running animal job is eaten as well." :spec (a/int-in 0 20)}
   ::low-health {:default 10 :doc "Below this health (of 20) precious food is eaten unnamed." :spec (a/int-in 0 nil)}
   ::fragile-health {:default 6 :doc "At or below this health a body with no food carried walks out for food only on a calm day (jobs.lib.dawn/calm?) and never past a known hostile that would likely kill it (the hungry trigger, get-food)." :spec (a/num-in 0 20)}})

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

(defn stock-points
  "Hunger points the pack holds in food a meal would eat unnamed and that does not hurt; inv: [{:name :count}]."
  [inv]
  (reduce + 0 (for [{:keys [name count]} inv
                    :when (and (food? name) (not (contains? harmful name)) (not (contains? named-only name)))]
                (* count (points name)))))

(defn edible?
  "A food worth carrying as food: any food that is not harmful."
  [item]
  (and (food? item) (not (contains? harmful item))))

(def default-food "Hungry below this much food (of 20), plus one per missing hp. 7: at 6 or less the body cannot sprint." 7)

(def top-up-food
  "Natural regeneration needs food of at least this much (of 20)."
  18)

(defn hungry-line
  "The food below which a body of this health is hungry: :food (default 7) plus one per missing hp, at most top-up-food: a
  hurt body below 18 food does not heal, so the more hurt it is the sooner it looks for food."
  [health args]
  (min top-up-food (+ (:food args default-food) (- 20 health))))

(defn hungry?
  "Food is below the hungry-line. The hungry trigger's line."
  [food health args]
  (< food (hungry-line health args)))

;; ------------------------------------------------------------------ eating from the pack (the hungry trigger and get-food)

(def default-health
  "Below this health (of 20) carried food is eaten up to a full bar: saturation heals fastest on a full bar."
  7)

(defn starving-rest-ms [] (settings/get settings ::starving-rest-ms))

(defn fragile?
  "Health at or below ::fragile-health: one hit may kill."
  [self]
  (<= (.-health self) (settings/get settings ::fragile-health)))

(defn starving?
  "Food 0, or health below default-health while hungry: the rest after a fruitless search shortens."
  [self args]
  (or (<= (.-food self) 0)
      (and (< (.-health self) default-health) (hungry? (.-food self) (.-health self) args))))

(def rare-food
  "Foods kept for emergencies; never eaten just to top up."
  precious)

(defn top-up?
  "Health is below full, food is below top-up-food and a common food is
  carried (names: the carried item names): eat to regenerate like a player."
  [food health carried-names]
  (boolean (and (< health 20) (< food top-up-food)
                (some #(and (edible? %) (not (rare-food %))) carried-names))))

(defn reserved
  "The item names animal jobs have reserved as bait lately (jobs.lib.animals/reserve-bait!), from a memory view."
  [view]
  (into #{} (mapcat :foods) (map :data (mem/entries view :bait-reserve))))

(defn spared?
  "Whether an unnamed meal leaves name alone: it is in reserved and food is above ::reserve-eat-below."
  [food name reserved]
  (and (contains? reserved name) (> food (settings/get settings ::reserve-eat-below))))

(defn carried-names
  "Names of the carried items an unnamed meal may eat: not the reserved bait (names), unless nearly starving."
  ([self] (carried-names self #{}))
  ([self reserved]
   (->> (array-seq (.-inventory self))
        (map #(.-name %))
        (remove #(spared? (.-food self) % reserved)))))

(defn eaten-unnamed?
  "Whether a meal that names no item would eat item at health: food, not harmful, not named-only, and precious only
  at low health (the rules of jobs.survival.eat)."
  [health item]
  (and (edible? item)
       (not (contains? named-only item))
       (or (not (contains? precious item)) (< health (low-health)))))

(defn carries-food?
  "Whether the sensed self carries something a meal would eat."
  ([self] (carries-food? self #{}))
  ([self reserved]
   (boolean (some #(eaten-unnamed? (.-health self) %) (carried-names self reserved)))))

(defn eat-now?
  "Health below :health (default 7), food below 20 and something a meal would eat carried."
  ([self args] (eat-now? self args #{}))
  ([self args reserved]
   (boolean (and (< (.-health self) (:health args default-health)) (< (.-food self) 20) (carries-food? self reserved)))))

(def fish-items #{"cod" "salmon" "tropical_fish" "pufferfish"})

(defn fish-carried [p]
  (reduce + 0 (keep #(when (fish-items (:name %)) (:count %)) (u/inventory p))))
