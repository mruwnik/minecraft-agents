(ns engine.jobs.combat
  "Helpers the survival jobs share: reading hostiles off sensing and ranking
  the weapons carried."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
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
  {"netherite" 5 "diamond" 4 "iron" 3 "stone" 2 "copper" 2 "golden" 1 "wooden" 0})

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

(defn ^:async equip-best!
  "Hold weapon in hand unless it is already held; nil does nothing."
  [c weapon]
  (when (and weapon (not= weapon (.-held (.self (:primitives c)))))
    (await (ctx/act c :equip #js {:item weapon :dest "hand"}))))

(def axe-gap-ms
  {"wooden_axe" 1250 "stone_axe" 1250 "iron_axe" 1112 "golden_axe" 1000 "diamond_axe" 1000 "netherite_axe" 1000})

(def min-gap-ms
  "Mobs ignore damage for 0.5 s after a hit, so a swing sooner than this is wasted."
  500)

(defn attack-gap-ms
  "The full-strength attack cooldown, in ms, of the held item (nil: a fist),
  never below min-gap-ms."
  [item-name]
  (max min-gap-ms
       (cond
         (and item-name (str/ends-with? item-name "_sword")) 625
         :else (get axe-gap-ms item-name min-gap-ms))))

(def mob-max-health
  "Full health of the common hostiles; unknown ones count as 20."
  {"zombie" 20 "husk" 20 "drowned" 20 "zombie_villager" 20 "skeleton" 20 "stray" 20 "bogged" 16
   "spider" 16 "cave_spider" 12 "creeper" 20 "witch" 26 "pillager" 24 "slime" 16 "silverfish" 8
   "endermite" 8 "phantom" 20 "vindicator" 24 "enderman" 40})

(def weapon-damage-by-name
  {"wooden_sword" 4 "golden_sword" 4 "stone_sword" 5 "iron_sword" 6 "diamond_sword" 7 "netherite_sword" 8
   "wooden_axe" 7 "golden_axe" 7 "stone_axe" 9 "iron_axe" 9 "diamond_axe" 9 "netherite_axe" 10})

(defn weapon-damage
  "Damage of one hit with item-name (a fist, 1, for nil or an unknown item)."
  [item-name]
  (get weapon-damage-by-name item-name 1))

(defn remaining-health
  "What is left of a mob: its reported :health when known, else its full
  health less :hits times :damage (armour ignored, so an estimate)."
  [{:keys [name hits damage health]}]
  (if (number? health)
    health
    (- (get mob-max-health name 20) (* (or hits 0) damage))))

(defn nearly-dead?
  "Whether one more hit of damage likely kills the mob."
  [m]
  (<= (remaining-health m) (:damage m)))

;; ---- the odds of a fight

(def armour-by-piece
  "Armour points of each worn piece, by material and slot; a turtle shell helmet is 2."
  {"leather" {"helmet" 1 "chestplate" 3 "leggings" 2 "boots" 1}
   "golden" {"helmet" 2 "chestplate" 5 "leggings" 3 "boots" 1}
   "chainmail" {"helmet" 2 "chestplate" 5 "leggings" 4 "boots" 1}
   "iron" {"helmet" 2 "chestplate" 6 "leggings" 5 "boots" 2}
   "diamond" {"helmet" 3 "chestplate" 8 "leggings" 6 "boots" 3}
   "netherite" {"helmet" 3 "chestplate" 8 "leggings" 6 "boots" 3}
   "turtle" {"helmet" 2}})

(defn piece-points [item-name]
  (let [[material piece] (str/split (or item-name "") #"_" 2)]
    (get-in armour-by-piece [material piece] 0)))

(defn armour-points
  "The armour points worn, from a primitives self's equipment (JS {head torso legs feet} of {name}); nil is 0."
  [equipment]
  (if equipment
    (reduce + (map #(piece-points (some-> (aget equipment %) .-name)) ["head" "torso" "legs" "feet"]))
    0))

(def mob-dps
  "Damage a mob deals a body per second while it can hit it (normal difficulty, about one hit a second for melee,
  an arrow every second or two for a skeleton); unknown ones count as 3."
  {"zombie" 3 "husk" 3 "drowned" 3 "zombie_villager" 3 "skeleton" 2 "stray" 2 "bogged" 2 "spider" 2 "cave_spider" 3
   "witch" 3 "pillager" 2 "vindicator" 8 "slime" 2 "silverfish" 1 "endermite" 2 "phantom" 2 "enderman" 7})

(def walk-speed "Blocks a second a body closes on a mob it walks up to." 4)

(defn fight-damage
  "The damage a body expects to take fighting mobs [{:name :distance :hits :health}] nearest first with weapon (an item
  name, nil a fist) wearing armour points: the mobs are killed one at a time, each taking ceil(what is left / the
  weapon's damage) swings at the weapon's attack gap, and every mob hits at its mob-dps until it dies. A ranged one also
  shoots while the body walks up to it (distance less a reach of 3, at walk-speed). Armour takes 4% a point off, at most
  80%."
  [{:keys [weapon armour mobs]}]
  (let [dmg (weapon-damage weapon)
        gap-s (/ (attack-gap-ms weapon) 1000)
        approach (fn [m] (if (ranged? #js {:name (:name m)}) (/ (max 0 (- (:distance m 0) 3)) walk-speed) 0))
        kill-s (fn [m] (* gap-s (js/Math.ceil (/ (max 0 (remaining-health (assoc m :damage dmg))) dmg))))
        ends (rest (reductions + 0 (map #(+ (approach %) (kill-s %)) mobs)))
        taken (reduce + (map (fn [m end] (* (get mob-dps (:name m) 3) end)) mobs ends))]
    (/ (* taken (- 100 (* 4 (min 20 (or armour 0))))) 100)))
