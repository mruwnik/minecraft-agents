(ns jobs.lib.cost.armour
  "Armour, the one formula: vanilla's per hit. The armour points and toughness reduction, then the enchantment reduction
  (EPF: protection 1 a level, feather falling 3 for falls, blast 2 for explosions, projectile 2 for arrows, fire 2 for fire and burning). Toughness is 2 a diamond
  piece and 3 a netherite piece.
  Equipment: {part {:name :enchants [{:name :lvl}]}} for head torso legs feet, as cljs (keyword or string keys) or JS;
  other slots never count."
  (:require [clojure.string :as str]))

(def armour-by-piece
  "Armour points of each worn piece, by material and slot."
  {"leather" {"helmet" 1 "chestplate" 3 "leggings" 2 "boots" 1}
   "golden" {"helmet" 2 "chestplate" 5 "leggings" 3 "boots" 1}
   "chainmail" {"helmet" 2 "chestplate" 5 "leggings" 4 "boots" 1}
   "iron" {"helmet" 2 "chestplate" 6 "leggings" 5 "boots" 2}
   "diamond" {"helmet" 3 "chestplate" 8 "leggings" 6 "boots" 3}
   "netherite" {"helmet" 3 "chestplate" 8 "leggings" 6 "boots" 3}
   "turtle" {"helmet" 2}})

(def toughness-by-material {"diamond" 2 "netherite" 3})

(def epf-per-level {"protection" {:all 1} "feather_falling" {:fall 3} "blast_protection" {:explosion 2} "projectile_protection" {:projectile 2}
                    "fire_protection" {:fire 2 :burning 2}})

(defn piece-points [item-name]
  (let [[material piece] (str/split (or item-name "") #"_" 2)]
    (get-in armour-by-piece [material piece] 0)))

(defn piece-of [x]
  (cond
    (nil? x) nil
    (map? x) x
    :else {:name (.-name x) :enchants (some-> (.-enchants x) (js->clj :keywordize-keys true))}))

(defn equipment-of
  "{part piece} for head torso legs feet of equipment given as cljs (keyword or string keys) or JS; nil is {}."
  [eq]
  (into {} (for [part ["head" "torso" "legs" "feet"]
                 :let [x (if (map? eq) (or (get eq (keyword part)) (get eq part)) (some-> eq (aget part)))]
                 :when x]
             [part (piece-of x)])))

(defn armour-stats
  "{:points :toughness :enchants} of equipment (see the ns doc)."
  [equipment]
  (reduce (fn [acc [_ {:keys [name enchants]}]]
            (-> acc
                (update :points + (piece-points name))
                (update :toughness + (get toughness-by-material (first (str/split (str name) #"_")) 0))
                (update :enchants into enchants)))
          {:points 0 :toughness 0 :enchants []} (equipment-of equipment)))

(defn epf [enchants dtype]
  (min 20 (reduce + 0 (map (fn [{:keys [name id lvl level]}]
                             (let [per (get epf-per-level (str (or name id)))]
                               (* (or lvl level 1) (+ (get per :all 0) (get per dtype 0)))))
                           enchants))))

(defn after-armour
  "The damage left of threat (spread over hits, at least 1) after armour stats, for damage type dtype
  (:melee :projectile :explosion :fire :burning; :fire is standing in fire or lava, armour applies; :burning
  bypasses armour points like vanilla on_fire, only fire protection counts)."
  [{:keys [points toughness enchants]} dtype threat hits]
  (let [hit (/ threat (max 1 hits))
        armour (if (= dtype :burning)
                 0
                 (min 20 (max (/ points 5) (- points (/ (* 4 hit) (+ toughness 8))))))
        after (* hit (- 1 (/ armour 25)) (- 1 (/ (epf enchants dtype) 25)))]
    (* after (max 1 hits))))

(defn fall-damage
  "hp a fall of blocks costs a body with equipment (vanilla: a point a block past 3; armour points do not count,
  enchantments do: protection and feather falling)."
  [equipment blocks]
  (* (max 0 (js/Math.ceil (- blocks 3)))
     (- 1 (/ (epf (:enchants (armour-stats equipment)) :fall) 25))))

(def max-fall "The longest drop in blocks ever allowed." 16)

(defn fall-profile
  "How a body {:health :equipment :damage-budget} takes drops: {:fall-factor the share of the usual fall damage left (1:
  none reduced), :max-drop the longest drop in blocks the planner may take}: the longest (up to max-fall, at least 3)
  whose fall damage the budget (hp, jobs.lib.cost.health/damage-budget) pays for."
  [{:keys [equipment damage-budget]}]
  (let [budget (or damage-budget 0)]
    {:fall-factor (fall-damage equipment 4)
     :max-drop (or (last (take-while #(<= (fall-damage equipment %) budget) (range 4 (inc max-fall)))) 3)}))
