(ns jobs.lib.meal
  "Whether the sensed self has a meal to eat unnamed: what the hungry checks count on. A precious food (golden apple)
  counts only when its hp are worth more than it (cost.food/eat-precious?, the price jobs.survival.eat uses)."
  (:require [jobs.lib.cost.food :as food-cost]
            [jobs.lib.foods :as foods]))

(defn precious-worth?
  "Whether precious food name is worth eating by a meal that names no item: the body's hp alone (no threat known),
  healing by waiting at top-up food."
  [self name]
  (food-cost/eat-precious? {:health (.-health self) :absorption (.-absorption self)} name (>= (.-food self) foods/top-up-food)))

(defn eaten-unnamed?
  "Whether item counts as carried food for a meal that names no item: food, not harmful, not named-only, and precious
  only when worth it."
  [self item]
  (and (foods/edible? item)
       (not (contains? foods/named-only item))
       (or (not (contains? foods/precious item)) (precious-worth? self item))))

(defn carries-food?
  "Whether the sensed self carries something a meal would eat."
  ([self] (carries-food? self #{}))
  ([self reserved]
   (boolean (some #(eaten-unnamed? self %) (foods/carried-names self reserved)))))

(defn eat-now?
  "Health below :health (default 7), food below 20 and something a meal would eat carried."
  ([self args] (eat-now? self args #{}))
  ([self args reserved]
   (boolean (and (< (.-health self) (:health args foods/default-health)) (< (.-food self) 20) (carries-food? self reserved)))))

(defn wants-meal?
  "The hungry trigger's hunger test, shared with the night hold: hungry?, top-up? or eat-now? (reserved: names an unnamed meal spares)."
  [self reserved args]
  (boolean (or (foods/hungry? (.-food self) (.-health self) args)
               (foods/top-up? (.-food self) (.-health self) (foods/carried-names self reserved))
               (eat-now? self args reserved))))
