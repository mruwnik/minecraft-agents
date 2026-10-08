(ns triggers.survival.hungry
  "The hungry trigger, and the condition the get-food job shares with it."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.foods :as foods]
            [engine.memory :as mem]))

(a/defargs settings
  {::bake-wheat {:default 3 :doc "Carried wheat enough to bake a loaf." :spec (a/int-in 0 nil)}
   ::urgent-food {:default 2 :doc "At or below this food the trigger holds at :urgent (it cuts a job's comfort section) instead of its default :comfort." :spec (a/int-in 0 20)}
   ::rest-s {:default 600 :doc "Seconds the trigger rests after get-food found nothing, unless food comes to hand. Same as get-food's :ask-cooldown-ms." :spec (a/int-in 0 nil)}})
(defn bake-wheat [] (settings/get settings ::bake-wheat))

(defn wheat-carried [self]
  (reduce + 0 (keep #(when (= "wheat" (.-name %)) (.-count %)) (array-seq (.-inventory self)))))

(defn default-rest-s [] (settings/get settings ::rest-s))

(defn resting?
  "Whether the trigger rests: get-food gave up (a :hungry entry, written with the food.none warn) less than :rest-s
  ago, and since then nothing to eat came to hand: no food carried, too little wheat to bake, no :food-source entry
  learned after the give-up."
  [self view args]
  (let [gave-up (:t (mem/latest view :hungry))
        learned (:t (mem/latest view :food-source))]
    (boolean (and gave-up
                  (< (- (:now view) gave-up) (* 1000 (:rest-s args (default-rest-s))))
                  (not (foods/carries-food? self))
                  (< (wheat-carried self) (bake-wheat))
                  (not (and learned (> learned gave-up)))))))

(def defaults {:food foods/default-food :health foods/default-health})

(defn hungry
  "Holds when hungry? says so, top-up? does (a hurt body below 18 food carrying common food), or eat-now? does.
  Holds at :urgent when eat-now? does (low health) or food is at most ::urgent-food (2), else at its :comfort level.
  Args: :food, :health, :rest-s.
  It rests (resting?) for :rest-s (600) after get-food found nothing, so a body with nothing at hand is not sent
  searching again every cooldown. The rest ends when food is carried, wheat to bake is, or a food source is learned."
  [world memory args]
  (let [self (.self world)
        urgent? (or (foods/eat-now? self args) (<= (.-food self) (settings/get settings ::urgent-food)))]
    (and (or (foods/hungry? (.-food self) (.-health self) args)
             (foods/top-up? (.-food self) (.-health self) (foods/carried-names self))
             (foods/eat-now? self args))
         (not (resting? self memory args))
         (if urgent? :urgent true))))
