(ns triggers.survival.hungry
  "The hungry trigger, and the condition the get-food job shares with it."
  (:require [jobs.lib.body :as body]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.foods :as foods]
            [jobs.lib.shelter :as sh]
            [engine.memory :as mem]))

(a/defargs settings
  {::bake-wheat {:default 3 :doc "Carried wheat enough to bake a loaf." :spec (a/int-in 0 nil)}
   ::urgent-food {:default 2 :doc "At or below this food the trigger holds at :urgent (it cuts a job's comfort section) instead of its default :comfort." :spec (a/int-in 0 20)}
   ::rest-s {:default 600 :doc "Seconds the trigger rests after get-food found nothing, unless food comes to hand. Same as get-food's :ask-cooldown-ms." :spec (a/int-in 0 nil)}})

(defn gave-up-at
  "When get-food last ended for finding no food (its nothing-possible :no-food ending, as body memory :job-report
  keeps it), or nil."
  [view]
  (some (fn [{:keys [t data]}]
          (when (and (= 'jobs.survival.get-food (:job data))
                     (= :nothing-possible (:kind data))
                     (= :no-food (contract/cause-key data)))
            t))
        (rseq (mem/entries view :job-report))))

(defn refused-at
  "When get-food last refused every way as too dangerous (its :food-too-dangerous entry), or nil."
  [view]
  (:t (mem/latest view :food-too-dangerous)))

(defn bake-wheat [] (settings/get settings ::bake-wheat))

(defn wheat-carried [self]
  (reduce + 0 (keep #(when (= "wheat" (.-name %)) (.-count %)) (array-seq (.-inventory self)))))

(defn default-rest-s [] (settings/get settings ::rest-s))

(defn rest-s [self args]
  (let [rest (:rest-s args (default-rest-s))]
    (if (foods/starving? self args) (min rest (/ (foods/starving-rest-ms) 1000)) rest)))

(defn resting?
  "Whether the trigger rests: get-food found no food (gave-up-at), or while the body is fragile refused every way as too dangerous (refused-at), less than :rest-s ago, and since then nothing to
  eat came to hand: no food carried, too little wheat to bake, no :food-source entry learned after the give-up."
  [self view args]
  (let [gave-up (->> [(gave-up-at view) (when (foods/fragile? self) (refused-at view))] (remove nil?) seq (#(when % (apply max %))))
        learned (:t (mem/latest view :food-source))]
    (boolean (and gave-up
                  (< (- (:now view) gave-up) (* 1000 (rest-s self args)))
                  (not (foods/carries-food? self (foods/reserved view)))
                  (< (wheat-carried self) (bake-wheat))
                  (not (and learned (> learned gave-up)))))))

(def defaults {:food foods/default-food :health foods/default-health})

(defn hungry
  "Holds when hungry? says so, top-up? does (a hurt body below 18 food carrying common food), or eat-now? does.
  Holds at :urgent when eat-now? does (low health) or food is at most ::urgent-food (2), else at its :comfort level.
  Args: :food, :health, :rest-s.
  At night it holds only with food carried to eat: a search in the dark walks into mobs, and starvation stops at half a heart.
  A fragile body (foods/fragile?) with no food carried holds only on a calm day (jobs.lib.dawn/calm?): it waits sheltered.
  It rests (resting?) for :rest-s (600; at most foods/::starving-rest-ms, 60 s, when starving) after get-food found nothing, so a body with nothing at hand is not sent
  searching again every cooldown. The rest ends when food is carried, wheat to bake is, or a food source is learned."
  [world memory args]
  (let [self (.self world)
        reserved (foods/reserved memory)
        urgent? (or (foods/eat-now? self args reserved) (<= (.-food self) (settings/get settings ::urgent-food)))]
    (and (not (body/recovery-pending? memory :hungry))
         (or (foods/hungry? (.-food self) (.-health self) args)
             (foods/top-up? (.-food self) (.-health self) (foods/carried-names self reserved))
             (foods/eat-now? self args reserved))
         (not (resting? self memory args))
         (or (foods/carries-food? self reserved)
             (and (not (sh/night? world)) (or (not (foods/fragile? self)) (dawn/calm? world))))
         (if urgent? :urgent true))))
