(ns triggers.survival.needs-weapon
  "The needs-weapon trigger: by day, an awake body carrying no weapon (jobs.lib.combat/default-weapons) gets a sword
  (jobs.items.obtain, the best it can make: iron, else stone, else wooden), so a mob it meets is fought, not fled from
  with the fist."
  (:require [jobs.lib.body :as body]
            [jobs.lib.combat :as combat]
            [jobs.lib.shelter :as sh]))

(def defaults {:any-of ["iron_sword" "stone_sword" "wooden_sword"] :count 1 :explore true})

(defn needs-weapon
  "Holds by day, alive and awake, when no carried item is a weapon."
  [p view _args]
  (boolean
   (and (not (body/dead? view))
        (not (sh/night? p))
        (not (sh/sleeping? p))
        (nil? (combat/best-weapon p combat/default-weapons)))))
