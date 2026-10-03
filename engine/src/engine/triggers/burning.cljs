(ns engine.triggers.burning
  "The burning trigger: the body is on fire or in lava and not fire resistant.")

(defn burning?
  "True when the sensed self (a JS object from primitives.self()) is on fire
  or in lava, and has no fire_resistance effect (which makes both harmless)."
  [self]
  (boolean (and (or (.-onFire self) (.-inLava self))
                (not-any? #(= "fire_resistance" (.-name %)) (array-seq (.-effects self))))))

(def burning
  "Holds when the body is on fire or in lava and has no fire_resistance
  effect. After its job ends with the
  body still burning, fires again after 2 s."
  {:name :burning
   :when (fn [world _memory _args] (burning? (.self world)))
   :job '(jobs.survival.extinguish)
   :args {}
   :persistence :cooldown
   :cooldown-s 2})
