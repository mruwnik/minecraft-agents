(ns triggers.survival.burning
  "The burning trigger: the body is on fire or in lava and not fire resistant, or a water pour is left to scoop."
  (:require [engine.memory :as mem]))

(defn burning?
  "True when the sensed self (a JS object from primitives.self()) is on fire
  or in lava, and has no fire_resistance effect (which makes both harmless)."
  [self]
  (boolean (and (or (.-onFire self) (.-inLava self))
                (not-any? #(= "fire_resistance" (.-name %)) (array-seq (.-effects self))))))

(defn burning
  "Holds when the body is on fire or in lava and has no fire_resistance
  effect, or when memory holds an unexpired :extinguish-pour (a run cut after
  the pour left its water source to scoop). A danger reflex: no cooldown (and
  extinguish has no backoff), so a job that ends with the body still burning is fired again at once."
  [world memory _args]
  (boolean (or (burning? (.self world))
               (mem/latest memory :extinguish-pour))))
