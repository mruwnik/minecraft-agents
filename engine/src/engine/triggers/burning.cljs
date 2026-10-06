(ns engine.triggers.burning
  "The burning trigger: the body is on fire or in lava and not fire resistant.")

(defn burning?
  "True when the sensed self (a JS object from primitives.self()) is on fire
  or in lava, and has no fire_resistance effect (which makes both harmless)."
  [self]
  (boolean (and (or (.-onFire self) (.-inLava self))
                (not-any? #(= "fire_resistance" (.-name %)) (array-seq (.-effects self))))))

(defn burning
  "Holds when the body is on fire or in lava and has no fire_resistance
  effect. A danger reflex: no cooldown (and extinguish has no backoff), so
  a job that ends with the body still burning is fired again at once."
  [world _memory _args]
  (burning? (.self world)))
