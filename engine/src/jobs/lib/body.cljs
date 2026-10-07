(ns jobs.lib.body
  "What state the body is in, shared by the burning, died and hostile-near triggers and their jobs: on fire, dead,
  an unrecovered death. Memory entries :died, :recovered and :respawned are written by the engine and
  jobs.survival.recover-drops."
  (:require [engine.memory :as mem]))

(defn burning?
  "True when the sensed self (a JS object from primitives.self()) is on fire
  or in lava, and has no fire_resistance effect (which makes both harmless)."
  [self]
  (boolean (and (or (.-onFire self) (.-inLava self))
                (not-any? #(= "fire_resistance" (.-name %)) (array-seq (.-effects self))))))

(defn unrecovered-death
  "The latest :died entry when no :recovered entry is newer, else nil."
  [view]
  (let [died (mem/latest view :died)
        recovered (mem/latest view :recovered)]
    (when (and died (or (nil? recovered) (> (:t died) (:t recovered))))
      died)))

(defn respawned-since?
  "True when a :respawned entry newer than death exists: the body is alive again."
  [view death]
  (let [respawned (mem/latest view :respawned)]
    (boolean (and respawned (> (:t respawned) (:t death))))))

(defn dead?
  "True between a :died entry and the :respawned entry that follows it."
  [view]
  (let [death (mem/latest view :died)]
    (boolean (and death (not (respawned-since? view death))))))
