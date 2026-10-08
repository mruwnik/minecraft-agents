(ns jobs.lib.body
  "What state the body is in, shared by the burning, died and hostile-near triggers and their jobs: on fire, dead,
  an unrecovered death. Memory entries :died, :recovered and :respawned are written by the engine and
  jobs.survival.recover-drops."
  (:require [engine.memory :as mem]
            [jobs.lib.util :as u]))

(def fires #{"fire" "soul_fire"})

(defn burning?
  "True when the body (primitives p) is on fire, in lava or in a fire block at its feet or head (rain puts the on-fire
  flag out every tick, the fire block still hurts), and has no fire_resistance effect (which makes all three harmless)."
  [p]
  (let [self (.self p)
        pos (.-pos self)
        feet {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))}]
    (boolean (and (or (.-onFire self) (.-inLava self)
                      (contains? fires (u/feel-name p feet))
                      (contains? fires (u/feel-name p (update feet :y inc))))
                  (not-any? #(= "fire_resistance" (.-name %)) (array-seq (.-effects self)))))))

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
