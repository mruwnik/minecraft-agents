(ns engine.triggers.died
  "The died trigger: a death the body has not yet decided about, still
  inside the drops' five minute despawn window."
  (:require [engine.memory :as mem]
            [engine.game :as game]))

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

(defn died
  "Holds while the latest :died entry is newer than the latest :recovered
  entry, younger than 5 minutes and followed by a :respawned entry (a dead
  body cannot walk). The recover-drops job writes :recovered."
  [_world memory _args]
  (let [d (unrecovered-death memory)]
    (boolean (and d
                  (respawned-since? memory d)
                  (< (- (:now memory) (:t d)) game/despawn-ms)))))
