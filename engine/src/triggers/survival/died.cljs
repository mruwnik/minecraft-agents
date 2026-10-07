(ns triggers.survival.died
  "The died trigger: a death the body has not yet decided about, still
  inside the drops' five minute despawn window."
  (:require [jobs.lib.body :as body]
            [engine.game :as game]))

(defn died
  "Holds while the latest :died entry is newer than the latest :recovered
  entry, younger than the despawn window (6000 game ticks) and followed by a :respawned entry (a dead
  body cannot walk). The recover-drops job writes :recovered."
  [_world memory _args]
  (let [d (body/unrecovered-death memory)]
    (boolean (and d
                  (body/respawned-since? memory d)
                  (< (game/ticks-since d memory) game/despawn-ticks)))))
