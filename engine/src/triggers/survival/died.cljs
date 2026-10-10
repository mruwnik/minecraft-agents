(ns triggers.survival.died
  "The died trigger: a death the body has not yet decided about, still
  inside the drops' five minute despawn window."
  (:require [jobs.lib.body :as body]
            [engine.game :as game]))

(defn died
  "Holds while a :died entry no :recovered covers is younger than the despawn window (6000 game ticks) and followed
  by a :respawned entry (a dead body cannot walk). The recover-drops job writes :recovered."
  [_world memory _args]
  (boolean (some #(and (body/respawned-since? memory %)
                       (< (game/ticks-since % memory) game/despawn-ticks))
                 (body/open-deaths memory))))
