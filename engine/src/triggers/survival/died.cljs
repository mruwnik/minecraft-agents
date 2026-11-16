(ns triggers.survival.died
  "The died trigger: a death the body has not yet decided about, still
  inside the drops' five minute despawn window, whose decline for a danger is not remembered, with no real danger near."
  (:require [jobs.lib.body :as body]
            [jobs.lib.hostile-loss :as loss]
            [engine.game :as game]))

(defn died
  "Holds while a :died entry no :recovered covers is younger than the despawn window (6000 game ticks) and followed
  by a :respawned entry (a dead body cannot walk). The recover-drops job writes :recovered. Not for a death it declined
  for a danger while that is remembered (jobs.lib.body/danger-hold): no other job is cut to weigh the same danger again.
  Not while a real danger is near (jobs.lib.hostile-loss/hostile-near?, args as the hostile-near trigger's): that is the
  hostile reflex's to answer, and a trip started now would walk back to the mobs a flight is taking the body from."
  [world memory args & [plans]]
  (boolean (and (some #(and (body/respawned-since? memory %)
                            (< (game/ticks-since % memory) game/despawn-ticks))
                      (body/actionable-deaths memory))
                (not (loss/hostile-near? (loss/trigger-ctx world plans) memory args)))))
