(ns triggers.survival.low-health
  "The low-health trigger: a body too hurt for risky work rests where it is safe until it has healed."
  (:require [jobs.lib.body :as body]
            [jobs.lib.shelter.sense :as sh-sense]))

(defn low-health
  "Holds while the body is alive with health below jobs.lib.body/risky-min-health, regenerates by waiting
  (jobs.lib.body/heals-by-rest?: food 18+, not burning or poisoned) and is not asleep (a bed already rests it). Food under
  18 is the hungry trigger's: only eating heals it."
  [world memory _args]
  (let [health (.-health (.self world))]
    (and (not (body/recovery-pending? memory :low-health))
         (pos? health)
         (< health (body/risky-min-health))
         (body/heals-by-rest? world)
         (not (sh-sense/sleeping? world)))))
