(ns triggers.survival.low-health
  "The low-health trigger: a body too hurt for risky work gets food and rests where it is safe until it has healed."
  (:require [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.shelter.sense :as sh-sense]))

(a/defargs settings
  {::retry-s {:default 300 :doc "Seconds the trigger waits after a rest ended without healing (nothing-possible), so a rest that cannot heal here is not run again every cooldown." :spec (a/num-in 0 nil)}})

(defn gave-up-at
  "When jobs.survival.rest last ended nothing-possible (body memory :job-report), or nil."
  [view]
  (some (fn [{:keys [t data]}]
          (when (and (= 'jobs.survival.rest (:job data)) (= :nothing-possible (:kind data))) t))
        (rseq (mem/entries view :job-report))))

(defn low-health
  "Holds while the body is alive with health below jobs.lib.body/risky-min-health, not burning or poisoned (their reflexes
  come first), not asleep (a bed already rests it), and no rest ended nothing-possible within ::retry-s."
  [world memory _args]
  (let [health (.-health (.self world))
        gave-up (gave-up-at memory)]
    (and (not (body/recovery-pending? memory :low-health))
         (pos? health)
         (< health (body/risky-min-health))
         (not (body/burning? world))
         (not (body/poisoned? world))
         (not (sh-sense/sleeping? world))
         (not (and gave-up (< (- (:now memory) gave-up) (* 1000 (settings/get settings ::retry-s))))))))
