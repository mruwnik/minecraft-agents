(ns triggers.survival.low-health
  "The low-health trigger: a body too hurt for risky work gets food and rests where it is safe until it has healed."
  (:require [engine.contract :as contract]
            [engine.memory :as mem]
            [jobs.lib.body :as body]
            [jobs.lib.foods :as foods]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.shelter.sense :as sh-sense]))

(defn gave-up
  "The newest jobs.survival.rest ending (body memory :job-report) when it was nothing-possible: {:t :health :cause}, else nil."
  [view]
  (when-let [{:keys [t data]} (some #(when (= 'jobs.survival.rest (:job (:data %))) %) (rseq (mem/entries view :job-report)))]
    (when (= :nothing-possible (:kind data))
      {:t t :health (:health (:data (mem/latest view :rest-gave-up))) :cause (contract/cause-key data)})))

(defn waiting?
  "Whether the last rest gave up and its reason still stands: lifted when health is below what it was then, food is carried
  or a food source was learned since (cause :no-food)."
  [self view]
  (let [{:keys [t health cause]} (gave-up view)]
    (boolean (and t
                  (not (and health (< (.-health self) health)))
                  (not (and (= :no-food cause)
                            (or (foods/carries-food? self (reserve/spared view self))
                                (when-let [learned (:t (mem/latest view :food-source))] (> learned t)))))))))

(defn low-health
  "Holds while the body is alive with health below jobs.lib.body/risky-min-health, not burning or poisoned (their reflexes
  come first), not asleep (a bed already rests it), and not waiting? on a rest that gave up."
  [world memory _args]
  (let [self (.self world)
        health (.-health self)]
    (and (not (body/recovery-pending? memory :low-health))
         (pos? health)
         (< health (body/risky-min-health))
         (not (body/burning? world))
         (not (body/poisoned? world))
         (not (sh-sense/sleeping? world))
         (not (waiting? self memory)))))
