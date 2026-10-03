(ns engine.triggers.died
  "The died trigger: a death the body has not yet decided about, still
  inside the drops' five minute despawn window."
  (:require [engine.memory :as mem]
            [engine.value :as value]))

(defn unrecovered-death
  "The latest :died entry when no :recovered entry is newer, else nil."
  [view]
  (let [died (mem/latest view :died)
        recovered (mem/latest view :recovered)]
    (when (and died (or (nil? recovered) (> (:t died) (:t recovered))))
      died)))

(def died
  "Holds while the latest :died entry is newer than the latest :recovered
  entry and younger than 5 minutes. The recover-drops job writes :recovered."
  {:name :died
   :when (fn [_world memory _args]
           (let [d (unrecovered-death memory)]
             (boolean (and d (< (- (:now memory) (:t d)) value/despawn-ms)))))
   :job '(jobs.survival.recover-drops)
   :args {}
   :persistence :cooldown
   :cooldown-s 30})
