(ns jobs.time.wait-for-light
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.cond :as cond]
            [jobs.lib.shelter.sense :as sh-sense]))

(def idle-ok true)

(def doc
  "Done once the body's cell is lit enough that monsters cannot spawn there (block light, or sky light less the
  darkening), whatever lit it: dawn, a torch. Waits (condition reason :dark) while it is dark, up to the job-time
  budget (::job-time-s). Needs nothing. Quote: the time till the sky lights the cell (sh-sense/light-in); ##Inf when
  no sky light reaches it, so only another light source ends that wait.")

(def target [])

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds the wait for light may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)

(def can-run? (cond/because :dark (cond/lit-here)))

(def can-start? ready/ok)

(defn done?
  "Done when the cell the body stands in is lit."
  [c]
  (let [p (:primitives c)]
    (if (sh-sense/lit-here? p (sh-sense/feet p))
      (contract/ok :lit "the cell is lit" :units :cells :done 1 :left 0)
      (contract/later :dark "the cell is dark" :units :cells :done 0 :left 1))))

(defn quote-of [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)]
    (cond
      (sh-sense/lit-here? p feet) {:uses {:job-time [0 0 0]}}
      :else (let [s (if (sh-sense/sky-light-at p feet) (or (sh-sense/light-in p feet) ##Inf) ##Inf)]
              {:uses {:job-time [0 s (if (and (pos? s) (settings/frozen?)) ##Inf s)]}}))))

(defn ^:async round [c] (ctx/end c :done nil))
