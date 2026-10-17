(ns jobs.time.wait-for-day
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cond :as cond]
            [jobs.lib.shelter.clock :as sh-clock]))

(def idle-ok true)

(def doc
  "Done once it is day. Waits (condition reason :day-not-come) at night, then ends :done. Needs nothing. Quote: the
  ticks left of the night at the live game rate (##Inf most while the server's tick is frozen).")

(def target [])

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds the wait for day may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)

(def can-run? (cond/because :day-not-come (cond/daytime)))

(def can-start? ready/ok)

(defn done?
  "Done when it is day."
  [c]
  (let [day? (boolean (.-isDay (.self (:primitives c))))]
    (if day?
      (contract/ok :day "it is day" :units :days :done 1 :left 0)
      (contract/later :day-not-come "it is not day yet" :units :days :done 0 :left 1))))

(defn quote-of [c] (sh-clock/night-left-quote c))

(defn ^:async round [c] (ctx/end c :done nil))
