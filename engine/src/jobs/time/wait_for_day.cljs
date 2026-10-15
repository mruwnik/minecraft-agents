(ns jobs.time.wait-for-day
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.shelter :as sh]))

(def idle-ok true)

(def doc
  "Done once it is day. Waits (condition reason :day-not-come) at night, then ends :done. Needs nothing. Quote: the
  ticks left of the night at the live game rate (##Inf most while the server's tick is frozen).")

(def target [])

(a/defargs settings
  {::time-s {:default 1800 :doc "Seconds the wait for day may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::time-s}})

(def conditions [[:holds '(daytime) :day-not-come]])

(defn done?
  "Done when it is day."
  [c]
  (let [day? (boolean (.-isDay (.self (:primitives c))))]
    {:done? day? :units :days :left (if day? 0 1)}))

(defn quote-of [c] (sh/night-left-quote c))

(defn ^:async round [c] (ctx/end c :done nil))
