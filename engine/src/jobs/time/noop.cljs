(ns jobs.time.noop
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.primitive :as primitive]))

(def idle-ok true)

(def doc
  "An intentional pause: hold the body still (holding :why) for :seconds, in :wait acts of at most prim/wait-max-ms
  inside one round. A job pauses by calling it as a child (jobs.lib.child/pause!): reflexes still cut in, and the time
  counts against both the caller's job time and its wall time. A pause writes its deadline once (job memory :until), so a cut pause waits only
  what is left and a long one adds no events per step. Needs nothing.
  Its hold stays until the caller's round ends or clears it. Done {:waited-s s}. Quote: :seconds.")

(a/defargs args
  {:seconds {:doc "how long to pause" :spec (a/num-in 0 nil) :default 0}
   :why {:doc "the hold reason (job.holding)" :spec (a/or-of keyword? string?) :default :pausing}})

(def target [])

(a/defargs settings
  {::job-time-s {:default 3600 :doc "Seconds one pause may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)

(defn total-ms [c] (* 1000 (:seconds (:args c) 0)))

(defn left-ms [c]
  (let [until (:until (ctx/mem c))]
    (if until (max 0 (min (total-ms c) (- until (ctx/now c)))) (total-ms c))))

(defn done?
  "Done once the pause has waited its time (a pause of none is done at once)."
  [c]
  (let [data [:units :ms :done (- (total-ms c) (left-ms c)) :left (left-ms c) :result {:waited-s (:seconds (:args c) 0)}]]
    (if (and (zero? (left-ms c)) (or (zero? (total-ms c)) (:until (ctx/mem c))))
      (apply contract/ok :waited "the pause is over" data)
      (apply contract/later :waiting "the pause is not over" data))))

(defn quote-of [c] (let [s (:seconds (:args c) 0)] {:uses {:job-time [s s s]}}))

(defn ^:async round [c]
  (let [why (:why (:args c) :pausing)]
    (ctx/hold-still! c why)
    (when-not (:until (ctx/mem c))
      (ctx/update-mem! c assoc :until (+ (ctx/now c) (total-ms c))))
    (loop [owed (left-ms c)]
      (if (pos? owed)
        (let [ms (min owed (primitive/js-setting "prim/wait-max-ms" 10000))]
          (await (ctx/act c :wait #js {:ms ms :why why}))
          (recur (min (- owed ms) (left-ms c))))
        (ctx/end c :done nil {:waited-s (:seconds (:args c) 0)})))))
