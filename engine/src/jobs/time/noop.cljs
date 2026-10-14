(ns jobs.time.noop
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.primitive :as primitive]))

(def idle-ok true)

(def doc
  "An intentional pause: hold the body still (holding :why) for :seconds, in :wait acts of at most prim/wait-max-ms
  inside one round. A job pauses by calling it as a child (jobs.lib.child/pause!): reflexes still cut in, and the time
  counts on the caller's report and budget. A cut pause waits only what is left (job memory :waited-ms). Needs nothing.
  Its hold stays until the caller's round ends or clears it. Done {:waited-s s}. Quote: :seconds.")

(a/defargs args
  {:seconds {:doc "how long to pause" :spec (a/num-in 0 nil) :default 0}
   :why {:doc "the hold reason (job.holding)" :spec (a/or-of keyword? string?) :default :pausing}})

(def target [])

(def conditions [])

(defn left-ms [c] (max 0 (- (* 1000 (:seconds (:args c) 0)) (:waited-ms (ctx/mem c) 0))))

(defn quote-of [c] (let [s (:seconds (:args c) 0)] {:uses {:time [s s s]}}))

(defn ^:async round [c]
  (let [why (:why (:args c) :pausing)]
    (ctx/hold-still! c why)
    (loop []
      (let [left (left-ms c)]
        (if (pos? left)
          (let [ms (min left (primitive/js-setting "prim/wait-max-ms" 10000))]
            (await (ctx/act c :wait #js {:ms ms :why why}))
            (ctx/update-mem! c update :waited-ms (fnil + 0) ms)
            (recur))
          (ctx/end c :done nil {:waited-s (:seconds (:args c) 0)}))))))
