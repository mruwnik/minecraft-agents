(ns jobs.movement.look-around
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.look :as look]
            [engine.primitive :as primitive]))

(def doc
  "Look around like a player: turn through the four headings (level, down at the floor ahead and, unless that ray is
  seen already, up) with a sight pass after each look, so what lies beside or behind the body enters perception's
  memory. Then write a :looked entry to body memory (a condition can read it: (since :looked)) and end :done. It only
  turns, so no hostile or dark makes it wait; it does not pause (to idle, use jobs.time.noop). Needs nothing.")

(a/defargs args {})

(def target [])

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def safe? ready/ok)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(def looks "Looks of one full turn: three per heading." 12)

(defn done?
  "Done once the body has looked round on this run."
  [c]
  (if (look/surveyed? c)
    (contract/ok :looked "the body has looked round")
    (contract/later :looking "the body has not looked round yet" :units :looks :done 0 :left looks)))

(defn quote-of
  "The quote of one look, times the looks of a turn."
  [c]
  (let [{:keys [quote-of]} (get primitive/table :look)
        [l m x] (get-in (quote-of (:primitives c) {}) [:uses :job-time])]
    {:uses {:job-time [(* looks l) (* looks m) (* looks x)]}}))

(def looked-policy {:cap 1 :ttl :forever})

(defn ^:async round [c]
  (await (look/look-around-until! c (constantly nil)))
  (ctx/remember! c :looked {} looked-policy)
  (ctx/end c :done nil))
