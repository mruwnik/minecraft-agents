(ns jobs.movement.look-around
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]
            [engine.primitive :as primitive]))

(def idle-ok true)

(def doc
  "Face a random point 3 blocks away (any yaw, from a block below to 1.5 above the body),
  write a :looked entry to body memory (a condition can read it: (since :looked)),
  then wait :every-ms so an idle body does not spin.")

(a/defargs args
  {:every-ms {:doc "milliseconds to wait after each look" :spec (a/num-in 0 nil) :default 2000}})

(def look-ahead 3)

(def looked-policy {:cap 1 :ttl :forever})

(def target [])

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn quote-of
  "The quotes of its two acts, the look and the wait, added."
  [c]
  (let [p (:primitives c)
        quote-act #(let [{:keys [quote-of]} (get primitive/table %)] (quote-of p %2))
        [l m x] (get-in (quote-act :look {}) [:uses :job-time])
        [wl wm wx] (get-in (quote-act :wait {:ms (:every-ms (:args c))}) [:uses :job-time])]
    {:uses {:job-time [(+ l wl) (+ m wm) (+ x wx)]}}))

(defn look-point
  "The point look-ahead blocks from pos horizontally at yaw 2*pi*u, and
  y - 1 + 2.5*v high (u and v in [0, 1))."
  [{:keys [x y z]} u v]
  (let [yaw (* 2 js/Math.PI u)]
    {:x (+ x (* look-ahead (js/Math.cos yaw)))
     :y (+ y -1 (* 2.5 v))
     :z (+ z (* look-ahead (js/Math.sin yaw)))}))

(defn ^:async round [c]
  (let [pos (look-point (u/self-pos c) (rand) (rand))]
    (await (ctx/act c :look (clj->js {:pos pos})))
    (ctx/remember! c :looked {} looked-policy)
    (await (child/pause! c (/ (:every-ms (:args c)) 1000) :looking))
    (ctx/end c :done nil)))
