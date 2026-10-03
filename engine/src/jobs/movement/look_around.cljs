(ns jobs.movement.look-around
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Face a point a few blocks ahead of the body, write a :looked entry to body
  memory (which the :every-interval trigger reads), then wait :every-ms before
  the next round, so an idle body does not spin.")

(def args
  {:every-ms {:doc "milliseconds to wait after each look" :default 2000}})

(def look-ahead 3)

(def looked-policy {:cap 1 :ttl :forever})

(defn check [_c] true)

(defn ^:async round [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    (await (ctx/act c :look (clj->js {:pos {:x (+ x look-ahead) :y (inc y) :z z}})))
    (ctx/remember! c :looked {} looked-policy)
    (await (ctx/act c :wait (clj->js {:ms (:every-ms (:args c))})))
    :done))
