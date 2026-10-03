(ns jobs.movement.look-around
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Face a point look-ahead blocks away in a random direction (any yaw, a
  height up to a block below or 1.5 above the body), write a :looked entry to body
  memory (which the :every-interval trigger reads), then wait :every-ms before
  the next round, so an idle body does not spin.")

(def args
  {:every-ms {:doc "milliseconds to wait after each look" :default 2000}})

(def look-ahead 3)

(def looked-policy {:cap 1 :ttl :forever})

(defn check [_c] true)

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
    (await (ctx/act c :wait (clj->js {:ms (:every-ms (:args c))})))
    :done))
