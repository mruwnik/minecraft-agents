(ns engine.regions-bench
  "The region map as a library for its bench (bench-lang/regions.mjs): JS in, JS out. Compiled into the planner-bench
  build (`shadow-cljs compile planner-bench`)."
  (:require [engine.path.regions :as regions]))

(defn create [world] (regions/create world))

(defn route
  "regions/route with JS from {x y z} and goals [{kind x y z range}]: the answer as JS (keywords as strings)"
  [rm from goals]
  (clj->js (regions/route rm (js->clj from :keywordize-keys true) (js->clj goals :keywordize-keys true))))

(defn queue-column [rm cx cz] (regions/queue-column! rm cx cz))
(defn build-step [rm ms] (regions/build-step! rm ms))
(defn pending [rm] (regions/pending rm))
(defn invalidate [rm x y z old] (regions/invalidate! rm x y z old))
(defn memory [rm] (clj->js (regions/memory rm)))
(defn stats [^js rm] (.-stats rm))
