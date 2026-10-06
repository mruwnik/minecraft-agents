(ns engine.search-bench
  "The job-side searches as a library for the search bench (bench-lang/search.mjs): the reach searches the hostile
  triggers and survival jobs run (jobs.lib.reach) over a primitives object the bench builds on a recorded world.
  Compiled by `tools/compile engine search-bench`."
  (:require [jobs.lib.reach :as reach]))

(defn walkable-way
  "reach/walkable-way? from mob {x y z} to body {x y z} over primitives p."
  [p mob body]
  (reach/walkable-way? p (js->clj mob :keywordize-keys true) (js->clj body :keywordize-keys true)))

(defn enclosed [p] (reach/enclosed? p))

(defn nearest-danger
  "The id of reach/nearest-danger within radius (ranged mobs within ranged-radius), sight ignored, or nil."
  [p radius ranged-radius]
  (some-> (reach/nearest-danger p radius {:ranged-radius ranged-radius} {:sight? false}) .-id))

(defn dangers
  "The ids of reach/dangers within radius (ranged within ranged-radius), sight ignored."
  [p radius ranged-radius]
  (clj->js (mapv #(.-id %) (reach/dangers p radius {:ranged-radius ranged-radius} {:sight? false}))))
