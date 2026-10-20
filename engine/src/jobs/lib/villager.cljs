(ns jobs.lib.villager
  "Villagers in sight, as the village jobs (breed, feed, trade, roll, maintain) look for them. One search radius for all;
  a call overrides it with its :settings."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.look :as look]))

(a/defargs settings
  {::search-radius {:default 48 :doc "Blocks within which villagers are looked for." :spec (a/int-in 1 96)}})

(defn search-radius [] (settings/get settings ::search-radius))

(defn seen
  "The villager entities in sight within the search radius."
  [p]
  (look/seen-objs p {:radius (search-radius) :names ["villager"]}))

(defn adult? [e] (not (.-baby e)))

(defn adults [p] (filter adult? (seen p)))

(defn find-by-uuid
  "The villager entity with uuid in sight within the search radius, or nil."
  [p uuid]
  (first (filter #(= uuid (.-uuid %)) (seen p))))
