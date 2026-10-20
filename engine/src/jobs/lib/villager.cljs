(ns jobs.lib.villager
  "Villagers in sight, as the village jobs (breed, feed, trade, roll) look for them. One search radius for all;
  a call overrides it with its :settings."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
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

(defn chosen
  "The uuid of the villager a job works with: the call's :villager, else the one picked first (kept in memory), else nil."
  [c]
  (or (:villager (:args c)) (:chosen (ctx/mem c))))

(defn pick!
  "Without :villager, choose the nearest adult villager in sight that ok? accepts and keep it. The uuid or nil."
  [c ok?]
  (or (chosen c)
      (when-let [e (first (filter ok? (adults (:primitives c))))]
        (ctx/update-mem! c assoc :chosen (.-uuid e))
        (.-uuid e))))
