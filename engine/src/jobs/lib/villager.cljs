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

(def id?
  "Whether x can name a villager: its uuid (string) or its numeric entity id, as entity listings give it."
  (a/pred "a villager's uuid string or entity id" #(or (string? %) (integer? %))))

(defn ident
  "What names villager entity e to the jobs: its uuid when known, else its numeric entity id."
  [e]
  (or (.-uuid e) (.-id e)))

(defn find-by-uuid
  "The villager entity named by id (uuid or numeric entity id) in sight within the search radius, or nil."
  [p id]
  (first (filter #(or (= id (.-uuid %)) (= id (.-id %))) (seen p))))

(defn chosen
  "The uuid of the villager a job works with: the call's :villager, else the one picked first (kept in memory), else nil."
  [c]
  (or (:villager (:args c)) (:chosen (ctx/mem c))))

(defn pick!
  "Without :villager, choose the nearest adult villager in sight that ok? accepts and keep it, one whose profession reads
  as a job first (the profession is not always readable). The id or nil."
  [c ok?]
  (or (chosen c)
      (let [es (filter ok? (adults (:primitives c)))]
        (when-let [e (or (first (remove #(= "unemployed" (.-profession %)) es)) (first es))]
          (ctx/update-mem! c assoc :chosen (ident e))
          (ident e)))))
