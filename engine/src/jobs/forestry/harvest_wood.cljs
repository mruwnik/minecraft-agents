(ns jobs.forestry.harvest-wood
  (:require [engine.ctx :as ctx]
            [engine.jobs.forestry :refer [default-radius drop-filter]]))

(def doc
  "Fell a tree, collect what dropped and replant: phases :fell, :collect and
  :plant, one child round per round.")

(def args
  {:species {:doc "log species; any when nil" :default nil}
   :radius {:doc "search radius in blocks" :default default-radius}
   :filter {:doc "items to collect; the species' log, sapling, stick and apple when nil" :default nil}})

(defn phases
  "The children in order: [phase job args]; the phase is also the slot."
  [{:keys [species radius filter]}]
  [[:fell 'jobs.forestry.fell-tree {:species species :radius radius}]
   [:collect 'jobs.forestry.collect-drops {:radius radius :filter (or filter (drop-filter species))}]
   [:plant 'jobs.forestry.plant-sapling {:species species}]])

(defn current-phase
  "The [phase job args] the job is in, from its memory."
  [c]
  (let [phase (:phase (ctx/mem c) :fell)]
    (some #(when (= phase (first %)) %) (phases (:args c)))))

(defn check
  "The current phase's child would run: its check, against its sub-map."
  [c]
  (let [[slot job args] (current-phase c)]
    (boolean (ctx/check-child c slot job args))))

(defn ^:async round
  "Steps the current phase's child once; when the child is done the phase
  advances. Done when the :plant child is done. A declined child is
  :continue (the check normally keeps the round from running at all)."
  [c]
  (let [[phase job args] (current-phase c)
        r (await (ctx/call-child c phase job args))
        next-phase (second (drop-while #(not= phase %) (map first (phases (:args c)))))]
    (cond
      (not= :done r) :continue
      (nil? next-phase) :done
      :else (do (ctx/update-mem! c assoc :phase next-phase) :continue))))
