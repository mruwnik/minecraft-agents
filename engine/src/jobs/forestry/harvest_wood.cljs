(ns jobs.forestry.harvest-wood
  (:require [engine.ctx :as ctx]
            [engine.jobs.forestry :refer [default-radius drop-filter]]))

(def doc
  "Fell a tree, collect what dropped and replant. Runs three child jobs in turn, one child round per round:
  :fell (jobs.forestry.fell-tree), :collect (collect-drops) and :plant (plant-sapling).
  Ends when :plant is done. The replant is skipped, and stays owed, when no sapling is carried or the spot is not
  clear.")

(def args
  {:species {:doc "log species; any when nil" :default nil}
   :radius {:doc "search radius in blocks" :default default-radius}
   :filter {:doc "items to collect; the species' log, sapling, stick and apple when nil" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims (passed to the felling and the planting); the rules of the game allow it" :default false}})

(defn phases
  "The children in order: [phase job args]; the phase is also the slot."
  [{:keys [species radius filter ignore-zones?]}]
  [[:fell 'jobs.forestry.fell-tree {:species species :radius radius :ignore-zones? ignore-zones?}]
   [:collect 'jobs.forestry.collect-drops {:radius radius :filter (or filter (drop-filter species))}]
   [:plant 'jobs.forestry.plant-sapling {:species species :ignore-zones? ignore-zones?}]])

(defn current-phase
  "The [phase job args] the job is in, from its memory."
  [c]
  (let [phase (:phase (ctx/mem c) :fell)]
    (some #(when (= phase (first %)) %) (phases (:args c)))))

(defn check
  "The current phase's child would run: its check, against its sub-map. The :plant phase always runs: a child that
  would wait (no sapling carried) ends the job instead of leaving it queued."
  [c]
  (let [[slot job args] (current-phase c)]
    (or (= :plant slot)
        (boolean (ctx/check-child c slot job args)))))

(defn ^:async round
  "Steps the current phase's child once; when the child is done the phase
  advances. Done when the :plant child is done. A declined child is
  :continue (the check normally keeps the round from running at all). The :plant phase with a child that would
  wait (no sapling carried, the spot not clear) is done without it: the replant stays owed."
  [c]
  (let [[phase job args] (current-phase c)
        r (if (and (= :plant phase) (not (ctx/check-child c phase job args)))
            :done
            (await (ctx/call-child c phase job args)))
        next-phase (second (drop-while #(not= phase %) (map first (phases (:args c)))))]
    (cond
      (not= :done r) :continue
      (nil? next-phase) :done
      :else (do (ctx/update-mem! c assoc :phase next-phase) :continue))))
