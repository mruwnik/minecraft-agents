(ns jobs.forestry.harvest-wood
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.lib.result :as result]
            [jobs.forestry.trees :refer [default-radius drop-filter debts near-debt? sapling-for]]))

(def doc
  "Fell a tree, collect what dropped and replant. Runs three child jobs in turn, one child round per round:
  :fell (jobs.forestry.fell-tree), :collect (collect-drops) and :plant (plant-sapling).
  :collect works around the felled tree's base, wherever a reflex has since taken the body; a felling that brings in
  no item at all ends the job :stopped (:nothing-collected). Ends when :plant is done. Only replant debts within :radius of where the body stood when :plant began are
  planted; the others (and any when no sapling is carried or the spot is not clear) stay owed, and the job
  warns harvest-wood.debts-owed with their :count and the :nearest one's :pos.
  Debts within the radius that stay unplanted (no sapling carried, spot refused) warn harvest-wood.replant-owed
  with :count, :pos, :reason (:no-sapling or :not-planted) and :text. The status stays :completed (the wood is
  the goal); the result is {:replant-owed n} then. No sapling is fetched: plant-sapling only plants what is carried.")

(def args
  {:species {:doc "log species; any when nil" :default nil}
   :radius {:doc "search radius in blocks" :default default-radius}
   :filter {:doc "items to collect; the species' log, sapling, stick and apple when nil" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims (passed to the felling and the planting); the rules of the game allow it" :default false}})

(defn phases
  "The children in order: [phase job args]; the phase is also the slot. tree is the felled tree's base, if any."
  [{:keys [species radius filter ignore-zones?]} origin tree]
  [[:fell 'jobs.forestry.fell-tree {:species species :radius radius :ignore-zones? ignore-zones?}]
   [:collect 'jobs.forestry.collect-drops {:radius radius :filter (or filter (drop-filter species)) :near tree}]
   [:plant 'jobs.forestry.plant-sapling {:species species :ignore-zones? ignore-zones? :near origin :within radius}]])

(defn current-phase
  "The [phase job args] the job is in, from its memory."
  [c]
  (let [phase (:phase (ctx/mem c) :fell)]
    (some #(when (= phase (first %)) %) (phases (:args c) (:origin (ctx/mem c)) (:tree (ctx/mem c))))))

(defn check
  "The current phase's child would run: its check, against its sub-map. The :plant phase always runs: a child that
  would wait (no sapling carried) ends the job instead of leaving it queued."
  [c]
  (let [[slot job args] (current-phase c)]
    (or (= :plant slot)
        (boolean (ctx/check-child c slot job args)))))

(defn warn-owed!
  "Warn once when replant debts of the species remain owed, far from here."
  [c]
  (let [{:keys [species]} (:args c)
        here (:origin (ctx/mem c))
        owed (filterv #(and (or (nil? species) (= species (:species %))) (not (near-debt? here (:radius (:args c)) %)))
                      (debts c))]
    (when (seq owed)
      (ctx/warn-once! c :owed :harvest-wood.debts-owed
                      {:count (count owed)
                       :nearest (:pos (apply min-key #(u/dist here (:pos %)) owed))}))))

(defn warn-replant-owed!
  "Debts of the species within the radius still owed when the job ends: warn once and hand over {:replant-owed n}."
  [c]
  (let [{:keys [species radius]} (:args c)
        here (:origin (ctx/mem c))
        owed (filterv #(and (or (nil? species) (= species (:species %))) (near-debt? here radius %)) (debts c))]
    (when (seq owed)
      (let [n (count owed)
            reason (if (some #(sapling-for (u/inventory (:primitives c)) (:species %)) owed) :not-planted :no-sapling)]
        (ctx/warn-once! c :replant-owed :harvest-wood.replant-owed
                        {:count n
                         :pos (:pos (apply min-key #(u/dist here (:pos %)) owed))
                         :reason reason
                         :text (str "felled " n (if (= 1 n) " tree" " trees") ", could not replant: "
                                    (if (= :no-sapling reason) "no sapling" "spot not plantable"))})
        (ctx/result! c {:replant-owed n})))))

(defn ^:async round
  "Steps the current phase's child once; when the child is done the phase
  advances. Done when the :plant child is done. A declined child is
  :continue (the check normally keeps the round from running at all). The :plant phase with a child that would
  wait (no sapling carried, the spot not clear) is done without it: the replant stays owed."
  [c]
  (let [_ (when-not (:origin (ctx/mem c)) (ctx/update-mem! c assoc :origin (u/self-pos c)))
        [phase job args] (current-phase c)
        r (if (and (= :plant phase) (not (ctx/check-child c phase job args)))
            :done
            (await (ctx/call-child c phase job args)))
        _ (when (and (= :fell phase) (= :done r))
            (some->> (ctx/child-result c :fell) :base (ctx/update-mem! c assoc :tree)))
        collected (when (and (= :collect phase) (= :done r)) (:collected (ctx/child-result c :collect)))
        next-phase (second (drop-while #(not= phase %) (map first (phases (:args c) nil nil))))]
    (cond
      (not= :done r) :continue
      (and (= :collect phase) (:tree (ctx/mem c)) (zero? (or collected 0)))
      (result/stop! c :nothing-collected "felled a tree but no drop came into the inventory")
      (nil? next-phase) (do (warn-owed! c) (warn-replant-owed! c) :done)
      :else (do (ctx/update-mem! c assoc :phase next-phase)
                (when (= :plant next-phase) (ctx/update-mem! c assoc :origin (u/self-pos c)))
                :continue))))
