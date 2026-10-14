(ns jobs.forestry.harvest-wood
  (:require [engine.args :as a] [jobs.lib.fetch :as fetch]
            [engine.ctx :as ctx]
            [clojure.string :as str]
            [jobs.lib.pace :as pace]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.lib.result :as result]
            [jobs.lib.trees :as trees :refer [default-radius drop-filter debts near-debt? sapling-for]]))

(def doc
  "Fell a tree, collect what dropped and replant. Runs three child jobs in turn, each called once and run to its end:
  :fell (jobs.forestry.fell-tree), :collect (collect-drops) and :plant (plant-sapling).
  :collect works around the felled tree's base (when fewer logs came in than were felled and no drop is in sight, it walks round the base to look: leaves hide drops), wherever a reflex has since taken the body; a felling that brings in
  no item at all (the inventory count of the filter's items, before the felling against after the collecting)
  ends the job :stopped (:nothing-collected, with :got the logs carried). Ends when :plant is done. Only replant debts within :radius of where the body stood when :plant began are
  planted; the others (and any when no sapling is carried or the spot is not clear) stay owed, and the job
  warns harvest-wood.debts-owed with their :count and the :nearest one's :pos.
  Before the first tree it deposits at the known chest when the bag is fullish (jobs.lib.trip/ready!; :deposit-first? false skips it).
  Debts within the radius that stay unplanted (no sapling carried, spot refused) warn harvest-wood.replant-owed
  with :count, :pos, :reason (:no-sapling or :not-planted) and :text. The status stays :completed (the wood is
  the goal); the result is {:replant-owed n} then. With :count, the three phases repeat on the next tree until that many logs are carried; when no tree is left to fell it ends :stopped :no-tree with :got (and :replant-owed, the warnings as at a normal end; with none in range at all it ends the same, :got 0). With no axe carried, fell-tree fetches one for a big tree (:fetch); a fell that ends :need (no pillar blocks) ends the job :stopped with that reason. A missing sapling is fetched by plant-sapling (a seen chest, or breaking the seen leaves of its tree); when none can be got it stays owed.")

(a/defargs args
  {:species {:doc "log species; any when nil" :spec a/name? :default nil}
   :radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :filter {:doc "items to collect; the species' log, sapling, stick and apple when nil" :spec (a/coll-of a/item?) :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims (passed to the felling and the planting); the rules of the game allow it" :spec boolean? :default false}
   :avoid {:doc "columns [x z] of trees to leave alone (passed to fell-tree)" :spec (a/coll-of (a/pred "an [x z] column" vector?)) :default nil}
   :deposit-first? {:doc "deposit at the known chest first when the bag is fullish and the chest is near (jobs.lib.trip/ready!)" :spec boolean? :default true}
   :count {:doc "logs to carry: another tree is felled while fewer of the species' logs (any log when no species) are carried; nil: one tree" :spec (a/int-in 1 nil) :default nil}
   :fetch {:doc "get a missing sapling or an axe (passed to plant-sapling and fell-tree; see their :fetch): true, a set of kinds or a map of limits; false leaves the replant owed" :spec fetch/option? :default true}})

(defn phases
  "The children in order: [phase job args]; the phase is also the slot. tree is the felled tree's base, if any."
  [{:keys [species radius filter ignore-zones? fetch avoid]} origin tree]
  [[:fell 'jobs.forestry.fell-tree (cond-> {:species species :radius radius :ignore-zones? ignore-zones?}
                                     (some? fetch) (assoc :fetch fetch)
                                     (seq avoid) (assoc :avoid avoid))]
   [:collect 'jobs.forestry.collect-drops {:radius radius :filter (or filter (drop-filter species)) :want-items (if species [(str species "_log")] trees/log-names) :near tree :search radius}]
   [:plant 'jobs.forestry.plant-sapling (cond-> {:species species :ignore-zones? ignore-zones? :near origin :within radius}
                                         (some? fetch) (assoc :fetch fetch))]])

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
        (and (= :fell slot) (pos? (:felled (ctx/mem c) 0)))
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
  "Debts of the species within the radius still owed when the job ends: warn once; the count n, nil when none."
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
        n))))

(defn carried-count
  "How many of the collected items the body carries: the filter's items, any item when there is no filter."
  [c]
  (let [{:keys [species] only :filter} (:args c)
        names (some-> (or only (drop-filter species)) set)]
    (->> (u/inventory (:primitives c))
         (filter #(or (nil? names) (names (:name %))))
         (map :count)
         (reduce + 0))))

(defn logs-carried
  "How many logs of the species (any log when nil) the body carries."
  [c]
  (let [log? (if-let [sp (:species (:args c))] #{(str sp "_log")} #(str/ends-with? % "_log"))]
    (->> (u/inventory (:primitives c)) (filter #(log? (:name %))) (map :count) (reduce + 0))))

(defn more-trees?
  "Whether :count asks for more logs than are carried."
  [c]
  (let [n (:count (:args c))]
    (and n (< (logs-carried c) n))))

(def fell-needs
  "The reasons a fell child ends for when it lacks what a tree needs (an axe, pillar blocks, room): the job ends with them."
  #{:need :tool :free-slots})

(defn ^:async step
  "Calls the current phase's child once; when the child is done the phase
  advances (:again). Done when the :plant child is done. A child that is not done is :continue (it waits on the
  world; a declined child too, the check normally keeps the round from running at all). The :plant phase with a child that would
  wait (no sapling carried, the spot not clear) is done without it: the replant stays owed."
  [c]
  (let [_ (when-not (:origin (ctx/mem c)) (ctx/update-mem! c assoc :origin (u/self-pos c)))
        _ (when-not (:carried-before (ctx/mem c)) (ctx/update-mem! c assoc :carried-before (carried-count c) :logs-before (logs-carried c)))
        [phase job args] (current-phase c)
        args (cond-> args (= :collect phase) (assoc :want (:want (ctx/mem c) 1)))
        wait (atom nil)
        no-more-trees? (and (= :fell phase) (:count (:args c)) (pos? (:felled (ctx/mem c) 0))
                            (not (ctx/check-child (assoc c :wait wait) phase job args))
                            (= :no-tree (:reason (let [w @wait] (if (map? w) w {:reason w})))))
        r (cond
            no-more-trees? :no-tree
            (and (= :plant phase) (not (ctx/check-child c phase job args))) :done
            :else (await (ctx/call-child c phase job args)))
        _ (when (and (= :fell phase) (= :done r))
            (some->> (ctx/child-result c :fell) :base (ctx/update-mem! c assoc :tree))
            (ctx/update-mem! c assoc :want (max 0 (- (:logs (ctx/child-result c :fell) 1)
                                                     (- (logs-carried c) (:logs-before (ctx/mem c) 0))))))
        gained (when (and (= :collect phase) (= :done r)) (- (carried-count c) (:carried-before (ctx/mem c))))
        next-phase (second (drop-while #(not= phase %) (map first (phases (:args c) nil nil))))]
    (cond
      (and (= :fell phase) (= :done r) (fell-needs (:reason (ctx/child-result c :fell))))
      (let [reason (:reason (ctx/child-result c :fell))]
        (warn-owed! c)
        (result/stop! c reason (str "felling stopped: " (name reason)) :got (logs-carried c) :replant-owed (warn-replant-owed! c)))
      (or no-more-trees? (and (= :fell phase) (= :done r) (or (:count (:args c)) (= :no-tree (:reason (ctx/child-result c :fell)))) (not (:tree (ctx/mem c)))))
      (let [why (:why (ctx/child-result c :fell))]
        (warn-owed! c)
        (result/stop! c :no-tree (str (if (:count (:args c)) (str "no tree left to fell: " (logs-carried c) " of " (:count (:args c)) " logs carried") "no tree in range")
                                      (when why (str "; the trees seen could not be reached (" why ")")))
                      :got (logs-carried c) :why why :columns (:columns (ctx/child-result c :fell)) :replant-owed (warn-replant-owed! c)))
      (not= :done r) :continue
      (and (= :collect phase) (:tree (ctx/mem c)) (not (pos? gained)))
      (result/stop! c :nothing-collected "felled a tree but no drop came into the inventory" :got (logs-carried c))
      (and (nil? next-phase) (more-trees? c))
      (do (ctx/update-mem! c #(-> % (assoc :phase :fell :tree nil :carried-before nil :logs-before nil :want nil :origin nil)
                                  (update :felled (fnil inc 0))
                                  (update :children dissoc :fell :collect :plant)))
          :again)
      (nil? next-phase) (do (warn-owed! c)
                            (when-let [n (warn-replant-owed! c)] (ctx/result! c {:replant-owed n}))
                            :done)
      :else (do (ctx/update-mem! c assoc :phase next-phase)
                (when (= :plant next-phase) (ctx/update-mem! c assoc :origin (u/self-pos c)))
                :again))))

(def max-steps "Child calls of one round before it gives the body back with :continue." 400)

(defn ^:async round
  [c]
  (if-let [t (await (trip/ready! c))]
    t
    (let [n (atom 0)]
      (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step c) :continue))))))
