(ns jobs.forestry.harvest-wood
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cond :as cond]
            [engine.settings :as settings]
            [clojure.string :as str]
            [jobs.forestry.fell-tree :as fell-tree]
            [jobs.lib.area :as area]
            [jobs.lib.checks :as checks]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.lib.trees :as trees :refer [default-radius drop-filter debts near-debt?]]))

(def doc
  "Fell a tree, collect what dropped and replant. Three child jobs in turn, each called once and run to its end:
  :fell (jobs.forestry.fell-tree), :collect (collect-drops, round the felled tree's base) and :plant (plant-sapling).
  :center (default: where the body stands at the first round) with :radius is the area: a call waits (:outside-area, jobs.lib.area) while the body stands farther than :radius from the centre; the caller walks there first.
  Only replant debts within :radius of where the body stood when :plant began are planted; the others stay owed and the job warns harvest-wood.debts-owed with their :count and the :nearest one's :pos.
  Before the first tree it deposits at the known chest when the bag is fullish (jobs.lib.trip/ready!).
  With :count, the three phases repeat on the next tree until that many logs are carried; done? holds at once when they are carried already (:done :already).
  Needs: a tree in range (a start wait :no-tree until one shows); an axe and a sapling are fetched (:fetch).
  Ends :done (the wood is the goal) with {:replant-owed n} when near debts stay unplanted. Any other ending is the felling's, passed up with :got (the logs carried) and :replant-owed in the data: condition-failed :no-tree when no tree is left, a need, a limit, nothing-possible :tree-unreachable / :tree-blocked (data :why, :columns). Nothing-possible :none-collected when a felling brings in no item at all.")

(a/defargs args
  {:species {:doc "log species; any when nil" :spec a/name? :default nil}
   :radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :center {:doc "the centre of the area the job works, a cell [x y z] or {:x :y :z}; nil: where the body stands when the job first runs (jobs.lib.area)" :spec a/position? :default nil}
   :ignore-zones? :shared
   :avoid {:doc "columns [x z] of trees to leave alone (passed to fell-tree)" :spec (a/coll-of (a/pred "an [x z] column" vector?)) :default nil}
   :count {:doc "logs to carry: another tree is felled while fewer of the species' logs (any log when no species) are carried; nil: one tree" :spec (a/int-in 1 nil) :default nil}
   :fetch {:doc "get a missing sapling or an axe (passed to plant-sapling and fell-tree; see their :fetch): true, a set of kinds or a map of limits; false leaves the replant owed" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a harvest may take: every tree's felling, collecting and planting." :spec (a/num-in 0 nil)}
   ::max-steps {:default 400 :doc "Child calls of one round before it gives the body back with :continue." :spec (a/int-in 1 nil)}})

(defn fell-args [{:keys [species radius ignore-zones? fetch avoid]}]
  (cond-> {:species species :radius radius :ignore-zones? ignore-zones?}
    (some? fetch) (assoc :fetch fetch)
    (seq avoid) (assoc :avoid avoid)))

(defn phases
  "The children in order: [phase job args]; the phase is also the slot. tree is the felled tree's base, if any."
  [args origin tree]
  (let [{:keys [species radius ignore-zones? fetch]} args]
    [[:fell 'jobs.forestry.fell-tree (fell-args args)]
     [:collect 'jobs.items.collect-drops {:radius radius :items (drop-filter species) :least-items (if species [(str species "_log")] trees/log-names) :near tree :search radius}]
     [:plant 'jobs.forestry.plant-sapling (cond-> {:species species :ignore-zones? ignore-zones? :near origin :within radius}
                                            (some? fetch) (assoc :fetch fetch))]]))

(defn current-phase
  "The [phase job args] the job is in, from its memory."
  [c]
  (let [phase (:phase (ctx/mem c) :fell)]
    (some #(when (= phase (first %)) %) (phases (:args c) (:origin (ctx/mem c)) (:tree (ctx/mem c))))))

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

(defn replant-owed
  "How many debts of the species within the radius are still owed; nil when none."
  [c]
  (let [{:keys [species radius]} (:args c)
        here (:origin (ctx/mem c))
        n (count (filter #(and (or (nil? species) (= species (:species %))) (near-debt? here radius %)) (debts c)))]
    (when (pos? n) n)))

(defn carried-count
  "How many of the collected items the body carries: the filter's items, any item when there is no filter."
  [c]
  (let [names (some-> (drop-filter (:species (:args c))) set)]
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

(defn owed-data [c] (when-let [n (replant-owed c)] {:replant-owed n}))

(defn end-with
  "The felling's ending e as this job's: its kind and cause, the logs carried and the replant owed in the data."
  [c e]
  (warn-owed! c)
  (ctx/end c (:kind e) (:cause e)
           (merge (select-keys (:data e) [:why :columns :unreachable])
                  {:got (logs-carried c)}
                  (owed-data c))))

(defn fell-ended!
  "What the :fell child's ending e means: a tree felled goes on to collect, any other ending ends the job."
  [c e]
  (if (not= :done (:kind e))
    (end-with c e)
    (do (some->> (:base (:data e)) (ctx/update-mem! c assoc :tree))
        (ctx/update-mem! c assoc :want (max 0 (- (:logs (:data e) 1) (- (logs-carried c) (:logs-before (ctx/mem c) 0)))))
        (ctx/update-mem! c assoc :phase :collect)
        :again)))

(defn ^:async step
  "Calls the current phase's child once; when the child is done the phase advances (:again). A child that waits on the
  world is :continue. The :plant child ending without a planting leaves the replant owed."
  [c]
  (let [_ (when-not (:origin (ctx/mem c)) (ctx/update-mem! c assoc :origin (u/self-pos c)))
        _ (when-not (:carried-before (ctx/mem c)) (ctx/update-mem! c assoc :carried-before (carried-count c) :logs-before (logs-carried c)))
        [phase job args] (current-phase c)
        args (if (= :collect phase)
                (let [want (:want (ctx/mem c) 1)]
                  (if (pos? want) (assoc args :least want) (assoc args :search 0)))
                args)]
    (let [e (await (ctx/call c phase job args))]
        (cond
          (= :continue e) :continue
          (= :fell phase) (fell-ended! c e)
          (and (= :collect phase) (:tree (ctx/mem c)) (not (pos? (- (carried-count c) (:carried-before (ctx/mem c))))))
          (ctx/end c :nothing-possible :none-collected {:got (logs-carried c) :text "felled a tree but no drop came into the inventory"})
          (and (= :plant phase) (more-trees? c))
          (do (ctx/update-mem! c #(-> % (assoc :phase :fell :tree nil :carried-before nil :logs-before nil :want nil :origin nil)
                                      (update :felled (fnil inc 0))
                                      (update :children dissoc :fell :collect :plant)))
              :again)
          (= :plant phase) (do (warn-owed! c)
                               (ctx/update-mem! c assoc :phase :done)
                               (ctx/end c :done nil (owed-data c)))
          :else (let [next-phase (second (drop-while #(not= phase %) (map first (phases (:args c) nil nil))))]
                  (ctx/update-mem! c assoc :phase next-phase)
                  (when (= :plant next-phase) (ctx/update-mem! c assoc :origin (u/self-pos c)))
                  :again)))))

(defn ^:async round
  [c]
  (if-let [t (await (trip/ready! c))]
    t
    (let [n (atom 0)]
      (await (pace/steps! c #(if (< (swap! n inc) (settings/get settings ::max-steps)) (step c) :continue))))))

;; ------------------------------------------------------------------ the contract

(def target [:species :center])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)

(defn with-fell-args [check] (fn [c] (check (assoc c :args (fell-args (:args c))))))

(def no-tree-to-fell
  "fell-tree's :no-tree while nothing is felled yet and the next tree is still to be felled; the other phases are the
  children's."
  (let [no-tree (with-fell-args fell-tree/no-tree)]
    (fn [c]
      (if (and (= :fell (:phase (ctx/mem c) :fell)) (not (pos? (:felled (ctx/mem c) 0))))
        (no-tree c)
        (contract/ok :tree-found "a tree is felled or not wanted now")))))

(def can-start? (cond/and area/inside no-tree-to-fell))

(defn done?
  "Ok once the last tree is planted, or (with :count) the logs are carried and no tree is under way; else later with the
  logs got this call."
  [c]
  (let [m (ctx/mem c)
        got (max 0 (- (logs-carried c) (:logs-before m (logs-carried c))))
        want (:count (:args c))
        idle? (and (= :fell (:phase m :fell)) (nil? (:tree m)) (not (checks/child-under-way? c)))
        data (concat [:units :logs :done got :result {:logs got}] (when want [:left (max 0 (- want (logs-carried c)))]))]
    (if (or (= :done (:phase m)) (and want idle? (not (more-trees? c))))
      (apply contract/ok :harvested (str "harvest-wood: " got " logs got") data)
      (apply contract/later :harvesting (str "harvest-wood: " got " logs, the plant phase follows") data))))

(defn ^:async quote-of [c]
  (let [e (await (fell-tree/quote-of (assoc c :args (fell-args (:args c)))))
        trees (max 1 (min 8 (quot (or (:count (:args c)) 3) 3)))
        t (:job-time (:uses e))
        scale #(* trees (+ % 6))]
    (cond-> {:uses {:job-time [(scale (nth t 0)) (scale (nth t 1)) (max (settings/get settings ::job-time-s) (scale (nth t 2)))]}
             :done [0 1 1]}
      (:yields e) (assoc :yields (:yields e)))))
