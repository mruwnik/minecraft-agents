(ns jobs.forestry.tidy
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as result]
            [jobs.lib.trees :as trees :refer [default-radius trees-near seen-name drop-filter replant-kind]]
            ))

(def doc
  "Fell the part-cut trunks round the body: a natural tree (logs with leaves near the top, as fell-tree reads one)
  whose lowest log is raised, the block under it seen as air, because the logs under it were cut. Runs three
  children in turn per trunk, each called once: fell-tree (:at the trunk's lowest log), collect-drops (round it) and
  plant-sapling (:at the seen ground cell under the trunk; the debt fell-tree wrote on the raised log is dropped).
  Acts only on trunks it has seen. A trunk with no leaves (a log wall of a build), a tree standing on the ground, and a
  trunk whose lowest log a zone, claim or plan footprint refuses (warn tidy.declined once) are left alone; so is a trunk
  fell-tree digs no log of.
  Ends after :max-trees trunks, or when none is left in sight. Result {:tidied n}. With nothing to tidy it ends :stopped
  :nothing-to-tidy. A missing axe, a pillar's blocks or a sapling are fetched by the children (:fetch).")

(a/defargs settings
  {::max-trees {:default 8 :doc "The most trunks one tidy run fells." :spec (a/int-in 1 nil)}
   ::ground-depth {:default 24 :doc "Blocks under a raised log looked down for the ground to replant on." :spec (a/int-in 1 nil)}})

(a/defargs args
  {:species {:doc "log species; any when nil" :spec a/name? :default nil}
   :radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :max-trees {:doc "trunks to fell at most; the settings' tidy max-trees when nil" :spec (a/int-in 1 nil) :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :fetch {:doc "get a missing axe, pillar blocks or sapling (passed to fell-tree and plant-sapling; see their :fetch)" :spec fetch/option? :default true}})

(def air #{"air" "cave_air" "void_air"})

(defn raised?
  "Whether the block under pos is seen as air."
  [p pos]
  (contains? air (seen-name p (update pos :y dec))))

(defn ground-cell
  "The cell over the first seen block below the raised log at pos that is not air, or nil when none is seen
  within the ground-depth setting (or an unseen cell comes first)."
  [p {:keys [y] :as pos}]
  (loop [dy 1]
    (when (<= dy (settings/get settings ::ground-depth))
      (let [below (assoc pos :y (- y dy))
            n (seen-name p below)]
        (cond
          (nil? n) nil
          (contains? air n) (recur (inc dy))
          :else (assoc below :y (inc (:y below))))))))

(defn allowed?
  "Whether the job may dig the log at pos (one warn per job when refused)."
  [c pos]
  (or (:ignore-zones? (:args c))
      (gate/allowed? c :tidy.declined "tidy" :dig pos {})))

(defn leftovers
  "The part-cut trunks in sight not skipped, nearest first, as trees-near gives them."
  [c]
  (let [{:keys [radius species]} (:args c)
        p (:primitives c)
        skipped (set (:skipped (ctx/mem c)))]
    (->> (trees-near p radius species)
         (filter #(and (not (skipped (:base %))) (raised? p (:base %)) (allowed? c (:base %)))))))

(defn phases
  "The children in order: [phase job args] for the trunk tree {:base :species :ground}."
  [{:keys [radius ignore-zones? fetch]} {:keys [base species ground]}]
  [[:fell 'jobs.forestry.fell-tree (cond-> {:at base :ignore-zones? ignore-zones?} (some? fetch) (assoc :fetch fetch))]
   [:collect 'jobs.forestry.collect-drops {:radius radius :filter (drop-filter species) :near base :search radius
                                           :want-items [(str species "_log")]}]
   [:plant 'jobs.forestry.plant-sapling (cond-> {:at ground :species species :ignore-zones? ignore-zones?}
                                          (some? fetch) (assoc :fetch fetch))]])

(defn limit [c] (or (:max-trees (:args c)) (settings/get settings ::max-trees)))

(defn finish!
  [c]
  (let [n (:tidied (ctx/mem c) 0)]
    (if (zero? n)
      (result/stop! c :nothing-to-tidy "no part-cut trunk in sight")
      (do (ctx/result! c {:tidied n}) :done))))

(defn next-tree!
  "Choose the next trunk into memory; false when none is left or the limit is reached."
  [c]
  (when (< (:tidied (ctx/mem c) 0) (limit c))
    (when-let [t (first (leftovers c))]
      (let [tree (assoc (select-keys t [:base :species]) :ground (ground-cell (:primitives c) (:base t)))]
        (ctx/update-mem! c assoc :tree tree :phase :fell :dug 0)
        tree))))

(defn end-tree!
  "The trunk is done (felled, or skipped when skip? is true): counts it, clears the children."
  [c skip?]
  (let [base (:base (:tree (ctx/mem c)))]
    (ctx/forget-where! c replant-kind #(= base (:pos %)))
    (ctx/update-mem! c #(-> %
                            (cond-> skip? (update :skipped (fnil conj []) base)
                                    (not skip?) (update :tidied (fnil inc 0)))
                            (dissoc :tree :phase :dug)
                            (update :children dissoc :fell :collect :plant)))
    :again))

(defn ^:async step
  "One piece of the work: :again, :continue (a child waits on the world) or :done."
  [c]
  (let [m (ctx/mem c)
        tree (or (:tree m) (next-tree! c))]
    (if-not tree
      (finish! c)
      (let [phase (:phase (ctx/mem c))
            [_ job cargs] (some #(when (= phase (first %)) %) (phases (:args c) tree))
            r (if (and (= :plant phase) (not (ctx/check-child c phase job cargs)))
                :done
                (await (ctx/call-child c phase job cargs)))]
        (cond
          (not= :done r) :continue
          (= :fell phase)
          (let [logs (:logs (ctx/child-result c :fell) 0)]
            (if (zero? logs)
              (end-tree! c true)
              (do (ctx/update-mem! c assoc :phase :collect) :again)))
          (= :collect phase)
          (if (:ground tree)
            (do (ctx/update-mem! c assoc :phase :plant) :again)
            (end-tree! c false))
          :else (end-tree! c false))))))

(def max-steps "Child calls of one round before it gives the body back with :continue." 400)

(defn check [_c] true)

(defn ^:async round
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step c) :continue)))))
