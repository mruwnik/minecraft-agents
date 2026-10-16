(ns jobs.forestry.tidy
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.need :as need]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.trees :as trees :refer [default-radius trees-near seen-name drop-filter replant-kind]]))

(def doc
  "Fell the part-cut trunks round the body: a natural tree (logs with leaves near the top, as fell-tree reads one)
  whose lowest log is raised, the block under it seen as air, because the logs under it were cut. Judged per tree: a
  raised log joined (side or corner touching, through logs) to a log standing on the ground is part of a living tree
  (an acacia or fancy oak branch, a mangrove root) and is left. Runs three
  children in turn per trunk, each called once: fell-tree (:at the trunk's lowest log), collect-drops (round it) and
  plant-sapling (:at the seen ground cell under the trunk; the debt fell-tree wrote on the raised log is dropped).
  Acts only on trunks it has seen. A trunk with no leaves (a log wall of a build), a tree standing on the ground, and a
  trunk whose lowest log a zone, claim or plan footprint refuses (warn tidy.declined once) are left alone; so is a trunk
  fell-tree digs no log of.
  Ends :done {:tidied n} (plus :replant-owed k when k felled trunks were not replanted: no seen ground, or plant-sapling
  would not run; their replant debts stay owed) after :max-trees trunks or when none is left in sight. With nothing to tidy
  (done? true once the first round has looked round) it ends done {:tidied 0}. A fell child that ends for want of a tool, blocks or food (a need, a limit,
  :hungry) ends the job with that ending (:tidied n in its data), done when trunks were tidied first;
  a trunk it cannot reach is left. A missing axe, a pillar's blocks or a sapling are fetched by the children (:fetch).
  Quote: time by the trunks to fell, at most :max-trees.")

(a/defargs settings
  {::max-trees {:default 8 :doc "The most trunks one tidy run fells." :spec (a/int-in 1 nil)}
   ::job-time-s {:default 1200 :doc "Seconds a tidy run may take: the trunks, their drops and replants." :spec (a/num-in 0 nil)}
   ::max-steps {:default 400 :doc "Child calls of one round before it gives the body back with :continue." :spec (a/int-in 1 nil)}
   ::ground-depth {:default 24 :doc "Blocks under a raised log looked down for the ground to replant on." :spec (a/int-in 1 nil)}})

(a/defargs args
  {:species {:doc "log species; any when nil" :spec a/name? :default nil}
   :radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :max-trees {:doc "trunks to fell at most; the settings' tidy max-trees when nil" :spec (a/int-in 1 nil) :default nil}
   :ignore-zones? :shared
   :fetch {:doc "get a missing axe, pillar blocks or sapling (passed to fell-tree and plant-sapling; see their :fetch)" :spec need/option? :default true}})

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

(defn neighbours [{:keys [x y z]}]
  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] :when (not (and (zero? dx) (zero? dy) (zero? dz)))]
    {:x (+ x dx) :y (+ y dy) :z (+ z dz)}))

(defn grounded?
  "Whether the log at pos stands on a seen block that is neither air nor a log."
  [p pos]
  (let [n (seen-name p (update pos :y dec))]
    (and n (not (contains? air n)) (not (trees/log-name? n)))))

(defn living?
  "Whether the log at pos is joined to a log standing on the ground: a flood fill over the seen logs (the set of
  positions), side or corner touching."
  [p logs pos]
  (loop [todo [pos] seen #{pos}]
    (when-let [at (peek todo)]
      (if (grounded? p at)
        true
        (let [more (remove seen (filter logs (neighbours at)))]
          (recur (into (pop todo) more) (into seen more)))))))

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
        skipped (set (:skipped (ctx/mem c)))
        logs (delay (into #{} (map :pos) (trees/scan-logs p radius nil)))]
    (->> (trees-near p radius species)
         (filter #(and (not (skipped (:base %))) (raised? p (:base %)) (not (living? p @logs (:base %)))
                       (allowed? c (:base %)))))))

(defn phases
  "The children in order: [phase job args] for the trunk tree {:base :species :ground}."
  [{:keys [radius ignore-zones? fetch]} {:keys [base species ground]}]
  [[:fell 'jobs.forestry.fell-tree (cond-> {:at base :ignore-zones? ignore-zones?} (some? fetch) (assoc :fetch fetch))]
   [:collect 'jobs.forestry.collect-drops {:radius radius :filter (drop-filter species) :near base :search radius
                                           :want-items [(str species "_log")]}]
   [:plant 'jobs.forestry.plant-sapling (cond-> {:at ground :species species :ignore-zones? ignore-zones?}
                                          (some? fetch) (assoc :fetch fetch))]])

(defn limit [c] (or (:max-trees (:args c)) (settings/get settings ::max-trees)))

(defn result-of
  "The trunks tidied, and the replants still owed."
  [c]
  (let [m (ctx/mem c)]
    (cond-> {:tidied (:tidied m 0)} (pos? (:owed m 0)) (assoc :replant-owed (:owed m)))))

(defn finish!
  "The ending when no trunk is left to take: done with the count."
  [c]
  (ctx/end c :done nil (result-of c)))

(defn next-tree!
  "Choose the next trunk into memory; false when none is left or the limit is reached."
  [c]
  (when (< (:tidied (ctx/mem c) 0) (limit c))
    (when-let [t (first (leftovers c))]
      (let [tree (assoc (select-keys t [:base :species]) :ground (ground-cell (:primitives c) (:base t)))]
        (ctx/update-mem! c assoc :tree tree :phase :fell :dug 0)
        tree))))

(defn end-tree!
  "The trunk is done (felled, or skipped when skip? is true; owed? when it was felled but not replanted): counts it,
  clears the children. The debt of an owed replant stays."
  [c skip? owed?]
  (let [base (:base (:tree (ctx/mem c)))]
    (when-not (or skip? owed?)
      (ctx/forget-where! c replant-kind #(= base (:pos %))))
    (ctx/update-mem! c #(-> %
                            (cond-> skip? (update :skipped (fnil conj []) base)
                                    (not skip?) (update :tidied (fnil inc 0))
                                    owed? (update :owed (fnil inc 0)))
                            (dissoc :tree :phase :dug)
                            (update :children dissoc :fell :collect :plant)))
    :again))

(def fell-stops
  "Causes of a fell child's ending that are the body's or the world's, not the trunk's: they end the job."
  #{:need :tool :free-slots :hungry})

(defn fell-ended!
  "What the fell child's ending e means for the trunk: done with logs goes on to collect, done with none leaves the
  trunk; a stop of the body ends the job (done when trunks were tidied before it), any other ending leaves the trunk."
  [c e]
  (let [stop? (or (= :limit-reached (:kind e)) (fell-stops (contract/cause-key e)))]
    (cond
      (and (= :done (:kind e)) (pos? (get-in e [:data :logs] 0))) (do (ctx/update-mem! c assoc :phase :collect) :again)
      stop? (if (pos? (:tidied (ctx/mem c) 0))
              (finish! c)
              (ctx/end c (:kind e) (:cause e) {:tidied 0}))
      :else (end-tree! c true false))))

(defn ^:async step
  "One piece of the work: :again, :continue (a child waits on the world) or the ending."
  [c]
  (let [tree (or (:tree (ctx/mem c)) (next-tree! c))]
    (if-not tree
      (or (and (zero? (:tidied (ctx/mem c) 0)) (await (look/survey! c))) (finish! c))
      (let [phase (:phase (ctx/mem c))
            [_ job cargs] (some #(when (= phase (first %)) %) (phases (:args c) tree))
            e (await (ctx/call c phase job cargs))]
        (cond
          (= :continue e) :continue
          (= :fell phase) (fell-ended! c e)
          (= :collect phase) (if (:ground tree)
                               (do (ctx/update-mem! c assoc :phase :plant) :again)
                               (end-tree! c false true))
          :else (end-tree! c false (not= :done (:kind e))))))))

(defn ^:async round
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) (settings/get settings ::max-steps)) (step c) :continue)))))

;; ------------------------------------------------------------------ the contract

(def target [:species])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)

(defn done?
  "No trunk is chosen or in sight, the body has looked round and no child is under way; the trunks tidied and the
  ones in sight."
  [c]
  (let [m (ctx/mem c)
        left (count (leftovers c))
        data [:units :trunks :done (:tidied m 0) :left left :result (result-of c)]]
    (cond
      (or (:tree m) (pos? left) (not (look/surveyed? c)))
      (apply contract/later :tidying (str "tidy: " left " trunks in sight" (when (:tree m) ", one under way")) data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up "tidy: no trunk left, wrapping up" data)
      :else (apply contract/ok :tidied (str "tidied " (:tidied m 0) " trunks, none left in sight") data))))

(defn quote-of [c]
  (let [k (or (:max-trees (:args c)) (settings/get settings ::max-trees))]
    {:uses {:job-time [2 (* 15 (min k 2)) (max (settings/get settings ::job-time-s) (* 30 k))]}
     :done [0 1 k]}))
