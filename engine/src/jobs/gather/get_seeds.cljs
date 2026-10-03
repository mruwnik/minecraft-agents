(ns jobs.gather.get-seeds
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [jobs.storage.deposit :as deposit]))

(def doc
  "Carry :count more of :item (seeds) than at the start, renewably: break the
  :sources blocks (grass drops wheat seeds 1 time in 8) and collect the item,
  or take it from :chest. The goal (carried + :count) is fixed in the first
  round. One step per round: (1) carrying the goal ends :count; (2) with a
  :chest the withdraw child takes the item from it, ending :count when the goal
  is carried, else :short (with withdraw's reason when it gave up); (3) after
  a dig batch the collect-drops child picks up the item (and only it) within
  :collect-radius; (4) :dry-digs digs in a row that brought no new item end
  :dry (warn get-seeds.gave-up); (5) the nearest :sources blocks within
  :radius not skipped, at most :per-round of them, are walked to (within 3)
  and dug in order: a block whose walk is blocked or partial is skipped and
  the batch goes on, a dig that is neither dug nor missing skips the block (tall grass
  takes its other half with it, so a missing one is fine); no block left ends
  :none; two batches in a row where nothing was diggable (no block dug and none
  skipped: all missing) end :barren (warn get-seeds.gave-up), while a batch of only
  skipped blocks is not barren (the job ends :none once every source in radius
  is skipped). Hands over {:got n :reason r} (info get-seeds.done);
  :got is how many more are carried than at the start, at least 0.")

(def args
  {:item {:doc "item to gather" :default "wheat_seeds"}
   :count {:doc "how many more to carry than at the start" :default 8}
   :radius {:doc "source blocks within this many blocks of the body count" :default 16}
   :sources {:doc "block names to break" :default ["short_grass" "tall_grass"]}
   :per-round {:doc "blocks dug per round at most" :default 4}
   :chest {:doc "chest position: take the item from it instead of breaking blocks" :default nil}
   :collect-radius {:doc "how far around to collect drops after a batch" :default 8}
   :dry-digs {:doc "digs in a row that brought no new item before giving up" :default 40}})

(def reach 3)

(defn carried
  "Total of the item carried over all stacks."
  [c]
  (deposit/carried (u/inventory (:primitives c)) (:item (:args c))))

(defn source-blocks
  "Source blocks within :radius not skipped, nearest first, as positions."
  [c]
  (let [{:keys [radius sources]} (:args c)
        skipped (set (:skipped (ctx/mem c)))
        here (u/self-pos c)]
    (->> (array-seq (.blocks (:primitives c) #js {:radius radius :names (clj->js sources) :max (+ 64 (count skipped))}))
         (map #(u/pos-of (.-pos %)))
         (remove skipped)
         (sort-by #(u/dist here %))
         vec)))

(defn check [c]
  (boolean (or (:goal (ctx/mem c))
               (some? (:chest (:args c)))
               (seq (source-blocks c)))))

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [{:keys [goal]} (ctx/mem c)
        got (max 0 (- (carried c) (- goal (:count (:args c)))))]
    (ctx/emit! c :get-seeds.done :info {:got got :reason reason
                                        :text (str "get-seeds done: " (name reason) ", got " got)})
    (ctx/result! c {:got got :reason reason})
    :done))

(defn give-up!
  "Warn and end."
  [c reason]
  (ctx/emit! c :get-seeds.gave-up :warn {:reason reason :skipped (count (:skipped (ctx/mem c)))
                                         :text (str "get-seeds gave up: " (name reason))})
  (finish! c reason))

(defn skip! [c pos] (ctx/update-mem! c update :skipped (fnil conj []) pos))

(defn ^:async dig-one!
  "Walk to pos and dig it: :dug, :missing or :skipped (a blocked or partial walk, or a dig that is neither dug nor missing)."
  [c pos]
  (let [walked (await (u/walk-near! c pos reach))]
    (cond
      (contains? #{:blocked :partial} walked) (do (skip! c pos) :skipped)
      :else (let [status (.-status (await (ctx/act c :dig (clj->js {:pos pos}))))]
              (cond
                (= "dug" status) (do (ctx/update-mem! c update :dry (fnil inc 0)) :dug)
                (= "missing" status) :missing
                :else (do (skip! c pos) :skipped))))))

(defn ^:async dig-batch!
  "Dig the positions in order; {:dug n :skipped m} counts."
  [c targets]
  (loop [todo targets counts {:dug 0 :skipped 0}]
    (if-let [pos (first todo)]
      (let [r (await (dig-one! c pos))]
        (recur (rest todo) (cond-> counts (contains? counts r) (update r inc))))
      counts)))

(defn ^:async take! [c]
  (let [{:keys [chest item]} (:args c)
        goal (:goal (ctx/mem c))
        r (await (ctx/call-child c :take 'jobs.storage.withdraw {:chest chest :items {item goal}}))
        out (ctx/child-result c :take)]
    (cond
      (not= :done r) :continue
      (>= (carried c) goal) (finish! c :count)
      (:gave-up out) (do (ctx/emit! c :get-seeds.gave-up :warn {:reason (:reason out) :text (str "get-seeds: withdraw gave up: " (:reason out))})
                         (finish! c :short))
      :else (finish! c :short))))

(defn ^:async collect! [c]
  (let [{:keys [item collect-radius]} (:args c)
        r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius collect-radius :filter [item]}))]
    (when (= :done r)
      (let [now (carried c)]
        (ctx/update-mem! c (fn [m]
                             (-> m
                                 (dissoc :collecting)
                                 (cond-> (> now (:last-carried m 0)) (assoc :dry 0))
                                 (assoc :last-carried now))))))
    :continue))

(defn ^:async dig-round! [c targets]
  (let [{:keys [dug skipped]} (await (dig-batch! c targets))
        barren (if (zero? (+ dug skipped)) (inc (:barren (ctx/mem c) 0)) 0)]
    (ctx/update-mem! c assoc :barren barren :collecting true)
    (if (>= barren 2)
      (give-up! c :barren)
      :continue)))

(defn ^:async round [c]
  (when-not (:goal (ctx/mem c))
    (let [now (carried c)]
      (ctx/update-mem! c assoc :goal (+ now (:count (:args c))) :last-carried now :dry 0 :barren 0)))
  (let [{:keys [goal collecting dry]} (ctx/mem c)
        {:keys [chest dry-digs per-round]} (:args c)
        targets (when-not (or chest collecting) (vec (take per-round (source-blocks c))))]
    (cond
      (>= (carried c) goal) (finish! c :count)
      (some? chest) (await (take! c))
      collecting (await (collect! c))
      (>= dry dry-digs) (give-up! c :dry)
      (empty? targets) (finish! c :none)
      :else (await (dig-round! c targets)))))
