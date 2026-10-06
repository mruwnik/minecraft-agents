(ns jobs.gather.get-seeds
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.items.smelt :as smelt]
            [jobs.storage.deposit :as deposit]
            [jobs.lib.world :as known]))

(def doc
  "Carry :count more of :item (a planting material) than at the start, renewably.

  The way depends on the material:
  - wheat_seeds: break grass (:sources, 1 drop in 8).
  - sugar_cane and bamboo: cut a wild stand above its base (the second segment from the bottom, so the base
    grows again; a stand of one is never cut).
  - carrot, potato, beetroot_seeds: take from a chest only, never from a field.
  - A :chest or :plan always means the chest way. :sources always means the break way. Any other material
    with neither declines.

  The goal (carried + :count) is fixed in the first round. One step per round:
  1. Carrying the goal ends :count.
  2. Chest way: the withdraw child takes the item. It ends :count when the goal is carried, else :short (warn
     get-seeds.gave-up with withdraw's reason when it gave up).
  3. After a dig batch the collect-drops child picks up the item (only it) within :collect-radius.
  4. :dry-digs digs in a row that brought no new item end :dry (warn get-seeds.gave-up).
  5. Otherwise a batch: the nearest source blocks within :radius not skipped (for a stand: its cut cells) are
     judged by jobs.lib.access.rules. At most :per-round of the permitted ones are walked to (within 3) and
     dug, each judged again right before its dig.

  Skipped blocks: a cell in a zone that bars :dig or in an active plan's footprint is skipped for good and
  remembered. So is one with a hazard not in :accept, one whose walk is blocked or partial, and one whose dig
  is neither dug nor missing (tall grass takes its other half with it, so missing is fine).

  No block left ends :refused (warn get-seeds.gave-up with :zones and :plans) when any was refused, else
  :none. Two batches in a row where nothing was diggable (none dug, none skipped: all missing) end :barren
  (warn get-seeds.gave-up). A batch of only skipped blocks is not barren.

  Hands over {:got n :reason r} (info get-seeds.done). :got is how many more are carried than at the start, at
  least 0.

  Declines (one warn get-seeds.declined {:reason r} per reason): :no-source (a material with no known way),
  :no-chest (chest way with no :chest and no known chest place), :plan-missing, :plan-broken,
  :no-chest-cell (the :plan has no chest cell), :no-zones (break way with no zone list read), and :too-short
  (cane or bamboo in range but no stand of two or more: the job stays queued, a stand may grow).")

(def args
  {:item {:doc "the planting material to gather" :default "wheat_seeds"}
   :count {:doc "how many more to carry than at the start" :default 8}
   :radius {:doc "source blocks within this many blocks of the body count" :default 16}
   :sources {:doc "block names to break for the item; nil: the material's own (grass for wheat_seeds)" :default nil}
   :per-round {:doc "blocks dug per round at most" :default 4}
   :chest {:doc "chest position: take the item from it instead of breaking blocks" :default nil}
   :plan {:doc "id of a plan: take the item from the chest cell (want \"chest\") of the plan" :default nil}
   :collect-radius {:doc "how far around to collect drops after a batch" :default 8}
   :dry-digs {:doc "digs in a row that brought no new item before giving up" :default 40}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block :under-feet)"
            :default #{:falling-block :under-feet}}})

(def reach 3)

(def materials
  {"wheat_seeds" {:from :grass :sources ["short_grass" "tall_grass"]}
   "sugar_cane" {:from :stalk :block "sugar_cane"}
   "bamboo" {:from :stalk :block "bamboo"}
   "carrot" {:from :chest}
   "potato" {:from :chest}
   "beetroot_seeds" {:from :chest}})

(defn mode
  "How the item is got: :chest, :grass, :stalk or :unknown."
  [{:keys [chest plan sources item]}]
  (cond
    (or chest plan) :chest
    sources :grass
    :else (get-in materials [item :from] :unknown)))

(defn carried
  "Total of the item carried over all stacks."
  [c]
  (deposit/carried (u/inventory (:primitives c)) (:item (:args c))))

(defn chest-want? [want]
  (or (= "chest" want) (and (map? want) (= "chest" (:block want)))))

(defn plan-chest
  "{:chest [x y z]} of the plan's chest cell, or {:trouble reason}."
  [c id]
  (let [answer (known/plan c id)
        cell (some #(when (chest-want? (:want %)) (:pos %)) (:cells answer))]
    (cond
      (nil? answer) {:trouble :plan-missing}
      (:broken answer) {:trouble :plan-broken}
      (nil? cell) {:trouble :no-chest-cell}
      :else {:chest cell})))

(defn chest-target
  "{:chest value-for-withdraw} (nil: the known chest place) or {:trouble reason}."
  [c]
  (let [{:keys [plan chest] :as a} (:args c)]
    (cond
      plan (plan-chest c plan)
      (deposit/chest-of (ctx/view c) a) {:chest chest}
      :else {:trouble :no-chest})))

(defn cut-cell?
  "Whether the stalk block at pos is the second segment of its stand: a stalk under it, none under that."
  [c name {:keys [x y z]}]
  (let [at #(u/block-name (:primitives c) {:x x :y % :z z})]
    (and (= name (at (dec y))) (not= name (at (- y 2))))))

(defn source-blocks
  "Source blocks within :radius not skipped, nearest first, as positions (for a stand, its cut cells)."
  [c]
  (let [{:keys [radius sources item] :as a} (:args c)
        stalk? (= :stalk (mode a))
        names (cond stalk? [(:block (materials item))] sources sources :else (:sources (materials item)))
        skipped (set (:skipped (ctx/mem c)))
        here (u/self-pos c)]
    (->> (smelt/seen-blocks (:primitives c) names radius (+ (if stalk? 512 64) (count skipped)))
         (map :pos)
         (remove skipped)
         (filter #(or (not stalk?) (cut-cell? c (first names) %)))
         (sort-by #(u/dist here %))
         vec)))

(defn stalks-in-range?
  "Whether any block of the stalk material is within :radius (a stand there may be too short to cut)."
  [c]
  (let [{:keys [radius item]} (:args c)]
    (boolean (seq (smelt/seen-blocks (:primitives c) [(:block (materials item))] radius 8)))))

(defn decline!
  "One warn per reason, then false for the check, noting the reason with ctx/wait."
  [c reason]
  (ctx/warn-once! c [:declined reason] :get-seeds.declined
                  {:reason reason :text (str "get-seeds declined: " (name reason))})
  (ctx/wait c reason))

(defn check [c]
  (let [m (mode (:args c))]
    (cond
      (= :unknown m) (decline! c :no-source)
      (= :chest m) (if-let [reason (:trouble (chest-target c))] (decline! c reason) true)
      (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) (decline! c :no-zones)
      (or (:goal (ctx/mem c)) (seq (source-blocks c))) true
      (and (= :stalk m) (stalks-in-range? c)) (decline! c :too-short)
      :else (ctx/wait c {:reason :nothing-in-range :radius (:radius (:args c))}))))

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
  ([c reason] (give-up! c reason {}))
  ([c reason fields]
   (ctx/emit! c :get-seeds.gave-up :warn (merge {:reason reason :skipped (count (:skipped (ctx/mem c)))
                                                 :text (str "get-seeds gave up: " (name reason))}
                                                fields))
   (finish! c reason)))

(defn refuse-up!
  "give-up! :refused, naming the zones and plans that refused."
  [c]
  (let [fields (access/refusal-fields (:refused (ctx/mem c)))]
    (give-up! c :refused (assoc fields :text (str "get-seeds gave up: refused by " (access/refusal-text fields))))))

(defn skip! [c pos] (ctx/update-mem! c update :skipped (fnil conj []) pos))

(defn refuse!
  "Skip pos for good and remember what refused it (verdict v)."
  [c pos v]
  (ctx/update-mem! c update :refused (fnil conj []) (select-keys v [:zone :claim :owner :plan]))
  (skip! c pos))

(defn vet!
  "The positions the rules permit to dig, in order. A refused one is skipped for good and remembered, a hazard
  not in :accept skips it; one not loaded is left for later."
  [c poss]
  (let [in (access/rules-input c)
        accept (:accept (:args c))]
    (reduce (fn [ok pos]
              (let [v (access/may-dig? in pos)]
                (case (access/judge v accept)
                  :ok (conj ok pos)
                  :refused (do (refuse! c pos v) ok)
                  :hazard (do (skip! c pos) ok)
                  ok)))
            [] poss)))

(defn ^:async dig-one!
  "Walk to pos and dig it: :dug, :missing or :skipped (a blocked or partial walk, a cell the rules no longer permit, or a dig that is neither dug nor missing)."
  [c pos]
  (let [walked (await (near/walk-near! c pos reach))]
    (if (contains? #{:blocked :partial} walked)
      (do (skip! c pos) :skipped)
      (let [v (access/may-dig? (access/rules-input c) pos)
            judged (access/judge v (:accept (:args c)))]
        (if (not= :ok judged)
          (do (if (= :refused judged) (refuse! c pos v) (skip! c pos)) :skipped)
          (let [status (.-status (await (ctx/act c :dig (clj->js {:pos pos}))))]
            (cond
              (= "dug" status) (do (ctx/update-mem! c update :dry (fnil inc 0)) :dug)
              (= "missing" status) :missing
              :else (do (skip! c pos) :skipped))))))))

(defn ^:async dig-batch!
  "Dig the positions in order; {:dug n :skipped m} counts."
  [c targets]
  (loop [todo targets counts {:dug 0 :skipped 0}]
    (if-let [pos (first todo)]
      (let [r (await (dig-one! c pos))]
        (recur (rest todo) (cond-> counts (contains? counts r) (update r inc))))
      counts)))

(defn ^:async take! [c]
  (let [{:keys [item]} (:args c)
        goal (:goal (ctx/mem c))
        chest (:chest (chest-target c))
        r (await (ctx/call-child c :take 'jobs.storage.withdraw
                                   (merge (select-keys (:args c) [:ignore-zones?]) {:chest chest :items {item goal}})))
        out (ctx/child-result c :take)]
    (cond
      (not= :done r) :continue
      (>= (carried c) goal) (finish! c :count)
      (:gave-up out) (do (ctx/emit! c :get-seeds.gave-up :warn (merge (select-keys out [:zones :claims])
                                                                      {:reason (:reason out) :text (str "get-seeds: withdraw gave up: " (:reason out))}))
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
        {:keys [dry-digs per-round]} (:args c)
        chest? (= :chest (mode (:args c)))
        targets (when-not (or chest? collecting) (vec (take per-round (vet! c (source-blocks c)))))]
    (cond
      (>= (carried c) goal) (finish! c :count)
      chest? (await (take! c))
      collecting (await (collect! c))
      (>= dry dry-digs) (give-up! c :dry)
      (and (empty? targets) (seq (:refused (ctx/mem c)))) (refuse-up! c)
      (empty? targets) (finish! c :none)
      :else (await (dig-round! c targets)))))
