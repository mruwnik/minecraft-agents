(ns jobs.gather.get-seeds
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.waits :as waits]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.storage :as storage]
            [jobs.lib.world :as known]))

(def doc
  "Carry at least :count of :item (a planting material), renewably.

  The way depends on the material:
  - wheat_seeds: break grass (:sources, 1 drop in 8). The :fertilize way (bone meal carried, never fetched) uses it once per run on
    the open grass within :radius (a jobs.farm.fertilize child) when no grass is left to break, then breaks what grew.
  - sugar_cane and bamboo: cut a wild stand above its base (the second segment from the bottom, so the base
    grows again; a stand of one is never cut).
  - Any other material declines (:no-source); a chest is items.obtain's way, not this job's. :sources always means the break way.
  The call's :ways: :break (default both) and :fertilize.

  The goal (:count carried, so a body that carries it is done at once) is fixed in the first step. One call runs these steps until the job ends (it yields with :continue only while a child or
  a walk waits on the world):
  1. Carrying the goal ends :count.
  2. After a dig batch the collect-drops child picks up the item (only it) within the collect-radius setting (8).
  3. The dry-digs setting (40) digs in a row that brought no new item end :dry (the ending says so).
  4. Otherwise a batch: the nearest source blocks within :radius not skipped (for a stand: its cut cells) are
     judged by jobs.lib.access.rules. At most the per-round setting (4) of the permitted ones are walked to (within 3) and
     dug, each judged again right before its dig.

  With none within :radius, the nearest one seen up to 64 blocks away is walked to (within the reach setting) and the batch goes on from there.

  Skipped blocks: a cell in a zone that bars :dig or in an active plan's footprint is skipped for good and
  remembered. So is one with a hazard not in :accept, one whose walk is blocked or partial, and one whose dig
  is neither dug nor missing (tall grass takes its other half with it, so missing is fine).

  No block left in reach ends :refused (the ending carries :zones and :plans) when any was refused, else
  :none (the text names the radius and the nearest source seen beyond it). Two batches in a row where nothing was diggable (none dug, none skipped: all missing) end :barren
  (the ending says so). A batch of only skipped blocks is not barren.

  done? is :ok when the goal is carried and no child is under way: ends :done (data {:got n :reason :count}). Any
  other end is condition-failed with the cause :refused or :none, else nothing-possible with the cause
  :dry or :barren, and data {:got n :reason r :text}; :got is how many more are
  carried than at the start, at least 0.

  Conditions (the check waits with the reason): :no-source (a material with no known way), :no-zones (break way with no zone list read), :too-short (cane or bamboo in range but no
  stand of two or more: the job stays queued, a stand may grow) and :nothing-in-range (after one look around).
  Quote: time by the way (about 8 breaks for a seed from grass), yields :count of the item.")

(a/defargs args
  {:item {:doc "the planting material to gather" :spec a/item? :default "wheat_seeds"}
   :count {:doc "how many to carry at least" :spec (a/int-in 0 nil) :default 8}
   :radius {:doc "source blocks within this many blocks of the body count" :spec (a/num-in 0 nil) :default 16}
   :sources {:doc "block names to break for the item; nil: the material's own (grass for wheat_seeds)" :spec (a/coll-of a/name?) :default nil}
   :ignore-zones? :shared
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent water, :lava-adjacent seen lava, :falling-block)"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:falling-block}}})

(a/defargs settings
  {::dry-digs {:default 40 :doc "Digs in a row that brought no new item before giving up (:dry)." :spec (a/int-in 1 nil)}
   ::per-round {:default 4 :doc "Blocks dug per step at most." :spec (a/int-in 1 nil)}
   ::collect-radius {:default 8 :doc "How far around to collect drops after a batch, in blocks." :spec (a/num-in 0 nil)}
   ::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds a gathering may take: the walks, digs and drops." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn dry-digs [] (settings/get settings ::dry-digs))
(defn per-round [] (settings/get settings ::per-round))
(defn collect-radius [] (settings/get settings ::collect-radius))
(defn time-s [] (settings/get settings ::job-time-s))

(def ways #{:break :fertilize})

(def target [:item])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)

(def materials
  {"wheat_seeds" {:from :grass :sources ["short_grass" "tall_grass"]}
   "sugar_cane" {:from :stalk :block "sugar_cane"}
   "bamboo" {:from :stalk :block "bamboo"}})

(defn mode
  "How the item is got: :grass, :stalk or :unknown."
  [{:keys [sources item]}]
  (cond
    sources :grass
    :else (get-in materials [item :from] :unknown)))

(defn carried
  "Total of the item carried over all stacks."
  [c]
  (storage/carried (u/inventory (:primitives c)) (:item (:args c))))

(defn cut-cell?
  "Whether the stalk block at pos is the second segment of its stand: a stalk under it, none under that."
  [c name {:keys [x y z]}]
  (let [at #(u/seen-name (:primitives c) {:x x :y % :z z})]
    (and (= name (at (dec y))) (not= name (at (- y 2))))))

(def far-radius "How far a source seen beyond :radius is still walked to." 64)

(defn source-blocks
  "Source blocks within radius (default :radius) not skipped, nearest first, as positions (for a stand, its cut cells)."
  ([c] (source-blocks c (:radius (:args c))))
  ([c radius]
  (let [{:keys [sources item] :as a} (:args c)
        stalk? (= :stalk (mode a))
        names (cond stalk? [(:block (materials item))] sources sources :else (:sources (materials item)))
        skipped (set (:skipped (ctx/mem c)))
        here (u/self-pos c)]
    (->> (look/seen-blocks (:primitives c) {:names names :radius radius :max (+ (if stalk? 512 64) (count skipped)) :live? true})
         (map :pos)
         (remove skipped)
         (filter #(or (not stalk?) (cut-cell? c (first names) %)))
         (sort-by #(u/dist here %))
         vec))))

(defn stalks-in-range?
  "Whether any block of the stalk material is within :radius (a stand there may be too short to cut)."
  [c]
  (let [{:keys [radius item]} (:args c)]
    (boolean (seq (look/seen-blocks (:primitives c) {:names [(:block (materials item))] :radius radius :max 8 :live? true})))))

(defn decline!
  "The reason as the check's cause."
  [c reason]
  reason)

(defn no-source [c]
  (when (= :unknown (mode (:args c))) (decline! c :no-source)))

(defn zones-missing? [c]
  (and (not= :unknown (mode (:args c))) (nil? (known/zones c)) (not (:ignore-zones? (:args c)))))

(defn no-zones [c]
  (when (zones-missing? c) (decline! c :no-zones)))

(defn spare-meal
  "The bone meal carried that the reserve register leaves to a fertilize."
  [c]
  (storage/carried (reserve/free c :fertilize) "bone_meal"))

(defn can-fertilize?
  "Whether the fertilize way may run now: allowed, grass is the source, unreserved bone meal is carried and it has not run yet."
  [c]
  (and (ctx/way? c :fertilize)
       (= :grass (mode (:args c)))
       (not (:fertilized (ctx/mem c)))
       (pos? (spare-meal c))))

(defn nothing-in-range
  "No source in range once the body has looked around: a stand too short to cut declines :too-short; the first look is
  the round's own."
  [c]
  (let [m (mode (:args c))]
    (when-not (or (= :unknown m) (zones-missing? c) (:goal (ctx/mem c)) (can-fertilize? c) (seq (source-blocks c far-radius)))
      (cond
        (and (= :stalk m) (stalks-in-range? c)) (decline! c :too-short)
        (look/surveyed? c) :nothing-in-range))))

(def can-start?
  (cond/and (checks/refusing :source "a source known" no-source)
            (checks/refusing :zones-read "zone list read" no-zones)
            (checks/refusing :in-range "a source in range" nothing-in-range)))

(defn got
  "How many more are carried than when the first round began."
  [c]
  (max 0 (- (carried c) (:had (ctx/mem c) (carried c)))))

(defn done?
  "The :count is carried (already at the start, or after the first round) and no child is under way (a collect
  still picks up); the seeds got and still wanted."
  [c]
  (let [{:keys [item count]} (:args c)
        left (max 0 (- count (carried c)))
        got (got c)
        data [:units :items :done got :left left]]
    (cond
      (pos? left)
      (apply contract/later :short (str "get-seeds: " left " " item " to get") (conj data :result {:got got}))

      (checks/child-under-way? c)
      (apply contract/later :wrapping-up "get-seeds: the goal is carried, wrapping up" (conj data :result {:got got :reason :count}))

      :else (apply contract/ok :carried (str "get-seeds done: count, got " got) (conj data :result {:got got :reason :count})))))

(defn quote-of [c]
  (let [{:keys [item count] :as a} (:args c)
        per (case (mode a) :grass 12 :stalk 4 3)
        mean (+ 2 (* per count))]
    {:uses {:job-time [(min 2 mean) mean (max mean (time-s))]}
     :yields {:items {item [0 count count]}}
     :done [0 count count]}))

(defn none-why
  "Why no source block is left: none within :radius, and the nearest seen beyond it (the seen-blocks cap is 64)."
  [c]
  (let [{:keys [radius sources item] :as a} (:args c)
        names (cond (= :stalk (mode a)) [(:block (materials item))] sources sources :else (:sources (materials item)))
        far (first (look/seen-blocks (:primitives c) {:names names :radius 64 :max 1 :live? true}))]
    (str (if-let [n (seq (:skipped (ctx/mem c)))]
           (str "all " (count n) " skipped within " radius)
           (str "no " (str/join "/" names) " within " radius))
         (when far (str ", the nearest seen is " (Math/round (u/dist (u/self-pos c) (:pos far))) " away")))))

(def condition-reasons #{:refused :none})

(defn finish!
  "End the job: :done for :count, condition-failed for :refused and :none (a zone, claim or source can change), else nothing-possible with the reason."
  ([c reason] (finish! c reason {}))
  ([c reason fields]
  (let [got (got c)
        data (merge {:got got :reason reason :text (str (if (= :count reason) "get-seeds done: " "get-seeds ended: ") (if (= :none reason) (none-why c) (name reason)) ", got " got)}
                    fields)]
    (if (= :count reason)
      (ctx/end c :done nil data)
      (ctx/end c (if (condition-reasons reason) :condition-failed :nothing-possible) reason data)))))

(defn give-up!
  "End with the reason, the skipped count and fields in the ending."
  ([c reason] (give-up! c reason {}))
  ([c reason fields]
   (finish! c reason (merge {:skipped (count (:skipped (ctx/mem c)))
                             :text (str "get-seeds gave up: " (name reason))}
                            fields))))

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
  "Walk to pos and dig it: :dug, :missing, :walking (the walk waits on the world: yield) or :skipped (a blocked walk, a cell the rules no longer permit, or a dig that is neither dug nor missing)."
  [c pos]
  (let [walked (await (near/go-near! c pos (reach)))]
    (cond
      (= :partial walked) :walking
      (= :blocked walked) (do (skip! c pos) :skipped)
      :else
      (let [v (access/may-dig? (access/rules-input c) pos)
            judged (access/judge v (:accept (:args c)))]
        (if (not= :ok judged)
          (do (if (= :refused judged) (refuse! c pos v) (skip! c pos)) :skipped)
          (let [outcome (await (blocks/dig-cell! c pos
                                                 {:accept (:accept (:args c))
                                                  :ignore-zones? (:ignore-zones? (:args c))}))]
            (case outcome
              :continue :walking
              :dug (do (ctx/update-mem! c #(-> % (update :dry (fnil inc 0)) (assoc :collecting true))) :dug)
              :missing :missing
              (do (skip! c pos) :skipped))))))))

(defn ^:async approach!
  "Walk toward the nearest source seen beyond :radius so it lies within it: :continue while the walk waits on the world, else :again (a blocked walk skips that block)."
  [c pos]
  (let [walked (await (near/go-near! c pos (reach)))]
    (when (= :blocked walked) (skip! c pos))
    (if (= :partial walked) :continue :again)))

(defn ^:async dig-batch!
  "Dig the positions in order; {:dug n :skipped m} counts, plus :walking true when a walk waits on the world (the rest is left for the next round)."
  [c targets]
  (loop [todo targets counts {:dug 0 :skipped 0}]
    (if-let [pos (first todo)]
      (let [r (await (dig-one! c pos))]
        (if (= :walking r)
          (assoc counts :walking true)
          (recur (rest todo) (cond-> counts (contains? counts r) (update r inc)))))
      counts)))

(defn ^:async collect! [c]
  (let [{:keys [item]} (:args c)
        r (waits/waiting-out c (await (ctx/call c :collect 'jobs.items.collect-drops {:radius (collect-radius) :items [item]})))]
    (when (not= :continue r)
      (let [now (carried c)]
        (ctx/update-mem! c (fn [m]
                             (-> m
                                 (dissoc :collecting)
                                 (cond-> (> now (:last-carried m 0)) (assoc :dry 0))
                                 (assoc :last-carried now))))))
    (if (= :continue r) :continue :again)))

(defn ^:async dig-round! [c targets]
  (let [{:keys [dug skipped walking]} (await (dig-batch! c targets))
        barren (if (zero? (+ dug skipped)) (inc (:barren (ctx/mem c) 0)) 0)]
    (cond
      (and walking (zero? (+ dug skipped))) :continue
      :else (do (ctx/update-mem! c assoc :barren barren :collecting true)
                (if (>= barren 2)
                  (give-up! c :barren)
                  :again)))))

(defn ^:async fertilize!
  "Bone meal on the open grass round the body (the child never fetches), then a look at what grew."
  [c]
  (let [{:keys [radius ignore-zones?]} (:args c)
        r (waits/waiting-out c (await (ctx/call c :fertilize 'jobs.farm.fertilize
                                                {:grass true :area {:pos (u/self-pos c) :radius radius}
                                                 :max (spare-meal c)
                                                 :ways #{:carried} :ignore-zones? ignore-zones?})))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c assoc :fertilized true)
          (await (look/survey! c))
          :again))))

(defn ^:async step [c]
  (when-not (:goal (ctx/mem c))
    (let [now (carried c)]
      (ctx/update-mem! c assoc :goal (:count (:args c)) :had now :last-carried now :dry 0 :barren 0)))
  (let [{:keys [goal collecting dry]} (ctx/mem c)
        targets (when-not collecting (vec (take (per-round) (vet! c (source-blocks c)))))]
    (cond
      (>= (carried c) goal) (finish! c :count)
      collecting (await (collect! c))
      (>= dry (dry-digs)) (give-up! c :dry)
      (and (empty? targets) (seq (:refused (ctx/mem c)))) (refuse-up! c)
      (and (empty? targets) (not (look/surveyed? c))) (do (await (look/survey! c)) :again)
      (and (empty? targets) (can-fertilize? c)) (await (fertilize! c))
      (empty? targets) (if-let [pos (first (vet! c (source-blocks c far-radius)))] (await (approach! c pos)) (finish! c :none))
      :else (await (dig-round! c targets)))))

(defn ^:async round
  [c]
  (await (pace/steps! c #(step c))))
