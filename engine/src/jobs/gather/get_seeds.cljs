(ns jobs.gather.get-seeds
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]
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

  The goal (carried + :count) is fixed in the first step. One call runs these steps until the job ends (it yields with :continue only while a child or
  a walk waits on the world, and after 400 steps):
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
  :none (the text names the radius and the nearest source seen beyond it). Two batches in a row where nothing was diggable (none dug, none skipped: all missing) end :barren
  (warn get-seeds.gave-up). A batch of only skipped blocks is not barren.

  done? is :ok when the goal is carried and no child is under way: ends :done (data {:got n :reason :count}). Any
  other end is condition-failed with the cause :refused or :withdraw-declined, else nothing-possible with the cause
  :none, :dry, :barren or :short (the chest gave less), and data {:got n :reason r :text}; :got is how many more are
  carried than at the start, at least 0.

  Conditions (one warn get-seeds.declined {:reason r} per reason): :no-source (a material with no known way),
  :no-chest (chest way with no :chest and no known chest place), :plan-missing, :plan-broken, :no-chest-cell (the
  :plan has no chest cell), :no-zones (break way with no zone list read), :too-short (cane or bamboo in range but no
  stand of two or more: the job stays queued, a stand may grow) and :nothing-in-range (after one look around).
  Quote: time by the way (about 8 breaks for a seed from grass), yields :count of the item.")

(a/defargs args
  {:item {:doc "the planting material to gather" :spec a/item? :default "wheat_seeds"}
   :count {:doc "how many more to carry than at the start" :spec (a/int-in 0 nil) :default 8}
   :radius {:doc "source blocks within this many blocks of the body count" :spec (a/num-in 0 nil) :default 16}
   :sources {:doc "block names to break for the item; nil: the material's own (grass for wheat_seeds)" :spec (a/coll-of a/name?) :default nil}
   :per-round {:doc "blocks dug per step at most" :spec (a/int-in 1 nil) :default 4}
   :chest {:doc "chest position: take the item from it instead of breaking blocks" :spec ::a/pos :default nil}
   :plan {:doc "id of a plan: take the item from the chest cell (want \"chest\") of the plan" :spec a/name? :default nil}
   :collect-radius {:doc "how far around to collect drops after a batch" :spec (a/num-in 0 nil) :default 8}
   :dry-digs {:doc "digs in a row that brought no new item before giving up" :spec (a/int-in 1 nil) :default 40}
   :ignore-zones? :shared
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent water, :lava-adjacent seen lava, :falling-block)"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:falling-block}}})

(a/defargs settings
  {::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds a gathering may take: the walks, digs, drops and the chest." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:item :chest :plan])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)

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
  (storage/carried (u/inventory (:primitives c)) (:item (:args c))))

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
      (storage/chest-of (ctx/view c) a) {:chest chest}
      :else {:trouble :no-chest})))

(defn cut-cell?
  "Whether the stalk block at pos is the second segment of its stand: a stalk under it, none under that."
  [c name {:keys [x y z]}]
  (let [at #(u/seen-name (:primitives c) {:x x :y % :z z})]
    (and (= name (at (dec y))) (not= name (at (- y 2))))))

(defn source-blocks
  "Source blocks within :radius not skipped, nearest first, as positions (for a stand, its cut cells)."
  [c]
  (let [{:keys [radius sources item] :as a} (:args c)
        stalk? (= :stalk (mode a))
        names (cond stalk? [(:block (materials item))] sources sources :else (:sources (materials item)))
        skipped (set (:skipped (ctx/mem c)))
        here (u/self-pos c)]
    (->> (look/seen-blocks (:primitives c) {:names names :radius radius :max (+ (if stalk? 512 64) (count skipped)) :live? true})
         (map :pos)
         (remove skipped)
         (filter #(or (not stalk?) (cut-cell? c (first names) %)))
         (sort-by #(u/dist here %))
         vec)))

(defn stalks-in-range?
  "Whether any block of the stalk material is within :radius (a stand there may be too short to cut)."
  [c]
  (let [{:keys [radius item]} (:args c)]
    (boolean (seq (look/seen-blocks (:primitives c) {:names [(:block (materials item))] :radius radius :max 8 :live? true})))))

(defn decline!
  "One warn per reason, then the reason as the check's cause."
  [c reason]
  (ctx/warn-once! c [:declined reason] :get-seeds.declined
                  {:reason reason :text (str "get-seeds declined: " (name reason))})
  reason)

(defn no-source [c]
  (when (= :unknown (mode (:args c))) (decline! c :no-source)))

(defn chest-trouble [c]
  (when (= :chest (mode (:args c)))
    (some->> (:trouble (chest-target c)) (decline! c))))

(defn zones-missing? [c]
  (and (not (#{:chest :unknown} (mode (:args c)))) (nil? (known/zones c)) (not (:ignore-zones? (:args c)))))

(defn no-zones [c]
  (when (zones-missing? c) (decline! c :no-zones)))

(defn nothing-in-range
  "No source in range once the body has looked around: a stand too short to cut declines :too-short; the first look is
  the round's own."
  [c]
  (let [m (mode (:args c))]
    (when-not (or (#{:chest :unknown} m) (zones-missing? c) (:goal (ctx/mem c)) (seq (source-blocks c)))
      (cond
        (and (= :stalk m) (stalks-in-range? c)) (decline! c :too-short)
        (look/surveyed? c) :nothing-in-range))))

(def can-start?
  (cond/and (checks/refusing :source "a source known" no-source)
            (checks/refusing :chest "the chest is fit" chest-trouble)
            (checks/refusing :zones-read "zone list read" no-zones)
            (checks/refusing :in-range "a source in range" nothing-in-range)))

(defn done?
  "The goal fixed by the first round (:count more than were carried) is carried and no child is under way (a collect
  still picks up); the seeds got and still wanted."
  [c]
  (let [{:keys [item count]} (:args c)
        goal (:goal (ctx/mem c))
        left (max 0 (- (or goal (+ (carried c) count)) (carried c)))
        got (max 0 (- count left))
        data [:units :items :done got :left left]]
    (cond
      (or (nil? goal) (pos? left))
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

(def condition-reasons #{:refused :withdraw-declined})

(defn finish!
  "End the job: :done for :count, condition-failed for :refused and :withdraw-declined (a zone, claim
  or chest can change), else nothing-possible with the reason."
  [c reason]
  (let [{:keys [goal]} (ctx/mem c)
        got (max 0 (- (carried c) (- goal (:count (:args c)))))
        data {:got got :reason reason :text (str "get-seeds done: " (if (= :none reason) (none-why c) (name reason)) ", got " got)}]
    (if (= :count reason)
      (ctx/end c :done nil data)
      (ctx/end c (if (condition-reasons reason) :condition-failed :nothing-possible) reason data))))

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

(defn ^:async walk!
  "Walk to within (reach) of pos with a go-to child: :there, :partial (it waits on the world, or a leg that got nearer)
  or :blocked."
  [c pos]
  (let [e (await (child/call-walk* c :walk {:pos pos :range (reach) :one-way :closed :warn false :retry false :zone-tolls true
                                            :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue e) :partial
      (not= :done (:kind e)) :blocked
      (:arrived (:data e)) :there
      (:leg (:data e)) :partial
      :else :blocked)))

(defn ^:async dig-one!
  "Walk to pos and dig it: :dug, :missing, :walking (the walk waits on the world: yield) or :skipped (a blocked walk, a cell the rules no longer permit, or a dig that is neither dug nor missing)."
  [c pos]
  (let [walked (await (walk! c pos))]
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

(defn ^:async take! [c]
  (let [{:keys [item]} (:args c)
        goal (:goal (ctx/mem c))
        chest (:chest (chest-target c))
        e (await (ctx/call c :take 'jobs.storage.withdraw
                           (merge (select-keys (:args c) [:ignore-zones?]) {:chest chest :items {item goal}})))
        out (:data e)]
    (cond
      (= :continue e) :continue
      (>= (carried c) goal) (finish! c :count)
      (and (= :condition-failed (:kind e)) (not (contains? out :gave-up))) (give-up! c :withdraw-declined)
      (:gave-up out) (let [why (contract/cause-key e)]
                       (ctx/emit! c :get-seeds.gave-up :warn (merge (select-keys out [:zones :claims]) {:reason why :text (str "get-seeds: withdraw gave up: " (name why))}))
                       (finish! c :short))
      :else (finish! c :short))))

(defn ^:async collect! [c]
  (let [{:keys [item collect-radius]} (:args c)
        r (await (ctx/call c :collect 'jobs.forestry.collect-drops {:radius collect-radius :filter [item]}))]
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

(defn ^:async step [c]
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
      (and (empty? targets) (not (look/surveyed? c))) (do (await (look/survey! c)) :again)
      (empty? targets) (finish! c :none)
      :else (await (dig-round! c targets)))))

(defn ^:async round
  [c]
  (await (pace/steps! c #(step c))))
