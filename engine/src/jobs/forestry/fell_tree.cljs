(ns jobs.forestry.fell-tree
  (:require [clojure.set :as set]
            [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.forestry.fell-tree.log :as log]
            [jobs.forestry.fell-tree.pillar :as pillar]
            [jobs.lib.waits :as waits]
            [jobs.lib.access :as access]
            [jobs.lib.checks :as checks]
            [jobs.lib.choose :as choose]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.part-cut :as part-cut]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.sense :as sense]
            [jobs.lib.targets :as targets]
            [jobs.lib.trees :as trees :refer [log-name? axe-worth-fetching? axe-fetch-limits trees-near tree-near tree-at unreachable-set debts replant-kind
                                    replant-policy default-radius max-partials]]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]))

(def doc
  "Fell the nearest tree: a log column with leaves near its top, lowest log first, the whole of it in one call. It yields
  (:continue) while a child waits on the world.
  The base log's :forestry/replant debt (one per cell of a 2x2 trunk; none with :replant? false or on a planned crop plot) is written before it is dug; a lower log of the
  column seen later becomes the base. It ends on the ground: from the canopy it walks down beside the base (warn
  a :stranded true result when it cannot). With :at it fells that one column instead (:radius and :species are then unused).
  The tree is the one the body walks to soonest among at most 32 candidates, nearest in a line first (jobs.lib.targets);
  one whose walk is blocked, or partial three times in a row, or ended 30 times in a row by the body shut in, is marked unreachable. A log out of reach from the foot
  is felled from a pillar (jobs.forestry.fell-tree.pillar: built of dirt, cobblestone and the logs cut, taken back; unless the call's :ways leave out :pillar); a tree that pillar cannot reach
  or whose pillar digs no log counts as unreachable. Once a tree is chosen the rest of it is a comfort section
  (ctx/atomic): reflexes at or below that level wait for it, up to their caps.
  Before the third log of a tree big enough for an axe to pay (jobs.lib.trees/axe-worth-fetching?) with none carried it
  gets one (jobs.lib.need) unless :ways is #{:carried}; a failed fetch ends the tree chopped by hand (:axe :none-found), asking once more (craft only) after two logs. Before the first log, a carried axe that is the last of
  its kind and worn gets a spare (jobs.lib.trip/tool-ready!).
  Each log is dug by a jobs.blocks.dig child that leaves the drop on the ground; a leaf hiding a log from the eye is dug
  first. When the tree is down a jobs.items.collect-drops child picks up the drops of the logs dug from the ground (by
  id: another player's logs are left); the pillar's logs are collected by the pillar way before it comes down.
  Zones: a tree whose base log is in another owner's zone or claim, or in a plan's footprint (but :for-plan's own), is
  not a candidate; a later log that turns out refused makes the tree unreachable (warn fell-tree.declined once, :reason
  :refused). :ignore-zones? true skips the check. Logs joined to the tree that the same rule refuses, or that lie more than
  the fell-tree.log/max-spread setting from the trunk's column, stay standing.

  Conditions: :no-tree (none in sight
  after one look around; data :radius, :species; with :at and no log there it is impossible, data :at).
  Its safe? is ready/safe-at-by-night at :at, else at the base of the tree it goes to, ready/safe-if-lit with no tree.
  Ends :done with {:base pos :logs n :axe why}: n logs dug; :axe is :carried, :skipped-fetch-off, :skipped-small-tree or
  :none-found (fetch failed: chopped by hand). Condition-failed :no-tree, :gone (no log left, none dug) or :logs-left
  (cause {:left n}: the pick-up stopped short). Nothing-possible :tree-unreachable when every tree in sight proved out of
  reach (data {:unreachable n :columns :why}) and :tree-blocked when a log cannot be dug or the pillar not taken back
  three rounds in a row. The pillar's blocks that cannot be had (condition-failed :need) or a limit hit end the call; a pillar that cannot be built or digs no log makes the tree unreachable.
  Quote: time by the walk to the nearest tree and the logs standing.")

(a/defargs args
  {:species {:doc "log species such as \"oak\"; any when nil" :spec a/name? :default nil}
   :radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :at {:doc "{:x :y :z} of a base log: fell that one column, wherever the body is (the radius and species are not used), instead of the nearest tree" :spec ::a/pos :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :lava-adjacent :falling-block" :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}
   :avoid {:doc "columns [x z] of trees to leave alone (ones an earlier run found cut off)" :spec (a/coll-of (a/pred "an [x z] column" vector?)) :default nil}
   :replant? {:doc "write a :forestry/replant debt per trunk cell felled; false when the tree is only in the way (clearing a site: a player would not plant there)" :spec boolean? :default true}
   :part-cut? {:doc "fell only part-cut trunks (jobs.lib.part-cut: the lowest log raised over air, not joined to a standing tree)" :spec boolean? :default false}
   :ignore-zones? :shared})

(def ways (conj need/fetch-ways :pillar))

(a/defargs settings
  {::approach-range {:default 3 :doc "Blocks from a tree's base the search for the tree to fell counts as reaching it." :spec (a/num-in 0 nil)}
   ::at-range {:default 6 :doc "Blocks from the :at column the body walks to when it has not seen the cell." :spec (a/num-in 0 nil)}
   ::at-close-range {:default 2 :doc "Blocks from the :at column it walks to when the cell stayed unseen from ::at-range (a wall hides the foot)." :spec (a/num-in 0 nil)}
   ::collect-radius {:default 8 :doc "Blocks around the base within which the drops of the logs dug are collected." :spec (a/num-in 0 nil)}
   ::collect-search {:default 4 :doc "Blocks round the base walked to look for drops that are not in sight." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 900 :doc "Seconds a felling may take: the search, a fetch, the walk, the digs and a pillar." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))
(defn approach-range [] (settings/get settings ::approach-range))

(def job 'jobs.forestry.fell-tree)

(defn tree-logs
  "The logs of the chosen column: read from :at up when the job was given one, else those in radius."
  [c radius]
  (log/tree-logs c radius (ctx/mem c)))

(defn lower-base
  "The position of the lowest of logs when it lies under the committed base of mem (lower logs of the trunk seen late),
  else nil."
  [mem logs]
  (let [low (:pos (first logs))
        base (:base mem)]
    (when (and low base (< (:y low) (:y base)) (= (:x low) (:x base)) (= (:z low) (:z base)))
      low)))

(defn rebase!
  "Move the committed base (and its replant debt) down to pos: the sapling goes on the ground, not on a stump."
  [c pos]
  (ctx/forget-where! c replant-kind #(= (log/debt-pos (:primitives c) (:base (ctx/mem c))) (:pos %)))
  (ctx/update-mem! c assoc :base pos))

(defn on-canopy?
  "Whether the body stands more than a block over the base: up in the leaves."
  [c]
  (let [base (:base (ctx/mem c))]
    (boolean (and base (> (js/Math.floor (:y (u/self-pos c))) (inc (:y base)))))))

(defn ^:async come-down!
  "Walk to the ground beside the base (a go-to child, slot :down) when the body stands up in the canopy. :continue while
  go-to waits on the world, else nil; a walk that does not arrive sets :stranded in the result."
  [c]
  (when (on-canopy? c)
    (let [{:keys [x y z]} (:base (ctx/mem c))
          e (await (child/call-walk* c :down {:pos [x y z] :range 2 :ways #{:walk :clear-seal}}))]
      (cond
        (= :continue e) :continue
        (not (and (= :done (:kind e)) (:arrived (:data e))))
        (ctx/update-mem! c assoc :stranded true)))))

(defn own-spared?
  "Whether the dig at pos is refused only by a plan this body made: a build it spares, not a refusal to report."
  [in pos]
  (let [v (access/may? in :dig pos)]
    (and (= :footprint (:reason v)) (contains? (:own-plans in) (:plan v)))))

(defn candidates
  "The trees the job may fell, nearest in a line first (lazily): the one at :at (none once marked unreachable), else
  those of species within radius not marked unreachable; a tree whose base log the zones refuse is left out, and with
  :part-cut? one that is not a part-cut trunk or whose column a fell-tree gave up on lately (the trigger skips those too)."
  [c radius species]
  (let [p (:primitives c)
        given-up (when (:part-cut? (:args c)) (into #{} (map (comp :column :data)) (ctx/entries c trees/unreachable-kind)))
        excluded (-> (unreachable-set (ctx/mem c)) (into (map vec) (:avoid (:args c))) (into given-up))
        logs (delay (into #{} (map :pos) (trees/scan-logs p radius nil)))
        wanted? (fn [t] (or (not (:part-cut? (:args c))) (part-cut/part-cut? p logs t)))]
    (if-let [at (:at (:args c))]
      (when-let [t (tree-at p at)]
        (when-not (or (excluded [(:x at) (:z at)]) (not (log/log-allowed? c (:base t))) (not (wanted? t))) [t]))
      (let [in (access/rules-input c (log/gate-opts c))]
        (filter #(and (not (own-spared? in (:base %))) (log/log-allowed? c (:base %)) (wanted? %))
                (trees-near p radius species excluded))))))

(defn commit-tree!
  "Commit the column, species and base of the tree t to fell."
  [c t]
  (ctx/update-mem! c #(-> % (merge (select-keys t [:column :species :base])) (assoc :partials 0) (dissoc :logs-seen :relooked :drop-ids :cut-cells :collected :stand-avoid)))
  t)

(defn mark-all-unreachable!
  "Remember the columns of trees as unreachable."
  [c trees]
  (ctx/update-mem! c update :unreachable (fnil into []) (map (fn [t] [(get-in t [:column :x]) (get-in t [:column :z])]) trees)))

(defn walk-target
  "The cell the body walks to for tree t: its base, or the ground under a base raised over air (a part-cut trunk; no walk
  reaches a cell in the air, so the search would flood the land)."
  [p t]
  (let [base (:base t)]
    (or (when (part-cut/raised? p base) (part-cut/ground-cell p base)) base)))

(defn ^:async choose-tree!
  "Choose the tree to fell among the candidates (at most targets/max-targets): the one the body walks to soonest
  (targets/nearest!), committed (commit-tree!). :searching while that search goes on; nil when no candidate is in
  sight, or when the search proved every one out of reach (all marked unreachable); the nearest in a line when it ran
  out of nodes, or when the body is shut in (its walk digs or builds out, which the search does not count): the
  nearest in a line (the walk decides)."
  [c radius species]
  (let [ts (vec (take targets/max-targets (candidates c radius species)))]
    (when (seq ts)
      (let [a (await (targets/nearest! c (mapv #(walk-target (:primitives c) %) ts) (approach-range) {:tag :fell-tree}))]
        (case (:status a)
          :found (commit-tree! c (nth ts (:index a)))
          :searching :searching
          (if (and (:proved a) (not (reach-room/enclosed? (:primitives c))))
            (do (mark-all-unreachable! c ts) nil)
            (commit-tree! c (first ts))))))))

;; ------------------------------------------------------------------ the pillar

(defn ^:async rerank-while-threatened!
  "While a hostile holds the job off its chosen tree before a log is dug, choose again with what the body senses now
  (the search prices known dangers): true once another tree is committed. The same tree again is kept as it was."
  [c]
  (when (and (zero? (:dug (ctx/mem c) 0)) (= :later (first (ready/threatened c))))
    (let [{:keys [radius species]} (:args c)
          before (ctx/mem c)]
      (loop [n 0]
        (let [t (await (choose-tree! c radius species))]
          (cond
            (and (= :searching t) (< n 20)) (do (await (pace/pace!)) (recur (inc n)))
            (not (map? t)) false
            (= (:column t) (:column before)) (do (ctx/update-mem! c assoc :partials (:partials before 0)) false)
            :else true))))))

(defn ^:async walk-failed!
  "Book a walk result of :blocked or :partial against the chosen tree: :again once the tree is given up, or another
  tree is chosen because a hostile holds the walk, :continue while the walk (its child) waits."
  [c r]
  (let [partials (inc (:partials (ctx/mem c) 0))]
    (cond
      (and (= :partial r) (await (rerank-while-threatened! c))) :again
      (or (= :blocked r) (>= partials max-partials)) (do (log/mark-unreachable! c (:walk-why (ctx/mem c))) :again)
      :else (do (ctx/update-mem! c assoc :partials partials) :continue))))

(defn axe-carried? [c]
  (boolean (some #(str/ends-with? (:name %) "_axe") (u/inventory (:primitives c)))))

(defn crown-height
  "Logs the seen leaves round the base column imply the trunk has (base to the highest such leaf), 0 without a base: the
  body's first sight misses the top logs, not the crown."
  [c]
  (let [{:keys [base column]} (ctx/mem c)]
    (if-not base
      0
      (let [near? (fn [{:keys [pos]}] (and (<= (abs (- (:x pos) (:x column))) 4) (<= (abs (- (:z pos) (:z column))) 4)))
            tops (keep #(when (near? %) (:y (:pos %))) (trees/scan (:primitives c) (+ (:radius (:args c) default-radius) 4) trees/leaves-name? trees/max-leaves))]
        (if (seq tops) (inc (- (apply max tops) (:y base))) 0)))))

(defn axe-wanted?
  "Whether an axe should be got before the next log of a tree of logs: none carried, a tree an axe pays for (the logs dug
  and still seen, or the height of the crown over the base: the first sight of a tall tree misses its top logs), and either fewer than two dug and no fetch failed, or a
  fetch failed and two logs are dug since (they may now craft one; once)."
  [c logs]
  (let [{:keys [dug axe-failed axe-retried]} (ctx/mem c)]
    (boolean (and (not (axe-carried? c))
                  (axe-worth-fetching? (max (+ (or dug 0) (count logs)) (crown-height c)))
                  (if axe-failed
                    (and (>= (or dug 0) 2) (not axe-retried))
                    (< (or dug 0) 2))))))

(defn axe-fetch-opts
  "The limits {:how} for getting an axe, nil when the call's ways and the axe's sources share none; retry? asks only to craft."
  [c retry?]
  (when-let [how (seq (set/intersection (need/fetch-how c) (:how (axe-fetch-limits))))]
    {:how (if retry? #{:craft} (set how))}))

(defn ^:async axe!
  "Get an axe before a log of a tree it pays for (axe-wanted?): nil to go on (one came, or none could be had: the tree is
  chopped by hand), :continue while the fetch waits on the world."
  [c logs]
  (when (axe-wanted? c logs)
    (when-let [opts (axe-fetch-opts c (:axe-failed (ctx/mem c)))]
      (let [e (await (need/tool! c {:kind "axe"} opts))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) nil
          :else (do (ctx/update-mem! c #(cond-> (assoc % :axe-failed true) (:axe-failed %) (assoc :axe-retried true)))
                    nil))))))

(defn axe-status
  "Why the tree was felled with or without an axe: :carried, :skipped-fetch-off, :skipped-small-tree or :none-found."
  [c]
  (cond
    (axe-carried? c) :carried
    (empty? (need/fetch-how c)) :skipped-fetch-off
    (not (axe-worth-fetching? (:first-logs (ctx/mem c) 0))) :skipped-small-tree
    :else :none-found))

(defn ^:async walk-to-at!
  "With :at on a cell the body has not seen, walk to within at-range of it (a go-to child; at-close-range once a look from
  there left it unseen) so the column can be read. :partial while that walk goes on, else nil."
  [c]
  (when-let [at (:at (:args c))]
    (when-not (u/seen-name (:primitives c) at)
      (let [range (settings/get settings (if (:at-looked (ctx/mem c)) ::at-close-range ::at-range))]
        (when (= :partial (await (log/walk-near! c at range))) :partial)))))

(defn no-tree-cause [c]
  (let [{:keys [radius species at]} (:args c)]
    (cond-> {:reason :no-tree :radius radius} species (assoc :species species) at (assoc :at at))))

(defn tree-result [c]
  (let [m (ctx/mem c)]
    (cond-> {:base (:base m) :logs (:dug m 0) :axe (axe-status c)}
      (:stranded m) (assoc :stranded true))))

(defn relook-cells
  "The cells of logs last seen of the tree that are unknown now (the body moved, e.g. to collect a drop), once per tree:
  they are looked at before the tree counts as down."
  [c]
  (let [m (ctx/mem c)]
    (when-not (:relooked m)
      (filterv #(sense/unknown? (:primitives c) %) (map (fn [{:keys [x y z]}] [x y z]) (:logs-seen m))))))

(defn ^:async collect-logs!
  "Pick up the drops of the logs dug from the ground (a collect-drops child by their ids, once per tree; it walks round
  the base to look when none is in sight). :continue while it waits on the world, else nil; drops it cannot find are not
  this job's failure, but a collect that stopped short (:least not met, held up) ends the tree :condition-failed :logs-left."
  [c]
  (let [{:keys [base drop-ids collected]} (ctx/mem c)]
    (when (and (seq drop-ids) (not collected))
      (let [e (waits/waiting-out c (await (ctx/call c :collect 'jobs.items.collect-drops
                               (log/collect-args c {:radius (settings/get settings ::collect-radius) :ids drop-ids :near base :search (settings/get settings ::collect-search) :least (count drop-ids)}))))]
        (cond
          (= :continue e) :continue
          (or (= :done (:kind e)) (and (= :nothing-possible (:kind e)) (= :none-collected (contract/cause-key e))))
          (do (ctx/update-mem! c assoc :collected (get-in e [:data :collected] 0)) nil)
          :else (ctx/end c :condition-failed {:reason :logs-left :left (max 0 (- (count drop-ids) (get-in e [:data :collected] 0)))} (tree-result c)))))))

(defn ^:async ground-step
  "One piece of the tree from the ground: the axe, the next log dug (walking in reach first), the tree ended when no log
  is left. :again, :continue or the ending."
  [c radius]
  (let [logs (await (sense/decide! c ::logs #(tree-logs c radius) {:retry? empty?}))]
    (cond
      (empty? logs)
      (if-let [unseen (not-empty (relook-cells c))]
        (do (ctx/update-mem! c assoc :relooked true)
            (await (sense/look-at-unknown! c unseen (sense/max-looks)))
            :again)
        (if-let [stop (or (when (= :continue (await (come-down! c))) :continue) (await (collect-logs! c)))]
          stop
          (if (zero? (:dug (ctx/mem c) 0))
            (ctx/end c :condition-failed {:reason :gone :base (:base (ctx/mem c))} nil)
            (ctx/end c :done nil (tree-result c)))))

      :else
      (do
        (ctx/update-mem! c assoc :logs-seen (mapv :pos logs))
        (when (and (zero? (:dug (ctx/mem c) 0)) (not (:first-logs (ctx/mem c))))
          (ctx/update-mem! c assoc :first-logs (count logs)))
        (or (await (axe! c logs))
            (when (and (zero? (:dug (ctx/mem c) 0)) (axe-carried? c))
              (await (trip/tool-ready! c (:name (first logs)) (count logs))))
            (let [_ (when-let [low (lower-base (ctx/mem c) logs)] (rebase! c low))
                  _ (await (watch/watch! c {:before-dig (:pos (first logs))}))
                  m (ctx/mem c)
                  r (await (log/dig-log! c (first logs) {:base (:base m) :species (:species m)}))]
              (case r
                :ok (do (ctx/update-mem! c assoc :partials 0)
                        (u/progress! c)
                        :again)
                :high (do (ctx/update-mem! c assoc :pillar {}) :again)
                (:partial :blocked) (walk-failed! c r)
                (:unreachable :cannot :out-of-reach :refused) (do (log/mark-unreachable! c) :again)
                (log/blocked! c (str "cannot dig the tree: " (name r))))))))))

(defn ^:async step
  "One piece of the felling: :again, :continue (a child waits on the world) or the ending."
  [c]
  (let [{:keys [species radius]} (:args c)
        walked (when (and (not (:column (ctx/mem c))) (not (first (candidates c radius species))))
                 (or (await (walk-to-at! c)) (await (look/survey! c :cell (look/glance-effort) {:match #(or (log-name? %) (trees/leaves-name? %)) :radius radius}))))
        _ (when (and (not (:pillar (ctx/mem c))) (log/open-scaffold? c))
            (ctx/update-mem! c assoc :pillar {}))
        fresh? (not (:column (ctx/mem c)))
        chosen (or (:column (ctx/mem c)) (await (choose-tree! c radius species)))]
    (cond
      (= :partial walked) :continue

      (= :searching chosen) :again

      (and (not chosen) (:at (:args c)) (not (:at-looked (ctx/mem c))) (not (u/seen-name (:primitives c) (:at (:args c)))))
      (do (ctx/update-mem! c assoc :at-looked true) :again)

      (and (not chosen) (seq (:unreachable (ctx/mem c))))
      (let [why (:unreachable-why (ctx/mem c))
            data (cond-> {:unreachable (count (:unreachable (ctx/mem c))) :columns (vec (:unreachable (ctx/mem c)))} why (assoc :why why))]
        (do (doseq [col (:columns data)] (ctx/remember! c trees/unreachable-kind {:column col} trees/unreachable-policy))
            (ctx/end c :nothing-possible :tree-unreachable data)))

      (not chosen) (ctx/end c :condition-failed (no-tree-cause c) nil)

      ;; the tree is chosen: the round felling it runs as one section
      fresh? :again

      (:pillar (ctx/mem c)) (await (pillar/round! c))

      :else (await (ground-step c radius)))))

(defn ^:async round
  "Steps until a tree is chosen, then that tree as one comfort section (ctx/atomic :tree): a full bag or
  tidying waits for the tree, up to its cap. A tree given up ends its section before the next is chosen."
  [c]
  (let [step! #(step c)
        chosen? #(some? (:column (ctx/mem c)))
        steps-while (fn [want] (pace/steps! c (fn ^:async part []
                                                (let [r (await (step!))]
                                                  (if (and (= :again r) (not= want (chosen?))) :switch r)))))]
    (loop []
      (let [r (if (chosen?)
                (await (ctx/atomic c :comfort :tree #(steps-while true)))
                (await (steps-while false)))]
        (if (= :switch r) (recur) r)))))

;; ------------------------------------------------------------------ the contract

(defn ^:async cleanup
  "However the call ends: take the pillar back."
  [c e]
  (await (pillar/cleanup c e)))

(def target [:at])

(def defaults {:spend {:job-time ::job-time-s}})

(defn place
  "Where the job goes: the cell of :at, else the base of the tree it has chosen or would choose first, nil when none."
  [c]
  (or (a/cell (:at (:args c)))
      (:base (ctx/mem c))
      (:base (first (candidates c (:radius (:args c)) (:species (:args c)))))))

(def safe? (ready/safe-at-or-if-lit place))

(def fit? "ready/fit-to-forage: a body only food heals felling wood (a rod, a hoe) is not held for health." ready/fit-to-forage)

(defn tree-missing
  "The cause :no-tree while no tree is chosen or marked unreachable, none is in sight and no unseen :at is to be walked
  to."
  [c]
  (let [m (ctx/mem c)
        {:keys [radius species at]} (:args c)]
    (when-not (or (:column m) (seq (:unreachable m)) (first (candidates c radius species))
                  (and at (not (u/seen-name (:primitives c) at))))
      (no-tree-cause c))))

(def no-tree
  "Later :no-tree as tree-missing; a look first while the body has not looked round within a few blocks of here. With
  :at the cell is seen (tree-missing says so): no log there, never."
  (checks/refusing #(cond (:at (:args %)) :never (look/surveyed? % :near) :later :else :look)
                   :tree-found "a tree to fell" tree-missing))

(def can-start? no-tree)

(defn done?
  "Ok once the chosen tree is down: logs were dug, every log last seen of it is seen gone, none of its column is left and the body is off the canopy, no child
  under way; else later with the logs dug."
  [c]
  (let [m (ctx/mem c)
        dug (:dug m 0)
        data [:units :trees :done (if (pos? dug) 1 0) :left (if (pos? dug) 0 1) :result (tree-result c)]
        gone? (fn [pos] (let [n (u/seen-name (:primitives c) pos)] (and n (not (log-name? n)))))
        down? (and (:column m) (pos? dug) (not (:pillar m))
                   (or (empty? (:drop-ids m)) (some? (:collected m)))
                   (every? gone? (some-> (:logs-seen m) (conj (update (apply max-key :y (:logs-seen m)) :y inc))))
                   (empty? (tree-logs c (:radius (:args c))))
                   (not (on-canopy? c))
                   (not (checks/child-under-way? c)))]
    (if down?
      (apply contract/ok :felled (str "fell-tree: the tree is down, " dug " logs dug") data)
      (apply contract/later :felling (str "fell-tree: " dug " logs dug") data))))

(defn ^:async quote-of
  "The walk to the tree's base as go-to quotes it, then 3 s and 1.5 s a log, and the wear of the axe on the logs. The
  logs are picked up: one log item per log of the tree."
  [c]
  (let [{:keys [species radius at]} (:args c)
        t (if at (tree-at (:primitives c) at) (tree-near (:primitives c) radius species))
        n (if t (count (log/tree-logs c radius t)) 0)
        [wl wm] (or (when t (get-in (await (ctx/quote c 'jobs.movement.go-to {:pos (walk-target (:primitives c) t) :range 2})) [:uses :job-time])) [0 0])
        least (+ 2 wl)
        mean (+ 2 wm 3 (* 1.5 n))]
    (cond-> {:uses {:job-time [least mean (max mean (time-s))]} :done [0 1 1]}
      (pos? n) (-> (assoc-in [:uses :wear] (choose/dig-wear c (repeat n (str (:species t) "_log"))))
                   (assoc :yields {:items {(str (:species t) "_log") [0 n n]}})))))
