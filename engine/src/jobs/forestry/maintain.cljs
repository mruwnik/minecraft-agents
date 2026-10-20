(ns jobs.forestry.maintain
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.ready :as ready]
            [engine.args :as a]
            [jobs.lib.access.rules :as rules]
            [engine.contract :as contract]
            [jobs.lib.cond :as cond]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.checks :as checks]
            [jobs.lib.access :as access]
            [jobs.lib.trees :as forestry]
            [jobs.lib.forestry :as lib :refer [cell-vec down feet-cell field-cells soil-for survey-field! trouble-status]]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.step-off :as step-off]))

(def doc
  "Keep the trees a forest plan wants (cells wanting {:tree species}; a large tree is four cells).
  One call plants bare planned cells and fells grown trees (jobs.forestry.fell-tree, plant-sapling, collect-drops as
  children) until none is left (:continue only while a child waits on the world). A cell is noted as owing a sapling
  before its base log is dug, so a restart or plan edit never loses the replant.
  With a planned cell never seen, the body looks around once from where it stands (again after moving over 3 blocks)
  before the start conditions decide, and the first round looks once more before it works.
  Cells are read from the plan and the world at every step. A sapling of the wanted species is left to grow.
  Anything else on a planned cell (another species, a block) is left and reported once (forest.foreign). Trees off
  the planned cells are never touched.
  A bare cell whose sapling is not carried gets it from jobs.forestry.plant-sapling (a child that fetches it: a seen
  chest, or the leaves of its tree). With :fetch off for items it is skipped, with one forest.no-sapling note.
  A tree is left standing (forest.left, once, and skipped for the rest of the call; the next call looks again) when:
  - its column holds more than ::max-logs (6) logs and :pillar? is false (else it is felled from a pillar),
  - fell-tree gave up on it (its cause is the :because of forest.left),
  - a log of it may not be dug (another plan's footprint or a zone; checked for the whole column before the first
    dig and again before every felling round), or
  - planting it was refused or failed three times.
  Does not start (condition-failed, a skipped job) unless a grown tree stands on a planned cell that is not left, or a
  bare planned cell has its sapling carried: the cause is the worst blocked state ({:reason :cells-foreign
  :cells-unplantable :no-sapling :cells [..]}) or :nothing-to-do. {:reason :plan-trouble :why text}
  a wait while the plan is missing or unreadable or no zone list is loaded; impossible when it
  has no tree cells, also later in the call.
  Ends :done with data {:felled :planted :left :bare} when it felled or planted a tree. With none of
  those it ends nothing-possible :left when it left trees standing, else condition-failed :nothing-to-do.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world; its tree cells are the forest" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :pillar? {:doc "fell a tree too tall for the ground from a pillar (jobs.forestry.fell-tree); false: leave one over ::max-logs" :spec boolean? :default true}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :lava-adjacent :falling-block"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}
   :fetch {:doc "get a missing sapling (passed to plant-sapling; see its :fetch): true, a set of kinds or a map of limits; false skips a bare cell whose sapling is not carried" :spec need/option? :default true}
   :ignore-zones? :shared})

(defn fetching?
  "Whether :fetch lets the job get a missing sapling."
  [c]
  (boolean (some-> (need/opts c 'jobs.forestry.plant-sapling) :what (contains? :item))))

;; ------------------------------------------------------------------ access

(defn access-input
  "The rules' input for a cell as the body is now."
  [c pos]
  (let [p (:primitives c)]
    (merge {:block-at (fn [[x y z]] (u/seen-name p {:x x :y y :z z}))
            :cell (cell-vec pos)
            :feet (feet-cell c)
            :ledger #{}}
           (access/zone-input c {:except (:plan (:args c)) :ignore-zones? (:ignore-zones? (:args c))}))))

(defn refusal
  "The reason the body may not dig the log at pos (a hazard it does not accept, or a refusal), nil when it may. A log
  under the feet is no refusal: the blocks.dig child digs it from beside."
  [c pos]
  (let [v (rules/may-dig? (access-input c pos))
        accept (conj (set (:accept (:args c))) :under-feet)]
    (when-not (and (:ok v) (every? (comp accept access/hazard-key) (:hazards v)))
      (or (:reason v) (access/hazard-key (first (remove (comp accept access/hazard-key) (:hazards v))))))))

(defn place-refusal [c pos]
  (let [v (rules/may-place? (access-input c pos))]
    (when-not (:ok v) (:reason v))))

;; ------------------------------------------------------------------ pure helpers

(defn up [pos] (update pos :y inc))
(defn classify
  "What stands on the planned cell pos for species: :ripe (its log), :growing (its sapling), :bare (air over its
  soil, nothing growing below a log), :no-ground (air, but no soil below), :cramped (air over soil with a log
  above), :unloaded, or :foreign (anything else)."
  [p pos species]
  (let [here (u/seen-name p pos)
        below (u/seen-name p (down pos))]
    (cond
      (nil? here) :unloaded
      (= here (str species "_log")) :ripe
      (= here (forestry/sapling-of species)) :growing
      (not (bs/air here)) :foreign
      (nil? below) :unloaded
      (not ((soil-for species) below)) :no-ground
      (some-> (u/seen-name p (up pos)) forestry/log-name?) :cramped
      :else :bare)))

(a/defargs settings
  {::max-logs {:default 6 :doc "With :pillar? false, a tree whose column holds more logs than this is too tall to fell from the ground and is left." :spec (a/int-in 1 nil)}
   ::collect-radius {:default 8 :doc "How far from where the body stands the drops of a felled tree are collected, in blocks." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 1800 :doc "Seconds a maintain run may take: the fellings, collecting, plantings and walks." :spec (a/num-in 0 nil)}})

;; ------------------------------------------------------------------ left cells

(defn left-cells
  "The cells to skip: those left in this call's memory."
  [c]
  (set (keys (:left (ctx/mem c)))))

(defn leave!
  "Skip the cell pos for the rest of the call and say why, once."
  [c pos reason & {:as more}]
  (ctx/update-mem! c assoc-in [:left pos] reason)
  (ctx/warn-once! c [:left pos reason] :forest.left
                  (merge {:pos pos :reason reason}
                         more
                         {:text (str "forest leaves the tree at " (pr-str (cell-vec pos)) " standing: " (name reason)
                                     (when-let [why (:why more)] (str " (" (name why) ")"))
                                     (when-let [because (:because more)] (str " (" (checks/cause-text because) ")")))})))

;; ------------------------------------------------------------------ the plan

(defn planned [c] (lib/planned c))

(defn note-cells!
  "Say, once each, which planned cells hold something foreign, cannot take a sapling, or want a sapling not carried."
  [c classes]
  (let [p (:primitives c)
        have (if (fetching? c) (constantly true) (u/carried-names p))]
    (doseq [[pos [kind species]] classes]
      (case kind
        :foreign (let [found (u/seen-name p pos)]
                   (ctx/warn-once! c [:foreign pos found] :forest.foreign
                                   {:pos pos :found found :wanted species
                                    :text (str "forest leaves " found " at " (pr-str (cell-vec pos)) " (the plan wants a " species " tree)")}))
        (:no-ground :cramped) (ctx/warn-once! c [:unplantable pos kind] :forest.unplantable
                                              {:pos pos :why kind :wanted species
                                               :text (str "forest cannot plant " species " at " (pr-str (cell-vec pos)) ": " (name kind))})
        nil))
    (let [missing (->> classes
                       (filter (fn [[_ [kind species]]] (and (= :bare kind) (not (have (forestry/sapling-of species))))))
                       (group-by (comp second val)))]
      (doseq [[species cells] missing]
        (ctx/warn-once! c [:no-sapling species] :forest.no-sapling
                        {:species species :cells (mapv key cells)
                         :text (str "forest has no " (forestry/sapling-of species) " for " (count cells) " bare planned cells")})))))

(defn classes-of
  "{pos [kind species]} of every planned cell."
  [c trees]
  (into {} (map (fn [[pos species]] [pos [(classify (:primitives c) pos species) species]])) trees))

(defn ripe-cells
  "The planned cells with a grown tree not left, nearest to the body first."
  [c classes]
  (let [left (left-cells c)
        here (u/self-pos c)]
    (->> classes
         (filter (fn [[pos [kind _]]] (and (= :ripe kind) (not (left pos)))))
         (map key)
         (sort-by #(u/dist here %)))))

(defn owed-cells
  "[{:pos :species}] cells to plant: the planned cells standing bare and those this job cut (its :replant debts)
  that are bare now, not left, nearest to the body first."
  [c classes]
  (let [p (:primitives c)
        left (left-cells c)
        here (u/self-pos c)
        planned (keep (fn [[pos [kind species]]] (when (= :bare kind) {:pos pos :species species})) classes)
        cut (keep (fn [{:keys [pos species]}]
                    (when (and (not (contains? classes pos)) (= :bare (classify p pos species)))
                      {:pos pos :species species}))
                  (:replant (ctx/mem c)))]
    (->> (concat planned cut)
         (remove #(left (:pos %)))
         (sort-by #(u/dist here (:pos %))))))

(defn plantable
  "The owed cells whose sapling is carried or can be fetched."
  [c owed]
  (if (fetching? c)
    (vec owed)
    (let [have (u/carried-names (:primitives c))]
      (filterv #(have (forestry/sapling-of (:species %))) owed))))

;; ------------------------------------------------------------------ check

(defn blocked-reason
  "Why no planned cell can be worked, as a wait reason: cells holding something foreign, cells that cannot take a
  sapling, or bare cells whose sapling is not carried; nil when none."
  [c classes]
  (let [kinds (group-by (comp first val) classes)
        cells (fn [& ks] (vec (sort (map (comp cell-vec key) (mapcat kinds ks)))))
        have (if (fetching? c) (constantly true) (u/carried-names (:primitives c)))
        short (filter (fn [[_ [_ species]]] (not (have (forestry/sapling-of species)))) (:bare kinds))]
    (cond
      (seq (:foreign kinds)) {:reason :cells-foreign :cells (cells :foreign)}
      (or (seq (:no-ground kinds)) (seq (:cramped kinds))) {:reason :cells-unplantable :cells (cells :no-ground :cramped)}
      (seq short) {:reason :no-sapling :cells (vec (sort (map (comp cell-vec key) short)))})))

(defn not-ready
  "Whether the job may start: ok while a grown tree is ripe on a planned cell or a bare one has its sapling carried; else
  later with the cells that hold it back, a look first while one was never seen. The cells are read once per
  check-cache window; the round says (warns) what is wrong with each."
  [c]
  (let [field (planned c)]
    (if (:trouble field)
      (checks/cause-status :later {:reason :plan-trouble :why (:trouble field)})
      (let [cells (field-cells field)
            unseen? (look/unseen? (:primitives c) cells)
            read #(classes-of c (:trees field))
            classes (if unseen? (read) (check-cache/cached c :classes read))]
        (if (or (seq (ripe-cells c classes))
                (seq (plantable c (owed-cells c classes))))
          (contract/ok :work "a tree is ripe or a sapling can be planted")
          (checks/cause-status (if (and unseen? (not (look/surveyed? c :near))) :look :later)
                               (or (blocked-reason c classes) {:reason :nothing-to-do})))))))

;; ------------------------------------------------------------------ steps

(defn bump [m k] (update m k (fnil inc 0)))

(defn ^:async collect!
  "Step 1: one collect-drops call while a sweep is owed. :again, :continue while it waits."
  [c]
  (when-let [species (:collect (ctx/mem c))]
    (let [r (await (ctx/call c :collect 'jobs.items.collect-drops
                                   {:radius (settings/get settings ::collect-radius)
                                    :items (vec (distinct (mapcat forestry/drop-filter species)))
                                    :holes (:holes (ctx/mem c))}))]
      (when (not= :continue r) (ctx/update-mem! c dissoc :collect))
      (if (= :continue r) :continue :again))))

(defn fell-slot [pos] (keyword (str "fell-" (:x pos) "-" (:y pos) "-" (:z pos))))

(defn remaining-logs
  "The logs still standing in the column of the tree begun at pos."
  [c pos species]
  (forestry/logs-at (:primitives c) pos species))

(defn column-refusal
  "[log-pos reason] of the first log of the column that may not be dug, nil when all may."
  [c logs]
  (some #(when-let [r (refusal c (:pos %))] [(:pos %) r]) logs))

(defn too-tall? [c logs] (and (not (:pillar? (:args c))) (> (count logs) (settings/get settings ::max-logs))))

(defn finished-felling!
  "The fell child ended with e: count the tree felled, or leave it (e's cause the :because) when logs of it still stand;
  collect either way."
  [c pos species e]
  (let [logs (remaining-logs c pos species)]
    (ctx/update-mem! c (fn [m] (-> m (dissoc :cut) (update :collect (fnil conj []) species) (update :holes (fnil conj []) pos))))
    (if (seq logs)
      (if-let [bad (column-refusal c logs)]
        (leave! c pos :refused :why (second bad) :log (first bad))
        (leave! c pos :unreachable :because (let [cause (:cause e)] (cond (map? cause) cause (keyword? cause) {:reason cause}))))
      (ctx/update-mem! c bump :felled))))

(defn fell-args
  "The args of the fell-tree child for the tree of species at pos."
  [c pos species]
  {:at pos :species species :radius 16 :for-plan (:plan (:args c))
   :accept (:accept (:args c)) :pillar? (:pillar? (:args c)) :ignore-zones? (:ignore-zones? (:args c))})

(defn ^:async fell!
  "Steps 2 and 4: go on with the tree begun, else begin the nearest ripe one. :again, :continue while the fell child
  waits, or nil with no tree."
  [c trees classes]
  (let [m (ctx/mem c)
        {:keys [pos species] :as cut} (:cut m)]
    (cond
      cut
      (let [logs (remaining-logs c pos species)
            bad (column-refusal c logs)]
        (cond
          (not= species (get trees pos)) (do (ctx/update-mem! c dissoc :cut) :again)
          bad (do (ctx/update-mem! c dissoc :cut)
                  (leave! c pos :refused :why (second bad) :log (first bad))
                  :again)
          :else
          (let [r (await (ctx/call c (fell-slot pos) 'jobs.forestry.fell-tree (fell-args c pos species)))]
            (when (not= :continue r) (finished-felling! c pos species r))
            (if (= :continue r) :continue :again))))

      :else
      (when-let [pos (first (ripe-cells c classes))]
        (let [species (get trees pos)
              logs (remaining-logs c pos species)
              bad (column-refusal c logs)]
          (cond
            (too-tall? c logs) (leave! c pos :too-tall :logs (count logs))
            bad (leave! c pos :refused :why (second bad) :log (first bad))
            :else (ctx/update-mem! c (fn [m] (-> m
                                                (assoc :cut {:pos pos :species species})
                                                (update :replant (fnil conj []) {:pos pos :species species}))))))
        :again))))

(defn fail-plant!
  "Count a failed attempt to plant at pos; leave the cell at the third."
  [c pos why & {:as more}]
  (let [n (inc (get-in (ctx/mem c) [:fails pos] 0))]
    (ctx/update-mem! c assoc-in [:fails pos] n)
    (when (>= n (u/max-failures))
      (apply leave! c pos why (mapcat identity more)))))

(defn settle-debts!
  "Drop the replant debts whose cell no longer wants a sapling: one now holding a sapling or something foreign, or
  with no soil. A cell still holding the tree's log (being felled, or left) keeps its debt."
  [c]
  (let [p (:primitives c)
        debts (:replant (ctx/mem c))
        open (filterv #(not (#{:growing :foreign :no-ground} (classify p (:pos %) (:species %)))) debts)]
    (when (not= (count debts) (count open))
      (ctx/update-mem! c assoc :replant open))))

(defn body-cells
  "The cells the body occupies: feet and head."
  [c]
  (let [[x y z] (feet-cell c)]
    #{[x y z] [x (inc y) z]}))

(defn ^:async step-off!
  "Walk off pos, which the body stands on, so a sapling can go there (jobs.lib.step-off). :again."
  [c pos]
  (let [r (await (step-off/step-off-zoned! c pos {}))]
    (when (:unreachable r)
      (fail-plant! c pos :unreachable))
    :again))

(defn plant-args
  "The args of the plant-sapling child for the cell at pos."
  [c pos species]
  (cond-> {:at pos :species species :for-plan (:plan (:args c))
           :ignore-zones? (boolean (:ignore-zones? (:args c)))}
    (some? (:fetch (:args c))) (assoc :fetch (:fetch (:args c)))))

(defn ^:async fetch-plant!
  "Plant at pos with jobs.forestry.plant-sapling, which fetches the sapling it lacks. :again, :continue while it waits."
  [c pos species]
  (let [e (await (ctx/call c :plant 'jobs.forestry.plant-sapling (plant-args c pos species)))]
    (cond
      (= :continue e) :continue
      (= (forestry/sapling-of species) (u/seen-name (:primitives c) pos))
      (do (ctx/update-mem! c bump :planted)
          (ctx/forget-where! c forestry/replant-kind #(= pos (:pos %)))
          :again)
      :else (do (fail-plant! c pos :no-sapling :because (let [cause (:cause e)] (cond (map? cause) cause (keyword? cause) {:reason cause}))) :again))))

(defn ^:async plant!
  "Step 3: plant at the nearest owed cell, those with the sapling carried first and the cells under the body last
  (walking off one when it is the only one), with a jobs.forestry.plant-sapling child. :again, :continue while the
  child waits, or nil."
  [c classes]
  (let [under (body-cells c)
        have (u/carried-names (:primitives c))
        owed (sort-by #(vector (not (have (forestry/sapling-of (:species %)))) (contains? under (cell-vec (:pos %))))
                      (plantable c (owed-cells c classes)))]
    (when-let [{:keys [pos species]} (first owed)]
      (cond
        (contains? under (cell-vec pos)) (await (step-off! c pos))
        (place-refusal c pos) (do (leave! c pos :refused :why (place-refusal c pos)) :again)
        :else (await (fetch-plant! c pos species))))))

(defn finish!
  "Step 5: say what was done and end: done when it felled or planted, else nothing-possible :left or condition-failed."
  [c classes]
  (let [m (ctx/mem c)
        bare (vec (keep (fn [[pos [kind _]]] (when (= :bare kind) pos)) classes))
        left (vec (sort-by (juxt :x :y :z) (left-cells c)))
        result {:felled (:felled m 0) :planted (:planted m 0) :left left :bare bare}
        done-text (str "forest done: felled " (:felled m 0) ", planted " (:planted m 0)
                       ", left " (count left) ", bare " (count bare))]
    (cond
      (pos? (+ (:felled result) (:planted result))) (ctx/end c :done nil (assoc result :text done-text))
      (seq left) (ctx/end c :nothing-possible :left result)
      :else (ctx/end c :condition-failed (or (blocked-reason c classes) {:reason :nothing-to-do}) result))))

(defn ^:async step [c]
  (let [field (planned c)]
    (if (:trouble field)
      :continue
      (let [trees (:trees field)]
        (or (await (survey-field! c field))
            (do (when-not (:begun (ctx/mem c))
                  (ctx/update-mem! c assoc :begun true))
                (settle-debts! c)
                (let [classes (classes-of c trees)]
                  (note-cells! c classes)
                  (or (await (collect! c))
                      (when (:cut (ctx/mem c)) (await (fell! c trees classes)))
                      (await (plant! c classes))
                      (await (fell! c trees classes))
                      (finish! c classes)))))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step c))))

;; ------------------------------------------------------------------ the contract

(def target [:plan :part])

(def defaults {:spend {:job-time ::job-time-s}})

(defn plan-place "A tree cell of the plan, nil when it cannot be worked." [c] (some-> (planned c) :trees keys first))

(def safe? (ready/safe-at-by-night plan-place))

(def fit? ready/fit?)

(defn can-run? [c] (trouble-status (planned c)))

(def can-start? (cond/and can-run? not-ready))

(defn done?
  "Ok once the forest is in order: no tree to collect, fell or replant, no cell blocked or left, the field seen and no
  child under way; else later with the trees felled and planted this call."
  [c]
  (let [m (ctx/mem c)
        field (planned c)
        data [:units :trees :done (+ (:felled m 0) (:planted m 0)) :result {:felled (:felled m 0) :planted (:planted m 0) :left (vec (sort-by (juxt :x :y :z) (left-cells c))) :bare []}]
        read #(classes-of c (:trees field))
        classes (when-not (:trouble field)
                  (if (or (:begun m) (look/unseen? (:primitives c) (field-cells field)))
                    (read)
                    (check-cache/cached c :classes read)))
        waiting (cond
                  (:trouble field) :plan-trouble
                  (and (not (:begun m)) (look/unseen? (:primitives c) (field-cells field))) :looking
                  (or (:collect m) (:cut m)) :working
                  (seq (ripe-cells c classes)) :ripe
                  (seq (plantable c (owed-cells c classes))) :planting
                  (blocked-reason c classes) :blocked
                  (seq (left-cells c)) :left
                  (checks/child-under-way? c) :wrapping-up)]
    (if waiting
      (apply contract/later waiting (str "forest: felled " (:felled m 0) ", planted " (:planted m 0)) data)
      (apply contract/ok :in-order "forest: no tree to fell or plant" data))))

(defn quote-of [_c]
  {:uses {:job-time [1 60 (settings/get settings ::job-time-s)]}})
