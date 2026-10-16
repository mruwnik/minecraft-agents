(ns jobs.forestry.prepare
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [jobs.lib.cond :as cond]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.trees :as forestry]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.checks :as checks]
            [jobs.lib.look :as look]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.step-off :as step-off]
            [jobs.forestry.maintain :as maintain]
            [jobs.forestry.prepare-field :as field]
            [jobs.forestry.prepare-rule :as rule]))

(def doc
  "Get the planting spots of a forest plan ready. A spot is a planned tree cell (want {:tree species}); the cell
  is where the sapling goes. The goal per cell: a sapling of the planned species can be planted and can grow there.
  One call takes steps on the nearest cell that can still be improved, until none is left:
  1. Dig the stray in the cell (grass, flowers, snow, leaves, stone: anything but air, the species' own sapling or
     log, or a thing never dug).
  2. Replace natural ground under the cell (stone, sand, gravel, cobblestone, deepslate ...) with carried dirt. It
     digs, then places. A hole this job dug is filled first, also after a restart.
  3. Plant the sapling when it is carried (jobs.forestry.plant-sapling as a child).
  Drops are collected (jobs.forestry.collect-drops) when the next step is on another cell or none is left.
  Bringing saplings and dirt is another job, e.g. (seq (jobs.storage.withdraw ...) (jobs.forestry.prepare ...)).
  Water in the cell is repaired. A source in the cell is filled with carried dirt (the dirt is then dug as a
  stray). A source beside it on its level is dammed first, since it would flood the dug cell back. A flow is
  traced upstream (at most 16 cells) and its source filled. The cell then gets 10 s to recede (the call yields
  :continue, then the check waits :receding with :ready-at; nothing is polled). Water that cannot be traced, no dirt carried, or a refused source leaves the
  cell as :wet {:pos :why :source} (warn prepare.wet).
  Never dug, only reported:
  - a stray or natural ground that no carried tool harvests (stone with no pickaxe: slow, nothing drops): result
    :no-tool {:pos :block}, warn prepare.no-tool. The cell is taken up when a tool is carried.
  - in the cell: another species' sapling, any log but the species' own, lava, a container, bed, sign or
    light (result :wrong).
  - under the cell: anything but natural ground (planks, logs, wool, a chest, air, fluid, a cell the plan names)
    gives :no-soil with :why :other-block, :kept, :hollow, :fluid or :planned. Natural ground with no dirt
    carried gives :no-soil :no-dirt.
  - above the cell: nothing is dug. A block in the growth space that a tree cannot grow through (anything but
    air, leaves, saplings, plants, snow, vines) leaves the cell untouched and reported as :cramped
    {:pos :at :block}.
  The growth space is HEADROOM cells from the planned cell up, the cell included: oak 7, birch 8, spruce 9,
  jungle 13, acacia 10, dark_oak 10, pale_oak 10, cherry 9. These are approximate (about 70% sure of each).
  Light level and the neighbours of a 2x2 tree are not checked. :headroom {species n} overrides. A species not in
  the table is refused as :unsupported-species. The species' own sapling is left growing, its own log is
  maintain's business.
  Every dig and place is checked against zones and the footprints of all other active plans, when the cell is
  chosen and again before the act. A refusal, a dig hazard not in :accept, or three failed tries (walk, dig,
  place, plant) skips the cell for 10 minutes (body memory :forestry/prepare-skip). It is reported as :refused
  {:pos :reason} (warn prepare.refused).
  Condition-failed (it does not start) unless a step can be taken. A field where every cell is done, wrong, cramped, without soil or
  short of saplings is left alone, with one note per cell (prepare.wrong, prepare.no-soil, prepare.cramped,
  prepare.wet) and one per species short (prepare.short {:species :missing}). The job wakes when what it lacked
  is carried. A started job runs on to finish.
  {:reason :plan-trouble :why text} (one prepare.declined warn): a wait while the plan is missing or unreadable or no zone
  list is loaded; impossible when it has no tree cells (in :part), also later in the call. A cell held back by cells never seen
  (the cell, the ground under it, its growth space) starts the job: it looks around once, then at each unseen cell.
  Ends :done with data {:cleared :soiled :planted :dammed :short {species missing} :wrong :no-soil :no-tool :cramped
  :wet :refused :text} when it cleared, soiled, planted or dammed a cell and no cell is left, or (done?) when every cell
  is ready, none refused, the drops collected and no child under way. Else it ends condition-failed (same data) :refused
  when cells were skipped, else with the worst blocked state, also after part of the work.
  Quote: time only.")

(a/defargs settings
  {::job-time-s {:default 1200 :doc "Seconds a prepare run may take: the digs, soil, plants and walks." :spec (a/num-in 0 nil)}
   ::max-steps {:default 400 :doc "Steps of one round before it gives the body back with :continue." :spec (a/int-in 1 nil)}})

(a/defargs args
  {:plan {:doc "id of a plan of the body's world; its tree cells are the planting spots" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :falling-block; lava beside is :lava-adjacent and never taken by default"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent}}
   :headroom {:doc "{species cells} overriding the table of growth space above a planted cell (the cell included)" :spec (a/map-of a/name? (a/int-in 1 nil)) :default {}}
   :collect-radius {:doc "how far from where the body stands the drops are collected, in blocks" :spec (a/num-in 0 nil) :default 8}
   :ignore-zones? :shared})

;; ------------------------------------------------------------------ notes

(defn note-cells!
  "Say, once each, what is wrong with a cell, and (when settled) which species are short of saplings."
  [c states settled?]
  (doseq [{:keys [pos species state found why at block source]} (sort-by (comp maintain/cell-vec :pos) (vals states))]
    (case state
      :wrong (ctx/warn-once! c [:wrong pos found] :prepare.wrong
                             {:pos pos :found found :species species
                              :text (str "prepare leaves " found " at " (pr-str (maintain/cell-vec pos)) " (the plan wants a " species " tree)")})
      :no-soil (ctx/warn-once! c [:no-soil pos why] :prepare.no-soil
                               {:pos pos :why why
                                :text (str "prepare cannot give " (pr-str (maintain/cell-vec pos)) " soil: " (name why))})
      :wet (ctx/warn-once! c [:wet pos why] :prepare.wet
                           {:pos pos :why why :water-source (some-> source vec)
                            :text (str "prepare leaves " (pr-str (maintain/cell-vec pos)) " wet: " (name why)
                                       (when source (str ", water source " (pr-str source))))})
      :no-tool (ctx/warn-once! c [:no-tool pos block] :prepare.no-tool
                               {:pos pos :block block
                                :text (str "prepare leaves " block " at " (pr-str (maintain/cell-vec pos)) ": no carried tool harvests it")})
      :cramped (ctx/warn-once! c [:cramped pos at] :prepare.cramped
                               {:pos pos :at at :block block
                                :text (str "prepare leaves " (pr-str (maintain/cell-vec pos)) ": " block " at " (pr-str at) " blocks the growth space")})
      nil))
  (when settled?
    (doseq [[species cells] (group-by :species (filter #(= :short (:state %)) (vals states)))]
      (ctx/warn-once! c [:short species (count cells)] :prepare.short
                      {:species species :missing (count cells)
                       :text (str "prepare has no " (forestry/sapling-of species) " for " (count cells) " cells")}))))

;; ------------------------------------------------------------------ check

(defn receding? [states] (boolean (some #(= :receding (:state %)) (vals states))))

(def blocked-states
  "The cell states that hold a cell back, worst first, with the wait reason each gives."
  [[:wrong :cells-wrong] [:no-soil :cells-no-soil] [:wet :cells-wet] [:no-tool :cells-no-tool] [:cramped :cells-cramped]
   [:short :cells-short] [:unloaded :cells-unloaded] [:unsupported :cells-unsupported]])

(defn blocked-reason
  "Why no cell can be worked, as a wait reason naming the worst state and its cells; nil when none is held back."
  [states]
  (let [by (group-by :state (vals states))]
    (some (fn [[state reason]]
            (when-let [cells (seq (get by state))]
              {:reason reason :cells (vec (sort (map (comp maintain/cell-vec :pos) cells)))}))
          blocked-states)))

(defn unseen-room
  "The cells of the :unloaded cells' ground and growth space that the body never saw (a look shows them; an unloaded
  chunk's stay unseen), as maps for the :look-at of a :look status."
  [c states]
  (let [p (:primitives c)
        over (:headroom (:args c))]
    (vec (distinct
          (for [{:keys [state pos species]} (vals states)
                :when (= :unloaded state)
                cell (cons (maintain/down pos)
                           (map #(update pos :y + %) (range (or (rule/headroom-of species over) 1))))
                :when (look/unseen? p [cell])]
            ((juxt :x :y :z) cell))))))

(defn states-of
  "The cell states of the field: read afresh while a cell or the room around one may not have been seen (a state :unloaded), else once
  per check-cache window."
  [c field cells]
  (let [read #(field/assessments c field)]
    (if (look/unseen? (:primitives c) cells)
      (read)
      (let [states (check-cache/cached c :states read)]
        (if (some #(= :unloaded (:state %)) (vals states)) (read) states)))))

(defn not-ready
  "Whether the job may start: ok while a step can be taken on a planned cell or a look is asked for cells never seen. The field
  is assessed once per check-cache window; the round says (warns) what is wrong with each cell."
  [c]
  (let [field (field/planned c)]
    (if (:trouble field)
      (checks/cause-status :later {:reason :plan-trouble :why (:trouble field)})
      (let [states (states-of c field (maintain/field-cells field))]
        (cond
          (seq (field/todo c states)) (contract/ok :work "a step can be taken on a planned cell")
          (seq (unseen-room c states)) (contract/look :unseen-room "cells never seen are looked at" :look-at (unseen-room c states))
          (seq (:recede (ctx/mem c))) (checks/cause-status :later {:reason :receding :ready-at (apply min (vals (:recede (ctx/mem c))))})
          :else (checks/cause-status :later (or (blocked-reason states) {:reason :nothing-to-do})))))))

;; ------------------------------------------------------------------ steps

(defn skip!
  "Leave the cell for a while and say why, once."
  [c pos reason]
  (ctx/remember! c field/skip-kind {:pos pos :reason reason} field/skip-policy)
  (ctx/warn-once! c [:refused pos reason] :prepare.refused
                  {:pos pos :reason reason
                   :text (str "prepare leaves " (pr-str (maintain/cell-vec pos)) " alone: " (name reason))}))

(defn count-cell-fail!
  "Count a failed try on the cell; skipped at the third. :again."
  [c pos reason]
  (let [n (inc (get-in (ctx/mem c) [:fails pos] 0))]
    (ctx/update-mem! c assoc-in [:fails pos] n)
    (when (>= n (u/max-failures))
      (skip! c pos reason))
    :again))

(defn blocked!
  "Book a verdict that is not :ok against the planned cell: a refusal skips it, a wait counts a failure. :again."
  [c cell v]
  (if (vector? v)
    (skip! c cell (second v))
    (count-cell-fail! c cell :not-loaded))
  :again)

(defn bump [m k] (update m k (fnil inc 0)))

(defn ^:async step-off!
  "Walk off the column of pos, which the body stands in (jobs.lib.step-off). :again."
  [c pos]
  (let [r (await (step-off/step-off-zoned! c pos {}))]
    (when (:unreachable r)
      (count-cell-fail! c pos :unreachable))
    :again))

(defn ^:async ready!
  "Walk within reach of target; with column? also off the column of the planned cell. nil when ready to act, else
  :again."
  [c cell target column?]
  (let [w (await (near/go-near! c target 3 {:zone-tolls true}))]
    (cond
      (= :partial w) :continue
      (= :blocked w) (count-cell-fail! c cell :unreachable)
      (and column? (field/in-column? c cell)) (await (step-off! c cell))
      :else nil)))

(defn ^:async dig!
  "Dig the block at target (a cell the planned cell owes work on) with a blocks.dig child. on-dug is called with c when it went."
  [c cell target column? on-dug]
  (let [v (field/dig-verdict c (maintain/cell-vec target))]
    (if (vector? v)
      (blocked! c cell v)
      (or (await (ready! c cell target column?))
          (let [v (field/dig-verdict c (maintain/cell-vec target))]
            (if (not= :ok v)
              (blocked! c cell v)
              (let [outcome (await (blocks/dig-cell! c target {:accept (set (:accept (:args c)))
                                                               :for-plan (:plan (:args c))
                                                               :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
                (case outcome
                  :continue :continue
                  :dug (do (on-dug c) :again)
                  :missing :again
                  :cannot (do (skip! c cell :cannot) :again)
                  :unreachable (do (count-cell-fail! c cell :unreachable) :again)
                  (do (count-cell-fail! c cell :failed) :again)))))))))

(defn dug-stray [pos]
  (fn [c] (ctx/update-mem! c #(-> % (bump :cleared) (assoc :collect pos)))))

(defn dug-ground [pos under]
  (fn [c] (ctx/update-mem! c #(-> % (update :holes (fnil conj #{}) (maintain/cell-vec under)) (assoc :collect pos)))))

(defn ^:async soil-place!
  "Fill the hole under the planned cell with the carried soil. :again."
  [c {:keys [pos item]}]
  (let [under (maintain/down pos)
        v (field/place-verdict c (maintain/cell-vec under))]
    (if (vector? v)
      (blocked! c pos v)
      (or (await (ready! c pos under true))
          (let [v (field/place-verdict c (maintain/cell-vec under))]
            (if (not= :ok v)
              (blocked! c pos v)
              (do (await (ctx/act c :equip (clj->js {:item item})))
                  (let [outcome (await (blocks/place-cell! c under item {:for-plan (:plan (:args c))
                                                                         :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
                    (case outcome
                      :continue :continue
                      (:placed :already) (do (ctx/update-mem! c #(-> % (bump :soiled) (update :holes disj (maintain/cell-vec under))))
                                             :again)
                      (:need :no-item :occupied :no-support) (do (count-cell-fail! c pos (if (= :need outcome) :no-item outcome)) :again)
                      (do (count-cell-fail! c pos :failed) :again))))))))))

(def recede-ms "How long a flow is given to recede after its source was dammed." 10000)

(defn ^:async place-source!
  "Place the carried soil into the water of a wet cell: the source in the cell (:fill), or the source elsewhere
  (:dam, after which the cell is receding for recede-ms). :again."
  [c {:keys [pos target item state]}]
  (let [at (maintain/cell-vec target)
        fill? (= :fill state)
        v (field/place-verdict c at)]
    (if (vector? v)
      (blocked! c pos v)
      (or (when (field/in-column? c target) (await (step-off! c pos)))
          (await (ready! c pos target fill?))
          (let [v (field/place-verdict c at)]
            (if (not= :ok v)
              (blocked! c pos v)
              (do (await (ctx/act c :equip (clj->js {:item item})))
                  (let [outcome (await (blocks/place-cell! c target item {:for-plan (:plan (:args c))
                                                                          :ignore-zones? (boolean (:ignore-zones? (:args c)))}))
                        dammed (fn [m] (cond-> (bump m :dammed)
                                         (not fill?) (update :recede assoc pos (+ (ctx/now c) recede-ms))))]
                    (case outcome
                      :continue :continue
                      (:placed :already) (do (ctx/update-mem! c dammed) :again)
                      (:need :no-item :occupied :no-support) (do (count-cell-fail! c pos (if (= :need outcome) :no-item outcome)) :again)
                      (do (count-cell-fail! c pos :failed) :again))))))))))

(defn ^:async plant!
  "Plant the carried sapling in the planned cell with jobs.forestry.plant-sapling as a child. :again."
  [c {:keys [pos species item]}]
  (let [v (field/place-verdict c (maintain/cell-vec pos))]
    (if (vector? v)
      (blocked! c pos v)
      (or (await (ready! c pos pos true))
          (let [v (field/place-verdict c (maintain/cell-vec pos))]
            (if (not= :ok v)
              (blocked! c pos v)
              (let [e (await (ctx/call c :plant 'jobs.forestry.plant-sapling
                                       {:at pos :species species :for-plan (:plan (:args c))
                                        :ignore-zones? (:ignore-zones? (:args c))}))]
                (if (= :continue e)
                  :continue
                  (do (if (= item (u/seen-name (:primitives c) pos))
                        (ctx/update-mem! c bump :planted)
                        (count-cell-fail! c pos :failed))
                      :again)))))))))

(defn ^:async collect!
  "One collect-drops round while the drops of the digs are owed. :again, :continue while it waits."
  [c]
  (let [e (await (ctx/call c :collect 'jobs.forestry.collect-drops
                           {:radius (:collect-radius (:args c))
                            :holes (vec (remove nil? (cons (:collect (ctx/mem c)) (:holes (ctx/mem c)))))}))]
    (when-not (= :continue e) (ctx/update-mem! c dissoc :collect))
    (if (= :continue e) :continue :again)))

(defn ^:async act!
  "The step the cell is owed."
  [c {:keys [state pos block] :as cell}]
  (case state
    (:fill :dam) (await (place-source! c cell))
    :clear (await (dig! c pos pos false (dug-stray pos)))
    :soil-dig (let [under (maintain/down pos)]
                (await (dig! c pos under true (dug-ground pos under))))
    :soil-place (await (soil-place! c cell))
    :plant (await (plant! c cell))))

;; ------------------------------------------------------------------ result

(defn by-pos [entries] (vec (sort-by :pos entries)))

(defn state-list
  "The result entries of the cells in state, each built by (f cell), by position."
  [states state f]
  (by-pos (map f (filter #(= state (:state %)) (vals states)))))

(defn refused-list
  "The refusals: cells skipped in body memory (of this plan's cells) and cells of an unsupported species."
  [c states]
  (by-pos (concat (keep (fn [[pos data]]
                          (when (contains? states pos) {:pos (maintain/cell-vec pos) :reason (:reason data)}))
                        (field/skipped c))
                  (state-list states :unsupported (fn [{:keys [pos]}] {:pos (maintain/cell-vec pos) :reason :unsupported-species})))))

(defn result-of
  "What was done and what is left, with a :text."
  [c states]
  (let [m (ctx/mem c)
        missing (frequencies (map :species (filter #(= :short (:state %)) (vals states))))
        result {:cleared (:cleared m 0) :soiled (:soiled m 0) :planted (:planted m 0) :dammed (:dammed m 0)
                :short missing
                :wrong (state-list states :wrong (fn [{:keys [pos found species]}] {:pos (maintain/cell-vec pos) :found found :species species}))
                :no-soil (state-list states :no-soil (fn [{:keys [pos why]}] {:pos (maintain/cell-vec pos) :why why}))
                :no-tool (state-list states :no-tool (fn [{:keys [pos block]}] {:pos (maintain/cell-vec pos) :block block}))
                :cramped (state-list states :cramped (fn [{:keys [pos at block]}] {:pos (maintain/cell-vec pos) :at at :block block}))
                :wet (state-list states :wet (fn [{:keys [pos why source]}]
                                               (cond-> {:pos (maintain/cell-vec pos) :why why} source (assoc :source source))))
                :refused (refused-list c states)}]
    (assoc result :text (str "prepare done: cleared " (:cleared result) ", soiled " (:soiled result)
                             ", planted " (:planted result) ", short " (reduce + (vals missing))
                             ", left " (+ (count (:wrong result)) (count (:no-soil result)) (count (:no-tool result)) (count (:cramped result))
                                         (count (:wet result)) (count (:refused result)))))))

(defn finish!
  "Be done, saying what was done. Work done with cells left (refused, wrong, short ...) ends condition-failed with the
  reason of what is left, the counts in the data."
  [c states]
  (let [result (result-of c states)
        worked? (pos? (+ (:cleared result) (:soiled result) (:planted result) (:dammed result)))
        left (when (seq (:refused result)) :refused)
        blocked (blocked-reason states)]
    (cond
      (and worked? (not left) (not blocked)) (ctx/end c :done nil result)
      left (ctx/end c :condition-failed :refused result)
      :else (ctx/end c :condition-failed (or blocked {:reason :nothing-to-do}) result))))

(defn ^:async step [c]
  (let [field (field/planned c)]
    (if (:trouble field)
      :continue
      (or (await (maintain/survey-field! c field))
          (do
            (when-not (:begun (ctx/mem c))
              (ctx/update-mem! c assoc :begun true))
            (let [states (await (sense/decide! c ::states #(field/assessments c field)
                                               {:retry? (fn [states] (some #(= :unloaded (:state %)) (vals states)))}))
                  work (field/todo c states)
                  target (first work)
                  owed (:collect (ctx/mem c))]
              (note-cells! c states (empty? work))
              (cond
                (and owed (or (nil? target) (not= owed (:pos target)))) (await (collect! c))
                target (await (act! c target))
                (receding? states) :continue
                :else (finish! c states))))))))

(defn ^:async round [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) (settings/get settings ::max-steps)) (step c) :continue)))))

;; ------------------------------------------------------------------ the contract

(def target [:plan :part])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at-by-night (fn [c] (some-> (field/planned c) :trees keys first))))

(def fit? ready/fit?)

(defn can-run? [c] (maintain/trouble-status (field/planned c)))

(def can-start? (cond/and can-run? not-ready))

(defn done?
  "Every cell of the field is seen and in its state: nothing to work, nothing held back, refused or receding, the drops
  collected and no child under way; the cells worked and the cells still to work."
  [c]
  (let [field (field/planned c)
        m (ctx/mem c)
        worked (reduce + (map #(% m 0) [:cleared :soiled :planted :dammed]))]
    (if (:trouble field)
      (contract/later :plan-trouble (str "prepare: " (:trouble field)) :units :cells :done worked :left 0 :result {})
      (let [cells (maintain/field-cells field)
            states (states-of c field cells)
            todo (field/todo c states)
            {:keys [refused text] :as result} (result-of c states)
            data [:units :cells :done worked :left (count todo) :result (dissoc result :text)]]
        (cond
          (or (seq todo) (seq (:recede m)) (blocked-reason states) (seq refused) (look/unseen? (:primitives c) cells))
          (apply contract/later :preparing (str "prepare: " (count todo) " cells to work") data)
          (or (:collect m) (checks/child-under-way? c)) (apply contract/later :wrapping-up "prepare: every cell is ready, wrapping up" data)
          :else (apply contract/ok :prepared text data))))))

(defn quote-of [_c]
  {:uses {:job-time [1 60 (settings/get settings ::job-time-s)]}
   :done [0 1 1]})
