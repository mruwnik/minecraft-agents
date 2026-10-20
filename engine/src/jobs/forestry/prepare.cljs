(ns jobs.forestry.prepare
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [jobs.lib.waits :as waits]
            [jobs.lib.cond :as cond]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.trees :as forestry]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.child :as child]
            [jobs.lib.checks :as checks]
            [jobs.lib.look :as look]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.forestry :as lib]
            [jobs.forestry.prepare-field :as field]
            [jobs.forestry.prepare-rule :as rule]))

(def doc
  "Get the planting spots of a forest plan ready. A spot is a planned tree cell (want {:tree species}). One call works
  the nearest cell with a step left until none is, each cell by the first rule that fits:
  1. Water in the cell: a source in it is filled with carried dirt (dug as a stray after); a source beside it on its
     level is dammed first, a flow is traced upstream (jobs.forestry.prepare-rule/max-upstream) and its source filled.
     While the traced water recedes the body pauses (jobs.time.noop) and looks again. Untraceable water, no dirt or a
     refused source: :wet.
  2. A stray in the cell (anything but air, the species' own sapling or log) is dug. Not dug, only reported: another
     species' sapling, any other log, lava, a container, bed, sign or light (:wrong); a stray or ground no carried tool
     harvests (:no-tool).
  3. A block in the growth space (the species' headroom setting, cells above the planted cell) that a tree cannot grow
     through leaves the cell untouched (:cramped). A species not in the setting is :unsupported-species.
  4. Natural ground under the cell is replaced with carried dirt (a hole this job dug is filled first); other ground
     is :no-soil with :why (:other-block :kept :hollow :fluid :planned :no-dirt).
  5. The sapling is planted (jobs.forestry.plant-sapling child); none carried: :short.
  Drops are collected (jobs.items.collect-drops) when the next step is on another cell or none is left. Saplings
  and dirt are brought by another job. Every dig and place is checked against zones and the other active plans'
  footprints, when the cell is chosen and before the act. A refusal or a dig hazard not in :accept skips the cell
  for the call (:refused); the third failed try (walk, dig, place, plant) gives it up (:gave-up).
  Does not start while the plan is missing or unreadable or no zone list is loaded (a wait), or has no tree cells
  (impossible), unless a step can be taken; cells never seen are looked at first. What is left is in the ending.
  Ends :done with {:cleared :soiled :planted :dammed :short {species n} :wrong :no-soil :no-tool :cramped :wet
  :refused :gave-up :text} when it worked and no cell is left, or (done?) when every cell is ready, none refused or
  given up, the drops collected and no child under way. Cells left after part of the work: condition-failed :refused,
  nothing-possible :gave-up, else condition-failed with the worst blocked state (:cells-wrong ...).
  Quote: time only.")

(a/defargs settings
  {::collect-radius {:default 8 :doc "How far from where the body stands the drops are collected, in blocks." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 1200 :doc "Seconds a prepare run may take: the digs, soil, plants and walks." :spec (a/num-in 0 nil)}
   ::recede-wait-s {:default 2 :doc "Seconds the body pauses while a dammed flow recedes, before it looks again." :spec (a/num-in 0 nil)}})

(a/defargs args
  {:plan {:doc "id of a plan of the body's world; its tree cells are the planting spots" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :falling-block; lava beside is :lava-adjacent and never taken by default"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent}}
   :ignore-zones? :shared})

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
              {:reason reason :cells (vec (sort (map (comp lib/cell-vec :pos) cells)))}))
          blocked-states)))

(defn unseen-room
  "The cells of the :unloaded cells' ground and growth space that the body never saw (a look shows them; an unloaded
  chunk's stay unseen), as maps for the :look-at of a :look status."
  [c states]
  (let [p (:primitives c)]
    (vec (distinct
          (for [{:keys [state pos species]} (vals states)
                :when (= :unloaded state)
                cell (cons (lib/down pos)
                           (map #(update pos :y + %) (range (or (rule/headroom-of species) 1))))
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
  is assessed once per check-cache window; the ending says what is wrong with each cell."
  [c]
  (let [field (field/planned c)]
    (if (:trouble field)
      (checks/cause-status :later {:reason :plan-trouble :why (:trouble field)})
      (let [states (states-of c field (lib/field-cells field))]
        (cond
          (seq (field/todo c states)) (contract/ok :work "a step can be taken on a planned cell")
          (seq (unseen-room c states)) (contract/look :unseen-room "cells never seen are looked at" :look-at (unseen-room c states))
          (receding? states) (checks/cause-status :later {:reason :receding})
          :else (checks/cause-status :later (or (blocked-reason states) {:reason :nothing-to-do})))))))

;; ------------------------------------------------------------------ steps

(defn skip!
  "Leave the cell for the rest of this call."
  [c pos reason]
  (ctx/update-mem! c assoc-in [:skipped pos] {:pos pos :reason reason}))

(defn count-cell-fail!
  "Count a failed try on the cell; given up at the third. :again."
  [c pos reason]
  (let [n (inc (get-in (ctx/mem c) [:fails pos] 0))]
    (ctx/update-mem! c assoc-in [:fails pos] n)
    (when (>= n (u/max-failures))
      (ctx/update-mem! c assoc-in [:gave-up pos] {:pos pos :reason reason}))
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
  (let [w (await (near/go-near! c target 3 {}))]
    (cond
      (= :partial w) :continue
      (= :blocked w) (count-cell-fail! c cell :unreachable)
      (and column? (field/in-column? c cell)) (await (step-off! c cell))
      :else nil)))

(defn ^:async dig!
  "Dig the block at target (a cell the planned cell owes work on) with a blocks.dig child. on-dug is called with c when it went."
  [c cell target column? on-dug]
  (let [v (field/dig-verdict c (lib/cell-vec target))]
    (if (vector? v)
      (blocked! c cell v)
      (or (await (ready! c cell target column?))
          (let [v (field/dig-verdict c (lib/cell-vec target))]
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
  (fn [c] (ctx/update-mem! c #(-> % (update :holes (fnil conj #{}) (lib/cell-vec under)) (assoc :collect pos)))))

(defn ^:async soil-place!
  "Fill the hole under the planned cell with the carried soil. :again."
  [c {:keys [pos item]}]
  (let [under (lib/down pos)
        v (field/place-verdict c (lib/cell-vec under))]
    (if (vector? v)
      (blocked! c pos v)
      (or (await (ready! c pos under true))
          (let [v (field/place-verdict c (lib/cell-vec under))]
            (if (not= :ok v)
              (blocked! c pos v)
              (do (await (ctx/act c :equip (clj->js {:item item})))
                  (let [outcome (await (blocks/place-cell! c under item {:for-plan (:plan (:args c))
                                                                         :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
                    (case outcome
                      :continue :continue
                      (:placed :already) (do (ctx/update-mem! c #(-> % (bump :soiled) (update :holes disj (lib/cell-vec under))))
                                             :again)
                      (:need :no-item :occupied :no-support) (do (count-cell-fail! c pos (if (= :need outcome) :no-item outcome)) :again)
                      (do (count-cell-fail! c pos :failed) :again))))))))))

(defn ^:async place-source!
  "Place the carried soil into the water of a wet cell: the source in the cell (:fill), or the source elsewhere
  (:dam, after which the cell is receding until its water is gone). :again."
  [c {:keys [pos target item state]}]
  (let [at (lib/cell-vec target)
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
                                         (not fill?) (update :recede (fnil conj #{}) pos)))]
                    (case outcome
                      :continue :continue
                      (:placed :already) (do (ctx/update-mem! c dammed) :again)
                      (:need :no-item :occupied :no-support) (do (count-cell-fail! c pos (if (= :need outcome) :no-item outcome)) :again)
                      (do (count-cell-fail! c pos :failed) :again))))))))))

(defn ^:async plant!
  "Plant the carried sapling in the planned cell with jobs.forestry.plant-sapling as a child. :again."
  [c {:keys [pos species item]}]
  (let [v (field/place-verdict c (lib/cell-vec pos))]
    (if (vector? v)
      (blocked! c pos v)
      (or (await (ready! c pos pos true))
          (let [v (field/place-verdict c (lib/cell-vec pos))]
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
  (let [e (waits/waiting-out c (await (ctx/call c :collect 'jobs.items.collect-drops
                           {:radius (settings/get settings ::collect-radius)
                            :holes (vec (remove nil? (cons (:collect (ctx/mem c)) (:holes (ctx/mem c)))))})))]
    (when-not (= :continue e) (ctx/update-mem! c dissoc :collect))
    (if (= :continue e) :continue :again)))

(defn ^:async act!
  "The step the cell is owed."
  [c {:keys [state pos block] :as cell}]
  (case state
    (:fill :dam) (await (place-source! c cell))
    :clear (await (dig! c pos pos false (dug-stray pos)))
    :soil-dig (let [under (lib/down pos)]
                (await (dig! c pos under true (dug-ground pos under))))
    :soil-place (await (soil-place! c cell))
    :plant (await (plant! c cell))))

;; ------------------------------------------------------------------ result

(defn by-pos [entries] (vec (sort-by :pos entries)))

(defn state-list
  "The result entries of the cells in state, each built by (f cell), by position."
  [states state f]
  (by-pos (map f (filter #(= state (:state %)) (vals states)))))

(defn gave-up-list
  "The cells given up on after their failed tries, of this plan's cells."
  [c states]
  (by-pos (keep (fn [[pos data]] (when (contains? states pos) {:pos (lib/cell-vec pos) :reason (:reason data)}))
                (field/gave-up c))))

(defn refused-list
  "The refusals: cells skipped in job memory (of this plan's cells) and cells of an unsupported species."
  [c states]
  (by-pos (concat (keep (fn [[pos data]]
                          (when (contains? states pos) {:pos (lib/cell-vec pos) :reason (:reason data)}))
                        (field/skipped c))
                  (state-list states :unsupported (fn [{:keys [pos]}] {:pos (lib/cell-vec pos) :reason :unsupported-species})))))

(defn result-of
  "What was done and what is left, with a :text."
  [c states]
  (let [m (ctx/mem c)
        missing (frequencies (map :species (filter #(= :short (:state %)) (vals states))))
        result {:cleared (:cleared m 0) :soiled (:soiled m 0) :planted (:planted m 0) :dammed (:dammed m 0)
                :short missing
                :wrong (state-list states :wrong (fn [{:keys [pos found species]}] {:pos (lib/cell-vec pos) :found found :species species}))
                :no-soil (state-list states :no-soil (fn [{:keys [pos why]}] {:pos (lib/cell-vec pos) :why why}))
                :no-tool (state-list states :no-tool (fn [{:keys [pos block]}] {:pos (lib/cell-vec pos) :block block}))
                :cramped (state-list states :cramped (fn [{:keys [pos at block]}] {:pos (lib/cell-vec pos) :at at :block block}))
                :wet (state-list states :wet (fn [{:keys [pos why source]}]
                                               (cond-> {:pos (lib/cell-vec pos) :why why} source (assoc :source source))))
                :refused (refused-list c states)
                :gave-up (gave-up-list c states)}]
    (assoc result :text (str "prepare done: cleared " (:cleared result) ", soiled " (:soiled result)
                             ", planted " (:planted result) ", short " (reduce + (vals missing))
                             ", left " (+ (count (:wrong result)) (count (:no-soil result)) (count (:no-tool result)) (count (:cramped result))
                                         (count (:wet result)) (count (:refused result)) (count (:gave-up result)))))))

(defn finish!
  "Be done, saying what was done. Work done with cells left (refused, wrong, short ...) ends condition-failed with the
  reason of what is left, the counts in the data."
  [c states]
  (let [result (result-of c states)
        worked? (pos? (+ (:cleared result) (:soiled result) (:planted result) (:dammed result)))
        left (cond (seq (:refused result)) :refused (seq (:gave-up result)) :gave-up)
        blocked (blocked-reason states)]
    (cond
      (and worked? (not left) (not blocked)) (ctx/end c :done nil result)
      (= :refused left) (ctx/end c :condition-failed :refused result)
      left (ctx/end c :nothing-possible :gave-up result)
      :else (ctx/end c :condition-failed (or blocked {:reason :nothing-to-do}) result))))

(defn ^:async step [c]
  (let [field (field/planned c)]
    (if (:trouble field)
      :continue
      (or (await (lib/survey-field! c field))
          (do
            (when-not (:begun (ctx/mem c))
              (ctx/update-mem! c assoc :begun true))
            (let [states (await (sense/decide! c ::states #(field/assessments c field)
                                               {:retry? (fn [states] (some #(= :unloaded (:state %)) (vals states)))}))
                  work (field/todo c states)
                  target (first work)
                  owed (:collect (ctx/mem c))]
              (cond
                (and owed (or (nil? target) (not= owed (:pos target)))) (await (collect! c))
                target (await (act! c target))
                (receding? states) (let [e (await (child/pause! c (settings/get settings ::recede-wait-s) :receding))]
                                     (if (= :continue e) :continue :again))
                :else (finish! c states))))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step c))))

;; ------------------------------------------------------------------ the contract

(def target [:plan :part])

(def defaults {:spend {:job-time ::job-time-s}})

(def meta {:category :phase :part-of '[jobs.forestry.maintain]})

(def safe? (ready/safe-at-by-night (fn [c] (some-> (field/planned c) :trees keys first))))

(def fit? ready/fit?)

(defn can-run? [c] (lib/trouble-status (field/planned c)))

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
      (let [cells (lib/field-cells field)
            states (states-of c field cells)
            todo (field/todo c states)
            {:keys [refused text] :as result} (result-of c states)
            data [:units :cells :done worked :left (count todo) :result (dissoc result :text)]]
        (cond
          (or (seq todo) (receding? states) (blocked-reason states) (seq refused) (seq (:gave-up result)) (look/unseen? (:primitives c) cells))
          (apply contract/later :preparing (str "prepare: " (count todo) " cells to work") data)
          (or (:collect m) (checks/child-under-way? c)) (apply contract/later :wrapping-up "prepare: every cell is ready, wrapping up" data)
          :else (apply contract/ok :prepared text data))))))

(defn quote-of [_c]
  {:uses {:job-time [1 60 (settings/get settings ::job-time-s)]}
   :done [0 1 1]})
