(ns jobs.forestry.fell-from-pillar
  (:require [engine.contract :as contract]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.forestry.fell-tree.log :as log]
            [jobs.lib.access :as access]
            [jobs.lib.access.approach :as approach]
            [jobs.lib.child :as child]
            [jobs.lib.choose :as choose]
            [jobs.lib.loose :as loose]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.pillar :as pl]
            [jobs.lib.sense :as sense]
            [jobs.lib.trees :refer [dig-reach default-radius log-name? log-names]]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]))

(def doc
  "Fell the logs of the tree at :base that are out of reach from the ground, standing on a pillar that is taken back.
  jobs.lib.access.approach/plan picks the stand or the pillar base for the highest logs (the logs cut so far are
  collected first, jobs.forestry.collect-drops), the body walks there (go-to), builds the pillar (jobs.access.pillar,
  every block in the scaffold ledger), digs the logs in reach top-down (jobs.blocks.dig) and jobs.access.cleanup takes
  the pillar back before the call ends (the same cleanup runs when the call starts with this job's scaffold still open).
  The pillar is built of dirt, cobblestone and the logs cut so far; with too few of those carried it gets the height in
  blocks with jobs.lib.need unless :fetch is false.
  The build, dig and clean are one urgent section (ctx/atomic): reflexes at or below that level wait for it, up to their caps.
  Zones: a pillar a zone, claim or footprint refuses warns fell-tree.declined (:reason :refused, :why :pillar), as does
  one the planner finds no way for (its :reason).
  Ends :done with {:logs n} when a log was dug and the pillar is gone. Condition-failed {:reason :need ...} when the
  blocks cannot be had (the fetch's cause is the :because; with :fetch false, :any-of and :count in the cause) and
  :refused; nothing-possible :no-stand (or the planner's reason), :walk, :build, :dig or :no-log-in-reach after the
  pillar was taken back, and :cleanup-open when it could not be (data {:logs n}); :resumed when the call began with
  a scaffold of an earlier call open, took it back and dug nothing.
  Quote: time by the logs standing.")

(a/defargs args
  {:base {:doc "{:x :y :z} of the tree's lowest log" :spec ::a/pos :default nil}
   :species {:doc "log species of the tree" :spec a/name? :default nil}
   :at {:doc "{:x :y :z} of a base log when the caller was given one: the logs are read up from it" :spec ::a/pos :default nil}
   :radius {:doc "search radius in blocks for the tree's logs" :spec (a/num-in 0 nil) :default default-radius}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :spare-own-builds {:doc "a log in a plan this body made is not felled; false: it may be" :spec boolean? :default true}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :lava-adjacent :falling-block" :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}
   :fetch {:doc "get the pillar's blocks with jobs.lib.need: true, a set of kinds or a map of limits; false ends :need" :spec need/option? :default true}
   :pillar? {:doc "dig a log out of reach of the ground from the pillar; false: the pillar is not built" :spec boolean? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(a/defargs settings
  {::drop-radius {:default 6 :doc "Blocks round the base the cut logs are picked up from before a pillar is built." :spec (a/num-in 0 nil)}
   ::time-s {:default 600 :doc "Seconds a pillar felling may take." :spec (a/num-in 0 nil)}
   ::max-steps {:default 400 :doc "Pieces of work of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}})
(defn time-s [] (settings/get settings ::time-s))
(defn max-steps [] (settings/get settings ::max-steps))

(def target [:base])

(def defaults {:spend {:time ::time-s}})

(def pillar-items (choose/fillers :pillar))

(defn base [c] (:base (:args c)))

(defn tree [c]
  (let [{:keys [x z]} (base c)]
    {:column {:x x :z z} :species (:species (:args c))}))

(defn tree-logs [c] (log/tree-logs c (:radius (:args c)) (tree c)))

(defn bad-args [c]
  (when-not (base c) {:reason :bad-args :why "no :base: the tree's lowest log"}))

(def conditions [[:fn (fn [c] (some-> (bad-args c) contract/impossible))]])

(defn done? [_c] {:done? false :units :trees :done 0 :left 1})

(defn quote-of [c]
  (let [n (count (when (base c) (tree-logs c)))]
    {:uses {:time [5 (+ 10 (* 4 n)) (time-s)]} :done [0 1 1]}))

(defn result [c] {:logs (:dug (ctx/mem c) 0)})

(defn set-pillar! [c m] (ctx/update-mem! c assoc :pillar m))

(defn pillar-failed!
  "Take back what was built, then end the call nothing-possible with cause why."
  [c why]
  (ctx/update-mem! c update :pillar assoc :phase :clean :failed why)
  :again)

(defn log-cell [l] (let [{:keys [x y z]} (:pos l)] [x y z]))

(defn pillar-plan
  "The approach plan (jobs.lib.access.approach/plan) for the highest logs: every log, then without the lowest one at a
  time until the planner finds a stand or a pillar (a zone refusal ends the tries)."
  [c logs]
  (let [in (assoc (access/rules-input c {:except (:for-plan (:args c)) :own-plans-ok? true}) :reach dig-reach)
        top-down (vec (sort-by #(- (:y (:pos %))) logs))]
    (loop [n (count top-down)]
      (let [r (approach/plan (assoc in :targets (set (map log-cell (take n top-down)))))]
        (if (and (:reason r) (not= :zone (:reason r)) (> n 1))
          (recur (dec n))
          r)))))

(defn pillar-blocks
  "The blocks the pillar is built from: dirt and cobblestone, then every kind of log carried."
  [c]
  (let [have (pl/carried (:primitives c))]
    (into pillar-items (filter #(and (pos? (get have % 0)) (log-name? %)) (sort (keys have))))))

(defn logs-carried
  "How many logs are carried."
  [c]
  (let [have (pl/carried (:primitives c))]
    (reduce + (map #(get have % 0) (remove (set pillar-items) (pillar-blocks c))))))

(defn blocks-carried
  "How many pillar blocks (dirt, cobblestone, logs) are carried."
  [c]
  (let [have (pl/carried (:primitives c))]
    (reduce + (map #(get have % 0) (pillar-blocks c)))))

(defn pillar-problem
  "The cause {:reason :need :any-of :count} while a planned pillar is not begun and fewer blocks than its height are
  carried; :count is the dirt or cobblestone wanted beside the logs carried."
  [c]
  (let [{:keys [phase plan]} (:pillar (ctx/mem c))]
    (when (and (= :walk phase) (:height plan) (< (blocks-carried c) (:height plan)))
      {:reason :need :any-of pillar-items :count (- (:height plan) (logs-carried c))})))

(defn fetch-opts
  "The limits {:how :depth :explore} for getting the pillar's blocks, nil when :fetch does not allow items."
  [c]
  (when-let [o (need/opts c 'jobs.forestry.fell-tree)]
    (when (contains? (:what o) :item) (select-keys o [:how :depth :explore]))))

(defn ^:async blocks!
  "Get the blocks a planned pillar lacks: nil when none is lacking, :continue while the fetch waits on the world, :again
  once they are had, else the ending of the call (the fetch's, or the :need cause when :fetch is off)."
  [c]
  (when-let [w (pillar-problem c)]
    (if-let [opts (fetch-opts c)]
      (let [e (await (need/blocks! c pillar-items (:count w) opts))]
        (cond (= :continue e) :continue
              (= :done (:kind e)) :again
              :else e))
      (ctx/end c :condition-failed w (result c)))))

(defn ^:async start-pillar!
  "Plan a stand or a pillar for the highest logs and remember it. A refusal that read unseen floor cells is decided again
  after a look at them (jobs.lib.sense/decide!); one that stays ends the call."
  [c]
  (let [r (await (sense/decide! c ::pillar #(pillar-plan c (tree-logs c)) {:retry? :reason}))]
    (cond
      (:stand r) (do (set-pillar! c {:phase :collect :plan {:stand (first (:stand r))}}) :again)
      (:pillar r) (do (set-pillar! c {:phase :collect :plan (:pillar r)}) :again)
      (= :zone (:reason r))
      (do (ctx/warn-once! c [:pillar-refused] :fell-tree.declined
                          {:reason :refused :why :pillar :text "a zone or plan refuses a pillar by the tree"})
          (ctx/end c :condition-failed :refused (result c)))
      :else
      (do (ctx/warn-once! c [:pillar-refused] :fell-tree.declined
                          {:reason (:reason r) :why :pillar :text (str "no way to reach the high logs: " (name (:reason r)))})
          (ctx/end c :nothing-possible (:reason r) (result c))))))

(defn own-log-drops
  "The ids of the logs lying within :drop-radius of the base that jobs.lib.loose lets the body take (not another
  player's throw, not in a zone that refuses taking)."
  [c base]
  (let [r (settings/get settings ::drop-radius)]
    (->> (loose/candidates (:primitives c) (ctx/view c) (:world (:engine c)) {:radius (+ r (u/dist base (u/self-pos c))) :always log-names} (:root c))
         (filter #(and (log-name? (some-> (.-item %) .-name)) (<= (u/dist base (u/pos-of (.-pos %))) r)))
         (mapv #(.-id %)))))

(defn ^:async pillar-collect!
  "Pick up the logs cut so far (they are the pillar's blocks when no dirt is carried): a collect-drops child for the
  ones lying round the base that the body may take. Finding none is no failure."
  [c]
  (let [ids (own-log-drops c (base c))
        e (if (seq ids)
            (await (child/run!* c :pcollect 'jobs.forestry.collect-drops
                                {:radius (settings/get settings ::drop-radius) :ids ids :near (base c) :search 0 :want 0}))
            :done)]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c assoc-in [:pillar :phase] :walk) :again))))

(defn ^:async pillar-walk!
  [c {:keys [plan]}]
  (let [e (await (child/call-walk* c :pwalk {:pos (u/cell-pos (or (:base plan) (:stand plan))) :range 0 :escalate false :zone-tolls true
                                             :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue e) :continue
      (and (= :done (:kind e)) (:arrived (:data e))) (do (ctx/update-mem! c assoc-in [:pillar :phase] (if (:height plan) :build :dig)) :again)
      :else (pillar-failed! c :walk))))

(defn ^:async pillar-build!
  [c {:keys [plan]}]
  (let [e (await (ctx/call c :pillar 'jobs.access.pillar {:height (:height plan) :use {:blocks (pillar-blocks c)} :fetch false
                                                          :ignore-zones? (:ignore-zones? (:args c))}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (ctx/update-mem! c assoc-in [:pillar :phase] :dig) :again)
      :else (pillar-failed! c :build))))

(defn ^:async pillar-dig!
  "Centre on the stand cell the plan counted reach from, then dig the highest log in reach; none in reach: take the
  pillar back."
  [c {:keys [plan]}]
  (let [[sx _ sz] (:stand plan)
        _ (await (walk/centre! c (+ sx 0.5) (+ sz 0.5)))
        here (u/self-pos c)
        in-reach (filter #(<= (u/eye-dist here (:pos %)) dig-reach) (tree-logs c))
        l (first (sort-by #(- (:y (:pos %))) in-reach))]
    (if-not l
      (do (ctx/update-mem! c update :pillar #(cond-> (assoc % :phase :clean) (not (:dug %)) (assoc :failed :no-log-in-reach))) :again)
      (let [r (await (log/dig-log! c l {:base (base c) :species (:species (:args c))}))]
        (if (= :ok r)
          (do (ctx/update-mem! c assoc-in [:pillar :dug] true) (u/progress! c) :again)
          (pillar-failed! c (if (#{:unreachable :cannot :refused :blocked} r) :dig r)))))))

(defn ^:async pillar-clean!
  "Take the pillar back (jobs.access.cleanup child for this job's blocks); then the call ends: done when a log was
  dug and the pillar did not fail, else nothing-possible with the failure. A cleanup that leaves the ledger open ends it
  :cleanup-open."
  [c {:keys [failed]}]
  (let [e (await (ctx/call c :cleanup 'jobs.access.cleanup {:job (:id c)}))]
    (cond
      (= :continue e) :continue
      (log/open-scaffold? c) (ctx/end c :nothing-possible :cleanup-open (result c))
      :else (do (ctx/update-mem! c dissoc :pillar)
                (if failed
                  (ctx/end c :nothing-possible failed (result c))
                  (ctx/end c :done nil (result c)))))))

(defn ^:async pillar-phase!
  "One phase of the pillar: pick up the logs, walk to its base (or stand), build it, dig from it, take it back."
  [c]
  (let [m (:pillar (ctx/mem c))]
    (case (:phase m)
      :collect (await (pillar-collect! c))
      :walk (await (pillar-walk! c m))
      :build (await (pillar-build! c m))
      :dig (await (pillar-dig! c m))
      :clean (await (pillar-clean! c m)))))

(def climb-phases #{:build :dig :clean})

(defn climbing? [c] (contains? climb-phases (:phase (:pillar (ctx/mem c)))))

(defn ^:async pillar-round!
  "The pillar's next phase. Build, dig and clean run as one urgent section (ctx/atomic :pillar): a night or low-food
  reflex waits for the pillar to be taken back, up to its cap."
  [c]
  (if (climbing? c)
    (await (ctx/atomic c :urgent :pillar
                       #(pace/steps! c (fn ^:async climb []
                                         (let [r (await (pillar-phase! c))]
                                           (if (and (= :again r) (not (climbing? c))) :climbed r))))))
    (await (pillar-phase! c))))

(defn ^:async step
  "One piece of the work: :again, :continue (a child waits on the world) or the ending."
  [c]
  (cond
    (nil? (:pillar (ctx/mem c)))
    (if (log/open-scaffold? c)
      (do (set-pillar! c {:phase :clean :failed :resumed}) :again)
      (await (start-pillar! c)))

    :else
    (or (await (blocks! c))
        (let [r (await (pillar-round! c))] (if (= :climbed r) :again r)))))

(defn ^:async round
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (step c) :continue)))))
