(ns jobs.forestry.fell-from-pillar
  (:require [clojure.string :as str]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.forestry.fell-tree.log :as log]
            [jobs.lib.access :as access]
            [jobs.lib.access.approach :as approach]
            [jobs.lib.blocks :as b]
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
  collected first, jobs.items.collect-drops, and so are the ones lying within dig reach of the pillar top before it comes down, those out of walking reach lowered by digging the leaf or log they lie on), the body walks there (go-to), builds the pillar (jobs.access.pillar,
  every block in the scaffold ledger), digs the logs in reach top-down (jobs.blocks.dig) and the cleanup export
  (jobs.access.cleanup) takes the pillar back however the call ends, a cut included (and when the call starts with
  this job's scaffold still open).
  The pillar is built of dirt, cobblestone and the logs cut so far; with too few of those carried it gets the height in
  blocks with jobs.lib.need unless :fetch is false.
  The build and dig, and the cleanup's take-back, are urgent sections (ctx/atomic): reflexes at or below that level wait for them, up to their caps.
  Zones: a pillar a zone, claim or footprint refuses warns fell-tree.declined (:reason :refused, :why :pillar), as does
  one the planner finds no way for (its :reason).
  Done? is ok once a log was dug from a pillar, the ledger is closed and no log is left in dig reach of its stand.
  Ends :done with {:logs n} when a log was dug (a pillar the cleanup could not take back is its :cleanup-open).
  A scaffold of an earlier call still open when the call starts is taken back first.
  Condition-failed {:reason :need ...} when the blocks cannot be had (the fetch's cause is the :because; with :fetch false, :any-of and :count in the cause) and
  :refused; nothing-possible :no-stand (or the planner's reason), :walk, :build, :dig or :no-log-in-reach after the
  pillar was taken back.
  Quote: time by the logs standing.")

(a/defargs args
  {:base {:doc "{:x :y :z} of the tree's lowest log" :spec ::a/pos :default nil}
   :species {:doc "log species of the tree" :spec a/name? :default nil}
   :at {:doc "{:x :y :z} of a base log when the caller was given one: the logs are read up from it" :spec ::a/pos :default nil}
   :radius {:doc "search radius in blocks for the tree's logs" :spec (a/num-in 0 nil) :default default-radius}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :lava-adjacent :falling-block" :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}
   :fetch {:doc "get the pillar's blocks with jobs.lib.need: true, a set of kinds or a map of limits; false ends :need" :spec need/option? :default true}
   :pillar? {:doc "dig a log out of reach of the ground from the pillar; false: the pillar is not built" :spec boolean? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::drop-radius {:default 6 :doc "Blocks round the base the cut logs are picked up from before a pillar is built." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds a pillar felling may take." :spec (a/num-in 0 nil)}
   ::settle-ms {:default 400 :doc "Milliseconds between two looks at the cut logs still falling when the top collect starts." :spec (a/int-in 1 nil)}
   ::settle-min {:default 5 :doc "Fewest looks at the cut logs before the top collect goes on: a drop may be seen a moment after it lands." :spec (a/int-in 0 nil)}
   ::settle-looks {:default 8 :doc "Most looks at the falling logs before the top collect goes on." :spec (a/int-in 0 nil)}
   ::max-steps {:default 400 :doc "Pieces of work of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))
(defn max-steps [] (settings/get settings ::max-steps))

(def target [:base])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (or (:at (:args c)) (:base (:args c)))))))

(def fit? ready/fit?)

(defn pillar-items
  "The dirt and cobblestone names to build the pillar from, less any a :choose/override avoids."
  [c]
  (choose/filler-names c :pillar))

(defn base [c] (:base (:args c)))

(defn tree [c]
  (let [{:keys [x z]} (base c)]
    {:column {:x x :z z} :species (:species (:args c))}))

(defn tree-logs [c] (log/tree-logs c (:radius (:args c)) (tree c)))

(defn bad-args [c]
  (if (base c)
    (contract/ok :args "a tree base given")
    (contract/never :bad-args "no :base: the tree's lowest log")))

(def can-start? bad-args)

(declare result)

(defn logs-left-in-reach?
  "Whether a log of the tree is still within dig reach of the stand the pillar was planned for."
  [c]
  (let [[sx sy sz] (get-in (ctx/mem c) [:pillar :plan :stand])
        here {:x (+ sx 0.5) :y sy :z (+ sz 0.5)}]
    (boolean (some #(<= (u/eye-dist here (:pos %)) dig-reach) (tree-logs c)))))

(defn done?
  "Ok when a log was dug from a pillar, the ledger is closed and no log of the tree is left in dig reach of its stand;
  else :later (the round ends the call once the pillar is taken back)."
  [c]
  (let [ok? (and (:dug (ctx/mem c)) (get-in (ctx/mem c) [:pillar :plan :stand]) (not (log/open-scaffold? c)) (not (logs-left-in-reach? c)))]
    ((if ok? contract/ok contract/later)
     :felling (str "fell-from-pillar: " (:dug (ctx/mem c) 0) " logs dug") :units :trees :done (if ok? 1 0) :left (if ok? 0 1)
     :result (result c))))

(defn quote-of [c]
  (let [n (count (when (base c) (tree-logs c)))]
    {:uses {:job-time [5 (+ 10 (* 4 n)) (time-s)]} :done [0 1 1]}))

(defn result [c] {:logs (:dug (ctx/mem c) 0)})

(def max-walk-fails 3)

(defn set-pillar! [c m] (ctx/update-mem! c assoc :pillar m))

(defn pillar-failed!
  "Take back what was built, then end the call nothing-possible with cause why."
  [c why]
  (ctx/update-mem! c update :pillar assoc :phase :clean :failed why)
  :again)

(defn walk-failed!
  "The walk to the planned stand or base failed: avoid that cell and plan again; the call ends :walk after
  max-walk-fails."
  [c plan]
  (ctx/update-mem! c update :avoid (fnil conj #{}) (or (:base plan) (:stand plan)))
  (if (< (count (:avoid (ctx/mem c))) max-walk-fails)
    (do (ctx/update-mem! c dissoc :pillar) :again)
    (pillar-failed! c :walk)))

(defn log-cell [l] (let [{:keys [x y z]} (:pos l)] [x y z]))

(defn pillar-plan
  "The approach plan (jobs.lib.access.approach/plan) for the highest logs: every log, then without the lowest one at a
  time until the planner finds a stand or a pillar (a zone refusal included)."
  [c logs avoid]
  (let [in (assoc (access/rules-input c {:except (:for-plan (:args c)) :own-plans-ok? true}) :reach dig-reach :avoid avoid)
        top-down (vec (sort-by #(- (:y (:pos %))) logs))]
    (loop [n (count top-down)]
      (let [r (approach/plan (assoc in :targets (set (map log-cell (take n top-down)))))]
        (if (and (:reason r) (> n 1))
          (recur (dec n))
          r)))))

(defn pillar-blocks
  "The blocks the pillar is built from: dirt and cobblestone, then every kind of log carried."
  [c]
  (let [have (pl/carried (:primitives c))]
    (into (pillar-items c) (filter #(and (pos? (get have % 0)) (log-name? %)) (sort (keys have))))))

(defn logs-carried
  "How many logs are carried."
  [c]
  (let [have (pl/carried (:primitives c))]
    (reduce + (map #(get have % 0) (remove (set (pillar-items c)) (pillar-blocks c))))))

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
      {:reason :need :any-of (pillar-items c) :count (- (:height plan) (logs-carried c))})))

(defn fetch-opts
  "The limits {:how :explore} for getting the pillar's blocks, nil when :fetch does not allow items."
  [c]
  (when-let [o (need/opts c 'jobs.forestry.fell-tree)]
    (when (contains? (:what o) :item) (select-keys o [:how :explore]))))

(defn ^:async blocks!
  "Get the blocks a planned pillar lacks: nil when none is lacking, :continue while the fetch waits on the world, :again
  once they are had, else the ending of the call (the fetch's, or the :need cause when :fetch is off)."
  [c]
  (when-let [w (pillar-problem c)]
    (if-let [opts (fetch-opts c)]
      (let [e (await (need/blocks! c (pillar-items c) (:count w) opts))]
        (cond (= :continue e) :continue
              (= :done (:kind e)) :again
              :else e))
      (ctx/end c :condition-failed w (result c)))))

(defn ^:async start-pillar!
  "Plan a stand or a pillar for the highest logs and remember it. A refusal that read unseen floor cells is decided again
  after a look at them (jobs.lib.sense/decide!); one that stays ends the call."
  [c]
  (let [r (await (sense/decide! c ::pillar #(pillar-plan c (tree-logs c) (:avoid (ctx/mem c))) {:retry? :reason}))]
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

(defn own-log-drop-entities
  "The logs lying within r of the centre that jobs.lib.loose lets the body take (not another player's throw, not in a
  zone that refuses taking)."
  [c centre r]
  (->> (loose/candidates (:primitives c) (ctx/view c) (:world (:engine c)) {:radius (+ r (u/dist centre (u/self-pos c))) :always log-names} (:root c))
       (filter #(and (log-name? (some-> (.-item %) .-name)) (<= (u/dist centre (u/pos-of (.-pos %))) r)))))

(defn own-log-drops
  "The ids of own-log-drop-entities."
  [c centre r]
  (mapv #(.-id %) (own-log-drop-entities c centre r)))

(defn ^:async pillar-collect!
  "Pick up the logs cut so far (they are the pillar's blocks when no dirt is carried): a collect-drops child for the
  ones lying round the base that the body may take. Finding none is no failure."
  [c]
  (let [ids (own-log-drops c (base c) (settings/get settings ::drop-radius))
        e (if (seq ids)
            (await (child/run!* c :pcollect 'jobs.items.collect-drops
                                (log/collect-args c {:radius (settings/get settings ::drop-radius) :ids ids :near (base c) :search 0})))
            :done)]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c assoc-in [:pillar :phase] :walk) :again))))

(defn ^:async pillar-walk!
  [c {:keys [plan]}]
  (let [e (await (child/call-walk* c :pwalk {:pos (u/cell-pos (or (:base plan) (:stand plan))) :range 0 :ways #{:walk :clear-seal}
                                             :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue e) :continue
      (and (= :done (:kind e)) (:arrived (:data e))) (do (ctx/update-mem! c assoc-in [:pillar :phase] (if (:height plan) :build :dig)) :again)
      :else (walk-failed! c plan))))

(defn ^:async pillar-build!
  [c {:keys [plan]}]
  (let [e (await (ctx/call c :pillar 'jobs.access.pillar {:height (:height plan) :use {:blocks (pillar-blocks c)} :fetch false
                                                          :ignore-zones? (:ignore-zones? (:args c)) :checks (ready/unforced c {:safe? '(not (exposed))})}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (ctx/update-mem! c assoc-in [:pillar :phase] :dig) :again)
      :else (pillar-failed! c :build))))

(defn blocking-leaf
  "The leaf cell nearest the eye on the line to the highest log in dig reach that the eye does not see, nil when there is
  none or something else than leaves is in the way."
  [c]
  (let [log (->> (tree-logs c) (filter #(<= (u/eye-dist (u/self-pos c) (:pos %)) dig-reach)) (sort-by #(- (:y (:pos %)))) first)]
    (some->> log :pos (log/blocking-leaf c))))

(defn ^:async pillar-dig!
  "Centre on the stand cell the plan counted reach from, then dig the highest log in reach that the eye sees (blocks.dig
  digs only what it sees; a stand inside the trunk sees only the log above its head); none: dig the leaf that hides the
  highest log in reach, else take the pillar back."
  [c {:keys [plan]}]
  (let [[sx _ sz] (:stand plan)
        _ (await (walk/centre! c (+ sx 0.5) (+ sz 0.5)))
        in-reach (filter #(b/in-reach? c (:pos %) true) (tree-logs c))
        l (first (sort-by #(- (:y (:pos %))) in-reach))
        leaf (when-not l (blocking-leaf c))]
    (cond
      leaf
      (let [e (await (ctx/call c :dig 'jobs.blocks.dig (log/log-dig-args c leaf)))]
        (if (or (= :continue e) (= :done (:kind e)))
          :again
          (pillar-failed! c :dig)))
      (not l)
      (do (ctx/update-mem! c update :pillar #(cond-> (assoc % :phase :clean) (not (:dug %)) (assoc :failed :no-log-in-reach))) :again)
      :else
      (let [r (await (log/dig-log! c l {:base (base c) :species (:species (:args c))}))]
        (if (= :ok r)
          (do (ctx/update-mem! c assoc-in [:pillar :dug] true) (u/progress! c) :again)
          (pillar-failed! c (if (#{:unreachable :cannot :refused :blocked} r) :dig r)))))))

(def item-half-width "An item rests on a block under any part of its 0.25-wide box." 0.13)

(defn support-cells
  "The cells a drop at pos rests on: those under its box (a log lies on the edge of a leaf beside the cell it is over)."
  [{:keys [x y z]}]
  (distinct (for [dx [(- item-half-width) item-half-width] dz [(- item-half-width) item-half-width]]
              [(js/Math.floor (+ x dx)) (js/Math.floor (- y 0.01)) (js/Math.floor (+ z dz))])))

(defn drop-support
  "The leaf or log cell the drop in cell lies on (never the cell column of the body), nil for any other block. pos is
  the drop's position when known."
  [c cell pos]
  (let [[x y z] cell
        [sx _ sz] (u/cell-vec (u/self-pos c))
        cells (if pos (support-cells pos) [[x (dec y) z]])]
    (->> cells
         (remove (fn [[cx _ cz]] (and (= cx sx) (= cz sz))))
         (filter (fn [cell] (let [n (u/seen-name (:primitives c) (u/cell-pos cell))]
                              (and n (or (log-name? n) (str/ends-with? n "_leaves"))))))
         first)))

(defn left-cells
  "The cells of the drops collect-drops left as unreachable, from its ending."
  [e]
  (into [] (comp (filter #(= :unreachable (:reason %))) (map #(u/cell-vec (:pos %)))) (get-in e [:data :left])))

(def max-lowered "Drops lowered by digging their support, per call." 4)

(defn ^:async settle-drops!
  "Wait until the logs lying in dig reach are seen and stop moving (a drop in the air is given up as unreachable, one
  not yet seen is not tried): at least ::settle-min looks, at most ::settle-looks. :continue when a pause was cut."
  [c]
  (let [spots #(into {} (map (juxt (fn [e] (.-id e)) (fn [e] (u/pos-of (.-pos e))))) (own-log-drop-entities c (u/self-pos c) dig-reach))]
    (loop [before (spots) n 0]
      (when (< n (settings/get settings ::settle-looks))
        (if (= :continue (await (child/pause! c (/ (settings/get settings ::settle-ms) 1000) :settling-drops)))
          :continue
          (let [after (spots)]
            (when (or (not= before after) (< n (settings/get settings ::settle-min)))
              (recur after (inc n)))))))))

(defn ^:async top-collect!
  "Before the pillar comes down: pick up the logs lying within dig reach of the pillar top (they rest on the canopy and
  cannot be reached from the ground). One that cannot be walked to is lowered: the leaf or log it lies on is dug, so it
  falls to the pillar or the ground, where the final collect takes it. nil when done or none lie there, :continue
  while a child waits."
  [c]
  (when-not (:topped (:pillar (ctx/mem c)))
    (let [settle (when-not (:settled (:pillar (ctx/mem c))) (await (settle-drops! c)))
          _ (when-not (= :continue settle) (ctx/update-mem! c assoc-in [:pillar :settled] true))
          n (:lowered (:pillar (ctx/mem c)) 0)
          ents (own-log-drop-entities c (u/self-pos c) dig-reach)
          ids (mapv #(.-id %) ents)
          e (if (and (seq ids) (not= :continue settle))
              (await (child/run!* c (keyword (str "tcollect" n)) 'jobs.items.collect-drops
                                  (log/collect-args c {:radius dig-reach :ids ids :near (u/self-pos c) :search 0})))
              :done)
          at (into {} (map (juxt #(u/cell-vec (u/pos-of (.-pos %))) #(u/pos-of (.-pos %)))) ents)
          cell (when (and (map? e) (< n max-lowered)) (some #(drop-support c % (at %)) (left-cells e)))]
      (cond
        (or (= :continue settle) (= :continue e)) :continue
        (not cell) (do (ctx/update-mem! c assoc-in [:pillar :topped] true) nil)
        :else
        (let [d (await (ctx/call c (keyword (str "lower" n)) 'jobs.blocks.dig (log/log-dig-args c (u/cell-pos cell))))]
          (if (= :continue d)
            :continue
            (do (ctx/update-mem! c update-in [:pillar :lowered] (fnil inc 0)) :again)))))))

(defn ^:async pillar-clean!
  "The call ends: done when a log was dug and the pillar did not fail, else nothing-possible with the failure. The
  logs lying in reach of the top are collected first; the pillar comes down in cleanup."
  [c {:keys [failed]}]
  (if failed
    (ctx/end c :nothing-possible failed (result c))
    (or (await (top-collect! c))
        (ctx/end c :done nil (result c)))))

(defn ^:async take-back!
  "Take the pillar back (jobs.access.cleanup child for this job's blocks) when its scaffold is open: nil once the
  ledger is closed, :continue while the child waits, else the nothing-possible :cleanup-open ending."
  [c]
  (when (log/open-scaffold? c)
    (let [e (await (ctx/atomic c :urgent :pillar #(ctx/call c :cleanup 'jobs.access.cleanup {:job (:id c) :checks {:fit? '(always) :safe? '(not (threatened))}})))]
      (cond
        (= :continue e) :continue
        (log/open-scaffold? c) (ctx/end c :nothing-possible :cleanup-open (result c))))))

(defn ^:async cleanup
  "However the call ends: take the pillar back. Done when the ledger is closed, else nothing-possible :cleanup-open."
  [c _]
  (or (await (take-back! c)) (ctx/end c :done nil)))

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
  reflex waits for the dig to end, up to its cap."
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
    (or (await (take-back! c)) (await (start-pillar! c)))

    :else
    (or (await (blocks! c))
        (let [r (await (pillar-round! c))] (if (= :climbed r) :again r)))))

(defn ^:async round
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (step c) :continue)))))
