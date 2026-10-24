(ns jobs.lib.build.scaffold
  "A temporary stand for jobs.build.from-plan where no ground stand reaches a cell: a pillar (jobs.access.pillar) whose
  base lies outside every plan's footprint, this plan's included (jobs.lib.access.approach/plan, which also prefers a
  stand that needs no pillar). Every block of it is in the scaffold ledger and jobs.access.cleanup takes it back once the
  cells in reach are placed, or however the call ends, so a scaffold block is never a plan block. Job memory :scaffold is
  {:phase :walk|:build|:bridge|:onto|:up|:clean :cell pos :plan {:base :height :stand :stands :onto :bridge row} :failed true}; a failed walk to a stand rules out every stand of that plan and books them as :scaffold-onto: a pillar then rises beside the nearest, to walk onto it (a roof)."
  (:require [engine.ctx :as ctx]
            [jobs.lib.waits :as waits]
            [jobs.lib.access :as access]
            [jobs.lib.access.approach :as approach]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.child :as child]
            [jobs.lib.choose :as choose]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.need :as need]
            [jobs.lib.pillar :as pl]
            [jobs.lib.ready :as ready]
            [jobs.lib.util :as u]))

(defn phase [c] (:phase (:scaffold (ctx/mem c))))

(defn open?
  "Whether this job's scaffold ledger entries (its own or a child's) are still open."
  [c]
  (boolean (some #(ledger/of-instance? (:id c) %) (ledger/open-entries (ctx/view c)))))

(defn avoid-cells
  "The cells no stand or base may be: a planned cell, or the one below it (its head would be in the plan), and the
  cells of earlier walks that failed."
  [c cells]
  (into (set (:scaffold-avoid (ctx/mem c))) (mapcat (fn [{:keys [pos]}] [pos (update pos 1 dec)])) cells))

(defn approach-input
  "The input of jobs.lib.access.approach for placing pos, with the plan's own footprint refusing a base; with sight? a
  stand must see pos (one in reach behind a wall cannot place it). A target loaded but never seen (a hole in a roof the
  body stands outside of) reads as air, and a stand must see it."
  ([c cells pos] (approach-input c cells pos true))
  ([c cells pos sight?]
   (let [in (access/rules-input c)
         block-at (:block-at in)
         hidden (dig-look/unknown? (:primitives c) pos)]
     (assoc in :block-at #(or (block-at %) (when (and hidden (= % pos)) "air"))
            :targets #{pos} :reach (plan/reach) :avoid (avoid-cells c cells) :sight? (or sight? hidden)))))

(defn blocks [c] (choose/filler-names c :pillar))

(defn carried-count [c]
  (let [have (pl/carried (:primitives c))]
    (reduce + (map #(get have % 0) (blocks c)))))

(defn plan-for
  "The approach plan (jobs.lib.access.approach/plan) for placing pos. Where it finds none but stands the body could not
  walk to (a roof), a carried pillar whose top is beside the nearest of them, with that stand as its :onto."
  [c cells pos sight?]
  (let [in (approach-input c cells pos sight?)
        r (approach/plan in)
        beside (fn [onto] (let [p (:pillar (approach/plan (assoc in :targets #{onto} :beside? true)))]
                            (when (and p (<= (:height p) (carried-count c))) (assoc p :onto onto))))
        up (when-not (or (:stand r) (:pillar r) (= :zone (:reason r)))
             (some beside (sort-by #(approach/dist3 pos %) (:scaffold-onto (ctx/mem c)))))]
    (if up {:pillar up} r)))

(defn starts-at
  "The bridge starts level with from: where a walk to one has failed, the others are as high. sight? as the row was found."
  [c cells pos from sight?]
  (filterv #(= (second from) (second %)) (approach/starts (approach-input c cells pos sight?))))

(defn bridge-block
  "The carried kind a bridge row is laid with (the one carried most), nil when none."
  [c]
  (let [have (pl/carried (:primitives c))]
    (->> (blocks c) (filter #(pos? (get have % 0))) (sort-by #(- (get have % 0))) first)))

(defn bridge-lack [c length]
  (- length (get (pl/carried (:primitives c)) (bridge-block c) 0)))

(defn fetch-opts
  "The limits {:how :explore} for getting the pillar's blocks, nil when the call's ways allow no fetch."
  [c]
  (when-let [o (need/fetch-opts c)]
    (assoc o :explore (boolean (:explore (:args c))))))

(defn ^:async fetch-blocks!
  "Get the n blocks the pillar lacks: :continue while the fetch waits, :again once had, else the ending of the call
  (condition-failed :needs :scaffold-item, with result as data)."
  [c n result]
  (let [short #(ctx/end c :condition-failed {:reason :needs :needs :scaffold-item :any-of (blocks c) :count n} result)]
    (if-let [o (fetch-opts c)]
      (let [e (await (need/blocks! c (blocks c) n o))]
        (cond (= :continue e) :continue
              (= :done (:kind e)) :again
              :else (short)))
      (short))))

(defn ^:async start!
  "Plan a stand above the ground's reach for cell and remember it: nil when none applies (the caller walks on), :again
  once planned, :continue while blocks are fetched, an ending when a zone refuses every base (cell refused) or no
  blocks can be had."
  [c cells cell result]
  (let [pos (:pos cell)
        way (fn [sight?]
              (let [r (plan-for c cells pos sight?)]
                [r (when (and (not (:pillar r)) (not (:stand r)) (not= :zone (:reason r)))
                     (let [in (approach-input c cells pos sight?)] (approach/bridge in (approach/starts in))))]))
        ;; a cell walled in on every side is seen from nowhere: a stand in reach is where the build tunnels to it or finds it has no tool
        [r row sight?] (let [[r row :as seen] (way true)] (if (or (:pillar r) (:stand r) row (= :zone (:reason r))) (conj seen true) (conj (way false) false)))
        height (get-in r [:pillar :height])
        lack (cond height (- height (carried-count c))
                   row (bridge-lack c (:length row)))]
    (cond
      (= :zone (:reason r)) (do (ctx/update-mem! c plan/refuse pos {:reason (or (:refused-by r) :zone) :why :scaffold}) :again)
      (not (or (:pillar r) (:stand r) row)) nil
      (pos? (or lack 0)) (await (fetch-blocks! c lack result))
      :else (do (ctx/update-mem! c assoc :scaffold {:phase :walk :cell pos
                                                    :plan (or (:pillar r) (when row {:stand (:from row) :stands (starts-at c cells pos (:from row) sight?) :bridge row})
                                                              {:stand (first (:stand r)) :stands (:stand r)})})
                :again))))

(defn set-phase! [c k] (ctx/update-mem! c assoc-in [:scaffold :phase] k))

(defn fail-cell!
  "Drop the scaffold plan and count a failure on its cell. The stands of a plan whose walk failed are avoided and
  booked as :scaffold-onto when two or more above the body (a climb, not a walk)."
  [c m give-up]
  (let [{:keys [base stands stand]} (:plan m)
        failed (when-not base (or stands [stand]))
        high (filter #(<= (+ 2 (js/Math.floor (:y (u/self-pos c)))) (second %)) failed)]
    (ctx/update-mem! c #(-> (plan/count-fail (dissoc % :scaffold) (:cell m) :unreachable give-up)
                            (update :scaffold-avoid (fnil into #{}) (if base [base] failed))
                            (update :scaffold-onto (fnil into #{}) high)))))

(defn ^:async walk!
  [c {:keys [plan] :as m} give-up]
  (let [[x y z] (or (:base plan) (:stand plan))
        e (await (child/call-walk* c :swalk {:pos {:x x :y y :z z} :range 0 :ways #{:walk :clear-seal}
                                             :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue e) :continue
      (and (= :done (:kind e)) (:arrived (:data e))) (do (set-phase! c (cond (:height plan) :build (:bridge plan) :bridge :else :up)) :again)
      :else (do (fail-cell! c m give-up) :again))))

(defn ^:async build!
  [c {:keys [plan]}]
  (let [e (waits/waiting-out c (await (ctx/call c :pillar 'jobs.access.pillar {:height (:height plan) :use {:blocks (blocks c)} :ways #{:carried}
                                                          :ignore-zones? (:ignore-zones? (:args c))
                                                          :checks (ready/unforced c {:safe? '(not (exposed))})})))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (set-phase! c (if (:onto plan) :onto :up)) :again)
      :else (do (ctx/update-mem! c assoc-in [:scaffold :failed] true) (set-phase! c :clean) :again))))

(defn ^:async onto!
  "Walk from the top of the pillar onto the stand it was raised beside."
  [c {:keys [plan]}]
  (let [[x y z] (:onto plan)
        e (await (child/call-walk* c :swalk {:pos {:x x :y y :z z} :range 0 :ways #{:walk}
                                             :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue e) :continue
      (and (= :done (:kind e)) (:arrived (:data e))) (do (set-phase! c :up) :again)
      :else (do (ctx/update-mem! c assoc-in [:scaffold :failed] true) (set-phase! c :clean) :again))))

(defn ^:async bridge!
  "Lay the planned bridge row (jobs.access.bridge) from the stand the body walked to; the stand is then the row's end."
  [c {:keys [plan]}]
  (let [{:keys [heading length]} (:bridge plan)
        e (waits/waiting-out c (await (ctx/call c :bridge 'jobs.access.bridge {:heading heading :length length :item (bridge-block c) :ways #{:carried}
                                                          :ignore-zones? (:ignore-zones? (:args c))
                                                          :checks (ready/unforced c {:safe? '(not (exposed))})})))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (set-phase! c :up) :again)
      :else (do (ctx/update-mem! c assoc-in [:scaffold :failed] true) (set-phase! c :clean) :again))))

(defn ^:async take-back!
  "Take the scaffold back (a jobs.access.cleanup child for this job's blocks) when it is open: nil once closed,
  :continue while the child waits."
  [c]
  (when (open? c)
    (let [e (await (ctx/atomic c :urgent :scaffold #(ctx/call c :cleanup 'jobs.access.cleanup {:of (:id c) :checks {:fit? '(always)}})))]
      (when (= :continue e) :continue))))

(defn ^:async clean!
  "Take the scaffold back, then drop its plan (a failed one counts on its cell)."
  [c {:keys [failed] :as m} give-up]
  (or (await (take-back! c))
      (do (if failed (fail-cell! c m give-up) (ctx/update-mem! c dissoc :scaffold)) :again)))

(defn ^:async step!
  "One step of the scaffold in its walk, build or clean phase: :again or :continue. A failed cell is given up after give-up failures."
  [c give-up]
  (let [m (:scaffold (ctx/mem c))]
    (case (:phase m)
      :walk (await (walk! c m give-up))
      :build (await (build! c m))
      :onto (await (onto! c m))
      :bridge (await (bridge! c m))
      :clean (await (clean! c m give-up)))))

(defn working?
  "Whether the scaffold is in a phase that step! runs; the :up phase is the build's own, placing from the stand."
  [c]
  (contains? #{:walk :build :bridge :onto :clean} (phase c)))

(defn stale?
  "Whether an earlier call left scaffold blocks open that this one has not yet tried to take back."
  [c]
  (and (nil? (:scaffold (ctx/mem c))) (not (:stale-cleaned (ctx/mem c))) (open? c)))

(defn begin-clean!
  "Start a clean phase; for a stale scaffold it is tried once."
  [c]
  (ctx/update-mem! c #(-> (assoc % :scaffold {:phase :clean}) (cond-> (not (:scaffold %)) (assoc :stale-cleaned true)))))

(defn bridge-on
  "A bridge row from the body's stand to a stand that sees the target cell when one is short enough and its blocks are carried, nil when not
  (a row to a stand that cannot see the cell places nothing); each target is tried once."
  [c cells target]
  (let [pos (:pos target)
        row (when (and pos (not (contains? (:scaffold-bridged (ctx/mem c)) pos)))
              (approach/bridge (approach-input c cells pos) nil))]
    (when (and row (not (pos? (bridge-lack c (:length row)))))
      row)))

(defn finish-up!
  "The body stands on its scaffold with nothing in reach to place or dig: a buildable cell still out of reach counts a
  failure (so a stand that does not reach it is not raised again for ever). The scaffold is then taken back, unless a
  short bridge row from the stand brings the next target cell (nearest of the lowest layer) in reach."
  [c cells todo target give-up]
  (let [cell (:cell (:scaffold (ctx/mem c)))]
    (when (some #(= cell (:pos %)) todo)
      (ctx/update-mem! c plan/count-fail cell :unreachable give-up))
    (if-let [row (bridge-on c cells target)]
      (ctx/update-mem! c #(-> % (assoc :scaffold {:phase :bridge :cell (:pos target) :plan {:bridge row}})
                              (update :scaffold-bridged (fnil conj #{}) (:pos target))))
      (set-phase! c :clean))))
