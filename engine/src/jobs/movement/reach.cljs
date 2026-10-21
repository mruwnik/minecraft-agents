(ns jobs.movement.reach
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.span :as span]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.waits :as waits]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.movement.go-to.holes :as holes]
            [jobs.movement.go-to.way-steps :as way-steps]))

(def doc
  "Walk to :pos, making a way where the walk finds none: go-to over the ways it does not take itself, a stair down to a
  goal 2 or more below and a bridge over a gap toward it, chosen by price (jobs.lib.cost.ways) against the way go-to would
  make (a pillar, a stair up, a dug door or route).
  Each round walks only (go-to with :ways #{:walk}, the other :walk args). A walk that ends in arrival ends this job done
  {:arrived true}. A walk that finds no way (nothing-possible with a planner :why that proves the world must change) prices the
  stair down and the bridge from where the body stands: when the cheaper of them is no dearer than go-to's own way it is
  built (jobs.access.stair :dir :down, jobs.access.bridge) and the next round walks on; else the whole go-to runs once with
  all the :ways it allows and its ending is this job's. A span that changed nothing is not tried again; at most the
  max-rounds setting of them are built. Other endings of the walk (a limit, a refused condition, a danger) pass up unchanged.
  :ways (default all) are go-to's plus :bridge; :stair covers the stair down. Every cell a span cuts or fills is checked
  against zones and plans (the cells go-to's :ignore-zones? lets through), natural blocks are cut only with :stair.
  Ends: done {:arrived true}; go-to's ending otherwise, with it as :because. :continue while a child waits on the world.
  Quote: a walk to the goal.")

(a/defargs args
  {:pos {:doc "where to go, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :range {:doc "how close counts as there, in blocks" :spec (a/num-in 0 nil) :default 1}
   :walk {:doc "the go-to call to walk with: its args and call keys, but not :pos, :range or :ways" :spec map? :default {}}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one call may take, its walks and spans included, before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-rounds {:default 4 :doc "The most spans (a stair down or a bridge) one call builds before the whole go-to runs." :spec (a/int-in 1 nil)}})

(def ways (conj way-steps/ways :bridge))

(def target [:pos])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def safe? (fn [_] (contract/ok :holds "go-to judges the walk")))
(def fit? safe?)

(def can-start?
  (checks/args-fit "needs a :pos" (fn [c] (some? (:pos (:args c))))))

(defn goal-of
  "The cell {:x :y :z} to walk to."
  [c]
  (some-> (:pos (:args c)) places/parse-pos :pos))

(defn done?
  [c]
  (let [cell (goal-of c)
        there? (and cell (u/within? (u/self-pos c) cell (:range (:args c))))]
    (checks/goal there? [:there "within range of the goal"] [:walking "not within range yet"] {:result {:arrived true}})))

(defn quote-of [c]
  (let [aim (goal-of c)]
    (cond-> {} aim (assoc :moves-to [(:x aim) (:y aim) (:z aim)]))))

(defn allowed?
  "Whether the call's :ways allow k."
  [c k]
  (way-steps/way? c k))

(defn price
  "Seconds way e from feet is reckoned to take (jobs.lib.cost): a stair, a pillar or a bridge by its blocks, a dug door or
  route by what its cells take to dig with the tool carried, plus a step a cell."
  [p feet {:keys [step] :as e}]
  (let [block-at (escape/block-at-of p)
        dig-s (fn [cells] (transduce (map #(+ (/ (tools/clear-ms p (block-at %)) 1000) (:cost (cost/walk-cost {:distance 1})))) + 0 cells))]
    (case step
      :stair (+ (cw/stair-cost (:steps e)) (* (:aside e 0) (or (cost/dig-cost "stone") 1)))
      :pillar (cw/pillar-cost (:height e))
      :bridge (cw/bridge-cost (:length e) (:short e 0))
      :dig-route (dig-s (map :cell (:cells e)))
      :clear-path (dig-s (:cells (escape/door block-at (escape/unseen-of p) feet (escape/heading-dirs (:heading e)) (escape/max-door))))
      0)))

(defn feet-of [c]
  (mapv #(js/Math.floor %) (let [{:keys [x y z]} (u/self-pos c)] [x y z])))

(defn stair-stand
  "{:step :approach :pos cell :then stair}: the cell the body can walk to (a flood of at most reach-room/room-cells cells)
  from which stair-from (span/down-stair) is cheapest to reach the goal: the fewest blocks it ends aside the goal's column,
  then the nearest to feet; nil when none."
  [p feet stair-from]
  (let [kind-at (reach/lookup p reach-room/body-kind-of)
        far (fn [cell] (u/dist (zipmap [:x :y :z] cell) (zipmap [:x :y :z] feet)))
        stands (->> (:cells (reach-room/flood-visit #(reach/forward kind-at %1 %2 %3) (reach/standing-vec p) reach-room/room-cells))
                    (keep (fn [cell] (let [cell (vec cell)] (when-let [stair (stair-from cell)] {:step :approach :pos cell :then stair})))))]
    (when (seq stands)
      (first (sort-by (juxt #(:aside (:then %)) #(far (:pos %))) stands)))))

(defn span-way
  "The cheapest span from feet toward goal that the call allows and that is no dearer than the way go-to itself would make
  (dig route, else escape/choose's pillar, stair up or dug door); nil when there is none. A stair down that starts too
  far from the goal's column to end at it starts at a cell the body can walk to instead: {:step :approach :pos cell :then stair}."
  [c feet goal skip]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)
        may-dig? (holes/may-dig-fn c)
        own? (holes/own-placed-fn c)
        opts {:own? own? :skip skip :fetch? (choose/fetch-tool? p (:doors (:walk (:args c)))) :pillar-item (escape/pillar-item c)}
        stair-from (fn [cell] (span/down-stair block-at cell goal may-dig? own? skip))
        stair (when (allowed? c :stair)
                (let [here (stair-from feet)
                      stand (when (and (not (skip :approach)) (>= (- (second feet) (second goal)) 2)) (stair-stand p feet stair-from))]
                  (if (and stand (or (nil? here) (<= (+ 2 (:aside (:then stand))) (:aside here)))) stand here)))
        spans (into [] (remove nil?)
                    [stair (when (allowed? c :bridge) (span/bridge-span block-at (escape/unseen-of p) feet goal opts))])
        priced (fn [e] (price p feet (or (:then e) e)))]
    (when (seq spans)
      (let [best (apply min-key priced spans)
            real (or (choose/dig-way p feet goal may-dig? opts)
                     (let [e (escape/choose p feet goal may-dig? opts)]
                       (when-not (#{:none :approach} (:step e)) e)))]
        (when (or (nil? real) (<= (priced best) (price p feet real)))
          best)))))

(defn span-job
  "[job args] of the child that builds span e."
  [{:keys [step] :as e} ignore-zones? fetch?]
  (let [ways-for (if fetch? #{:carried :chest :craft :gather} #{:carried})]
    (case step
      :approach ['jobs.movement.go-to {:pos (zipmap [:x :y :z] (:pos e)) :range 0 :ways #{:walk} :checks {:fit? '(always)}}]
      :stair ['jobs.access.stair {:dir :down :heading (:heading e) :steps (:steps e) :checks {:fit? '(always)} :accept #{:falling-block}
                                  :ways ways-for :settings {:jobs.lib.trip/spent-uses 0} :ignore-zones? ignore-zones? :need-drop true}]
      :bridge ['jobs.access.bridge (cond-> {:heading (:heading e) :length (:length e) :ways ways-for :ignore-zones? ignore-zones? :checks {:fit? '(always)}}
                                     (:item e) (assoc :item (:item e)))])))

(defn skip-key
  "What a span that changed nothing rules out: a stair only its heading, a bridge itself."
  [{:keys [step heading]}]
  (if (= :stair step) [:stair heading] step))

(defn walk-args
  "The go-to call of a round: the :walk map with the goal and ways (walk-only unless all? and then the call's, :bridge left out)."
  [c all?]
  (let [{:keys [pos range walk]} (:args c)
        call-ways (:ways (ctx/frozen-call c))]
    (cond-> (assoc walk :pos pos :range range)
      (not all?) (assoc :ways #{:walk})
      (and all? call-ways) (assoc :ways (disj (set call-ways) :bridge)))))

(defn forward
  "go-to's ending e as this job's own, with e as its :because."
  [c e]
  (contract/with-because (ctx/end c (:kind e) (:cause e) (:data e)) e))

(def refusals
  "go-to's nothing-possible reasons that no change to the world answers: a goal or call that is not valid."
  #{:bad-pos :bad-name :unsupported :no-way :goal-on-magma})

(defn no-way?
  "Whether go-to's ending e says the walk found no way: nothing-possible for a reason that is no refusal."
  [e]
  (and (= :nothing-possible (:kind e))
       (not (contains? refusals (some-> (or (:why (:data e)) (:why (:cause e)) (:reason (:cause e))) keyword)))))

(defn pick!
  "After a walk found no way (ending e): remember the cheapest span (span-way) to build and return ::next; else, when the
  call allows no way but walking, e is this job's ending, else the whole go-to runs (::next)."
  [c e]
  (let [m (ctx/mem c)
        goal (mapv #(js/Math.floor %) ((juxt :x :y :z) (goal-of c)))
        span (when (< (:built m 0) (settings/get settings ::max-rounds)) (span-way c (feet-of c) goal (:skip m #{})))]
    (when span
      (ctx/emit! c :reach.span :info {:step (:step (or (:then span) span)) :approach (= :approach (:step span)) :at (feet-of c) :text (str "no way on foot: " (name (:step span)) (when (= :approach (:step span)) " to where a stair can start"))}))
    (if (and (not span) (= #{:walk} (disj (set (or (:ways (ctx/frozen-call c)) ways)) :bridge)))
      (forward c e)
      (do (ctx/update-mem! c (fn [m] (if span (assoc m :span span :from (feet-of c)) (assoc m :full true))))
          ::next))))

(defn ^:async build-span!
  "One call of the span in memory (:span): :continue while its child waits; else the span is over. One that changed
  nothing is skipped from now on; an approach that moved the body is followed by the next span without a walk first
  (a walk would take the body back to the nearest point)."
  [c]
  (let [{:keys [span from built]} (ctx/mem c)
        [job args] (span-job span (boolean (:ignore-zones? (:args c))) (choose/fetch-tool? (:primitives c) (:doors (:walk (:args c)))))
        r (waits/waiting-out c (await (ctx/call c (keyword (str "span-" (or built 0))) job args)))
        moved? (not= from (feet-of c))]
    (when-not (= :continue r)
      (ctx/update-mem! c #(cond-> (-> % (dissoc :span :from) (update :built (fnil inc 0)))
                            (not moved?) (update :skip (fnil conj #{}) (skip-key span))
                            (and moved? (= :approach (:step span))) (assoc :direct true))))
    r))

(defn ^:async round
  [c]
  (loop []
    (let [m (ctx/mem c)
          stepped (cond
                    (:span m) (await (build-span! c))
                    (:full m) (let [e (await (child/call-walk* c :full (walk-args c true)))]
                                (if (= :continue e) :continue (forward c e)))
                    (:direct m) (do (ctx/update-mem! c dissoc :direct) (pick! c (:last-walk m)))
                    :else
                    (let [e (await (child/call-walk* c (keyword (str "walk-" (:built m 0))) (walk-args c false)))]
                      (cond
                        (= :continue e) :continue
                        (:arrived (:data e)) (forward c e)
                        (not (no-way? e)) (forward c e)
                        :else (do (ctx/update-mem! c assoc :last-walk (select-keys e [:kind :cause :data])) (pick! c e)))))]
      (if (or (= ::next stepped) (and (:span m) (not= :continue stepped))) (recur) stepped))))
