(ns jobs.movement.reach
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.span :as span]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.util :as u]
            [jobs.lib.waits :as waits]
            [jobs.movement.go-to.column :as column]
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
  {:pos {:doc "where to go: [x y z] or {:x :y :z}; [x z] or {:x :z}: the ground of that column (go-to walks it, no span is built)" :spec column/pos-or-column? :default nil}
   :range {:doc "how close counts as there, in blocks" :spec (a/num-in 0 nil) :default 1}
   :walk {:doc "the go-to call to walk with: its args and call keys, but not :pos, :range or :ways" :spec map? :default {}}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one call may take, its walks and spans included, before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-rounds {:default 4 :doc "The most spans (a stair down or a bridge) one call builds before the whole go-to runs." :spec (a/int-in 1 nil)}})

(def ways (conj way-steps/ways :bridge))

(def target [:pos])

(def defaults {:spend {:job-time ::job-time-s}})

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

(def price choose/price)

(defn feet-of [c]
  (mapv #(js/Math.floor %) (let [{:keys [x y z]} (u/self-pos c)] [x y z])))

(defn stands
  "The cells [x y z] the body can walk to (a flood of at most reach-room/room-cells cells)."
  [p]
  (let [kind-at (reach/lookup p reach-room/body-kind-of)]
    (map vec (:cells (reach-room/flood-visit #(reach/forward kind-at %1 %2 %3) (reach/standing-vec p) reach-room/room-cells)))))

(defn approach
  "{:step :approach :pos cell :then span} for the best cell the body can walk to from which (span-from cell) gives a span,
  ranked by (rank span cell) then by the nearest to feet; nil when none."
  [p feet span-from rank]
  (let [far (fn [cell] (u/dist (zipmap [:x :y :z] cell) (zipmap [:x :y :z] feet)))
        options (keep (fn [cell] (when-let [span (span-from cell)] {:step :approach :pos cell :then span})) (stands p))]
    (first (sort-by (juxt #(rank (:then %) (:pos %)) #(far (:pos %))) options))))

(defn span-way
  "The cheapest span from feet toward goal that the call allows and that is no dearer than the way go-to itself would make
  (dig route, else escape/choose's pillar, stair up or dug door); nil when there is none. A stair down that starts too
  far from the goal's column to end at it starts at a cell the body can walk to instead: {:step :approach :pos cell :then stair}; likewise to the edge a bridge starts from."
  [c feet goal skip]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)
        may-dig? (holes/may-dig-fn c)
        own? (holes/own-placed-fn c)
        opts {:own? own? :skip skip :fetch? (choose/fetch-tool? p (:doors (:walk (:args c)))) :pillar-item (escape/pillar-item c)}
        stair-from (fn [cell] (span/down-stair block-at cell goal may-dig? own? skip))
        stair (when (allowed? c :stair)
                (let [here (stair-from feet)
                      stand (when (and (not (skip :approach)) (>= (- (second feet) (second goal)) 2))
                              (approach p feet stair-from (fn [stair _] (:aside stair))))]
                  (if (and stand (or (nil? here) (<= (+ 2 (:aside (:then stand))) (:aside here)))) stand here)))
        bridge-from (fn [cell] (span/bridge-span block-at (escape/unseen-of p) cell goal opts))
        bridge (when (allowed? c :bridge)
                 (or (bridge-from feet)
                     (when-not (skip :approach)
                       (approach p feet #(when (>= (second %) (second feet)) (bridge-from %))
                                 (fn [b _] (u/dist (zipmap [:x :y :z] (:landing b)) (zipmap [:x :y :z] goal)))))))
        spans (into [] (remove nil?) [stair bridge])
        priced (fn [e] (price p feet (or (:then e) e)))
        real (or (choose/dig-way p feet goal may-dig? opts)
                 (let [e (escape/choose p feet goal may-dig? opts)]
                   (when-not (#{:none :approach} (:step e)) e)))]
    (when (seq spans)
      (let [best (apply min-key priced spans)]
        (when (or (nil? real) (<= (priced best) (price p feet real)))
          best)))))

(defn span-job
  "[job args] of the child that builds span e."
  [{:keys [step] :as e} ignore-zones? fetch?]
  (let [ways-for (if fetch? #{:carried :chest :craft :gather} #{:carried})]
    (case step
      :approach ['jobs.movement.go-to {:pos (zipmap [:x :y :z] (:pos e)) :range 0 :ways #{:walk}}]
      :stair ['jobs.access.stair {:dir :down :heading (:heading e) :steps (:steps e) :accept #{:falling-block}
                                  :ways ways-for :settings {:jobs.lib.trip/spent-uses 0} :ignore-zones? ignore-zones? :need-drop true}]
      :bridge ['jobs.access.bridge (cond-> {:heading (:heading e) :length (:length e) :ways ways-for :ignore-zones? ignore-zones?}
                                     (:item e) (assoc :item (:item e)))])))

(defn skip-key
  "What a span that changed nothing rules out: a stair only its heading, a bridge itself."
  [{:keys [step heading]}]
  (if (= :stair step) [:stair heading] step))

(defn walk-args
  "The go-to call of a round: the :walk map with the goal and ways (walk-only unless all? and then the call's, :bridge left out).
  :torches defaults to :fetch: a player lights a dark way first, so an :unseen-floor-dark ending means no torch could be had."
  [c all?]
  (let [{:keys [pos range walk]} (:args c)
        call-ways (:ways (ctx/frozen-call c))]
    (cond-> (assoc (merge {:torches :fetch} walk) :pos pos :range range)
      (not all?) (assoc :ways #{:walk})
      (and all? call-ways) (assoc :ways (disj (set call-ways) :bridge)))))

(defn forward
  "go-to's ending e as this job's own, with e as its :because."
  [c e]
  (contract/with-because (ctx/end c (:kind e) (:cause e) (:data e)) e))

(def no-route-reasons
  "go-to's reasons for a walk that found no route: the planner ran out of land or the goal is cut off or walled in, or the only way goes down onto floors never seen in the dark (a stair needs no light).
  Others (a danger, a door or mob in the way, health, a wait) are no case for a span."
  (conj choose/escalate-reasons :detour :unseen-floor-dark))

(defn walk-why
  "The reason go-to's ending e gives for not arriving."
  [e]
  (some-> (or (:why (:data e)) (:why (:cause e))) keyword))

(def gap-kinds
  "The :abilities kinds that are a gap or step the body cannot cross (not a sprint it lacks the food for, nor a door)."
  #{:gap :gap-up :gap-down :gap-width})

(defn no-way?
  "Whether go-to's ending e says walking has no route: a no-route reason, or the only way needing a gap or step the body
  cannot make (gap-kinds)."
  [e]
  (and (= :nothing-possible (:kind e))
       (let [why (walk-why e)]
         (or (contains? no-route-reasons why)
             (and (= :abilities why) (contains? gap-kinds (some-> (:kind (:data e)) keyword)))))))

(defn ^:async look-ahead!
  "Look at each floor cell along the four headings from feet, nearest first, that the body has not sensed, so a bridge
  is judged over cells it has seen."
  [c feet]
  (await (dig-look/look-unknown! c (for [k (range 1 (inc (span/max-bridge))) d escape/cardinals] (escape/up (escape/ahead feet d k) -1)))))

(defn ^:async pick!
  "After a walk found no way (ending e): remember the cheapest span (span-way) to build and return ::next; else, when the
  call allows no way but walking, e is this job's ending, else the whole go-to runs (::next)."
  [c e]
  (await (look-ahead! c (feet-of c)))
  (let [m (ctx/mem c)
        goal (mapv #(js/Math.floor %) ((juxt :x :y :z) (goal-of c)))
        span (when (< (:built m 0) (settings/get settings ::max-rounds)) (span-way c (feet-of c) goal (:skip m #{})))]
    (when span
      (ctx/emit! c :reach.span :info {:why (walk-why e) :step (:step (or (:then span) span)) :approach (= :approach (:step span)) :at (feet-of c) :text (str "no way on foot: " (name (:step span)) (when (= :approach (:step span)) " to where a stair can start"))}))
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
                    (or (:full m) (nil? (goal-of c))) (let [e (await (child/call-walk* c :full (walk-args c true)))]
                                (if (= :continue e) :continue (forward c e)))
                    (:direct m) (do (ctx/update-mem! c dissoc :direct) (await (pick! c (:last-walk m))))
                    :else
                    (let [e (await (child/call-walk* c (keyword (str "walk-" (:built m 0))) (walk-args c false)))]
                      (cond
                        (= :continue e) :continue
                        (:arrived (:data e)) (forward c e)
                        (not (no-way? e)) (forward c e)
                        :else (do (ctx/update-mem! c assoc :last-walk (select-keys e [:kind :cause :data])) (await (pick! c e))))))]
      (if (or (= ::next stepped) (and (:span m) (not= :continue stepped))) (recur) stepped))))
