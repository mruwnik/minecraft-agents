(ns jobs.movement.go-to.escalation
  "go-to's way out of a shut-in body, or in to a walled-in goal: choose pillar, stair, clear-path or a walk to the wall,
  run the child, note the holes it dug, put them back once the walk is on past them."
  (:require [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.cost :as cost]
            [jobs.lib.ready :as ready]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.own-pen :as own-pen]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.movement.go-to.climb :as climb]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.torch :as torch]
            [jobs.movement.go-to.tuning :as tuning :refer [max-blocked]]
            [jobs.movement.go-to.way-steps :as ways]))

(def max-escalations 3)

(defn skips
  "The escalation steps this call does not take: those that failed (memory :skipped-steps) and those of the ways it
  does not allow."
  [c]
  (into (:skipped-steps (ctx/mem c) #{}) (ways/barred-steps c)))

(def escalate-reasons
  "Planner reasons that prove the way needs the world changed. :start-enclosed is the search running out of land
  with no loaded edge reached (a sealed pen, the goal far and unloaded); :one-way is every nearer node lying behind a drop
  (escalating then needs the body shut in, escalate?)."
  #{:exhausted :goal-enclosed :goal-cut-off :start-enclosed :one-way})

(defn walker-fault?
  "Whether a walk result is the walker's fault: stuck, off its plan (not a partial plan's end), a failed steer, or a
  found plan walked to its end no nearer. Once the walks round it are spent it escalates as a planner reason does, for
  a free body too: go-to arrives unless truly impossible."
  [{:keys [status partial]}]
  (boolean (or (#{:stuck :failed :arrived} status) (and (= :off-plan status) (not partial)))))

(defn sprint-only?
  "Whether a walk result is the planner's only way needing a sprint the body lacks the food for (:abilities of kind
  :corner-jump or :gap-sprint): it cannot walk that way, so it digs or pillars out like a shut-in body."
  [result]
  (and (= :abilities (some-> (:reason result) keyword))
       (contains? #{:corner-jump :gap-sprint} (some-> (:kind result) keyword))))

(defn escalate-reason?
  "Whether a walk result's reason proves the way needs the world changed (escalate-reasons), or is the planner's way
  through a door that :doors :never refuses (:abilities of kind :open): such a door is a wall."
  [result doors]
  (let [reason (some-> (:reason result) keyword)]
    (boolean (or (contains? escalate-reasons reason)
                 (and (= :abilities reason) (= :never doors) (= :open (some-> (:kind result) keyword)))))))

(defn dive?
  "Whether a give-up with result is a way that dives (:way-dives) that go-to may climb out of by a stair (c :dive-stair,
  the setting)."
  [c result]
  (boolean (and (:dive-stair c) (ways/way? c :stair) (= :way-dives (some-> (:reason result) keyword)))))

(defn shelter-exit?
  "Whether a give-up with result may leave the body's own shelter instead: the walk proved the way needs the world
  changed, the body is shut in its shelter and it is not night (dig-in-leave/leave-unsafe)."
  [c result]
  (boolean (and (not (contains? (skips c) :leave-shelter))
                (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result))
                (sh/sheltered-in c)
                (nil? (dig-leave/leave-unsafe (:primitives c))))))

(defn fetch-tool?
  "Whether the escalation may fetch a pickaxe (stair :fetch, a :dig-route dig's): the body is free (a shut-in body
  cannot walk to one; by-hand is its fallback) and carries one pickaxe or none, so the last one breaking is no stop."
  [c]
  (let [p (:primitives c)]
    (and (not (escape/enclosed? p (some-> (:doors (:args c)) keyword)))
         (<= (count (filter #(str/ends-with? (str (:name %)) "_pickaxe") (u/inventory p))) 1))))

(defn escalation-job
  "[job args] of the child that carries out escalation e (jobs.lib.escape/choose); tag marks the holes it digs;
  by-hand? digs what no carried tool clears bare-handed; spare-own? keeps the children off the body's own plans;
  fetch? lets a stair fetch a pickaxe (fetch-tool?). The body's own stair cut again (:own) is marked by no tag: it
  stays open."
  [{:keys [step] :as e} tag ignore-zones? by-hand? spare-own? fetch?]
  (case step
    :pillar ['jobs.access.pillar (cond-> {:height (:height e) :fetch false :ignore-zones? ignore-zones? :checks {:safe? '(not (exposed))}} (:item e) (assoc :use {:blocks [(:item e)]}))]
    :stair ['jobs.access.stair {:dir :up :heading (:heading e) :steps (:steps e) :note (when-not (:own e) tag) :accept #{:falling-block} :fetch fetch? :spent-ok true :ignore-zones? ignore-zones? :need-drop (not by-hand?) :spare-own-builds spare-own?}]
    :clear-path ['jobs.access.clear-path {:heading (:heading e) :note tag :ignore-zones? ignore-zones? :need-drop (not by-hand?) :spare-own-builds spare-own? :fetch false}]
    :approach ['jobs.movement.go-to {:pos (zipmap [:x :y :z] (:pos e)) :range 0 :ways #{:walk :clear-seal}}]
    :fill-gap ['jobs.blocks.place {:pos (zipmap [:x :y :z] (:cell e)) :use {:blocks [(:item e)]} :fetch false :ignore-zones? ignore-zones?}]))

(defn succeeded? [{:keys [step]} e]
  (and (= :done (:kind e))
       (case step
         :approach (true? (:arrived (:data e)))
         :fill-gap (boolean (:placed (:data e)))
         true)))

(defn stop-detail
  "What a child's ending e says of why it stopped, as an escalation detail: the cause's fields, its :reason (the
  cause's key) and the child's own result fields that name the place or the wait."
  [e]
  (merge (when (map? (:cause e)) (:cause e))
         (select-keys (:data e) [:why :cell :block :at :short :detail :wait])
         {:reason (contract/cause-key e)}))

(defn spare-own?
  "Whether go-to keeps off the plans this body made (arg :spare-own-builds, default true)."
  [c]
  (not (false? (:spare-own-builds (:args c)))))

(defn own-plan-fn
  "(own-plan cell): the id of the plan this body made that covers cell, else nil."
  [c]
  (let [footprints (known/footprints c)
        own (access/own-plans c)]
    (fn [cell] (some own [(get footprints (vec cell))]))))

(defn may-dig-fn
  "(may-dig? cell): no other owner's zone, claim or plan footprint refuses a dig there (jobs.lib.tidy/refusal), and
  when spare? (default: go-to's spare-own?) no plan this body made covers it and, when pens? (default spare?), it is
  not the wall of the own pen holding livestock the body stands in (jobs.lib.own-pen)."
  ([c] (may-dig-fn c (spare-own? c)))
  ([c spare?] (may-dig-fn c spare? spare?))
  ([c spare? pens?]
   (let [own-plan (own-plan-fn c)
         pen-wall? (if pens? (own-pen/wall? c (end/feet-cell c)) (constantly false))]
     (fn [cell]
       (and (or (not spare?) (nil? (own-plan cell)))
            (not (pen-wall? cell))
            (or (boolean (:ignore-zones? (:args c))) (nil? (tidy/refusal c :dig (zipmap [:x :y :z] cell)))))))))

(def placed-policy
  "Body memory policy of :escalation-placed, the cells go-to put a block back in: later escalations may dig them."
  {:cap 100 :ttl (* 6 60 60 1000)})

(defn own-placed-fn
  "(own? cell block): go-to itself placed block at cell (a put-back), or the body's scaffold ledger holds it there
  placed (a pillar, a plug: its own temporary block; an :intent proves nothing), so it is not someone's build."
  [c]
  (let [placed (into (set (map (comp (juxt :cell :block) :data) (ctx/entries c :escalation-placed)))
                     (comp (filter (comp #{:placed :removing} :state)) (map (juxt :cell :item)))
                     (ledger/open-entries (ctx/view c)))]
    (fn [cell block] (contains? placed [(vec cell) block]))))

(defn forget-dug-own!
  "Drop from the scaffold ledger each entry of dug ({:cell :block}) whose block it was and whose cell is air now: a
  block built there later is someone else's."
  [c dug]
  (let [p (:primitives c)
        l (ledger/open-entries (ctx/view c))
        gone (set (for [{:keys [cell block]} dug
                        :let [e (ledger/entry-at l (vec cell))]
                        :when (and e (= block (:item e)) (b/air (u/seen-name p (zipmap [:x :y :z] cell))))]
                    (vec cell)))]
    (when (seq gone)
      (ledger/remember! c (remove (comp gone :cell) l)))))

(defn note-placed! [c cell]
  (when-let [block (u/seen-name (:primitives c) (zipmap [:x :y :z] cell))]
    (ctx/remember! c :escalation-placed {:cell (vec cell) :block block} placed-policy)))

(defn gap-fill
  "{:step :fill-gap :cell :item} when result (a planner no-path) is a gap jump refused for a low ceiling, a carried
  pillar block can fill the gap's floor, and no zone refuses the place; else nil."
  [c result skip]
  (let [p (:primitives c)
        {:keys [item]} (escape/pillar-item p)
        cell (when (and (= :abilities (some-> (:reason result) keyword)) (= :gap-low-ceiling (some-> (:kind result) keyword))
                        (not (skip :fill-gap)) item (:at result) (:from result))
               (escape/gap-floor (:at result) (:from result)))]
    (when (and cell (or (:ignore-zones? (:args c)) (nil? (tidy/refusal c :place (zipmap [:x :y :z] cell)))))
      {:step :fill-gap :cell cell :item item})))

(defn planned-cells
  "The solid cells escalation e from feet would cut, [{:cell :block}]: the stair's cuts or clear-path's door, those
  the body has sensed (the child notes what it digs of the rest)."
  [p {:keys [step heading steps]} feet]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        cells (case step
                :stair (escape/stair-cuts feet (escape/heading-dirs heading) steps)
                :clear-path (:cells (escape/door block-at unseen? feet (escape/heading-dirs heading) (escape/max-door)))
                nil)]
    (vec (for [cell cells :let [n (block-at cell)] :when (and (escape/solid? n) (not (unseen? cell)))] {:cell cell :block n}))))

(defn own-holes
  "The ledger entries (jobs.lib.tidy :tidy, body memory) of cells this go-to's escalations dug, lowest first."
  [c]
  (->> (tidy/entries c)
       (filter #(and (:escalation %) (= (:root c) (:job %)) (= (:id c) (:go-to %))))
       (sort-by (comp second :cell))
       vec))

(defn hole-tag
  "What marks a hole as this go-to's in the tidy ledger; its stair and clear-path children write it (:note) as they dig."
  [c]
  {:escalation true :go-to (:id c)})

(defn note-holes!
  "Write every cell of dug ({:cell :block}) that is air now and not yet noted to the ledger, as a :dig entry with
  :escalation true and :any-of, the items that put it back (the block or what it drops). Body memory outlives the
  round, a cut, a cancel and a restart: jobs.survival.restore-broken puts back what go-to did not."
  [c dug]
  (let [own-plan (own-plan-fn c)]
    (doseq [{:keys [cell block]} (distinct dug)]
      (when-let [plan (own-plan cell)]
        (ctx/warn-once! c [:dug-own cell] :go-to.dug-own-build
                        {:cell (vec cell) :block block :plan plan :text (str "dug " block " at " (pr-str (vec cell)) " in its own plan " plan)}))
      (escape/note-hole! c (hole-tag c) cell block))))

(defn ^:async look-level!
  "Look at each cell beside the body at level lvl above feet that it has not sensed."
  [c feet lvl]
  (loop [todo (for [d escape/cardinals] (escape/up (escape/ahead feet d 1) lvl))]
    (when-let [cell (first todo)]
      (when (dig-look/unknown? (:primitives c) cell) (await (dig-look/look-at! c cell)))
      (recur (rest todo)))))

(defn ^:async look-round!
  "Before choosing: look at each cell beside the body (the first look-levels levels up, then on while the level below
  reads walled, as far as pit-depth reads) and over its head that it has not sensed, so the choice reads walls it
  sees; a look shows the cells round the one looked at too. Cells inside rock stay unknown (escape reads them as
  rock)."
  [c feet]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)
        top (inc (escape/max-depth))
        over (escape/up feet 2)
        [fx fy fz] feet]
    (await (dig-look/look-unknown! c (step-off/floors {:x fx :y fy :z fz} 2)))
    (when (dig-look/unknown? p over) (await (dig-look/look-at! c over)))
    (loop [lvl 0]
      (when (< lvl top)
        (await (look-level! c feet lvl))
        (when (or (< lvl (dec escape/look-levels)) (escape/walled-at? block-at (escape/unseen-of p) (escape/up feet lvl)))
          (recur (inc lvl)))))))

(defn count-escalation
  "The count toward max-escalations of an escalation starting d from the goal, given memory m: {:fruitless n
  :escalation-best b}. One that starts more than 1 block nearer than any before (b, the least so far) counts as the
  first again: a stair that climbed is a way made, and a deep climb takes more than max-escalations of them. On a
  climb away from a dive (memory :dive-climb) d is the height still to gain: its stairs may lead off sideways."
  [m d]
  (let [best (:escalation-best m)
        nearer? (and best (< d (dec best)))]
    {:fruitless (inc (if nearer? 0 (:fruitless m 0)))
     :escalation-best (min d (or best d))}))

(defn own-stair
  "The body's own stair cut again (climb/recut) as the way out for a body standing on it under goal: what stands in its
  walk space (a cap the body put there) is cleared before any other way is cut, its floors are not. nil once it failed
  this call (memory :own-way-done) and on a climb away from a dive (climb/own-route chose the way there)."
  [c feet goal]
  (when (and (> (second goal) (second feet)) (ways/way? c :stair) (not-any? (ctx/mem c) [:own-way-done :dive-climb]))
    (climb/recut feet (ledger/ways-of c))))

(defn goal-walled?
  "Whether a give-up with result is a free body's (not escape/enclosed?) whose goal the planner proved walled in: the
  ways that change the world run toward the goal (goal-way)."
  [c result]
  (and (= :goal-enclosed (some-> (:reason result) keyword))
       (not (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword)))))

(defn route-cells
  "[{:cell :block :guess}]: the cheapest cells to dig for a walk from feet cell from down or across into goal
  (jobs.lib.dig-route priced by jobs.lib.cost: a block's walk and dig-cost; at most the dig-route settings' digs and fall),
  in walking order, every one escape/diggable?, :guess on a cell not seen (read as rock); [] when nothing is left to
  dig, nil when no such route."
  [p from goal may-dig? own?]
  (let [block-at (escape/block-at-of p)
        ;; a floor never seen under a cell seen open may be a void: dig-route reads nil as no floor
        seen-floor (fn [[x y z :as cell]]
                     (when-not (and (dig-look/unknown? p cell) (dig-route/open? (block-at [x (inc y) z]))
                                    (not (dig-look/unknown? p [x (inc y) z])))
                       (block-at cell)))
        cells (:dig (dig-route/route seen-floor from goal {:dig-s cost/dig-cost
                                                         :step-s (:cost (cost/walk-cost {:distance 1}))}))]
    (when (and cells (every? #(escape/diggable? block-at may-dig? own? %) cells))
      (mapv (fn [cell] {:cell cell :block (block-at cell) :guess (dig-look/unknown? p cell)}) cells))))

(defn dig-way
  "{:step :dig-route :cells (route-cells)} when there are cells to dig, else nil."
  [p from goal may-dig? {:keys [own? skip]}]
  (when-not (skip :dig-route)
    (when-let [cells (seq (route-cells p from goal may-dig? own?))]
      {:step :dig-route :cells (vec cells)})))

(defn nearest-stand
  "The cell [x y z] nearest goal that the body can walk to (a flood of at most reach/room-cells cells), of equally near
  ones the first the flood reaches (the shortest walk)."
  [p goal]
  (let [kind-at (reach/lookup p reach/body-kind-of)
        to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))]
    (->> (:cells (reach/flood-visit #(reach/forward kind-at %1 %2 %3) (reach/standing-vec p) reach/room-cells))
         (map vec)
         (reduce (fn [a b] (if (< (to-goal b) (to-goal a)) b a))))))

(defn goal-way
  "A free body's way into a walled-in goal: escape/choose from where it stands, or, while the cell it can walk to
  nearest the goal (nearest-stand) is more than a block nearer, from there, as
  {:step :approach :pos cell} first. Only a door, or a stair or pillar up to a higher goal, leads in (a walk along
  the wall too, from where it stands, to a goal not below it), else the fewest cells dug (dig-way); else {:step :none :why :no-dig}."
  [c feet goal may-dig? opts]
  (let [p (:primitives c)
        to-goal #(u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] goal))
        near (nearest-stand p goal)
        here? (or ((:skip opts) :approach) (>= (to-goal near) (dec (to-goal feet))))
        from (if here? feet near)
        e (escape/choose p from goal may-dig? opts)
        in? (case (:step e)
              :clear-path true
              (:stair :pillar) (> (second goal) (second from))
              :approach (and here? (>= (second goal) (second from)))
              false)
        e (cond in? e
                :else (or (dig-way p from goal may-dig? opts)
                          (if (= :none (:step e)) e {:step :none :why :no-dig})))]
    (cond
      (= :none (:step e)) e
      here? e
      :else {:step :approach :pos near})))

(defn detour-way
  "A buried body's way to a goal its only found way reaches by a long detour (walk.cljs :detour): the cheapest cells
  dug from the feet into goal pos (dig-way), else a stair when the goal is above; nil when the call may not escalate,
  tried it already (memory :detour-tried) or neither is there."
  [c pos]
  (when (and (ways/escalating? c) (not (:detour-tried (ctx/mem c))))
    (let [p (:primitives c)
          feet (end/feet-cell c)
          goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
          opts {:own? (own-placed-fn c) :skip (skips c)}
          may-dig? (may-dig-fn c)]
      (or (dig-way p feet goal may-dig? opts)
          (when (> (second goal) (second feet))
            (let [e (escape/choose p feet goal may-dig? opts)]
              (when (= :stair (:step e)) e)))))))

(defn detour-escalate!
  "Start escalation e (detour-way) instead of giving up :detour (why: the bound's {:route :straight :allowed}); a
  later give-up keeps the detour as its reason. :again."
  [c tries why e]
  (let [feet (end/feet-cell c)]
    (ctx/emit! c :go-to.escalated :info (merge why {:step (:step e) :why :detour :n 1 :at feet
                                                    :text (str "the only way round is " (:route why) " blocks for a goal " (:straight why)
                                                               " blocks off: " (name (:step e)) " instead")}))
    (ctx/update-mem! c #(assoc % :detour-tried true :give-up {:tries tries :status :detour :result (merge {:status :no-path :reason :detour} why)}
                               :fruitless 1 :escalations (inc (:escalations % 0)) :escalation e :escalation-from feet
                               :planned (planned-cells (:primitives c) e feet) :holes-before (count (own-holes c))))
    :again))

(defn ^:async escalate!
  "Start the next escalation for the give-up kept in memory (:give-up), or give up with it when there is none (or
  with failed, the detail of the escalation that just failed, when there is none left after it; past max-escalations
  with the last one that failed, if any). A climb begun because a walkable plan went down through caves (memory
  :cave-climb) walks that plan after all instead of giving up (memory :cave-ok). A detour's escalation that failed
  (detour-escalate!) gives the detour up at once."
  [c pos failed]
  (let [{:keys [tries status result]} (:give-up (ctx/mem c))
        feet (end/feet-cell c)
        counted (count-escalation (ctx/mem c) (if (:dive-climb (ctx/mem c))
                                                (- (:y pos) (second feet))
                                                (u/dist (zipmap [:x :y :z] feet) pos)))
        n (:fruitless counted)
        _ (when (<= n max-escalations) (await (look-round! c feet)))
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        opts {:own? (own-placed-fn c) :skip (skips c)}
        into? (goal-walled? c result)
        ;; a shut-in body (a pit, a plateau it cannot walk off) whose goal is walled in digs straight into it when it can
        dig-in? (and (not into?) (= :goal-enclosed (some-> (:reason result) keyword)))
        choose (cond into? #(goal-way c feet goal % opts)
                     dig-in? #(or (dig-way (:primitives c) feet goal % opts) (escape/choose (:primitives c) feet goal % opts))
                     :else #(escape/choose (:primitives c) feet goal % opts))
        e (or (when (shelter-exit? c result) {:step :leave-shelter})
              (gap-fill c result (:skip opts))
              (own-stair c feet goal)
              (choose (may-dig-fn c)))
        choosable? #(not= :none (:step (choose %)))
        e (cond
            (not= :none (:step e)) e
            (and (spare-own? c) (choosable? (may-dig-fn c false)))
            {:step :none :why (if (choosable? (may-dig-fn c true false)) :own-pen :own-build)}
            (choosable? (constantly true)) {:step :none :why :zone}
            :else e)]
    (cond
      (and (:cave-climb (ctx/mem c)) (or (> n max-escalations) (= :none (:step e))))
      (do (ctx/update-mem! c #(-> % (dissoc :cave-climb :dive-climb :give-up) (assoc :cave-ok true)))
          :again)
      (and failed (= :detour status)) (end/give-up! c pos tries status result (merge (select-keys result [:route :straight :allowed]) {:escalation failed}))
      (> n max-escalations) (end/give-up! c pos tries status result (merge (select-keys result [:route :straight :allowed]) {:escalation (cond-> (or failed (:last-failed (ctx/mem c)) {:step :spent :n max-escalations}) into? (assoc :into true))}))
      (= :none (:step e)) (end/give-up! c pos tries status result (merge (select-keys result [:route :straight :allowed]) {:escalation (cond-> (or failed e) into? (assoc :into true))}))
      :else
      (do (ctx/emit! c :go-to.escalated :info {:step (:step e) :why (if (:own e) :own-way-blocked (:why (end/give-up-fields result))) :n n :at feet
                                               :text (if (:own e) "its own stair up is blocked: cutting it again" (str "no way " (if (or into? (= :dig-route (:step e))) "in" "out") " on foot: " (name (:step e))))})
          (ctx/update-mem! c #(-> (merge % counted)
                                  (assoc :escalations (inc (:escalations % 0)) :escalation e :escalation-from feet
                                         :planned (planned-cells (:primitives c) e feet) :holes-before (count (own-holes c)))))
          :again))))

(def stair-step-s
  "Seconds one step of a stair up is reckoned to take (two cuts and a step), against a found plan's seconds."
  3)

(def stair-first-rise
  "How far above the feet a goal is for a found plan to be weighed against a stair up."
  8)

(defn stair-check
  "A fn of a plan and whether it is the first of its round, saying why it is not walked and a stair up is cut instead (a
  re-plan inside a walk is held to :dives only), when escalating is on, the body is not in its shelter and escape/choose would stair (steps n):
  - :dives: under a cave floor (c :cave-floor, with c :dive-stair) the plan goes below it on a cell that is no stair of
    the body's own (climb/dives?), or below c :own-floor back down a stair of its own (climb/returns?);
  - :cheaper: it is a whole path costing more seconds than 3 such stairs, the goal is stair-first-rise or more above
    the feet and the stair has not been tried this call.
  Else nil."
  [c pos]
  (let [m (ctx/mem c)
        feet (end/feet-cell c)
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        cave (when (:dive-stair c) (:cave-floor c))
        back (:own-floor c)
        cheaper? (and (not (:stair-tried m)) (>= (- (second goal) (second feet)) stair-first-rise))
        stair (delay (let [e (escape/choose (:primitives c) feet goal (may-dig-fn c) {:own? (own-placed-fn c) :skip (skips c)})]
                       (when (= :stair (:step e)) e)))]
    (when (and (ways/way? c :stair) (not (sh/sheltered-in c)) (or cave cheaper? back))
      (fn [{:keys [status steps ^js r]} first?]
        (let [seconds (some-> r .-path .-cost .-priced)]
          (cond
            (and cave (climb/dives? steps cave (ledger/ways-of c)) @stair) :dives
            (and back (climb/returns? steps back (ledger/stair-stands c)) @stair) :dives
            (and first? cheaper? (= "found" status) (number? seconds) @stair (> seconds (* 3 stair-step-s (:steps @stair)))) :cheaper))))))

(defn dive-refused!
  "A plan refused because it goes down through caves (stair-check :dives): climb as from a dive (memory :dive-climb,
  :cave-climb), the give-up kept being :way-dives."
  [c pos]
  (ctx/update-mem! c assoc :escalate-now true :dive-climb true :cave-climb true
                   :give-up {:tries 0 :status :no-path
                             :result {:status :no-path :reason :way-dives :dip (climb/cave-dip)
                                      :rise (js/Math.round (- (:y pos) (:y (u/self-pos c))))}})
  :again)

(defn own-recut!
  "Cut the body's own stair again from where it stands on it (climb/recut over ledger/ways-of), as the escalation, after
  a go-to.escalated {:step :stair :why :own-way-blocked}: :again, nil when no stair of its own leads up from there."
  [c]
  (let [feet (end/feet-cell c)]
    (when-let [e (and (ways/way? c :stair) (climb/recut feet (ledger/ways-of c)))]
      (ctx/emit! c :go-to.escalated :info {:step :stair :why :own-way-blocked :at feet :heading (:heading e) :steps (:steps e)
                                           :text "its own stair up is blocked: cutting it again"})
      (ctx/update-mem! c assoc :escalation e :escalation-from feet :planned [] :holes-before (count (own-holes c)))
      :again)))

(defn own-failed!
  "The cut of the body's own stair stopped where it began: no more of its own way this call (memory :own-way-done),
  walk again, after a go-to.escalation-stopped warn."
  [c {:keys [step] :as detail}]
  (ctx/emit! c :go-to.escalation-stopped :warn (assoc detail :text (str "its own " (name step) " could not be cut again: "
                                                                        (some-> (:reason detail) name) "; walking again")))
  (ctx/update-mem! c #(-> % (dissoc :escalation :escalation-from :planned :holes-before) (assoc :own-way-done true)))
  :again)

(defn escalate?
  "Whether a give-up after tries fruitless walks with result may escalate instead: a carried block fills the 1-cell gap it was refused (gap-fill), its only way dives (dive?), the goal is walled in (goal-walled?), or the walker failed max-blocked walks running (walker-fault?; one stuck walk of a :retry false caller is no proof) or the body is shut in (reach/enclosed?), and no door or
  gate borders where it can walk (escape/door-beside?: that is its way out; an iron door, or any door under :doors :never,
  is a wall). Never out of
  the body's own shelter: it is shut in on purpose."
  [c tries result]
  (and (ways/escalating? c)
       (or (shelter-exit? c result) (not (sh/sheltered-in c)))
       (or (shelter-exit? c result)
           (some? (gap-fill c result (skips c)))
           (sprint-only? result)
           (dive? c result)
           (goal-walled? c result)
           (let [doors (some-> (:doors (:args c)) keyword)]
             (and (or (and (walker-fault? result) (>= tries max-blocked))
                      (and (escalate-reason? result doors) (escape/enclosed? (:primitives c) doors)))
                  (not (escape/door-beside? (:primitives c) (end/feet-cell c) doors)))))))

(defn ^:async give-up-or-escalate! [c pos tries status result]
  (if (escalate? c tries result)
    (do (ctx/update-mem! c assoc :dive-climb (dive? c result)
                         :give-up {:tries tries :status status
                                   :result (select-keys result [:status :reason :kind :cells :near :one-way :at :from :dip :rise :move :why :step :partial])})
        (await (escalate! c pos nil)))
    (end/give-up! c pos tries status result
                  (when (and (not (sh/sheltered-in c))
                             (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result))
                             (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword)))
                    {:shut-in true}))))

(defn climb-on?
  "Whether a stair that is done leaves the body still shut in (escape/enclosed?) under the goal pos: a walk could only
  lead back down the stair (live: a partial walk toward the goal's column took the body back under it), so the next
  escalation follows at once."
  [c pos]
  (and (< (second (end/feet-cell c)) (js/Math.floor (:y pos)))
       (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword))))

(defn dive-climb-on?
  "Whether a stair cut because the only way dives (memory :dive-climb) leaves the body still under ground below the
  goal pos: a walk would lead back down the stair toward the dive, so the next stair follows at once."
  [c pos]
  (boolean (and (:dive-climb (ctx/mem c))
                (< (second (end/feet-cell c)) (js/Math.floor (:y pos)))
                (sh/buried? (:primitives c)))))

(defn escalated!
  "The child made a way (or part of one, done false): walk again from a fresh count, or escalate again at once after an
  approach, after a whole stair that left the body shut in under the goal (climb-on?), or after any stair that left it
  under ground on its climb away from a dive (dive-climb-on?). Its holes are put back once a walk round gets on past
  it (restore-next!)."
  [c pos {:keys [step]} done?]
  (let [again? (or (= :approach step) (and (= :stair step) (or (and done? (climb-on? c pos)) (dive-climb-on? c pos))))]
    (ctx/update-mem! c #(cond-> (-> % (dissoc :escalation :escalation-from :planned :holes-before :best)
                                    (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {}))
                          again? (assoc :escalate-now true)
                          (not (#{:approach :fill-gap} step)) (assoc :restore-pending true))))
  :again)

(defn partly-made?
  "Whether the escalation changed something before it stopped: it dug a cell or moved the body. Then walking again
  may get through (a stair stopped short of a stone step may already reach the rim)."
  [c]
  (let [m (ctx/mem c)]
    (or (> (count (own-holes c)) (:holes-before m 0))
        (not= (:escalation-from m) (end/feet-cell c)))))

(defn skip-key
  "What a failed escalation e rules out: a stair only its heading (the other headings may be open), else its step."
  [{:keys [step heading]}]
  (if (= :stair step) [:stair heading] step))

(defn no-tool?
  "Whether an escalation's stop or wait (detail) is for want of a tool: its own :no-tool, or its dig child's."
  [detail]
  (or (= :no-tool (:reason detail)) (= :no-tool (:reason (:wait detail)))))

(defn by-hand!
  "From now on this go-to digs what no carried tool clears bare-handed (memory :by-hand): the last resort before giving
  up, as go-to fetches no tool. A go-to.by-hand warn says so once."
  [c {:keys [step] :as detail}]
  (ctx/update-mem! c assoc :by-hand true)
  (let [block (or (:block detail) (:block (:wait detail)))]
    (ctx/emit! c :go-to.by-hand :warn {:step step :block block
                                       :text (str "no tool for " (or block "the wall") ": digging the " (name step)
                                                  " by hand (slow, the drops are lost)")})))

(defn escalation-stopped!
  "The child changed something before it stopped: walk again (escalated!) after a go-to.escalation-stopped warn. On a
  climb away from a dive (dive-climb-on?) the next stair takes another heading instead."
  [c pos {:keys [step] :as detail}]
  (ctx/emit! c :go-to.escalation-stopped :warn (assoc detail :text (str (name step) " stopped part way: "
                                                                        (some-> (:reason detail) name)
                                                                        (if (dive-climb-on? c pos) "; climbing on another way" "; walking again"))))
  (when (dive-climb-on? c pos)
    (ctx/update-mem! c update :skipped-steps (fnil conj #{}) (skip-key (:escalation (ctx/mem c)))))
  (escalated! c pos detail false))

(defn ^:async escalation-next!
  "The child changed nothing: try the next method (escalate!, never the same step twice, at most max-escalations in
  all) after a go-to.escalation-stopped warn, giving up with detail when none is left."
  [c pos {:keys [step] :as detail}]
  (ctx/emit! c :go-to.escalation-stopped :warn (assoc detail :text (str (name step) " failed: "
                                                                        (some-> (:reason detail) name)
                                                                        "; trying another way")))
  (ctx/update-mem! c #(cond-> (-> % (update :skipped-steps (fnil conj #{}) (skip-key (:escalation (ctx/mem c))))
                                  (assoc :last-failed detail))
                        (= :stair step) (update :fruitless dec)))
  (await (escalate! c pos detail)))

(defn other-heading?
  "Whether escalation e is a stair (not the body's own) and escape/choose finds another way with its heading skipped
  that cuts no block (the one it had no tool for)."
  [c pos {:keys [step own] :as e} block]
  (and (= :stair step) (not own)
       (let [p (:primitives c)
             goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
             block-at (escape/block-at-of p)
             may-dig? (may-dig-fn c)]
         (not= :none (:step (escape/choose p (end/feet-cell c) goal #(and (may-dig? %) (not= block (block-at %)))
                                           {:own? (own-placed-fn c) :skip (conj (skips c) (skip-key e))}))))))

(defn ^:async escalation-failed!
  "The child stopped or would wait: when it changed something, walk again (escalation-stopped!); when it had no tool
  and has not dug by hand yet, take another stair heading clear of that block when there is one (other-heading?), else
  run it again by hand (by-hand!); else try the next method (escalation-next!), or after the body's own stair walk
  again (own-failed!)."
  [c pos detail]
  (let [e (:escalation (ctx/mem c))
        no-tool (and (no-tool? detail) (not (:by-hand (ctx/mem c))))
        other? (and no-tool (other-heading? c pos e (or (:block detail) (:block (:wait detail)))))
        hand? (and no-tool (not other?))]
    (when other? (ctx/update-mem! c update :skipped-steps (fnil conj #{}) (skip-key e)))
    (when hand? (by-hand! c detail))
    (cond
      (partly-made? c) (escalation-stopped! c pos detail)
      hand? :again
      (:own (:escalation (ctx/mem c))) (own-failed! c detail)
      :else (await (escalation-next! c pos detail)))))

(defn ^:async leave-shelter-round!
  "One round of leaving the body's own shelter (dig-in-leave/leave!) toward the goal."
  [c pos]
  (let [e (:escalation (ctx/mem c))
        r (await (dig-leave/leave! c {:toward pos}))]
    (cond
      (= :continue r) :continue
      (= :done (:status r)) (do (ctx/update-mem! c update :skipped-steps (fnil conj #{}) :leave-shelter)
                                (escalated! c pos e true))
      :else (escalation-failed! c pos {:step :leave-shelter :reason (:reason r) :why (:why r)}))))

(defn ^:async escalation-child-round!
  "One call of the escalation child (ctx/call): the whole pillar, stair or door. After it, every planned cell now dug
  is in the ledger (note-holes!). A child that ends other than done gives up, with its cause, unless it got part of
  the way."
  [c pos]
  (let [{:keys [step] :as e} (:escalation (ctx/mem c))
        [job args] (escalation-job e (hole-tag c) (boolean (:ignore-zones? (:args c))) (boolean (:by-hand (ctx/mem c))) (spare-own? c) (fetch-tool? c))
        _ (await (torch/return-torch! c))
        r (await (ctx/call c :escalation job (update args :checks #(some->> % (ready/unforced c)))))]
    (let [dug (concat (:planned (ctx/mem c)) (when (not= :continue r) (:dug (:data r))))]
      (forget-dug-own! c dug)
      (when-not (:own e) (note-holes! c dug)))
    (cond
      (= :continue r) :continue
      (succeeded? e r) (escalated! c pos e true)
      :else (escalation-failed! c pos (merge {:step step} (stop-detail r))))))

(defn ^:async route-cell!
  "Dig one cell of a :dig-route escalation: look first at a cell the body has not sensed, then check it again as it
  is now (escape/protected?, escape/diggable? over may-dig-fn and own-placed-fn: a block built there since the route
  was planned is someone's) and dig it (b/dig-cell!: no tool needed, the drops left), noting it as a hole once dug.
  The dig's outcome, :open for a cell open already, :replan for a guessed cell the look shows otherwise (the route
  was planned over rock that is not there), or :not-diggable."
  [c {:keys [cell guess block] :as planned}]
  (let [p (:primitives c)]
    (when (dig-look/unknown? p cell) (await (dig-look/look-at! c cell)))
    (let [block-at (escape/block-at-of p)
          n (block-at cell)]
      (cond
        (and guess (not (dig-look/unknown? p cell)) (not= n block)) :replan
        (escape/passable? n) :open
        (or (escape/protected? n) (not (escape/diggable? block-at (may-dig-fn c) (own-placed-fn c) cell))) :not-diggable
        :else
        (let [o (await (b/dig-cell! c (zipmap [:x :y :z] cell) {:ignore-zones? (boolean (:ignore-zones? (:args c))) :fetch (fetch-tool? c)}))
              dug (assoc planned :block n)]
          (when (= :dug o)
            (forget-dug-own! c [dug])
            (note-holes! c [dug]))
          o)))))

(defn ^:async replan-route!
  "Plan the :dig-route escalation again from the feet into goal pos with what the body sees now (route-cells): :again,
  the walk on (escalated!) when nothing is left to dig, or the escalation fails :no-route when no route is left or it
  planned tuning/max-route-replans times."
  [c pos cell]
  (let [m (ctx/mem c)
        n (inc (:route-replans m 0))
        cells (when (<= n (tuning/max-route-replans))
                (route-cells (:primitives c) (end/feet-cell c) (mapv #(js/Math.floor (% pos)) [:x :y :z]) (may-dig-fn c) (own-placed-fn c)))]
    (cond
      (nil? cells) (await (escalation-failed! c pos {:step :dig-route :reason :no-route :cell cell}))
      (empty? cells) (await (escalated! c pos (:escalation m) true))
      :else (do (ctx/update-mem! c #(-> % (assoc-in [:escalation :cells] cells) (assoc :route-replans n))) :again))))

(defn ^:async dig-route-round!
  "One round of digging a :dig-route escalation's cells in walking order (route-cell!); a cell already open is passed,
  so a resumed round goes on where it stopped; a guessed cell seen otherwise plans the route again (replan-route!).
  A cell that may not or could not be dug ends it (escalation-failed!)."
  [c pos]
  (let [{:keys [cells] :as e} (:escalation (ctx/mem c))]
    (loop [todo cells]
      (if-let [{:keys [cell] :as planned} (first todo)]
        (let [o (await (route-cell! c planned))]
          (when (= :dug o)
            (ctx/update-mem! c update-in [:escalation :cells] (partial mapv #(cond-> % (= cell (:cell %)) (assoc :guess false)))))
          (cond
            (= :continue o) :continue
            (= :replan o) (await (replan-route! c pos cell))
            (#{:dug :missing :open} o) (recur (rest todo))
            :else (await (escalation-failed! c pos (cond-> {:step :dig-route :reason o :cell cell}
                                                     (= :not-diggable o) (assoc :block ((escape/block-at-of (:primitives c)) cell)))))))
        (escalated! c pos e true)))))

(defn ^:async escalation-round!
  "One round of the escalation: leaving the shelter, digging a route, or the child's call (escalation-child-round!)."
  [c pos]
  (case (:step (:escalation (ctx/mem c)))
    :leave-shelter (leave-shelter-round! c pos)
    :dig-route (dig-route-round! c pos)
    (escalation-child-round! c pos)))
