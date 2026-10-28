(ns jobs.movement.go-to.escalation
  "go-to's way out of a shut-in body, or in to a walled-in goal: run the way escalate-choose picked (pillar, stair,
  clear-path, dig route or a walk to the wall), note the holes it dug, put them back once the walk is on past them.

  Rules of go-to:
  - A goal that is lava, stands over lava or has lava on all four sides (escalate-choose/goal-lava?) is refused at once when the walk finds it walled in: no way is made, in or out; the dig rules below are for an acceptable goal that is walled in.
  - A goal cut off by a drop (:goal-cut-off, or :goal-enclosed with a drop edge in its region) is bridged: a carried block fills the floor beyond the drop edge nearest the body that it can step onto (escalate-choose/drop-fill, gap-fill; a go-to.escalated {:step :fill-gap}) when that is no dearer than the stair, pillar or dig the escape would make (jobs.lib.cost.ways/fill-cost: placing, the block spent, a fetch when the pack runs low), then it walks again.
  - :dig-out: a body sealed in its own shelter (a :shelter entry) when the walk finds no way, leaves it first at any hour (dig-in-leave/leave!, a go-to.escalated {:step :leave-shelter}); the start check priced the way out. Not at a dark sky while the body is :dug-in (sh-inside/night-sheltered): it waits for light, as the readiness checks do.
  - With an escalating way: a body not in its own shelter whose search ran out of land (:exhausted, in any size of cave or mine), or that is shut in (jobs.lib.reach.room/enclosed?) and whose
    search ran out of land (:goal-enclosed or :goal-cut-off) or of nodes (:budget) within the budget-near setting of the goal, any body whose only way needs a sprint it lacks the food for (:abilities :corner-jump or :gap-sprint), or any body whose last 3 walks failed at the walker (stuck, off-plan, steer failed, no nearer; escalation/walker-fault?) with no door it can open beside its room (one with room for the body behind it; a trunk over that cell makes it no way out, and the dig route cuts the tree's logs: escape/tree-log?), makes a way instead of giving up, one child job
    each (jobs.lib.escape/choose, over the ways the call allows), at most 3 in a row that start no nearer the goal than one before (escalation/count-escalation; a dig route is taken past them): standing on a stair of its own under the goal, that stair cut again when something shuts it (escalation/own-stair: a cap it put in the stair is dug, :why :own-way-blocked); else, shut in under a higher goal, the cheapest climb there (climb-route: one dig route of digs, laid stair floors and pillar blocks, never through a floor of its own stairs), its stairs, pillars and dig routes run back to back with no walk between; else jobs.access.pillar up out of a pit when it carries enough blocks; else
    jobs.access.clear-path through a wall up to 3 thick toward the goal (into a walled-in goal no door or stair reaches:
    the fewest cells dug down or across to it, escalation/dig-way, tried first for a shut-in body, none that takes over max-route-s seconds to dig with the pickaxe it carries or would fetch; for a shut-in body with no stair, pillar or door out, a dig down to the seen stand beyond its land's edge nearest the goal, else a walk to that edge to look over it (escalate-choose/frontier-way); a gap jump the walker refuses with no block to fill it counts as a walled-in goal (escalate-choose/gap-walled?): a step up over a one-cell gap cuts the riser beyond it (gap-dig), else the body digs toward the goal; a leg of it (escalate-choose/dig-leg) is tried first when the search of a body under ground ran out of land over rock it guessed (jobs.lib.walk.world/rock-guess?) and the goal is no higher than it is across); else jobs.access.stair up out of a pit or
    toward a higher goal (as far as the goal is above, at most 8; a stair that leaves the body shut in under the goal is
    followed by the next escalation at once); else a walk to a wall first; when no way can be made where the body stands, a walk to a cell it can stand on from which one can that leads toward the goal (escape/way-spot: a body in its hut beside the doorway, whose walls it spares); each such cell the cheapest walk there and on to the goal (escape/priced-spot). Each emits go-to.escalated {:step :why :n}. Once
    through, it puts back what clear-path or stair dug in another's zone, claim or plan (with :ignore-zones?;
    its own diggings elsewhere stay) (jobs.blocks.place, the dug block or its drop, when carried): info
    go-to.restored, warn go-to.restore-skipped for cells it could not. Pillar blocks and a stair's placed floors are
    left to jobs.access.cleanup. Every way spares the body's own builds (a plan it made, the wall of its own pen holding
    livestock), unless the way through them is the only one or costs less with each own cell's put-back (and a stocked
    pen's escape risk) on top (own-way); those cells are put back at once (hole-tag :mend). Its walk child (ctx/call) has :ways #{:walk :clear-seal}. Jobs that must not change the world
    on the way pass :ways #{:walk}: a shut-in body then gives up with :shut-in true and says so in the text."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.need :as need]
            [jobs.lib.ready :as ready]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.trip :as trip]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.access :as access]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.movement.go-to.climb :as climb]
            [jobs.movement.go-to.climb-route :as climb-route]
            [jobs.movement.go-to.route-ways :as route-ways]
            [jobs.movement.go-to.escalate-choose :refer [cheaper-fill climb-on? dive-climb-on? dive? escalate-reason? escalate? fetch-tool? gap-fill
                                                         goal-way dig-leg dig-way drop-fill price frontier-way gap-dig gap-walled? goal-lava? leg-ends goal-walled? own-stair route-cells shelter-exit? skips sprint-only?
                                                         route-owner route-slice routes-searching! searching? searching-ends keep-routes! forget-routes! search-opts]]
            [jobs.movement.go-to.holes :refer [forget-dug-own! hole-tag may-dig-fn note-holes! own-cut-fn own-fn own-holes own-placed-fn planned-cells with-own-pen]]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.search-bound :as search-bound]
            [jobs.movement.go-to.torch :as torch]
            [jobs.movement.go-to.tuning :as tuning]
            [jobs.movement.go-to.way-steps :as ways]))

(defn ways-for "The :ways of an access child that may fetch a pickaxe when fetch? is true." [fetch?]
  (if fetch? #{:carried :chest :craft :gather} #{:carried}))

(defn escalation-job
  "[job args] of the child that carries out escalation e (jobs.lib.escape/choose); tag marks the holes it digs;
  by-hand? digs what no carried tool clears bare-handed;
  fetch? lets a stair fetch a pickaxe (fetch-tool?). The body's own stair cut again (:own) is marked by no tag: it
  stays open."
  [{:keys [step] :as e} tag ignore-zones? by-hand? fetch?]
  (case step
    :pillar ['jobs.access.pillar (cond-> {:height (:height e) :ways #{:carried} :ignore-zones? ignore-zones? :checks {:safe? '(always) :fit? '(always)}}
                                   (:item e) (assoc :use {:blocks [(:item e)]})
                                   ;; a climb's pillar digs the roof its route priced
                                   (:roof e) (assoc :settings {:jobs.access.pillar/max-ceiling-digs (:roof e)}))]
    :stair ['jobs.access.stair {:dir :up :heading (:heading e) :steps (:steps e) :note (when-not (:own e) tag) :checks {:fit? '(always)} :accept #{:falling-block} :ways (ways-for fetch?) :settings (cond-> {:jobs.lib.trip/spent-uses 0} (:own-cut e) (assoc :jobs.lib.access/spare-own-builds false)) :ignore-zones? ignore-zones? :need-drop (not by-hand?) :leave-drops true}]
    :clear-path ['jobs.access.clear-path (cond-> {:checks {:fit? '(always)} :heading (:heading e) :ignore-zones? ignore-zones? :need-drop (not by-hand?) :ways #{:carried :craft}}
                                           (:own-cut e) (assoc :settings {:jobs.lib.access/spare-own-builds false}))]
    :approach ['jobs.movement.go-to {:checks {:fit? '(always)} :pos (zipmap [:x :y :z] (:pos e)) :range 0 :ways #{:walk :clear-seal}}]
    :fill-gap ['jobs.blocks.place {:pos (zipmap [:x :y :z] (:cell e)) :use {:blocks [(:item e)]} :ways #{:carried} :ignore-zones? ignore-zones?}]))

(defn succeeded? [{:keys [step]} e]
  (and (= :done (:kind e))
       (case step
         :approach (and (true? (:arrived (:data e))) (not (:re-aimed (:data e))))
         :fill-gap (boolean (:placed (:data e)))
         true)))

(defn stop-detail
  "What a child's ending e says of why it stopped, as an escalation detail: the cause's fields, its :reason (the
  cause's key) and the child's own result fields that name the place or the wait."
  [e]
  (let [reason (contract/cause-key e)
        because (:because e)]
    (merge (when (map? (:cause e)) (:cause e))
           (select-keys (:data e) [:why :cell :block :at :short :detail :wait])
           (when (and (= :dig-waits reason) because)
             {:wait (if (map? (:cause because)) (:cause because) {:reason (contract/cause-key because)})})
           {:reason reason})))

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

(defn zone-exit?
  "Whether the body stands in another's zone, claim or plan: leaving it is never refused by it."
  [c [x y z]]
  (some? (access/foreign-holder c {:x x :y y :z z})))

(defn exit-dig-fn
  "(may-dig? cell) for the way out of the holding at feet: zones are ignored only in cells that holding covers or that
  nobody holds, so a neighbouring zone, claim or plan still refuses."
  [c feet]
  (let [home (access/foreign-holder c (zipmap [:x :y :z] feet))
        free (may-dig-fn (assoc-in c [:args :ignore-zones?] true))]
    (fn [cell]
      (and (free cell)
           (let [h (access/foreign-holder c (zipmap [:x :y :z] cell))]
             (or (nil? h) (= h home)))))))

(defn dig-progress?
  "Whether dig-route escalation e ends more than a block nearer goal than best, the least any way before it started
  from: a distance, or with dive? (a climb away from a dive, whose best is a height to gain) the height still to gain."
  [e goal best dive?]
  (let [[x y z] (:cell (last (:cells e)))]
    (boolean (and x (< (if dive?
                         (- (goal 1) y)
                         (u/dist {:x x :y y :z z} (zipmap [:x :y :z] goal)))
                       (dec best))))))

(defn way-opts
  "The opts of go-to call c's choice of a way from feet (escape/choose and the dig ways): a search that stood the body
  still past its worth (search-bound, memory :spent-routes) is not begun again; a dig route prices a cell of the body's
  own builds at its put-back (:own-s)."
  [c feet fetch? slice]
  {:own? (own-placed-fn c) :skip (skips c) :spent (:spent-routes (ctx/mem c) #{}) :fetch? fetch? :own-s (own-cut-fn c)
   :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c) :stands (ledger/stair-stands c) :slice slice})

(defn own-cut?
  "Whether the escalation under way may cut the body's own builds (:own-cut, chosen by own-way)."
  [c]
  (boolean (:own-cut (:escalation (ctx/mem c)))))

(defn own-dig-fn
  "(own? cell block) of the escalation under way: own-placed-fn's, the body's own builds too when it may cut them (own-cut?)."
  [c]
  (cond->> (own-placed-fn c) (own-cut? c) (own-fn c)))

(def priced-steps #{:stair :pillar :clear-path :dig-route :bridge :fill-gap})

(defn own-cells
  "The cells of the body's own builds way e from feet cuts that it has sensed: a stair's or door's planned cells, a dig
  route's, and those of the dig routes of a climb's later ways."
  [p e feet own-s]
  (->> (concat (if (= :dig-route (:step e)) (:cells e) (planned-cells p e feet))
               (mapcat :cells (filter #(= :dig-route (:step %)) (:then e))))
       (map :cell)
       (filter #(pos? (own-s %)))))

(defn own-way
  "The way that cuts the body's own builds (own) when it beats base, the way that spares them: base is none, or a priced
  way dearer (escalate-choose/price) than own's price plus each own cell's put-back (opts :own-s, holes/own-cut-fn).
  It is marked :own-cut (its holes are put back at once, its child may dig own plans). {:step :searching} while own's
  search goes on and base is none; else nil."
  [p feet base own {:keys [own-s]}]
  (cond
    (nil? own) nil
    (= :searching (:step own)) (when (= :none (:step base)) own)
    (#{:none :approach} (:step own)) nil
    :else (let [cut-s (reduce + 0 (map own-s (own-cells p own feet own-s)))]
            (when (and (pos? cut-s)
                       (or (= :none (:step base))
                           (and (priced-steps (:step base)) (< (+ (price p feet own) cut-s) (price p feet base)))))
              (assoc own :own-cut true)))))

(defn own-may-beat?
  "Whether a way cutting the body's own builds may beat base, the way that spares them, so it is worth searching: base is
  none, or dearer (price) than the least such a way adds, one cell's put-back."
  [p feet base]
  (or (= :none (:step base)) (> (price p feet base) (cw/put-back-cost 0))))

(defn search-begun!
  "Note in memory (:route-search) when the dig-route searches kept over rounds from feet began and what the way without
  them is worth (search-bound/fallback-s), once."
  [c feet goal opts]
  (when-not (:route-search (ctx/mem c))
    (ctx/update-mem! c assoc :route-search {:since (ctx/now c) :fallback-s (search-bound/fallback-s c feet goal (may-dig-fn c) opts)})))

(defn search-spent!
  "Drop the call's kept searches, which stood the body still as long as the way without them costs (search-bound/spent?): those
  that had not ended are not begun again in the call (memory :spent-routes)."
  [c feet]
  (ctx/emit! c :go-to.search-spent :info (merge {:at feet :text "searched for a dig route as long as the way without one costs: taking that way"}
                                                (select-keys (:route-search (ctx/mem c)) [:fallback-s])))
  (let [ends (searching-ends (route-owner c))]
    (forget-routes! (route-owner c))
    (ctx/update-mem! c #(-> % (dissoc :route-search) (update :spent-routes (fnil into #{}) ends)))))

(defn stand-goal
  "The cell a way is made to for goal: a solid goal cell with an open cell above it, with :range of one block or more, is
  worked from on top of it, not dug away."
  [c [x y z :as goal]]
  (let [block-at (escape/block-at-of (:primitives c))]
    (if (and (>= (:range (:args c) 1) 1)
             (not (dig-route/open? (block-at goal)))
             (dig-route/open? (block-at [x (inc y) z])))
      [x (inc y) z]
      goal)))

(defn ^:async choose-escalation!
  "escalate!'s choice and start of the next way, or its give-up."
  [c pos failed]
  (let [c (with-own-pen c)
        {:keys [tries status result]} (:give-up (ctx/mem c))
        feet (end/feet-cell c)
        counted (count-escalation (ctx/mem c) (if (:dive-climb (ctx/mem c))
                                                (- (:y pos) (second feet))
                                                (u/dist (zipmap [:x :y :z] feet) pos)))
        n (:fruitless counted)
        _ (when (<= n (tuning/max-escalations)) (await (look-round! c feet)))
        goal (stand-goal c (mapv #(js/Math.floor (% pos)) [:x :y :z]))
        slice (route-slice (route-owner c))
        shut? (delay (escape/walk-enclosed? c))
        opts (way-opts c feet (fetch-tool? (:primitives c) (:doors (:args c)) shut?) slice)
        gap? (gap-walled? c result (skips c))
        into? (or (goal-walled? c result shut?) (and gap? (not @shut?)))
        ;; a goal over or ringed by lava is refused: no dig, in or out, makes it acceptable
        lava-goal? (and (= :goal-enclosed (some-> (:reason result) keyword)) (goal-lava? (:primitives c) goal))
        ;; a shut-in body (a pit, a plateau it cannot walk off) whose goal is walled in digs straight into it when it can
        dig-in? (and (not into?) (or gap? (= :goal-enclosed (some-> (:reason result) keyword))))
        ;; a buried body whose plan ran out of land read the rock round it as rock: to a goal no higher than it is
        ;; across (a stair climbs to that) it digs toward it leg by leg, the last one into it; else the ways out as before
        rock-dig? (and (#{:exhausted :start-enclosed} (some-> (:reason result) keyword)) (not (:edge-mode (ctx/mem c)))
                       (seq (leg-ends feet goal))
                       (<= (- (goal 1) (feet 1)) (js/Math.hypot (- (goal 0) (feet 0)) (- (goal 2) (feet 2))))
                       (wworld/rock-guess? (:primitives c) (:unknown c)))
        choose-by (cond into? #(goal-way c feet goal %1 %2)
                     ;; a goal too far for one dig route, no stair, pillar or door out (a plateau top): a dig off the land's edge
                     dig-in? #(or (dig-way (:primitives c) feet goal %1 %2)
                                  (let [e (escape/choose (:primitives c) feet goal %1 %2)]
                                    (or (when (#{:none :approach} (:step e)) (frontier-way (:primitives c) feet goal %1 %2))
                                        e)))
                     ;; its route searches are shared through the slice's answers: choose runs again per zone rule below
                     rock-dig? #(or (dig-leg c feet goal %1 %2) (escape/choose (:primitives c) feet goal %1 %2))
                     (and (not= :never (some-> (:doors (:args c)) keyword)) (escape/blocked-door-beside? (:primitives c) feet))
                     #(or (dig-way (:primitives c) feet goal %1 %2) (escape/choose (:primitives c) feet goal %1 %2))
                     @shut? #(or (climb-route/climb-way (:primitives c) feet goal %1 %2) (escape/choose (:primitives c) feet goal %1 %2))
                     :else #(escape/choose (:primitives c) feet goal %1 %2))
        ;; a step up over a gap with nothing to fill it: cutting the riser on the far side makes the jump level
        spare (may-dig-fn c)
        choose (memoize (fn [may-dig? o] (if gap? (or (gap-dig c result may-dig? o) (choose-by may-dig? o)) (choose-by may-dig? o))))
        e (or (when (shelter-exit? c result) {:step :leave-shelter})
              (cheaper-fill (:primitives c) feet
                            (or (own-stair c feet goal) (choose spare opts))
                            (gap-fill c result (:skip opts))
                            (drop-fill c result (:skip opts) feet)))
        choosable? #(not= :none (:step (choose % opts)))
        zone-free (exit-dig-fn c feet)
        ;; the only way on is a drop the planner will not take (a loaded pit it cannot climb back out of): a shut-in body takes it, then digs out from the bottom
        drop-in (let [{:keys [kind at]} (when (= :one-way (some-> (:reason result) keyword)) (:one-way result))]
                  (when (and (= :drop (some-> kind keyword)) at (not= :closed (some-> (:one-way (:args c)) keyword)) (not-any? (:skip opts) [:approach]) (not (contains? (:approached (ctx/mem c)) (vec at)))
                             (not= :none (:step (escape/choose (:primitives c) (vec at) goal spare opts))))
                    (vec at)))
        chosen e
        spot (when (= :none (:step e)) (when-not (or (contains? (:skip opts) :approach) (searching? slice)) (escape/way-spot (:primitives c) feet goal spare opts (or (:approached (ctx/mem c)) #{}))))
        ;; a way through the body's own builds, priced against the one that spares them (own-way)
        own (delay (when (and (or (seq (access/own-plans c)) (access/in-own-area? c (zipmap [:x :y :z] feet))) (own-may-beat? (:primitives c) feet e))
                     (own-way (:primitives c) feet e (choose (may-dig-fn c true) (assoc opts :own? (own-fn c (:own? opts)) :own-cut true)) opts)))
        e (cond
            (priced-steps (:step e)) (or @own e)
            (not= :none (:step e)) e
            spot {:step :approach :pos spot}
            drop-in {:step :approach :pos drop-in}
            @own @own
            (and (zone-exit? c feet) (choosable? zone-free)) (assoc (choose zone-free opts) :exit-zone true)
            (choosable? (constantly true)) {:step :none :why :zone}
            :else e)
        e (if (and (= :none (:step e)) (searching? slice)) {:step :searching} e)]
    (if (= :searching (:step e)) (keep-routes! slice) (forget-routes! (route-owner c)))
    (ctx/hold-still! c (when (= :searching (:step e)) :searching))
    (cond
      lava-goal? (end/give-up! c pos result (merge (select-keys result [:route :straight :allowed]) {:escalation {:step :none :why :goal-lava}}))
      (and (:cave-climb (ctx/mem c)) (or (> n (tuning/max-escalations)) (= :none (:step e))))
      (do (ctx/update-mem! c #(-> % (dissoc :cave-climb :dive-climb :give-up :escalate-failed :route-search) (assoc :cave-ok true)))
          :again)
      (and failed (= :detour status)) (end/give-up! c pos result (merge (select-keys result [:route :straight :allowed]) {:escalation failed}))
      ;; past the count a dig route is still taken while it ends more than a block nearer the goal than any way before
      (and (> n (tuning/max-escalations)) (not (and (= :dig-route (:step e)) (dig-progress? e goal (:escalation-best counted) (:dive-climb (ctx/mem c))))))
      (end/give-up! c pos result (merge (select-keys result [:route :straight :allowed]) {:escalation (cond-> (or failed (:last-failed (ctx/mem c)) (when (= :none (:step chosen)) chosen) {:step :spent :n (tuning/max-escalations)}) into? (assoc :into true))}))
      (= :searching (:step e))
      (do (search-begun! c feet goal opts)
          (ctx/update-mem! c assoc :escalate-now true :escalate-failed failed)
          :again)
      (= :none (:step e)) (end/give-up! c pos result (merge (select-keys result [:route :straight :allowed]) {:escalation (cond-> (or failed e) into? (assoc :into true))}))
      :else
      (do (ctx/emit! c :go-to.escalated :info {:step (:step e) :why (if (:own e) :own-way-blocked (:why (end/give-up-fields result))) :n n :at feet
                                               :text (if (:own e) "its own stair up is blocked: cutting it again" (str "no way " (if (or into? (= :dig-route (:step e))) "in" "out") " on foot: " (name (:step e))))})
          (ctx/update-mem! c #(-> (merge (dissoc % :escalate-failed :route-search) counted)
                                  (cond-> (:leg-s e) (update :leg-dig-s (fnil + 0) (:leg-s e)))
                                  (assoc :escalations (inc (:escalations % 0)) :escalation e :escalation-from feet
                                         :planned (planned-cells (:primitives c) e feet) :holes-before (count (own-holes c)))))
          :again))))

(defn ^:async escalate!
  "Start the next escalation for the give-up kept in memory (:give-up), or give up with it when there is none (or
  with failed, the detail of the escalation that just failed, when there is none left after it; past max-escalations
  with the last one that failed, if any). A climb begun because a walkable plan went down through caves (memory
  :cave-climb) walks that plan after all instead of giving up (memory :cave-ok). A detour's escalation that failed
  (detour-escalate!) gives the detour up at once. A dig route search that has not ended within the call's slice
  (route-slice) goes on in the next calls, each only that (routes-searching!), before the way is chosen with it; once
  standing still for them has cost what the way without them costs (search-bound/spent?), they are
  dropped and that way is chosen (memory :spent-routes: none of them is begun again)."
  [c pos failed]
  (let [failed (or failed (:escalate-failed (ctx/mem c)))]
    (cond
      (search-bound/spent? c (ctx/mem c))
      (do (search-spent! c (end/feet-cell c))
          (await (choose-escalation! c pos failed)))
      (routes-searching! (route-owner c)) (do (ctx/hold-still! c :searching) (ctx/update-mem! c assoc :escalate-now true) :again)
      :else (await (choose-escalation! c pos failed)))))

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

(defn ^:async give-up-or-escalate!
  "Give up the walk with result, or escalate instead (escalate?): at the next step, so the walk's search and the
  escalation's do not block the body in one. A give-up whose reason may mean the body is shut in looks round first
  (look-round!), so {:shut-in true} rests on walls the body has seen."
  [c pos tries status result]
  (if (escalate? c tries result)
    (do (ctx/update-mem! c assoc :dive-climb (dive? c result) :escalate-now true
                         :give-up {:tries tries :status status
                                   :result (select-keys result [:status :reason :kind :cells :near :one-way :at :from :dip :rise :move :why :step :partial :goal-left])})
        :again)
    (let [shut-reason? (and (not (sh-inside/sheltered-in c))
                            (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result)))]
      (when shut-reason? (await (look-round! c (end/feet-cell c))))
      (end/give-up! c pos result (when (and shut-reason? (escape/walk-enclosed? c)) {:shut-in true})))))

(defn escalated!
  "The child made a way (or part of one, done false): start a climb's next way at once when it made all of it
  (route-ways/next-way), walk on after its last, else walk again from a fresh count, or escalate again at once after an
  approach, after a whole stair that left the body shut in under the goal (climb-on?), or after any stair that left it
  under ground on its climb away from a dive (dive-climb-on?). Its holes are put back once a walk round gets on past
  it (restore-next!); a cut of the body's own builds (:own-cut) at once, done or stopped in a dead end (what
  restore/skip-why spares then, the cells the body stands in or needs, waits for that walk). An approach to a cell more than a block nearer the goal than it set out from does not count toward
  max-escalations (its look may show a wall to dig or not); one no nearer does, so a chain of them ends; nor is a cell
  walked to twice (memory :approached)."
  [c pos {:keys [step]} done?]
  (let [feet (end/feet-cell c)
        e (:escalation (ctx/mem c))
        nxt (when done? (route-ways/next-way e feet))
        ;; a climb's last way ends where its walk to the goal begins
        again? (and (not nxt) (not (and done? (contains? e :then))) (or (= :approach step) (and (= :stair step) (or (and done? (climb-on? c pos)) (dive-climb-on? c pos)))))
        target (some-> e :pos vec)
        to-goal #(u/dist (zipmap [:x :y :z] %) pos)
        nearer? (and (= :approach step) target (some-> (:escalation-from (ctx/mem c)) to-goal dec (> (to-goal target))))]
    (ctx/update-mem! c #(cond-> (-> % (dissoc :escalation :escalation-from :planned :holes-before :best)
                                    (cond-> (= :approach step) (update :approached (fnil conj #{}) target)
                                            nearer? (update :fruitless (fn [n] (max 0 (dec (or n 0))))))
                                    (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {}))
                          again? (assoc :escalate-now true)
                          nxt (assoc :escalation (cond-> nxt (:own-cut e) (assoc :own-cut true)) :escalation-from feet :planned (planned-cells (:primitives c) nxt feet) :holes-before (count (own-holes c)))
                          ;; a cut of its own build (a pen open to the animals): put back at once, the walk on not waited for
                          (:own-cut e) (assoc :restore-now true)
                          (not (#{:approach :fill-gap} step)) (assoc :restore-pending true)))
    (when nxt
      (ctx/emit! c :go-to.escalated :info {:step (:step nxt) :why :climb-on :at feet :text (str "climbing on: " (name (:step nxt)))})))
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
  that cuts no block the body has no tool for (stone is not left for andesite: both want the pickaxe)."
  [c pos {:keys [step own] :as e}]
  (and (= :stair step) (not own)
       (let [p (:primitives c)
             goal (stand-goal c (mapv #(js/Math.floor (% pos)) [:x :y :z]))
             block-at (escape/block-at-of p)
             may-dig? (may-dig-fn c)]
         (not= :none (:step (escape/choose p (end/feet-cell c) goal #(and (may-dig? %) (not (tools/needs-tool-to-clear? p (block-at %))))
                                           {:own? (own-placed-fn c) :skip (conj (skips c) (skip-key e)) :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c) :stands (ledger/stair-stands c)}))))))

(defn ^:async escalation-failed!
  "The child stopped or would wait: when it changed something, walk again (escalation-stopped!); when it had no tool
  and has not dug by hand yet, take another stair heading clear of that block when there is one (other-heading?), else
  run it again by hand (by-hand!); else try the next method (escalation-next!), or after the body's own stair walk
  again (own-failed!)."
  [c pos detail]
  (let [e (:escalation (ctx/mem c))
        no-tool (and (no-tool? detail) (not (:by-hand (ctx/mem c))))
        other? (and no-tool (other-heading? c pos e))
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
        r (await (dig-leave/leave! c {:toward pos :any-time? true}))]
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
        [job args] (escalation-job e (hole-tag c) (boolean (or (:exit-zone e) (:ignore-zones? (:args c)) (zone-exit? c (end/feet-cell c)))) (boolean (:by-hand (ctx/mem c))) (fetch-tool? c))
        _ (await (torch/return-torch! c))
        r (await (ctx/call c :escalation job (cond-> (update args :checks #(some->> % (ready/unforced c))) (= :approach step) (->> (ready/forage-walk c)))))]
    (let [dug (concat (:planned (ctx/mem c)) (when (not= :continue r) (:dug (:data r))))]
      (forget-dug-own! c dug)
      (when-not (:own e) (note-holes! c dug)))
    (cond
      (= :continue r) :continue
      (succeeded? e r) (escalated! c pos e true)
      :else (escalation-failed! c pos (merge {:step step} (stop-detail r))))))

(defn ^:async route-cell!
  "Dig one cell of a :dig-route escalation: look first at a cell the body has not sensed, then check it again as it
  is now (escape/protected?, escape/diggable? over may-dig-fn and own-dig-fn: a block built there since the route
  was planned is someone's) and dig it (b/dig-cell!: no tool needed, the drops left; a cell whose floor is unseen is dug
  on top only after the dig found no side stand to step aside to and look from), noting it as a hole once dug.
  The dig's outcome, :open for a cell open already, :replan for a guessed cell the look shows otherwise (the route
  was planned over rock that is not there), or :not-diggable."
  [c {:keys [cell guess block] :as planned}]
  (let [p (:primitives c)]
    (when (dig-look/unknown? p cell) (await (dig-look/look-at! c cell)))
    (let [block-at (escape/block-at-of p)
          n (block-at cell)]
      (cond
        (and guess (not (dig-look/unknown? p cell)) (not= n block)) :replan
        (bs/passable? n) :open
        (or (escape/protected? n) (not (escape/diggable? block-at (may-dig-fn c (own-cut? c)) (own-dig-fn c) cell))) :not-diggable
        :else
        (let [fetch? (fetch-tool? c)
              args {:ignore-zones? (boolean (:ignore-zones? (:args c))) :ways (ways-for fetch?) :need-drop fetch? :accept #{:lava-adjacent}}
              pos (zipmap [:x :y :z] cell)
              e (await (b/dig-cell-ending! c pos args))
              boxed? (and (not= :continue e) (= :no-side-stand (:why (:cause e))))
              e (if boxed? (await (b/dig-cell-ending! c pos (update args :accept conj :under-feet))) e)
              o (b/dig-outcome e)
              dug (assoc planned :block n)]
          (when (= :dug o)
            (forget-dug-own! c [dug])
            (note-holes! c [dug]))
          o)))))

(defn replans-spent?
  "Whether the dig route's replans (memory :replan-budget {:since :fallback-s}, begun at the first) have taken the body as long
  as the way without a dig route costs (search-bound/fallback-s): the most they can save."
  [c m]
  (when-let [{:keys [since fallback-s]} (:replan-budget m)]
    (>= (/ (- (ctx/now c) since) 1000) fallback-s)))

(defn ^:async replan-route!
  "Plan the :dig-route escalation again from the feet into goal pos with what the body sees now (route-cells): :again,
  the walk on (escalated!) when nothing is left to dig or the only thing missing is a pocket's floor (it shows once the
  body walks in, and the walk keeps off a floor it then sees to be a void), the ways chosen again when no route is
  left, or the escalation fails :replans-spent once the replans have taken the body as long as the way without a dig route
  costs (replans-spent?; the walk on is a replan too, so a walk that keeps failing ends it). A search that spends its slice (route-slice) goes on next round: :continue. A loosened route's cells are dropped: the cells it still guesses stay unseen from here and
  digging one that is open fails :cannot, and the walk meets any real rock and escalates again. A climb's dig route
  (climb-route) walks again instead, counted as a replan too: the climb is searched again, and past the count it fails
  :replans-spent, which skips dig routes for the rest of the call."
  [c pos cell]
  (let [c (with-own-pen c)
        feet (end/feet-cell c)
        _ (when-not (:replan-budget (ctx/mem c))
            (ctx/update-mem! c assoc :replan-budget
                             {:since (ctx/now c)
                              :fallback-s (search-bound/fallback-s c feet (stand-goal c (mapv #(js/Math.floor (% pos)) [:x :y :z]))
                                                                   (may-dig-fn c) (way-opts c feet (fetch-tool? c) nil))}))
        m (ctx/mem c)
        spent? (replans-spent? c m)
        in-range? (not spent?)]
    (cond
      ;; a climb's dig over rock it guessed: the climb is searched again from here
      (and (contains? (:escalation m) :then) in-range?)
      (escalated! c pos (:escalation m) false)
      ;; escalation-failed! would walk again after a part made and choose the climb anew: the replans are spent
      (contains? (:escalation m) :then)
      (await (escalation-next! c pos {:step :dig-route :reason :replans-spent :cell cell :fallback-s (:fallback-s (:replan-budget m))}))
      ;; the replan's searches stood the body still as long as the way without the route costs: the ways are chosen again
      (search-bound/spent? c m)
      (do (search-spent! c (end/feet-cell c)) (escalated! c pos (:escalation m) false))
      :else
      (let [slice (route-slice (route-owner c))
            feet (end/feet-cell c)
            goal (stand-goal c (mapv #(js/Math.floor (% pos)) [:x :y :z]))
            plan (fn [floors-unseen-ok?]
                   (route-cells (:primitives c) feet goal (may-dig-fn c (own-cut? c)) (own-dig-fn c) floors-unseen-ok? (fetch-tool? c)
                                (search-opts {:stands (ledger/stair-stands c) :slice slice :spent (:spent-routes m) :own-s (own-cut-fn c) :own-cut (own-cut? c)})))
            cells (when in-range? (plan false))
            loosened (when (and in-range? (nil? cells)) (plan true))
            walk-on! (fn [] (escalated! c pos (:escalation m) true))]
        ;; an answered replan must not be answered again from the kept search when the world has changed since
        (cond
          (searching? slice)
          (do (search-begun! c feet goal (way-opts c feet (fetch-tool? c) nil))
              (keep-routes! slice)
              (ctx/hold-still! c :searching)
              :continue)
          :else
          (do (forget-routes! (route-owner c))
              (ctx/update-mem! c dissoc :route-search)
              (cond
                (some? loosened) (walk-on!)
                ;; the guess the route rested on is gone: the ways are chosen again from what the body sees now
                (and in-range? (nil? cells)) (escalated! c pos (:escalation m) false)
                (nil? cells) (await (escalation-failed! c pos {:step :dig-route :reason :replans-spent :cell cell}))
                (empty? cells) (walk-on!)
                :else (do (ctx/update-mem! c assoc-in [:escalation :cells] cells) :again))))))))

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
