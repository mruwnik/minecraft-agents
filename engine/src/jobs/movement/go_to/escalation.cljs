(ns jobs.movement.go-to.escalation
  "go-to's way out of a shut-in body, or in to a walled-in goal: run the way escalate-choose picked (pillar, stair,
  clear-path, dig route or a walk to the wall), note the holes it dug, put them back once the walk is on past them.

  Rules of go-to:
  - A goal that is lava, stands over lava or has lava on all four sides (escalate-choose/goal-lava?) is refused at once when the walk finds it walled in: no way is made, in or out; the dig rules below are for an acceptable goal that is walled in.
  - :dig-out: a body sealed in its own shelter (a :shelter entry) when the walk finds no way, leaves it first at any hour (dig-in-leave/leave!, a go-to.escalated {:step :leave-shelter}); the start check priced the way out.
  - With an escalating way: a body not in its own shelter whose search ran out of land (:exhausted, in any size of cave or mine), or that is shut in (jobs.lib.reach.room/enclosed?) and whose
    search ran out of land (:goal-enclosed or :goal-cut-off) or of nodes (:budget) within the budget-near setting of the goal, any body whose only way needs a sprint it lacks the food for (:abilities :corner-jump or :gap-sprint), or any body whose last 3 walks failed at the walker (stuck, off-plan, steer failed, no nearer; escalation/walker-fault?) with no door it can open beside its room (one with room for the body behind it; a trunk over that cell makes it no way out, and the dig route cuts the tree's logs: escape/tree-log?), makes a way instead of giving up, one child job
    each (jobs.lib.escape/choose, over the ways the call allows), at most 3 in a row that start no nearer the goal than one before (escalation/count-escalation): standing on a stair of its own under the goal, that stair cut again when something shuts it (escalation/own-stair: a cap it put in the stair is dug, :why :own-way-blocked); else jobs.access.pillar up out of a pit when it carries enough blocks; else
    jobs.access.clear-path through a wall up to 3 thick toward the goal (into a walled-in goal no door or stair reaches:
    the fewest cells dug down or across to it, escalation/dig-way, tried first for a shut-in body, none that takes over max-route-s seconds to dig with the pickaxe it carries or would fetch); else jobs.access.stair up out of a pit or
    toward a higher goal (as far as the goal is above, at most 8; a stair that leaves the body shut in under the goal is
    followed by the next escalation at once); else a walk to the nearest wall first; when no way can be made where the body stands, a walk to the nearest cell it can stand on from which one can (escape/way-spot: a body in its hut beside the doorway, whose walls it spares). Each emits go-to.escalated {:step :why :n}. Once
    through, it puts back what clear-path or stair dug in another's zone, claim or plan (with :ignore-zones?;
    its own diggings elsewhere stay) (jobs.blocks.place, the dug block or its drop, when carried): info
    go-to.restored, warn go-to.restore-skipped for cells it could not. Pillar blocks and a stair's placed floors are
    left to jobs.access.cleanup. Its walk child (ctx/call) has :ways #{:walk :clear-seal}. Jobs that must not change the world
    on the way pass :ways #{:walk}: a shut-in body then gives up with :shut-in true and says so in the text."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.need :as need]
            [jobs.lib.ready :as ready]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.access :as access]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.movement.go-to.climb :as climb]
            [jobs.movement.go-to.escalate-choose :refer [climb-on? dive-climb-on? dive? escalate-reason? escalate? fetch-tool? gap-fill
                                                         goal-way dig-way goal-lava? goal-walled? own-stair route-cells shelter-exit? skips sprint-only?]]
            [jobs.movement.go-to.holes :refer [forget-dug-own! hole-tag may-dig-fn note-holes! own-holes own-placed-fn planned-cells]]
            [jobs.movement.go-to.result :as end]
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
    :pillar ['jobs.access.pillar (cond-> {:height (:height e) :ways #{:carried} :ignore-zones? ignore-zones? :checks {:safe? '(always) :fit? '(always)}} (:item e) (assoc :use {:blocks [(:item e)]}))]
    :stair ['jobs.access.stair {:dir :up :heading (:heading e) :steps (:steps e) :note (when-not (:own e) tag) :checks {:fit? '(always)} :accept #{:falling-block} :ways (ways-for fetch?) :settings {:jobs.lib.trip/spent-uses 0} :ignore-zones? ignore-zones? :need-drop (not by-hand?)}]
    :clear-path ['jobs.access.clear-path {:checks {:fit? '(always)} :heading (:heading e) :ignore-zones? ignore-zones? :need-drop (not by-hand?) :ways #{:carried}}]
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
        _ (when (<= n (tuning/max-escalations)) (await (look-round! c feet)))
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        opts {:own? (own-placed-fn c) :skip (skips c) :fetch? (fetch-tool? c) :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c)}
        into? (goal-walled? c result)
        ;; a goal over or ringed by lava is refused: no dig, in or out, makes it acceptable
        lava-goal? (and (= :goal-enclosed (some-> (:reason result) keyword)) (goal-lava? (:primitives c) goal))
        ;; a shut-in body (a pit, a plateau it cannot walk off) whose goal is walled in digs straight into it when it can
        dig-in? (and (not into?) (= :goal-enclosed (some-> (:reason result) keyword)))
        choose (cond into? #(goal-way c feet goal % opts)
                     dig-in? #(or (dig-way (:primitives c) feet goal % opts) (escape/choose (:primitives c) feet goal % opts))
                     (and (not= :never (some-> (:doors (:args c)) keyword)) (escape/blocked-door-beside? (:primitives c) feet))
                     #(or (dig-way (:primitives c) feet goal % opts) (escape/choose (:primitives c) feet goal % opts))
                     :else #(escape/choose (:primitives c) feet goal % opts))
        e (or (when (shelter-exit? c result) {:step :leave-shelter})
              (gap-fill c result (:skip opts))
              (own-stair c feet goal)
              (choose (may-dig-fn c)))
        choosable? #(not= :none (:step (choose %)))
        zone-free (exit-dig-fn c feet)
        spot (when (= :none (:step e)) (when-not (contains? (:skip opts) :approach) (escape/way-spot (:primitives c) feet goal (may-dig-fn c) opts (or (:approached (ctx/mem c)) #{}))))
        e (cond
            (not= :none (:step e)) e
            spot {:step :approach :pos spot}
            (and (access/spare-own-builds) (choosable? (may-dig-fn c false)))
            {:step :none :why (if (choosable? (may-dig-fn c true false)) :own-pen :own-build)}
            (and (zone-exit? c feet) (choosable? zone-free)) (assoc (choose zone-free) :exit-zone true)
            (choosable? (constantly true)) {:step :none :why :zone}
            :else e)]
    (cond
      lava-goal? (end/give-up! c pos result (merge (select-keys result [:route :straight :allowed]) {:escalation {:step :none :why :goal-lava}}))
      (and (:cave-climb (ctx/mem c)) (or (> n (tuning/max-escalations)) (= :none (:step e))))
      (do (ctx/update-mem! c #(-> % (dissoc :cave-climb :dive-climb :give-up) (assoc :cave-ok true)))
          :again)
      (and failed (= :detour status)) (end/give-up! c pos result (merge (select-keys result [:route :straight :allowed]) {:escalation failed}))
      (> n (tuning/max-escalations)) (end/give-up! c pos result (merge (select-keys result [:route :straight :allowed]) {:escalation (cond-> (or failed (:last-failed (ctx/mem c)) {:step :spent :n (tuning/max-escalations)}) into? (assoc :into true))}))
      (= :none (:step e)) (end/give-up! c pos result (merge (select-keys result [:route :straight :allowed]) {:escalation (cond-> (or failed e) into? (assoc :into true))}))
      :else
      (do (ctx/emit! c :go-to.escalated :info {:step (:step e) :why (if (:own e) :own-way-blocked (:why (end/give-up-fields result))) :n n :at feet
                                               :text (if (:own e) "its own stair up is blocked: cutting it again" (str "no way " (if (or into? (= :dig-route (:step e))) "in" "out") " on foot: " (name (:step e))))})
          (ctx/update-mem! c #(-> (merge % counted)
                                  (assoc :escalations (inc (:escalations % 0)) :escalation e :escalation-from feet
                                         :planned (planned-cells (:primitives c) e feet) :holes-before (count (own-holes c)))))
          :again))))

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

(defn ^:async give-up-or-escalate! [c pos tries status result]
  (if (escalate? c tries result)
    (do (ctx/update-mem! c assoc :dive-climb (dive? c result)
                         :give-up {:tries tries :status status
                                   :result (select-keys result [:status :reason :kind :cells :near :one-way :at :from :dip :rise :move :why :step :partial :goal-left])})
        (await (escalate! c pos nil)))
    (end/give-up! c pos result
                  (when (and (not (sh-inside/sheltered-in c))
                             (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result))
                             (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword)))
                    {:shut-in true}))))

(defn escalated!
  "The child made a way (or part of one, done false): walk again from a fresh count, or escalate again at once after an
  approach, after a whole stair that left the body shut in under the goal (climb-on?), or after any stair that left it
  under ground on its climb away from a dive (dive-climb-on?). Its holes are put back once a walk round gets on past
  it (restore-next!). An approach does not count toward max-escalations (its look may show a wall to dig or not), but
  its cell is not walked to twice (memory :approached), so a chain of them ends."
  [c pos {:keys [step]} done?]
  (let [again? (or (= :approach step) (and (= :stair step) (or (and done? (climb-on? c pos)) (dive-climb-on? c pos))))
        target (some-> (:escalation (ctx/mem c)) :pos vec)]
    (ctx/update-mem! c #(cond-> (-> % (dissoc :escalation :escalation-from :planned :holes-before :best)
                                    (cond-> (= :approach step) (-> (update :approached (fnil conj #{}) target)
                                                                   (update :fruitless (fn [n] (max 0 (dec (or n 0)))))))
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
  that cuts no block the body has no tool for (stone is not left for andesite: both want the pickaxe)."
  [c pos {:keys [step own] :as e}]
  (and (= :stair step) (not own)
       (let [p (:primitives c)
             goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
             block-at (escape/block-at-of p)
             may-dig? (may-dig-fn c)]
         (not= :none (:step (escape/choose p (end/feet-cell c) goal #(and (may-dig? %) (not (tools/needs-tool-to-clear? p (block-at %))))
                                           {:own? (own-placed-fn c) :skip (conj (skips c) (skip-key e)) :approached (:approached (ctx/mem c)) :pillar-item (escape/pillar-item c)}))))))

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
  is now (escape/protected?, escape/diggable? over may-dig-fn and own-placed-fn: a block built there since the route
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
        (or (escape/protected? n) (not (escape/diggable? block-at (may-dig-fn c) (own-placed-fn c) cell))) :not-diggable
        :else
        (let [args {:ignore-zones? (boolean (:ignore-zones? (:args c))) :ways (ways-for (fetch-tool? c)) :need-drop (fetch-tool? c) :accept #{:lava-adjacent}}
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

(defn ^:async replan-route!
  "Plan the :dig-route escalation again from the feet into goal pos with what the body sees now (route-cells): :again,
  the walk on (escalated!) when nothing is left to dig or the only thing missing is a pocket's floor (it shows once the
  body walks in, and the walk keeps off a floor it then sees to be a void), or the escalation fails :no-route when no
  route is left or it planned tuning/max-route-replans times. The walk on counts as a replan, so a walk that keeps
  failing ends :no-route. A loosened route's cells are dropped: the cells it still guesses stay unseen from here and
  digging one that is open fails :cannot, and the walk meets any real rock and escalates again."
  [c pos cell]
  (let [m (ctx/mem c)
        n (inc (:route-replans m 0))
        plan (fn [floors-unseen-ok?]
               (route-cells (:primitives c) (end/feet-cell c) (mapv #(js/Math.floor (% pos)) [:x :y :z]) (may-dig-fn c) (own-placed-fn c) floors-unseen-ok? (fetch-tool? c)))
        in-range? (<= n (tuning/max-route-replans))
        cells (when in-range? (plan false))
        walk-on! (fn [] (ctx/update-mem! c assoc :route-replans n) (escalated! c pos (:escalation m) true))]
    (cond
      (and in-range? (nil? cells) (some? (plan true))) (walk-on!)
      (nil? cells) (await (escalation-failed! c pos {:step :dig-route :reason :no-route :cell cell}))
      (empty? cells) (walk-on!)
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
