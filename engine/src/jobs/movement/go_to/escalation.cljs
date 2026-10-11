(ns jobs.movement.go-to.escalation
  "go-to's way out of a shut-in body: choose pillar, stair, clear-path or a walk to the wall, run the child, note the
  holes it dug, put them back once the walk is on past them."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.movement.go-to.climb :as climb]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.torch :as torch]))

(def max-escalations 3)

(def escalate-reasons
  "Planner reasons that prove the way needs the world changed. A stuck or off-plan walk is the walker's fault and
  never escalates: changing the world there would hide a go-to bug. :start-enclosed is the search running out of land
  with no loaded edge reached (a sealed pen, the goal far and unloaded); :one-way is every nearer node lying behind a drop
  (escalating then needs the body shut in, escalate?)."
  #{:exhausted :goal-enclosed :goal-cut-off :start-enclosed :one-way})

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
  (boolean (and (:dive-stair c) (= :way-dives (some-> (:reason result) keyword)))))

(defn shelter-exit?
  "Whether a give-up with result may leave the body's own shelter instead: the walk proved the way needs the world
  changed, the body is shut in its shelter and it is not night (dig-in-leave/leave-unsafe)."
  [c result]
  (boolean (and (:escalate (:args c))
                (not (contains? (:skipped-steps (ctx/mem c) #{}) :leave-shelter))
                (or (escalate-reason? result (some-> (:doors (:args c)) keyword)) (sprint-only? result))
                (sh/sheltered-in c)
                (nil? (dig-leave/leave-unsafe (:primitives c))))))

(defn escalation-job
  "[job args] of the child that carries out escalation e (jobs.lib.escape/choose); tag marks the holes it digs;
  by-hand? digs what no carried tool clears bare-handed. The body's own stair cut again (:own) is marked by no tag: it
  stays open."
  [{:keys [step] :as e} tag ignore-zones? by-hand?]
  (case step
    :pillar ['jobs.access.pillar {:height (:height e) :item (:item e) :ignore-zones? ignore-zones?}]
    :stair ['jobs.access.stair {:dir :up :heading (:heading e) :steps (:steps e) :note (when-not (:own e) tag) :fetch false :spent-ok true :ignore-zones? ignore-zones? :by-hand by-hand?}]
    :clear-path ['jobs.access.clear-path {:heading (:heading e) :note tag :ignore-zones? ignore-zones? :by-hand by-hand?}]
    :approach ['jobs.movement.go-to {:pos (zipmap [:x :y :z] (:pos e)) :range 0 :escalate false}]
    :fill-gap ['jobs.blocks.place {:pos (zipmap [:x :y :z] (:cell e)) :any-of [(:item e)] :fetch false :ignore-zones? ignore-zones?}]))

(defn succeeded? [{:keys [step]} result]
  (case step
    :approach (true? (:arrived result))
    :fill-gap (boolean (:placed result))
    (= :done (:status result))))

(defn may-dig-fn
  "(may-dig? cell): no other owner's zone, claim or plan footprint refuses a dig there (jobs.lib.tidy/refusal), nor
  does a plan this body made (a way out never digs its own building)."
  [c]
  (let [own (access/rules-input c {:spare-own? true})]
    (fn [cell]
      (or (boolean (:ignore-zones? (:args c)))
          (let [pos (zipmap [:x :y :z] cell)]
            (and (nil? (tidy/refusal c :dig pos)) (nil? (access/own-keep own pos))))))))

(def placed-policy
  "Body memory policy of :escalation-placed, the cells go-to put a block back in: later escalations may dig them."
  {:cap 100 :ttl (* 6 60 60 1000)})

(defn own-placed-fn
  "(own? cell block): go-to itself placed block at cell (a put-back), so it is not someone's build."
  [c]
  (let [placed (set (map (comp (juxt :cell :block) :data) (ctx/entries c :escalation-placed)))]
    (fn [cell block] (contains? placed [(vec cell) block]))))

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
                        (not (skip :fill-gap)) item (:at result))
               (escape/gap-floor (escape/block-at-of p) (:at result)))]
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
  (doseq [{:keys [cell block]} (distinct dug)]
    (escape/note-hole! c (hole-tag c) cell block)))

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

(defn ^:async escalate!
  "Start the next escalation for the give-up kept in memory (:give-up), or give up with it when there is none (or
  with failed, the detail of the escalation that just failed, when there is none left after it; past max-escalations
  with the last one that failed, if any). A climb begun because a walkable plan went down through caves (memory
  :cave-climb) walks that plan after all instead of giving up (memory :cave-ok)."
  [c pos failed]
  (let [{:keys [tries status result]} (:give-up (ctx/mem c))
        feet (end/feet-cell c)
        counted (count-escalation (ctx/mem c) (if (:dive-climb (ctx/mem c))
                                                (- (:y pos) (second feet))
                                                (u/dist (zipmap [:x :y :z] feet) pos)))
        n (:fruitless counted)
        _ (when (<= n max-escalations) (await (look-round! c feet)))
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        opts {:own? (own-placed-fn c) :skip (:skipped-steps (ctx/mem c) #{})}
        e (or (when (shelter-exit? c result) {:step :leave-shelter})
              (gap-fill c result (:skip opts))
              (escape/choose (:primitives c) feet goal (may-dig-fn c) opts))
        e (if (and (= :none (:step e)) (not= :none (:step (escape/choose (:primitives c) feet goal (constantly true) opts))))
            {:step :none :why :zone}
            e)]
    (cond
      (and (:cave-climb (ctx/mem c)) (or (> n max-escalations) (= :none (:step e))))
      (do (ctx/update-mem! c #(-> % (dissoc :cave-climb :dive-climb :give-up) (assoc :cave-ok true)))
          :again)
      (> n max-escalations) (end/give-up! c pos tries status result {:escalation (or failed (:last-failed (ctx/mem c)) {:step :spent :n max-escalations})})
      (= :none (:step e)) (end/give-up! c pos tries status result {:escalation (or failed e)})
      :else
      (do (ctx/emit! c :go-to.escalated :info {:step (:step e) :why (:why (end/give-up-fields result)) :n n :at feet
                                               :text (str "no way out on foot: " (name (:step e)))})
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
    the body's own (climb/dives?);
  - :cheaper: it is a whole path costing more seconds than 3 such stairs, the goal is stair-first-rise or more above
    the feet and the stair has not been tried this call.
  Else nil."
  [c pos]
  (let [m (ctx/mem c)
        feet (end/feet-cell c)
        goal (mapv #(js/Math.floor (% pos)) [:x :y :z])
        cave (when (:dive-stair c) (:cave-floor c))
        cheaper? (and (not (:stair-tried m)) (>= (- (second goal) (second feet)) stair-first-rise))
        stair (delay (let [e (escape/choose (:primitives c) feet goal (may-dig-fn c) {:own? (own-placed-fn c) :skip (:skipped-steps m #{})})]
                       (when (= :stair (:step e)) e)))]
    (when (and (:escalate (:args c)) (not (sh/sheltered-in c)) (or cave cheaper?))
      (fn [{:keys [status steps ^js r]} first?]
        (let [seconds (some-> r .-path .-cost .-priced)]
          (cond
            (and cave (climb/dives? steps cave (ledger/ways-of c)) @stair) :dives
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
    (when-let [e (and (:escalate (:args c)) (climb/recut feet (ledger/ways-of c)))]
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
  "Whether a give-up with result may escalate instead: a carried block fills the 1-cell gap it was refused (gap-fill), its only way dives (dive?), or the body is shut in (reach/enclosed?) and no door or gate
  borders where it can walk (escape/door-beside?: that is its way out; an iron door, or any door under :doors :never,
  is a wall). Never out of
  the body's own shelter: it is shut in on purpose."
  [c result]
  (and (:escalate (:args c))
       (or (shelter-exit? c result) (not (sh/sheltered-in c)))
       (or (shelter-exit? c result)
           (some? (gap-fill c result (:skipped-steps (ctx/mem c) #{})))
           (sprint-only? result)
           (dive? c result)
           (and (escalate-reason? result (some-> (:doors (:args c)) keyword))
                (escape/enclosed? (:primitives c) (some-> (:doors (:args c)) keyword))
                (not (escape/door-beside? (:primitives c) (end/feet-cell c) (some-> (:doors (:args c)) keyword)))))))

(defn ^:async give-up-or-escalate! [c pos tries status result]
  (if (escalate? c result)
    (do (ctx/update-mem! c assoc :dive-climb (dive? c result)
                         :give-up {:tries tries :status status
                                   :result (select-keys result [:status :reason :kind :cells :near :one-way :at :dip :rise])})
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

(defn ^:async escalation-failed!
  "The child stopped or would wait: when it changed something, walk again (escalation-stopped!); when it had no tool
  and has not dug by hand yet, run it again by hand (by-hand!); else try the next method (escalation-next!), or after
  the body's own stair walk again (own-failed!)."
  [c pos detail]
  (let [hand? (and (no-tool? detail) (not (:by-hand (ctx/mem c))))]
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
  "One call of the escalation child: the whole pillar, stair or door. After it, every planned cell now dug is in the
  ledger (note-holes!). Its wait or its failure gives up, with the child's reason, unless it got part of the way."
  [c pos]
  (let [{:keys [step] :as e} (:escalation (ctx/mem c))
        [job args] (escalation-job e (hole-tag c) (boolean (:ignore-zones? (:args c))) (boolean (:by-hand (ctx/mem c))))]
    (if-let [wait (b/child-wait c :escalation job args)]
      (escalation-failed! c pos (merge {:step step} wait))
      (let [_ (await (torch/return-torch! c))
            r (await (ctx/call-child c :escalation job args))
            res (ctx/child-result c :escalation)]
        (when-not (:own e) (note-holes! c (concat (:planned (ctx/mem c)) (when (not= :continue r) (:dug res)))))
        (cond
          (= :continue r) :continue
          (= :declined r) (escalation-failed! c pos {:step step :reason :not-ready})
          (succeeded? e res) (escalated! c pos e true)
          :else (escalation-failed! c pos (merge {:step step}
                                                 (select-keys res [:reason :why :cell :block :at :short :detail :wait]))))))))

(defn ^:async escalation-round!
  "One round of the escalation: leaving the shelter, or the child's call (escalation-child-round!)."
  [c pos]
  (if (= :leave-shelter (:step (:escalation (ctx/mem c))))
    (leave-shelter-round! c pos)
    (escalation-child-round! c pos)))

(defn restore-done!
  "Tell what was put back and what was left, and end the put-back (the walk goes on from a fresh count)."
  [c]
  (let [{:keys [restored skipped]} (ctx/mem c)]
    (when (seq restored)
      (ctx/emit! c :go-to.restored :info {:cells restored :text (str "put back " (count restored) " dug blocks")}))
    (when (seq skipped)
      (ctx/emit! c :go-to.restore-skipped :warn {:cells skipped
                                                 :text (str "left " (count skipped) " dug blocks for restore-broken")}))
    (ctx/update-mem! c #(-> % (dissoc :restore-now :restore-seen :restored :skipped :changed :best)
                            (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {})))))

(defn changed?
  "Whether hole cell is seen filled (not air); a cell the body does not know is not."
  [p cell]
  (let [n (u/seen-name p (zipmap [:x :y :z] cell))]
    (boolean (and n (not (b/air n))))))

(defn deep-below?
  "Whether cell lies more than 2 below the body's feet: a place walk (range 3) has to go down into the shaft for it."
  [c [_ y _]]
  (> (- (second (end/feet-cell c)) y) 2))

(defn skip-why
  "Why hole e must not be filled now, or nil: :changed (no longer air: forgotten), :occupied (the body is in it),
  :needed (filling it would shut in a body that has room now: the stair it stands on, its way out), :away (out of
  reach and deep below the feet: walking down to it would put the body inside its own refilled stair)."
  [c {:keys [cell]}]
  (let [p (:primitives c)]
    (cond
      (changed? p cell) :changed
      (tidy/in-body? p cell) :occupied
      (and (not (reach/enclosed? p)) (reach/enclosed? p #{(vec cell)})) :needed
      (and (not (b/in-reach? c cell)) (deep-below? c cell)) :away)))

(defn ^:async restore-round!
  "Put back the next hole this job's escalations dug (own-holes, each in another's zone, claim or plan: undoing the
  trespass, so the place ignores zones), lowest first, with jobs.blocks.place (the dug block or what it drops), one
  child call each. It runs once the body got on past the escalation (restore-next!). A hole
  skip-why refuses, or the place child would wait on (nothing to place), stays in the ledger for restore-broken (a
  :changed one is forgotten). A placed one is forgotten."
  [c]
  (let [p (:primitives c)
        seen (:restore-seen (ctx/mem c) #{})
        e (first (remove #(seen (:cell %)) (own-holes c)))]
    (if (nil? e)
      (do (restore-done! c) :again)
      (let [{:keys [cell was any-of]} e
            args {:pos cell :any-of any-of :fetch false :ignore-zones? true}
            done! (fn [k entry]
                    (ctx/update-mem! c #(-> % (update :restore-seen (fnil conj #{}) cell) (update k (fnil conj []) entry)))
                    :again)
            skip! (fn [why]
                    (if (= :changed why)
                      (do (tidy/forget-cell! c cell) (done! :changed cell))
                      (done! :skipped {:cell cell :block was :why why})))]
        (if-let [why (skip-why c e)]
          (skip! why)
          (if-let [wait (b/child-wait c :restore 'jobs.blocks.place args)]
            (skip! (:reason wait))
            (let [_ (await (torch/return-torch! c))
                  r (await (ctx/call-child c :restore 'jobs.blocks.place args))
                  res (ctx/child-result c :restore)]
              (cond
                (= :continue r) :continue
                (= :declined r) (skip! (:reason (b/child-wait c :restore 'jobs.blocks.place args) :declined))
                (:placed res) (do (tidy/forget-cell! c cell) (note-placed! c cell) (done! :restored cell))
                :else (skip! (:reason res))))))))))

(defn restore-next!
  "The body got on past an escalation (a walk round that arrived or made progress): put back its holes next round
  (restore-round!). Not before: a stair or door short of the way on is still the body's way, and refilling it under
  the body would strand it."
  [c]
  (ctx/update-mem! c #(-> % (dissoc :restore-pending) (assoc :restore-now true)))
  :again)
