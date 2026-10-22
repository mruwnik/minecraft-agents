(ns jobs.movement.go-to
  (:require [engine.args :as a]
            [clojure.string :as str]
            [engine.contract :as contract]
            [jobs.lib.cond :as cond]
            [engine.ctx :as ctx]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.health :as cost-health]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.need :as need]
            [jobs.lib.ready :as ready]
            [jobs.lib.pace :as pace]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.air :as air]
            [jobs.movement.go-to.aim :as aim :refer [args-refusal goal-danger goal-pos pre-aim!]]
            [jobs.movement.go-to.column :as column]
            [jobs.movement.go-to.cut :refer [approaches cut-loop? nearest-hostile note-restart! start-attempt! way-hostiles]]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.faults :as faults]
            [jobs.movement.go-to.health :as health]
            [jobs.movement.go-to.holes :refer [note-holes!]]
            [jobs.movement.go-to.reprice :as reprice]
            [jobs.movement.go-to.restore :as restore]
            [jobs.movement.go-to.preview :as preview]
            [jobs.lib.walk-risk :as risk]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.tuning :as tuning :refer [max-cut-resets]]
            [jobs.movement.go-to.torch :as torch]
            [jobs.movement.go-to.walk :as walk]
            [jobs.movement.go-to.way-steps :as ways]))

(def doc
  "Walk to :pos ([x y z] or {:x :y :z}, fractions floored to the cell) until the body's cell is within :range cells of
  it (a sphere; range 0: in that cell, 1: next to it). :pos [x z] (no y) walks to the seen ground of that column nearest the
  body's feet (jobs.movement.go-to.column); :place (a name such as :home) walks to that place's recorded position
  instead, a name not in memory falling back to the shared marker of that name (jobs.lib.world/marker). With :see a cell
  counts only where the eye sees the block at :pos (jobs.lib.aim); a search that finds no such stand gives up :why :out-of-sight.
  How it walks, escalates, climbs and swims is in the docstrings of jobs.movement.go-to.* (walk, escalation, own-way, air, torch, health, preview).
  - Refused at submit (or the caller's ctx/call throws): an arg its spec refuses, such as a :tolls entry that is not {:x :y :z :factor} (finite cell, factor 0 or more), a :gait that is not :auto, :walk or :sneak.
  - Ends at once, before any walk, data {:arrived false :text}: impossible :bad-pos (a :pos
    outside the world), :bad-name (a :place that is not a valid name), :unsupported (a body with no pathWorld sensing)
    or :bad-args (a given :floor :health over 20, a :spend :job-time under 0.1); condition-failed :unknown-place (no
    recorded position). A call whose :ways leave out :walk ends nothing-possible :no-way.
  - Ways (:ways, all by default): :walk, and those that change the world: :clear-seal (its own ledger blocks round the body shut in by them, or round a
    goal it cannot reach taken back, one jobs.access.cleanup child a call), :pillar (a pillar, a gap filled with a
    block), :clear-path (a door dug through a wall), :stair (a stair up, its own stair cut again) and :dig-out (out of
    its own shelter).
  - Its safe? is: no hostile near (a walk leading away from every hostile near is not held, unless one catches the walk (ready/melee-catches?) or has hurt the body lately) and the walk's risk acceptable (jobs.lib.walk-risk: the expected damage of mobs spawning on dark open stretches by the light seen, of the hostiles near the way that catch it, and of a recent death by the goal, within a share of the health; else later :risky with the figures; no wait for dawn); always ok with the head under water (it swims up for air first). It is asked again mid-walk on a re-plan that knows of a new hostile near the way (jobs.movement.go-to.reprice): when it fails the round ends as a failed can-run? would. A caller's :checks (an escape job's
    defaults, an agent's own) replace it.
  - A body already within :range of a goal with a y (not in water or hanging; under :see seeing it) ends :done, cause :already, before any round: no plan, no arrival look (done?).
  - One call is one whole attempt: it plans and walks (jobs.lib.walk: slices of about 100 ms of search, walks of at
    most 60 s each) until it arrives or gives up. A search that needs more slices walks on toward where it has got
    to, or not at all while it goes on; every iteration awaits a pace-ms timer. A goal in unloaded land is walked
    toward walk by walk, and to the edge of loaded land when that is the only way on. It returns :continue only while
    an escalation or put-back child is waiting on the world. A round that ran out of re-plans while each learned an unseen cell counts as progress (no try booked). Each fruitless round that plans again says so (info go-to.retrying: reason, try); each books a try, and a call whose :spend :tries (its own, or a caller's least) is used up
    gives up instead (a caller that re-aims itself passes {:tries 1}).
  - Gives up with the cause {:reason :unreachable :why ...}, data {:arrived false :why ... :text
    words ...}: limit-reached for :needs-health (:line :health) and :search-spent (:line :job-time); condition-failed
    for :goal-dangerous and :goal-unloaded; else (:explore-spent too) nothing-possible. :why is the planner's
    reason (:exhausted, :goal-unloaded (the goal lies in unloaded land and the loaded land leads no nearer), :goal-enclosed, :goal-cut-off, :goal-on-magma, :door-stuck with :cells, :one-way with :near and :one-way ...), or :stuck
    (:kind the step, :detail the executor's text), :off-plan, :steer-failed, :no-progress, :cut-again (sent back by 3 cuts, each restart more than send-back-margin farther than where the cut found the body, with no 2 blocks gained on the closest approach in between (in-place cuts do not count or break it); a restart that knows of no hostile within the hostile-near radius of the way ahead clears the count instead; an escalation child's own walk is its own call, a parent waiting on it is not walking; :nearest-hostile names a guess at the cause, :nearest-hostile-at its cell, or {:direction :band} when only heard) or :moved-while-searching; :at is the
    body's feet cell and :near its blocks from the goal. A failed
    escalation adds :escalation {:step :reason ...}, the child's reason or wait.
  - Success is :done with data {:arrived true}, with :resolved the cell when go-to chose the y, plus :provisional true (a column's
    estimated y) or :stand-in true (a column with no ground known: the feet y). A y goal moved to the ground of its column (:why :explicit)
    adds :re-aimed true, :aimed-at the cell reached and a :text saying the goal cell was not where the body stands: a caller that needs the exact cell checks :re-aimed.
  - Every walk writes a :moved memory entry {:from :to :status :target} (arrived, partial or blocked) for
    the stuck trigger.
  - :doors (jobs.lib.pass): :shut (default) opens a shut door, gate or trapdoor with an empty hand, passes and
    shuts what it opened. :leave-open leaves it open. :never treats them as walls. A door in or beside another
    owner's zone is always shut again. A door that will not open is a wall for that walk (:door-stuck). Iron
    doors are walls.")

(a/defargs args
  {:pos {:doc "target position [x y z] or {:x :y :z}; [x z] or {:x :z}: the surface of that column (highest seen stand open to the sky; a column seen only under cover: the stand nearest the feet)" :spec column/pos-or-column? :default nil}
   :place {:doc "name of a place in body memory (:home, :bed, ...) to walk to instead of :pos" :spec (a/or-of keyword? string?) :default nil}
   :range {:doc "how close counts as there: arrive within this many cells of :pos (a sphere)" :spec (a/num-in 0 nil) :default 1}
   :see {:doc "true: a cell within :range counts as there only when the eye there sees the block at :pos (jobs.lib.aim: a clear line over seen cells to its centre or a face), as a body that digs, places or uses it needs; a [x z] column goal ignores it" :spec boolean? :default false}
   :doors {:doc "what to do at shut doors, gates and trapdoors: :shut (open, pass, shut again what the walk opened), :leave-open (open and pass), :never (walls)"
           :spec #{:shut :leave-open :never} :default :shut}
   :unknown {:doc "true: plan over what the body has seen or felt only, unseen floors checked by eye before they are stepped on; false: plan over the world as it is" :spec boolean? :default true}
   :torches {:doc "with :unknown, how a dark floor the body cannot see is lit: :never (no torch is placed), :carried (default: a torch from the inventory, placed at the takeoff, priced by jobs.lib.cost/light-cost), :fetch (the same, fetching torches first when none is carried: a jobs.lib.need child)" :spec #{:never :carried :fetch} :default :carried}
   :tolls {:doc "cells to cross only as a last resort, [{:x :y :z :factor}]: each costs factor times its own seconds more; ##Inf: no way through it is planned (jobs.lib.cost farm-tolls, zone-tolls; jobs.lib.pen-entry gives a block job the gates of pens holding animals)" :spec (a/valid-by "a list of {:x :y :z :factor}, x y z finite numbers, :factor >= 0 (##Inf allowed)" wworld/tolls-problem) :default nil}
   :gait {:doc "how the body walks: :auto (sprints on long straight runs and over gaps, as the walker does), :walk (never sprints: no gap jump over 2 or more), :sneak (slow, never sprints; holds sneak on level steps so it never walks off an edge, lets go for a planned drop, gap, climb or water); nil: the body's setting jobs.lib.walk.world/gait (:auto)" :spec #{:auto :walk :sneak} :default nil}
   :one-way {:doc "arg, not the :one-way key of a give-up result: :closed takes no drop of 2 or 3 or gap jump down that the body cannot climb back, and walks to no frontier of loaded land (a walk to something visible); :open (default) takes one when the land past it runs on into unloaded land (under :unknown also land the body has not seen), or a plan that stops short of the goal (a plan to it over seen land takes any drop), and the landing's expected loss (jobs.lib.walk.one-way: the chance it traps the body, a floor not seen lit far more than one seen lit or reached with a light carried, times ::trap-s) is within the goal's worth (:worth, else a tour's worth)" :spec #{:open :closed} :default :open}
   :worth {:doc "seconds the caller counts reaching the goal worth, nil or 0: not said. It budgets a tour of frontiers when no route is known (that less the trip and the straight walk; nil: a few times the straight walk) and lets the start check accept a route risk over the share of health it allows, up to all but 1 hp, when the hp cost (jobs.lib.cost/hp-seconds, dearer when hurt) is within it" :spec (a/num-in 0 nil) :default nil}
   :ignore-zones? :shared})

(defn ^:async step!
  "One iteration of a call: :again to go on in the same call, else what the call returns."
  [c0 pos]
  (let [c (health/with-body-food c0)
        m (ctx/mem c)]
    (when (:unknown c)
      (wworld/with-seeds (:unknown c) (when (:y pos) (mapv #(js/Math.floor (% pos)) [:x :y :z])) (into (ledger/stair-stands c) (ledger/tunnel-stands c))))
    (cond
      (:restore-now m) (await (restore/restore-round! c))
      (:escalation m) (await (esc/escalation-round! c pos))
      (:escalate-now m) (do (ctx/update-mem! c dissoc :escalate-now)
                            (await (esc/escalate! c pos nil)))
      :else (or (when (:torch-fetching m) (await (torch/fetch-torches! c)))
                (await (walk/walk! c pos
                              (cond (and (= :guess (:column-open m)) (not (:column-near m))) (max (:range (:args c)) column/approach-range)
                                    :else (:range (:args c)))
                              (some-> (:doors (:args c)) keyword)))))))

(def target [:pos :place])

(def ways ways/ways)

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one go-to call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds one go-to call may last from its start, the waits on its checks included (:limit-reached)." :spec (a/num-in 0 nil)}})

(def meta {:own-tries? true})

(def defaults {:least :all :spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(defn limits-refusal
  "The refusal {:reason :bad-args :message} of the call's given :floor :health or :spend :job-time, nil when in range."
  [{:keys [floor spend]}]
  (let [h (:health floor) t (:job-time spend)]
    (cond
      (and h (> h 20)) {:reason :bad-args :message (str ":floor :health must be 0 to 20, got " h)}
      (and t (< t 0.1)) {:reason :bad-args :message (str ":spend :job-time must be at least 0.1 s, got " t)})))

(defn walk-limits
  "The walk's {:min-health :allow-over-budget :max-damage} from the call keys given and frozen (with a caller's
  floor): the frozen floor :health, a caller's only above default-floor, is :min-health; a given floor of 1 or less that
  stays so is :allow-over-budget; the given :spend :health is :max-damage."
  [given frozen default-floor]
  (let [g (get-in given [:floor :health])
        f (or (get-in frozen [:floor :health]) g)
        {:keys [health]} (:spend given)]
    (cond-> {}
      (and f (if g (> f 1) (> f default-floor))) (assoc :min-health f)
      (and g (<= f 1)) (assoc :allow-over-budget true)
      (some? health) (assoc :max-damage health))))

(defn with-limits
  "c with walk-limits in its args, where the walk code (jobs.lib.walk.world/walk-settings) reads them."
  [c]
  (update c :args merge (walk-limits (:args c) (ctx/frozen-call c) (cost-health/default-min-health))))

(defn ^:async quote-of
  "The planner's route to the goal from where the body stands, with this call's policy, costs, dangers, tolls and
  limits, walked nowhere (go-to.preview): time from its seconds, health its planned damage, :moves-to the goal cell,
  :route what the planner found."
  [c]
  (let [c (with-limits (health/with-body-food c))]
    (preview/quote-of-route (await (preview/route! c)) (ways/escalating? c))))

(defn limits-fit
  "Never :bad-args for a floor or leg out of range, before the engine's floor check could hold the call."
  [c]
  (if-let [{:keys [message]} (limits-refusal (:args c))]
    (contract/never :bad-args message)
    (contract/ok :args "args fit")))

(def can-start? limits-fit)

(defn goal-of "The cell go-to walks to, nil when its target is refused (the round says why)." [c]
  (let [t (aim/target c)] (when-not (:reason t) (:pos t))))

(def safe?
  "Not threatened (a walk leading away from a hostile is not held) and the walk's risk acceptable (risk/status). Always ok
  with the head under water: breathing comes before any wait, and go-to swims up for air first."
  (with-meta (fn [c] (if-let [goal (goal-of c)] (reprice/status c goal) (contract/ok :no-goal "no goal to judge")))
    {:jobs.lib.ready/exposure (fn [c] (if-let [goal (goal-of c)] (risk/status c goal) (contract/ok :no-goal "no goal to judge")))}))

(def fit? ready/ok)

(defn done?
  "Ok before the call starts when the body already stands within :range of a goal cell with a y (and, under :see, sees
  it), not in water and not hanging in a climbable: nothing to plan or walk, the engine ends it :done (cause :already)
  with {:arrived true}. Later once started (the walk decides arrival: put-back of holes, looks, water), for a column
  goal, a :place not yet recorded and any other case."
  [c]
  (let [later (contract/later :walking "not at the goal yet")
        {:keys [pos reason]} (when (:quote-call c) (aim/target c))]
    (if (or reason (nil? (:y pos)))
      later
      (let [c (cond-> c (:unknown (:args c)) (assoc :unknown (wworld/unknown-spec)))
            stand (wworld/stand-goal (wworld/path-world (:primitives c) (:unknown c)) pos)]
        (if (and (aim/here? c stand (:range (:args c)))
                 (not (breath/in-water? (:primitives c)))
                 (not (faults/hanging-cell c stand)))
          (contract/ok :there "already within range of the goal" :result {:arrived true})
          later)))))

(defn ^:async run-steps!
  "pace/steps! over step!, goal aimed again each step (goal-pos), so a column seen on the way resolves; the approach
  record and :open go when the attempt ends (a cut keeps them)."
  [c goal]
  (let [r (await (pace/steps! c (fn [] (step! c (goal-pos c goal)))))]
    (when (ctx/alive? c) (swap! approaches dissoc (:id c)) (ctx/update-mem! c dissoc :open))
    r))

(defn ^:async attempt
  "One whole attempt: step! until it arrives, gives up, or waits on a child (:continue), with pace! between steps. A
  cut ends it at once (ctx/alive? per iteration, else at the next memory write or act, which throws). A goal inside a
  fled mob's radius ends it at once under the goal-danger setting :end, else says so once. With :unknown, the attempt's mask spec
  (wworld/unknown-spec: one missed Set for all its plans) rides on c as :unknown."
  [c0]
  (let [u (:unknown (:args c0))
        c (cond-> c0 u (assoc :unknown (wworld/unknown-spec)))
        refusal (args-refusal c)
        goal (when-not refusal (:pos (aim/target c)))
        pos (goal-pos c goal)]
    (cond
      refusal (end/refuse! c refusal)
      (not (ways/way? c :walk)) (end/refuse! c {:reason :no-way :message "the call's :ways leave out :walk"})
      :else
      (let [state (note-restart! c pos)
            spent? (and (cut-loop? state) (>= (:resets state) (max-cut-resets)))
            cycle? (and (cut-loop? state) (or spent? (seq (way-hostiles c pos))))]
        (await (torch/return-torch! c))
        (await (start-attempt! c pos))
        (when (and (cut-loop? state) (not cycle?))
          (ctx/update-mem! c assoc :cuts 0 :ref (:closest state) :resets (inc (:resets state))))
        (if cycle?
          (do (ctx/update-mem! c #(-> % (dissoc :open :goal :closest :ref :last-dist) (assoc :cuts 0)))
              (await (end/give-up! c pos {:reason :cut-again} (cond-> (or (nearest-hostile c pos) {})
                                                                         spent? (assoc :resets-spent true)))))
          (let [danger (goal-danger c pos)]
            (when danger (ctx/emit! c :goal-in-danger :info (assoc danger :target pos)))
            (if (and danger (= :end (tuning/goal-danger)))
              (await (end/give-up! c pos {:reason :goal-dangerous} danger))
              (do (pre-aim! c goal pos)
                  (await (run-steps! c goal))))))))))

(defn ^:async round
  "One attempt (see attempt)."
  [c]
  (await (attempt (with-limits c))))

(defn ^:async cleanup
  "However the call ended (a cut too): the planned cells of an escalation child cut mid-round are noted to the tidy
  ledger, the off-hand torch goes back and the planner's kept state is released."
  [c _]
  (when-not (:own (:escalation (ctx/mem c))) (note-holes! c (:planned (ctx/mem c))))
  (wsearch/release! c)
  (choose/forget-routes! (:route-owner (ctx/mem c)))
  (await (torch/return-torch! c))
  (ctx/end c :done nil {}))
