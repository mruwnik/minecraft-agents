(ns jobs.movement.go-to
  (:require [engine.args :as a]
            [clojure.string :as str]
            [engine.contract :as contract]
            [jobs.lib.cond :as cond]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.health :as cost-health]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.watch :as wwatch]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.air :as air]
            [jobs.movement.go-to.aim :as aim :refer [args-refusal goal-danger goal-pos pre-aim!]]
            [jobs.movement.go-to.column :as column]
            [jobs.movement.go-to.cut :refer [approaches cut-loop? nearest-hostile note-restart! start-attempt! way-hostiles]]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.restore :as restore]
            [jobs.movement.go-to.holds :refer [night-wait plug-since plug-wait wait-reason]]
            [jobs.movement.go-to.preview :as preview]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.tuning :refer [max-cut-resets night-wait-ms]]
            [jobs.movement.go-to.torch :as torch]
            [jobs.movement.go-to.walk :as walk]
            [jobs.movement.go-to.way-steps :as ways]
            [jobs.lib.child :as child]))

(def doc
  "Walk to :pos ([x y z] or {:x :y :z}, fractions floored to the cell) until the body's cell is within :range cells of
  it (a sphere; range 0: in that cell, 1: next to it); with :see a cell in it counts only where the eye sees the block at
  :pos (jobs.lib.aim), and a search that finds no such stand gives up :why :out-of-sight. :pos [x z] or {:x :z} (no y) walks to the seen ground of that
  column nearest the body's feet (unseen: first to within 8 of a height estimated from the seen columns around it, then looks); a y the planner finds no standable cell within :range of, or under :unknown a y seen in the ground with no seen stand within :range (asked before the first walk, pre-aim!), is re-aimed once to its column's
  ground nearest it, at most :y-slack blocks off and never past a solid (info goal-resolved {:goal :to}); see jobs.movement.go-to.column. A
  column whose ground is not made out is looked at after a fruitless walk (at most 3 a call); with no stand in reach of its guess it walks to seen stands toward it (column/waypoint, off the line past a cliff), until ::max-waypoint-misses bring it no nearer. A cell over a short floor (farmland, a path, a bottom slab) stands for that
  block's own cell, where the body's feet are. :place (a name such as :home, set by jobs.memory.set-place) walks to
  that place's recorded position instead of :pos; a name not in memory falls back to the shared marker of that name (jobs.lib.world/marker).
  - Refused at submit (or the caller's call-child throws): an arg its spec refuses, such as a :tolls entry that is not {:x :y :z :factor} of finite numbers, a :drop-cost that is not a number >= 0 or false, a :costs that is not a map of known price names to numbers >= 0, a :landing that is not a map of block names to numbers, a :gait that is not :auto, :walk or :sneak, a :danger-shape that is not {:sensed|:remembered|:creeper {:close :radius}}.
  - Ends at once, before any walk, after a :refused warn, data {:arrived false :text}: impossible :bad-pos (a :pos
    outside the world), :bad-name (a :place that is not a valid name), :unsupported (a body with no pathWorld sensing)
    or :bad-args (a given :floor :health over 20, a :spend :job-time outside 0.1-120); condition-failed :unknown-place (no
    recorded position). A call whose :ways leave out :walk ends nothing-possible :no-way.
  - Ways (:ways, all by default): :walk, and those that change the world: :clear-seal (its own ledger blocks round a
    goal it cannot reach taken back, one jobs.access.cleanup child a call), :pillar (a pillar, a gap filled with a
    block), :clear-path (a door dug through a wall), :stair (a stair up, its own stair cut again) and :dig-out (out of
    its own shelter).
  - A listed go-to waits (its condition, before it starts) while it would wait on the night or a retreat plug (below);
    once started such a wait is a :continue yield, kept in its memory as :hold for a parent's jobs.lib.child walk.
  - One call is one whole attempt: it plans and walks (jobs.lib.walk: slices of about 100 ms of search, walks of at
    most 60 s each) until it arrives or gives up. A search that needs more slices walks on toward where it has got
    to, or not at all while it goes on; every iteration awaits a pace-ms timer. A goal in unloaded land is walked
    toward walk by walk, and to the edge of loaded land when that is the only way on. It returns :continue only while
    an escalation or put-back child, or healing, is waiting on the world. Each fruitless round that plans again says so (info go-to.retrying: reason, try).
  - A drop or plant that costs hp is planned only within a damage budget (jobs.lib.cost/damage-budget: the health over
    the call's :floor :health, default 12, less 1, at most its :spend :health; a caller's floor counts only above 12
    unless this call gives one, walk-limits). When that budget is why no way was found, the body that can heal waits
    (info go-to.waiting-health, :continue; it eats a carried food first when hungry), and one that cannot heal or whose
    healing stalled goes over it, down to 1 hp (info go-to.over-budget) only with a given :floor :health of 1 or less;
    otherwise, and with a higher floor or a :spend :health, it gives up :why :needs-health. See jobs.movement.go-to.health.
  - :unknown true (default false) plans over what the body has seen or felt only: a cell it never saw is unknown (air to
    the body), a floor it never saw is stepped onto only after the walk stops to look at it (jobs.lib.walk.sight), and
    only where it can see it from the takeoff (a drop deeper than it sees in the dark is refused). An unseen y goal
    behind a seen solid, or one a found plan tours round (jobs.movement.go-to.walk/lookout-check), is first looked at
    from the nearest seen stand within 4 of it that sees it (info go-to.lookout, at most 3 stands a call). A map overrides the
    prices of jobs.lib.cost/unknown-prices. When a dark drop was the better way it gives up :why :unseen-floor-dark; in the
    dark with a torch carried and the off hand empty it first holds the torch in the off hand (info go-to.off-hand-torch,
    once a call: it sees deeper) and plans again; it puts the torch back into the inventory before an escalation child runs and when the call ends (full: it
    stays, info go-to.off-hand-kept); one a cut call left in the hand goes back at the next call's start.
  - :place-torches true (with :unknown; default false) may light such a dark floor instead: the planner prices a torch
    (jobs.lib.cost/light-cost, or :unknown's :light-cost) where it can stand on the takeoff, in no other owner's zone or
    claim; the walk stops there, places one (an off-hand stack goes back to the inventory first; info walk.torch-placed) and
    walks on once it sees the floor. With none carried it fetches torches once a call (:fetch, default true; a jobs.lib.need child);
    with none and no fetch it gives up :unseen-floor-dark saying so. A torch that cannot go there is not tried there again.
  - :dark false plans dark cells like lit ones (default: a dark cell costs twice a lit one, so a lit route up to about 2x
    longer is taken: a cell seen and dark, or one never seen at night; look/dark-fn).
  - :at-night :wait (default: the setting ::tuning/at-night): at night a walk that would take the body out of the shelter it stands in
    (jobs.lib.shelter/walk-leaves-shelter?: a closed room, its dug-in shelter, a roof, underground; a walk down to open air leaves a roofed hut) waits for day: go-to waits 1 s and yields :continue (cause carries :shelter; info
    go-to.night-sheltered, once a call). :go walks anyway; a reflex job's go-to always walks.
  - A walk with no escalating way from a body shut in by its own retreat plug (ledger) with a known hostile near waits the same way, {:reason :sealed-hostile-near}, until the hostile goes (a heard-only one counts not beyond the near band, 8 blocks, nor while heard mobs are muted, jobs.lib.body/heard-muted?), :plug-wait-ms (3 min, in all across the go-to children of one root job) pass, or food or health fall to :plug-wait-min-food or -health (it never holds a starving or hurt body).
  - :goal-danger :end gives up at once (:why :goal-dangerous, :mob, :mob-pos, :mob-distance) when the goal lies within the full-cost radius of a known mob the body would flee; :wait (default) walks on, an info goal-in-danger says so.
  - :dangers false plans straight past known dangers (default: the plans keep away from them, jobs.lib.threats). A
    :spend :job-time of n walks one leg of at most n s and, when it got more than 1 block nearer, ends limit-reached
    {:reason :leg :line :job-time}, data {:arrived false :leg true}, for a caller chasing a moving target to call again; a
    leg that got no nearer goes on as any round, and a search that goes on past n s gives up :leg-search-spent.
  - An arrival in water walks on to the nearest seen dry stand within :range and ::dry-radius of the body (settings), once
    a call, a short walk of its own; with none, or that walk failing, it ends {:arrived true :in-water true} with an info
    go-to.in-water saying which. :leave-water false keeps an arrival in water. Under water with its search going on (no
    walk) it swims to air first (go-to.surfacing) when under ::surface-air seconds of air are left (always), or when the
    search began afresh more than ::wet-restarts times (at most 3 such swims in a row that leave the head under).
  - A call starts afresh from the world: its walk counters are per call (the cut count carries over only a cut of this run's call to the same goal), and a saved escalation is dropped when the body
    is already there (a cut call's memory is a hint).
  - A walk that gets more than 1 block nearer is progress. Three walks in a row without progress give up (or escalate, below), and so
    does a goal the planner proves walled in (:goal-enclosed, :goal-cut-off) or on magma (:goal-on-magma), at once; a
    walled-in goal it does not escalate into is first walked up to (the cell nearest it the body can reach, once a call, when
    that walk fits the time left; info go-to.wall-walk).
  - A search that runs out of loaded land walks to its frontier (the loaded edge), and a walk that gets nearer that edge
    is progress too: a tour of frontiers ends when every frontier within 1000 blocks of the goal is spent, or at the end of
    a frontier walk once the tour's price (jobs.lib.cost/tour-cost of the blocks and walks since the body last came nearer its goal than
    ever in the call) passes what the goal is worth (:tour-worth, default jobs.lib.cost/tour-worth of the goal's straight
    blocks at the call's start): it gives up :why :explore-spent (:explored the blocks, :walks the walks).
  - A walk stuck or off its plan at a cell is a :walker-fault warn; the next plans of the call cost that cell
    (up to 16; after a fall also the cells above it) far more (wworld/with-avoid), so a way round is taken when there is one.
    A body such a walk left stuck in water first swims at a bank beside it (go-to.unstick, info go-to.unstuck).
  - :dig-out: a body sealed in its own shelter (a :shelter entry) by day, when the walk finds no way, leaves it first (dig-in-leave/leave!, a go-to.escalated {:step :leave-shelter}); never at night.
  - With an escalating way: a body that is shut in (jobs.lib.reach/enclosed?), not in its own shelter, and whose
    search ran out of land (:exhausted, :goal-enclosed or :goal-cut-off), any body whose only way needs a sprint it lacks the food for (:abilities :corner-jump or :gap-sprint), or any body whose last 3 walks failed at the walker (stuck, off-plan, steer failed, no nearer; escalation/walker-fault?) with no door it can open beside its room, makes a way instead of giving up, one child job
    each (jobs.lib.escape/choose, over the ways the call allows), at most 3 in a row that start no nearer the goal than one before (escalation/count-escalation): standing on a stair of its own under the goal, that stair cut again (escalation/own-stair: a cap it put in the stair is dug, :why :own-way-blocked); else jobs.access.pillar up out of a pit when it carries enough blocks; else
    jobs.access.clear-path through a wall up to 3 thick toward the goal (into a walled-in goal no door or stair reaches:
    the fewest cells dug down or across to it, escalation/dig-way, tried first for a shut-in body); else jobs.access.stair up out of a pit or
    toward a higher goal (as far as the goal is above, at most 8; a stair that leaves the body shut in under the goal is
    followed by the next escalation at once); else a walk to the nearest wall first. Each emits go-to.escalated {:step :why :n}. Once
    through, it puts back what clear-path or stair dug in another's zone, claim or plan (with :ignore-zones?;
    its own diggings elsewhere stay) (jobs.blocks.place, the dug block or its drop, when carried): info
    go-to.restored, warn go-to.restore-skipped for cells it could not. Pillar blocks and a stair's placed floors are
    left to jobs.access.cleanup. Its walk child (ctx/call) has :ways #{:walk :clear-seal}. Jobs that must not change the world
    on the way pass :ways #{:walk}: a shut-in body then gives up with :shut-in true and says so in the text.
  - Gives up with an :unreachable warn and the cause {:reason :unreachable :why ...}, data {:arrived false :why ... :text
    words ...}: limit-reached for :needs-health (:line :health) and :leg-search-spent (:line :job-time); condition-failed
    for :goal-dangerous and :goal-unloaded; else (:explore-spent too) nothing-possible. :why is the planner's
    reason (:exhausted, :goal-unloaded (the goal lies in unloaded land and the loaded land leads no nearer), :goal-enclosed, :goal-cut-off, :goal-on-magma, :door-stuck with :cells, :one-way with :near and :one-way ...), or :stuck
    (:kind the step, :detail the executor's text), :off-plan, :steer-failed, :no-progress, :cut-again (sent back by 3 cuts, each restart more than send-back-margin farther than where the cut found the body, with no 2 blocks gained on the closest approach in between (in-place cuts do not count or break it); a restart that knows of no hostile within the hostile-near radius of the way ahead clears the count instead; an escalation child's own walk is its own call, a parent waiting on it is not walking; :nearest-hostile names a guess at the cause, :nearest-hostile-at its cell, or {:direction :band} when only heard) or :moved-while-searching; :at is the
    body's feet cell and :near its blocks from the goal. A failed
    escalation adds :escalation {:step :reason ...}, the child's reason or wait.
  - Under water with no way to the goal in the air left (:air), it first walks to the nearest air it sees or finds by
    looking (jobs.movement.go-to.air, info go-to.surfacing, at most 3 a call) and plans again.
  - Success is :done with data {:arrived true}, with :resolved the cell when go-to chose the y, plus :provisional true (a column's
    estimated y) or :stand-in true (a column with no ground known: the feet y). The result is also a :result info event.
  - A body under ground (jobs.lib.shelter/buried?) whose goal lies ::climb-rise blocks or more above it plans no route
    deeper than ::climb-dip blocks below where the call first found it so (it does not dive through deep caves to come
    up under the goal). With no other way it cuts a stair up toward the goal instead (setting ::climb-stair, with
    :stair: the escalation's stair, a go-to.escalated {:step :stair :why :way-dives}), stair after stair with no walk
    in between while it is under ground below the goal; else, or when no stair can be cut, it gives up :why :way-dives
    with :dip and :rise. Such a body (jobs.movement.go-to.climb):
    - first walks its own way up, once a call (setting ::climb/own-way): over its stairs and tunnels (memory :stair-way,
      :tunnel-way) and standable cells it has seen, to the nearest of its stairs that stands at the goal's height or
      at its top under the open sky (a stair that ends in rock is no way), in hops of ::climb/own-way-reach; the walk is weighed
      against a new stair first (info go-to.own-way {:take :to :blocks :walk-s :cut-steps :cut-s}) and a slower one
      is not taken; a hop it cannot walk, standing on such a stair, has the stair cut again from
      there (a go-to.escalated {:step :stair :why :own-way-blocked}; these holes stay open);
    - walks no plan (first or re-plan inside a walk) that goes more than ::climb/cave-dip below where the call began over cells that are no stair of
      its own, while a stair up can be cut there (with :stair and ::climb-stair): it climbs as from a dive
      (:why :way-dives); when no stair can be cut it walks the plan after all. Likewise, once at the end of its own
      way (memory :own-way-top), a plan that goes more than ::climb/cave-dip below it back down a stair of its own.
    An explicit :y-margin switches all of this off.
  - :detour (default true): a body under ground (jobs.lib.shelter/buried?) whose goal is under ::climb-rise above it
    walks no found way longer than ::detour/ratio times the distance to its goal at the call's start (at least
    ::detour/floor blocks) when half or more of its cells were never seen: it gives up :why :detour with :route,
    :straight and :allowed (blocks) before it sets out. :detour false walks it. See jobs.movement.go-to.detour.
  - Its quote (./bin/quote) is a dry run: one plan from where the body stands with the call's args, nothing walked,
    opened or dug. :uses :job-time [s s 3s] from the planner's seconds (partial or no route: no most; none and no way that
    changes the world: ##Inf), :health the planned damage, :moves-to the goal cell, and :route {:found :length :seconds
    :summary :steps :moves :doors :waypoints :damage :drown :nodes :dark :dark-seconds} (:dark: feet cells seen dark, or unseen at night; :dark-seconds the planner's extra seconds for them), or {:found false :reason :near :partial} (:reason
    as :why, :bad-pos and other refusals, :late past the quote deadline); with :unknown :unseen-floors and :dark-best.
  - Every walk writes a :moved memory entry {:from :to :status :target} (arrived, partial or blocked) for
    the stuck trigger.
  - :doors (jobs.lib.pass): :shut (default) opens a shut door, gate or trapdoor with an empty hand, passes and
    shuts what it opened. :leave-open leaves it open. :never treats them as walls. A door in or beside another
    owner's zone is always shut again. A door that will not open is a wall for that walk (:door-stuck). Iron
    doors are walls.")

(a/defargs args
  {:pos {:doc "target position [x y z] or {:x :y :z}; [x z] or {:x :z}: the ground of that column nearest the body's feet" :spec column/pos-or-column? :default nil}
   :place {:doc "name of a place in body memory (:home, :bed, ...) to walk to instead of :pos" :spec (a/or-of keyword? string?) :default nil}
   :range {:doc "how close counts as there: arrive within this many cells of :pos (a sphere)" :spec (a/num-in 0 nil) :default 1}
   :see {:doc "true: a cell within :range counts as there only when the eye there sees the block at :pos (jobs.lib.aim: a clear line over seen cells to its centre or a face), as a body that digs, places or uses it needs; a [x z] column goal ignores it" :spec boolean? :default false}
   :y-slack {:doc "blocks a :pos y with no standable cell within :range may be re-aimed up or down its column to the ground; 0: never" :spec (a/int-in 0 nil) :default 4}
   :doors {:doc "what to do at shut doors, gates and trapdoors: :shut (open, pass, shut again what the walk opened), :leave-open (open and pass), :never (walls)"
           :spec #{:shut :leave-open :never} :default :shut}
   :dangers {:doc "false: plan straight past known dangers (a walk up to the hostile being fought); true: keep away from them" :spec boolean? :default true}
   :unknown {:doc "true: plan over what the body has seen or felt only, unseen floors checked by eye before they are stepped on; a map of :cell :floor :peek-s :aim-s :see-lit-depth :light-depth overrides their prices and depths (jobs.lib.cost/unknown-prices), :light-cost the seconds of a torch :place-torches places; false: plan over the world as it is" :spec (a/valid-by "true, false or a map of :cell :floor :peek-s :aim-s :see-lit-depth :light-depth :light-cost to numbers >= 0" wworld/unknown-problem) :default false}
   :place-torches {:doc "with :unknown: light a dark floor it cannot see with a torch from the inventory, placed at the takeoff (priced by jobs.lib.cost/light-cost, or :unknown's :light-cost), fetching torches when none is carried (:fetch); false: never place one" :spec boolean? :default false}
   :fetch {:doc "get torches for :place-torches when none is carried (a jobs.lib.need child): true, a set of kinds or a map of limits; false: give up instead" :spec need/option? :default true}
   :dark {:doc "false: plan dark cells like lit ones; true: a dark cell (seen dark, or unseen at night) costs twice a lit one" :spec boolean? :default true}
   :tolls {:doc "cells to cross only as a last resort, [{:x :y :z :factor}]: each costs factor times its own seconds more (jobs.lib.cost farm-tolls, zone-tolls)" :spec (a/valid-by "a list of {:x :y :z :factor} of finite numbers, :factor >= 0" wworld/tolls-problem) :default nil}
   :food {:doc "the food level (0-20) the walk counts on for its damage budget, its sprinting and whether it can heal by waiting (18 or more regenerates); default the body's own" :spec (a/num-in 0 20) :default nil}
   :fall-margin {:doc "hp the falls of a walk may cost over the plan before an info go-to damage-mismatch" :spec (a/num-in 0 nil) :default wwatch/fall-margin}
   :flee-factor {:doc "danger price: a known mob the body would flee costs this times its hp a second (jobs.lib.cost.danger)" :spec (a/num-in 0 nil) :default (:flee cost/danger-stances)}
   :fight-factor {:doc "danger price: a known mob the body would fight costs this times its hp a second" :spec (a/num-in 0 nil) :default (:fight cost/danger-stances)}
   :danger-max-rate {:doc "danger price: hp a second one known danger costs at most; above the default it also raises the total of all dangers a second (planner dangerCap)" :spec a/pos-num? :default cost/danger-max-rate}
   :danger-shape {:doc (str "danger price: {:sensed|:remembered|:creeper {:close :radius}} blocks, full within :close, none past :radius; only the terms given replace the default (" (str/join ", " (map (fn [[k {:keys [close radius]}]] (str (name k) " " close " " radius)) cost/danger-default-shape)) ")") :spec (a/valid-by "a map of :sensed, :remembered or :creeper to {:close :radius} numbers >= 0, :close under :radius" cost/danger-shape-problem) :default nil}
   :hp-seconds {:doc "seconds an hp costs at full health when the planner weighs a drop or a plant's prick against a longer way (more at low health)" :spec a/pos-num? :default cost/hp-seconds}
   :drop-cost {:doc "number: scales the cost of a drop (fall seconds and damage; 1 as is, 0 free, 5 dear); false: no drop of 2 or 3 at all. :one-way :closed instead refuses only a drop the body cannot climb back" :spec (a/or-of (a/num-in 0 nil) false?) :default 1}
   :y-margin {:doc "blocks the walk's search may reach above the higher and below the lower of body and goal (default 96): a route dipping deeper, such as through deep caves, is not planned; nil: as default" :spec (a/int-in 0 nil) :default nil}
   :costs {:doc (str "map of price name to seconds, overriding the planner's price of a move (jobs.lib.cost.planner: " (str/join " " (map name (keys cost/planner-names))) "); a price left out keeps the planner's default; :lethal-air is no price: 1 lets a plan drown the body when no other way exists, else such a plan is refused (:why :air-lethal)" ) :spec (a/valid-by "a map of planner price name to a number of seconds >= 0" cost/planner-costs-problem) :default nil}
   :landing {:doc "map of block name to the share of a fall's damage a landing on it takes (0.2: 80% off), negative: a bounce, no damage but settle time (only onto a pad in view, bouncing blocks or a wall all round, not under :gait :sneak; else the full fall); entries override the defaults hay_block 0.2, honey_block 0.2 and slime_block -1" :spec (a/valid-by "a map of block name to a number" cost/landing-problem) :default nil}
   :gait {:doc "how the body walks: :auto (sprints on long straight runs and over gaps, as the walker does), :walk (never sprints: no gap jump over 2 or more), :sneak (slow, never sprints; holds sneak on level steps so it never walks off an edge, lets go for a planned drop, gap, climb or water); nil: the body's :walk-settings :gait, else :auto" :spec #{:auto :walk :sneak} :default nil}
   :goal-danger {:doc "when the goal lies within the full-cost radius of a known mob the body would flee: :wait (walk on; an info goal-in-danger says so once), :end (give up at once, :why :goal-dangerous with the mob)" :spec #{:wait :end "wait" "end"} :default :wait}
   :keep-out {:doc "cells the walk treats as solid, [{:x :y :z}]: no way through them is planned (jobs.lib.pen-entry gives a block job the gates of pens holding animals)" :spec (a/valid-by "a list of {:x :y :z} of whole numbers" (fn [cells] (when-not (and (sequential? cells) (every? #(and (map? %) (every? integer? (map % [:x :y :z]))) cells)) "keep-out is a list of {:x :y :z} of whole numbers"))) :default nil}
   :zone-tolls {:doc "true: also toll the cells of other bodies' zones near each walk (jobs.lib.toll-cells/zone-walk-tolls), none with :ignore-zones?; for a job that respects zones" :spec boolean? :default false}
   :leg-alert-within {:doc "with :leg-alert-s and a :spend :job-time leg: blocks within which a hostile that attacks at close range (not a ranged mob or an enderman; a creeper counts), or a loss of health since the call began, shortens a leg's search to :leg-alert-s seconds from its start" :spec (a/num-in 0 nil) :default nil}
   :leg-alert-s {:doc "seconds a search may last once the :leg-alert-within alert is on (0.1 to 120)" :spec (a/num-in 0.1 120) :default nil}
   :one-way {:doc "arg, not the :one-way key of a give-up result: :closed takes no drop of 2 or 3 or gap jump down that the body cannot climb back, and walks to no frontier of loaded land (a walk to something visible); :open (default) takes one when the land past it runs on into unloaded land" :spec #{:open :closed} :default :open}
   :detour {:doc "false: under ground, walk a found way however long; true: one over mostly unseen cells and longer than the bound of jobs.movement.go-to.detour for a goal that near is not walked (:why :detour)" :spec boolean? :default true}
   :tour-worth {:doc "seconds a tour of frontiers (walks to the edge of loaded land without coming nearer its goal) may cost (jobs.lib.cost/tour-cost), then it gives up :why :explore-spent; nil: jobs.lib.cost/tour-worth of the goal's straight blocks" :spec (a/num-in 0 nil) :default nil}
   :tour-ratio {:doc "times the walk of its straight blocks (at least :tour-floor-blocks) a goal is worth of a tour, on top of a trip; nil: the setting jobs.lib.cost.value/tour-ratio" :spec (a/num-in 0 nil) :default nil}
   :tour-floor-blocks {:doc "straight blocks a goal counts as at least when worth a tour, so a near goal behind a ravine is still toured to; nil: the setting jobs.lib.cost.value/tour-floor-blocks" :spec (a/num-in 0 nil) :default nil}
   :per-frontier-walk {:doc "seconds each frontier walk of a tour costs on top of its blocks; nil: the setting jobs.lib.cost.value/per-frontier-walk" :spec (a/num-in 0 nil) :default nil}
   :retry {:doc "false: a walk that got no nearer gives up at once instead of walking again (up to 3 times), for a caller that re-aims itself" :spec boolean? :default true}
   :look-round {:doc "false: no look round on arrival, for a caller that keeps moving" :spec boolean? :default true}
   :warn {:doc "false: a give-up or refusal is an info event, not a warn, for a caller that reports the failure itself"
          :spec boolean? :default true}
   :air-looks {:doc "under water with no air known: rounds of looking again for air (goal head, open water ahead), the aim shifted a little each round, before giving up :air" :spec (a/int-in 0 nil) :default air/default-air-looks}
   :air-look-min {:doc "seconds of air left below which it looks again for air no more (:air-looks), the fallback (swim up, breathe) needs them" :spec (a/num-in 0 nil) :default air/default-air-look-min}
   :leave-water {:doc "false: an arrival within range in water stays there (a swim target, a boat), no walk on to a dry stand" :spec boolean? :default true}
   :at-night {:doc ":wait: at night a walk out of the shelter the body stands in waits for day; :go: walk anyway (a reflex job's go-to always walks); nil: the setting jobs.movement.go-to.tuning/at-night (:wait)" :spec #{:wait :go "wait" "go"} :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims in the escalation (pillar, stair, clear-path); the rules of the game allow it" :spec boolean? :default false}
   :spare-own-builds {:doc "the escalation never digs a cell of a plan this body made (stops :own-build); false: it may, and warns of each cell dug" :spec boolean? :default true}})

(defn with-body-food
  "c with the :food arg the caller gave, else the body's own food now (a felt fact): the walk counts on it."
  [c]
  (if (number? (:food (:args c)))
    c
    (let [food (.-food (.self (:primitives c)))]
      (cond-> c (number? food) (assoc-in [:args :food] food)))))

(defn ^:async step!
  "One iteration of a call: :again to go on in the same call, else what the call returns."
  [c0 pos]
  (let [c (with-body-food c0)
        m (ctx/mem c)]
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
  {::job-time-s {:default 1800 :doc "Seconds one go-to call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(defn limits-refusal
  "The refusal {:reason :bad-args :message} of the call's given :floor :health or :spend :job-time, nil when in range."
  [{:keys [floor spend]}]
  (let [h (:health floor) t (:job-time spend)]
    (cond
      (and h (> h 20)) {:reason :bad-args :message (str ":floor :health must be 0 to 20, got " h)}
      (and t (not (<= 0.1 t 120))) {:reason :bad-args :message (str ":spend :job-time must be 0.1 to 120 s (one leg), got " t)})))

(defn walk-limits
  "The walk's {:min-health :allow-over-budget :max-damage :leg-s} from the call keys given and frozen (with a caller's
  floor): the frozen floor :health, a caller's only above default-floor, is :min-health; a given floor of 1 or less that
  stays so is :allow-over-budget; the given :spend :health and :job-time are :max-damage and :leg-s (a caller's remainder
  is the engine's to enforce, not a shorter leg)."
  [given frozen default-floor]
  (let [g (get-in given [:floor :health])
        f (or (get-in frozen [:floor :health]) g)
        {:keys [health job-time]} (:spend given)]
    (cond-> {}
      (and f (if g (> f 1) (> f default-floor))) (assoc :min-health f)
      (and g (<= f 1)) (assoc :allow-over-budget true)
      (some? health) (assoc :max-damage health)
      (some? job-time) (assoc :leg-s job-time))))

(defn with-limits
  "c with walk-limits in its args, where the walk code (jobs.lib.walk.world/walk-settings) reads them."
  [c]
  (update c :args merge (walk-limits (:args c) (ctx/frozen-call c) (cost-health/default-min-health))))

(defn ^:async quote-of
  "The planner's route to the goal from where the body stands, with this call's policy, costs, dangers, tolls and
  limits, walked nowhere (go-to.preview): time from its seconds, health its planned damage, :moves-to the goal cell,
  :route what the planner found."
  [c]
  (let [c (with-limits (with-body-food c))]
    (preview/quote-of-route (await (preview/route! c)) (ways/escalating? c))))

(defn limits-fit
  "Never :bad-args for a floor or leg out of range, before the engine's floor check could hold the call."
  [c]
  (if-let [{:keys [message]} (limits-refusal (:args c))]
    (contract/never :bad-args message)
    (contract/ok :args "args fit")))

(defn waits
  "Later while wait-reason holds, for a listed go-to not yet started; a started call, or a child, yields instead (attempt)."
  [c]
  (if-let [{:keys [reason] :as w} (when (and (empty? (:slots c)) (nil? (ctx/frozen-call c))) (wait-reason c))]
    (apply contract/later reason (name reason) (mapcat identity (dissoc w :reason)))
    (contract/ok :no-hold "nothing to wait for")))

(def can-start? (cond/and limits-fit waits))

(defn hold!
  "Keep in memory what the call waits on (:hold, nil when it walks), for a parent's jobs.lib.child walk to read."
  [c reason]
  (when-not (= reason (:hold (ctx/mem c)))
    (ctx/update-mem! c assoc :hold reason)))

(defn ^:async run-steps!
  "step! until it arrives, gives up, or waits on a child (:continue), with pace! between steps; goal aimed again each
  step (goal-pos), so a column seen on the way resolves."
  [c goal]
  (loop []
    (let [r (await (step! c (goal-pos c goal)))]
      (cond
        (= :again r) (if (ctx/alive? c) (do (await (pace/pace!)) (recur)) :continue)
        :else (do (swap! approaches dissoc (:id c)) (ctx/update-mem! c dissoc :open) r)))))

(defn ^:async attempt
  "One whole attempt: step! until it arrives, gives up, or waits on a child (:continue), with pace! between steps. A
  cut ends it at once (ctx/alive? per iteration, else at the next memory write or act, which throws). A goal inside a
  fled mob's radius ends it at once under :goal-danger :end, else says so once. With :unknown, the attempt's mask spec
  (wworld/unknown-spec: one missed Set for all its plans) rides on c as :unknown."
  [c0]
  (let [u (:unknown (:args c0))
        c (cond-> c0 u (assoc :unknown (wworld/unknown-spec (when (map? u) u))))
        refusal (args-refusal c)
        night (when-not refusal (night-wait c))
        plug (when-not (or refusal night) (plug-wait c))
        goal (when-not (or refusal night plug) (:pos (aim/target c)))
        pos (goal-pos c goal)]
    (hold! c (or (:reason night) (:reason plug)))
    (cond
      refusal (end/refuse! c refusal)
      (not (ways/way? c :walk)) (end/refuse! c {:reason :no-way :message "the call's :ways leave out :walk"})
      night (do (when-not (:night-said (ctx/mem c))
                  (ctx/update-mem! c assoc :night-said true)
                  (ctx/emit! c :go-to.night-sheltered :info
                             (assoc night :text (str "night: waiting for day in its shelter (" (name (:shelter night)) ") instead of walking out"))))
                (await (child/pause! c (/ (night-wait-ms) 1000) :night-sheltered))
                :continue)
      plug (do (when-not (:plug-said (ctx/mem c))
                 (ctx/update-mem! c assoc :plug-said true)
                 (ctx/emit! c :go-to.sealed-hostile-near :info
                            (assoc (select-keys plug [:mob :band :sensed])
                                   :text (str "sealed in: waiting for a " (name (:sensed plug)) " " (:mob plug) " (" (name (:band plug)) ") to go"))))
               (await (child/pause! c (/ (night-wait-ms) 1000) (:reason plug)))
               :continue)
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
              (await (end/give-up! c pos 0 :cut {:reason :cut-again} (cond-> (or (nearest-hostile c pos) {})
                                                                         spent? (assoc :resets-spent true)))))
          (let [danger (goal-danger c pos)]
            (when danger (ctx/emit! c :goal-in-danger :info (assoc danger :target pos)))
            (if (and danger (= :end (some-> (:goal-danger (:args c)) keyword)))
              (await (end/give-up! c pos 0 :goal-dangerous {:reason :goal-dangerous} danger))
              (do (pre-aim! c goal pos)
                  (await (run-steps! c goal))))))))))

(defn ^:async round
  "attempt, then (unless it waits on a child) the off-hand torch goes back whatever the outcome, a throw too (rethrown),
  and the planner's kept state is released. A cut ends the call without either."
  [c0]
  (let [c (with-limits c0)
        r (try (await (attempt c))
               (catch :default e
                 (when (ctx/alive? c) (await (torch/return-torch! c)))
                 (throw e)))]
    (when-not (= :continue r)
      (swap! plug-since dissoc (:id c))
      (wsearch/release! c)
      (await (torch/return-torch! c)))
    r))
