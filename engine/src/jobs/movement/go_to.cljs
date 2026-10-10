(ns jobs.movement.go-to
  (:require [engine.args :as a]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.aim :as aim]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.combat :as combat]
            [jobs.lib.reach :as reach]
            [jobs.lib.breath :as breath]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter :as sh]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.lib.near :as near]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.escape :as escape]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.threats :as threats]
            [jobs.lib.walk.watch :as wwatch]
            [jobs.lib.look :as look]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.places :as places]
            [jobs.lib.torch :as ltorch]
            [jobs.lib.world :as known]
            [jobs.movement.go-to.air :as air]
            [jobs.movement.go-to.column :as column]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.health :as health]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.torch :as torch]))

(def doc
  "Walk to :pos ([x y z] or {:x :y :z}, fractions floored to the cell) until the body's cell is within :range cells of
  it (a sphere; range 0: in that cell, 1: next to it); with :see a cell in it counts only where the eye sees the block at
  :pos (jobs.lib.aim), and a search that finds no such stand gives up :why :out-of-sight. :pos [x z] or {:x :z} (no y) walks to the seen ground of that
  column nearest the body's feet (unseen: first to within 8 of a height estimated from the seen columns around it, then looks); a y the planner finds no standable cell within :range of, or under :unknown a y seen in the ground with no seen stand within :range (asked before the first walk, pre-aim!), is re-aimed once to its column's
  ground nearest it, at most :y-slack blocks off and never past a solid (info goal-resolved {:goal :to}); see jobs.movement.go-to.column. A
  column whose ground is not made out is looked at after a fruitless walk (at most 3 a call); with no stand in reach of its guess it walks to seen stands toward it (column/waypoint, off the line past a cliff), until ::max-waypoint-misses bring it no nearer. A cell over a short floor (farmland, a path, a bottom slab) stands for that
  block's own cell, where the body's feet are. :place (a name such as :home, set by jobs.memory.set-place) walks to
  that place's recorded position instead of :pos; a name not in memory falls back to the shared marker of that name (jobs.lib.world/marker).
  - Refused at submit (or the caller's call-child throws): an arg its spec refuses, such as a :tolls entry that is not {:x :y :z :factor} of finite numbers, a :drop-cost that is not a number >= 0 or false, a :costs that is not a map of known price names to numbers >= 0, a :landing that is not a map of block names to numbers, a :gait that is not :auto, :walk or :sneak, a :min-health outside 1-20, a :danger-shape that is not {:sensed|:remembered|:creeper {:close :radius}}.
  - Refused at once, before any walk: a :pos outside the world (:bad-pos), a :place that is not a valid name (:bad-name)
    or has no recorded position (:unknown-place), a body with no pathWorld sensing (:unsupported). Both give a :refused warn and {:status :stopped :arrived false :reason <it> :text}.
  - One call is one whole attempt: it plans and walks (jobs.lib.walk: slices of about 100 ms of search, walks of at
    most 60 s each) until it arrives or gives up. A search that needs more slices walks on toward where it has got
    to, or not at all while it goes on; every iteration awaits a pace-ms timer. A goal in unloaded land is walked
    toward walk by walk, and to the edge of loaded land when that is the only way on. It returns :continue only while
    an escalation or put-back child, or healing, is waiting on the world.
  - A drop or plant that costs hp is planned only within a damage budget (jobs.lib.cost/damage-budget: the health over :min-health,
    default 12, less 1, at most :max-damage). When that budget is why no way was found, the body that can heal waits (info
    go-to.waiting-health, :continue; it eats a carried food first when hungry), and one that cannot heal or whose healing
    stalled goes over it, down to 1 hp (info go-to.over-budget) only with :allow-over-budget; otherwise, and with a
    :min-health or :max-damage given, it gives up :why :needs-health. See jobs.movement.go-to.health.
  - :unknown true (default false) plans over what the body has seen or felt only: a cell it never saw is unknown (air to
    the body), a floor it never saw is stepped onto only after the walk stops to look at it (jobs.lib.walk.sight), and
    only where it can see it from the takeoff (a drop deeper than it sees in the dark is refused). A map overrides the
    prices of jobs.lib.cost/unknown-prices. When a dark drop was the better way it gives up :why :unseen-floor-dark; in the
    dark with a torch carried and the off hand empty it first holds the torch in the off hand (info go-to.off-hand-torch,
    once a call: it sees deeper) and plans again; it puts the torch back into the inventory before an escalation child runs and when the call ends (full: it
    stays, info go-to.off-hand-kept); one a cut call left in the hand goes back at the next call's start.
  - :place-torches true (with :unknown; default false) may light such a dark floor instead: the planner prices a torch
    (jobs.lib.cost/light-cost, or :unknown's :light-cost) where it can stand on the takeoff, in no other owner's zone or
    claim; the walk stops there, places one (an off-hand stack goes back to the inventory first; info walk.torch-placed) and
    walks on once it sees the floor. With none carried it fetches torches once a call (:fetch, default true; jobs.lib.fetch);
    with none and no fetch it gives up :unseen-floor-dark saying so. A torch that cannot go there is not tried there again.
  - :dark false plans dark cells like lit ones (default: a dark cell costs twice a lit one, so a lit route up to about 2x
    longer is taken: a cell seen and dark, or one never seen at night; look/dark-fn).
  - :at-night :wait (default): at night a walk that would take the body out of the shelter it stands in
    (jobs.lib.shelter/walk-leaves-shelter?: a closed room, its dug-in shelter, a roof, underground) waits for day: a listed
    go-to declines {:reason :night-sheltered :shelter :goal}, a child call waits 1 s and yields :continue (info
    go-to.night-sheltered, once a call). :go walks anyway; a reflex job's go-to always walks.
  - A walk without :escalate from a body shut in by its own retreat plug (ledger) with a known hostile near waits the same way, {:reason :sealed-hostile-near}, until the hostile goes, :plug-wait-ms (3 min) pass, or food or health fall to :plug-wait-min-food or -health (it never holds a starving or hurt body).
  - :goal-danger :end gives up at once (:why :goal-dangerous, :mob, :mob-pos, :mob-distance) when the goal lies within the full-cost radius of a known mob the body would flee; :wait (default) walks on, an info goal-in-danger says so.
  - :dangers false plans straight past known dangers (default: the plans keep away from them, jobs.lib.threats). :leg-s n
    walks one leg of at most n s and, when it got more than 1 block nearer, ends {:arrived false :leg true} (job :done)
    for a caller chasing a moving target to call again; a leg that got no nearer goes on as any round.
  - An arrival in water walks on to the nearest seen dry stand within :range and ::dry-radius of the body (settings), once
    a call, a short walk of its own; with none, or that walk failing, it ends {:arrived true :in-water true} with an info
    go-to.in-water saying which. :leave-water false keeps an arrival in water. Under water with its search going on (no
    walk) it swims to air first (go-to.surfacing) when under ::surface-air seconds of air are left (always), or when the
    search began afresh more than ::wet-restarts times (at most 3 such swims in a row that leave the head under).
  - A call starts afresh from the world: its walk counters are per call (the cut count carries over only a cut of this run's call to the same goal), and a saved escalation is dropped when the body
    is already there (a cut call's memory is a hint).
  - A walk that gets more than 1 block nearer is progress. Three walks in a row without progress give up, and so
    does a goal the planner proves walled in (:goal-enclosed, :goal-cut-off) or on magma (:goal-on-magma), at once.
  - A search that runs out of loaded land walks to its frontier (the loaded edge), and a walk that gets nearer that edge
    is progress too: a tour of frontiers ends only when every frontier within 1000 blocks of the goal is spent. :explore-blocks n
    bounds it: once the body has walked n blocks since it last came nearer its goal than ever in the call, the call gives
    up :why :explore-spent (:explored the blocks) at the end of that walk.
  - A walk stuck or off its plan at a cell is a go-to.walker-fault warn; the next plans of the call cost that cell
    (up to 16; after a fall also the cells above it) far more (wworld/with-avoid), so a way round is taken when there is one.
  - :escalate (default true): a body that is shut in (jobs.lib.reach/enclosed?), not in its own shelter, and whose
    search ran out of land (:exhausted, :goal-enclosed or :goal-cut-off) makes a way instead of giving up, one child job
    each (jobs.lib.escape/choose), at most 3 in a row that start no nearer the goal than one before (escalation/count-escalation): jobs.access.pillar up out of a pit when it carries enough blocks; else
    jobs.access.clear-path through a wall up to 3 thick toward the goal; else jobs.access.stair up out of a pit or
    toward a higher goal (as far as the goal is above, at most 8; a stair that leaves the body shut in under the goal is
    followed by the next escalation at once); else a walk to the nearest wall first. Each emits go-to.escalated {:step :why :n}. Once
    through, it puts back what clear-path or stair dug in another's zone, claim or plan (with :ignore-zones?;
    its own diggings elsewhere stay) (jobs.blocks.place, the dug block or its drop, when carried): info
    go-to.restored, warn go-to.restore-skipped for cells it could not. Pillar blocks and a stair's placed floors are
    left to jobs.access.cleanup. Its children's walks never escalate. Jobs that must not change the world on the way pass
    :escalate false: a shut-in body then gives up with :shut-in true and says so in the text.
  - Gives up with {:status :stopped :arrived false :reason :unreachable :why ... :text words} (the job ends :stopped) and an :unreachable warn: :why is the planner's
    reason (:exhausted, :goal-unloaded (the goal lies in unloaded land and the loaded land leads no nearer), :goal-enclosed, :goal-cut-off, :goal-on-magma, :door-stuck with :cells, :one-way with :near and :one-way ...), or :stuck
    (:kind the step, :detail the executor's text), :off-plan, :steer-failed, :no-progress, :cut-again (sent back by 3 cuts, each restart more than send-back-margin farther than where the cut found the body, with no 2 blocks gained on the closest approach in between (in-place cuts do not count or break it); a restart that knows of no hostile within the hostile-near radius of the way ahead clears the count instead; an escalation child's own walk is its own call, a parent waiting on it is not walking; :nearest-hostile names a guess at the cause) or :moved-while-searching; :at is the
    body's feet cell and :near its blocks from the goal. A failed
    escalation adds :escalation {:step :reason ...}, the child's reason or wait.
  - Under water with no way to the goal in the air left (:air), it first walks to the nearest air it sees or finds by
    looking (jobs.movement.go-to.air, info go-to.surfacing, at most 3 a call) and plans again.
  - Success is {:arrived true}, with :resolved the cell when go-to chose the y, plus :provisional true (a column's
    estimated y) or :stand-in true (a column with no ground known: the feet y). The result is also a :result info event.
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
   :escalate {:doc "when shut in with no way out, pillar, stair or dig a door to get out (and put back what was dug); false: give up"
              :spec boolean? :default true}
   :dangers {:doc "false: plan straight past known dangers (a walk up to the hostile being fought); true: keep away from them" :spec boolean? :default true}
   :unknown {:doc "true: plan over what the body has seen or felt only, unseen floors checked by eye before they are stepped on; a map of :cell :floor :peek-s :aim-s :see-lit-depth :light-depth overrides their prices and depths (jobs.lib.cost/unknown-prices), :light-cost the seconds of a torch :place-torches places; false: plan over the world as it is" :spec (a/valid-by "true, false or a map of :cell :floor :peek-s :aim-s :see-lit-depth :light-depth :light-cost to numbers >= 0" wworld/unknown-problem) :default false}
   :place-torches {:doc "with :unknown: light a dark floor it cannot see with a torch from the inventory, placed at the takeoff (priced by jobs.lib.cost/light-cost, or :unknown's :light-cost), fetching torches when none is carried (:fetch); false: never place one" :spec boolean? :default false}
   :fetch {:doc "get torches for :place-torches when none is carried (jobs.lib.fetch): true, a set of kinds or a map of limits; false: give up instead" :spec fetch/option? :default true}
   :dark {:doc "false: plan dark cells like lit ones; true: a dark cell (seen dark, or unseen at night) costs twice a lit one" :spec boolean? :default true}
   :tolls {:doc "cells to cross only as a last resort, [{:x :y :z :factor}]: each costs factor times its own seconds more (jobs.lib.cost farm-tolls, zone-tolls)" :spec (a/valid-by "a list of {:x :y :z :factor} of finite numbers, :factor >= 0" wworld/tolls-problem) :default nil}
   :min-health {:doc "hp (1-20) a drop or a plant's prick may not take the body below, less a margin of 1 (the walk's damage budget); 12 when absent; a floor the walk never crosses, not even when it goes over its budget" :spec (a/num-in 1 20) :default nil}
   :food {:doc "the food level (0-20) the walk counts on for its damage budget, its sprinting and whether it can heal by waiting (18 or more regenerates); default the body's own" :spec (a/num-in 0 20) :default nil}
   :fall-margin {:doc "hp the falls of a walk may cost over the plan before an info go-to damage-mismatch" :spec (a/num-in 0 nil) :default wwatch/fall-margin}
   :flee-factor {:doc "danger price: a known mob the body would flee costs this times its hp a second (jobs.lib.cost.danger)" :spec (a/num-in 0 nil) :default (:flee cost/danger-stances)}
   :fight-factor {:doc "danger price: a known mob the body would fight costs this times its hp a second" :spec (a/num-in 0 nil) :default (:fight cost/danger-stances)}
   :danger-max-rate {:doc "danger price: hp a second one known danger costs at most; above the default it also raises the total of all dangers a second (planner dangerCap)" :spec a/pos-num? :default cost/danger-max-rate}
   :danger-shape {:doc (str "danger price: {:sensed|:remembered|:creeper {:close :radius}} blocks, full within :close, none past :radius; only the terms given replace the default (" (str/join ", " (map (fn [[k {:keys [close radius]}]] (str (name k) " " close " " radius)) cost/danger-default-shape)) ")") :spec (a/valid-by "a map of :sensed, :remembered or :creeper to {:close :radius} numbers >= 0, :close under :radius" cost/danger-shape-problem) :default nil}
   :hp-seconds {:doc "seconds an hp costs at full health when the planner weighs a drop or a plant's prick against a longer way (more at low health)" :spec a/pos-num? :default cost/hp-seconds}
   :allow-over-budget {:doc "when the damage budget is why no way is found and the body cannot heal, go over it, down to 1 hp; false: give up :needs-health" :spec boolean? :default false}
   :max-damage {:doc "hp at most a walk may spend on drops and plants that hurt (0: none), under the :min-health budget" :spec (a/num-in 0 nil) :default nil}
   :drop-cost {:doc "number: scales the cost of a drop (fall seconds and damage; 1 as is, 0 free, 5 dear); false: no drop of 2 or 3 at all. :one-way :closed instead refuses only a drop the body cannot climb back" :spec (a/or-of (a/num-in 0 nil) false?) :default 1}
   :costs {:doc (str "map of price name to seconds, overriding the planner's price of a move (jobs.lib.cost.planner: " (str/join " " (map name (keys cost/planner-names))) "); a price left out keeps the planner's default; :lethal-air is no price: 1 lets a plan drown the body when no other way exists, else such a plan is refused (:why :air-lethal)" ) :spec (a/valid-by "a map of planner price name to a number of seconds >= 0" cost/planner-costs-problem) :default nil}
   :landing {:doc "map of block name to the share of a fall's damage a landing on it takes (0.2: 80% off), negative: a bounce, no damage but settle time (only onto a pad in view, bouncing blocks or a wall all round, not under :gait :sneak; else the full fall); entries override the defaults hay_block 0.2, honey_block 0.2 and slime_block -1" :spec (a/valid-by "a map of block name to a number" cost/landing-problem) :default nil}
   :gait {:doc "how the body walks: :auto (sprints on long straight runs and over gaps, as the walker does), :walk (never sprints: no gap jump over 2 or more), :sneak (slow, never sprints; holds sneak on level steps so it never walks off an edge, lets go for a planned drop, gap, climb or water); nil: the body's :walk-settings :gait, else :auto" :spec #{:auto :walk :sneak} :default nil}
   :goal-danger {:doc "when the goal lies within the full-cost radius of a known mob the body would flee: :wait (walk on; an info goal-in-danger says so once), :end (give up at once, :why :goal-dangerous with the mob)" :spec #{:wait :end "wait" "end"} :default :wait}
   :zone-tolls {:doc "true: also toll the cells of other bodies' zones near each walk (jobs.lib.toll-cells/zone-walk-tolls), none with :ignore-zones?; for a job that respects zones" :spec boolean? :default false}
   :leg-s {:doc "walk one leg of at most this many seconds (0.1 to 120), then end {:arrived false :leg true} so the caller can re-aim at a moving target; nil: the whole way" :spec (a/num-in 0.1 120) :default nil}
   :one-way {:doc "arg, not the :one-way key of a give-up result: :closed takes no drop of 2 or 3 or gap jump down that the body cannot climb back, and walks to no frontier of loaded land (a walk to something visible); :open (default) takes one when the land past it runs on into unloaded land" :spec #{:open :closed} :default :open}
   :explore-blocks {:doc "blocks the body may walk in a call without coming nearer its goal than ever before (a tour of frontiers past a dead end), then it gives up :why :explore-spent; nil: no bound (the tour ends when every frontier is spent)" :spec (a/num-in 0 nil) :default nil}
   :retry {:doc "false: a walk that got no nearer gives up at once instead of walking again (up to 3 times), for a caller that re-aims itself" :spec boolean? :default true}
   :look-round {:doc "false: no look round on arrival, for a caller that keeps moving" :spec boolean? :default true}
   :warn {:doc "false: a give-up or refusal is an info event, not a warn, for a caller that reports the failure itself"
          :spec boolean? :default true}
   :air-looks {:doc "under water with no air known: rounds of looking again for air (goal head, open water ahead), the aim shifted a little each round, before giving up :air" :spec (a/int-in 0 nil) :default air/default-air-looks}
   :air-look-min {:doc "seconds of air left below which it looks again for air no more (:air-looks), the fallback (swim up, breathe) needs them" :spec (a/num-in 0 nil) :default air/default-air-look-min}
   :leave-water {:doc "false: an arrival within range in water stays there (a swim target, a boat), no walk on to a dry stand" :spec boolean? :default true}
   :at-night {:doc ":wait: at night a walk out of the shelter the body stands in waits for day; :go: walk anyway (a reflex job's go-to always walks)" :spec #{:wait :go "wait" "go"} :default :wait}
   :ignore-zones? {:doc "act regardless of zones and claims in the escalation (pillar, stair, clear-path); the rules of the game allow it" :spec boolean? :default false}})

(def max-blocked 3)

(a/defargs settings
  {::max-cut-restarts {:default 3 :doc "Cuts in a row, each sending the body back, before a go-to call gives up :cut-again." :spec (a/int-in 1 nil)}
   ::cut-progress {:default 2 :doc "Blocks a go-to's closest approach to its goal must improve to clear its cut count." :spec (a/num-in 0 nil)}
   ::max-cut-resets {:default 2 :doc "Times a go-to clears its cut count at a restart that knows of no hostile near the way, per goal; after that a full set of cuts gives up :cut-again whatever is near." :spec (a/int-in 0 nil)}
   ::send-back-margin {:default 3 :doc "Blocks farther from the goal than where the cut found the body a go-to restart must begin to count as sent back (knockback and in-place reflexes stay under it)." :spec (a/num-in 0 nil)}
   ::surface-air {:default 7.5 :doc "Seconds of air left under which a go-to whose head is under water and whose search goes on (no walk) swims to air first: a body holding still in water sinks while it thinks." :spec (a/num-in 0 nil)}
   ::wet-restarts {:default 2 :doc "Times a go-to's search under water may begin afresh (no walk) after its first slice before it swims to air first and plans from there." :spec (a/int-in 0 nil)}
   ::dry-radius {:default 6 :doc "Blocks from the body an arrival in water reads for a dry stand (also within :range of the goal)." :spec (a/num-in 0 nil)}
   ::dry-walk-rounds {:default 3 :doc "Walk rounds (search slices or walks) the walk on from an arrival in water to a dry stand takes at most." :spec (a/int-in 1 nil)}
   ::night-wait-ms {:default 1000 :doc "Milliseconds a go-to waits before it yields :continue while it holds for day inside a shelter." :spec (a/int-in 0 nil)}
   ::plug-wait-ms {:default 180000 :doc "Milliseconds a go-to shut in by its retreat plug holds for a known hostile (:sealed-hostile-near) before it walks on as without the hold." :spec (a/int-in 0 nil)}
   ::plug-wait-min-food {:default 6 :doc "Food at or under which a go-to shut in by its retreat plug no longer holds for a hostile: starving beats waiting." :spec (a/num-in 0 20)}
   ::plug-wait-min-health {:default 8 :doc "Health at or under which a go-to shut in by its retreat plug no longer holds for a hostile." :spec (a/num-in 0 20)}
   ::max-waypoint-misses {:default 3 :doc "Waypoint walks toward a column goal whose ground is only guessed that a call takes while they bring the body no nearer it." :spec (a/int-in 0 nil)}
   ::waypoint-gain {:default 2 :doc "Blocks (across) a waypoint must lie nearer a guessed column goal than the body, and a waypoint walk must bring the body nearer, not to be a miss." :spec (a/num-in 1 nil)}
   ::way-looks {:default [[6 1] [3 4]] :doc "The looks taken in turn before each waypoint walk: [blocks ahead toward the goal, blocks under the feet] (ahead and a little down: a slope up; close and steeply down: a slope or drop below an edge)." :spec (a/coll-of (a/pred "a pair [ahead down] of numbers >= 0" #(and (sequential? %) (= 2 (count %)) (every? (fn [n] (and (number? n) (>= n 0))) %))))}})

(defn max-cut-restarts [] (settings/get settings ::max-cut-restarts))
(defn max-cut-resets [] (settings/get settings ::max-cut-resets))
(defn cut-progress [] (settings/get settings ::cut-progress))
(defn send-back-margin [] (settings/get settings ::send-back-margin))

(defn surface-air [] (settings/get settings ::surface-air))
(defn wet-restarts [] (settings/get settings ::wet-restarts))
(defn dry-radius [] (settings/get settings ::dry-radius))
(defn dry-walk-rounds [] (settings/get settings ::dry-walk-rounds))
(defn night-wait-ms [] (settings/get settings ::night-wait-ms))
(defn plug-wait-ms [] (settings/get settings ::plug-wait-ms))
(defn plug-wait-min-food [] (settings/get settings ::plug-wait-min-food))
(defn plug-wait-min-health [] (settings/get settings ::plug-wait-min-health))
(defn max-waypoint-misses [] (settings/get settings ::max-waypoint-misses))
(defn waypoint-gain [] (settings/get settings ::waypoint-gain))
(defn way-looks [] (settings/get settings ::way-looks))

(def max-fault-cells "Cells a call keeps its plans away from (the walker's faults: fault-cells), the oldest dropped." 16)

(def hurt-window-ms "How recent a hurt event is that an off-plan walk is taken to have fallen." 10000)

(def fall-column "Cells above the point a damaging off-plan walk left its plan that the next plans avoid too: where the fall began." 6)

(def max-searching
  "Search slices in a row whose search is still going on and began afresh (wsearch/round-budget expansions each) before
  go-to gives up: a search that goes on from the same cell always ends after the planner's maxNodes; only a body moved
  off its search's start every slice (pushed, drifting) starts afresh each time."
  100)

(def pace-ms "The timer each iteration of a call's loop awaits, so a search loop never starves the event loop." 50)

(defn pace!
  "A promise that resolves after pace-ms (a timer, never a microtask)."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve pace-ms))))

(defn ^:async arrived!
  "Look round once if the place is risky (jobs.lib.watch), then finish: the next job starts facing along the walk.
  A goal whose y go-to chose (goal-pos) adds :resolved, the cell it walked to, with :provisional true when that was a
  column's estimated y, :stand-in true when no ground of it was known."
  [c]
  (when-not (false? (:look-round (:args c)))
    (await (watch/watch! c {})))
  (let [{:keys [aimed column-open]} (ctx/mem c)]
    (end/finish! c (cond-> {:arrived true}
                     aimed (assoc :resolved aimed)
                     (= :guess column-open) (assoc :provisional true)
                     (= :stand-in column-open) (assoc :stand-in true)
                     (:in-water (ctx/mem c)) (assoc :in-water true)))))

(defn forget-known-land!
  "Forget the land earlier searches knew to their end (wsearch/known-land) at the go-to's first walking round and after each
  escalation (:escalations, the world changed): a new go-to, or a dug way, may open a way through it."
  [c]
  (let [epoch (:escalations (ctx/mem c) 0)]
    (when-not (= epoch (:known-epoch (ctx/mem c)))
      (wsearch/forget-known! c)
      (ctx/update-mem! c assoc :known-epoch epoch))))

(defn known-frontier-result
  "A round's walk result as go-to judges it: a walk to a frontier in land earlier searches knew to their end
  (:frontier-known: there was no other edge) explores nothing, so it is the search's own answer, :exhausted, and never
  progress, unless it brings an edge seen earlier and now out of view nearer (:frontier-target)."
  [result]
  (if (:frontier-known result)
    (cond-> {:status :no-path :reason :exhausted :frontier-known true}
      (:frontier-target result) (assoc :frontier-target (:frontier-target result))
      (:damage-refused result) (assoc :damage-refused true))
    result))

(defn unloaded-end
  "A fruitless round's result as go-to gives it up when the goal cell reads unloaded: a search that ran out of loaded land
  (:exhausted) or a partial plan walked to its end (:off-plan :partial) says :goal-unloaded, the far side was never seen."
  [result unloaded?]
  (if (and unloaded? (or (= :exhausted (:reason result)) (and (= :off-plan (:status result)) (:partial result))))
    {:status :no-path :reason :goal-unloaded}
    result))

(defn foreign?
  "Whether cell [x y z] lies in, or next to, a zone of an owner other than self-name."
  [zones self-name [x y z]]
  (boolean (some (fn [{:keys [owner min max]}]
                   (and (not= owner self-name)
                        (every? true? (map (fn [v lo hi] (<= (dec lo) v (inc hi))) [x y z] min max))))
                 zones)))

(defn shut-foreign
  "The walker's :shut-also (jobs.lib.pass): a block opened in or next to another owner's zone is shut again whatever
  :doors says, so a pass through somebody's pen lets nothing out."
  [c]
  (fn [cell] (foreign? (known/zones c) (ctx/self-name c) cell)))

(defn see-cell
  "The cell [x y z] the eye must see under :see: the :pos arg's block (not the cell the walk aims at, which may stand for
  it: a crop's farmland), else nil (no :see, a column or a place)."
  [c]
  (when (:see (:args c))
    (let [{:keys [x y z]} (:pos (places/parse-pos (:pos (:args c))))]
      (when (and x y z) [x y z]))))

(defn here?
  "Whether the body is within range of pos, by its position or by the cell it stands on, and under :see sees pos's block."
  [c pos range]
  (and (or (u/within? (u/self-pos c) pos range) (u/within? (reach/standing-cell (:primitives c)) pos range))
       (if-let [cell (see-cell c)] (aim/body-sees? c cell) true)))

(defn sight-end
  "A fruitless round's result as go-to gives it up under :see: a search that ran out of land (:exhausted) found no stand
  in range that sees the block: :out-of-sight."
  [c pos result]
  (if (and (see-cell c) (= :exhausted (some-> (:reason result) keyword)))
    (assoc result :reason :out-of-sight)
    result))

(defn fault-cells
  "The cells a walk that failed at the walker (:stuck at :target, :off-plan at :at) says it cannot pass, [x y z] each.
  An off-plan walk that was hurt (a fall) also takes the cells above :at, where the fall began (a vine or ladder it
  left). A partial plan walked to its end (:partial) is a normal end."
  ([result] (fault-cells result false))
  ([{:keys [status target at partial]} hurt?]
   (case status
     :stuck (some-> target vector)
     :off-plan (when-not partial
                 (when-let [[x y z] at]
                   (if hurt?
                     (mapv (fn [dy] [x (+ y dy) z]) (range (inc fall-column)))
                     [at])))
     nil)))

(def climbables "Blocks a body hangs in (a vine slides it off once nothing else holds it)." #{"vine" "ladder" "scaffolding" "twisting_vines" "twisting_vines_plant" "weeping_vines" "weeping_vines_plant" "cave_vines" "cave_vines_plant"})

(def max-hang-walks "Walks off a floorless climbable a call tries before it takes the stand." 2)

(defn hanging-cell
  "The feet cell [x y z] when the body hangs in a climbable with no floor of its own under it and pos is no climbable
  cell itself (a walk to a vine means to climb), else nil."
  [c pos]
  (let [p (:primitives c)
        feet (reach/standing-cell p)
        name-at (fn [{:keys [x y z]}] (u/feel-name p {:x x :y y :z z}))]
    (when (and (climbables (name-at feet))
               (not (climbables (u/seen-name p {:x (js/Math.floor (:x pos)) :y (js/Math.floor (:y pos)) :z (js/Math.floor (:z pos))})))
               (not (reach/standable-cell? p feet)))
      [(:x feet) (:y feet) (:z feet)])))

(defn refused-hang
  "The cell [x y z] the body hangs in while a floor stand is still to be found (hanging-cell, up to max-hang-walks)."
  [c pos]
  (when (< (:hang-walks (ctx/mem c) 0) max-hang-walks) (hanging-cell c pos)))

(defn note-fault!
  "A walk that failed at the walker is a bug to see (a go-to.walker-fault warn) and a cell to keep the next plans off
  (mem :fault-cells, at most max-fault-cells; a new call starts with none): the walks go round it when there is a way."
  [c {:keys [status] :as result}]
  (when-let [cells (seq (fault-cells result (pos? (ctx/count-in c :hurt hurt-window-ms))))]
    (ctx/emit! c :walker-fault :warn (cond-> {:why status :target (first cells)}
                                       (:move result) (assoc :kind (:move result))
                                       (:why result) (assoc :detail (:why result))))
    (ctx/update-mem! c update :fault-cells #(vec (take-last max-fault-cells (distinct (into (vec %) cells)))))))

(def approaches
  "This run's record of each open go-to's walk, by job id: {:last-dist :closest :explored ...}, the body's distance to the
  goal now, its least, and the blocks walked since it last came nearer (approach-step). In process, not in job memory: a
  walk updates it every tick and a memory write is an event."
  (atom {}))

(defn approach-step
  "Approach record m after a tick's pose {:x :z ...} at d from the goal: :last-dist d, :closest the least, and :explored
  the blocks walked along x and z since the body last came more than 1 block nearer than ever (:anchor, then 0 again)."
  [m pose d]
  (let [{:keys [prev anchor explored] :or {anchor d explored 0}} m
        nearer? (< d (dec anchor))
        step (if prev (js/Math.hypot (- (:x pose) (:x prev)) (- (:z pose) (:z prev))) 0)]
    (assoc m :last-dist d :closest (min d (:closest m d)) :prev (select-keys pose [:x :z])
             :anchor (if nearer? d anchor) :explored (if nearer? 0 (+ explored step)))))

(defn approach-recorder
  "The walk's :on-pose for a call to pos: while the round is alive, keeps approaches' record (approach-step), so a cut walk
  leaves where the cut found it and :explore-blocks can be checked."
  [c pos]
  (fn [pose]
    (when (ctx/alive? c)
      (swap! approaches update (:id c) approach-step pose (u/dist pose pos)))))

(defn explore-spent?
  "Whether the call's :explore-blocks, when given, is spent: the body walked more than that since it last came nearer."
  [c]
  (let [budget (:explore-blocks (:args c))]
    (boolean (and budget (> (:explored (get @approaches (:id c)) 0) budget)))))

(def torch-need "The wait :place-torches fetches for when no torch is carried." {:reason :need :item "torch" :count 4})
(def torch-fetch-key [:item ["torch"]])

(defn torch-fetch-open?
  "Whether :place-torches may still fetch torches this call: :fetch is on, none was fetched yet, none failed lately."
  [c]
  (boolean (and (fetch/opts c 'jobs.movement.go-to) (not (:torch-fetched (ctx/mem c))) (nil? (fetch/failure c torch-fetch-key)))))

(defn light-ok
  "The planner's lightOk: (fn [x y z]) true where a torch may go at a takeoff (x y z its feet): the feet or the head cell
  in no other owner's zone or claim (none under :ignore-zones?); jobs.lib.walk/light! places it in the one allowed."
  [c]
  (let [in (access/zone-input c)
        ok? (fn [cell] (true? (:ok (zones/verdict (assoc in :action :place :cell cell)))))]
    (fn [x y z] (or (:ignore-zones? in) (ok? [x y z]) (ok? [x (inc y) z])))))

(defn round-unknown
  "The mask spec of this round (c's :unknown): with :place-torches and a torch carried or a fetch still open, torches on
  (wworld/with-light), priced by :unknown's :light-cost or jobs.lib.cost/light-cost."
  [c]
  (let [u (:unknown c)
        n (ltorch/torches-held (:primitives c))
        arg (:unknown (:args c))]
    (if (and u (:place-torches (:args c)) (or (pos? n) (torch-fetch-open? c)))
      (wworld/with-light u (or (when (map? arg) (:light-cost arg)) (:cost (cost/light-cost {:carried n}))) (light-ok c))
      u)))

(defn ^:async fetch-torches!
  "A walk that reached a floor to light with no torch carried: fetch torches once a call (jobs.lib.fetch), :continue while
  the fetch child waits, else :again (the next round plans without torches when none came)."
  [c]
  (if (torch-fetch-open? c)
    (do (ctx/update-mem! c assoc :torch-fetched true)
        (or (await (fetch/step! c 'jobs.movement.go-to torch-need)) :again))
    :again))

(defn dark-end
  "A fruitless round's result as go-to gives it up when a dark way was the better way (:dark-best, jobs.lib.near/dark-best):
  :no-path :unseen-floor-dark with that cell (and :torches :none or :fetch-failed when :place-torches had no torch), unless the
  round failed for a reason of its own (stuck, health, air, ...)."
  [c result]
  (if (end/dark-end? result)
    (cond-> {:status :no-path :reason :unseen-floor-dark :dark-best (:dark-best result)}
      (and (:place-torches (:args c)) (zero? (ltorch/torches-held (:primitives c))))
      (assoc :torches (if (fetch/failure c torch-fetch-key) :fetch-failed :none)))
    result))

(defn ^:async look-at-goal-air!
  "Under water, a plan refused for air whose goal head cell the body has not seen (the planner counts only seen air as a
  way to air): one look at that cell a call (mem :air-looked); true when it looked."
  [c pos]
  (let [p (:primitives c)
        cell (mapv #(js/Math.floor %) [(:x pos) (inc (:y pos)) (:z pos)])]
    (when (and (not (:air-looked (ctx/mem c))) (breath/head-under? p) (sense/unknown? p cell))
      (ctx/update-mem! c assoc :air-looked true)
      (await (sense/look-at! c cell))
      true)))

(defn aimed
  "{:pos cell :resolved bool} to walk to for goal (target's :pos; no :y for a column), jobs.movement.go-to.column/aim."
  [c goal]
  (let [{:keys [y-slack unknown]} (:args c)]
    (column/aim (:primitives c) goal (or y-slack 4) (js/Math.floor (:y (u/self-pos c))) (not unknown))))

(defn not-standable?
  "Whether the planner answers goal-not-standable for pos alone (no search, wplan/goal-stand-problem)."
  [c pos]
  (let [pw (wworld/path-world (:primitives c) (:unknown c))]
    (boolean (and pw (= :goal-not-standable (wplan/goal-stand-problem c pw [(:x pos) (:y pos) (:z pos)] (:range (:args c) 1)))))))

(defn seen-stand-within?
  "Whether a cell within r0 (at most column/approach-range) of the cell [x y z] stands by the cells the body has seen
  (kind-at: column/seen-kind-at)."
  [kind-at [x y z] r0]
  (let [reach (min r0 column/approach-range)
        r (js/Math.floor reach)]
    (boolean (some (fn [[dx dy dz]] (and (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* reach reach))
                                         (column/stands? kind-at (+ x dx) (+ y dy) (+ z dz))))
                   (for [dx (range (- r) (inc r)) dy (range (- r) (inc r)) dz (range (- r) (inc r))] [dx dy dz])))))

(defn in-seen-ground?
  "Under :unknown, whether the y goal pos is a cell the body has seen that is not free (a position in the ground) with
  no seen stand within the :range arg of it."
  [c pos]
  (boolean (and (:unknown c) (:y pos)
                (let [ka (column/seen-kind-at (:primitives c))
                      cell (mapv #(js/Math.floor (% pos)) [:x :y :z])
                      k (apply ka cell)]
                  (and (not (or (keyword-identical? :unknown k) (column/free? k)))
                       (not (seen-stand-within? ka cell (:range (:args c) 1))))))))

(def max-column-looks "Looks a call takes at a column goal whose ground it has not made out, one after a fruitless walk." 3)

(defn ^:async look-at-column!
  "A column goal with no ground made out yet (mem :column-open), or under :unknown a y goal whose cell it has not seen,
  after a fruitless walk or within reach of its guess: look at its cell at the body's feet y, then one above (a look
  glances the cell and its neighbours: the ground nearest the feet), then its aimed cell (a y goal: its cell first), at most max-column-looks a
  call; true when it looked (goal-pos aims again from what it saw, the planner proves a seen solid goal cell not standable)."
  [c pos]
  (let [n (:column-looks (ctx/mem c) 0)
        feet (js/Math.floor (:y (u/self-pos c)))
        cell (mapv #(js/Math.floor (% pos)) [:x :y :z])]
    (when (and (or (:column-open (ctx/mem c)) (and (:unknown c) (sense/unknown? (:primitives c) cell)))
               (< n max-column-looks))
      (ctx/update-mem! c assoc :column-looks (inc n))
      (await (sense/look-at! c (assoc cell 1 (nth (vec (distinct (if (:column-open (ctx/mem c)) [feet (inc feet)] [(nth cell 1) feet (inc feet)]))) n (nth cell 1)))))
      true)))

(defn waypoint-due?
  "Whether a column goal is only guessed (mem :column-open :guess) and fewer than ::max-waypoint-misses waypoint walks
  were fruitless."
  [c]
  (and (= :guess (:column-open (ctx/mem c))) (< (:waypoint-misses (ctx/mem c) 0) (max-waypoint-misses))))

(defn guess-waypoint
  "For a column goal whose guess has no stand in reach (waypoint-due?): a seen stand to walk to toward it
  (column/waypoint), else nil."
  [c pos]
  (when (waypoint-due? c)
    (let [{:keys [x y z]} (u/self-pos c)]
      (column/waypoint (column/seen-kind-at (:primitives c)) (:x pos) (:z pos) (mapv #(js/Math.floor %) [x y z]) (waypoint-gain)))))

(defn ^:async look-along!
  "Before a waypoint walk (waypoint-due?): the next of ::way-looks not yet taken since the last waypoint walk (mem
  :way-looks), at the cell that far along the line to pos and that far under the feet; true when it looked."
  [c pos]
  (let [n (:way-looks (ctx/mem c) 0)
        {:keys [x y z]} (u/self-pos c)
        h (js/Math.hypot (- (:x pos) x) (- (:z pos) z))
        looks (vec (way-looks))]
    (when (and (waypoint-due? c) (< n (count looks)) (> h 2))
      (let [[ahead down] (nth looks n)
            k (/ (min ahead (- h 1)) h)]
        (ctx/update-mem! c assoc :way-looks (inc n))
        (await (sense/look-at! c (mapv #(js/Math.floor %) [(+ x (* k (- (:x pos) x))) (- y down) (+ z (* k (- (:z pos) z)))])))
        true))))

(defn ^:async walk-waypoint!
  "Walk to way (guess-waypoint) on the way to pos: from there the body sees more of the land toward pos and goal-pos
  aims again. The walk keeps the call's approach (approach-recorder, mem :closest and :last-dist), so a cut during it
  sees the progress. A walk that brings it less than ::waypoint-gain nearer pos (across) is a miss (mem
  :waypoint-misses); a walk refused or failed for a reason (needs-health, no torch, a dark drop, ...) is kept as mem
  :waypoint-why (end/give-up-fields), which a give-up of the call names. :again."
  [c pos way opts]
  (let [across #(js/Math.hypot (- (:x pos) (:x %)) (- (:z pos) (:z %)))
        before (across (u/self-pos c))
        {:keys [result status]} (await (near/walk-round! (assoc c :on-pose (approach-recorder c pos)) way 1 (dissoc opts :progress)))
        left (u/dist (u/self-pos c) pos)
        why (when-not (#{"arrived" "partial" "searching"} status) (:why (end/give-up-fields result)))]
    (ctx/update-mem! c #(cond-> (-> % (dissoc :way-looks) (assoc :last-dist left :closest (min left (:closest % left))))
                          (and why (not= :no-progress why)) (assoc :waypoint-why why)))
    (when (> (across (u/self-pos c)) (- before (waypoint-gain)))
      (ctx/update-mem! c update :waypoint-misses (fnil inc 0)))
    :again))

(defn walk-opts
  "The near/walk-round! options of a go-to walk round with doors (the :doors arg as a keyword)."
  [c doors]
  (let [closed? (= :closed (some-> (:one-way (:args c)) keyword))]
    {:doors doors :explore (not closed?)
     :dangers (not (false? (:dangers (:args c))))
     :dark (not (false? (:dark (:args c))))
     :timeout-s (or (:leg-s (:args c)) near/walk-timeout-s)
     :one-way (when-not closed? :open)
     :shut-also (shut-foreign c)
     :budget wsearch/round-budget
     :avoid (set (:fault-cells (ctx/mem c)))
     :tolls (:tolls (:args c))
     :zone-tolls (:zone-tolls (:args c))
     :drop-cost (:drop-cost (:args c))
     :costs (:costs (:args c))
     :unknown (round-unknown c)}))

(defn ^:async walk-dry!
  "The walk on from an arrival in water to the dry stand dry: its own walk rounds, range 0, at most dry-walk-rounds
  while its search goes on or it gets part of the way; the last round's status."
  [c dry]
  (let [opts (walk-opts c (some-> (:doors (:args c)) keyword))]
    (loop [i 1]
      (let [{:keys [status]} (await (near/walk-round! c dry 0 opts))]
        (if (and (#{"searching" "partial"} status) (< i (dry-walk-rounds)))
          (do (await (pace!)) (recur (inc i)))
          status)))))

(defn ^:async leave-hang!
  "The body hangs in a climbable at feet cell hang within range of pos: walk on to the nearest floor stand within range
  (jobs.lib.step-off, range 0), :again; with none a hanging body is no arrival, the call ends :unreachable."
  [c pos hang]
  (let [p (:primitives c)
        [x y z] hang
        range (:range (:args c) 1)
        stand (first (step-off/candidates p {:x x :y y :z z} {:reach 3 :ok? #(<= (u/dist % pos) range)}))]
    (ctx/emit! c :go-to.hanging :info {:at hang :stand (some-> stand ((juxt :x :y :z))) :text (str "within range but hanging in a climbable at " hang)})
    (if stand
      (do (await (walk-dry! c stand)) :again)
      (end/finish! c {:status :stopped :arrived false :reason :unreachable :why :hanging
                      :text (str "the only stand within range is a climbable at " hang " with no floor under it")}))))

(defn ^:async leave-water!
  "An arrival within range of pos, in water: with :leave-water (default), once a call (mem :dry-tried), walk on to the
  nearest seen dry stand within range (air/dry-stand, dry-radius of the body; walk-dry!). Out of water and in range:
  arrived; out of range: :again (the call walks back); else it ends in water, an info go-to.in-water saying why."
  [c pos]
  (let [p (:primitives c)
        r (:range (:args c) 1)
        keep? (false? (:leave-water (:args c)))
        tried? (:dry-tried (ctx/mem c))
        dry (when-not (or keep? tried?) (air/dry-stand p pos r (u/self-pos c) (dry-radius)))
        status (when dry
                 (ctx/update-mem! c assoc :dry-tried true)
                 (await (walk-dry! c dry)))]
    (cond
      (not (breath/in-water? p)) (if (here? c pos r) (arrived! c) :again)
      (not (here? c pos r)) :again
      :else
      (do (ctx/update-mem! c assoc :in-water true)
          (when-not keep?
            (ctx/emit! c :go-to.in-water :info
                       {:at (end/feet-cell c)
                        :text (str "arrived within range but in water: "
                                   (cond dry (str "the walk on to the dry stand at " [(:x dry) (:y dry) (:z dry)] " ended " status)
                                         tried? "the walk on to a dry stand failed earlier in the call"
                                         :else "no dry stand seen within range"))}))
          (arrived! c)))))

(defn ^:async arrive!
  "Within range of pos: done, but for a column whose ground is only guessed (mem :column-open :guess) walked to within
  column/approach-range of it: it looks at it (look-at-column!), its looks spent it walks on within the :range arg (mem
  :column-near). In water, leave-water!."
  [c pos]
  (let [m (ctx/mem c)
        p (:primitives c)
        wet? (and (or (not= :guess (:column-open m)) (:column-near m)) (breath/in-water? p))
        hang (refused-hang c pos)]
    (cond
      (:restore-pending m) (esc/restore-next! c)
      hang (do (ctx/update-mem! c update :hang-walks (fnil inc 0))
               (await (leave-hang! c pos hang)))
      wet? (await (leave-water! c pos))
      (or (not= :guess (:column-open m)) (:column-near m)) (arrived! c)
      (await (look-at-column! c pos)) :again
      :else (do (ctx/update-mem! c assoc :column-near true) :again))))

(def seal-clear-radius 3)

(defn ^:async clear-goal-seal!
  "A goal the walk could not reach (:goal-enclosed, :goal-cut-off, or a search that ran out of land: :exhausted, :ladder-gap)
  with blocks of the body's own ledger within seal-clear-radius of it (a dig-in's seal beside a gate, a plug in a stair
  mouth): one jobs.access.cleanup child takes them back. True when it ran, so the walk is planned again."
  [c pos result]
  (let [near {:pos {:x (js/Math.floor (:x pos)) :y (js/Math.floor (:y pos)) :z (js/Math.floor (:z pos))} :radius seal-clear-radius}]
    (when (and (#{:goal-enclosed :goal-cut-off :exhausted :ladder-gap} (:reason result)) (not (:seal-cleared (ctx/mem c)))
               (seq (ledger/near-of (ledger/open-entries (ctx/view c)) near)))
      (ctx/update-mem! c assoc :seal-cleared true)
      (await (ctx/call-child c :seal-cleanup 'jobs.access.cleanup {:job :all :near near}))
      true)))

(defn ^:async walk! [c0 pos range doors]
  (let [c (cond-> c0 (:over-budget (ctx/mem c0)) (assoc :over-budget true))
        from (u/self-pos c)
        d (u/dist from pos)
        pw (wworld/path-world (:primitives c))]
    (cond
      (and (here? c pos range) (not (refused-hang c pos)))
      (await (arrive! c pos))

      (nil? pw)
      (end/refuse! c {:reason :unsupported :message "the body cannot sense the world for path planning"})

      :else
      (let [_ (forget-known-land! c)
            opts (assoc (walk-opts c doors) :progress (empty? (:frontier-best (ctx/mem c))))
            stair-check (esc/stair-check c pos)
            {walked :result status :status to :to} (await (near/walk-round! (assoc c :on-pose (approach-recorder c pos)) pos range (assoc opts :see-cell (see-cell c) :stair-check stair-check)))
            _ (ctx/hold-still! c (when (= "searching" status) :searching))
            fetched (when (= :no-torch (:status walked)) (await (fetch-torches! c)))
            aided? (when (and (not fetched) (not= "arrived" status) (torch/torch-aid? c walked)) (await (torch/hold-torch! c)))
            looked? (when (and (not aided?) (= :air (:reason walked))) (await (look-at-goal-air! c pos)))
            surfaced? (when (and (not aided?) (not looked?) (= :air (:reason walked))) (await (air/surface! c (dissoc opts :progress) pos)))
            way-looked? (when (and (not fetched) (= :goal-not-standable (:reason walked))) (await (look-along! c pos)))
            way (when (and (not fetched) (not way-looked?) (= :goal-not-standable (:reason walked))) (guess-waypoint c pos))
            col-looked? (when (and (not aided?) (not looked?) (not surfaced?) (not fetched) (not way) (not way-looked?) (not (#{"arrived" "searching"} status)))
                          (await (look-at-column! c pos)))
            result (known-frontier-result walked)
            left (u/dist to pos)
            best (:best (ctx/mem c) d)
            frontier (:frontier result)
            fcell (when frontier (zipmap [:x :y :z] frontier))
            fbests (:frontier-best (ctx/mem c) {})
            fbest (when frontier (get fbests frontier (u/dist from fcell)))
            explored? (boolean (and frontier (< (u/dist to fcell) (dec fbest))))
            target (:frontier-target result)
            tbests (:target-best (ctx/mem c) {})
            tdist (when target (u/dist to {:x (first target) :y (:y to) :z (second target)}))
            nearer? (boolean (and target (< tdist (dec (get tbests target js/Infinity)))))]
        (cond
          (= "arrived" status)
          (await (arrive! c pos))

          (= "stair" status)
          (do (ctx/update-mem! c assoc :stair-tried true :escalate-now true) :again)

          (and (:leg-s (:args c)) (< left (dec d)))
          (end/finish! c {:arrived false :leg true})

          (explore-spent? c)
          (end/give-up! c pos (:blocked (ctx/mem c) 0) status {:status :no-path :reason :explore-spent}
                        {:explored (js/Math.round (:explored (get @approaches (:id c))))})

          fetched fetched

          way-looked? :again

          way (await (walk-waypoint! c pos way opts))

          ;; the planner proved no cell in range stands: aim at the column's ground once (goal-pos, mem :re-aim); under
          ;; :unknown it proves nothing next to unseen cells, so a goal cell seen in the ground is re-aimed too
          (and (not (:re-aim (ctx/mem c)))
               (or (= :goal-not-standable (:reason walked)) (in-seen-ground? c pos))
               (:resolved (aimed c pos)))
          (do (ctx/update-mem! c assoc :re-aim true) :again)

          aided? :again

          looked? :again

          surfaced? :again

          col-looked? :again

          (and (= "searching" status)
               (await (air/surface-while-searching! c (dissoc opts :progress) pos (:fresh walked)
                                                    {:surface-air (surface-air) :restarts (wet-restarts)})))
          :again

          (= "searching" status)
          (let [n (cond-> (:searching (ctx/mem c) 0) (:fresh walked) inc)]
            (ctx/update-mem! c assoc :searching n)
            (if (< n max-searching)
              :again
              (end/give-up! c pos (:blocked (ctx/mem c) 0) :searching {:status :no-path :reason :moved-while-searching})))

          :else
          (let [handled (when (health/refused? c result) (await (health/heal-or-drop! c pos range)))]
           (or (#{:continue :again} handled)
          (let [result (cond-> result (= :needs-health handled) (assoc :reason :needs-health))
                _ (note-fault! c walked)
                progress? (or (< left (dec best)) explored? nearer?)
                tries (if progress? 0 (inc (:blocked (ctx/mem c) 0)))]
            (ctx/update-mem! c assoc :blocked tries :searching 0 :best (if (< left (dec best)) left best)
                             :closest (min left (:closest (ctx/mem c) left)) :last-dist left
                             :frontier-best (cond-> fbests frontier (assoc frontier (min fbest (u/dist to fcell))))
                             :target-best (cond-> tbests target (assoc target (min tdist (get tbests target js/Infinity)))))
            (when (and progress? (:restore-pending (ctx/mem c))) (esc/restore-next! c))
            (if (and (if (false? (:retry (:args c))) progress? (< tries max-blocked)) (not (#{:goal-enclosed :goal-cut-off :goal-on-magma} (:reason result))))
              :again
              (let [unloaded? (wsearch/goal-unloaded? (.-snapshot pw) (mapv #(js/Math.floor (% pos)) [:x :y :z]))
                    result (dark-end c result)
                    ended (cond-> (if (esc/escalate? c result) result (sight-end c pos (unloaded-end result unloaded?)))
                            (:waypoint-why (ctx/mem c)) (assoc :waypoint-why (:waypoint-why (ctx/mem c))))]
                (if (await (clear-goal-seal! c pos ended))
                  :again
                  (await (esc/give-up-or-escalate! c pos tries status ended)))))))))))))

(defn start-attempt!
  "Begin a call from the world, with memory as a hint: the counters of an earlier call (cut, or ended :continue) start
  again (but for the cut count, kept by note-restart!), and a saved escalation, or a pending one, is dropped when the body is already there (its holes, if any, are
  put back as after a made way)."
  [c pos]
  (let [m (ctx/mem c)
        there? (here? c pos (:range (:args c)))]
    (wworld/warn-bad-settings! c)
    (ctx/update-mem! c #(cond-> (-> % (dissoc :best :fault-cells :over-budget :off-hand-tried :air-looked :air-relooks :surfacings :column-looks :column-near :hang-walks :waypoint-misses :waypoint-why :way-looks :dry-tried :in-water :wet-fresh :wet-surfacings) (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {}))
                          (not (:fetching m)) (dissoc :torch-fetched :fetched)
                          (and there? (:escalate-now m)) (dissoc :escalate-now)
                          (and there? (:escalation m))
                          (-> (dissoc :escalation :escalation-from :planned :holes-before)
                              (assoc :restore-pending true))))))

(def session-id
  "Names this run of the engine: a call left open by an earlier run was not cut, so it counts for nothing."
  (str (js/Date.now) "-" (rand-int 1000000000)))

(defn cut-state
  "The cut bookkeeping after a call restarts at dist from goal, from m, the call's memory: {:goal :closest :ref :cuts
  :last-dist}. A restart is a cut only when the earlier call was left open in this session, for the same goal. :cuts
  starts again at 0 when :closest (the least distance a walk reached) has improved cut-progress over :ref (its value then);
  a cut adds one when it begins more than send-back-margin farther than :last-dist, where the cut found the body (a reflex
  sent it back; a meal, a breath, a fight in place or knockback do not, on a detour neither)."
  [m goal dist]
  (let [cut? (and (= session-id (:open m)) (= goal (:goal m)))
        closest (min dist (if cut? (:closest m dist) dist))
        ref (if cut? (:ref m closest) closest)
        improved? (>= (- ref closest) (cut-progress))
        sent-back? (and cut? (> dist (+ (:last-dist m dist) (send-back-margin))))]
    {:goal goal :closest closest :ref (if improved? closest ref) :last-dist dist
     :cuts (+ (if (and cut? (not improved?)) (:cuts m 0) 0) (if sent-back? 1 0))
     :resets (if cut? (:resets m 0) 0)}))

(defn cut-loop? [{:keys [cuts]}] (>= cuts (max-cut-restarts)))

(defn note-restart!
  "Record this call's start in memory: the call is open (this session) until it returns, an open one found at the start
  was cut. The cut state."
  [c pos]
  (let [walked (get @approaches (:id c))
        state (cut-state (merge (ctx/mem c) walked) pos (u/dist (u/self-pos c) pos))]
    (swap! approaches dissoc (:id c))
    (ctx/update-mem! c #(-> % (assoc :open session-id) (merge state)))
    state))

(defn way-hostiles
  "The hostiles the body knows of (seen, heard or remembered) within the hostile-near trigger's radius (ranged ones its
  ranged radius) of the way still ahead to pos, nearest the way first: what may send the body back again."
  [c pos]
  (danger-q/hostiles-near-way (:primitives c) pos (danger-q/hostile-radius) {:ranged-radius (danger-q/ranged-radius)}))

(defn nearest-hostile
  "The name of the hostile nearest the way ahead to pos, else of the nearest the body knows within 16 blocks, a guess
  at the sender back (the engine does not say which reflex cut), else nil."
  [c pos]
  (some-> (or (first (way-hostiles c pos)) (first (combat/hostiles (:primitives c) 16))) .-name))

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
      (:restore-now m) (await (esc/restore-round! c))
      (:escalation m) (await (esc/escalation-round! c pos))
      (:escalate-now m) (do (ctx/update-mem! c dissoc :escalate-now)
                            (await (esc/escalate! c pos nil)))
      :else (or (when (:fetching m) (await (fetch/step! c 'jobs.movement.go-to torch-need)))
                (await (walk! c pos
                              (cond (and (= :guess (:column-open m)) (not (:column-near m))) (max (:range (:args c)) column/approach-range)
                                    :else (:range (:args c)))
                              (some-> (:doors (:args c)) keyword)))))))

(defn shared-marker
  "The shared marker called s: the exact name first, else the first whose name matches in any case; nil when none."
  [c s]
  (or (known/marker c s)
      (let [low (str/lower-case s)]
        (first (filter #(= low (some-> (:name %) str/lower-case)) (known/markers c))))))

(defn target
  "{:pos {:x :y :z}} to walk to: the recorded position of :place when given (the body's own place of that name, else the
  shared marker of that name, any 1 to 100 characters, exact then in any case), else :pos; else a refusal."
  [c]
  (let [{:keys [place pos]} (:args c)]
    (if (nil? place)
      (or (column/parse pos) (places/parse-pos pos))
      (let [named (places/parse-name place)
            there (when-not (:reason named) (mem/place (ctx/view c) (:name named)))
            s (cond (keyword? place) (when (nil? (namespace place)) (name place)) (string? place) place)
            bad (when-not (and s (<= 1 (count s) 100))
                  (places/refusal :bad-name (str "a place name is 1 to 100 characters; got " (pr-str place))))
            shared (when-not (or there bad) (shared-marker c s))]
        (cond
          there (places/parse-pos there)
          bad bad
          shared (places/parse-pos [(:x shared) (:y shared) (:z shared)])
          :else (places/refusal :unknown-place (str "no place called " s " is recorded in memory or among the shared markers")))))))

(defn args-refusal
  "The refusal {:reason :message} of the target go-to and its dry run share, nil when it is usable (the arg shapes are
  the spec's: refused at submit). The target's parse is {:pos ...} when there is no refusal."
  [c]
  (let [parsed (target c)]
    (when (:reason parsed) parsed)))

(defn goal-pos
  "The cell go-to plans to this step for goal: aimed when a column, or once the planner found no cell in range of its y
  stands (mem :re-aim), then as the planner stands (wworld/stand-goal). A y chosen for the
  goal that differs from the last one chosen (or firms up an estimate) is an info goal-resolved {:goal :to} (:provisional true for a column's
  estimate; mem :said); the cell aimed at, a
  column's stand-in too, is mem :aimed (the result's :resolved). A column not resolved from seen cells is :column-open
  (:guess for an estimate, :stand-in for no ground known)."
  [c goal]
  (when goal
    (let [column? (nil? (:y goal))
          {:keys [pos resolved provisional]} (if (or column? (:re-aim (ctx/mem c))) (aimed c goal) {:pos goal})
          open (when column? (cond provisional :guess (not resolved) :stand-in))
          aimed-at (when (or resolved column?) pos)]
      (when (not= open (:column-open (ctx/mem c)))
        (ctx/update-mem! c assoc :column-open open))
      (when (not= aimed-at (:aimed (ctx/mem c)))
        (ctx/update-mem! c assoc :aimed aimed-at))
      (when (and resolved (not= [pos provisional] (:said (ctx/mem c))))
        (ctx/update-mem! c assoc :said [pos provisional])
        (ctx/emit! c :goal-resolved :info (cond-> {:goal goal :to pos} provisional (assoc :provisional true))))
      (wworld/stand-goal (wworld/path-world (:primitives c) (:unknown c)) pos))))

(defn goal-danger
  "The nearest known mob the body would flee (sensed or remembered, jobs.lib.threats/known-dangers) that has the cell pos
  within its full-cost radius, as {:mob :mob-pos :mob-distance}, else nil."
  [c pos]
  (let [p (:primitives c)
        spots (keep #(when-let [at (threats/rough-pos (:data %))] (assoc (:data %) :pos at)) (ctx/entries c :threat))
        body (threats/body-of p)]
    (->> (threats/known-dangers p spots (cost/danger-opts (:args c)))
         (keep (fn [{:keys [mob close] :as d}]
                 (let [dist (u/dist pos d)]
                   (when (and (<= dist close) (= :flee (cost/stance body mob)))
                     {:mob mob :mob-pos (select-keys d [:x :y :z]) :mob-distance (js/Math.round dist)}))))
         (sort-by :mob-distance)
         first)))

(defn pre-aim!
  "Before the first walk: a y goal pos (goal-pos) that the planner answers goal-not-standable for alone (no search,
  wplan/goal-stand-problem), or under :unknown seen in the ground, and whose column aims it elsewhere is re-aimed now
  (mem :re-aim), not after a walk."
  [c goal pos]
  (when (and (:y goal) (not (:re-aim (ctx/mem c))) (pos? (:y-slack (:args c) 4))
             (or (not-standable? c pos) (in-seen-ground? c pos))
             (:resolved (aimed c goal)))
    (ctx/update-mem! c assoc :re-aim true)))

(defn manual-slot?
  "Whether this call runs for the driver of a manual lease (its slot job): the agent's own order."
  [c]
  (let [slot (some-> (:manual-job (:engine c)) deref)]
    (and (some? slot) (= slot (:root c)))))

(defn night-wait
  "The wait {:reason :night-sheltered :shelter :goal} while the walk would take the body out of its shelter at night
  (sh/walk-leaves-shelter?), else nil. Never under a reflex job, the manual driver's own go-to or with :at-night :go."
  [c]
  (let [p (:primitives c)]
    (when (and (not (:reflex c)) (not (manual-slot? c)) (not= :go (some-> (:at-night (:args c)) keyword)) (sh/night? p))
      (let [{goal :pos :as t} (target c)
            kind (when (and goal (not (:reason t)))
                   (sh/walk-leaves-shelter? p (:data (ctx/latest c :shelter)) goal (:range (:args c) 1)))]
        (when kind {:reason :night-sheltered :shelter kind :goal goal})))))

(def plug-since
  "Job id -> when its plug-wait began (in process, like approaches); kept while the hold is due, so a deadline stays passed."
  (atom {}))

(defn plug-held?
  "Whether the body is shut in with a retreat plug of its ledger standing, a known hostile is near and the body is
  neither starving nor hurt (waiting costs food and health, and underground a hostile may stay for ever)."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    (and (some #(= :retreat-plug (:purpose %)) (ledger/open-entries (ctx/view c)))
         (> (or (.-food self) 20) (plug-wait-min-food))
         (> (or (.-health self) 20) (plug-wait-min-health))
         (escape/enclosed? p (some-> (:doors (:args c)) keyword))
         (seq (danger-q/known-hostiles p threats/max-follow-range {:ranged-radius (danger-q/ranged-radius)})))))

(defn plug-wait
  "The wait {:reason :sealed-hostile-near} while plug-held? and this walk may not dig out (no :escalate), until
  :plug-wait-ms have passed since it began. Never under a reflex job."
  [c]
  (when (and (not (:reflex c)) (not (:escalate (:args c))))
    (if (plug-held? c)
      (let [since (get (swap! plug-since update (:id c) #(or % (ctx/now c))) (:id c))]
        (when (< (- (ctx/now c) since) (plug-wait-ms))
          {:reason :sealed-hostile-near}))
      (do (swap! plug-since dissoc (:id c)) nil))))

(defn wait-reason [c] (or (night-wait c) (plug-wait c)))

(defn check
  "A listed go-to waits while wait-reason holds (a child's round yields instead: its parent goes on, and asks with
  :report-wait, jobs.lib.child/call-walk, to name the wait)."
  [c]
  (if-let [w (and (or (empty? (:slots c)) (:report-wait c)) (wait-reason c))]
    (ctx/wait c w)
    true))

(defn ^:async run-steps!
  "step! until it arrives, gives up, or waits on a child (:continue), with pace! between steps; goal aimed again each
  step (goal-pos), so a column seen on the way resolves."
  [c goal]
  (loop []
    (let [r (await (step! c (goal-pos c goal)))]
      (cond
        (= :again r) (if (ctx/alive? c) (do (await (pace!)) (recur)) :continue)
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
        goal (when-not (or refusal night plug) (:pos (target c)))
        pos (goal-pos c goal)]
    (cond
      refusal (end/refuse! c refusal)
      night (do (when-not (:night-said (ctx/mem c))
                  (ctx/update-mem! c assoc :night-said true)
                  (ctx/emit! c :go-to.night-sheltered :info
                             (assoc night :text (str "night: waiting for day in its shelter (" (name (:shelter night)) ") instead of walking out"))))
                (await (ctx/act c :wait #js {:ms (night-wait-ms)}))
                :continue)
      plug (do (await (ctx/act c :wait #js {:ms (night-wait-ms)}))
               :continue)
      :else
      (let [state (note-restart! c pos)
            spent? (and (cut-loop? state) (>= (:resets state) (max-cut-resets)))
            cycle? (and (cut-loop? state) (or spent? (seq (way-hostiles c pos))))]
        (await (torch/return-torch! c))
        (start-attempt! c pos)
        (when (and (cut-loop? state) (not cycle?))
          (ctx/update-mem! c assoc :cuts 0 :ref (:closest state) :resets (inc (:resets state))))
        (if cycle?
          (do (ctx/update-mem! c #(-> % (dissoc :open :goal :closest :ref :last-dist) (assoc :cuts 0)))
              (await (end/give-up! c pos 0 :cut {:reason :cut-again} (cond-> {}
                                                                         spent? (assoc :resets-spent true)
                                                                         (nearest-hostile c pos) (assoc :nearest-hostile (nearest-hostile c pos))))))
          (let [danger (goal-danger c pos)]
            (when danger (ctx/emit! c :goal-in-danger :info (assoc danger :target pos)))
            (if (and danger (= :end (some-> (:goal-danger (:args c)) keyword)))
              (await (end/give-up! c pos 0 :goal-dangerous {:reason :goal-dangerous} danger))
              (do (pre-aim! c goal pos)
                  (await (run-steps! c goal))))))))))

(defn ^:async round
  "attempt, then (unless it waits on a child) the off-hand torch goes back whatever the outcome, a throw too (rethrown),
  and the planner's kept state is released. A cut ends the call without either."
  [c]
  (let [r (try (await (attempt c))
               (catch :default e
                 (when (ctx/alive? c) (await (torch/return-torch! c)))
                 (throw e)))]
    (when-not (= :continue r)
      (swap! plug-since dissoc (:id c))
      (wsearch/release! c)
      (await (torch/return-torch! c)))
    r))
