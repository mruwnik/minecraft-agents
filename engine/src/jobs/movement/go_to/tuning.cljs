(ns jobs.movement.go-to.tuning
  "go-to's settings (but for the call's time, jobs.movement.go-to/settings) and the limits of one call."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {:jobs.movement.go-to.tuning/max-blocked {:default 3 :doc "Fruitless walks of a go-to call in a row before it gives up (or escalates)." :spec (a/int-in 1 nil)}
   :jobs.movement.go-to.tuning/max-searching {:default 100 :doc "Search slices in a row whose search is still going on and began afresh (a body pushed or drifting off its search's start every slice) before a go-to gives up; a search that goes on from the same cell always ends after the planner's maxNodes." :spec (a/int-in 1 nil)}
   :jobs.movement.go-to.tuning/max-escalations {:default 3 :doc "Ways out in a row, none starting nearer the goal than one before, that a go-to tries before it gives up." :spec (a/int-in 1 nil)}
   :jobs.movement.go-to.tuning/y-slack {:default 4 :doc "Blocks a go-to :pos y with no standable cell within :range may be re-aimed up or down its column to the ground; 0: never." :spec (a/int-in 0 nil)}
   :jobs.movement.go-to.tuning/goal-danger {:default :wait :doc "When a go-to's goal lies within the full-cost radius of a known mob the body would flee: :wait (walk on; an info goal-in-danger says so once), :end (give up at once, :why :goal-dangerous with the mob)." :spec #{:wait :end}}
   :jobs.movement.go-to.tuning/look-round {:default true :doc "Whether a go-to looks round once on arrival; false for a caller that keeps moving." :spec boolean?}
   :jobs.movement.go-to.tuning/max-cut-restarts {:default 3 :doc "Cuts in a row, each sending the body back, before a go-to call gives up :cut-again." :spec (a/int-in 1 nil)}
   :jobs.movement.go-to.tuning/cut-progress {:default 2 :doc "Blocks a go-to's closest approach to its goal must improve to clear its cut count." :spec (a/num-in 0 nil)}
   :jobs.movement.go-to.tuning/max-cut-resets {:default 2 :doc "Times a go-to clears its cut count at a restart that knows of no hostile near the way, per goal; after that a full set of cuts gives up :cut-again whatever is near." :spec (a/int-in 0 nil)}
   :jobs.movement.go-to.tuning/send-back-margin {:default 3 :doc "Blocks farther from the goal than where the cut found the body a go-to restart must begin to count as sent back (knockback and in-place reflexes stay under it)." :spec (a/num-in 0 nil)}
   :jobs.movement.go-to.tuning/surface-air {:default 7.5 :doc "Seconds of air left under which a go-to whose head is under water and whose search goes on (no walk) swims to air first: a body holding still in water sinks while it thinks." :spec (a/num-in 0 nil)}
   :jobs.movement.go-to.tuning/wet-restarts {:default 2 :doc "Times a go-to's search under water may begin afresh (no walk) after its first slice before it swims to air first and plans from there." :spec (a/int-in 0 nil)}
   :jobs.movement.go-to.tuning/air-looks {:default 3 :doc "Under water with no air known: rounds of looking again for air (goal head, open water ahead), the aim shifted a little each round, before a go-to gives up :air." :spec (a/int-in 0 nil)}
   :jobs.movement.go-to.tuning/air-look-min {:default 1.5 :doc "Seconds of air left below which a go-to looks again for air no more; the fallback (swim up, breathe) needs them." :spec (a/num-in 0 nil)}
   :jobs.movement.go-to.tuning/leave-water {:default true :doc "Whether an arrival within range in water walks on to a dry stand; false keeps it in the water (a swim target, a boat)." :spec boolean?}
   :jobs.movement.go-to.tuning/dry-radius {:default 6 :doc "Blocks from the body an arrival in water reads for a dry stand (also within :range of the goal)." :spec (a/num-in 0 nil)}
   :jobs.movement.go-to.tuning/dry-walk-rounds {:default 3 :doc "Walk rounds (search slices or walks) the walk on from an arrival in water to a dry stand takes at most." :spec (a/int-in 1 nil)}
   :jobs.movement.go-to.tuning/climb-rise {:default 16 :doc "Blocks a goal must lie above a body under ground for its go-to to plan no dive (:jobs.movement.go-to.tuning/climb-dip)." :spec (a/num-in 0 nil)}
   :jobs.movement.go-to.tuning/climb-dip {:default 24 :doc "Blocks below where it began that a go-to of a body under ground to a goal far above (:jobs.movement.go-to.tuning/climb-rise) may plan; a way that dives deeper is not walked (:way-dives)." :spec (a/num-in 0 nil)}
   :jobs.movement.go-to.tuning/climb-stair {:default true :doc "Whether a go-to that would give up :way-dives cuts a stair up instead (its escalation, when the call allows :stair)." :spec boolean?}
   :jobs.movement.go-to.tuning/max-waypoint-misses {:default 3 :doc "Waypoint walks toward a column goal whose ground is only guessed that a call takes while they bring the body no nearer it." :spec (a/int-in 0 nil)}
   :jobs.movement.go-to.tuning/waypoint-gain {:default 2 :doc "Blocks (across) a waypoint must lie nearer a guessed column goal than the body, and a waypoint walk must bring the body nearer, not to be a miss." :spec (a/num-in 1 nil)}
   :jobs.movement.go-to.tuning/off-line-radius {:default 16 :doc "Blocks round the body an off-line waypoint (a stand past a cliff on the line to a guessed column goal) is looked for: about what a body sees of the land." :spec (a/num-in 1 nil)}
   :jobs.movement.go-to.tuning/off-line-cone {:default 60 :doc "The most, in degrees, an off-line waypoint's bearing may turn from the bearing of the column goal." :spec (a/num-in 0 180)}
   :jobs.movement.go-to.tuning/quote-most-factor {:default 3 :doc "A go-to quote's most time for a found route: this many times the planner's seconds (replans, doors, escalation)." :spec (a/num-in 1 nil)}
   :jobs.movement.go-to.tuning/quote-unseen-factor {:default 2 :doc "A go-to quote's mean counts each block still to go past a partial route (or with no route, when the call may change the world) as this many walked blocks of unseen terrain (jobs.lib.cost.planner/walk-floor each)." :spec (a/num-in 0 nil)}
   :jobs.movement.go-to.tuning/max-route-replans {:default 8 :doc "Times a go-to's :dig-route escalation plans its route again, as looks show cells it guessed were rock otherwise, before it fails :no-route." :spec (a/int-in 0 nil)}
   :jobs.movement.go-to.tuning/way-looks {:default [[6 1] [3 4]] :doc "The looks taken in turn before each waypoint walk: [blocks ahead toward the goal, blocks under the feet] (ahead and a little down: a slope up; close and steeply down: a slope or drop below an edge)." :spec (a/coll-of (a/pred "a pair [ahead down] of numbers >= 0" #(and (sequential? %) (= 2 (count %)) (every? (fn [n] (and (number? n) (>= n 0))) %))))}})

(defn max-blocked [] (settings/get settings :jobs.movement.go-to.tuning/max-blocked))
(defn max-searching [] (settings/get settings :jobs.movement.go-to.tuning/max-searching))
(defn max-escalations [] (settings/get settings :jobs.movement.go-to.tuning/max-escalations))
(defn y-slack [] (settings/get settings :jobs.movement.go-to.tuning/y-slack))
(defn goal-danger [] (settings/get settings :jobs.movement.go-to.tuning/goal-danger))
(defn look-round [] (settings/get settings :jobs.movement.go-to.tuning/look-round))
(defn max-route-replans [] (settings/get settings :jobs.movement.go-to.tuning/max-route-replans))
(defn max-cut-restarts [] (settings/get settings :jobs.movement.go-to.tuning/max-cut-restarts))
(defn max-cut-resets [] (settings/get settings :jobs.movement.go-to.tuning/max-cut-resets))
(defn cut-progress [] (settings/get settings :jobs.movement.go-to.tuning/cut-progress))
(defn send-back-margin [] (settings/get settings :jobs.movement.go-to.tuning/send-back-margin))

(defn surface-air [] (settings/get settings :jobs.movement.go-to.tuning/surface-air))
(defn wet-restarts [] (settings/get settings :jobs.movement.go-to.tuning/wet-restarts))
(defn air-looks [] (settings/get settings :jobs.movement.go-to.tuning/air-looks))
(defn air-look-min [] (settings/get settings :jobs.movement.go-to.tuning/air-look-min))
(defn leave-water [] (settings/get settings :jobs.movement.go-to.tuning/leave-water))
(defn dry-radius []  (settings/get settings :jobs.movement.go-to.tuning/dry-radius))
(defn dry-walk-rounds [] (settings/get settings :jobs.movement.go-to.tuning/dry-walk-rounds))
(defn climb-rise [] (settings/get settings :jobs.movement.go-to.tuning/climb-rise))
(defn climb-dip [] (settings/get settings :jobs.movement.go-to.tuning/climb-dip))
(defn climb-stair [] (settings/get settings :jobs.movement.go-to.tuning/climb-stair))
(defn max-waypoint-misses [] (settings/get settings :jobs.movement.go-to.tuning/max-waypoint-misses))
(defn off-line-radius [] (settings/get settings :jobs.movement.go-to.tuning/off-line-radius))
(defn off-line-cone [] (settings/get settings :jobs.movement.go-to.tuning/off-line-cone))
(defn waypoint-gain [] (settings/get settings :jobs.movement.go-to.tuning/waypoint-gain))
(defn way-looks [] (settings/get settings :jobs.movement.go-to.tuning/way-looks))
(defn quote-most-factor [] (settings/get settings :jobs.movement.go-to.tuning/quote-most-factor))
(defn quote-unseen-factor [] (settings/get settings :jobs.movement.go-to.tuning/quote-unseen-factor))
