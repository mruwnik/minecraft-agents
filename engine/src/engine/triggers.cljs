(ns engine.triggers
  "Trigger definitions. :job is the default job spec (an expression, see
  engine.expr); :args are the trigger's own. See README.md, Triggers and the
  register."
  (:require [engine.jobs.combat :as combat]
            [engine.jobs.reach :as reach]
            [engine.jobs.util :as u]
            [engine.triggers.suffocating :as suffocating]
            [engine.triggers.burning :as burning]
            [engine.triggers.wedged :as wedged]
            [engine.triggers.door-left :as door-left]
            [engine.triggers.hungry :as hungry]
            [engine.triggers.pen-gate :as pen-gate]
            [engine.triggers.night :as night]
            [engine.triggers.scaffold-left :as scaffold-left]
            [engine.triggers.stuck :as stuck]
            [engine.triggers.tidy-pending :as tidy-pending]
            [engine.triggers.mounted :as mounted]
            [engine.triggers.player-joined :as player-joined]
            [engine.triggers.died :as died]))

(def hostile-radius 8)

(def ranged-radius 16)

(def hostile-near
  "Holds when a real danger is near. A danger is either:
    a known hostile mob within :radius (args, default 8) that has a walkable way to the body
      (engine.jobs.reach: not walled off, not across a pit it cannot climb, body not sealed in), or
    a known ranged mob (engine.jobs.combat/ranged-mobs) with a line of fire within :ranged-radius (default 16).
  Only known mobs count (engine.perception's mob memory), so an unseen silent creeper behind the body does not.
  :visible-only false lets a heard melee mob count unseen; the way to the body still counts.
  Players and passive mobs never count. A dead body (a :died entry with no newer :respawned) sees no danger.
  The job's own :radius and :ranged-radius are set in the entry's :job spec.
  A danger reflex: it never cools down by default (:persistence :retry) and its job is never backed off.
  An agent may set :persistence :cooldown with :cooldown-s, or :backoff, in its own entry."
  {:name :hostile-near
   :when (fn [world memory args]
           (boolean (and (not (died/dead? memory))
                         (some? (reach/nearest-danger world (:radius args hostile-radius)
                                                      {:ranged-radius (:ranged-radius args ranged-radius)}
                                                      {:sight? (:visible-only args true)})))))
   :job '(jobs.survival.respond-to-hostile)
   :args {:radius hostile-radius :ranged-radius ranged-radius}
   :persistence :retry})

(def nearly-full-free 2)

(def inventory-nearly-full
  "Holds when at most :free (args, default 2) of the 36 main and hotbar slots are empty.
  make-room puts things in a known chest, else tosses the least valuable stacks.
  The 120 s cooldown stops a body with nothing it may toss from retrying every tick."
  {:name :inventory-nearly-full
   :when (fn [world _memory args]
           (<= (u/free-slots world) (:free args nearly-full-free)))
   :job '(jobs.storage.make-room)
   :args {:free nearly-full-free}
   :persistence :cooldown
   :cooldown-s 120})

(def all
  "Every trigger by name, listed in the order a survival scenario registers
  them (the register is ordered by the scenario, not by this map)."
  (into {} (map (juxt :name identity))
        [suffocating/suffocating burning/burning wedged/wedged hostile-near hungry/hungry
         night/trigger stuck/stuck died/died pen-gate/trigger
         door-left/trigger inventory-nearly-full scaffold-left/trigger tidy-pending/trigger mounted/trigger player-joined/trigger]))
