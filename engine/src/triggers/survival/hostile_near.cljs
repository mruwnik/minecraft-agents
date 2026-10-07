(ns triggers.survival.hostile-near
  "The hostile-near trigger: a real danger is near."
  (:require [jobs.lib.danger :as danger-q]
            [triggers.survival.died :as died]))

(def hostile-radius 8)

(def ranged-radius 16)

(def defaults {:radius hostile-radius :ranged-radius ranged-radius})

(defn hostile-near
  "Holds when a real danger is near. A danger is either:
    a known hostile mob within :radius (args, default 8) that has a walkable way to the body
      (jobs.lib.danger: not walled off, not across a pit it cannot climb, body not sealed in), or
    a known ranged mob (jobs.lib.cost.weapon/ranged-mobs) with a line of fire within :ranged-radius (default 16).
  Only known mobs count (engine.perception's mob memory), so an unseen silent creeper behind the body does not.
  :visible-only false lets a heard melee mob count unseen; the way to the body still counts.
  Players and passive mobs never count. A dead body (a :died entry with no newer :respawned) sees no danger.
  The job's own :radius and :ranged-radius are set in the entry's :job spec.
  A danger reflex: it never cools down by default (:persistence :retry) and its job is never backed off.
  An agent may set :persistence :cooldown with :cooldown-s, or :backoff, in its own entry."
  [world memory args]
  (boolean (and (not (died/dead? memory))
                (danger-q/danger-near? world (:radius args hostile-radius) (:ranged-radius args ranged-radius)
                                       {:sight? (:visible-only args true)}))))
