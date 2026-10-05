(ns engine.triggers
  "Trigger definitions. :job is the default job spec (an expression, see
  engine.expr); :args are the trigger's own. See README.md, Triggers and the
  register."
  (:require [engine.memory :as mem]
            [engine.jobs.combat :as combat]
            [engine.jobs.reach :as reach]
            [engine.jobs.util :as u]
            [engine.triggers.suffocating :as suffocating]
            [engine.triggers.burning :as burning]
            [engine.triggers.door-left :as door-left]
            [engine.triggers.hungry :as hungry]
            [engine.triggers.pen-gate :as pen-gate]
            [engine.triggers.night-unsafe :as night-unsafe]
            [engine.triggers.shut-in-by-day :as shut-in-by-day]
            [engine.triggers.player-sleeping-nearby :as player-sleeping-nearby]
            [engine.triggers.scaffold-left :as scaffold-left]
            [engine.triggers.stuck :as stuck]
            [engine.triggers.tidy-pending :as tidy-pending]
            [engine.triggers.mounted :as mounted]
            [engine.triggers.player-joined :as player-joined]
            [engine.triggers.died :as died]))

(def default-health 7)

(def health-low
  "Holds when health is below :health (args, default 7 of 20). Runs recover,
  which flees, eats and waits for regeneration."
  {:name :health-low
   :when (fn [world _memory args] (< (.-health (.self world)) (:health args default-health)))
   :job '(jobs.survival.recover)
   :args {:health default-health}
   :persistence :cooldown
   :cooldown-s 10})

(def hostile-radius 8)

(def ranged-radius 16)

(def hostile-near
  "Holds when a real danger is within :radius (args, default 8): a hostile
  mob the body can see AND that has a walkable way to it (engine.jobs.reach:
  not walled off, not across a pit it cannot climb, not with the body sealed
  in), or a ranged one (skeleton, stray, bogged, pillager, witch; see
  engine.jobs.combat/ranged-mobs) with a line of fire within :ranged-radius
  (default 16, about a skeleton's range). Sight is a block raycast from the
  eye to the mob (the `visible` field of sensing). :visible-only false drops
  the sight test of a melee mob (the way to the body still counts). Players and passive mobs are
  other entity kinds and never count. A dead body (a :died entry with no
  newer :respawned) sees no danger. The job's own :radius and
  :ranged-radius are set in the entry's :job spec."
  {:name :hostile-near
   :when (fn [world memory args]
           (boolean (and (not (died/dead? memory))
                         (seq (reach/dangers world (:radius args hostile-radius)
                                             {:ranged-radius (:ranged-radius args ranged-radius)}
                                             {:sight? (:visible-only args true)})))))
   :job '(jobs.survival.respond-to-hostile)
   :args {:radius hostile-radius :ranged-radius ranged-radius}
   :persistence :cooldown
   :cooldown-s 5})

(def nearly-full-free 2)

(def inventory-nearly-full
  "Holds when at most :free (args, default 2) of the 36 main and hotbar slots
  are empty. No chest is needed: make-room puts things in a known chest when
  there is one and tosses the least valuable stacks when there is not. A
  cooldown of 120 s makes a body with nothing it may toss not retry
  every tick."
  {:name :inventory-nearly-full
   :when (fn [world _memory args]
           (<= (u/free-slots world) (:free args nearly-full-free)))
   :job '(jobs.storage.make-room)
   :args {:free nearly-full-free}
   :persistence :cooldown
   :cooldown-s 120})

(def default-interval-s 60)

(def every-interval
  "Holds when the latest :looked entry (written by the look-around job, so it
  survives a restart) is at least :seconds old. With none it holds at once,
  so the first firing is not delayed. Once the job has written the entry it
  stops holding, so there is no cooldown to wait out."
  {:name :every-interval
   :when (fn [_world memory args]
           (let [last (:t (mem/latest memory :looked))]
             (or (nil? last)
                 (>= (- (:now memory) last) (* 1000 (:seconds args default-interval-s))))))
   :job '(jobs.movement.look-around)
   :args {:seconds default-interval-s}
   :persistence :cooldown
   :cooldown-s 0})

(def night-and-bed-known
  "Alias of night-unsafe under its old name, which the shipped scenarios
  still register (they are not edited here). It now fires shelter."
  (assoc night-unsafe/trigger :name :night-and-bed-known))

(def all
  "Every trigger by name, listed in the order a survival scenario registers
  them (the register is ordered by the scenario, not by this map)."
  (into {} (map (juxt :name identity))
        [suffocating/suffocating burning/burning hostile-near health-low hungry/hungry
         night-unsafe/trigger shut-in-by-day/trigger player-sleeping-nearby/trigger night-and-bed-known stuck/stuck died/died pen-gate/trigger
         door-left/trigger inventory-nearly-full every-interval scaffold-left/trigger tidy-pending/trigger mounted/trigger player-joined/trigger]))
