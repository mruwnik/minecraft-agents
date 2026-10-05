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

(def default-healed 16)

(def spell-ms
  "A :hurt entry (written by recover when a spell starts) older than this no longer keeps a spell going; the same
  window as recover's own check."
  (* 5 60 1000))

(def regen-food "Natural regeneration needs at least this much food." 18)

(defn spell-under-way?
  "A recover spell started (the latest :hurt entry, younger than spell-ms) and has not ended (no :heal-ended entry
  newer than it)."
  [view]
  (let [hurt (:t (mem/latest view :hurt))
        ended (:t (mem/latest view :heal-ended))]
    (boolean (and hurt
                  (< (- (:now view) hurt) spell-ms)
                  (not (and ended (>= ended hurt)))))))

(defn cannot-heal?
  "recover gave up (the latest :heal-ended says :cannot-heal) and healing still cannot work: food below regen-food and
  nothing to eat carried."
  [self view]
  (boolean (and (= :cannot-heal (:why (:data (mem/latest view :heal-ended))))
                (< (.-food self) regen-food)
                (not (hungry/carries-food? self)))))

(def health-low
  "Holds when health is below :health (args, default 7 of 20), or a recover spell is under way (spell-under-way?)
  and health is still below :healed (default 16): a recover cut by a higher reflex is fired again until the body is
  healed. It rests while recover's give-up stands (cannot-heal?: a cannot_heal warn said so) until food is carried
  or food reaches 18. Runs recover, which flees, eats and waits for regeneration."
  {:name :health-low
   :when (fn [world memory args]
           (let [self (.self world)
                 health (.-health self)]
             (and (or (< health (:health args default-health))
                      (and (< health (:healed args default-healed)) (spell-under-way? memory)))
                  (not (cannot-heal? self memory)))))
   :job '(jobs.survival.recover)
   :args {:health default-health :healed default-healed}
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
  :ranged-radius are set in the entry's :job spec. A danger reflex: by default
  it never cools down (:persistence :retry) and its job is never backed off,
  so it reacts every time the danger is there; an agent may set
  :persistence :cooldown with :cooldown-s, or :backoff, in its own entry."
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

(def all
  "Every trigger by name, listed in the order a survival scenario registers
  them (the register is ordered by the scenario, not by this map)."
  (into {} (map (juxt :name identity))
        [suffocating/suffocating burning/burning hostile-near health-low hungry/hungry
         night-unsafe/trigger shut-in-by-day/trigger player-sleeping-nearby/trigger stuck/stuck died/died pen-gate/trigger
         door-left/trigger inventory-nearly-full scaffold-left/trigger tidy-pending/trigger mounted/trigger player-joined/trigger]))
