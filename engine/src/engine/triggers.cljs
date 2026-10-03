(ns engine.triggers
  "Trigger definitions. :job is the default job spec (an expression, see
  engine.expr); :args are the trigger's own. See README.md, Triggers and the
  register."
  (:require [engine.memory :as mem]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]
            [engine.triggers.suffocating :as suffocating]
            [engine.triggers.burning :as burning]
            [engine.triggers.hungry :as hungry]
            [engine.triggers.night-unsafe :as night-unsafe]
            [engine.triggers.stuck :as stuck]
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
  "Holds when a hostile mob the body can see is within :radius (args, default
  8), or a ranged one (skeleton, stray, bogged, pillager, witch; see
  engine.jobs.combat/ranged-mobs) within :ranged-radius (default 16, about a
  skeleton's range). Sight is a block raycast from the eye to the mob (the
  `visible` field of sensing), so a hostile behind a wall is silent;
  :visible-only false counts every hostile again. Players and passive mobs are
  other entity kinds and never count. The job's own :radius and
  :ranged-radius are set in the entry's :job spec."
  {:name :hostile-near
   :when (fn [world _memory args]
           (let [seen? (:visible-only args true)]
             (boolean (some #(or (not seen?) (.-visible %))
                            (combat/hostiles world (:radius args hostile-radius)
                                             {:ranged-radius (:ranged-radius args ranged-radius)})))))
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
         night-unsafe/trigger night-and-bed-known stuck/stuck died/died
         inventory-nearly-full every-interval]))
