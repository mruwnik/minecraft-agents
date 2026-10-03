(ns engine.triggers
  "Trigger definitions. :job is the default job spec (an expression, see
  engine.expr); :args are the trigger's own. See README.md, Triggers and the
  register."
  (:require [engine.memory :as mem]
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

(def hostile-near
  "Holds when a hostile mob the body can see is within :radius (args, default
  8). Sight is a block raycast from the eye to the mob (the `visible` field of
  sensing), so a hostile behind a wall is silent; :visible-only false counts
  every hostile again. Players and passive mobs are other entity kinds and
  never count. The job's own :radius (default 8) is set in the entry's :job
  spec."
  {:name :hostile-near
   :when (fn [world _memory args]
           (let [radius (:radius args hostile-radius)
                 seen? (:visible-only args true)]
             (boolean (some #(or (not seen?) (.-visible %))
                            (array-seq (.entities world #js {:radius radius :kind "hostile" :max 32}))))))
   :job '(jobs.survival.respond-to-hostile)
   :args {:radius hostile-radius}
   :persistence :cooldown
   :cooldown-s 5})

(def nearly-full-stacks 30)

(def inventory-nearly-full
  "Holds when at least :stacks (args, default 30) stacks are carried and a
  chest is known."
  {:name :inventory-nearly-full
   :when (fn [world memory args]
           (and (>= (.-length (.-inventory (.self world))) (:stacks args nearly-full-stacks))
                (some? (mem/place memory :chest))))
   :job '(jobs.storage.deposit)
   :args {:stacks nearly-full-stacks}
   :persistence :cooldown
   :cooldown-s 60})

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
        [suffocating/suffocating burning/burning health-low hostile-near hungry/hungry
         night-unsafe/trigger night-and-bed-known stuck/stuck died/died
         inventory-nearly-full every-interval]))
