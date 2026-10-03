(ns engine.triggers
  "Trigger definitions. :job is the default job spec (an expression, see
  engine.expr); :args are the trigger's own. See README.md, Triggers and the
  register."
  (:require [engine.memory :as mem]
            [engine.triggers.stuck :as stuck]))

(def default-health 8)

(def health-low
  "Holds when health is at most :health (args, default 8)."
  {:name :health-low
   :when (fn [world _memory args] (<= (.-health (.self world)) (:health args default-health)))
   :job '(jobs.survival.eat)
   :args {:health default-health}
   :persistence :cooldown
   :cooldown-s 30})

(def hostile-radius 8)

(def hostile-near
  "Holds when a hostile is within :radius (args, default 8). The retreat
  job's own :radius (default 8) is set in the entry's :job spec."
  {:name :hostile-near
   :when (fn [world _memory args]
           (let [radius (:radius args hostile-radius)]
             (pos? (.-length (.entities world #js {:radius radius :kind "hostile" :max 1})))))
   :job '(jobs.survival.retreat)
   :args {:radius hostile-radius}
   :persistence :cooldown
   :cooldown-s 5})

(def night-and-bed-known
  {:name :night-and-bed-known
   :when (fn [world memory _args]
           (and (not (.-isDay (.self world)))
                (some? (mem/place memory :bed))))
   :job '(jobs.survival.sleep)
   :args {}
   :persistence :cooldown
   :cooldown-s 60})

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

(def all
  "Every trigger by name."
  (into {} (map (juxt :name identity))
        [health-low hostile-near night-and-bed-known inventory-nearly-full every-interval stuck/stuck]))
