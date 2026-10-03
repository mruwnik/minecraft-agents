(ns engine.triggers
  "Trigger definitions. See README.md, Triggers and the register."
  (:require [engine.memory :as mem]))

(def health-low
  {:name :health-low
   :when (fn [world _memory _args] (<= (.-health (.self world)) 8))
   :job :eat
   :args {}
   :persistence :cooldown
   :cooldown-s 30})

(def hostile-radius 8)

(def hostile-near
  {:name :hostile-near
   :when (fn [world _memory _args]
           (pos? (.-length (.entities world #js {:radius hostile-radius :kind "hostile" :max 1}))))
   :job :retreat
   :args {:radius hostile-radius}
   :persistence :cooldown
   :cooldown-s 5})

(def night-and-bed-known
  {:name :night-and-bed-known
   :when (fn [world memory _args]
           (and (not (.-isDay (.self world)))
                (boolean (seq (mem/places memory :bed)))))
   :job :sleep
   :args {}
   :persistence :cooldown
   :cooldown-s 60})

(def nearly-full-stacks 30)

(def inventory-nearly-full
  {:name :inventory-nearly-full
   :when (fn [world memory _args]
           (and (>= (.-length (.-inventory (.self world))) nearly-full-stacks)
                (boolean (seq (mem/places memory :chest)))))
   :job :deposit
   :args {}
   :persistence :cooldown
   :cooldown-s 60})

(def default-interval-s 60)

(def every-interval
  "Holds when the last look-around (recorded in body memory by the job, so it
  survives a restart) is at least :seconds old. With no record it holds at
  once, so the first firing is not delayed. Once the job has recorded the
  time it stops holding, so there is no cooldown to wait out."
  {:name :every-interval
   :when (fn [_world memory args]
           (let [last (get-in memory [:body :every-interval-last])]
             (or (nil? last)
                 (>= (- (:now memory) last) (* 1000 (:seconds args default-interval-s))))))
   :job :look-around
   :args {:seconds default-interval-s}
   :persistence :cooldown
   :cooldown-s 0})
