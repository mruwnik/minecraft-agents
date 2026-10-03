(ns engine.triggers
  "Trigger definitions. See README.md, Triggers and the register."
  (:require [engine.memory :as mem]))

(def health-low
  {:name :health-low
   :when (fn [world _memory] (<= (.-health (.self world)) 8))
   :job :eat
   :args {}
   :persistence :cooldown
   :cooldown-s 30})

(def hostile-radius 8)

(def hostile-near
  {:name :hostile-near
   :when (fn [world _memory]
           (pos? (.-length (.entities world #js {:radius hostile-radius :kind "hostile" :max 1}))))
   :job :retreat
   :args {:radius hostile-radius}
   :persistence :cooldown
   :cooldown-s 5})

(def night-and-bed-known
  {:name :night-and-bed-known
   :when (fn [world memory]
           (and (not (.-isDay (.self world)))
                (boolean (seq (mem/places memory :bed)))))
   :job :sleep
   :args {}
   :persistence :cooldown
   :cooldown-s 60})

(def nearly-full-stacks 30)

(def inventory-nearly-full
  {:name :inventory-nearly-full
   :when (fn [world memory]
           (and (>= (.-length (.-inventory (.self world))) nearly-full-stacks)
                (boolean (seq (mem/places memory :chest)))))
   :job :deposit
   :args {}
   :persistence :cooldown
   :cooldown-s 60})
