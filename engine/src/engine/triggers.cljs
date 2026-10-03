(ns engine.triggers
  "Trigger definitions. See README.md, Triggers and the register.")

(def health-low
  {:name :health-low
   :when (fn [world _memory] (<= (.-health (.self world)) 8))
   :job :eat
   :args {}
   :persistence :cooldown
   :cooldown-s 30})
