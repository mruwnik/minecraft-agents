(ns jobs.lib.clock
  "The wall clock jobs time their own work by (search rounds, planning, steers, looks for air). Tests stop it
  (engine.test-util): how much work a call does must not hang on the machine's load.")

(def now "Wall-clock ms." (fn [] (js/performance.now)))
