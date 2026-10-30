(ns jobs.lib.clock
  "The wall clock jobs time their own work by (search rounds, planning, steers, looks for air). The :test build stops
  it (engine.core.clock/frozen)."
  (:require [engine.core.clock :as engine-clock]))

(def now "Wall-clock ms." (if engine-clock/frozen (constantly 0) (fn [] (js/performance.now))))
