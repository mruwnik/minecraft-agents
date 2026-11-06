(ns jobs.lib.clock
  "The performance clock (ms since process start) jobs time their own work by (search rounds, planning, steers, looks for air). The :test build stops
  it (engine.core.clock/frozen)."
  (:require [engine.core.clock :as engine-clock]))

(def now "Process-relative ms (performance.now)." (if engine-clock/frozen (constantly 0) (fn [] (js/performance.now))))
