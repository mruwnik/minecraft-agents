(ns jobs.lib.per-body
  "The globals code keeps per body (keyed by its primitives) beside job memory. Nothing drops an entry once its body is gone
  (a test's fake, a warm process's earlier run), so each registers a clear fn here (store!), run at a warm start and after
  every test var (engine.timing-test)."
  (:require [engine.core.base :as base]))

(defonce ^{:doc "{key (fn [])}: empties one per-body global."} stores (atom {}))

(defn store! "Register (clear) under key k." [k clear] (swap! stores assoc k clear))

(defn clear-stores! "Empty every registered per-body global." [] (doseq [[_ clear] @stores] (clear)))

(base/on-warm-start! ::stores clear-stores!)
