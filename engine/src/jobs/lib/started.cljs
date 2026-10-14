(ns jobs.lib.started
  "A condition that holds only at a call's start: the engine re-checks conditions at every re-entered round, and a fact a
  started job changes itself (or the world moves on) must not end it condition-failed."
  (:require [engine.ctx :as ctx]))

(defn at-start
  "f of the call, at its start only; nil once it began."
  [f]
  (fn [c] (when-not (ctx/frozen-call c) (f c))))
