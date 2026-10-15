(ns jobs.lib.furnace
  "What a smelt call's memory says about its furnace load; items.smelt and its callers read it only here."
  (:require [engine.ctx :as ctx]))

(defn loaded?
  "Whether a smelt call's memory m holds a load put in the furnace."
  [m]
  (some? (:owed m)))

(defn child-loaded?
  "Whether the smelt child in slot of the running job holds a load in the furnace."
  [c slot]
  (loaded? (get-in (ctx/mem c) [:children slot])))
