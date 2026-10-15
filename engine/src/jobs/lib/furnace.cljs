(ns jobs.lib.furnace
  "What a smelt call's memory says about its furnace load.")

(defn loaded?
  "Whether a smelt call's memory m holds a load put in the furnace."
  [m]
  (some? (:owed m)))
