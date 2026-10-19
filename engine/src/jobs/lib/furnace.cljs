(ns jobs.lib.furnace
  "What a smelt call's memory says about its furnace load.")

(defn loaded?
  "Whether a smelt call's memory m holds a load put in the furnace."
  [m]
  (some? (:owed m)))

(defn loaded-count
  "The count a smelt call's memory m has put in its furnace and still owes while the cook is not due at now, else nil."
  [m now]
  (when (and (loaded? m) (some? (:ready-at m)) (< now (:ready-at m)))
    (let [{:keys [count base] :or {count 0 base 0}} (:owed m)]
      (- count base))))
