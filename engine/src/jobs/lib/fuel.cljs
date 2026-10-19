(ns jobs.lib.fuel
  "What burns in a furnace.")

(defn wood-fuel?
  "Overworld planks, logs and wood burn; the nether ones (crimson, warped) do not."
  [name]
  (boolean (and (re-find #"_(planks|log|wood)$" name) (not (re-find #"^(crimson|warped)_" name)))))

(defn fuel-per-unit
  "How many items one fuel item smelts (the same in every kind: the quick ones cook and burn twice as fast), or nil
  when it is no fuel this job knows."
  [_kind name]
  (cond
    (#{"coal" "charcoal"} name) 8
    (= "coal_block" name) 80
    (= "blaze_rod" name) 12
    (= "dried_kelp_block" name) 20
    (wood-fuel? name) 1.5
    (= "stick" name) 0.5))
