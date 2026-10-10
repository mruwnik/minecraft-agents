(ns jobs.lib.animal-spare
  "Which food animals a hunt leaves alone: kinds on the body's off-limits list, animals in the body's own pens or in any closed enclosure, and the
  last few adults of a kind so a breeding pair survives."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::off-limits {:default #{} :doc "Animal kinds (\"cow\") a hunt for food never takes, wherever they are." :spec (a/set-of a/item?)}})

(defn off-limits
  "The kinds a hunt never takes: the job's :off-limits arg over the ::off-limits setting."
  [args]
  (set (or (:off-limits args) (settings/get settings ::off-limits))))

(defn why
  "Why a hunt leaves an adult animal alone, or nil: :off-limits, :pen (in a zone or plan of the body's own, unless
  :spare-pens is false) or :kept (no more than :keep-adults adults of its kind in sight, unless starved?)."
  [args kind adults-of-kind in-own-area? starved?]
  (cond (contains? (off-limits args) kind) :off-limits
        (and (:spare-pens args) in-own-area?) :pen
        (and (<= adults-of-kind (:keep-adults args)) (not starved?)) :kept))

(defn allowed-by
  "Which rule let a hunt take an adult why left alone: :pen-taken (:spare-pens false and it stood in a pen), :surplus (more
  than :keep-adults of its kind) or :starved (the kept pair, lifted by hunger)."
  [args adults-of-kind in-own-area?]
  (cond (and in-own-area? (not (:spare-pens args))) :pen-taken
        (> adults-of-kind (:keep-adults args)) :surplus
        :else :starved))
