(ns jobs.movement.go-to.air
  "go-to under water when no way to the goal fits the air left: swim to air first, then plan again."
  (:require [engine.ctx :as ctx]
            [jobs.lib.breath :as breath]
            [jobs.lib.near :as near]
            [jobs.lib.util :as u]
            [jobs.survival.breathe :as breathe]))

(def max-surfacings "Walks to air one go-to call makes at most." 3)

(def look-args "breathe/find-air!'s look: columns within :radius, cells up to :reach above the feet." {:radius 2 :reach 10})

(def air-radius "Columns this far sideways are read for air when the looks found none nearer (breathe's :air-radius default)." 8)

(defn ^:async surface!
  "A round refused :air with the head under water: the planner refuses a swim past the air limit unless the goal is a
  way to air, so a far goal leaves the body under water. Look for air (up the own column first, then the columns beside
  it, breathe/find-air!), else read seen air within air-radius, and walk one round (near/walk-round! with walk-opts) to
  that surface cell, range 0: a way to air, whose drowning is priced, not refused. At most max-surfacings a call (mem
  :surfacings). True when it walked there or on (the caller plans again); nil when the head is out, the tries are spent
  or no air is known."
  [c walk-opts]
  (let [p (:primitives c)
        n (:surfacings (ctx/mem c) 0)]
    (when (and (< n max-surfacings) (breath/head-under? p))
      (let [pos (u/self-pos c)
            target (or (await (breathe/find-air! (assoc c :args look-args) pos))
                       (breathe/nearest-air p pos air-radius (:reach look-args) nil false))]
        (when target
          (ctx/update-mem! c assoc :surfacings (inc n))
          (ctx/emit! c :go-to.surfacing :info {:at target :text (str "short of air: swimming up to " [(:x target) (:y target) (:z target)] " first")})
          (let [{:keys [status]} (await (near/walk-round! c target 0 walk-opts))]
            (contains? #{"arrived" "partial" "searching"} status)))))))
