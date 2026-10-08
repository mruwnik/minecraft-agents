(ns jobs.movement.go-to.air
  "go-to under water when no way to the goal fits the air left: swim to air first, then plan again."
  (:require [engine.ctx :as ctx]
            [jobs.lib.breath :as breath]
            [jobs.lib.near :as near]
            [jobs.lib.util :as u]))

(def max-surfacings "Walks to air one go-to call makes at most." 3)

(def look-args "breath/find-air!'s look: columns within :radius, cells up to :reach above the feet." {:radius 2 :reach 10})

(def air-radius "Columns this far sideways are read for air when the looks found none nearer (breathe's :air-radius default)." 8)

(defn ^:async swim-up-blind!
  "No air known and nothing seen over the own column: swim up it, as breathe does in the dark (a body learns by moving).
  True when the swim surfaced or rose; nil otherwise."
  [c pos]
  (let [fy (js/Math.floor (:y pos))
        r (await (ctx/act c :swim #js {}))
        y (js/Math.floor (:y (u/self-pos c)))]
    (or (= "surfaced" (.-status r)) (> y fy))))

(defn ^:async surface!
  "A round refused :air with the head under water: the planner refuses a swim past the air limit unless the goal is a
  way to air, so a far goal leaves the body under water. Look for air (up the own column first, then the columns beside
  it, breath/find-air!), else read seen air within air-radius, and walk one round (near/walk-round! with walk-opts) to
  that surface cell, range 0: a way to air, whose drowning is priced, not refused. At most max-surfacings a call (mem
  :surfacings). True when it walked there or on (the caller plans again); nil when the head is out, the tries are spent
  or no air is known and the own column is not open (a block seen over it). With no air known and the own column open, swims up it blind (one swim)."
  [c walk-opts]
  (let [p (:primitives c)
        n (:surfacings (ctx/mem c) 0)]
    (when (and (< n max-surfacings) (breath/head-under? p))
      (let [pos (u/self-pos c)
            target (or (await (breath/find-air! (assoc c :args look-args) pos))
                       (breath/nearest-air p pos air-radius (:reach look-args) nil false))]
        (if-not target
          (when (breath/column-open? p (js/Math.floor (:x pos)) (js/Math.floor (:z pos)) (js/Math.floor (:y pos)) (:reach look-args))
            (ctx/update-mem! c assoc :surfacings (inc n))
            (ctx/emit! c :go-to.surfacing :info {:text "short of air, nothing seen above: swimming up blind"})
            (await (swim-up-blind! c pos)))
          (do
            (ctx/update-mem! c assoc :surfacings (inc n))
            (ctx/emit! c :go-to.surfacing :info {:at target :text (str "short of air: swimming up to " [(:x target) (:y target) (:z target)] " first")})
            (let [{:keys [status]} (await (near/walk-round! c target 0 walk-opts))]
              (contains? #{"arrived" "partial" "searching"} status))))))))
