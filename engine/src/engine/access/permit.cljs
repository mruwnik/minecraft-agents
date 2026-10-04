(ns engine.access.permit
  "Ask engine.access.rules about a cell of a job's world: the zones and the footprints of every active plan but the one
  the job works are read afresh on each call, the body's feet where they are now. For jobs that work a plan's cells
  and so must ask when a cell is chosen and again right before the act."
  (:require [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(defn cell-of [{:keys [x y z]}] [x y z])

(defn feet-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn permit
  "The rules' verdict for action at pos {:x :y :z} for a job working plan (nil: no plan's footprint is left out):
  :dig, :place, or :sow (a place of a plant, which has no collision, so the body standing in the cell is no reason to
  refuse it)."
  [c plan action pos]
  (let [p (:primitives c)
        in {:block-at (fn [[x y z]] (u/block-name p {:x x :y y :z z})) :cell (cell-of pos) :feet (feet-of c)
            :zones (ctx/zones c) :footprints (ctx/footprints c {:except plan}) :ledger #{}}]
    (case action
      :dig (rules/may-dig? in)
      :place (rules/may-place? in)
      :sow (rules/may-place? (assoc in :feet nil)))))

(defn ok?
  "Whether action at pos is permitted; a hazard of a dig is no refusal (the job decides what it accepts)."
  [c plan action pos]
  (boolean (:ok (permit c plan action pos))))

(defn refusal
  "The reason of a refusal that will not go away by itself (:footprint, :zone, :no-zones, :not-replaceable), or nil when
  the cell is permitted or only passing (not loaded, the body stands in it)."
  [c plan action pos]
  (let [{:keys [ok reason]} (permit c plan action pos)]
    (when-not (or ok (#{:not-loaded :own-body} reason))
      reason)))
