(ns jobs.farm.permit
  "Ask jobs.lib.access.rules about a cell of a job's world: the zones, claims and the footprints of every plan but the
  one the job works are read afresh on each call, the body's feet where they are now. For jobs that work a plan's cells
  and so must ask when a cell is chosen and again right before the act."
  (:require [jobs.lib.access.rules :as rules]
            [jobs.lib.access :as access]
            [jobs.lib.util :as u]))

(defn cell-of [{:keys [x y z]}] [x y z])

(defn feet-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn permit
  "The rules' verdict for action at pos {:x :y :z} for a job working plan (nil: no plan's footprint is left out):
  :dig, :place, or :sow (a place of a plant, which has no collision, so the body standing in the cell is no reason to
  refuse it). opts: :ignore-zones?, the job's opt-out (see jobs.lib.access/zone-input)."
  ([c plan action pos] (permit c plan action pos {}))
  ([c plan action pos opts]
   (let [p (:primitives c)
         in (merge {:block-at (fn [[x y z]] (u/block-name p {:x x :y y :z z})) :cell (cell-of pos) :feet (feet-of c)
                    :ledger #{}}
                   (access/zone-input c (assoc opts :except plan)))]
     (case action
       :dig (rules/may-dig? in)
       :place (rules/may-place? in)
       :sow (rules/may-place? (assoc in :feet nil))))))

(defn ok?
  "Whether action at pos is permitted; a hazard of a dig is no refusal (the job decides what it accepts)."
  ([c plan action pos] (ok? c plan action pos {}))
  ([c plan action pos opts] (boolean (:ok (permit c plan action pos opts)))))

(defn refusal
  "The reason of a refusal that will not go away by itself (:footprint, :zone, :no-zones, :not-replaceable), or nil when
  the cell is permitted or only passing (not loaded, the body stands in it)."
  ([c plan action pos] (refusal c plan action pos {}))
  ([c plan action pos opts]
  (let [{:keys [ok reason]} (permit c plan action pos opts)]
    (when-not (or ok (#{:not-loaded :own-body} reason))
      reason))))
