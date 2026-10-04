(ns engine.jobs.access
  "engine.access.rules as a dig job asks it: the blocks read through the body's primitives, the zones and the active
  plans' footprints through ctx, read once and reused for every cell asked in a round."
  (:require [clojure.string :as str]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(defn cell [{:keys [x y z]}] [x y z])

(defn rules-input
  "The rules' input without :cell. opts: :except, a plan id whose own footprint is left out (for a job working
  that plan)."
  ([c] (rules-input c {}))
  ([c {:keys [except]}]
   (let [p (:primitives c)
         {:keys [x y z]} (u/self-pos c)]
     {:block-at (fn [[bx by bz]] (u/block-name p {:x bx :y by :z bz}))
      :feet [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]
      :zones (ctx/zones c)
      :footprints (ctx/footprints c {:except except})
      :ledger #{}})))

(defn may-dig?
  "The rules' dig verdict for pos ({:x :y :z}) over input in."
  [in pos]
  (rules/may-dig? (assoc in :cell (cell pos))))

(defn judge
  "What a dig job does with verdict v given the hazards it accepts: :ok (dig), :refused (a zone or another plan's
  footprint: leave the cell for good), :hazard (a hazard not accepted), :no-zones (no zone list: dig nothing now) or
  :not-loaded."
  [v accept]
  (cond
    (rules/accepts? v (set accept)) :ok
    (:ok v) :hazard
    (#{:zone :footprint} (:reason v)) :refused
    :else (:reason v)))

(defn refusal-fields
  "Event fields naming what refused the verdicts: {:zones [name ..] :plans [id ..]}."
  [verdicts]
  {:zones (vec (distinct (keep :zone verdicts))) :plans (vec (distinct (keep :plan verdicts)))})

(defn refusal-text [{:keys [zones plans]}]
  (str (when (seq zones) (str "zone " (str/join ", " zones)))
       (when (and (seq zones) (seq plans)) "; ")
       (when (seq plans) (str "plan " (str/join ", " plans)))))

(defn decline!
  "Warn kind once for this job (per reason: :no-zones or :refused, the latter with refusal fields) and answer false,
  for a check that declines."
  [c kind job-name {:keys [reason] :as fields}]
  (ctx/warn-once! c [:access reason] kind
                  (assoc fields :text (if (= :no-zones reason)
                                        (str job-name " declined: no zone list has been read (zones.edn missing or never valid)")
                                        (str job-name " declined: every target is refused by " (refusal-text fields)))))
  false)
