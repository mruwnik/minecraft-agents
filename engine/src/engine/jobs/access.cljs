(ns engine.jobs.access
  "engine.access.rules and engine.access.zones as a job asks them: the blocks read through the body's primitives, the
  zones, claims and the active plans' footprints through ctx, read once and reused for every cell asked in a round.
  Zones are a rule the jobs consult, never enforced by the engine: a job passes :ignore-zones? to act regardless of
  zones, claims and footprints (the rules of the game allow it)."
  (:require [clojure.string :as str]
            [engine.access.rules :as rules]
            [engine.access.zones :as zones]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(defn cell [{:keys [x y z]}] [x y z])

(defn zone-input
  "The social half of the rules' input: {:zones :footprints :claims :self :now :ignore-zones?}. opts: :except, a plan
  id whose own footprint is left out (for a job working that plan); :ignore-zones?, the job's opt-out (default: the
  job's own :ignore-zones? arg)."
  ([c] (zone-input c {}))
  ([c {:keys [except] :as opts}]
   {:zones (ctx/zones c)
    :footprints (ctx/footprints c {:except except})
    :claims (ctx/claims c)
    :self (ctx/self-name c)
    :now (ctx/now c)
    :ignore-zones? (boolean (if (contains? opts :ignore-zones?) (:ignore-zones? opts) (:ignore-zones? (:args c))))}))

(defn rules-input
  "The rules' input without :cell: the blocks, the body's feet and zone-input. opts as zone-input."
  ([c] (rules-input c {}))
  ([c opts]
   (let [p (:primitives c)
         {:keys [x y z]} (u/self-pos c)]
     (merge {:block-at (fn [[bx by bz]] (u/block-name p {:x bx :y by :z bz}))
             :feet [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]
             :ledger #{}}
            (zone-input c opts)))))

(defn may-dig?
  "The rules' dig verdict for pos ({:x :y :z}) over input in."
  [in pos]
  (rules/may-dig? (assoc in :cell (cell pos))))

(defn may?
  "Whether action (:dig :place :sow :harvest :take :put) at pos ({:x :y :z}, or [x y z]) is permitted, over a ctx
  (read afresh, see rules-input) or a rules input map. :dig and :place (:sow is a place of a plant, ignoring the body's
  own cell) give the rules' verdict, physics and hazards included; :harvest, :take and :put ask zones, claims and
  footprints only. Same output shape: {:ok true :hazards [..]} or {:ok false :reason kw ...detail}."
  [c-or-in action pos]
  (let [in (if (:primitives c-or-in) (rules-input c-or-in) c-or-in)
        in (assoc in :cell (if (vector? pos) pos (cell pos)))]
    (case action
      :dig (rules/may-dig? in)
      :place (rules/may-place? in)
      :sow (rules/may-place? (assoc in :feet nil))
      (if (:ignore-zones? in)
        {:ok true}
        (let [v (zones/verdict (assoc (select-keys in [:zones :footprints :claims :self :now :cell]) :action action))]
          (if (:ok v) {:ok true} v))))))

(defn judge
  "What a dig job does with verdict v given the hazards it accepts: :ok (dig), :refused (a zone or another plan's
  footprint or a claim: leave the cell for good), :hazard (a hazard not accepted), :no-zones (no zone list: dig nothing now) or
  :not-loaded."
  [v accept]
  (cond
    (rules/accepts? v (set accept)) :ok
    (:ok v) :hazard
    (#{:zone :claim :footprint} (:reason v)) :refused
    :else (:reason v)))

(defn refusal-fields
  "Event fields naming what refused the verdicts: {:zones [name ..] :claims [id ..] :plans [id ..]}."
  [verdicts]
  {:zones (vec (distinct (keep :zone verdicts)))
   :claims (vec (distinct (keep :claim verdicts)))
   :plans (vec (distinct (keep :plan verdicts)))})

(defn refusal-text [{:keys [zones claims plans]}]
  (str/join "; " (remove nil? [(when (seq zones) (str "zone " (str/join ", " zones)))
                               (when (seq claims) (str "claim " (str/join ", " claims)))
                               (when (seq plans) (str "plan " (str/join ", " plans)))])))

(defn decline!
  "Warn kind once for this job (per reason: :no-zones or :refused, the latter with refusal fields) and answer false,
  for a check that declines."
  [c kind job-name {:keys [reason] :as fields}]
  (ctx/warn-once! c [:access reason] kind
                  (assoc fields :text (if (= :no-zones reason)
                                        (str job-name " declined: no zone list has been read (zones.edn missing or never valid)")
                                        (str job-name " declined: every target is refused by " (refusal-text fields)))))
  false)
