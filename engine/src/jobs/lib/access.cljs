(ns jobs.lib.access
  "jobs.lib.access.rules and jobs.lib.access.zones as a job asks them. Blocks come from the body's primitives; zones,
  claims and plan footprints come from ctx, read once per round. Zones are a rule the jobs consult, never enforced
  by the engine. A job given :ignore-zones? acts regardless of zones, claims and footprints."
  (:require [clojure.string :as str]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.access.zones :as zones]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(defn cell [{:keys [x y z]}] [x y z])

(defn own-plans
  "The ids of the plans this body made (plan :metadata :by names it)."
  [c]
  (into #{} (keep (fn [[id by]] (when (zones/same-owner? by (ctx/self-name c)) id))) (known/plan-authors c)))

(defn zone-input
  "The social half of the rules' input: {:zones :footprints :plan-cells :claims :self :own-plans :now :ignore-zones?}.
  opts:
  :except is a plan id for a job working that plan. Its footprint is left out of :footprints and its cells are given
  as :plan-cells, because the plan is the permission (see jobs.lib.access.zones/verdict).
  :own-plans-ok? drops the footprints of plans this body made too (a body's own plan never blocks its own work).
  :ignore-zones? defaults to the job's own :ignore-zones? arg."
  ([c] (zone-input c {}))
  ([c {:keys [except own-plans-ok?] :as opts}]
   {:zones (known/zones c)
    :footprints (cond->> (known/footprints c {:except except})
                  own-plans-ok? (into {} (remove (comp (own-plans c) val))))
   :plan-cells (if (nil? except)
                 #{}
                 (into #{} (keep (fn [[cell id]] (when (= id except) cell))) (known/footprints c)))
    :claims (known/claims c)
    :self (ctx/self-name c)
    :own-plans (own-plans c)
    :now (ctx/now c)
    :ignore-zones? (boolean (if (contains? opts :ignore-zones?) (:ignore-zones? opts) (:ignore-zones? (:args c))))}))

(def hidden-guess "What a cell the body has not sensed is taken for: rock, so a dig goes ahead and looks." "stone")

(defn sensed-at
  "A block-at fn [x y z] -> name over what the body senses (jobs.lib.util/sensed): guess for a cell it has not sensed,
  nil when the cell is not loaded (the rules' :not-loaded)."
  [p guess]
  (fn [[x y z]] (when-let [b (u/sensed p {:x x :y y :z z})] (if (true? (.-unknown b)) guess (.-name b)))))

(defn seen-at
  "A block-at fn [x y z] -> name over what the body has seen, nil for a cell it has not seen or that is not loaded."
  [p]
  (fn [[x y z]] (when-let [b (u/sensed p {:x x :y y :z z})] (when-not (true? (.-unknown b)) (.-name b)))))

(defn rules-input
  "The rules' input without :cell: the blocks (as sensed), :floor-at (blocks as seen, no guess), the body's feet and
  zone-input. opts as zone-input."
  ([c] (rules-input c {}))
  ([c opts]
   (let [p (:primitives c)
         {:keys [x y z]} (u/self-pos c)]
     (merge {:block-at (sensed-at p hidden-guess)
             :floor-at (seen-at p)
             :feet [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]
             :ledger #{}}
            (zone-input c opts)))))

(defn may-dig?
  "The rules' dig verdict for pos ({:x :y :z}) over input in."
  [in pos]
  (rules/may-dig? (assoc in :cell (cell pos))))

(defn may?
  "Whether action (:dig :place :sow :harvest :take :put) at pos ({:x :y :z} or [x y z]) is permitted, over a ctx
  (read afresh) or a rules input map.
  :dig and :place give the rules' verdict, physics and hazards included. :sow is a place that ignores the body's own cell.
  :harvest, :take and :put ask zones, claims and footprints only.
  Returns {:ok true :hazards [..]} or {:ok false :reason kw ...detail}."
  [c-or-in action pos]
  (let [in (if (:primitives c-or-in) (rules-input c-or-in) c-or-in)
        in (assoc in :cell (if (vector? pos) pos (cell pos)))]
    (case action
      :dig (rules/may-dig? in)
      :place (rules/may-place? in)
      :sow (rules/may-place? (assoc in :feet nil))
      (if (:ignore-zones? in)
        {:ok true}
        (let [v (zones/verdict (assoc (select-keys in [:zones :footprints :plan-cells :claims :self :now :cell]) :action action))]
          (if (:ok v) {:ok true} v))))))

(defn judge
  "What a dig job does with verdict v given the hazards it accepts:
  :ok (dig), :refused (zone, claim or another plan's footprint: leave the cell for good), :hazard (a hazard not
  accepted), :no-zones (no zone list: dig nothing now), :not-loaded, or another reason from the rules."
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
  "For a check that declines. Warns kind once per reason (:no-zones or :refused) and waits via ctx/wait with the
  reason and fields, which the agent sees as why the job waits."
  [c kind job-name {:keys [reason] :as fields}]
  (ctx/warn-once! c [:access reason] kind
                  (assoc fields :text (if (= :no-zones reason)
                                        (str job-name " declined: no zone list has been read (zones.edn missing or never valid)")
                                        (str job-name " declined: every target is refused by " (refusal-text fields)))))
  (ctx/wait c fields))

(defn container-refusal
  "nil when action (:take or :put) at the container or furnace at pos is permitted, else the refusing verdict
  {:ok false :reason :zone|:claim ...}. Only zones and claims count. An unread zone list refuses nothing, and a plan's
  footprint does not make a chest anyone's. Honours the job's :ignore-zones? arg."
  [c action pos]
  (when pos
    (let [v (may? c action pos)]
      (when (contains? #{:zone :claim} (:reason v))
        v))))

(defn refused-result
  "The result map of a job that gave up on refusal verdict v: {:gave-up true :reason :refused :zones [..] :claims [..]}."
  [v]
  (assoc (select-keys (refusal-fields [v]) [:zones :claims]) :gave-up true :reason :refused))

;; ------------------------------------------------------------------ survival jobs: last resort

(def social-reasons #{:zone :claim :footprint})

(defn trespass-refusal
  "The verdict refusing action at pos for a social reason (another's zone, claim or plan footprint), else nil.
  Physics and hazards do not count, and a missing zone list refuses nothing. A footprint of a plan this body made
  (:own-plans of the input) is not another's. c-or-in as may?."
  [c-or-in action pos]
  (let [in (if (:primitives c-or-in) (rules-input c-or-in) c-or-in)
        v (may? in action pos)]
    (when (and (contains? social-reasons (:reason v))
               (not (and (= :footprint (:reason v)) (contains? (:own-plans in) (:plan v)))))
      v)))

(defn choose
  "Pick from options the first whose cells (cells-of option) are all permitted for action, else the first option as a
  last resort. Returns {:option o :trespass v}, v the first refusal of the chosen option (nil when permitted), or nil
  without options."
  [c action options cells-of]
  (when (seq options)
    (let [in (rules-input c)
          scored (map (fn [o] [o (some #(trespass-refusal in action %) (cells-of o))]) options)
          [o v] (or (first (filter (comp nil? second) scored)) (first scored))]
      {:option o :trespass v})))

(defn trespass!
  "Warn <job-name>.trespass-last-resort once, naming the zone or claim, for a survival job acting on a refused cell
  because no permitted option existed. A nil verdict does nothing."
  [c job-name v]
  (when v
    (let [fields (refusal-fields [v])]
      (ctx/warn-once! c [:trespass job-name] (keyword (str job-name ".trespass-last-resort"))
                      (assoc fields :text (str job-name " acted in another's area as a last resort: " (refusal-text fields)))))))
