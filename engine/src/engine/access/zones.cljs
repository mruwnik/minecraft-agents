(ns engine.access.zones
  "Pure social verdict: may this body act on a cell, given the world's zones, the active claims and the footprints of
  other plans. Zones and claims are a rule jobs consult (engine.jobs.access); the engine never enforces them, and a
  job may opt out. No sensing, no side effects.

  Input, one map:
    :zones       nil (no zone list read) or [{:name :min :max :owner :allow #{..}} ...]; :allow is what OTHERS may do
                 in the zone, the owner may always act
    :claims      [{:id :owner :status :active :until ms :min :max}]; only :active ones not yet past :until count
    :footprints  {cell plan-id} of the plans other than the one the caller builds (the caller leaves its own out,
                 see engine.ctx/footprints :except)
    :self        the body's name; owners are compared case-insensitively
    :now         ms clock, against a claim's :until
    :action      :dig :place :harvest :take :put
    :cell        [x y z]
  Output: {:ok true :why :own-zone|:own-claim|:open} or {:ok false :reason :no-zones|:footprint|:zone|:claim
  ...detail}: :footprint + :plan, :zone + :zone + :owner, :claim + :claim + :owner.
  Order: no zones, another plan's footprint, a foreign zone not allowing the action (the most restrictive wins on
  nesting), a foreign claim (claims allow nothing), else ok.")

;; The three owner decisions still open: each is one line to change.

(def deposit-into-foreign-chest?
  "False: putting items into another's chest (:put) is refused like any act in a foreign zone or claim unless the
  zone's :allow has :put. True: :put is never refused for a zone or claim."
  false)

(def plan-footprint-beats-zone?
  "True: a plan's own builder may work over a foreign zone. This is what the caller's footprints :except already does
  (it drops the job's own plan before asking); this flag documents the rule."
  true)

(def unknown-owner-foreign?
  "True: a zone owned by \"unknown\" counts as foreign to every body, like any other owner."
  true)

(defn in-box? [[x y z] {[x0 y0 z0] :min [x1 y1 z1] :max}]
  (and (<= x0 x x1) (<= y0 y y1) (<= z0 z z1)))

(defn same-owner? [a b]
  (and (string? a) (string? b) (= (.toLowerCase a) (.toLowerCase b))))

(defn foreign-owner? [owner self]
  (cond
    (and (= "unknown" owner) (not unknown-owner-foreign?)) false
    :else (not (same-owner? owner self))))

(defn active? [now {:keys [status until]}]
  (and (= "active" (some-> status name)) (> until now)))

(defn verdict
  "See the namespace docstring."
  [{:keys [zones claims footprints self now action cell]}]
  (let [here (filter #(in-box? cell %) zones)
        foreign-zone (first (filter #(and (foreign-owner? (:owner %) self) (not (contains? (:allow %) action))) here))
        live (filter #(and (active? now %) (in-box? cell %)) claims)
        foreign-claim (first (filter #(not (same-owner? (:owner %) self)) live))
        pass? (and (= :put action) deposit-into-foreign-chest?)]
    (cond
      (nil? zones) {:ok false :reason :no-zones}
      (contains? footprints cell) {:ok false :reason :footprint :plan (get footprints cell)}
      (and foreign-zone (not pass?)) {:ok false :reason :zone :zone (:name foreign-zone) :owner (:owner foreign-zone)}
      (and foreign-claim (not pass?)) {:ok false :reason :claim :claim (:id foreign-claim) :owner (:owner foreign-claim)}
      (some #(same-owner? (:owner %) self) here) {:ok true :why :own-zone}
      (some #(same-owner? (:owner %) self) live) {:ok true :why :own-claim}
      :else {:ok true :why :open})))
