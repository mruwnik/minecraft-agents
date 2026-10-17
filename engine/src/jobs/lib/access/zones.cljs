(ns jobs.lib.access.zones
  "Pure social verdict: may this body act on a cell, given the world's zones, the active claims and the footprints of
  other plans. Zones and claims are a rule jobs consult (jobs.lib.access); the engine never enforces them, and a
  job may opt out. No sensing, no side effects.

  Input, one map:
    :zones       nil (no zone list read) or [{:name :min :max :owner :allow #{..}} ...]; :allow is what OTHERS may do
                 in the zone, the owner may always act
    :claims      [{:id :owner :status :active :until ms :min :max}]; only :active ones not yet past :until count
    :footprints  {cell plan-id} of the plans other than the one the caller builds (the caller leaves its own out,
                 see jobs.lib.world/footprints :except)
    :shafts      the ids of the :mineshaft plans: their footprint refuses every action but :dig (a shaft is shared to dig
                 through; zones and claims still rule, and builders keep out of it)
    :plan-cells  the set of cells of the plan the caller builds (see jobs.lib.access/zone-input): with
                 plan-footprint-beats-zone? a cell in it is permitted over a foreign zone or claim, the plan is the
                 permission (another plan's footprint still refuses)
    :self        the body's name; owners are compared case-insensitively
    :now         ms clock, against a claim's :until
    :action      :dig :place :harvest :take :put
    :cell        [x y z]
  Output: {:ok true :why :own-zone|:own-claim|:open|:plan}, or {:ok false :reason ...} with detail:
  :no-zones, :footprint + :plan, :zone + :zone + :owner, :claim + :claim + :owner.

  Checks run in this order, the first that applies is the verdict:
  1. no zone list: refused
  2. another plan's footprint: refused (a :dig in a mineshaft's is not)
  3. a cell of the caller's own plan: ok :plan
  4. a foreign zone that does not allow the action: refused (on nesting the most restrictive wins)
  5. a foreign claim: refused (claims allow nothing)
  6. otherwise ok.")

;; Policy switches. Each is one line to change.

(def deposit-into-foreign-chest?
  "False: putting items into another's chest (:put) is refused like any act in a foreign zone or claim unless the
  zone's :allow has :put. True: :put is never refused for a zone or claim."
  false)

(def plan-footprint-beats-zone?
  "True: a plan's own builder may work over a foreign zone or claim: a cell in the verdict's :plan-cells is ok :plan
  right after the :no-zones and :footprint checks. False: the plan's cells are refused like any other."
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

(defn refusal
  "{:ok false :reason reason ...detail}, a detail without a value left out."
  [reason detail]
  (into {:ok false :reason reason} (remove (comp nil? val)) detail))

(defn verdict
  "See the namespace docstring."
  [{:keys [zones claims footprints shafts plan-cells self now action cell]}]
  (let [here (filter #(in-box? cell %) zones)
        foreign-zone (first (filter #(and (foreign-owner? (:owner %) self) (not (contains? (:allow %) action))) here))
        live (filter #(and (active? now %) (in-box? cell %)) claims)
        foreign-claim (first (filter #(not (same-owner? (:owner %) self)) live))
        pass? (and (= :put action) deposit-into-foreign-chest?)]
    (cond
      (nil? zones) {:ok false :reason :no-zones}
      (and (contains? footprints cell) (not (and (= :dig action) (contains? shafts (get footprints cell))))) (refusal :footprint {:plan (get footprints cell)})
      (and plan-footprint-beats-zone? (contains? plan-cells cell)) {:ok true :why :plan}
      (and foreign-zone (not pass?)) (refusal :zone {:zone (:name foreign-zone) :owner (:owner foreign-zone)})
      (and foreign-claim (not pass?)) (refusal :claim {:claim (:id foreign-claim) :owner (:owner foreign-claim)})
      (some #(same-owner? (:owner %) self) here) {:ok true :why :own-zone}
      (some #(same-owner? (:owner %) self) live) {:ok true :why :own-claim}
      :else {:ok true :why :open})))

(defn foreign-holder
  "Who else holds cell: {:kind :zone :name :owner} or {:kind :claim :id :owner} for the first zone (whatever its :allow) or
  active claim of another owner containing it, else nil. Same input keys as verdict (:zones :claims :self :now :cell);
  never refuses anything, it is a query for jobs, triggers and agents."
  [{:keys [zones claims self now cell]}]
  (or (some #(when (and (in-box? cell %) (foreign-owner? (:owner %) self))
               {:kind :zone :name (:name %) :owner (:owner %)})
            zones)
      (some #(when (and (active? now %) (in-box? cell %) (not (same-owner? (:owner %) self)))
               {:kind :claim :id (:id %) :owner (:owner %)})
            claims)))

(defn exit-goal
  "The cell outside zone zn's box by margin m + 2 on its nearest side, at the level of here."
  [{:keys [x y z]} zn m]
  (let [[x0 _ z0] (:min zn) [x1 _ z1] (:max zn)
        out (+ m 2)
        sides [[(- x x0) {:x (- x0 out) :z z}] [(- x1 x) {:x (+ x1 out) :z z}] [(- z z0) {:x x :z (- z0 out)}] [(- z1 z) {:x x :z (+ z1 out)}]]]
    (assoc (second (apply min-key first sides)) :y y)))
