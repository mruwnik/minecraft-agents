(ns jobs.movement.go-to.own-way
  "go-to's walk up a body's own stairs when its goal lies far above it under ground, the dive floor that bounds its plans, and
  the detour bound on a whole plan."
  (:require [engine.ctx :as ctx]
            [jobs.access.stair.memory :as stair-mem]
            [jobs.lib.near :as near]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.climb :as climb]
            [jobs.movement.go-to.column :as column]
            [jobs.movement.go-to.detour :as detour]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.movement.go-to.escalation :as esc]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.tuning :refer [climb-rise climb-dip max-searching]]))

(defn dive-floor!
  "The lowest y this call's searches may reach, or nil: the body is under ground, pos lies ::climb-rise or more above it
  and the y-margin setting is unset. It is ::climb-dip below where the call first found the body so (mem :dive-floor), so the
  legs of one call do not step down floor by floor."
  [c pos]
  (let [y (js/Math.floor (:y (u/self-pos c)))]
    (when (and (nil? (wworld/y-margin)) (:y pos) (>= (- (:y pos) y) (climb-rise)) (sh-sense/buried? (:primitives c)))
      (or (:dive-floor (ctx/mem c))
          (let [floor (- y (climb-dip))]
            (ctx/update-mem! c assoc :dive-floor floor)
            floor)))))

(defn dive-end
  "A fruitless round's result as go-to gives it up under a dive floor: a search the floor cut short (:box) is :way-dives."
  [c pos result]
  (if (and (:dive-floor c) (= :box (some-> (:reason result) keyword)))
    (assoc result :reason :way-dives :dip (climb-dip) :rise (js/Math.round (- (:y pos) (:y (u/self-pos c)))))
    result))

(defn seen-blocked?
  "Whether the body has seen a solid in the feet or head of the stand cell [x y z]."
  [c [x y z]]
  (let [kind-at (column/seen-kind-at (:primitives c))]
    (boolean (some #(keyword-identical? :solid (kind-at x % z)) [y (inc y)]))))

(defn own-way
  "Under a dive floor, or once on them (memory :own-way-on), the next hop (climb/hop) of the walk up the body's own ways
  to pos's height (climb/own-route), unless the setting is off or this call is done with them (memory :own-way-done);
  nil when none. Before the first hop of a call the walk is weighed against a new stair (climb/weigh, info
  go-to.own-way): a slower walk is not taken (memory :own-way-done)."
  [c pos floor]
  (let [m (ctx/mem c)
        feet (end/feet-cell c)
        top (js/Math.floor (:y pos))
        route (when (and (or floor (:own-way-on m)) (climb/own-way) (not (:own-way-done m))) (climb/own-route c feet top))
        weighed (when (and route (next route) (not (:own-way-on m))) (climb/weigh route (- top (second feet)) choose/stair-step-s))]
    (when weighed (ctx/emit! c :go-to.own-way :info weighed))
    (if (false? (:take weighed))
      (do (ctx/update-mem! c assoc :own-way-done true) nil)
      (some-> route (climb/hop (u/self-pos c) (climb/own-way-reach) #(seen-blocked? c %))))))

(defn ^:async walk-own-way!
  "Walk onto way (own-way), planning nowhere below the feet and the way's own cells (:floor): a way round through the
  caves is not it; only way's own cell is there (under a floor that is gone is not). A walk that does not get there
  nor a block nearer, or max-searching searches begun afresh (memory :searching), cuts the stair again from where the
  body stands on it (esc/own-recut!), else ends the call's use of its own stairs (memory :own-way-done). :again."
  [c way opts]
  (let [level (assoc c :dive-floor (min (:floor way) (js/Math.floor (:y (u/self-pos c)))))
        cell (dissoc way :floor)
        _ (stair-mem/use-way! c [(:x way) (:y way) (:z way)])
        {:keys [status result from to]} (await (near/walk-round! level cell 0 (dissoc opts :progress)))
        n (cond-> (:searching (ctx/mem c) 0) (and (= "searching" status) (:fresh result)) inc)
        nearer? (< (u/dist to cell) (dec (u/dist from cell)))]
    (ctx/update-mem! c assoc :own-way-on true :searching n)
    (when-not (or (= "arrived" status) (and (= "searching" status) (< n max-searching)) nearer? (esc/own-recut! c))
      (ctx/update-mem! c assoc :own-way-done true))
    :again))

(defn own-way-left!
  "Once the body has climbed its own stairs (memory :own-way-on) and none leads on (way nil): done with them for this call,
  the dive floor counts from here, and where it got to is kept (memory :own-way-top)."
  [c way]
  (when (and (nil? way) (:own-way-on (ctx/mem c)))
    (ctx/update-mem! c #(-> % (dissoc :own-way-on :dive-floor) (assoc :own-way-done true :own-way-top (second (end/feet-cell c)))))))

(defn plan-check
  "near/walk-round!'s :stair-check of a walk round: stair-check's why (may be nil: none), else the detour bound's
  (detour/refusal, a map) for a whole plan to pos, straight blocks from it at the call's start, of a body that senses
  and stands under ground, unless pos lies ::climb-rise or more above it (the climb's)."
  [c pos stair-check straight]
  (let [p (:primitives c)
        kind-at (column/seen-kind-at p)
        unseen? #(keyword-identical? :unknown (kind-at %1 %2 %3))
        bound? (and (some? (.-sensedAt p)) (< (- (:y pos) (:y (u/self-pos c))) (climb-rise)))]
    (fn [{:keys [status steps] :as plan} first?]
      (or (when stair-check (stair-check plan first?))
          (when (and bound? (= "found" status) (sh-sense/buried? p)) (detour/refusal steps straight unseen?))))))

(defn straight-from!
  "Blocks from the goal the call's first walk round found the body, d then (memory :detour-from)."
  [c d]
  (or (:detour-from (ctx/mem c))
      (do (ctx/update-mem! c assoc :detour-from d) d)))
