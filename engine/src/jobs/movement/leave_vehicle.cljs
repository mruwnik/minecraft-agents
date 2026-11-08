(ns jobs.movement.leave-vehicle
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.cond :as cond]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Get off whatever the body rides (boat, raft, minecart, mount). Ends at once when on foot.
  Faces :toward when given, else the nearest seen dry cell within swim-reach of the vehicle
  (feet and head air over a solid, non-fluid block), else leaves without a look. The server picks the exit from that look.
  Then it uses the dismount primitive (sneak).
  A landing in water is not the end: the body walks (jobs.movement.go-to, swimming) to the nearest seen dry cell within swim-reach of the landing.
  On success the call is done with data {:pos :landed}, :landed being :dry, or :in-water when no dry cell is seen within swim-reach.
  A walk that does not arrive ends nothing-possible :not-landed (the go-to ending as :because).
  A failed dismount is retried in the same run after ::retry-wait-s. After ::max-tries it ends nothing-possible :dismount-failed
  (data :tries :status), with the body still aboard. On foot at the start the call is refused (condition :not-mounted). Never :continue. The :mounted trigger runs it (cooldown persistence: a body still aboard is tried again 30 s later).")

(a/defargs args
  {:toward {:doc "a position {:x :y :z} to face when getting off (its cell's centre); nil picks a dry cell" :spec ::a/pos :default nil}})

(def not-mounted
  "Later :not-mounted when the body is on foot."
  (cond/because :not-mounted (cond/mounted)))

(def can-start? not-mounted)

(a/defargs settings
  {::job-time-s {:default 30 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-tries {:default 8 :doc "Dismounts tried before giving up." :spec (a/int-in 1 nil)}
   ::retry-wait-s {:default 0.1 :doc "Seconds waited after a failed dismount." :spec (a/num-in 0 nil)}})

(def meta {:category :reflex})

(def safe? ready/threatened)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn done?
  "Done once the body is off and, after a water landing, on its dry cell."
  [c]
  (cond
    (vehicle/mounted? (:primitives c)) (contract/later :aboard "still aboard" :units :vehicles :done 0 :left 1)
    (:ashore (ctx/mem c)) (contract/later :swimming "off the vehicle, in water" :units :vehicles :done 0 :left 1)
    :else (contract/ok :ashore "off the vehicle" :units :vehicles :done 1 :left 0)))

(defn quote-of [_c]
  {:uses {:job-time [0.1 0.5 (+ 1 (* (settings/get settings ::max-tries) (settings/get settings ::retry-wait-s)))]}})

(defn swim-reach
  "Blocks the body swims in the call's ::job-time-s (cost.planner/swim-speed of what it wears): no dry cell farther is reached within it."
  [c]
  (* (settings/get settings ::job-time-s) (cost-planner/swim-speed (.-equipment (.self (:primitives c))))))

(defn vehicle-pos
  "Where the ridden entity is, else the body."
  [c v]
  (let [p (:primitives c)
        e (some #(when (= (:id v) (.-id %)) %) (array-seq (.entities p #js {:radius 8})))]
    (if e (u/pos-of (.-pos e)) (u/self-pos c))))

(defn yaw-for
  "The yaw to face when getting off, or nil."
  [c v]
  (let [from (vehicle-pos c v)
        target (or (:toward (:args c)) (vehicle/nearest-dry-cell (:primitives c) from (js/Math.floor (swim-reach c))))]
    (when target (vehicle/yaw-toward from (vehicle/centre target)))))

(defn landed-in-water?
  "Whether the landing is in water: water at the feet, or over water (the body is put down at the boat's height and drops in). The physics flag keeps its value from the ride (a boat is boarded from the water), so it is read only when neither cell is seen."
  [p pos flag]
  (let [cell (look/cell-of pos)
        names (keep #(u/seen-name p %) [cell (update cell :y dec)])]
    (if (seq names) (boolean (some #{"water"} names)) (boolean flag))))

(defn ^:async walk-ashore
  "The swim from a water landing to the nearest seen dry cell within swim-reach of it (a go-to child, range 0); done
  :in-water with none. :continue while it goes on, else the ending."
  [c landing]
  (let [p (:primitives c)
        dry (vehicle/nearest-dry-cell p landing (js/Math.floor (swim-reach c)))]
    (if-not dry
      (do (ctx/update-mem! c dissoc :ashore)
          (ctx/end c :done nil {:pos landing :landed :in-water}))
      (let [r (await (child/call-walk* c :ashore {:pos dry :range 0 :ways #{:walk :clear-seal}}))]
        (if (= :continue r)
          :continue
          (do (ctx/update-mem! c dissoc :ashore)
              (cond
                (not= :done (:kind r)) (ctx/end c :nothing-possible :not-landed {:cell landing :walk (:cause r)})
                (landed-in-water? p (u/self-pos c) (.-inWater (.self p))) (ctx/end c :done nil {:pos landing :landed :in-water})
                :else (ctx/end c :done nil {:pos (u/self-pos c) :landed :dry}))))))))

(defn ^:async step
  "One dismount try; :again after a failed one (the tries are kept in job memory), else the ending."
  [c]
  (let [p (:primitives c)
        max-tries (settings/get settings ::max-tries)
        v (vehicle/vehicle-of (.self p))]
    (cond
      (:ashore (ctx/mem c)) (await (walk-ashore c (:ashore (ctx/mem c))))
      (not v) (ctx/end c :done nil nil)
      :else
      (let [yaw (yaw-for c v)
            r (await (ctx/act c :dismount (clj->js (if (some? yaw) {:yaw yaw} {}))))]
        (if (= "dismounted" (.-status r))
          (let [self (.self p)
                pos (u/pos-of (.-pos r))
                wet? (landed-in-water? p pos (.-inWater self))]
            (if wet?
              (do (ctx/update-mem! c assoc :ashore pos) :again)
              (ctx/end c :done nil {:pos pos :landed :dry})))
          (let [tries (inc (or (:tries (ctx/mem c)) 0))]
            (ctx/update-mem! c assoc :tries tries)
            (if (< tries max-tries)
              (if (= :continue (await (child/pause! c (settings/get settings ::retry-wait-s) :dismount-retry))) :continue :again)
              (ctx/end c :nothing-possible :dismount-failed {:tries tries :status (.-status r)}))))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step c))))

(defn worth-of
  "The loss getting off averts: a body left riding (cost/riding-s)."
  [_c]
  cost/riding-s)
