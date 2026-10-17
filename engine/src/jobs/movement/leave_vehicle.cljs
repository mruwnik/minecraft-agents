(ns jobs.movement.leave-vehicle
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.cond :as cond]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Get off whatever the body rides (boat, raft, minecart, mount). Ends at once when on foot.
  Faces :toward when given, else the nearest dry cell within ::dry-radius of the vehicle
  (feet and head air over a solid, non-fluid block), else leaves without a look. The server picks the exit from that look.
  Then it uses the dismount primitive (sneak).
  On success the call is done with data {:pos :landed}, :landed being :dry or :in-water.
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
   ::retry-wait-s {:default 0.1 :doc "Seconds waited after a failed dismount." :spec (a/num-in 0 nil)}
   ::dry-radius {:default 2 :doc "Dry cells this many blocks (horizontally) from the vehicle are faced." :spec (a/num-in 0 nil)}})

(def meta {:category :reflex})

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn done?
  "Done once the body is off."
  [c]
  (let [aboard (boolean (vehicle/mounted? (:primitives c)))]
    (if aboard
      (contract/later :aboard "still aboard" :units :vehicles :done 0 :left 1)
      (contract/ok :ashore "off the vehicle" :units :vehicles :done 1 :left 0))))

(defn quote-of [_c]
  {:uses {:job-time [0.1 0.5 (+ 1 (* (settings/get settings ::max-tries) (settings/get settings ::retry-wait-s)))]}})

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
        target (or (:toward (:args c)) (vehicle/nearest-dry-cell (:primitives c) from (settings/get settings ::dry-radius)))]
    (when target (vehicle/yaw-toward from (vehicle/centre target)))))

(defn landed-in-water?
  "Whether the landing cell is water. The physics flag keeps its value from the ride (a boat is boarded from the water), so it is read only for a cell not seen."
  [p pos flag]
  (let [n (u/seen-name p (look/cell-of pos))]
    (if n (= "water" n) (boolean flag))))

(defn ^:async round [c]
  (let [p (:primitives c)
        max-tries (settings/get settings ::max-tries)]
    (loop [tries 0]
      (let [v (vehicle/vehicle-of (.self p))]
        (if-not v
          (ctx/end c :done nil nil)
          (let [yaw (yaw-for c v)
                r (await (ctx/act c :dismount (clj->js (if (some? yaw) {:yaw yaw} {}))))]
            (if (= "dismounted" (.-status r))
              (let [self (.self p)
                    pos (u/pos-of (.-pos r))
                    out {:pos pos :landed (if (landed-in-water? p pos (.-inWater self)) :in-water :dry)}]
                (ctx/end c :done nil out))
              (let [tries (inc tries)]
                (if (< tries max-tries)
                  (let [r (await (child/pause! c (settings/get settings ::retry-wait-s) :dismount-retry))]
                    (if (= :continue r) :continue (recur tries)))
                  (ctx/end c :nothing-possible :dismount-failed {:tries tries :status (.-status r)}))))))))))
