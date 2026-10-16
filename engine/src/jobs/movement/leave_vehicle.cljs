(ns jobs.movement.leave-vehicle
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Get off whatever the body rides (boat, raft, minecart, mount). Ends at once when on foot.
  Faces :toward when given, else the nearest dry cell within :radius of the vehicle
  (feet and head air over a solid, non-fluid block), else leaves without a look. The server picks the exit from that look.
  Then it uses the dismount primitive (sneak).
  On success: info vehicle.left {:pos :landed}, :landed being :dry or :in-water, and the call is done with that map as data.
  A failed dismount is retried in the same run, the wait doubling from :wait-ms up to 1 s. After :max-tries it warns vehicle.dismount_failed {:tries :status}
  and ends nothing-possible :dismount-failed (data :tries :status), with the body still aboard. On foot at the start the call is refused (condition :not-mounted). Never :continue. The :mounted trigger runs it (cooldown persistence: a body still aboard is tried again 30 s later).")

(a/defargs args
  {:toward {:doc "a position {:x :y :z} to face when getting off (its cell's centre); nil picks a dry cell" :spec ::a/pos :default nil}
   :max-tries {:doc "dismounts tried before giving up" :spec (a/int-in 1 nil) :default 8}
   :wait-ms {:doc "the wait after the first failed dismount; it doubles per try, at most 1000" :spec (a/num-in 0 nil) :default 50}
   :radius {:doc "dry cells this many blocks (horizontally) from the vehicle are faced" :spec (a/num-in 0 nil) :default 2}})

(def not-mounted
  "Later :not-mounted when the body is on foot."
  (cond/because :not-mounted (cond/mounted)))

(def can-start? not-mounted)

(a/defargs settings
  {::job-time-s {:default 30 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

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

(defn quote-of [c]
  (let [{:keys [max-tries wait-ms]} (:args c)]
    {:uses {:job-time [0.1 0.5 (+ 1 (* max-tries (min 1 (/ wait-ms 1000))))]}}))

(defn vehicle-pos
  "Where the ridden entity is, else the body."
  [c v]
  (let [p (:primitives c)
        e (some #(when (= (:id v) (.-id %)) %) (array-seq (.entities p #js {:radius 8})))]
    (if e (u/pos-of (.-pos e)) (u/self-pos c))))

(defn yaw-for
  "The yaw to face when getting off, or nil."
  [c v]
  (let [{:keys [toward radius]} (:args c)
        from (vehicle-pos c v)
        target (or toward (vehicle/nearest-dry-cell (:primitives c) from (or radius 2)))]
    (when target (vehicle/yaw-toward from (vehicle/centre target)))))

(defn ^:async round [c]
  (let [p (:primitives c)
        max-tries (:max-tries (:args c) 8)
        wait-ms (:wait-ms (:args c) 50)]
    (loop [tries 0]
      (let [v (vehicle/vehicle-of (.self p))]
        (if-not v
          (ctx/end c :done nil nil)
          (let [yaw (yaw-for c v)
                r (await (ctx/act c :dismount (clj->js (if (some? yaw) {:yaw yaw} {}))))]
            (if (= "dismounted" (.-status r))
              (let [self (.self p)
                    out {:pos (u/pos-of (.-pos r)) :landed (if (.-inWater self) :in-water :dry)}]
                (ctx/emit! c :vehicle.left :info out)
                (ctx/end c :done nil out))
              (let [tries (inc tries)]
                (if (< tries max-tries)
                  (let [r (await (child/pause! c (/ (min 1000 (* wait-ms (bit-shift-left 1 (dec tries)))) 1000) :dismount-retry))]
                    (if (= :continue r) :continue (recur tries)))
                  (do (ctx/emit! c :vehicle.dismount_failed :warn {:tries tries :status (.-status r)})
                      (ctx/end c :nothing-possible :dismount-failed {:tries tries :status (.-status r)})))))))))))
