(ns jobs.movement.leave-vehicle
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.jobs.vehicle :as vehicle]))

(def doc
  "Get off whatever the body rides (boat, raft, minecart, mount). Ends at once when on foot.
  Faces :toward when given, else the nearest dry cell within :radius of the vehicle
  (feet and head air over a solid, non-fluid block), else leaves without a look. The server picks the exit from that look.
  Then it uses the dismount primitive (sneak).
  On success: info vehicle.left {:pos :landed}, :landed being :dry or :in-water. The result is the same map.
  A failed dismount is retried next round. After :max-tries it warns vehicle.dismount_failed {:tries :status} and ends,
  with the body still aboard. The :mounted trigger runs it.")

(def args
  {:toward {:doc "a position {:x :y :z} to face when getting off (its cell's centre); nil picks a dry cell" :default nil}
   :max-tries {:doc "dismounts tried before giving up" :default 2}
   :radius {:doc "dry cells this many blocks (horizontally) from the vehicle are faced" :default 2}})

(defn check [c] (vehicle/mounted? (:primitives c)))

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
        v (vehicle/vehicle-of (.self p))]
    (if-not v
      :done
      (let [yaw (yaw-for c v)
            r (await (ctx/act c :dismount (clj->js (if (some? yaw) {:yaw yaw} {}))))]
        (if (= "dismounted" (.-status r))
          (let [self (.self p)
                out {:pos (u/pos-of (.-pos r)) :landed (if (.-inWater self) :in-water :dry)}]
            (ctx/emit! c :vehicle.left :info out)
            (ctx/result! c out)
            :done)
          (let [tries (inc (:tries (ctx/mem c) 0))]
            (ctx/update-mem! c assoc :tries tries)
            (if (< tries (:max-tries (:args c) 2))
              :continue
              (do (ctx/emit! c :vehicle.dismount_failed :warn {:tries tries :status (.-status r)})
                  :done))))))))
