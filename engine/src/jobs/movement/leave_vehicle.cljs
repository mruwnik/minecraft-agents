(ns jobs.movement.leave-vehicle
  (:require [engine.ctx :as ctx]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [jobs.movement.vehicle :as vehicle]))

(def doc
  "Get off whatever the body rides (boat, raft, minecart, mount). Ends at once when on foot.
  Faces :toward when given, else the nearest dry cell within :radius of the vehicle
  (feet and head air over a solid, non-fluid block), else leaves without a look. The server picks the exit from that look.
  Then it uses the dismount primitive (sneak).
  On success: info vehicle.left {:pos :landed}, :landed being :dry or :in-water. The result is the same map.
  A failed dismount is retried in the same run, the wait doubling from :wait-ms up to 1 s. After :max-tries it warns vehicle.dismount_failed {:tries :status}
  and ends stopped :dismount-failed, with the body still aboard. Never :continue. The :mounted trigger runs it (cooldown persistence: a body still aboard is tried again 30 s later).")

(def args
  {:toward {:doc "a position {:x :y :z} to face when getting off (its cell's centre); nil picks a dry cell" :default nil}
   :max-tries {:doc "dismounts tried before giving up" :default 8}
   :wait-ms {:doc "the wait after the first failed dismount; it doubles per try, at most 1000" :default 50}
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

(defn wait! [ms] (js/Promise. (fn [resolve] (js/setTimeout resolve ms))))

(defn ^:async round [c]
  (let [p (:primitives c)
        max-tries (:max-tries (:args c) 8)
        wait-ms (:wait-ms (:args c) 50)]
    (loop [tries 0]
      (let [v (vehicle/vehicle-of (.self p))]
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
              (let [tries (inc tries)]
                (if (< tries max-tries)
                  (do (await (wait! (min 1000 (* wait-ms (bit-shift-left 1 (dec tries)))))) (recur tries))
                  (do (ctx/emit! c :vehicle.dismount_failed :warn {:tries tries :status (.-status r)})
                      (result/stop! c :dismount-failed (str "could not get off the vehicle: " (.-status r)) :tries tries)))))))))))
