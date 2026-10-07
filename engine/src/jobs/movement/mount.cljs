(ns jobs.movement.mount
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.movement.vehicle :as vehicle]))

(def doc
  "Get on the boat, raft, minecart or rideable mob :id, or the nearest one in sight named :name (e.g. oak_boat, pig). One call is the whole attempt: find the entity in sight, walk
  into reach (jobs.movement.go-to, range 2, no escalation), mount (a mob needs an empty hand, a boat or minecart does not), wait for the server to seat the body.
  Holds the vehicle (jobs.movement.vehicle) while the job lives, so the :mounted trigger does not step the body off:
  the body stays aboard only in manual mode, otherwise the hold ends with the job and the trigger steps it off ~30 s later.
  By :name it prefers a vehicle with no rider.
  The check always passes. It yields :continue only when the walk waits on the world.

  Ends {:status :done :id :vehicle name} (plus :already true when aboard it already), info vehicle.mounted, or
  {:status :stopped :reason r}: :bad-args, :gone (no such entity in sight), :not-mountable, :occupied, :hand-full
  (a mob, with an item in hand that cannot be put away), :timeout (the server never seated the body), :unreachable (the walk failed twice),
  :aboard-other (already on another vehicle), :failed (any other status, with :mount the primitive's).")

(def args
  {:id {:doc "the entity id of the vehicle or mount (observe entities lists them)" :default nil}
   :name {:doc "instead of :id: the entity name to mount, the nearest in sight" :default nil}})

(def reach 2.5)
(def max-walks 2)

(defn check [_c] true)

(defn entity-of [p id]
  (first (filter #(= id (.-id %)) (array-seq (.entities p #js {:radius 64 :max 64})))))

(defn nearest-named
  "The id of the nearest sensed entity named name, a free one before an occupied one, or nil."
  [c name]
  (let [p (:primitives c) me (u/self-pos c)]
    (some->> (array-seq (.entities p #js {:radius 64 :max 64}))
             (filter #(= name (.-name %)))
             (sort-by (juxt #(pos? (count (.-passengers %))) #(u/dist me (u/pos-of (.-pos %)))))
             first .-id)))

(defn stop! [c id reason & [more]]
  (ctx/emit! c :vehicle.mount_refused :warn (merge {:id id :reason reason :text (str "cannot mount entity " id ": " (name reason))} more))
  (ctx/result! c (merge {:status :stopped :id id :reason reason} more))
  :done)

(defn done! [c result]
  (ctx/emit! c :vehicle.mounted :info (assoc result :text (str "aboard " (:vehicle result))))
  (ctx/result! c (assoc result :status :done))
  :done)

(defn ^:async round [c]
  (let [{:keys [name] :as a} (:args c)
        id (if (and (nil? (:id a)) (string? name)) (nearest-named c name) (:id a))
        p (:primitives c)]
    (cond
      (and (nil? (:id a)) (string? name) (nil? id)) (stop! c nil :gone)
      (not (and (integer? id) (pos? id))) (stop! c id :bad-args)
      (some? (vehicle/vehicle-of (.self p)))
      (let [v (vehicle/vehicle-of (.self p))]
        (if (= id (:id v))
          (do (vehicle/hold! c) (done! c {:id id :vehicle (:name v) :already true}))
          (stop! c id :aboard-other {:vehicle (:name v)})))
      :else
      (loop [fails 0]
        (let [e (entity-of p id)]
          (cond
            (nil? e) (stop! c id :gone)
            (> (u/dist (u/self-pos c) (u/pos-of (.-pos e))) reach)
            (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos (u/pos-of (.-pos e)) :range 2 :escalate false}))
                  res (ctx/child-result c :walk)]
              (cond
                (= :continue r) :continue
                (and (= :done r) (:arrived res)) (recur fails)
                (>= (inc fails) max-walks) (stop! c id :unreachable {:why (:reason res)})
                :else (recur (inc fails))))
            :else
            (do (vehicle/hold! c)
                (let [r (await (ctx/act c :mount #js {:id id}))
                      status (.-status r)]
                  (case status
                    "mounted" (done! c {:id id :vehicle (.-name (.-vehicle r))})
                    "already-mounted" (done! c {:id id :vehicle (.-name (.-vehicle r)) :already true})
                    "gone" (stop! c id :gone)
                    "not-mountable" (stop! c id :not-mountable)
                    "occupied" (stop! c id :occupied)
                    "hand-full" (stop! c id :hand-full)
                    "timeout" (stop! c id :timeout)
                    "out-of-reach" (if (>= (inc fails) max-walks) (stop! c id :unreachable) (recur (inc fails)))
                    (stop! c id :failed {:mount status}))))))))))
