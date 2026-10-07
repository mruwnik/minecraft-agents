(ns jobs.movement.boat-land
  (:require [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.result :as result]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Bring the boat the body is in to a shore and get off onto land.
  The shore is a land cell the body has seen (feet and head air over a solid block, no hazard) with seen water beside it at the level of that block:
  :pos names the land cell, else the nearest within :radius of the boat. jobs.movement.boat-drive steers to the water cell beside it
  (a spot the boat cannot reach is dropped for the next, up to :max-spots), then jobs.movement.leave-vehicle steps off facing the land cell.
  With :recover the boat is then taken back (jobs.movement.boat-launch :recover; it needs the boat in sight).
  The check waits (:unseen) while a given :pos is not sensed. Ends {:status :done :land {:x :y :z} :boat id :recovered bool}, info boat.landed, or
  {:status :stopped :reason r}: :bad-args, :not-aboard (on foot: jobs.movement.boat-launch puts the body in a boat), :no-shore (none in sight, or :pos is no shore),
  :blocked (every spot tried could not be reached; with :cause), :drive-failed (the drive failed otherwise, with :cause), :not-landed (the dismount failed or left the body in water),
  :recover-failed (landed, the boat not taken back; with :cause).

  Never :continue except when a child waits on the world.")

(def args
  {:pos {:doc "the land cell to step onto, [x y z] or {:x :y :z}; nil: the nearest shore in sight" :type :pos :default nil}
   :radius {:doc "how far from the boat to look for a shore" :type :number :min 1 :default 12}
   :max-spots {:doc "shore spots tried before giving up :blocked" :type :int :min 1 :default 3}
   :recover {:doc "take the boat back after landing" :default false}})

(def landed-kind :boat-land)
(def landed-policy {:cap 4 :ttl (* 10 60 1000)})

(defn given-pos [c] (when (:pos (:args c)) (:pos (b/parse (:args c)))))

(defn check
  "Waits while a given land cell is not sensed."
  [c]
  (let [cell (given-pos c)]
    (or (nil? cell)
        (some? (u/seen-name (:primitives c) cell))
        (ctx/wait c {:reason :unseen :why "the land cell is not sensed yet"}))))

(defn candidates [c]
  (let [p (:primitives c)
        {:keys [radius]} (:args c)]
    (if-let [cell (given-pos c)]
      (some-> (shore/spot-at p cell) vector)
      (shore/spots p (u/self-pos c) {:radius radius}))))

(defn landed-boat
  "The boat id remembered by this job's landing, or nil."
  [c]
  (some #(when (= (:root c) (:job (:data %))) (:data %)) (ctx/entries c landed-kind)))

(defn ^:async recover! [c land boat-id]
  (let [r (await (ctx/call-child c :recover 'jobs.movement.boat-launch {:action :recover :id boat-id}))
        res (ctx/child-result c :recover)]
    (cond
      (= :continue r) :continue
      (and (= :done r) (not= :stopped (:status res)))
      (do (ctx/forget-where! c landed-kind #(= (:root c) (:job %)))
          (ctx/emit! c :boat.landed :info {:land land :boat boat-id :recovered true :text "landed and took the boat back"})
          (result/finish! c {:land land :boat boat-id :recovered true}))
      :else
      (do (ctx/forget-where! c landed-kind #(= (:root c) (:job %)))
          (result/stop! c :recover-failed "landed, but the boat was not taken back" :land land :boat boat-id
                        :cause (when res (result/cause-of :recover res)))))))

(defn ^:async drive-to! [c water]
  (let [r (await (ctx/call-child c :drive 'jobs.movement.boat-drive {:pos water}))]
    (if (= :continue r) :continue {:r r :res (ctx/child-result c :drive)})))

(defn ^:async round [c]
  (let [p (:primitives c)
        {:keys [max-spots recover]} (:args c)
        landed (landed-boat c)]
    (cond
      (and landed (not (vehicle/mounted? p))) (await (recover! c (:land landed) (:boat landed)))
      (not (vehicle/mounted? p)) (result/stop! c :not-aboard "the body is not in a boat")
      (and (:pos (:args c)) (nil? (given-pos c))) (result/stop! c :bad-args (:error (b/parse (:args c))))
      :else
      (let [spots (take max-spots (candidates c))
            boat-id (:id (vehicle/vehicle-of (.self p)))]
        (if (empty? spots)
          (result/stop! c :no-shore "no seen shore to land on")
          (loop [[{:keys [land water]} & more] spots last-fail nil]
            (if (nil? land)
              (result/stop! c (if (= :blocked (:reason last-fail)) :blocked :drive-failed)
                            (str "could not steer to any of " (count spots) " shore spots")
                            :cause (some->> last-fail (result/cause-of :drive)))
              (let [d (await (drive-to! c water))]
                (if (= :continue d)
                  :continue
                  (let [{:keys [r res]} d]
                    (if (not= :done r)
                      (recur more {:reason :drive-failed})
                      (cond
                        (= :not-aboard (:reason res)) (result/stop! c :not-aboard "the body is not in a boat")
                        (= :stopped (:status res)) (recur more res)
                        :else
                        (let [lv (await (ctx/call-child c :leave 'jobs.movement.leave-vehicle {:toward (vehicle/centre land)}))
                              lres (ctx/child-result c :leave)]
                          (cond
                            (= :continue lv) :continue
                            (or (not= :done lv) (= :stopped (:status lres)))
                            (result/stop! c :not-landed "could not get off the boat" :land land :cause (when lres (result/cause-of :leave lres)))
                            (= :in-water (:landed lres))
                            (result/stop! c :not-landed "the dismount left the body in water" :land land)
                            :else
                            (do (ctx/emit! c :boat.landed :info {:land land :boat boat-id :recovered false :text "got off the boat onto the shore"})
                                (if recover
                                  (do (ctx/remember! c landed-kind {:job (:root c) :land land :boat boat-id} landed-policy)
                                      (await (recover! c land boat-id)))
                                  (result/finish! c {:land land :boat boat-id :recovered false})))))))))))))))))
