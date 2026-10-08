(ns jobs.movement.boat-land
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.result :as result]
            [jobs.lib.sense :as sense]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Bring the boat the body is in to a shore and get off onto land.
  The shore is a land cell the body has seen (feet and head air over a solid block, no hazard) with seen water beside it at the level of that block:
  :pos names the land cell, else the nearest within :radius of the boat. jobs.movement.boat-drive steers to the water cell beside it
  (a spot the boat cannot reach is dropped for the next, up to :max-spots), then jobs.movement.leave-vehicle steps off facing the land cell.
  With :recover the boat is then taken back (jobs.movement.boat-launch :recover; it needs the boat in sight).
  A given :pos the body cannot make out after a look is driven nearer once (boat-drive to the water cell on the boat's side of it), then decided again. Ends {:status :done :land {:x :y :z} :boat id :recovered bool}, info boat.landed, or
  {:status :stopped :reason r}: :bad-args, :not-aboard (on foot: jobs.movement.boat-launch puts the body in a boat), :no-shore (none in sight, :pos is no shore, or it could not be made out even from near),
  :blocked (every spot tried could not be reached; with :cause), :drive-failed (the drive failed otherwise, with :cause), :not-landed (the dismount failed or left the body in water),
  :recover-failed (landed and boat.landed emitted, the boat not taken back; with :cause).

  Never :continue except when a child waits on the world.")

(a/defargs args
  {:pos {:doc "the land cell to step onto, [x y z] or {:x :y :z}; nil: the nearest shore in sight" :spec ::a/pos :default nil}
   :radius {:doc "how far from the boat to look for a shore" :spec (a/num-in 1 nil) :default 12}
   :max-spots {:doc "shore spots tried before giving up :blocked" :spec (a/int-in 1 nil) :default 3}
   :recover {:doc "take the boat back after landing" :spec boolean? :default false}})

(def landed-kind :boat-land)
(def landed-policy {:cap 4 :ttl (* 10 60 1000)})

(defn given-pos [c] (when (:pos (:args c)) (:pos (b/parse (:args c)))))

(defn check
  "Passes: the round looks at a given land cell not sensed yet, drives nearer to one it cannot make out, or stops with the reason."
  [_c] true)

(defn candidates [c]
  (let [p (:primitives c)
        {:keys [radius]} (:args c)]
    (if-let [cell (given-pos c)]
      (some-> (shore/spot-at p cell) vector)
      (shore/spots p (u/self-pos c) {:radius radius}))))

(def approach-range "A given land cell this near the boat is not driven nearer to." 3)

(defn approach-cell
  "The water-level cell beside land cell on the boat's side (along the axis the boat is farther off on)."
  [{:keys [x y z]} {bx :x bz :z}]
  (let [dx (- bx (+ x 0.5)) dz (- bz (+ z 0.5))]
    (if (>= (js/Math.abs dx) (js/Math.abs dz))
      {:x (+ x (if (pos? dx) 1 -1)) :y (dec y) :z z}
      {:x x :y (dec y) :z (+ z (if (pos? dz) 1 -1))})))

(defn unmade?
  "Whether the given land cell, or a cell its shore test read, is not made out (never seen, or not loaded)."
  [c reads]
  (boolean (when-let [cell (given-pos c)]
             (or (seq (:cells reads)) (nil? (u/seen-name (:primitives c) cell))))))

(defn ^:async approach!
  "Drive toward a given land cell the body could not make out: nil when not worth it (no :pos, made out, near, already
  driven nearer this run), else the drive child's outcome (:continue while it runs)."
  [c reads]
  (let [cell (given-pos c)
        pos (u/self-pos c)]
    (when (and (unmade? c reads) (not (:approached (ctx/mem c)))
               (> (u/dist pos (vehicle/centre cell)) approach-range))
      (let [r (await (ctx/call-child c :approach 'jobs.movement.boat-drive {:pos (approach-cell cell pos) :range 2}))]
        (if (= :continue r)
          :continue
          (do (ctx/update-mem! c assoc :approached true) :approached))))))

(defn no-shore-text [c reads]
  (cond
    (nil? (given-pos c)) "no seen shore to land on"
    (unmade? c reads) (let [{:keys [x y z]} (given-pos c)] (str "could not make out the land cell " x " " y " " z))
    :else "the given cell is no shore"))

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
          (ctx/emit! c :boat.landed :info {:land land :boat boat-id :recovered false :text "landed, the boat was not taken back"})
          (result/stop! c :recover-failed "landed, but the boat was not taken back" :land land :boat boat-id
                        :cause (when res (result/cause-of :recover res)))))))

(defn ^:async drive-to! [c water]
  (let [r (await (ctx/call-child c :drive 'jobs.movement.boat-drive {:pos water}))]
    (if (= :continue r) :continue {:r r :res (ctx/child-result c :drive)})))

(defn drive-failure
  "[reason failed-drive-result] for spots that all failed to drive to: :blocked only when every one was blocked
  (the last one is shown), else :drive-failed with the first other failure."
  [fails]
  (let [other (some #(when (not= :blocked (:reason %)) %) fails)]
    [(if other :drive-failed :blocked) (or other (peek fails))]))

(defn ^:async round [c]
  (let [p (:primitives c)
        {:keys [max-spots recover]} (:args c)
        landed (landed-boat c)]
    (cond
      (and landed (not (vehicle/mounted? p))) (await (recover! c (:land landed) (:boat landed)))
      (not (vehicle/mounted? p)) (result/stop! c :not-aboard "the body is not in a boat")
      (and (:pos (:args c)) (nil? (given-pos c))) (result/stop! c :bad-args (:error (b/parse (:args c))))
      :else
      (let [found (await (sense/decide! c :boat-land.shore #(candidates c) {:retry? empty?}))
            [_ reads] (sense/decide #(candidates c))
            approached (when (empty? found) (await (approach! c reads)))
            found (if (= :approached approached) (await (sense/decide! c :boat-land.shore #(candidates c) {:retry? empty?})) found)
            [_ reads] (if (= :approached approached) (sense/decide #(candidates c)) [nil reads])
            spots (take max-spots found)
            boat-id (:id (vehicle/vehicle-of (.self p)))]
        (cond
          (= :continue approached) :continue
          (empty? spots) (result/stop! c :no-shore (no-shore-text c reads))
          :else
          (loop [[{:keys [land water]} & more] spots fails []]
            (if (nil? land)
              (let [[reason shown] (drive-failure fails)]
                (result/stop! c reason
                              (str "could not steer to any of " (count spots) " shore spots")
                              :cause (some->> shown (result/cause-of :drive))))
              (let [d (await (drive-to! c water))]
                (if (= :continue d)
                  :continue
                  (let [{:keys [r res]} d]
                    (if (not= :done r)
                      (recur more (conj fails (or res {})))
                      (cond
                        (= :not-aboard (:reason res)) (result/stop! c :not-aboard "the body is not in a boat")
                        (= :stopped (:status res)) (recur more (conj fails res))
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
                            (if recover
                              (do (ctx/remember! c landed-kind {:job (:root c) :land land :boat boat-id} landed-policy)
                                  (await (recover! c land boat-id)))
                              (do (ctx/emit! c :boat.landed :info {:land land :boat boat-id :recovered false :text "got off the boat onto the shore"})
                                  (result/finish! c {:land land :boat boat-id :recovered false})))))))))))))))))
