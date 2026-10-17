(ns jobs.movement.boat-land
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
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
  A given :pos the body cannot make out after a look is driven nearer once (boat-drive to the water cell on the boat's side of it), then decided again.
  Impossible: :bad-args (:pos names no cell). Conditions (the not-aboard one before every round too): :not-aboard (on foot, with no landing of this call remembered: jobs.movement.boat-launch
  puts the body in a boat), :no-shore (no :pos given and no shore in sight; the engine looks round once first). A listed call waits on them.
  Ends done (data {:land {:x :y :z} :boat id :recovered bool}), info boat.landed, or
  condition-failed :no-shore (a given :pos is no shore, or could not be made out even from near), :not-aboard (got out on the way), or
  nothing-possible: :blocked (every spot tried could not be reached), :drive-failed (the drive failed otherwise), :not-landed (the dismount failed or left the body in water),
  :recover-failed (landed and boat.landed emitted, the boat not taken back). The failed child's ending is the :because.

  Never :continue except when a child waits on the world.")

(a/defargs args
  {:pos {:doc "the land cell to step onto, [x y z] or {:x :y :z}; nil: the nearest shore in sight" :spec ::a/pos :default nil}
   :radius {:doc "how far from the boat to look for a shore" :spec (a/num-in 1 nil) :default 12}
   :max-spots {:doc "shore spots tried before giving up :blocked" :spec (a/int-in 1 nil) :default 3}
   :recover {:doc "take the boat back after landing" :spec boolean? :default false}})

(def target [:pos])

(def landed-kind :boat-land)
(def landed-policy {:cap 4 :ttl :forever})

(defn given-pos [c] (when (:pos (:args c)) (:pos (b/parse (:args c)))))

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

(def bad-args
  "Never :bad-args when :pos names no cell."
  (checks/args-fit "no cell at :pos" #(not (and (:pos (:args %)) (nil? (given-pos %))))))

(defn not-aboard
  "Later :not-aboard while the body is on foot and has no landing of this job to finish."
  [c]
  (if (or (vehicle/mounted? (:primitives c)) (landed-boat c))
    (contract/ok :mounted "aboard")
    (contract/later :not-aboard "on foot" :hint "the body is not in a boat; board one first, e.g. boat-launch")))

(defn no-shore
  "Later :no-shore while no :pos is given and no shore is in sight; a look first while the body has not looked round."
  [c]
  (if (and (nil? (:pos (:args c))) (vehicle/mounted? (:primitives c)) (empty? (candidates c)))
    (contract/look :no-shore "no seen shore to land on" :hint "no seen shore to land on")
    (contract/ok :shore "a shore to land on")))

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}})

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c))))))
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(def can-start? (cond/and bad-args not-aboard no-shore))

(def can-run? not-aboard)

(defn done?
  "Never done? itself: off the boat the landing is still to be recorded (event, recovery), by the round."
  [c]
  (let [off (not (vehicle/mounted? (:primitives c)))]
    (contract/later :landing (if off "off the boat, the landing is to be recorded" "on the boat")
                    :units :landings :done 0 :left 1)))

(def drive-s "Seconds a landing takes, for the quote." 20)

(defn quote-of [c]
  {:uses {:job-time [2 drive-s (or (get-in (ctx/frozen-call c) [:spend :job-time]) ##Inf)]}})

(def beside-range "How near the middle of the water cell beside the land the boat must be: the dismount exit is picked from the boat's own cell." 0.6)

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
      (let [r (await (ctx/call c :approach 'jobs.movement.boat-drive {:pos (approach-cell cell pos) :range 2}))]
        (if (= :continue r)
          :continue
          (do (ctx/update-mem! c assoc :approached true) :approached))))))

(defn no-shore-text [c reads]
  (cond
    (nil? (given-pos c)) "no seen shore to land on"
    (unmade? c reads) (let [{:keys [x y z]} (given-pos c)] (str "could not make out the land cell " x " " y " " z))
    :else "the given cell is no shore"))

(defn ^:async recover! [c land boat-id]
  (let [e (await (ctx/call c :recover 'jobs.movement.boat-launch {:action :recover :id boat-id}))]
    (if (= :continue e)
      :continue
      (do (ctx/forget-where! c landed-kind #(= (:root c) (:job %)))
          (if (= :done (:kind e))
            (do (ctx/emit! c :boat.landed :info {:land land :boat boat-id :recovered true :text "landed and took the boat back"})
                (ctx/end c :done nil {:land land :boat boat-id :recovered true}))
            (do (ctx/emit! c :boat.landed :info {:land land :boat boat-id :recovered false :text "landed, the boat was not taken back"})
                (ctx/end c :nothing-possible :recover-failed {:land land :boat boat-id})))))))

(defn drive-failure
  "[reason failed-drive-ending] for spots that all failed to drive to: :blocked only when every one was blocked
  (the last one is shown), else :drive-failed with the first other failure."
  [fails]
  (let [other (some #(when (not= :blocked (contract/cause-key %)) %) fails)]
    [(if other :drive-failed :blocked) (or other (peek fails))]))

(defn ^:async dismount!
  "Step off toward the land cell and finish: record the landing and recover the boat, or end."
  [c land boat-id]
  (let [lv (await (ctx/call c :leave 'jobs.movement.leave-vehicle {:toward (vehicle/centre land)}))]
    (cond
      (= :continue lv) :continue
      (not= :done (:kind lv)) (ctx/end c :nothing-possible :not-landed {:land land})
      (= :in-water (:landed (:data lv))) (ctx/end c :nothing-possible :not-landed {:land land :landed :in-water})
      (:recover (:args c))
      (do (ctx/remember! c landed-kind {:job (:root c) :land land :boat boat-id} landed-policy)
          (await (recover! c land boat-id)))
      :else
      (do (ctx/emit! c :boat.landed :info {:land land :boat boat-id :recovered false :text "got off the boat onto the shore"})
          (ctx/end c :done nil {:land land :boat boat-id :recovered false})))))

(defn ^:async try-spots!
  "Drive to each spot's water cell in turn and land on the first that works; ends :nothing-possible when none does."
  [c spots boat-id]
  (loop [[{:keys [land water]} & more] spots fails []]
    (if (nil? land)
      (let [[reason shown] (drive-failure fails)]
        (ctx/end c :nothing-possible reason {:spots (count spots) :drive (some-> shown contract/cause-key)}))
      (let [d (await (ctx/call c :drive 'jobs.movement.boat-drive {:pos water :range beside-range}))]
        (cond
          (= :continue d) :continue
          (= :not-aboard (contract/cause-key d)) (ctx/end c :condition-failed :not-aboard)
          (not= :done (:kind d)) (recur more (conj fails d))
          :else (await (dismount! c land boat-id)))))))

(defn ^:async round [c]
  (let [p (:primitives c)
        landed (landed-boat c)]
    (if (and landed (not (vehicle/mounted? p)))
      (await (recover! c (:land landed) (:boat landed)))
      (let [found (await (sense/decide! c :boat-land.shore #(candidates c) {:retry? empty?}))
            [_ reads] (sense/decide #(candidates c))
            approached (when (empty? found) (await (approach! c reads)))
            found (if (= :approached approached) (await (sense/decide! c :boat-land.shore #(candidates c) {:retry? empty?})) found)
            [_ reads] (if (= :approached approached) (sense/decide #(candidates c)) [nil reads])
            spots (take (:max-spots (:args c)) found)]
        (cond
          (= :continue approached) :continue
          (empty? spots) (ctx/end c :condition-failed {:reason :no-shore :hint (no-shore-text c reads)})
          :else (await (try-spots! c spots (:id (vehicle/vehicle-of (.self p))))))))))
