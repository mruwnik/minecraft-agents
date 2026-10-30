(ns jobs.lib.vehicle
  "Vehicles from the job side: what the body rides (self().vehicle, see engine/js/vehicle.mjs) and where to step off."
  (:require [jobs.lib.util :as u]
            [jobs.lib.land :as land]
            [shadow.resource :as rc]))

(def ^:private seat-table (js/JSON.parse (rc/inline "jobs/lib/vehicle-seats.json")))

(defn vehicle-of
  "{:id :uuid :name} of what a sensed self (JS, from primitives.self()) rides, or nil (on foot or offline)."
  [self]
  (when-let [v (.-vehicle self)]
    {:id (.-id v) :uuid (.-uuid v) :name (.-name v)}))

(defn mounted? [p] (some? (vehicle-of (.self p))))

(defn seats-of
  "Riders the entity named name takes; 0 when the body cannot ride it (js/vehicle.mjs seatsOf, the same constants)."
  [name]
  (cond
    (re-find #"_chest_(boat|raft)$" name) (unchecked-get seat-table "chestBoatSeats")
    (re-find #"_(boat|raft)$" name) (unchecked-get seat-table "boatSeats")
    :else (or (unchecked-get (unchecked-get seat-table "mountSeats") name) 0)))

(defn yaw-toward
  "Minecraft yaw in degrees (0 south +z, 90 west -x, 180 north, 270 east) from from to to, by x and z."
  [from to]
  (let [deg (/ (* 180 (js/Math.atan2 (- (- (:x to) (:x from))) (- (:z to) (:z from)))) js/Math.PI)]
    (mod (+ deg 360) 360)))

(defn dry-cell?
  "A feet cell to step off onto: feet and head air, a block below that is not air or a hazard, as the body sees them.
  Unloaded or unseen is not."
  [p cell]
  (land/land-cell? #(u/seen-name p %) cell))

(defn centre [{:keys [x y z]}] {:x (+ x 0.5) :y y :z (+ z 0.5)})

(defn nearest-dry-cell
  "The dry cell nearest pos, within radius horizontally and one block up or down, not in pos's own column; or nil."
  [p pos radius]
  (let [fx (js/Math.floor (:x pos)) fy (js/Math.floor (:y pos)) fz (js/Math.floor (:z pos))]
    (->> (for [dx (range (- radius) (inc radius)) dz (range (- radius) (inc radius)) dy [0 1 -1]
               :when (and (not (and (zero? dx) (zero? dz)))
                          (<= (+ (* dx dx) (* dz dz)) (* radius radius)))]
           {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)})
         (filter #(dry-cell? p %))
         (sort-by #(u/dist pos (centre %)))
         first)))
