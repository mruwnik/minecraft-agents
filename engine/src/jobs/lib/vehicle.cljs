(ns jobs.lib.vehicle
  "Vehicles from the job side: what the body rides (self().vehicle, see engine/js/vehicle.mjs), the vehicle hold, and
  where to step off.

  A job that means to ride (launch, drive, land, cross-water) calls hold! before it mounts and release! after it is
  off. The hold is body memory :vehicle-hold {:job root-id}. It counts only while that root job's memory
  (kind :job/<id>) exists, so a cancelled holder stops holding at once. The :mounted trigger fires on a body aboard
  that nothing holds."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.land :as land]))

(def hold-kind :vehicle-hold)

(a/defargs settings
  {::hold-cap {:default 8 :doc "Most vehicle-hold entries the body's memory keeps." :spec (a/int-in 1 nil)}
   ::hold-ttl-ms {:default (* 24 60 60 1000) :doc "How long a vehicle hold stays in memory, in ms." :spec (a/int-in 1 nil)}})

(defn hold-policy [] {:cap (settings/get settings ::hold-cap) :ttl (settings/get settings ::hold-ttl-ms)})

(defn vehicle-of
  "{:id :uuid :name} of what a sensed self (JS, from primitives.self()) rides, or nil (on foot or offline)."
  [self]
  (when-let [v (.-vehicle self)]
    {:id (.-id v) :uuid (.-uuid v) :name (.-name v)}))

(defn mounted? [p] (some? (vehicle-of (.self p))))

(defn live-job? [view id] (boolean (seq (mem/entries view (mem/job-kind id)))))

(defn held?
  "True when some :vehicle-hold entry belongs to a job that still lives."
  [view]
  (boolean (some #(live-job? view (:job (:data %))) (mem/entries view hold-kind))))

(defn held-by-root?
  "True when this ctx's root job holds the vehicle."
  [c]
  (boolean (some #(= (:root c) (:job (:data %))) (ctx/entries c hold-kind))))

(defn hold!
  "Mark the vehicle as this ctx's root job's to ride; once per job."
  [c]
  (when-not (held-by-root? c)
    (ctx/remember! c hold-kind {:job (:root c)} (hold-policy))))

(defn release!
  "Drop this ctx's root job's hold."
  [c]
  (ctx/forget-where! c hold-kind #(= (:root c) (:job %))))

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
