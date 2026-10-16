(ns jobs.movement.boat-drive
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.cond :as cond]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Steer the boat or raft the body is in to the water cell :pos, in bounded strokes (the paddle primitive: a few ticks of turn and forward each).
  Each stroke reads where the boat is and which way it points, turns toward the cell while the heading is off, and paddles forward once it is close to right.
  Done when the boat is within :range of the middle of the cell. Holds the vehicle (jobs.lib.vehicle) while the job lives.
  A cell the body has not sensed is looked at first; one it still cannot make out is steered toward all the same, looked at again from each
  new cell on the way, and judged once it is made out (a boat that gets there on a cell never made out is done). Every key is released at the end of a stroke and by a cut.

  Impossible: :bad-args, and between strokes :not-a-boat (another vehicle), :not-water (the cell is seen not to be water).
  Condition: :not-aboard (on foot, at the start or got out on the way: a listed call waits; jobs.movement.boat-launch puts the body in a boat).
  Ends done (data {:left blocks}) or
  nothing-possible: :blocked (land in the way: two forward strokes in a row that could not move),
  :failed (any other paddle status, data :primitive); limit-reached :job-time (the call's :spend :job-time used up, data the distance left).

  Never :continue.")

(a/defargs args
  {:pos {:doc "the water cell to steer to, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :range {:doc "done when the boat is within this many blocks (horizontally) of the middle of the cell" :spec (a/num-in 0 nil) :default 1.5}})

(def target [:pos])

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c))))))
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a drive may take before it gives up." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def aligned-deg 8)
(def forward-while-turning-deg 30)
(def max-turn-ticks 10)
(def max-forward-ticks 20)
(def max-blocked 2)
(def boat-speed "Blocks a second a boat covers, for the quote." 5)

(defn centre-of [{:keys [x z]}] {:x (+ x 0.5) :z (+ z 0.5)})

(defn cell-of-call [c] (:pos (b/parse (:args c))))

(defn bad-args
  "Never :bad-args unless :pos names a cell."
  [c]
  (if (cell-of-call c) (contract/ok :args "a cell to steer to") (contract/never :bad-args "no :pos cell to steer to")))

(def can-start? (cond/and bad-args (cond/mounted)))

(def boat-name? #(boolean (re-find #"_(chest_)?(boat|raft)$" (or % ""))))

(defn can-run?
  "Between strokes: still aboard (:later, a listed call waits), in a boat, and the cell not seen to be land or another block."
  [c]
  (let [p (:primitives c)
        ride (vehicle/vehicle-of (.self p))
        seen (some->> (cell-of-call c) (u/seen-name p))]
    (cond
      (nil? ride) (contract/later :not-aboard "on foot" :hint "board a boat first, e.g. mount")
      (not (boat-name? (:name ride))) (contract/never :not-a-boat "the vehicle is not a boat" :vehicle (:name ride))
      (and seen (not= "water" seen)) (contract/never :not-water "the cell is not water" :seen seen)
      :else (contract/ok :aboard "aboard a boat, the cell is water"))))

(defn left-of
  "Blocks (horizontally) from the vehicle to the middle of the cell, or nil when not aboard."
  [c]
  (when-let [cell (cell-of-call c)]
    (let [e (some-> (vehicle/vehicle-of (.self (:primitives c))) :id)
          boat (when e (some #(when (= e (.-id %)) %) (array-seq (.entities (:primitives c) #js {:radius 64}))))]
      (when boat (u/dist (assoc (u/pos-of (.-pos boat)) :y 0) (assoc (centre-of cell) :y 0))))))

(defn done?
  "Done when the boat is within :range of the cell."
  [c]
  (let [left (left-of c)
        near? (and left (<= left (:range (:args c))))]
    (if near?
      (contract/ok :near "the boat is at the cell" :units :cells :done 1 :left 0 :result {:left left})
      (contract/later :driving "the boat is not at the cell yet" :units :cells :done 0 :left 1 :result {:left left}))))

(defn quote-of [c]
  (let [cell (cell-of-call c)
        most (or (get-in (ctx/frozen-call c) [:spend :job-time]) (time-s))
        left (or (left-of c) 20)
        mean (min most (/ left boat-speed))]
    (cond-> {:uses {:job-time [0 mean most]}}
      cell (assoc :moves-to [(+ (:x cell) 0.5) (:y cell) (+ (:z cell) 0.5)]))))

(defn heading-error
  "Degrees to turn to face bearing from yaw, in (-180, 180]: positive turns right (the yaw grows)."
  [yaw bearing]
  (- (mod (+ (- bearing yaw) 540) 360) 180))

(defn stroke-for
  "The next stroke {:turn :forward :ticks} from the boat's pose, the middle of the cell, :range and the model's turn-deg (degrees
  a tick of turning) and speed (blocks a tick of forward), as the paddle primitive reports them."
  [{:keys [x z]} yaw goal range turn-deg speed]
  (let [err (heading-error yaw (vehicle/yaw-toward {:x x :z z} goal))
        miss (u/dist {:x x :y 0 :z z} (assoc goal :y 0))]
    (if (> (js/Math.abs err) aligned-deg)
      {:turn (if (pos? err) "right" "left")
       :forward (< (js/Math.abs err) forward-while-turning-deg)
       :ticks (min max-turn-ticks (js/Math.ceil (/ (js/Math.abs err) turn-deg)))}
      {:turn nil :forward true
       :ticks (max 1 (min max-forward-ticks (js/Math.ceil (/ (- miss range) speed))))})))

(defn pose-of [r] {:pos {:x (.. r -pos -x) :y (.. r -pos -y) :z (.. r -pos -z)} :yaw (.-yaw r) :turn-deg (.-turnDeg r) :speed (.-speed r)})

(defn paddle! [c {:keys [turn forward ticks]}]
  (ctx/act c :paddle (clj->js (cond-> {:ticks ticks :forward forward} turn (assoc :turn turn)))))

(defn ^:async stroke!
  "One stroke from the boat's pose: :again, or the ending (the engine ends the call :done between strokes once done? holds). st is an atom {:strokes :blocked :looked-from}."
  [c st]
  (let [p (:primitives c)
        {:keys [range]} (:args c)
        cell (cell-of-call c)
        goal (centre-of cell)
        stop! (fn [kind reason & [data]] (ctx/end c kind reason data))
        r (await (ctx/act c :paddle #js {:ticks 0}))]
    (if (not= "ok" (.-status r))
      (stop! :nothing-possible :failed {:primitive (.-status r)})
      (let [{:keys [pos yaw turn-deg speed]} (pose-of r)
            {:keys [strokes blocked looked-from]} @st
            left (u/dist (assoc pos :y 0) (assoc goal :y 0))
            here (look/cell-of pos)
            look? (and (nil? (u/seen-name p cell)) (not= here looked-from) (> left range))
            _ (when look? (await (sense/look-at! c [(:x cell) (:y cell) (:z cell)])))
            looked (if look? here looked-from)]
        (let [s (stroke-for pos yaw goal range turn-deg speed)
                out (await (paddle! c s))
                status (.-status out)]
            (cond
              (= "blocked" status)
              (if (and (:forward s) (>= (inc blocked) max-blocked))
                (stop! :nothing-possible :blocked {:pos (js->clj (.-pos out) :keywordize-keys true) :left left})
                (do (reset! st {:strokes (inc strokes) :blocked (inc blocked) :looked-from looked}) :again))
              (= "ok" status) (do (reset! st {:strokes (inc strokes) :blocked 0 :looked-from looked}) :again)
              :else (stop! :nothing-possible :failed {:primitive status})))))))

(defn ^:async round [c]
  (let [cell (cell-of-call c)
        p (:primitives c)
        st (atom {:strokes 0 :blocked 0 :looked-from nil})]
    (when (and cell (vehicle/mounted? p))
      (await (sense/decide! c :boat-drive.cell #(u/seen-name p cell))))
    (vehicle/hold! c)
    (await (pace/steps! c #(stroke! c st)))))
