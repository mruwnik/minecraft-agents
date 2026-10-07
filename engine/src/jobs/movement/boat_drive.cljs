(ns jobs.movement.boat-drive
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Steer the boat or raft the body is in to the water cell :pos, in bounded strokes (the paddle primitive: a few ticks of turn and forward each).
  Each stroke reads where the boat is and which way it points, turns toward the cell while the heading is off, and paddles forward once it is close to right.
  Done when the boat is within :range of the middle of the cell. Holds the vehicle (jobs.lib.vehicle) while the job lives.
  The check waits (:unseen) while the cell is not sensed; a cell that turns unsensed later yields :continue until it is. Every key is released at the end of a stroke and by a cut.

  Ends {:status :done :pos {:x :y :z} :strokes n}, info boat.driven, or
  {:status :stopped :reason r}: :bad-args, :not-aboard (on foot: jobs.movement.boat-launch puts the body in a boat), :not-a-boat (another vehicle),
  :not-water (the cell is not water), :blocked (land in the way: two forward strokes in a row that could not move), :timeout (:max-strokes or :max-s used up,
  with the distance left), :failed (any other paddle status, with :primitive).

  Never :continue except for that.")

(a/defargs args
  {:pos {:doc "the water cell to steer to, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :range {:doc "done when the boat is within this many blocks (horizontally) of the middle of the cell" :spec (a/num-in 0 nil) :default 1.5}
   :max-strokes {:doc "strokes before giving up" :spec (a/int-in 1 nil) :default 80}
   :max-s {:doc "seconds before giving up" :spec (a/num-in 0 nil) :default 120}})

(def aligned-deg 8)
(def forward-while-turning-deg 30)
(def max-turn-ticks 10)
(def max-forward-ticks 20)
(def max-blocked 2)

(defn centre-of [{:keys [x z]}] {:x (+ x 0.5) :z (+ z 0.5)})

(defn target [c] (:pos (b/parse (:args c))))

(defn check
  "Waits while the target cell is not sensed; bad args pass for the round to stop on."
  [c]
  (let [cell (target c)]
    (or (nil? cell)
        (some? (u/seen-name (:primitives c) cell))
        (ctx/wait c {:reason :unseen :why "the water cell is not sensed yet"}))))

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

(defn ^:async round [c]
  (let [p (:primitives c)
        {:keys [range max-strokes max-s]} (:args c)
        cell (target c)
        deadline (+ (js/Date.now) (* 1000 max-s))
        goal (some-> cell centre-of)
        stop! (fn [reason text & more] (apply result/stop! c reason text more))]
    (cond
      (nil? cell) (stop! :bad-args (:error (b/parse (:args c))))
      (not (vehicle/mounted? p)) (stop! :not-aboard "the body is not in a boat")
      (nil? (u/seen-name p cell)) :continue
      (not= "water" (u/seen-name p cell)) (stop! :not-water (str "the cell is " (u/seen-name p cell) ", not water") :pos cell)
      :else
      (do
        (vehicle/hold! c)
        (loop [strokes 0 blocked 0]
          (let [r (await (ctx/act c :paddle #js {:ticks 0}))
                status (.-status r)]
            (if (not= "ok" status)
              (case status
                "not-mounted" (stop! :not-aboard "the body is not in a boat")
                "not-a-boat" (stop! :not-a-boat "the vehicle is not a boat")
                (stop! :failed (str "paddle: " status) :primitive status))
              (let [{:keys [pos yaw turn-deg speed]} (pose-of r)
                    left (u/dist (assoc pos :y 0) (assoc goal :y 0))]
                (cond
                  (<= left range)
                  (do (ctx/emit! c :boat.driven :info {:pos pos :strokes strokes :text (str "steered the boat to " (:x cell) " " (:y cell) " " (:z cell))})
                      (result/finish! c {:pos pos :strokes strokes}))
                  (or (>= strokes max-strokes) (>= (js/Date.now) deadline))
                  (stop! :timeout (str "the boat is still " (int left) " blocks from the cell after " strokes " strokes") :pos pos :left left)
                  :else
                  (let [s (stroke-for pos yaw goal range turn-deg speed)
                        out (await (paddle! c s))
                        st (.-status out)]
                    (cond
                      (= "blocked" st)
                      (if (and (:forward s) (>= (inc blocked) max-blocked))
                        (stop! :blocked (str "land in the way, " (int left) " blocks from the cell") :pos (js->clj (.-pos out) :keywordize-keys true) :left left)
                        (recur (inc strokes) (inc blocked)))
                      (= "ok" st) (recur (inc strokes) 0)
                      :else (stop! :failed (str "paddle: " st) :primitive st))))))))))))
