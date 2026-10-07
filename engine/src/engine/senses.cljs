(ns engine.senses
  "The sensing rules over what the primitives report raw: is it day, is it raining, can the body see or hit an entity.
  The primitives give timeOfDay, rainState and thunderState, entities with their height and lying flag, and the raw world
  gives cells (stateAt, sightTable, shapesAt, eye); wrap derives isDay, raining, thundering, visible, hittable and
  sleeping from them, lists only what a player would see, and passes the sleep preconditions down."
  (:require [engine.sight :as sight]))

(def rain-level 0.2)   ;; vanilla client: raining above this rain level, thundering above thunder-level while raining
(def thunder-level 0.9)
(def hit-range 6)      ;; melee reach checked by entities: hittable is reported within it
(def monster-range 8)  ;; sleep refuses with a hostile within this many blocks (level) and 5 vertically
(def monster-height 5)
(def default-height 1.8)
(def max-listed 1000000)

(defn day-at? [time-of-day] (or (< time-of-day 12542) (> time-of-day 23460)))

(defn weather-of
  "{:raining :thundering} of the rain and thunder levels."
  [rain thunder]
  (let [raining (> (or rain 0) rain-level)]
    {:raining raining :thundering (and raining (> (or thunder 0) thunder-level))}))

;; ---- sight over the raw world

(defn blocks-sight?
  "Whether the cell holds something that stops the eye; an unloaded cell never does, so a threat is not hidden by a gap."
  [^js raw ^js table x y z]
  (let [id (.stateAt raw x y z)]
    (and (>= id 0) (some? table) (= 1 (aget table id)))))

(defn can-see?
  "A clear line from the eye to the middle of the entity (its height, 1.8 when unknown)."
  [^js raw ^js eye ^js e]
  (let [^js pos (.-pos e) table (.sightTable raw)]
    (sight/line-clear (.-x eye) (.-y eye) (.-z eye)
                      (.-x pos) (+ (.-y pos) (/ (or (.-height e) default-height) 2)) (.-z pos)
                      (fn [x y z] (blocks-sight? raw table x y z)))))

(defn can-hit?
  "A melee swing needs a clear line from the eye to the feet, the middle or the head of the entity, over collision boxes."
  [^js raw ^js eye ^js e]
  (let [^js pos (.-pos e) h (or (.-height e) default-height)]
    (boolean (some (fn [dy] (sight/ray-clear (.-x eye) (.-y eye) (.-z eye) (.-x pos) (+ (.-y pos) dy) (.-z pos)
                                             (fn [x y z] (.shapesAt raw x y z))))
                   [0.2 (/ h 2) (- h 0.1)]))))

;; ---- the wrapped primitives

(defn- self-view
  "The raw self with isDay, raining and thundering derived from timeOfDay, rainState and thunderState."
  [^js s]
  (if (= "offline" (.-status s))
    s
    (let [{:keys [raining thundering]} (weather-of (.-rainState s) (.-thunderState s))
          out (js/Object.assign #js {} s)]
      (js-delete out "rainState")
      (js-delete out "thunderState")
      (aset out "isDay" (day-at? (.-timeOfDay s)))
      (aset out "raining" raining)
      (aset out "thundering" thundering)
      out)))

(defn- sense-entities
  "The raw entity list as a player's senses give it: a passive mob or a villager behind a wall is dropped; hostiles, items
  and players stay with `visible`; hittable within hit-range (not for items); a player's sleeping needs sight."
  [raw ^js eye entities max]
  (->> (array-seq entities)
       (map (fn [^js e] (js/Object.assign #js {} e)))
       (map (fn [^js e] (let [k (.-kind e)
                              see? (can-see? raw eye e)]
                          [e k see?])))
       (filter (fn [[_ k see?]] (or (#{"hostile" "item" "player"} k) see?)))
       (take max)
       (map (fn [[^js e k see?]]
              (when (#{"hostile" "item" "player"} k) (aset e "visible" see?))
              (when (and (not= k "item") (<= (.-distance e) hit-range)) (aset e "hittable" (can-hit? raw eye e)))
              (when (= k "player") (aset e "sleeping" (boolean (and (.-lyingDown e) see?))))
              (js-delete e "lyingDown")
              (js-delete e "height")
              e))))

(defn- monsters-near?
  "A hostile within monster-range level and monster-height vertically of the body."
  [^js p ^js s]
  (let [^js me (.-pos s)]
    (boolean (some (fn [^js e] (let [^js pos (.-pos e)]
                                 (and (<= (js/Math.hypot (- (.-x pos) (.-x me)) (- (.-z pos) (.-z me))) monster-range)
                                      (<= (js/Math.abs (- (.-y pos) (.-y me))) monster-height))))
                   (array-seq (.entities p #js {:kind "hostile" :radius (+ monster-range monster-height) :max max-listed}))))))

;; ---- useOn line

(def inset 0.02)
(def full-box #js [0 0 0 1 1 1])

(defn- aim-points
  "Where a click could land on the block at (px py pz): the centre and the face centres (pulled inside) of each box,
  nearest to the eye first."
  [px py pz boxes ^js eye]
  (let [pts (for [^js b boxes
                  :let [[x0 y0 z0 x1 y1 z1] (array-seq b)
                        cx (/ (+ x0 x1) 2) cy (/ (+ y0 y1) 2) cz (/ (+ z0 z1) 2)]
                  [x y z] [[cx cy cz] [(+ x0 inset) cy cz] [(- x1 inset) cy cz] [cx (+ y0 inset) cz]
                           [cx (- y1 inset) cz] [cx cy (+ z0 inset)] [cx cy (- z1 inset)]]]
              [(+ px x) (+ py y) (+ pz z)])
        dist (fn [[x y z]] (js/Math.hypot (- x (.-x eye)) (- y (.-y eye)) (- z (.-z eye))))]
    (sort-by dist pts)))

(defn no-line?
  "True when the eye's ray reaches no part of the block at p ({:x :y :z}) before another block's shape; the target's
  own other half (same name, adjacent, beds also sideways) never blocks."
  [^js p ^js raw ^js eye ^js pos]
  (let [px (js/Math.floor (.-x pos)) py (js/Math.floor (.-y pos)) pz (js/Math.floor (.-z pos))
        name-at (fn [x y z] (some-> (.blockAt p #js {:x x :y y :z z}) .-name))
        target (name-at px py pz)
        partner? (fn [x y z] (and (= 1 (+ (js/Math.abs (- x px)) (js/Math.abs (- y py)) (js/Math.abs (- z pz))))
                                  (or (not= y py) (boolean (re-find #"_bed$|^respawn_anchor$" (or target ""))))
                                  (= (name-at x y z) target)))
        shapes-at (fn [x y z] (if (partner? x y z) #js [] (let [s (.shapesAt raw x y z)] (if (pos? (alength s)) s #js []))))
        boxes (let [s (.shapesAt raw px py pz)] (if (pos? (alength s)) s #js [full-box]))]
    (not-any? (fn [[tx ty tz]] (sight/ray-clear (.-x eye) (.-y eye) (.-z eye) tx ty tz shapes-at))
              (aim-points px py pz boxes eye))))

(defn wrap
  "The primitives object p with self, entities, sleep and onBodyEvent as the contract has them (README.md): derived from
  what p reports raw, over the raw world (engine/js/raw-world.mjs's shape: eye, stateAt, sightTable, shapesAt)."
  [^js p ^js raw]
  (let [out (js/Object.assign #js {} p)]
    (aset out "self" (fn [] (self-view (.self p))))
    (aset out "entities"
          (fn [a]
            (let [opts (or a #js {})
                  max (if (some? (.-max opts)) (.-max opts) 32)
                  entities (.entities p (doto (js/Object.assign #js {} opts) (aset "max" max-listed)))
                  eye (.eye raw)]
              (if (and eye (pos? (alength entities)))
                (into-array (sense-entities raw eye entities max))
                entities))))
    (aset out "sleep"
          (fn [token a]
            (let [^js s (.self p)
                  online? (not= "offline" (.-status s))
                  flags #js {:notNight (and online? (day-at? (.-timeOfDay s)))
                             :monstersNear (and online? (monsters-near? p s))}]
              (.sleep p token (js/Object.assign #js {} a flags)))))
    (aset out "useOn"
          (fn [token a]
            (let [^js eye (.eye raw)
                  ^js pos (some-> a .-pos)
                  flags (when (and eye pos (number? (.-x pos)) (number? (.-y pos)) (number? (.-z pos)))
                          #js {:noLine (no-line? p raw eye pos)})]
              (.useOn p token (js/Object.assign #js {} a flags)))))
    (aset out "onBodyEvent"
          (fn [listener]
            (let [now (fn [] (let [^js s (self-view (.self p))] {:raining (boolean (.-raining s)) :thundering (boolean (.-thundering s))}))
                  weather (atom (try (now) (catch :default _ {:raining false :thundering false})))]
              (.onBodyEvent p (fn [^js e]
                                (if (not= "weather-levels" (.-kind e))
                                  (listener e)
                                  (let [next (weather-of (.-rain e) (.-thunder e))]
                                    (when (not= next @weather)
                                      (reset! weather next)
                                      (listener #js {:kind "weather-changed" :raining (:raining next) :thundering (:thundering next)})))))))))
    out))
