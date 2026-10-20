(ns plan.rail-route
  "Where a rail line between two cells can run: pure, no IO. `connect` searches the known ground for waypoints that
  `plan.rail/layout` takes: straight legs along x or z, each flat or a slope of 1 per cell, corners only on flat
  ground between flat legs, legs long enough for the launch groups. Positions are [x y z]."
  (:require [plan.rail :as rail]))

(def ^:private dirs [[1 0] [-1 0] [0 1] [0 -1]])

(def ^:private turn-cost 3)
(def ^:private slope-cost 2)
(def ^:private min-leg 3)
(def ^:private flat-after-turn 2)

(defn- moves
  "The states reachable from state s = [x z dir run first? flat dy] with the ground height at the next cell known: each
  [state cost] keeps the corner and slope rules of `plan.rail/structure-error`."
  [{:keys [height near far]} [x z [dx dz :as dir] run first? flat dy]]
  (let [y (height x z)
        turn-ok? (and (>= flat 2) (>= run (if first? near min-leg)))
        options (cond-> [[dir 0]] turn-ok? (into (for [d [(rail/left-of dir) (rail/right-of dir)]] [d turn-cost])))]
    (for [[[ndx ndz :as d] extra] options
          :let [nx (+ x ndx) nz (+ z ndz)
                ny (height nx nz)
                step (when ny (- ny y))]
          :when (and step (<= -1 step 1))
          :let [turned? (pos? extra)
                keep-flat? (or turned? (and first? (< run near)) (and (not first?) (< run flat-after-turn)))]
          :when (or (zero? step) (not keep-flat?))
          :when (not (and (pos? step) (neg? dy)))]
      [[nx nz d (if turned? 1 (min (inc run) near)) (and first? (not turned?)) (if (zero? step) (min (inc flat) near) 0) step]
       (+ 1 extra (if (zero? step) 0 slope-cost))])))

(defn- waypoints
  "The cells of a path with a waypoint wherever the way turns or the slope changes."
  [cells]
  (let [kind (fn [[x1 y1 z1] [x2 y2 z2]] [(compare x2 x1) (compare z2 z1) (- y2 y1)])]
    (->> (partition 3 1 cells)
         (keep (fn [[a b c]] (when (not= (kind a b) (kind b c)) b)))
         (#(concat [(first cells)] % [(peek cells)]))
         vec)))

(defn connect
  "A route from cell `from` to cell `to` as {:waypoints [[x y z] ...]} for `plan.rail/layout`, or {:error why}.
  `height` is (fn [x z]) -> the y a rail would have in that column, nil where none can run (unknown, water, a wall).
  The two ends take their y from the arguments. Options: :launch (as layout's, default its own), :launch-ends,
  :max-nodes (search budget, default 20000). Errors: :no-route (none exists in the known ground or budget spent),
  or layout's error for the cheapest path found (e.g. :touching, when the path runs beside itself)."
  [height [fx fy fz :as from] [tx ty tz :as to] opts]
  (let [{:keys [launch launch-ends max-nodes]} (merge rail/defaults {:max-nodes 20000} opts)
        near (+ launch 3)
        far (if (= :both launch-ends) near 3)
        ends {[fx fz] fy [tx tz] ty}
        env {:height (fn [x z] (or (ends [x z]) (height x z))) :near near :far far}
        goal? (fn [[x z _ run _ flat dy]] (and (= [x z] [tx tz]) (>= run far) (>= flat far) (zero? (or dy 0))))
        h (fn [[x z]] (+ (abs (- x tx)) (abs (- z tz))))
        starts (for [d dirs] [fx fz d 0 true 0 0])]
    (loop [open (into (sorted-set) (map-indexed (fn [i s] [(h s) i 0 s]) starts))
           came (into {} (map (fn [s] [s [0 nil]])) starts)
           n (count starts)]
      (if-let [[_ _ g s :as top] (first open)]
        (cond
          (goal? s) (let [cells (->> (iterate (comp second came) s) (take-while some?) reverse
                                     (mapv (fn [[x z]] [x ((:height env) x z) z])))
                          wps (waypoints cells)
                          laid (rail/layout wps opts)]
                      (if (:error laid) (select-keys laid [:error :at :leg]) {:waypoints wps}))
          (> n max-nodes) {:error :no-route}
          :else (let [fresh (for [[t c] (moves env s) :let [g2 (+ g c)] :when (< g2 (first (get came t [##Inf])))] [t g2])]
                  (recur (into (disj open top) (map-indexed (fn [i [t g2]] [(+ g2 (h t)) (+ n i) g2 t])) fresh)
                         (into came (map (fn [[t g2]] [t [g2 s]])) fresh)
                         (+ n (count fresh)))))
        {:error :no-route}))))
