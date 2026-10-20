(ns plan.rail-route
  "Where a rail line between two cells can run: pure, no IO. `connect` searches the known ground for waypoints that
  `plan.rail/layout` takes: straight legs along x or z, each flat or a slope of 1 per cell, corners only on flat
  ground between flat legs, legs long enough for the launch groups. Positions are [x y z]."
  (:require [plan.rail :as rail]))

(def ^:private dirs [[1 0] [-1 0] [0 1] [0 -1]])

(def ^:private turn-cost 3)
(def ^:private slope-cost 2)
(def ^:private tunnel-cost 4)
(def ^:private bridge-cost 3)
(def ^:private min-leg 3)
(def ^:private flat-after-turn 2)

(defn- span-move
  "The span a straight flat step from height y onto a column of ground height ny (nil: none known) continues or starts,
  as [kind length], or nil: rock 2 or more above the rail is tunnelled, a drop of 2 or more (or a known gap with no
  ground) is bridged, each up to its limit. A span ends where the ground is within one of the rail again."
  [{:keys [tunnel bridge gap?]} y ny [kind n] x z]
  (let [rock? (and ny (>= (- ny y) 2))
        drop? (if ny (>= (- y ny) 2) (and gap? (gap? x z)))
        next-kind (cond rock? :tunnel drop? :bridge)]
    (when (and next-kind (or (nil? kind) (= kind next-kind)))
      (let [n (inc (or n 0))]
        (when (<= n (if (= :tunnel next-kind) tunnel bridge)) [next-kind n])))))

(defn- moves
  "The states reachable from state s = [x z dir run first? flat dy y span] with the ground height at the next cell known:
  each [state cost] keeps the corner and slope rules of `plan.rail/structure-error`. Inside a span (a tunnel or a bridge,
  flat and straight) the way keeps its height and direction."
  [{:keys [height near far] :as env} [x z [dx dz :as dir] run first? flat dy y span]]
  (let [turn-ok? (and (nil? span) (>= flat 2) (>= run (if first? near min-leg)))
        options (cond-> [[dir 0]] turn-ok? (into (for [d [(rail/left-of dir) (rail/right-of dir)]] [d turn-cost])))]
    (for [[[ndx ndz :as d] extra] options
          :let [nx (+ x ndx) nz (+ z ndz)
                ny (height nx nz)
                step (when ny (- ny y))
                turned? (pos? extra)
                spanned (when (and (not turned?) (zero? dy) (>= flat flat-after-turn)) (span-move env y ny span nx nz))
                walked? (and step (<= -1 step 1) (not (and span (not (zero? step)))))
                keep-flat? (or turned? (and first? (< run near)) (and (not first?) (< run flat-after-turn)))]
          :when (or spanned (and walked? (or (zero? step) (not keep-flat?)) (not (and (pos? step) (neg? dy)))))
          :let [y2 (if spanned y ny)
                step2 (if spanned 0 step)]]
      [[nx nz d (if turned? 1 (min (inc run) near)) (and first? (not turned?)) (if (zero? step2) (min (inc flat) near) 0) step2 y2 spanned]
       (+ 1 extra (case (first spanned) :tunnel tunnel-cost :bridge bridge-cost (if (zero? step2) 0 slope-cost)))])))

(defn- waypoints
  "The cells of a path with a waypoint wherever the way turns or the slope changes."
  [cells]
  (let [kind (fn [[x1 y1 z1] [x2 y2 z2]] [(compare x2 x1) (compare z2 z1) (- y2 y1)])]
    (->> (partition 3 1 cells)
         (keep (fn [[a b c]] (when (not= (kind a b) (kind b c)) b)))
         (#(concat [(first cells)] % [(peek cells)]))
         vec)))

(defn- spans-of
  "The tunnel and bridge spans of a path: states [... y span] and the cells they stand in, as
  [{:kind :tunnel|:bridge :from [x y z] :to [x y z] :length n}] (from and to are the span's first and last cell)."
  [states cells]
  (->> (map vector states cells)
       (partition-by (fn [[s _]] (first (peek s))))
       (keep (fn [part]
               (let [kind (some-> part first first peek first)]
                 (when kind {:kind kind :from (second (first part)) :to (second (last part)) :length (count part)}))))
       vec))

(defn connect
  "A route from cell `from` to cell `to` as {:waypoints [[x y z] ...] :spans [...]} for `plan.rail/layout`, or {:error why}.
  `height` is (fn [x z]) -> the y a rail would have in that column, nil where none can run (unknown, water, a wall).
  The two ends take their y from the arguments. Options: :launch (as layout's, default its own), :launch-ends,
  :max-nodes (search budget, default 20000), :tunnel and :bridge (the longest span of each, default 0: none; a tunnel
  runs flat through ground 2 or more above the rail, a bridge flat over ground 2 or more below it or over a column
  where (:gap? x z) holds and `height` knows none), :gap?. :spans lists each tunnel or bridge as {:kind :from :to
  :length}, in line order. Errors: :no-route (none exists in the known ground or budget spent), or layout's error for
  the cheapest path found (e.g. :touching, when the path runs beside itself)."
  [height [fx fy fz :as from] [tx ty tz :as to] opts]
  (let [{:keys [launch launch-ends max-nodes tunnel bridge gap?]} (merge rail/defaults {:max-nodes 20000 :tunnel 0 :bridge 0} opts)
        near (+ launch 3)
        far (if (= :both launch-ends) near 3)
        ends {[fx fz] fy [tx tz] ty}
        env {:height (fn [x z] (or (ends [x z]) (height x z))) :near near :far far :tunnel tunnel :bridge bridge :gap? gap?}
        goal? (fn [[x z _ run _ flat dy _ span]] (and (= [x z] [tx tz]) (>= run far) (>= flat far) (zero? (or dy 0)) (nil? span)))
        h (fn [[x z]] (+ (abs (- x tx)) (abs (- z tz))))
        starts (for [d dirs] [fx fz d 0 true 0 0 fy nil])]
    (loop [open (into (sorted-set) (map-indexed (fn [i s] [(h s) i 0 s]) starts))
           came (into {} (map (fn [s] [s [0 nil]])) starts)
           n (count starts)]
      (if-let [[_ _ g s :as top] (first open)]
        (cond
          (goal? s) (let [states (->> (iterate (comp second came) s) (take-while some?) reverse vec)
                          cells (mapv (fn [[x z _ _ _ _ _ y]] [x y z]) states)
                          wps (waypoints cells)
                          laid (rail/layout wps opts)]
                      (if (:error laid) (select-keys laid [:error :at :leg]) {:waypoints wps :spans (spans-of states cells)}))
          (> n max-nodes) {:error :no-route}
          :else (let [fresh (for [[t c] (moves env s) :let [g2 (+ g c)] :when (< g2 (first (get came t [##Inf])))] [t g2])]
                  (recur (into (disj open top) (map-indexed (fn [i [t g2]] [(+ g2 (h t)) (+ n i) g2 t])) fresh)
                         (into came (map (fn [[t g2]] [t [g2 s]])) fresh)
                         (+ n (count fresh)))))
        {:error :no-route}))))

(def min-span "Shortest tunnel or bridge worth its own job: shorter ones the line's own build handles." 3)

(defn built-spans
  "The tunnel and bridge spans of a laid chain `cells` ([x y z] in track order) as judged by kind-of, (fn [pos]) -> :tunnel,
  :bridge or nil: runs of at least `min-span` straight, level cells of one kind that have a cell before them to stand on, in
  line order, each {:kind :cells [...] :heading [dx dz] :stand pos}."
  [cells kind-of]
  (let [n (count cells)
        straight? (fn [i] (and (pos? i) (< i n) (= (get-in cells [i 1]) (get-in cells [(dec i) 1]))
                               (or (= 1 i) (= (rail/heading (cells (- i 2)) (cells (dec i))) (rail/heading (cells (dec i)) (cells i))))))
        runs (partition-by (fn [i] (when (straight? i) (kind-of (cells i)))) (range 1 n))]
    (vec (for [run runs
               :let [kind (when (straight? (first run)) (kind-of (cells (first run))))]
               :when (and kind (>= (count run) min-span))
               :let [i (first run)]]
           {:kind kind :cells (mapv cells run) :heading (rail/heading (cells (dec i)) (cells i)) :stand (cells (dec i))}))))
