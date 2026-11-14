(ns plan.rail-route
  "Where a rail line between two cells can run: pure, no IO. `connect` searches the known ground for waypoints that
  `plan.rail/layout` takes: straight legs along x or z, each flat or a slope of 1 per cell, corners only on flat
  ground between flat legs, legs long enough for the launch groups. The rail may leave the ground: it cuts a bump, fills
  a dip, tunnels or bridges, each priced. Positions are [x y z]."
  (:require [plan.rail :as rail]))

(def ^:private dirs [[1 0] [-1 0] [0 1] [0 -1]])

(def ^:private turn-cost "A corner's extra price, in line cells (its two lit rails and their source)." 3)
(def ^:private slope-cost "A climbing cell's extra price, in line cells (every other one lit, with its source)." 2)
(def ^:private min-leg 3)
(def ^:private flat-after-turn 2)

(defn- earthwork
  "What a rail at height y needs over the column x z whose own rail height is ny (nil: no ground known there): nil
  (nothing), :cut or :fill (one block dug or placed), :tunnel (ground 2 or more above the rail) or :bridge (2 or more
  below, or a gap where (gap x z) is at most y), or :none when no rail can run there."
  [gap ny y x z]
  (if ny
    (let [d (- ny y)]
      (cond (zero? d) nil (= 1 d) :cut (= -1 d) :fill (pos? d) :tunnel :else :bridge))
    (if (some-> gap (apply [x z]) (<= y)) :bridge :none)))

(defn- work-price
  "Seconds the earthwork of kind takes at the rail cell [x y z] (prices: :dig and :fill (fn [pos]) -> seconds or nil
  where it may not be done, :dark the light of a tunnel cell), or nil when it may not be done."
  [{:keys [dig fill dark]} kind [x y z]]
  (case kind
    nil 0
    :cut (dig [x y z])
    :tunnel (let [a (dig [x y z]) b (dig [x (inc y) z])] (when (and a b) (+ a b dark)))
    (:fill :bridge) (fill [x (dec y) z])))

(defn- span-of
  "The span [kind n] a step of earthwork kind continues or starts from span (nil: none), or nil when it may not: tunnels
  and bridges run on, each kind on its own, up to its limit (nil: none, n stays 0 so the search has fewer states)."
  [{:keys [tunnel bridge]} kind [k n]]
  (when (and (#{:tunnel :bridge} kind) (or (nil? k) (= k kind)))
    (let [limit (if (= :tunnel kind) tunnel bridge)]
      (cond (nil? limit) [kind 0]
            (< (or n 0) limit) [kind (inc (or n 0))]))))

(defn- moves
  "The states reachable from state s = [x z dir run first? flat dy y span], each [state cost] with cost in seconds. A
  step goes on or turns, and the rail goes up 1, down 1 or keeps its height, as `plan.rail/structure-error` allows; the
  ground there is cut, filled, tunnelled or bridged up to the rail as earthwork says. Tunnels and bridges (spans) are
  flat and straight: inside one the way keeps its height and direction; the step out of one may climb or fall."
  [{:keys [height near gap prices] :as env} [x z [dx dz :as dir] run first? flat dy y span]]
  (let [turn-ok? (and (nil? span) (>= flat 2) (>= run (if first? near min-leg)))
        options (cond-> [[dir 0]] turn-ok? (into (for [d [(rail/left-of dir) (rail/right-of dir)]] [d turn-cost])))]
    (for [[[ndx ndz :as d] extra] options
          step [0 1 -1]
          :let [nx (+ x ndx) nz (+ z ndz)
                y2 (+ y step)
                turned? (pos? extra)
                keep-flat? (or turned? (and first? (< run near)) (and (not first?) (< run flat-after-turn)))
                kind (earthwork gap (height nx nz) y2 nx nz)
                spans? (#{:tunnel :bridge} kind)
                span2 (when spans? (span-of env kind span))
                price (when-not (= :none kind) (work-price prices kind [nx y2 nz]))]
          :when (and price
                     (or (zero? step) (not keep-flat?))
                     (not (and (pos? step) (neg? dy)))
                     (or (not spans?) (and span2 (zero? step) (not turned?) (zero? dy) (>= flat flat-after-turn))))]
      [[nx nz d (if turned? 1 (min (inc run) near)) (and first? (not turned?) (< (inc run) near)) (if (zero? step) (min (inc flat) near) 0) step y2 span2]
       (+ price (* (:cell prices) (+ 1 extra (if (zero? step) 0 slope-cost))))])))

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

(defn- turns-left
  "The fewest corners a way heading dir from [x z] takes to reach [tx tz]: none straight ahead, one to a side ahead,
  two behind."
  [[x z] [dx dz] [tx tz]]
  (let [ex (- tx x) ez (- tz z)
        ahead (+ (* ex dx) (* ez dz))
        aside (- (* ex dz) (* ez dx))]
    (cond (and (zero? aside) (pos? ahead)) 0
          (and (zero? aside) (zero? ahead)) 0
          (and (not (zero? aside)) (>= ahead 0)) 1
          :else 2)))

(defn connect
  "The cheapest route from cell `from` to cell `to` as {:waypoints [[x y z] ...] :spans [...]} for `plan.rail/layout`, or
  {:error why}. `height` is (fn [x z]) -> the y a rail would have on the ground of that column, nil where none is known
  (unknown, water, a wall). The two ends take their y from the arguments. :prices (required) gives the seconds of
  each part: :cell a cell of line, :dig and :fill (fn [[x y z]]) -> digging the block at, or placing one in, a cell (nil:
  it may not be), :dark lighting a tunnel cell. A cell of rail one off its ground is cut or filled; two or more off,
  flat and straight, it is tunnelled through ground above (rail and head cell dug, lit) or bridged over ground below or
  over a column where (:gap x z) (the least rail height over a column `height` knows none of, e.g. water) is at most
  the rail's. Other options: :launch (as layout's, default its own), :launch-ends, :max-nodes (search budget, default
  20000; nil: none), :spent? (fn []) -> true once the caller's budget is spent, :tunnel and :bridge (the longest span of each, nil: no limit but the price). :spans lists each tunnel or bridge
  as {:kind :from :to :length}, in line order. Errors: :no-route (none exists in the known ground; :spent true: the budget ran
  out first), or
  layout's error for the cheapest path found (e.g. :touching, when the path runs beside itself)."
  [height [fx fy fz :as from] [tx ty tz :as to] opts]
  (let [{:keys [launch launch-ends max-nodes spent? tunnel bridge gap prices]} (merge rail/defaults {:max-nodes 20000} opts)
        near (+ launch 3)
        far (if (= :both launch-ends) near 3)
        ends {[fx fz] fy [tx tz] ty}
        env {:height (fn [x z] (or (ends [x z]) (height x z))) :near near :far far :tunnel tunnel :bridge bridge :gap gap
             :prices prices}
        goal? (fn [[x z _ run _ flat dy y span]]
                (and (= [x z] [tx tz]) (= y ty) (>= run far) (>= flat far) (zero? (or dy 0)) (nil? span)))
        h (fn [[x z dir _ _ _ _ y]]
            (* (:cell prices) (+ (abs (- x tx)) (abs (- z tz)) (* slope-cost (abs (- y ty))) (* turn-cost (turns-left [x z] dir [tx tz])))))
        starts (for [d dirs] [fx fz d 0 true 0 0 fy nil])]
    (loop [open (into (sorted-set) (map-indexed (fn [i s] [(h s) 0 i 0 s]) starts))
           came (into {} (map (fn [s] [s [0 nil]])) starts)
           n (count starts)]
      (if-let [[_ _ _ g s :as top] (first open)]
        (cond
          (goal? s) (let [states (->> (iterate (comp second came) s) (take-while some?) reverse vec)
                          cells (mapv (fn [[x z _ _ _ _ _ y]] [x y z]) states)
                          wps (waypoints cells)
                          laid (rail/layout wps opts)]
                      (if (:error laid) (select-keys laid [:error :at :leg]) {:waypoints wps :spans (spans-of states cells)}))
          (or (and max-nodes (> n max-nodes)) (and spent? (spent?))) {:error :no-route :spent true}
          :else (let [fresh (for [[t c] (moves env s) :let [g2 (+ g c)] :when (< g2 (first (get came t [##Inf])))] [t g2])]
                  (recur (into (disj open top) (map-indexed (fn [i [t g2]] [(+ g2 (h t)) (- g2) (+ n i) g2 t])) fresh)
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
