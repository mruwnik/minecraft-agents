(ns jobs.movement.go-to.air
  "go-to and water: under water when no way to the goal fits the air left (or a search goes on with little air left, or
  keeps beginning afresh), swim to air first, then plan again; an arrival in water reads a dry stand in range."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.perception :as perception]
            [engine.path.planner.base :as base]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.breath :as breath]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.movement.go-to.column :as column]
            [jobs.movement.go-to.tuning :as tuning]))

(def max-surfacings "Walks to air one go-to call makes at most." 3)

(def look-args "breath/find-air!'s look: columns within :radius, cells up to :reach above the feet." {:radius 2 :reach 10})

(def air-radius "Columns this far sideways are read for air when the looks found none nearer (breathe's :air-radius default)." 8)

(defn dry-stand
  "The seen cell within radius of pos (the body) and within range within of goal, nearest pos, where the body stands
  out of water: feet and head open, a solid under the feet; nil when there is none."
  [p goal within pos radius]
  (let [k (column/kind-at p false)
        [gx gy gz] (mapv #(js/Math.floor (% goal)) [:x :y :z])
        [bx by bz] (mapv #(js/Math.floor (% pos)) [:x :y :z])
        n (js/Math.floor radius)
        sq (fn [a b c] (+ (* a a) (* b b) (* c c)))
        dry? (fn [[x y z]] (and (keyword-identical? :open (k x y z)) (keyword-identical? :open (k x (inc y) z))
                                (keyword-identical? :solid (k x (dec y) z))))
        cells (for [dx (range (- n) (inc n)) dy (range (- n) (inc n)) dz (range (- n) (inc n))
                    :let [x (+ bx dx) y (+ by dy) z (+ bz dz)]
                    :when (and (<= (sq dx dy dz) (* radius radius)) (<= (sq (- x gx) (- y gy) (- z gz)) (* within within)))]
                [x y z])
        far (fn [[x y z]] (u/dist pos {:x (+ x 0.5) :y y :z (+ z 0.5)}))
        found (filter dry? cells)]
    (when (seq found)
      (zipmap [:x :y :z] (apply min-key far found)))))

(defn air-left
  "Seconds of air the body has left before it drowns: the planner's airSupply less the air its oxygen says is used."
  [p]
  (- (unchecked-get base/DEFAULT-COSTS "airSupply") (cost/air-used (.-oxygen (.self p)))))

(defn look-cells
  "The cells a drowning body looks again at for air, those still unknown: the goal's head cell, then the cell over the
  head 4 ahead along each heading whose head cell 4 ahead is seen water (open water, where a roof pocket may be)."
  [p pos goal]
  (let [fx (js/Math.floor (:x pos)) hy (inc (js/Math.floor (:y pos))) fz (js/Math.floor (:z pos))
        head [(js/Math.floor (:x goal)) (inc (js/Math.floor (:y goal))) (js/Math.floor (:z goal))]
        ahead (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                    :let [x (+ fx (* 4 dx)) z (+ fz (* 4 dz))]
                    :when (= "water" (breath/seen-water-name p {:x x :y hy :z z}))]
                [x (inc hy) z])]
    (filterv #(sense/unknown? p %) (distinct (cons head ahead)))))

(defn aim-point
  "The point a look at cell aims at from eye, shifted up and sideways by frac of the sight rays' spacing there, so a
  pass lands its rays where the last one did not (a cell seen edge-on shows to some rays only)."
  [eye [x y z] frac ray-deg]
  (let [cx (+ x 0.5) cy (+ y 0.5) cz (+ z 0.5)
        vx (- cx (:x eye)) vz (- cz (:z eye))
        flat (max 1e-6 (js/Math.hypot vx vz))
        d (js/Math.hypot vx (- cy (:y eye)) vz)
        s (* frac d (js/Math.tan (* ray-deg (/ js/Math.PI 180))))]
    {:x (+ cx (* s (/ (- vz) flat))) :y (+ cy s) :z (+ cz (* s (/ vx flat)))}))

(defn ^:async look-again!
  "Look again at the look-cells for air, aims shifted by i/(n+1) of the ray spacing for the i-th of n tries (mem
  :air-relooks), a sight pass after each look, as a player who glimpsed air keeps looking. True when it looked; nil when
  the tries are spent, nothing is left unknown or the air left is under the air-look-min setting (the fallback needs it)."
  [c pos goal]
  (let [p (:primitives c)
        n (tuning/air-looks)
        i (inc (:air-relooks (ctx/mem c) 0))
        cells (look-cells p pos goal)]
    (when (and (<= i n) (seq cells)
               (>= (air-left p) (tuning/air-look-min)))
      (ctx/update-mem! c assoc :air-relooks i)
      (ctx/emit! c :go-to.air-look :info {:cells cells :text (str "short of air: looking again for air (" i " of " n ")")})
      (let [eye {:x (:x pos) :y (+ (:y pos) game/eye-height) :z (:z pos)}
            ray-deg (or (some-> (aget p "perception") :opts :ray-deg) (settings/get perception/settings :engine.perception/ray-deg))]
        (loop [[cell & more] cells]
          (when cell
            (await (ctx/act c :look (clj->js {:pos (aim-point eye cell (/ i (inc n)) ray-deg)})))
            (look/see! c)
            (recur more))))
      true)))

(defn ^:async swim-up-blind!
  "No air known and nothing seen over the own column: swim up it, as breathe does in the dark (a body learns by moving).
  True when the swim surfaced or rose; nil otherwise."
  [c pos]
  (let [fy (js/Math.floor (:y pos))
        r (await (ctx/act c :swim #js {}))
        y (js/Math.floor (:y (u/self-pos c)))]
    (or (= "surfaced" (.-status r)) (> y fy))))

(defn ^:async swim-to-air!
  "Look for air (up the own column first, then the columns beside it, breath/find-air!), else read seen air within
  air-radius, and walk one round (near/walk-round! with walk-opts) to that surface cell, range 0: a way to air, whose
  drowning is priced, not refused; why starts its go-to.surfacing info. With no air known and the own column open, swims
  up it blind (one swim); with the column not open (a block seen over it), look-again! toward goal. counted is called
  when it swims. True when it walked there or on (the caller plans again); nil when no air is known and nothing is left
  to look at."
  [c walk-opts goal why counted]
  (let [p (:primitives c)
        pos (u/self-pos c)
        target (or (await (breath/find-air! c pos (:radius look-args) (:reach look-args)))
                   (breath/nearest-air p pos air-radius (:reach look-args) nil false))]
    (if-not target
      (if (breath/column-open? p (js/Math.floor (:x pos)) (js/Math.floor (:z pos)) (js/Math.floor (:y pos)) (:reach look-args))
        (do (counted)
            (ctx/emit! c :go-to.surfacing :info {:text (str why ", nothing seen above: swimming up blind")})
            (await (swim-up-blind! c pos)))
        (await (look-again! c pos goal)))
      (do
        (counted)
        (ctx/emit! c :go-to.surfacing :info {:at target :text (str why ": swimming up to " [(:x target) (:y target) (:z target)] " first")})
        (let [{:keys [status]} (await (near/walk-round! c target 0 walk-opts))]
          (contains? #{"arrived" "partial" "searching"} status))))))

(defn ^:async surface!
  "A round refused :air with the head under water: the planner refuses a swim past the air limit unless the goal is a
  way to air, so a far goal leaves the body under water: swim-to-air!, at most max-surfacings a call (mem :surfacings).
  Nil when the head is out or the tries are spent."
  [c walk-opts goal]
  (let [n (:surfacings (ctx/mem c) 0)]
    (when (and (< n max-surfacings) (breath/head-under? (:primitives c)))
      (await (swim-to-air! c walk-opts goal "short of air" #(ctx/update-mem! c assoc :surfacings (inc n)))))))

(defn ^:async surface-while-searching!
  "A round whose search goes on (no walk) with the head under water: swim-to-air! first, as a player swims up while
  thinking, when less than surface-air seconds of air are left, or when the search began afresh (fresh?) more than
  restarts times since the last swim up (mem :wet-fresh): a kept search under water is dropped once its air drifts
  (jobs.lib.walk.search/air-drift), so one that needs more slices never ends there. Never capped while air is short;
  else at most max-surfacings swims in a row that left the head under water (mem :wet-surfacings). True when it swam."
  [c walk-opts goal fresh? {:keys [surface-air restarts]}]
  (let [p (:primitives c)]
    (when (breath/head-under? p)
      (let [m (ctx/mem c)
            n (cond-> (:wet-fresh m 0) fresh? inc)
            low? (< (air-left p) surface-air)
            restarted? (and (> n restarts) (< (:wet-surfacings m 0) max-surfacings))]
        (when fresh? (ctx/update-mem! c assoc :wet-fresh n))
        (when (or low? restarted?)
          (ctx/update-mem! c assoc :wet-fresh 0)
          (let [swam (await (swim-to-air! c walk-opts goal (if low? "short of air" "searching under water") (fn [])))]
            (ctx/update-mem! c update :wet-surfacings #(if (breath/head-under? p) (inc (or % 0)) 0))
            swam))))))
