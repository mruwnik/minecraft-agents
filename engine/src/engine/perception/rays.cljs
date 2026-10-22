(ns engine.perception.rays
  "Sight: the view cone, the sight pass that casts rays from the eye into block memory, and keeping memory current
  after block changes and touches."
  (:require ["module" :refer [createRequire]]
            [engine.game :as game]
            [engine.path.offsets :as offsets]
            [engine.sight :as sight]
            [engine.perception.light :refer [darken-now max-light table-now]]
            [engine.perception.store :refer [cell-index new-stamp! record! section-key store-of]]))

;; ---- the view cone

(defn half-extents
  "The tangent half-extents [hx hy] of the view plane for a vertical field of view in degrees and a width over height."
  [fov aspect]
  (let [hy (js/Math.tan (/ (* fov js/Math.PI) 360))] [(* hy aspect) hy]))

(defn grid
  "{:cols :rows :hx :hy}: the ray grid over the view plane, tangent half-extents hx (horizontal) and hy. At effort e
  (0-1, default 1) the grid keeps the share effort-min + (1 - effort-min) e of the rays of a ray-deg grid."
  ([opts] (grid opts 1))
  ([{:keys [fov aspect ray-deg effort-min]} effort]
   (let [[hx hy] (half-extents fov aspect)
         share (+ effort-min (* (- 1 effort-min) (max 0 (min 1 effort))))
         spacing (js/Math.tan (/ (* (/ ray-deg (js/Math.sqrt share)) js/Math.PI) 180))]
     {:hx hx :hy hy :cols (js/Math.ceil (/ (* 2 hx) spacing)) :rows (js/Math.ceil (/ (* 2 hy) spacing))})))

(defn basis
  "Forward, right and up of a mineflayer look (yaw 0 faces -z, pitch up positive), as a flat array of 9."
  [yaw pitch]
  (let [cp (js/Math.cos pitch) sp (js/Math.sin pitch) cy (js/Math.cos yaw) sy (js/Math.sin yaw)
        fx (- (* sy cp)) fy sp fz (- (* cy cp))]
    #js [fx fy fz
         cy 0 (- sy)
         (* sy fy) (- (- (* sy fx)) (* cy fz)) (* cy fy)]))

(defn in-cone? [^js b hx hy vx vy vz]
  (let [f (+ (* vx (aget b 0)) (* vy (aget b 1)) (* vz (aget b 2)))]
    (and (> f 0)
         (<= (js/Math.abs (+ (* vx (aget b 3)) (* vz (aget b 5)))) (* hx f))
         (<= (js/Math.abs (+ (* vx (aget b 6)) (* vy (aget b 7)) (* vz (aget b 8)))) (* hy f)))))

(defn steps-per-pass [{:keys [opts]}] (max 1 (js/Math.round (/ (:pass-ms opts) (:step-ms opts)))))
(defn rays-per-pass
  "The rays of the last pass started (grid and aimed rows), else of a full-effort grid."
  [{:keys [grid st]}]
  (or (.-passRays ^js st) (* (:cols grid) (:rows grid))))

(defn aimed-rays
  "The rays a pass aims at fixed places, from an eye at yaw and pitch, kept when inside the view cone. A flat
  [d l dy ...] array (forward and sideways blocks along the yaw, and height above the eye), cast in that order. Surfaces
  that run along the line of sight (far ground, a tunnel's walls and ceiling) fall between grid rays, so for each block of
  distance there is a row of
    ground rays: ground-across blocks apart at the ground an eye height below the eye, from ground-from to ground-to;
    wall rays: at each of wall-lanes sideways (both sides) and each of wall-heights, from wall-from to wall-to."
  [{:keys [ground-from ground-to ground-across wall-from wall-to wall-lanes wall-heights]} hx hy yaw pitch jx jy]
  (let [b (basis yaw pitch)
        fx (- (js/Math.sin yaw)) fz (- (js/Math.cos yaw)) rx (js/Math.cos yaw) rz (- (js/Math.sin yaw))
        out #js []
        aim! (fn [d l dy]
               (when (in-cone? b hx hy (+ (* d fx) (* l rx)) dy (+ (* d fz) (* l rz)))
                 (.push out d l dy)))]
    (loop [d (+ ground-from jy)]
      (when (< d ground-to)
        (let [k (js/Math.ceil (/ (* hx (+ d game/eye-height)) ground-across))]
          (doseq [i (range (- k) (inc k))]
            (aim! d (* ground-across (+ i jx)) (- game/eye-height))))
        (recur (inc d))))
    (loop [d (+ wall-from jy)]
      (when (< d wall-to)
        (doseq [lane wall-lanes side (if (zero? lane) [1] [-1 1]) dy wall-heights]
          (aim! d (* side lane) dy))
        (recur (inc d))))
    out))

(defn sight-of
  "The sight table, or nil while the raw world has none yet (no registry): an empty table is never kept."
  [{:keys [raw]} ^js st]
  (or (.-sight st)
      (let [^js table (.sightTable ^js raw)]
        (when (and table (pos? (.-length table)))
          (set! (.-sight st) table)))))

;; ---- thin blocks

(def thin-cache (js/WeakMap.)) ;; sight table -> Map of state id -> its clipped boxes, or false

(defn thin-boxes
  "The collision boxes (clipped to the cell) of state id when it blocks sight under `table` but does not fill its cell
  (a door, a trapdoor, a slab, stairs): it stops a line only where the line meets one. Nil for a state that fills it (a
  full cube, or no box: lava) or when the raw world has no stateInfo."
  [^js raw ^js table id]
  (when-let [info-of (aget raw "stateInfo")]
    (let [^js m (or (.get thin-cache table) (let [m (js/Map.)] (.set thin-cache table m) m))
          hit (.get m id)]
      (if (some? hit)
        (when hit hit)
        (let [^js info (.call info-of raw id)
              shapes (some-> info .-shapes array-seq)
              boxes (when (and info (not (.-fullCube info)) (seq shapes))
                      (to-array (map (fn [b] (let [[x0 y0 z0 x1 y1 z1] (array-seq b)]
                                               [(max 0 x0) (max 0 y0) (max 0 z0) (min 1 x1) (min 1 y1) (min 1 z1)]))
                                     shapes)))]
          (.set m id (or boxes false))
          boxes)))))

(defn blocks-line?
  "Whether the cell (x y z), holding state id, stops the segment from (fx fy fz) to (tx ty tz): it blocks sight under
  `table` and, for a thin block (thin-boxes), the segment meets one of its boxes."
  [^js raw ^js table id x y z fx fy fz tx ty tz]
  (and (== 1 (aget table id))
       (if-let [boxes (thin-boxes raw table id)]
         (let [from [fx fy fz] to [tx ty tz]]
           (boolean (some (fn [[x0 y0 z0 x1 y1 z1]]
                            (sight/segment-hits-box? from to [(+ x x0) (+ y y0) (+ z z0) (+ x x1) (+ y y1) (+ z z1)]))
                          (array-seq boxes))))
         true)))

;; ---- the sight pass

(defn near-of
  "How close a dark cell must be to count as seen: :near-torch with a torch in either hand, else :near."
  [{:keys [raw opts]}]
  (let [name-in (fn [k] (when-let [f (aget ^js raw k)] (f)))]
    (if (some #{"torch" "soul_torch"} [(name-in "offHand") (name-in "heldItem")]) (max (:near opts) (:near-torch opts)) (:near opts))))

(def max-mark-sections 600)

(defn marks-of
  "The slice marks of section `key` for the current mark stamp: the kernel's Uint8Array (MARKS_LENGTH), zeroed when the
  stamp is new to it. Kept in st.marks (dropped past max-mark-sections) with the last one asked for cached."
  [^js st key marks-length]
  (let [stamp (.-markStamp st)]
    (if (and (== key (.-markKey st)) (== stamp (.-markArrStamp st)))
      (.-markArr st)
      (let [^js m (or (.-marks st) (set! (.-marks st) (js/Map.)))
            ^js arr (or (.get m key)
                        (do (when (>= (.-size m) max-mark-sections) (.clear m))
                            (let [a (js/Uint8Array. marks-length)] (set! (.-stamp a) stamp) (.set m key a) a)))]
        (when-not (== (.-stamp arr) stamp)
          (.fill arr 0)
          (set! (.-stamp arr) stamp))
        (set! (.-markKey st) key)
        (set! (.-markArrStamp st) stamp)
        (set! (.-markArr st) arr)
        arr))))

(defn mark-kind
  "How a recorded cell of state id stops sight, stored with its slice mark: 1 never, 2 for every line (a full cube),
  3 depends on the line (a thin block)."
  [^js raw ^js sight id]
  (cond (not (== 1 (aget sight id))) 1
        (thin-boxes raw sight id) 3
        :else 2))

(defn moved-why
  "Why a pass is due for the eye's change since the last pass started: :moved, :turned, or nil."
  [{:keys [opts]} ^js last ^js eye]
  (cond
    (or (not= (.-dimension last) (.-dimension eye))
        (>= (js/Math.hypot (- (.-x eye) (.-x last)) (- (.-y eye) (.-y last)) (- (.-z eye) (.-z last))) (:move-blocks opts)))
    :moved
    (>= (* (/ 180 js/Math.PI) (max (js/Math.abs (- (.-yaw eye) (.-yaw last))) (js/Math.abs (- (.-pitch eye) (.-pitch last)))))
        (:turn-deg opts))
    :turned))

(defn epoch-of
  "The raw world's change counter, or nil when it has none."
  [raw]
  (when-let [f (aget ^js raw "epoch")] (f)))

(defn idle-why
  "Why a still body (not moved or turned) looks again: :changed (after :idle-ms, the world around it changed since the
  last pass started), :still (after :still-ms), :idle (no raw-world change counter, every :idle-ms), or nil."
  [{:keys [raw opts] :as per} ^js last now]
  (let [waited (- now (.-at last))]
    (when (>= waited (:idle-ms opts))
      (cond
        (>= waited (:still-ms opts)) :still
        (nil? (.-epoch last)) :idle
        (or (not= (.-epoch last) (epoch-of raw))
            (not= (.-darken last) (darken-now raw))
            (not= (.-near last) (near-of per))) :changed))))

(defn start-why
  "The reason a pass starts now, or nil when none is due."
  [per ^js last ^js eye now force?]
  (cond
    force? (if (keyword? force?) force? :forced)
    (nil? last) :first
    :else (or (moved-why per (.-eye last) eye) (idle-why per last now))))

(defn count-why!
  "Adds n to the per-reason counter (:passes or :rays) of why, kept apart from the totals so a probe can see which
  reason dominates."
  [^js st why k n]
  (let [^js m (or (.-why st) (set! (.-why st) #js {}))
        ^js c (or (aget m (name why)) (aset m (name why) #js {:passes 0 :rays 0}))]
    (aset c (name k) (+ (aget c (name k)) n))))

(def tile-rays
  "Rays per tile of a pass: adjacent rays run back to back, so they share the sections they read."
  16)

(defn stride-of
  "A step near the golden share of n that is coprime to n: visiting tile t*stride mod n puts any prefix of a pass spread
  over the whole view, so a pass cut short by a turn still sampled all of its view."
  [n]
  (let [gcd (fn gcd [a b] (if (zero? b) a (recur b (mod a b))))]
    (loop [s (max 1 (js/Math.round (* 0.618034 n)))]
      (if (== 1 (gcd s n)) s (recur (inc s))))))

(defn start-pass!
  "Starts a pass at effort (0-1: the share of grid rays, see grid; the opts' :effort when nil), unless the sight table
  is not ready yet (the next step tries again)."
  [{:keys [raw opts] :as per} ^js st ^js eye now effort why]
  (when (sight-of per st)
    (set! (.-visible st) (table-now raw (:seeing-min opts)))
    (set! (.-lastStart st) #js {:at now :eye eye :epoch (epoch-of raw) :darken (darken-now raw) :near (near-of per)
                                :effort (or effort (:effort opts)) :done false})
    (let [{:keys [cols rows hx hy]} (grid opts (or effort (:effort opts)))
          jx ((:rand opts)) jy ((:rand opts))
          ground (aimed-rays opts hx hy (.-yaw eye) (.-pitch eye) jx jy)]
      (set! (.-pass st) #js {:next 0 :jx jx :jy jy :cols cols :rows rows :ground ground
                             :total (+ (* cols rows) (/ (.-length ground) 3)) :why why})
      (let [^js pass (.-pass st) tiles (js/Math.ceil (/ (* cols rows) tile-rays))]
        (set! (.-tiles pass) tiles)
        (set! (.-stride pass) (stride-of tiles))
        (set! (.-span pass) (+ (/ (.-length ground) 3) (* tiles tile-rays))))
      (count-why! st why :passes 1)
      (set! (.-passRays st) (.-span (.-pass st))))))

(def mark-gen-ms
  "How long a view generation (the mark stamp) lasts: a cell it marked keeps its recorded time that long."
  2000)

(defn renew-marks!
  "Gives the next slice a new mark stamp unless it can share the last slice's: same dimension, raw-world epoch, darkness
  and near, at most mark-gen-ms old. Then the cells marked so far are still recorded as they are in the world, and
  look-around, turn and walking passes do not read them again wherever the eye is (a mark says what the cell is, not
  who saw it). No epoch counter: a new stamp every slice."
  [per ^js st ^js eye now]
  (let [raw (:raw per) epoch (epoch-of raw) darken (darken-now raw) near (near-of per)
        ^js g (.-gen st)]
    (when-not (and g epoch (some? (.-markStamp st))
                   (== (.-epoch g) epoch) (== (.-darken g) darken) (== (.-near g) near)
                   (= (.-dim g) (.-dimension eye))
                   (< (- now (.-at g)) mark-gen-ms))
      (set! (.-markStamp st) (inc (or (.-markStamp st) 0)))
      (set! (.-gen st) #js {:epoch epoch :darken darken :near near :dim (.-dimension eye) :at now}))))

(def kernel-cache (atom nil)) ;; [root module]

(defn kernel
  "The sight kernel module (engine/js/sight-kernel.mjs), loaded from the engine root."
  []
  (let [r (offsets/root) [loaded-root module] @kernel-cache]
    (if (= r loaded-root)
      module
      (let [module ((createRequire (str r "/")) "./js/sight-kernel.mjs")]
        (reset! kernel-cache [r module])
        module))))

(defn sections-of
  "Where the kernel reads section arrays: the raw world's own, else lazy sections filled from its stateAt and lightAt,
  kept for the mark stamp (the cells read stay valid as long as the marks do)."
  [raw ^js st]
  (if (.-sectionStates ^js raw)
    raw
    (let [^js lazy (.-lazy st)]
      (if (and lazy (== (.-stamp lazy) (.-markStamp st)))
        (.-sections lazy)
        (let [sections (.lazySections ^js (kernel))]
          (set! (.-lazy st) #js {:stamp (.-markStamp st) :sections sections})
          sections)))))

(def ^:private dirs-buffer (atom (js/Float64Array. 0)))

(defn slice-dirs
  "The unit directions of rays [from, to) of the pass as a Float64Array of triples (NaN x: a ground ray outside the cone now).
  The array is a shared buffer, longer than the slice maybe, valid until the next call."
  [{:keys [grid]} ^js pass ^js eye from to]
  (let [yaw (.-yaw eye)
        need (* 3 (- to from))
        out (if (>= (.-length ^js @dirs-buffer) need) @dirs-buffer (reset! dirs-buffer (js/Float64Array. need)))]
    (.sliceDirs ^js (kernel) out from to
                #js {:b (basis yaw (.-pitch eye)) :hx (:hx grid) :hy (:hy grid) :cols (.-cols pass) :rows (.-rows pass)
                     :jx (.-jx pass) :jy (.-jy pass) :ground (.-ground pass) :tiles (.-tiles pass) :stride (.-stride pass)
                     :tileRays tile-rays :fx (- (js/Math.sin yaw)) :fz (- (js/Math.cos yaw))
                     :rx (js/Math.cos yaw) :rz (- (js/Math.sin yaw))})
    out))

(defn cast-slice!
  "Casts rays [from, to) of the pass from the eye's current place and look: the aimed rows first (along the
  eye's yaw now, and only those still inside the view cone), then the grid in tiles. Returns the cells entered."
  [{:keys [raw opts] :as per} ^js st ^js pass ^js eye from to now]
  (let [^js sight (.-sight st)
        store (store-of st (.-dimension eye))]
    (set! (.-near st) (near-of per))
    (renew-marks! per st eye now)
    (.castRays ^js (kernel)
               #js {:raw raw :sections (sections-of raw st) :sight sight :visible (.-visible st)
                    :radius (:radius opts) :near (.-near st)
                    :marksOf (fn [cx cy cz] (marks-of st (section-key cx cy cz) (.-MARKS_LENGTH ^js (kernel))))
                    :record (fn [x y z id] (record! st store x y z id now))
                    :markKind (fn [id] (mark-kind raw sight id))
                    :thin (fn [id x y z ox oy oz ex ey ez] (blocks-line? raw sight id x y z ox oy oz ex ey ez))}
               (.-x eye) (.-y eye) (.-z eye) (slice-dirs per pass eye from to) (- to from))))

(defn step!
  "One slice of the sight pass (one game tick's share). Starts a new pass when none runs and the eye moved, turned or
  idled long enough. While offline (no eye) it casts nothing and drops the pass in flight."
  ([per] (step! per false nil))
  ([{:keys [raw opts st] :as per} force? effort]
   (let [^js st st
         ^js eye (.eye ^js raw)
         now ((:now opts))
         started (js/performance.now)]
     (when eye
       (set! (.-dim st) (.-dimension eye))
       (new-stamp! st)
       (when-let [why (and (nil? (.-pass st)) (start-why per (.-lastStart st) eye now force?))]
         (start-pass! per st eye now effort why))
       (when-let [^js pass (.-pass st)]
         (let [total (.-span pass)
               from (.-next pass)
               to (min total (+ from (js/Math.ceil (/ total (steps-per-pass per)))))
               cells (cast-slice! per st pass eye from to now)]
           (set! (.-next pass) to)
           (set! (.-rays st) (+ (.-rays st) (- to from)))
           (count-why! st (.-why pass) :rays (- to from))
           (set! (.-cells st) (+ (.-cells st) cells))
           (when (>= to total)
             (set! (.-pass st) nil)
             (set! (.-done (.-lastStart st)) true)
             (set! (.-passes st) (inc (.-passes st))))))
       (let [ms (- (js/performance.now) started)]
         (set! (.-steps st) (inc (.-steps st)))
         (set! (.-stepMs st) (+ (.-stepMs st) ms))
         (set! (.-stepMsMax st) (max (.-stepMsMax st) ms))))
     ;; No eye: the body is offline. The pass in flight belongs to the old world, so drop it.
     ;; The first step after the next spawn starts a fresh one. Memory stays as it is.
     (when-not eye
       (set! (.-pass st) nil)
       (set! (.-lastStart st) nil)))))

(defn repeat-pass?
  "Whether a pass now would see what the last one (finished) did: the same eye spot and look, raw-world epoch, darkness
  and near, at the same effort or lower, still inside its view generation (see renew-marks!). Then the cells it would
  record are recorded already, at most mark-gen-ms old."
  [per ^js last ^js eye now effort]
  (let [raw (:raw per) ^js e (some-> last .-eye)]
    (boolean
     (and last (.-done last) (some? (.-epoch last))
          (<= (or effort (:effort (:opts per))) (.-effort last))
          (< (- now (.-at last)) mark-gen-ms)
          (== (.-epoch last) (epoch-of raw)) (== (.-darken last) (darken-now raw)) (== (.-near last) (near-of per))
          (= (.-dimension e) (.-dimension eye))
          (== (.-x e) (.-x eye)) (== (.-y e) (.-y eye)) (== (.-z e) (.-z eye))
          (== (.-yaw e) (.-yaw eye)) (== (.-pitch e) (.-pitch eye))))))

(defn pass!
  "A whole sight pass from the eye as it is now (tests, and a forced look), at {:effort} (0-1, default the opts' :effort).
  A pass in flight is dropped, not finished: this one covers the whole cone. {:why kw} names the caller in the per-reason counts. With {:reuse? true} a pass that would see
  what the last finished one did (repeat-pass?) is not cast; returns true then."
  ([per] (pass! per nil))
  ([{:keys [st] :as per} {:keys [effort reuse? why]}]
   (let [^js st st ^js eye (.eye ^js (:raw per))]
     (set! (.-pass st) nil)
     (cond
       (not eye) nil
       (and reuse? (repeat-pass? per (.-lastStart st) eye ((:now (:opts per))) effort)) (do (count-why! st :repeated :passes 1) true)
       :else (do (step! per (or why true) effort)
                 (while (.-pass st) (step! per)))))))

;; ---- block changes and touch

(defn line-ctx
  "What the kernel's cellLight reads: the raw world, the sight table, the raw world as the section source when it has
  section arrays, and the thin-block test."
  [^js raw ^js sight]
  #js {:raw raw :sight sight :src (when (.-sectionStates raw) raw)
       :blocks (fn [id x y z ox oy oz tx ty tz] (blocks-line? raw sight id x y z ox oy oz tx ty tz))})

(defn visible-now?
  "Whether the cell (x y z), now holding state id, is in view: its centre inside the cone and the radius, a point of it
  (the centre first, then inset points on the faces towards the eye) with nothing blocking sight on the way from the
  eye (kernel cellLight), and lit (or within near)."
  [{:keys [raw opts grid] :as per} ^js st ^js eye x y z id]
  (let [ox (.-x eye) oy (.-y eye) oz (.-z eye)
        vx (- (+ x 0.5) ox) vy (- (+ y 0.5) oy) vz (- (+ z 0.5) oz)
        dist (js/Math.hypot vx vy vz)
        ^js sight (sight-of per st)
        ^js visible (or (.-visible st) (set! (.-visible st) (table-now raw (:seeing-min opts))))]
    (and sight
         (<= dist (:radius opts))
         (in-cone? (basis (.-yaw eye) (.-pitch eye)) (:hx grid) (:hy grid) vx vy vz)
         (let [prev (.cellLight ^js (kernel) (line-ctx raw sight) ox oy oz x y z)]
           (when (>= prev 0)
             (let [light (.lightAt ^js raw x y z)
                   shown (if (== 1 (aget sight id)) (max-light light prev) light)]
               (or (== 1 (aget visible shown)) (<= dist (near-of per)))))))))

(defn line-clear?
  "Whether the eye at (ox oy oz) has a clear line to the point (tx ty tz): no cell strictly between the eye's cell and
  the point's cell blocks the line (blocks-line?) or is unloaded. No cone and no light rule:
  this answers whether a thing there could be seen by turning to it."
  [^js raw ^js table ox oy oz tx ty tz]
  (sight/line-clear ox oy oz tx ty tz
                    (fn [x y z] (let [id (.stateAt raw x y z)] (or (< id 0) (blocks-line? raw table id x y z ox oy oz tx ty tz))))))

(defn on-change!
  "A block changed in the raw world: memory takes the new state only if the body sees the cell now."
  [{:keys [raw opts st] :as per} x y z id]
  (let [^js st st ^js eye (.eye ^js raw)]
    (when (and eye (visible-now? per st eye x y z id))
      (new-stamp! st)
      (record! st (store-of st (.-dimension eye)) x y z id ((:now opts))))))

(defn start-listening!
  "Follow the raw world's block changes (on-change!). Returns the unsubscribe."
  [{:keys [raw st] :as per}]
  (let [off (.onBlockChange ^js raw (fn [x y z id] (on-change! per x y z id)))]
    (set! (.-unsubscribe ^js st) off)
    off))

(defn touch!
  "The body touched the cell (dug, placed, clicked, stood in it): memory takes its true state."
  [{:keys [raw opts st]} [x y z]]
  (let [^js st st
        id (.stateAt ^js raw x y z)]
    (when (>= id 0)
      (new-stamp! st)
      (record! st (store-of st (.-dim st)) x y z id ((:now opts))))))

(defn glance!
  "One ray instead of a pass: the state id of the cell (x y z) when the body sees it now (visible-now?: view cone, radius,
  clear line, lit or within near), which memory takes. Nil when it does not, or the cell is unloaded, or offline."
  [{:keys [raw opts st] :as per} [x y z]]
  (let [^js st st ^js eye (.eye ^js raw)]
    (when eye
      (let [id (.stateAt ^js raw x y z)]
        (when (and (>= id 0) (visible-now? per st eye x y z id))
          (new-stamp! st)
          (record! st (store-of st (.-dimension eye)) x y z id ((:now opts)))
          id)))))

(def hazard-names #{"lava" "fire" "soul_fire" "magma_block" "powder_snow"})

(def max-targets
  "Candidates a targeted look checks one by one; past this a full pass is cheaper."
  400)

(defn candidates
  "The cells within radius of the eye and inside its view cone whose block name satisfies match?, and the given cells
  (any block), as a flat array [x y z id ...], or nil when more than max-targets lie there. Read from the raw world, so
  only cells that pass the sight rules afterwards may be recorded."
  [{:keys [raw grid] :as per} ^js st ^js eye match? radius cells]
  (let [^js sight (sight-of per st)
        kinds (js/Uint8Array. (.-length sight))
        yes? (fn [id] (and (< id (.-length kinds))
                           (let [k (aget kinds id)]
                             (if (zero? k)
                               (let [hit (boolean (match? (.-name ^js (.stateInfo ^js raw id))))]
                                 (aset kinds id (if hit 2 1))
                                 hit)
                               (== k 2)))))
        ox (.-x eye) oy (.-y eye) oz (.-z eye)
        b (basis (.-yaw eye) (.-pitch eye))
        out #js []
        take! (fn [x y z id wanted?]
                (when (or wanted? (yes? id))
                  (let [vx (- (+ x 0.5) ox) vy (- (+ y 0.5) oy) vz (- (+ z 0.5) oz)]
                    (when (and (or wanted? (<= (js/Math.hypot vx vy vz) radius)) (in-cone? b (:hx grid) (:hy grid) vx vy vz))
                      (.push out x y z id)))))
        x0 (js/Math.floor (- ox radius)) x1 (js/Math.floor (+ ox radius))
        y0 (js/Math.floor (- oy radius)) y1 (js/Math.floor (+ oy radius))
        z0 (js/Math.floor (- oz radius)) z1 (js/Math.floor (+ oz radius))]
    (doseq [[x y z] cells
            :let [id (.stateAt ^js raw x y z)]
            :when (pos? id)]
      (take! x y z id true))
    (if (.-sectionStates ^js raw)
      (let [^js rg (.sectionRange ^js raw)
            min-y (.-minY rg)]
        (doseq [cx (range (bit-shift-right x0 4) (inc (bit-shift-right x1 4)))
                cz (range (bit-shift-right z0 4) (inc (bit-shift-right z1 4)))
                s (range (max 0 (bit-shift-right (- y0 min-y) 4)) (min (.-sections rg) (inc (bit-shift-right (- y1 min-y) 4))))
                :let [^js ids (.sectionStates ^js raw cx s cz)]
                :when ids]
          (dotimes [i 4096]
            (let [id (aget ids i)]
              (when (pos? id)
                (take! (+ (* 16 cx) (bit-and i 15)) (+ min-y (* 16 s) (bit-shift-right i 8)) (+ (* 16 cz) (bit-and (bit-shift-right i 4) 15)) id false))))))
      (doseq [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))
              :let [id (.stateAt ^js raw x y z)]
              :when (pos? id)]
        (take! x y z id false)))
    (when (<= (/ (.-length out) 4) max-targets) out)))

(defn look-for!
  "A look that wants only some blocks: ray-checks the cells of the {:names set or :match fn} kinds and the {:cells
  [[x y z]...]} (and always the hazards lava, fire, magma and powder snow) within {:radius} (default 16) and in view
  now with visible-now?, and records those it sees. A full pass (pass!) when the candidates are too many or the raw
  world is not ready. {:why} as pass!."
  [{:keys [raw opts st] :as per} {:keys [names match radius cells effort why]}]
  (let [^js st st ^js eye (.eye ^js raw)
        match? (fn [n] (or (contains? hazard-names n) (and names (contains? names n)) (and match (match n))))
        cands (when (and eye (sight-of per st)) (candidates per st eye match? (or radius 16) cells))]
    (if-not cands
      (pass! per {:effort effort :why (keyword (str (name (or why :forced)) "-full")) :reuse? true})
      (let [n (/ (.-length cands) 4) now ((:now opts))]
        (new-stamp! st)
        (dotimes [i n]
          (let [x (aget cands (* 4 i)) y (aget cands (+ 1 (* 4 i))) z (aget cands (+ 2 (* 4 i))) id (aget cands (+ 3 (* 4 i)))]
            (when (visible-now? per st eye x y z id)
              (record! st (store-of st (.-dimension eye)) x y z id now))))
        (count-why! st :look-for :passes 1)
        (count-why! st :look-for :rays n)
        (set! (.-rays st) (+ (.-rays st) n))
        true))))

(def feel-margin 0.1)

(defn in-reach?
  "Whether the cell [x y z] could be touched by a body with its feet at (fx fy fz), whatever its collision shape: over the
  hitbox column (grown by feel-margin) or at most one cell below the feet. Cheap, so callers test it before reading state."
  [fx fy fz [x y z]]
  (let [r (+ game/hitbox-half feel-margin)]
    (and (< x (+ fx r)) (> (inc x) (- fx r))
         (< z (+ fz r)) (> (inc z) (- fz r))
         (< y (+ fy game/body-height feel-margin))
         (>= y (dec (js/Math.floor fy))))))

(defn touches?
  "Whether a body with its feet at (fx fy fz) touches the cell [x y z] whose collision shape reaches `top` above its floor:
  it overlaps the body's hitbox (0.6 wide, 1.8 tall) grown by feel-margin, or is a tall support (a fence) under the feet
  whose top reaches the feet. The one rule for perception's feel and the jobs' feel."
  [fx fy fz [x y z :as pos] top]
  (and (in-reach? fx fy fz pos)
       (let [over? (fn [c lo hi] (and (< c hi) (> (inc c) lo)))]
         (or (over? y (- fy feel-margin) (+ fy game/body-height feel-margin))
             (and (< y fy) (>= (+ y top) (- fy feel-margin)))))))

(defn felt?
  "touches? for the body whose eye is `eye` (feet at the eye less eye-height)."
  [^js eye pos top]
  (touches? (.-x eye) (- (.-y eye) game/eye-height) (.-z eye) pos top))

(defn feel!
  "The state id of a cell the body touches (felt?), in any light; memory takes it. Nil for any other cell, an unloaded
  one, or offline."
  [{:keys [raw opts st]} [x y z :as pos]]
  (let [^js st st ^js eye (.eye ^js raw)]
    (when (and eye (in-reach? (.-x eye) (- (.-y eye) game/eye-height) (.-z eye) pos))
      (let [id (.stateAt ^js raw x y z)]
        (when (and (>= id 0) (felt? eye pos (.-top ^js (.stateInfo ^js raw id))))
          (new-stamp! st)
          (record! st (store-of st (.-dimension eye)) x y z id ((:now opts)))
          id)))))

(def touching-primitives
  "The primitives whose {pos} argument is a cell the body changes or clicks."
  #{"dig" "place" "jumpPlace" "useOn"})

(defn touching
  "The primitive f (called on p) that, once it settles, whatever its outcome, lets memory take the true state of the cell
  in its pos argument."
  [per ^js p ^js f]
  (fn [token ^js a]
    (let [touch (fn []
                  (when-let [^js pos (some-> a .-pos)]
                    (when (every? number? [(.-x pos) (.-y pos) (.-z pos)])
                      (touch! per (mapv #(js/Math.floor %) [(.-x pos) (.-y pos) (.-z pos)])))))]
      (-> (.call f p token a)
          (.then (fn [r] (touch) r)
                 (fn [e] (touch) (throw e)))))))
