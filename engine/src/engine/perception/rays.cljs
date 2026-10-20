(ns engine.perception.rays
  "Sight: the view cone, the sight pass that casts rays from the eye into block memory, and keeping memory current
  after block changes and touches."
  (:require [engine.game :as game]
            [engine.sight :as sight]
            [engine.perception.light :refer [darken-now max-light table-now]]
            [engine.perception.store :refer [cell-index new-stamp! record! section-key store-of]]))

;; ---- the view cone

(defn grid
  "{:cols :rows :hx :hy}: the ray grid over the view plane, tangent half-extents hx (horizontal) and hy. At effort e
  (0-1, default 1) the grid keeps the share effort-min + (1 - effort-min) e of the rays of a ray-deg grid."
  ([opts] (grid opts 1))
  ([{:keys [fov aspect ray-deg effort-min]} effort]
   (let [hy (js/Math.tan (/ (* fov js/Math.PI) 360))
         hx (* hy aspect)
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
  "The rays of the last pass started (grid and ground rows), else of a full-effort grid."
  [{:keys [grid st]}]
  (or (.-passRays ^js st) (* (:cols grid) (:rows grid))))

(defn ground-rays
  "The ground rows of a pass from an eye at yaw and pitch: for each ground distance d from ground-from (plus jy) to
  ground-to, a row of rays ground-across blocks apart (offset by jx) at the ground eye-height below the eye, kept when
  inside the view cone. A flat [d l ...] array (forward, sideways, in blocks): far ground at a grazing angle falls
  between grid rays, a row per block of distance does not."
  [{:keys [ground-from ground-to ground-across]} hx hy yaw pitch jx jy]
  (let [b (basis yaw pitch)
        fx (- (js/Math.sin yaw)) fz (- (js/Math.cos yaw)) rx (js/Math.cos yaw) rz (- (js/Math.sin yaw))
        out #js []]
    (loop [d (+ ground-from jy)]
      (when (< d ground-to)
        (let [k (js/Math.ceil (/ (* hx (+ d game/eye-height)) ground-across))]
          (doseq [i (range (- k) (inc k))]
            (let [l (* ground-across (+ i jx))]
              (when (in-cone? b hx hy (+ (* d fx) (* l rx)) (- game/eye-height) (+ (* d fz) (* l rz)))
                (.push out d l)))))
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
  "The slice marks of section `key`: a Uint32Array of the slice stamp that last recorded each cell, kept in st.marks
  (dropped past max-mark-sections) with the last one asked for cached."
  [^js st key]
  (if (== key (.-markKey st))
    (.-markArr st)
    (let [^js m (or (.-marks st) (set! (.-marks st) (js/Map.)))
          arr (or (.get m key)
                  (do (when (>= (.-size m) max-mark-sections) (.clear m))
                      (let [a (js/Uint32Array. 4096)] (.set m key a) a)))]
      (set! (.-markKey st) key)
      (set! (.-markArr st) arr)
      arr)))

(defn cast!
  "One ray from (ox oy oz) along the unit vector (dx dy dz). Returns the number of cells entered.
  A cell an earlier ray of this slice recorded (st.markStamp) is not read or recorded again: recording it again would
  write the same state id and time. Cells not recorded are never marked, so another ray may still record them. The
  light of the previous cell is read only when a blocking cell needs it."
  [{:keys [raw opts]} ^js st store ox oy oz dx dy dz now]
  (let [radius (:radius opts) near (.-near st)
        stamp (.-markStamp st)
        ^js sight (.-sight st) ^js visible (.-visible st)
        sx (if (pos? dx) 1 -1) sy (if (pos? dy) 1 -1) sz (if (pos? dz) 1 -1)
        tdx (if (zero? dx) js/Infinity (js/Math.abs (/ 1 dx)))
        tdy (if (zero? dy) js/Infinity (js/Math.abs (/ 1 dy)))
        tdz (if (zero? dz) js/Infinity (js/Math.abs (/ 1 dz)))
        x0 (js/Math.floor ox) y0 (js/Math.floor oy) z0 (js/Math.floor oz)
        ex (+ ox (* dx radius)) ey (+ oy (* dy radius)) ez (+ oz (* dz radius))
        stops? (fn [id x y z] (blocks-line? raw sight id x y z ox oy oz ex ey ez))
        first-id (.stateAt ^js raw x0 y0 z0)
        first-light (.lightAt ^js raw x0 y0 z0)]
    (when (>= first-id 0)
      (let [^js marks (marks-of st (section-key (bit-shift-right x0 4) (bit-shift-right y0 4) (bit-shift-right z0 4)))
            i (cell-index x0 y0 z0)]
        (when-not (== stamp (aget marks i))
          (record! st store x0 y0 z0 first-id now)
          (aset marks i stamp))))
    (if (or (< first-id 0) (stops? first-id x0 y0 z0))
      1
      (loop [x x0 y y0 z z0
             tmx (if (zero? dx) js/Infinity (* tdx (if (pos? dx) (- (inc x0) ox) (- ox x0))))
             tmy (if (zero? dy) js/Infinity (* tdy (if (pos? dy) (- (inc y0) oy) (- oy y0))))
             tmz (if (zero? dz) js/Infinity (* tdz (if (pos? dz) (- (inc z0) oz) (- oz z0))))
             prev first-light
             n 1]
        (let [t (js/Math.min tmx tmy tmz)]
          (if (> t radius)
            n
            (let [ax (== t tmx) ay (and (not ax) (== t tmy)) az (and (not ax) (not ay))
                  px x py y pz z
                  x (if ax (+ x sx) x) y (if ay (+ y sy) y) z (if az (+ z sz) z)
                  id (.stateAt ^js raw x y z)]
              (if (< id 0)
                n
                (let [^js marks (marks-of st (section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4)))
                      i (cell-index x y z)
                      marked? (== stamp (aget marks i))
                      blocks? (stops? id x y z)]
                  (if marked?
                    (if blocks?
                      (inc n)
                      (recur x y z
                             (if ax (+ tmx tdx) tmx) (if ay (+ tmy tdy) tmy) (if az (+ tmz tdz) tmz)
                             -1 (inc n)))
                    (let [light (.lightAt ^js raw x y z)
                          shown (if blocks?
                                  (max-light light (if (neg? prev) (.lightAt ^js raw px py pz) prev))
                                  light)]
                      (when (or (== 1 (aget visible shown)) (<= t near))
                        (record! st store x y z id now)
                        (aset marks i stamp))
                      (if blocks?
                        (inc n)
                        (recur x y z
                               (if ax (+ tmx tdx) tmx) (if ay (+ tmy tdy) tmy) (if az (+ tmz tdz) tmz)
                               light (inc n))))))))))))))

(defn moved? [{:keys [opts]} ^js last ^js eye]
  (or (nil? last)
      (not= (.-dimension last) (.-dimension eye))
      (>= (js/Math.hypot (- (.-x eye) (.-x last)) (- (.-y eye) (.-y last)) (- (.-z eye) (.-z last))) (:move-blocks opts))
      (>= (* (/ 180 js/Math.PI) (max (js/Math.abs (- (.-yaw eye) (.-yaw last))) (js/Math.abs (- (.-pitch eye) (.-pitch last)))))
          (:turn-deg opts))))

(defn epoch-of
  "The raw world's change counter, or nil when it has none."
  [raw]
  (when-let [f (aget ^js raw "epoch")] (f)))

(defn idle-due?
  "A still body (not moved or turned) looks again after :idle-ms if the world around it changed since the last pass
  started, else after :still-ms. Without a raw-world change counter every :idle-ms."
  [{:keys [raw opts] :as per} ^js last now]
  (let [waited (- now (.-at last))]
    (and (>= waited (:idle-ms opts))
         (or (>= waited (:still-ms opts))
             (nil? (.-epoch last))
             (not= (.-epoch last) (epoch-of raw))
             (not= (.-darken last) (darken-now raw))
             (not= (.-near last) (near-of per))))))

(defn start-pass!
  "Starts a pass at effort (0-1: the share of grid rays, see grid; the opts' :effort when nil), unless the sight table
  is not ready yet (the next step tries again)."
  [{:keys [raw opts] :as per} ^js st ^js eye now effort]
  (when (sight-of per st)
    (set! (.-visible st) (table-now raw (:seeing-min opts)))
    (set! (.-lastStart st) #js {:at now :eye eye :epoch (epoch-of raw) :darken (darken-now raw) :near (near-of per)})
    (let [{:keys [cols rows hx hy]} (grid opts (or effort (:effort opts)))
          jx ((:rand opts)) jy ((:rand opts))
          ground (ground-rays opts hx hy (.-yaw eye) (.-pitch eye) jx jy)]
      (set! (.-pass st) #js {:next 0 :jx jx :jy jy :cols cols :rows rows :ground ground
                             :total (+ (* cols rows) (/ (.-length ground) 2))})
      (set! (.-passRays st) (.-total (.-pass st))))))

(defn cast-slice!
  "Casts rays [from, to) of the pass from the eye's current place and look: the grid, then the ground rows (along the
  eye's yaw now, and only those still inside the view cone)."
  [{:keys [grid] :as per} ^js st ^js pass ^js eye from to now]
  (let [{:keys [hx hy]} grid
        cols (.-cols pass) n-grid (* cols (.-rows pass))
        ^js ground (.-ground pass)
        yaw (.-yaw eye)
        b (basis yaw (.-pitch eye))
        fx (- (js/Math.sin yaw)) fz (- (js/Math.cos yaw)) rx (js/Math.cos yaw) rz (- (js/Math.sin yaw))
        store (store-of st (.-dimension eye))
        ox (.-x eye) oy (.-y eye) oz (.-z eye)
        du (/ (* 2 hx) cols) dv (/ (* 2 hy) (.-rows pass))
        cast-dir (fn [dx dy dz]
                   (let [len (js/Math.hypot dx dy dz)]
                     (cast! per st store ox oy oz (/ dx len) (/ dy len) (/ dz len) now)))]
    (set! (.-near st) (near-of per))
    (set! (.-markStamp st) (inc (or (.-markStamp st) 0)))
    (loop [k from cells 0]
      (cond
        (>= k to) cells
        (< k n-grid)
        (let [a (+ (- hx) (* du (+ (mod k cols) (.-jx pass))))
              v (+ (- hy) (* dv (+ (js/Math.floor (/ k cols)) (.-jy pass))))]
          (recur (inc k) (+ cells (cast-dir (+ (aget b 0) (* a (aget b 3)) (* v (aget b 6)))
                                            (+ (aget b 1) (* v (aget b 7)))
                                            (+ (aget b 2) (* a (aget b 5)) (* v (aget b 8)))))))
        :else
        (let [i (* 2 (- k n-grid)) d (aget ground i) l (aget ground (inc i))
              dx (+ (* d fx) (* l rx)) dy (- game/eye-height) dz (+ (* d fz) (* l rz))]
          (recur (inc k) (if (in-cone? b hx hy dx dy dz) (+ cells (cast-dir dx dy dz)) cells)))))))

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
       (when (and (nil? (.-pass st))
                  (or force?
                      (nil? (.-lastStart st))
                      (idle-due? per (.-lastStart st) now)
                      (moved? per (.-eye (.-lastStart st)) eye)))
         (start-pass! per st eye now effort))
       (when-let [^js pass (.-pass st)]
         (let [total (.-total pass)
               from (.-next pass)
               to (min total (+ from (js/Math.ceil (/ total (steps-per-pass per)))))
               cells (cast-slice! per st pass eye from to now)]
           (set! (.-next pass) to)
           (set! (.-rays st) (+ (.-rays st) (- to from)))
           (set! (.-cells st) (+ (.-cells st) cells))
           (when (>= to total)
             (set! (.-pass st) nil)
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

(defn pass!
  "A whole sight pass from the eye as it is now (tests, and a forced look), at {:effort} (0-1, default the opts' :effort).
  A pass in flight is dropped, not finished: this one covers the whole cone."
  ([per] (pass! per nil))
  ([{:keys [st] :as per} {:keys [effort]}]
  (let [^js st st]
    (set! (.-pass st) nil)
    (when (.eye ^js (:raw per))
      (step! per true effort)
      (while (.-pass st) (step! per))))))

;; ---- block changes and touch

(defn walk-cells
  "Walk the cells from the eye (ox oy oz) towards the point (tx ty tz), up to (excluding) the point's own cell. Nil when a
  cell on the way blocks the line (blocks-line?), is unloaded, or the walk overruns its budget;
  else the light of the last cell before the point's cell (the eye's own cell when they are the same)."
  [^js raw ^js table ox oy oz tx ty tz]
  (let [vx (- tx ox) vy (- ty oy) vz (- tz oz)
        d (js/Math.hypot vx vy vz)
        x1 (js/Math.floor tx) y1 (js/Math.floor ty) z1 (js/Math.floor tz)
        x0 (js/Math.floor ox) y0 (js/Math.floor oy) z0 (js/Math.floor oz)
        dx (if (zero? d) 0 (/ vx d)) dy (if (zero? d) 0 (/ vy d)) dz (if (zero? d) 0 (/ vz d))
        sx (if (pos? dx) 1 -1) sy (if (pos? dy) 1 -1) sz (if (pos? dz) 1 -1)
        tdx (if (zero? dx) js/Infinity (js/Math.abs (/ 1 dx)))
        tdy (if (zero? dy) js/Infinity (js/Math.abs (/ 1 dy)))
        tdz (if (zero? dz) js/Infinity (js/Math.abs (/ 1 dz)))]
    (loop [cx x0 cy y0 cz z0
           tmx (if (zero? dx) js/Infinity (* tdx (if (pos? dx) (- (inc x0) ox) (- ox x0))))
           tmy (if (zero? dy) js/Infinity (* tdy (if (pos? dy) (- (inc y0) oy) (- oy y0))))
           tmz (if (zero? dz) js/Infinity (* tdz (if (pos? dz) (- (inc z0) oz) (- oz z0))))
           prev (or (.lightAt raw x0 y0 z0) 0)
           budget (+ 3 (js/Math.abs (- x1 x0)) (js/Math.abs (- y1 y0)) (js/Math.abs (- z1 z0)))]
      (let [t (js/Math.min tmx tmy tmz)]
        (cond
          (or (and (== cx x1) (== cy y1) (== cz z1)) (> t d)) prev
          (<= budget 0) nil
          :else
          (let [ax (== t tmx) ay (and (not ax) (== t tmy)) az (and (not ax) (not ay))
                nx (if ax (+ cx sx) cx) ny (if ay (+ cy sy) cy) nz (if az (+ cz sz) cz)
                target? (and (== nx x1) (== ny y1) (== nz z1))
                here (.stateAt raw nx ny nz)]
            (cond
              (and (not target?) (or (< here 0) (blocks-line? raw table here nx ny nz ox oy oz tx ty tz))) nil
              target? prev
              :else (recur nx ny nz
                           (if ax (+ tmx tdx) tmx) (if ay (+ tmy tdy) tmy) (if az (+ tmz tdz) tmz)
                           (or (.lightAt raw nx ny nz) 0) (dec budget)))))))))

(def face-inset 0.15)

(defn face-points
  "Points just inside the cell (x y z) on each face the eye (ox oy oz) is outside of: four inset corners per face, so a
  block counts as seen when any such point is, as a player sees it by any visible face."
  [ox oy oz x y z]
  (let [lo face-inset hi (- 1 face-inset)
        side (fn [o c] (cond (< o c) -1 (> o (inc c)) 1 :else 0))
        sx (side ox x) sy (side oy y) sz (side oz z)
        near (fn [s c] (if (neg? s) (+ c lo) (+ c hi)))]
    (concat
     (when-not (zero? sx) (for [b [lo hi] c [lo hi]] [(near sx x) (+ y b) (+ z c)]))
     (when-not (zero? sy) (for [a [lo hi] c [lo hi]] [(+ x a) (near sy y) (+ z c)]))
     (when-not (zero? sz) (for [a [lo hi] b [lo hi]] [(+ x a) (+ y b) (near sz z)])))))

(defn visible-now?
  "Whether the cell (x y z), now holding state id, is in view: its centre inside the cone and the radius, a point of it
  (the centre first, then inset points on the faces towards the eye) with nothing blocking sight on the way from the
  eye, and lit (or within near)."
  [{:keys [raw opts grid] :as per} ^js st ^js eye x y z id]
  (let [ox (.-x eye) oy (.-y eye) oz (.-z eye)
        vx (- (+ x 0.5) ox) vy (- (+ y 0.5) oy) vz (- (+ z 0.5) oz)
        dist (js/Math.hypot vx vy vz)
        ^js sight (sight-of per st)
        ^js visible (or (.-visible st) (set! (.-visible st) (table-now raw (:seeing-min opts))))]
    (and sight
         (<= dist (:radius opts))
         (in-cone? (basis (.-yaw eye) (.-pitch eye)) (:hx grid) (:hy grid) vx vy vz)
         (when-let [prev (or (walk-cells raw sight ox oy oz (+ x 0.5) (+ y 0.5) (+ z 0.5))
                             (some (fn [[tx ty tz]] (walk-cells raw sight ox oy oz tx ty tz)) (face-points ox oy oz x y z)))]
           (let [light (.lightAt ^js raw x y z)
                 shown (if (== 1 (aget sight id)) (max-light light prev) light)]
             (or (== 1 (aget visible shown)) (<= dist (near-of per))))))))

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
