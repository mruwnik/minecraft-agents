(ns engine.perception.rays
  "Sight: the view cone, the sight pass that casts rays from the eye into block memory, and keeping memory current
  after block changes and touches."
  (:require [engine.perception.light :refer [darken-now max-light table-now]]
            [engine.perception.store :refer [eye-height new-stamp! record! store-of]]))

;; ---- the view cone

(defn grid
  "{:cols :rows :hx :hy}: the ray grid over the view plane, tangent half-extents hx (horizontal) and hy."
  [{:keys [fov aspect ray-deg]}]
  (let [hy (js/Math.tan (/ (* fov js/Math.PI) 360))
        hx (* hy aspect)
        spacing (js/Math.tan (/ (* ray-deg js/Math.PI) 180))]
    {:hx hx :hy hy :cols (js/Math.ceil (/ (* 2 hx) spacing)) :rows (js/Math.ceil (/ (* 2 hy) spacing))}))

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
(defn rays-per-pass [{:keys [grid]}] (* (:cols grid) (:rows grid)))

(defn sight-of
  "The sight table, or nil while the raw world has none yet (no registry): an empty table is never kept."
  [{:keys [raw]} ^js st]
  (or (.-sight st)
      (let [^js table (.sightTable ^js raw)]
        (when (and table (pos? (.-length table)))
          (set! (.-sight st) table)))))

;; ---- the sight pass

(defn near-of
  "How close a dark cell must be to count as seen: :near-torch with a torch in either hand, else :near."
  [{:keys [raw opts]}]
  (let [name-in (fn [k] (when-let [f (aget ^js raw k)] (f)))]
    (if (some #{"torch" "soul_torch"} [(name-in "offHand") (name-in "heldItem")]) (max (:near opts) (:near-torch opts)) (:near opts))))

(defn cast!
  "One ray from (ox oy oz) along the unit vector (dx dy dz). Returns the number of cells entered."
  [{:keys [raw opts]} ^js st store ox oy oz dx dy dz now]
  (let [radius (:radius opts) near (.-near st)
        ^js sight (.-sight st) ^js visible (.-visible st)
        sx (if (pos? dx) 1 -1) sy (if (pos? dy) 1 -1) sz (if (pos? dz) 1 -1)
        tdx (if (zero? dx) js/Infinity (js/Math.abs (/ 1 dx)))
        tdy (if (zero? dy) js/Infinity (js/Math.abs (/ 1 dy)))
        tdz (if (zero? dz) js/Infinity (js/Math.abs (/ 1 dz)))
        x0 (js/Math.floor ox) y0 (js/Math.floor oy) z0 (js/Math.floor oz)
        first-id (.stateAt ^js raw x0 y0 z0)
        first-light (.lightAt ^js raw x0 y0 z0)]
    (when (>= first-id 0) (record! st store x0 y0 z0 first-id now))
    (if (or (< first-id 0) (== 1 (aget sight first-id)))
      1
      (loop [x x0 y y0 z z0
             tmx (if (zero? dx) js/Infinity (* tdx (if (pos? dx) (- (inc x0) ox) (- ox x0))))
             tmy (if (zero? dy) js/Infinity (* tdy (if (pos? dy) (- (inc y0) oy) (- oy y0))))
             tmz (if (zero? dz) js/Infinity (* tdz (if (pos? dz) (- (inc z0) oz) (- oz z0))))
             prev first-light
             n 1]
        (let [t (min tmx tmy tmz)]
          (if (> t radius)
            n
            (let [ax (== t tmx) ay (and (not ax) (== t tmy)) az (and (not ax) (not ay))
                  x (if ax (+ x sx) x) y (if ay (+ y sy) y) z (if az (+ z sz) z)
                  id (.stateAt ^js raw x y z)]
              (if (< id 0)
                n
                (let [light (.lightAt ^js raw x y z)
                      blocks? (== 1 (aget sight id))
                      shown (if blocks? (max-light light prev) light)]
                  (when (or (== 1 (aget visible shown)) (<= t near)) (record! st store x y z id now))
                  (if blocks?
                    (inc n)
                    (recur x y z
                           (if ax (+ tmx tdx) tmx) (if ay (+ tmy tdy) tmy) (if az (+ tmz tdz) tmz)
                           light (inc n))))))))))))

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
  "Starts a pass, unless the sight table is not ready yet (the next step tries again)."
  [{:keys [raw opts] :as per} ^js st ^js eye now]
  (when (sight-of per st)
    (set! (.-visible st) (table-now raw (:seeing-min opts)))
    (set! (.-lastStart st) #js {:at now :eye eye :epoch (epoch-of raw) :darken (darken-now raw) :near (near-of per)})
    (set! (.-pass st) #js {:next 0 :jx (js/Math.random) :jy (js/Math.random)})))

(defn cast-slice!
  "Casts rays [from, to) of the pass from the eye's current place and look."
  [{:keys [grid] :as per} ^js st ^js pass ^js eye from to now]
  (let [{:keys [cols rows hx hy]} grid
        b (basis (.-yaw eye) (.-pitch eye))
        store (store-of st (.-dimension eye))
        ox (.-x eye) oy (.-y eye) oz (.-z eye)
        du (/ (* 2 hx) cols) dv (/ (* 2 hy) rows)]
    (set! (.-near st) (near-of per))
    (loop [k from cells 0]
      (if (>= k to)
        cells
        (let [a (+ (- hx) (* du (+ (mod k cols) (.-jx pass))))
              v (+ (- hy) (* dv (+ (js/Math.floor (/ k cols)) (.-jy pass))))
              dx (+ (aget b 0) (* a (aget b 3)) (* v (aget b 6)))
              dy (+ (aget b 1) (* v (aget b 7)))
              dz (+ (aget b 2) (* a (aget b 5)) (* v (aget b 8)))
              len (js/Math.hypot dx dy dz)]
          (recur (inc k) (+ cells (cast! per st store ox oy oz (/ dx len) (/ dy len) (/ dz len) now))))))))

(defn step!
  "One slice of the sight pass (one game tick's share). Starts a new pass when none runs and the eye moved, turned or
  idled long enough. While offline (no eye) it casts nothing and drops the pass in flight."
  ([per] (step! per false))
  ([{:keys [raw opts st] :as per} force?]
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
         (start-pass! per st eye now))
       (when-let [^js pass (.-pass st)]
         (let [total (rays-per-pass per)
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
  "A whole sight pass at once (tests, and a forced look): finishes any pass in flight, then runs a fresh one."
  [{:keys [st] :as per}]
  (let [^js st st]
    (while (.-pass st) (step! per))
    (when (.eye ^js (:raw per))
      (step! per true)
      (while (.-pass st) (step! per)))))

;; ---- block changes and touch

(defn walk-cells
  "Walk the cells from the eye (ox oy oz) towards the point (tx ty tz), up to (excluding) the point's own cell. Nil when a
  cell on the way blocks sight under `table` (the raw world's sightTable), is unloaded, or the walk overruns its budget;
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
      (let [t (min tmx tmy tmz)]
        (cond
          (or (and (== cx x1) (== cy y1) (== cz z1)) (> t d)) prev
          (<= budget 0) nil
          :else
          (let [ax (== t tmx) ay (and (not ax) (== t tmy)) az (and (not ax) (not ay))
                nx (if ax (+ cx sx) cx) ny (if ay (+ cy sy) cy) nz (if az (+ cz sz) cz)
                target? (and (== nx x1) (== ny y1) (== nz z1))
                here (.stateAt raw nx ny nz)]
            (cond
              (and (not target?) (or (< here 0) (== 1 (aget table here)))) nil
              target? prev
              :else (recur nx ny nz
                           (if ax (+ tmx tdx) tmx) (if ay (+ tmy tdy) tmy) (if az (+ tmz tdz) tmz)
                           (or (.lightAt raw nx ny nz) 0) (dec budget)))))))))

(defn visible-now?
  "Whether the cell (x y z), now holding state id, is in view: inside the cone and the radius, nothing blocking sight on
  the way from the eye to its centre, and lit (or within near)."
  [{:keys [raw opts grid] :as per} ^js st ^js eye x y z id]
  (let [ox (.-x eye) oy (.-y eye) oz (.-z eye)
        vx (- (+ x 0.5) ox) vy (- (+ y 0.5) oy) vz (- (+ z 0.5) oz)
        dist (js/Math.hypot vx vy vz)
        ^js sight (sight-of per st)
        ^js visible (or (.-visible st) (set! (.-visible st) (table-now raw (:seeing-min opts))))]
    (and sight
         (<= dist (:radius opts))
         (in-cone? (basis (.-yaw eye) (.-pitch eye)) (:hx grid) (:hy grid) vx vy vz)
         (when-let [prev (walk-cells raw sight ox oy oz (+ x 0.5) (+ y 0.5) (+ z 0.5))]
           (let [light (.lightAt ^js raw x y z)
                 shown (if (== 1 (aget sight id)) (max-light light prev) light)]
             (or (== 1 (aget visible shown)) (<= dist (near-of per))))))))

(defn line-clear?
  "Whether the eye at (ox oy oz) has a clear line to the point (tx ty tz): no cell strictly between the eye's cell and
  the point's cell blocks sight under `table` (the raw world's sightTable) or is unloaded. No cone and no light rule:
  this answers whether a thing there could be seen by turning to it."
  [^js raw ^js table ox oy oz tx ty tz]
  (boolean (walk-cells raw table ox oy oz tx ty tz)))

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

(def hitbox-half 0.3)
(def hitbox-height 1.8)
(def feel-margin 0.1)

(defn felt?
  "Whether the body touches the cell (x y z) whose collision shape reaches `top` above its floor: it overlaps the
  body's hitbox (0.6 wide, 1.8 tall, feet at the eye less eye-height) grown by feel-margin, or is a tall support (a
  fence) under the feet whose top reaches the feet."
  [^js eye [x y z] top]
  (let [fx (.-x eye) fz (.-z eye) feet (- (.-y eye) eye-height)
        r (+ hitbox-half feel-margin)
        over? (fn [c lo hi] (and (< c hi) (> (inc c) lo)))]
    (and (over? x (- fx r) (+ fx r))
         (over? z (- fz r) (+ fz r))
         (or (over? y (- feet feel-margin) (+ feet hitbox-height feel-margin))
             (and (< y feet) (>= (+ y top) (- feet feel-margin)))))))

(defn feel!
  "The state id of a cell the body touches (felt?), in any light; memory takes it. Nil for any other cell, an unloaded
  one, or offline."
  [{:keys [raw opts st]} [x y z :as pos]]
  (let [^js st st ^js eye (.eye ^js raw)]
    (when eye
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
