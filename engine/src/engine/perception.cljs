(ns engine.perception
  "What the body has seen: the layer between the raw world and everything above the body.
  It wraps the primitives object once (main.cljs, and the same wrapper over the test fake).
  The raw world comes from a rawWorld reader (engine/js/raw-world.mjs for the body, engine.fake.raw-world for tests):
  stateAt, lightAt, eye, sky, version, sightTable, stateInfo, onBlockChange.

  Seen blocks. A sight pass casts rays from the eye through a vanilla-like view cone (70 degrees vertical, 16:9)
  along the body's real yaw and pitch, out to `:radius` (48).
    A ray records every cell it enters that the light rule lets the body make out.
    It stops at the first cell that blocks sight (recorded too) or that is not loaded.
    Light rule: vanilla's lightmap brightness `seeing` of the cell's light is at least 0.2, or the cell is within 2 blocks.
    A sight-blocking cell is lit by the brighter of itself and the cell the ray came from.
    A dark cell is not recorded and the ray goes on, so a lit room is seen across a dark gap.
    A pass is spread over `:pass-ms` (1.5 s) in slices of `:step-ms` (one game tick).
    A new pass starts when the eye moved or turned, or every `:idle-ms`.
    What is behind the body is seen only once it turns.

  Block memory. Per dimension and 16^3 section: a Uint16Array of stateId + 1 (0 = unknown), a Uint8Array of each
  cell's seen time (whole minutes after the section's :base) and the section's last-seen time.
    A cell more than 255 minutes older than the newest write reads as 255 minutes older.
    Capped at `:cap-bytes` (32 MB, 2730 sections of 12 KB) by forgetting the least recently seen section.
    Saved to and loaded from a file (engine/js/seen-file.mjs).
    A block change in view (cone, line of sight, light) updates memory at once.
    A change out of view leaves the old state, a true memory error.

  Mob memory. The hostile mobs the body has seen or heard, by entity id. The danger checks (engine.jobs.reach and the
  callers of engine.jobs.danger) take their candidates from it, not from every mob the server tracks.
  A sample (every `:mob-ms`, and at each knownMobs call) reads the hostiles within `:mob-scan` and senses each one:
    heard  within `:hearing` (16) of the eye, unless the mob makes no sound while it stalks (`silent-mobs`: creeper).
    seen   all of: a clear line from the eye to its middle (the entity's `visible` field), within `:radius`,
           lit (the cell of its feet or head is bright enough by the block light rule; a mob in the dark is seen
           only within `:dark-sight` (4)), and inside the view cone or heard.
  A sensed mob's entry takes its place and time (seen-at when seen).
  An entry not sensed now stays at its last place while the mob could not have drifted `:mob-drift` (16) blocks
  (time x `mob-speed`): about 6 s for a zombie.
  An entry whose id the client no longer tracks (dead, despawned, far off) is dropped at once.
  So an unseen silent creeper behind the body is no danger, but a creeper seen 3 s ago that went round a corner still is.

  Wrapping leaves blocks, blockAt and entities raw. It adds seenBlockAt, seenBlocks and knownMobs.
  dig, place, jumpPlace and useOn let memory take the true state of their cell once they settle.")

(def defaults
  {:radius 48
   :fov 70                ; vertical field of view, degrees (vanilla's default)
   :aspect (/ 16 9)
   :ray-deg 1             ; ray spacing at the centre of the view
   :pass-ms 1500
   :step-ms 50
   :idle-ms 3000          ; a still body looks again this often
   :move-blocks 0.5
   :turn-deg 2
   :seeing-min 0.2
   :near 2
   :cap-bytes (* 32 1024 1024)
   :save-ms 60000
   :stats-ms 60000
   :error-every-ms 60000
   :mob-ms 250            ; a mob sample this often
   :mob-scan 64           ; hostiles within this of the body are sampled
   :hearing 16            ; a mob within this of the eye is heard
   :dark-sight 4          ; a mob standing in the dark (light too low to make out) is seen only within this
   :mob-drift 16})        ; a mob not sensed is forgotten once it could have walked this far

(def section-bytes 12288) ; ids (8192) and per-cell seen times (4096)
(def minute-ms 60000)
(def max-seen-radius 64)
(def eye-height 1.62)

;; ---- the light curve: tools/view/web/shading.mjs (vanilla's lightmap at default brightness), max channel

(defn clamp [v lo hi] (min hi (max lo v)))
(defn mix [a b t] (+ a (* (- b a) t)))
(defn fract [v] (- v (js/Math.floor v)))

(defn celestial-angle [t]
  (let [d (fract (- (/ t 24000) 0.25))
        e (- 0.5 (/ (js/Math.cos (* d js/Math.PI)) 2))]
    (/ (+ (* 2 d) e) 3)))

(defn sky-darken [t rain thunder]
  (let [f (celestial-angle t)
        g (- 1 (clamp (- 1 (+ (* (js/Math.cos (* f 2 js/Math.PI)) 2) 0.2)) 0 1))
        g (* g (- 1 (/ (* rain 5) 16)) (- 1 (/ (* thunder 5) 16)))]
    (+ (* g 0.8) 0.2)))

(defn brightness [level] (let [f (/ level 15)] (/ f (- 4 (* 3 f)))))

(defn channel [v sky-v s]
  (let [c (clamp (mix (+ v (* sky-v s)) 0.75 0.04) 0 1)
        c (mix c (- 1 (js/Math.pow (- 1 c) 4)) 0.5)]
    (clamp (mix c 0.75 0.04) 0 1)))

(defn seeing
  "How bright a cell with sky and block light (0..15) looks: the brightest channel of the lightmap, 0.099..0.99."
  [sky block darken]
  (let [s (* (brightness sky) (+ (* darken 0.95) 0.05))
        b (* (brightness block) 1.5)
        sv (mix darken 1 0.35)]
    (max (channel b sv s)
         (channel (* b (+ (* (+ (* b 0.6) 0.4) 0.6) 0.4)) sv s)
         (channel (* b (+ (* b b 0.6) 0.4)) 1 s))))

(defn visible-table
  "Uint8Array over packed light (sky << 4 | block): 1 where the cell is bright enough to make out."
  [darken seeing-min]
  (let [out (js/Uint8Array. 256)]
    (dotimes [i 256]
      (when (>= (seeing (bit-shift-right i 4) (bit-and i 15) darken) seeing-min) (aset out i 1)))
    out))

(defn table-now [raw seeing-min]
  (let [sky (.sky ^js raw)]
    (visible-table (sky-darken (.-timeOfDay sky) (.-rain sky) (.-thunder sky)) seeing-min)))

(defn max-light
  "Per channel, the brighter of two packed lights."
  [a b]
  (bit-or (max (bit-and a 0xF0) (bit-and b 0xF0)) (max (bit-and a 15) (bit-and b 15))))

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

;; ---- the store

(defn section-key [cx sy cz]
  (+ (* (+ (* (+ cx 2097152) 4194304) (+ cz 2097152)) 256) (+ sy 128)))

(defn cell-index [x y z]
  (bit-or (bit-shift-left (bit-and y 15) 8) (bit-shift-left (bit-and z 15) 4) (bit-and x 15)))

(defn store-of
  "The section map of a dimension, made when first asked for."
  [^js st dim]
  (or (.get (.-stores st) dim)
      (let [m (js/Map.)] (.set (.-stores st) dim m) m)))

(defn evict-oldest! [^js st]
  (let [oldest (reduce (fn [best ^js m]
                         (let [e (.next (.entries m))]
                           (if (or (.-done e) (and best (<= (.-seen ^js (aget (:entry best) 1)) (.-seen ^js (aget (.-value e) 1)))))
                             best
                             {:map m :entry (.-value e)})))
                       nil (es6-iterator-seq (.values (.-stores st))))]
    (when oldest
      (.delete ^js (:map oldest) (aget (:entry oldest) 0))
      (set! (.-count st) (dec (.-count st)))
      (set! (.-lastKey st) -1))))

(defn section-for!
  "The section holding key in store, made (forgetting the least recently seen one past the cap) and moved to the
  newest end once per stamp."
  [^js st ^js store key cx sy cz now]
  (let [found (.get store key)
        ^js sec (or found
                    (do (while (>= (.-count st) (.-cap st)) (evict-oldest! st))
                        (set! (.-count st) (inc (.-count st)))
                        #js {:ids (js/Uint16Array. 4096) :times (js/Uint8Array. 4096) :base now :seen now :stamp -1 :cx cx :sy sy :cz cz}))]
    (when (not= (.-stamp sec) (.-stamp st))
      (.delete store key)
      (.set store key sec)
      (set! (.-seen sec) now)
      (set! (.-stamp sec) (.-stamp st)))
    (set! (.-lastKey st) key)
    (set! (.-lastSec st) sec)
    sec))

(defn minute-of!
  "The cell time (minutes after the section's base) for a write at now; the base moves on when it would pass 255."
  [^js sec now]
  (let [m (js/Math.max 0 (js/Math.floor (/ (- now (.-base sec)) minute-ms)))]
    (if (<= m 255)
      m
      (let [shift (- m 255) ^js times (.-times sec)]
        (dotimes [i 4096] (aset times i (js/Math.max 0 (- (aget times i) shift))))
        (set! (.-base sec) (+ (.-base sec) (* shift minute-ms)))
        255))))

(defn cell-seen
  "When the cell was last seen, in ms."
  [^js sec i]
  (+ (.-base sec) (* minute-ms (aget (.-times sec) i))))

(defn record! [^js st store x y z id now]
  (let [cx (bit-shift-right x 4) sy (bit-shift-right y 4) cz (bit-shift-right z 4)
        key (section-key cx sy cz)
        ^js sec (if (== key (.-lastKey st)) (.-lastSec st) (section-for! st store key cx sy cz now))]
    (let [i (cell-index x y z)]
      (aset (.-ids sec) i (inc id))
      (aset (.-times sec) i (minute-of! sec now)))))

(defn new-stamp!
  "A new write round: sections written from now on are moved to the newest end again."
  [^js st]
  (set! (.-stamp st) (inc (.-stamp st)))
  (set! (.-lastKey st) -1))

;; ---- create

(defn create
  "A perception over a rawWorld reader. opts override `defaults` (plus :now, a clock in ms)."
  [raw opts]
  (let [o (merge defaults {:now #(js/Date.now)} opts)
        g (grid o)]
    {:raw raw
     :opts o
     :grid g
     :st #js {:stores (js/Map.) :count 0 :cap (max 1 (js/Math.floor (/ (:cap-bytes o) section-bytes)))
              :stamp 0 :lastKey -1 :lastSec nil :dim "overworld" :sight nil :visible nil
              :pass nil :lastStart nil :unsubscribe nil
              :mobs (js/Map.) :mobSource nil
              :passes 0 :rays 0 :cells 0 :steps 0 :stepMs 0 :stepMsMax 0}}))

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

(defn cast!
  "One ray from (ox oy oz) along the unit vector (dx dy dz). Returns the number of cells entered."
  [{:keys [raw opts]} ^js st store ox oy oz dx dy dz now]
  (let [radius (:radius opts) near (:near opts)
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

(defn start-pass!
  "Starts a pass, unless the sight table is not ready yet (the next step tries again)."
  [{:keys [raw opts] :as per} ^js st ^js eye now]
  (when (sight-of per st)
    (set! (.-visible st) (table-now raw (:seeing-min opts)))
    (set! (.-lastStart st) #js {:at now :eye eye})
    (set! (.-pass st) #js {:next 0 :jx (js/Math.random) :jy (js/Math.random)})))

(defn cast-slice!
  "Casts rays [from, to) of the pass from the eye's current place and look."
  [{:keys [grid] :as per} ^js st ^js pass ^js eye from to now]
  (let [{:keys [cols rows hx hy]} grid
        b (basis (.-yaw eye) (.-pitch eye))
        store (store-of st (.-dimension eye))
        ox (.-x eye) oy (.-y eye) oz (.-z eye)
        du (/ (* 2 hx) cols) dv (/ (* 2 hy) rows)]
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
                      (>= (- now (.-at (.-lastStart st))) (:idle-ms opts))
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
         (let [dx (/ vx dist) dy (/ vy dist) dz (/ vz dist)
               sx (if (pos? dx) 1 -1) sy (if (pos? dy) 1 -1) sz (if (pos? dz) 1 -1)
               tdx (if (zero? dx) js/Infinity (js/Math.abs (/ 1 dx)))
               tdy (if (zero? dy) js/Infinity (js/Math.abs (/ 1 dy)))
               tdz (if (zero? dz) js/Infinity (js/Math.abs (/ 1 dz)))
               x0 (js/Math.floor ox) y0 (js/Math.floor oy) z0 (js/Math.floor oz)]
           (loop [cx x0 cy y0 cz z0
                  tmx (if (zero? dx) js/Infinity (* tdx (if (pos? dx) (- (inc x0) ox) (- ox x0))))
                  tmy (if (zero? dy) js/Infinity (* tdy (if (pos? dy) (- (inc y0) oy) (- oy y0))))
                  tmz (if (zero? dz) js/Infinity (* tdz (if (pos? dz) (- (inc z0) oz) (- oz z0))))
                  prev (.lightAt ^js raw x0 y0 z0)
                  budget (+ 3 (js/Math.abs (- x x0)) (js/Math.abs (- y y0)) (js/Math.abs (- z z0)))]
             (if (and (== cx x) (== cy y) (== cz z))
               (let [light (.lightAt ^js raw x y z)
                     shown (if (== 1 (aget sight id)) (max-light light prev) light)]
                 (or (== 1 (aget visible shown)) (<= dist (:near opts))))
               (let [t (min tmx tmy tmz)
                     ax (== t tmx) ay (and (not ax) (== t tmy)) az (and (not ax) (not ay))
                     nx (if ax (+ cx sx) cx) ny (if ay (+ cy sy) cy) nz (if az (+ cz sz) cz)
                     here (.stateAt ^js raw nx ny nz)
                     target? (and (== nx x) (== ny y) (== nz z))]
                 (cond
                   (<= budget 0) false
                   (and (not target?) (or (< here 0) (== 1 (aget sight here)))) false
                   :else (recur nx ny nz
                                (if ax (+ tmx tdx) tmx) (if ay (+ tmy tdy) tmy) (if az (+ tmz tdz) tmz)
                                (if target? prev (.lightAt ^js raw nx ny nz)) (dec budget))))))))))

(defn line-clear?
  "Whether the eye at (ox oy oz) has a clear line to the point (tx ty tz): no cell strictly between the eye's cell and
  the point's cell blocks sight under `table` (the raw world's sightTable) or is unloaded. No cone and no light rule:
  this answers whether a thing there could be seen by turning to it."
  [^js raw ^js table ox oy oz tx ty tz]
  (let [vx (- tx ox) vy (- ty oy) vz (- tz oz)
        d (js/Math.hypot vx vy vz)
        x1 (js/Math.floor tx) y1 (js/Math.floor ty) z1 (js/Math.floor tz)]
    (or (zero? d)
        (let [dx (/ vx d) dy (/ vy d) dz (/ vz d)
              sx (if (pos? dx) 1 -1) sy (if (pos? dy) 1 -1) sz (if (pos? dz) 1 -1)
              tdx (if (zero? dx) js/Infinity (js/Math.abs (/ 1 dx)))
              tdy (if (zero? dy) js/Infinity (js/Math.abs (/ 1 dy)))
              tdz (if (zero? dz) js/Infinity (js/Math.abs (/ 1 dz)))
              x0 (js/Math.floor ox) y0 (js/Math.floor oy) z0 (js/Math.floor oz)]
          (loop [cx x0 cy y0 cz z0
                 tmx (if (zero? dx) js/Infinity (* tdx (if (pos? dx) (- (inc x0) ox) (- ox x0))))
                 tmy (if (zero? dy) js/Infinity (* tdy (if (pos? dy) (- (inc y0) oy) (- oy y0))))
                 tmz (if (zero? dz) js/Infinity (* tdz (if (pos? dz) (- (inc z0) oz) (- oz z0))))
                 budget (+ 3 (js/Math.abs (- x1 x0)) (js/Math.abs (- y1 y0)) (js/Math.abs (- z1 z0)))]
            (let [t (min tmx tmy tmz)]
              (cond
                (or (and (== cx x1) (== cy y1) (== cz z1)) (> t d)) true
                (<= budget 0) false
                :else
                (let [ax (== t tmx) ay (and (not ax) (== t tmy)) az (and (not ax) (not ay))
                      nx (if ax (+ cx sx) cx) ny (if ay (+ cy sy) cy) nz (if az (+ cz sz) cz)
                      here (.stateAt raw nx ny nz)]
                  (cond
                    (and (== nx x1) (== ny y1) (== nz z1)) true
                    (or (< here 0) (== 1 (aget table here))) false
                    :else (recur nx ny nz
                                 (if ax (+ tmx tdx) tmx) (if ay (+ tmy tdy) tmy) (if az (+ tmz tdz) tmz)
                                 (dec budget)))))))))))

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

;; ---- reading memory

(defn remembered
  "[state id, seen time in ms] of the remembered cell (x y z) in the current dimension, or nil when never seen."
  [^js st x y z]
  (when-let [^js sec (.get (store-of st (.-dim st))
                           (section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4)))]
    (let [v (aget (.-ids sec) (cell-index x y z))]
      (when (pos? v) [(dec v) (cell-seen sec (cell-index x y z))]))))

(defn properties-of [^js info]
  (let [props (js->clj (.-properties info) :keywordize-keys true)]
    (when (seq props) props)))

(defn seen-block
  "{:name :pos :properties :age-ms} as last seen, or {:unknown true :pos} for a cell the body never saw."
  [{:keys [raw opts st]} [x y z :as pos]]
  (if-let [[id seen-at] (remembered st x y z)]
    (let [^js info (.stateInfo ^js raw id)]
      (cond-> {:name (.-name info) :pos pos :age-ms (- ((:now opts)) seen-at)}
        (properties-of info) (assoc :properties (properties-of info))))
    {:unknown true :pos pos}))

(defn seen-blocks
  "Remembered blocks within radius (at most max-seen-radius) of the body's feet, nearest first:
  [{:name :pos :distance :age-ms}]. names (a coll) or match (name -> truthy) filters; max caps the list."
  [{:keys [raw opts st]} {:keys [radius names match max] :or {radius 16 max 64}}]
  (let [^js st st ^js eye (.eye ^js raw)]
    (if-not eye
      []
      (let [radius (min radius max-seen-radius)
            fx (.-x eye) fy (- (.-y eye) eye-height) fz (.-z eye)
            wanted? (cond names (set names) match match :else (constantly true))
            by-id (js/Map.)
            ok? (fn [id] (if (.has by-id id)
                           (.get by-id id)
                           (let [name (.-name ^js (.stateInfo ^js raw id))
                                 hit (when (wanted? name) name)]
                             (.set by-id id hit)
                             hit)))
            now ((:now opts))
            found (array)]
        (.forEach (store-of st (.-dim st))
                  (fn [^js sec]
                    (let [bx (* 16 (.-cx sec)) by (* 16 (.-sy sec)) bz (* 16 (.-cz sec))
                          ^js ids (.-ids sec)]
                      (when (<= (js/Math.hypot (- (+ bx 8) fx) (- (+ by 8) fy) (- (+ bz 8) fz)) (+ radius 14))
                        (dotimes [i 4096]
                          (let [v (aget ids i)]
                            (when (pos? v)
                              (when-let [name (ok? (dec v))]
                                (let [x (+ bx (bit-and i 15)) y (+ by (bit-shift-right i 8)) z (+ bz (bit-and (bit-shift-right i 4) 15))
                                      d (js/Math.hypot (- (+ x 0.5) fx) (- (+ y 0.5) fy) (- (+ z 0.5) fz))]
                                  (when (<= d radius)
                                    (.push found #js {:name name :pos [x y z] :distance d :ageMs (- now (cell-seen sec i))})))))))))))
        (.sort found (fn [^js a ^js b] (- (.-distance a) (.-distance b))))
        (into [] (comp (take max) (map (fn [^js r] {:name (.-name r) :pos (.-pos r) :distance (.-distance r) :age-ms (.-ageMs r)}))) found)))))

(defn stats [{:keys [st] :as per}]
  (let [^js st st]
    {:passes (.-passes st) :rays (.-rays st) :cells (.-cells st) :steps (.-steps st)
     :step-ms-mean (if (pos? (.-steps st)) (/ (.-stepMs st) (.-steps st)) 0) :step-ms-max (.-stepMsMax st)
     :sections (.-count st) :bytes (* section-bytes (.-count st))
     :steps-per-pass (steps-per-pass per) :rays-per-pass (rays-per-pass per)}))

;; ---- mobs: the hostiles the body has seen or heard

(def silent-mobs "Hostiles that make no sound while they stalk: known only once seen, or while fusing (hissing)." #{"creeper"})

(def mob-speed
  "Blocks a second a hostile covers when it chases (rough game values); one not listed `default-mob-speed`."
  {"zombie" 2.3 "husk" 2.3 "drowned" 2.3 "zombie_villager" 2.3 "zombified_piglin" 2.3 "blaze" 2.3
   "skeleton" 2.5 "stray" 2.5 "bogged" 2.5 "wither_skeleton" 2.5 "creeper" 2.5 "witch" 2.5
   "silverfish" 2.5 "endermite" 2.5 "slime" 2 "magma_cube" 2
   "spider" 3 "cave_spider" 3 "enderman" 3 "hoglin" 3 "zoglin" 3 "ravager" 3 "piglin_brute" 3.5
   "pillager" 3.5 "vindicator" 3.5 "evoker" 3 "guardian" 5 "phantom" 6 "vex" 8})

(def default-mob-speed 3)

(defn mob-forget-ms
  "How long a mob of name not sensed again stays known: the time it takes to drift :mob-drift blocks."
  [{:keys [opts]} name]
  (* 1000 (/ (:mob-drift opts) (get mob-speed name default-mob-speed))))

(def mob-middle 0.9)

(defn mob-lit?
  "Whether the mob at pos stands in light bright enough to make out: the light rule of the sight table, read at the
  cells of its feet and head."
  [{:keys [raw opts st]} ^js pos]
  (let [^js visible (or (.-visible ^js st) (set! (.-visible ^js st) (table-now raw (:seeing-min opts))))
        x (js/Math.floor (.-x pos)) z (js/Math.floor (.-z pos)) y (js/Math.floor (.-y pos))
        lit? (fn [yy] (== 1 (aget visible (.lightAt ^js raw x yy z))))]
    (or (lit? y) (lit? (inc y)))))

(defn sense-thing
  "How the body senses a thing at pos (feet) from eye, given whether a line from the eye to it is clear (`visible?`)
  and whether it makes no sound (`silent?`): :seen, :heard or nil. One rule for mobs (mob-sense) and for what
  engine.entity-observations lists (the /entities tool)."
  [{:keys [opts grid] :as per} ^js eye ^js pos visible? silent?]
  (let [vx (- (.-x pos) (.-x eye)) vy (- (+ (.-y pos) mob-middle) (.-y eye)) vz (- (.-z pos) (.-z eye))
        d (js/Math.hypot vx vy vz)
        heard? (and (<= d (:hearing opts)) (not silent?))
        seen? (and visible?
                   (<= d (:radius opts))
                   (or (<= d (:dark-sight opts)) (mob-lit? per pos))
                   (or heard? (in-cone? (basis (.-yaw eye) (.-pitch eye)) (:hx grid) (:hy grid) vx vy vz)))]
    (cond seen? :seen heard? :heard :else nil)))

(defn mob-sense
  "How the body senses hostile e (an entities() entry) from eye: :seen, :heard or nil (see the ns doc)."
  [per ^js eye ^js e]
  (sense-thing per eye (.-pos e) (true? (.-visible e)) (and (silent-mobs (.-name e)) (not (true? (.-fusing e))))))

(defn sense-mobs!
  "One mob sample over primitives src: sensed mobs take their place and time, mobs the client no longer tracks or that
  could have drifted :mob-drift blocks since they were last sensed are forgotten. Returns the map of live entities
  sensed now, by id."
  [{:keys [raw opts st] :as per} ^js src]
  (let [^js st st
        ^js mobs (.-mobs st)
        ^js eye (.eye ^js raw)
        now ((:now opts))
        sensed (js/Map.)]
    (if-not (and eye src)
      sensed
      (let [listed (js/Set.)]
        (doseq [^js e (array-seq (.entities src #js {:radius (:mob-scan opts) :kind "hostile" :max 64}))
                :when (.-pos e)]
          (.add listed (.-id e))
          (when-let [how (mob-sense per eye e)]
            (let [^js old (.get mobs (.-id e))
                  ^js pos (.-pos e)]
              (.set sensed (.-id e) e)
              (.set mobs (.-id e)
                    #js {:id (.-id e) :name (.-name e) :kind (.-kind e)
                         :pos #js {:x (.-x pos) :y (.-y pos) :z (.-z pos)}
                         :at now
                         :seenAt (if (keyword-identical? how :seen) now (some-> old .-seenAt))
                         :heard (keyword-identical? how :heard)}))))
        (doseq [[id ^js m] (vec (es6-iterator-seq (.entries mobs)))]
          (when (or (not (.has listed id))
                    (> (- now (.-at m)) (mob-forget-ms per (.-name m))))
            (.delete mobs id)))
        sensed))))

(defn known-mobs
  "The hostiles the body knows of after a fresh sample over primitives src, nearest first, as entities() entries: a mob
  sensed now is its live entry; one remembered is {:id :name :kind :pos (where it was last sensed) :distance :visible
  false}. Each adds seen (seen now or since it was last out of mind), heard (heard now, unseen), remembered (not
  sensed now) and ageMs (since last sensed)."
  [{:keys [raw opts st] :as per} ^js src]
  (let [sensed (sense-mobs! per src)
        ^js eye (.eye ^js raw)
        now ((:now opts))]
    (if-not eye
      #js []
      (let [fx (.-x eye) fy (- (.-y eye) eye-height) fz (.-z eye)
            out (into-array
                 (for [^js m (es6-iterator-seq (.values (.-mobs ^js st)))]
                   (let [^js live (.get sensed (.-id m))
                         ^js pos (.-pos m)
                         base (if live
                                (js/Object.assign #js {} live)
                                #js {:id (.-id m) :name (.-name m) :kind (.-kind m) :pos pos
                                     :distance (js/Math.hypot (- (.-x pos) fx) (- (.-y pos) fy) (- (.-z pos) fz))
                                     :visible false})]
                     (when (= "creeper" (.-name m)) (aset base "creeper" true))
                     (js/Object.assign base #js {:seen (some? (.-seenAt m))
                                                 :heard (and (some? live) (.-heard m))
                                                 :remembered (nil? live)
                                                 :ageMs (- now (.-at m))}))))]
        (.sort out (fn [^js a ^js b] (- (.-distance a) (.-distance b))))
        out))))

;; ---- persistence (io: engine/js/seen-file.mjs)

(defn all-sections
  "Every remembered section, least recently seen first, as the file module takes them."
  [^js st]
  (->> (for [[dim ^js m] (es6-iterator-seq (.entries (.-stores st)))
             ^js sec (es6-iterator-seq (.values m))]
         #js {:dim dim :cx (.-cx sec) :sy (.-sy sec) :cz (.-cz sec) :seen (.-seen sec) :base (.-base sec)
              :ids (.-ids sec) :times (.-times sec)})
       (sort-by #(.-seen ^js %))
       to-array))

(defn save!
  "Writes memory to file. Returns a promise."
  [{:keys [raw st]} ^js io file]
  (.saveSeen io file #js {:version (.version ^js raw) :sections (all-sections st)}))

(defn load!
  "Reads memory from file, unless it is missing, damaged or from another game version. Returns the sections loaded."
  [{:keys [raw st]} ^js io file]
  (let [^js st st ^js data (.loadSeen io file)]
    (if-not (and data (= (.-version data) (.version ^js raw)))
      0
      (do (doseq [^js s (.-sections data)]
            (while (>= (.-count st) (.-cap st)) (evict-oldest! st))
            (let [store (store-of st (.-dim s))
                  key (section-key (.-cx s) (.-sy s) (.-cz s))]
              (when-not (.has store key) (set! (.-count st) (inc (.-count st))))
              (.set store key #js {:ids (.-ids s) :times (.-times s) :base (.-base s) :seen (.-seen s) :stamp -1 :cx (.-cx s) :sy (.-sy s) :cz (.-cz s)})))
          (set! (.-lastKey st) -1)
          (count (.-sections data))))))

;; ---- running in the body

(defn start!
  "Runs the perception in the body: loads file, follows block changes, one sight step every step-ms, saves every
  save-ms and on stop, and reports perception.stats (every stats-ms) and perception.error (at most once a minute)
  through on-event. Returns stop, which resolves once memory is saved."
  [{:keys [opts st] :as per} {:keys [file io on-event] :or {on-event (fn [_])}}]
  (let [^js st st
        last-error (atom (- js/Infinity))
        report! (fn [e]
                  (let [now ((:now opts))]
                    (when (>= (- now @last-error) (:error-every-ms opts))
                      (reset! last-error now)
                      (on-event {:kind :perception.error :source :body :level :warn :error (str (or (.-message e) e))}))))
        save (fn [] (-> (save! per io file) (.catch report!)))
        _ (try (load! per io file) (catch :default e (report! e)))
        off (start-listening! per)
        timer (fn [ms f] (doto (js/setInterval (fn [] (try (f) (catch :default e (report! e)))) ms) (.unref)))
        timers [(timer (:step-ms opts) #(step! per))
                (timer (:save-ms opts) save)
                (timer (:mob-ms opts) #(when-let [src (.-mobSource st)] (sense-mobs! per src)))
                (timer (:stats-ms opts) #(on-event (merge {:kind :perception.stats :source :body :level :info}
                                                          (stats per))))]]
    (fn []
      (run! js/clearInterval timers)
      (off)
      (save))))

(defn wrap
  "The primitives object p with every primitive as it is (blocks, blockAt, entities stay raw; dig, place, jumpPlace and useOn also let memory
  take the true state of their cell, see touching), plus
  seenBlockAt({x,y,z}) and seenBlocks({radius, names, max}) over memory, knownMobs() (known-mobs: the hostiles the body
  has seen or heard, sampled from p's entities), and the perception itself."
  [p per]
  (let [out (js/Object.assign #js {} p)
        pos-js (fn [[x y z]] #js {:x x :y y :z z})]
    (aset out "perception" per)
    (set! (.-mobSource ^js (:st per)) p)
    (aset out "knownMobs" (fn [] (known-mobs per p)))
    (run! #(when-let [f (aget p %)] (aset out % (touching per p f))) touching-primitives)
    (aset out "seenBlockAt" (fn [^js a]
                              (let [b (seen-block per [(.-x a) (.-y a) (.-z a)])]
                                (clj->js (update b :pos pos-js)))))
    (aset out "seenBlocks" (fn [^js a]
                             (let [q (js->clj (or a #js {}) :keywordize-keys true)]
                               (clj->js (mapv #(update % :pos pos-js) (seen-blocks per q))))))
    out))
