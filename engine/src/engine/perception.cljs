(ns engine.perception
  "What the body has seen: the layer between the raw world and everything above the body.
  It wraps the primitives object once (main.cljs, and the same wrapper over the test fake).
  The raw world comes from a rawWorld reader (engine/js/raw-world.mjs for the body, engine.fake.raw-world for tests):
  stateAt, lightAt, eye, sky, version, sightTable, stateInfo, onBlockChange.

  Seen blocks. A sight pass casts rays from the eye through a vanilla-like view cone (70 degrees vertical, 16:9)
  along the body's real yaw and pitch, out to `:radius` (48).
    A ray records every cell it enters that the light rule lets the body make out.
    It stops at the first cell that blocks sight (recorded too) or that is not loaded.
    Light rule: vanilla's lightmap brightness `seeing` of the cell's light is at least 0.2, or the cell is within `:near` (4; `:near-torch` 7 with a torch in either hand) blocks.
    A sight-blocking cell is lit by the brighter of itself and the cell the ray came from.
    A dark cell is not recorded and the ray goes on, so a lit room is seen across a dark gap.
    Aimed rows: besides the grid, per block of distance ahead, a row of ground rays from `:ground-from` (8) to `:ground-to`
    (32) at the ground an eye height below the eye, `:ground-across` (0.7) blocks apart, and wall rays from `:wall-from` (4)
    to `:wall-to` (32) at each of `:wall-lanes` to either side and `:wall-heights` above the eye (a tunnel's walls and ceiling),
    inside the cone only. Surfaces along the line of sight fall between grid rays; these rows see them as a player does.
    `:effort` (0-1) scales the grid: a pass casts effort-min (0.15) to all of a `:ray-deg` grid; aimed rows always.
    A forced pass with `{:reuse? true}` (jobs.lib.look/see!) casts nothing when the last finished pass had the same eye spot, look, world and light, at the same effort or more, less than 2 s ago.
    A pass is spread over `:pass-ms` (1.5 s) in slices of `:step-ms` (one game tick).
    Slices share one mark stamp while the raw-world epoch, darkness and near stay and for 2 s at most, wherever the eye moves: a cell recorded in it is not read again, so its recorded time is up to 2 s old (a thin block is read every time).
    A new pass starts when the eye moved or turned, every `:idle-ms` if the world changed (a block or chunk update, the
    daylight, a torch in hand), else every `:still-ms`.
    What is behind the body is seen only once it turns.

  Block memory. Per dimension and 16^3 section: a Uint16Array of stateId + 1 (0 = unknown), a Uint16Array of each
  cell's seen time (whole seconds after the section's :base) and the section's last-seen time.
    A cell more than 255 minutes (15300 s) older than the newest write reads as 255 minutes older.
    Capped at `:cap-bytes` (44 MB, 2816 sections of 16 KB) by forgetting the least recently seen section.
    Saved to and loaded from a file (engine/js/seen-file.mjs).
    A block change in view (cone, line of sight, light) updates memory at once.
    A change out of view leaves the old state, a true memory error.

  Mob memory. The hostile mobs the body has seen or heard, by entity id. The danger checks (jobs.lib.reach and the
  callers of jobs.survival.danger) take their candidates from it, not from every mob the server tracks.
  A sample (every `:mob-ms`, and at each knownMobs call) reads the hostiles within `:mob-scan` and senses each one:
    heard  within `:hearing` (16) of the eye, unless the mob makes no sound while it stalks (`silent-mobs`: creeper).
    seen   all of: a clear line from the eye to its middle (the entity's `visible` field), within `:radius`,
           lit (the cell of its feet or head is bright enough by the block light rule; a mob in the dark is seen
           only within the same near distance), and inside the view cone or heard.
  A sensed mob's entry takes its place and time (seen-at when seen).
  An entry not sensed now stays at its last place while the mob could not have drifted `:mob-drift` (16) blocks
  (time x `mob-speed`): about 6 s for a zombie.
  An entry whose id the client no longer tracks (dead, despawned, far off) is dropped at once.
  So an unseen silent creeper behind the body is no danger, but a creeper seen 3 s ago that went round a corner still is.

  One cell. `sensed` answers what the body knows of a cell now, first match:
    nil          unloaded, or offline;
    felt         the cell touches the body (hitbox grown by 0.1, and the cell under the feet), any light;
    visible      a glance: one ray, by the pass's rules (cone, line, light or near); memory takes it;
    remembered   the state last seen, :age-ms old (to the ms for fluids, doors, gates, trapdoors and fire; else
                 to the second); one of those mutable states older than `:mutable-max-ms` (10 s) reads unknown;
    unknown      {:unknown true :pos}.
  Answers are cached for one tick (`:step-ms`) while the eye and the raw world's epoch stay the same.
  A cell outside the cone needs a turn first: the wrapped look glances its cell and the 6 neighbours once it settles.

  Wrapping leaves blocks, blockAt and entities raw. It adds seenBlockAt, seenBlocks, knownMobs, and sensedAt, feel and
  glance (blockAt-like answers: name, pos, properties, age, fullCube, plus ageMs and felt / visible).
  dig, place, jumpPlace and useOn let memory take the true state of their cell once they settle.

  The parts: engine.perception.light (light curve), .store (block memory and reading it), .rays (view cone, sight pass,
  keeping memory current), .mobs (mob memory), .persist (file). This namespace creates, runs and wraps them."
  (:require [engine.args :as a]
            [engine.perception.light :as light]
            [engine.perception.mobs :as mobs]
            [engine.perception.persist :as persist]
            [engine.perception.rays :as rays]
            [engine.perception.store :as store]
            [engine.settings :as settings]))

(def sky-darken light/sky-darken)
(def seeing light/seeing)
(def section-bytes store/section-bytes)
(def seen-block store/seen-block)
(def seen-blocks store/seen-blocks)
(def line-clear? rays/line-clear?)
(def pass! rays/pass!)
(def look-for! rays/look-for!)
(def step! rays/step!)
(def start-listening! rays/start-listening!)
(def touch! rays/touch!)
(def touching-primitives rays/touching-primitives)
(def sense-thing mobs/sense-thing)
(def save! persist/save!)
(def load! persist/load!)

(a/defargs settings
  "The tuning numbers of what the body sees. :engine.perception/save-ms is in engine.settings."
  {:engine.perception/radius {:default 48 :spec (a/int-in 1 nil) :doc "How far the body looks, in blocks."}
   :engine.perception/fov {:default 70 :spec (a/num-in 1 nil) :doc "Vertical field of view, degrees (vanilla's default)."}
   :engine.perception/aspect {:default (/ 16 9) :spec (a/num-in 0.1 nil) :doc "Width over height of the view."}
   :engine.perception/ray-deg {:default 1 :spec (a/num-in 0.1 nil) :doc "Ray spacing at the centre of the view, degrees."}
   :engine.perception/effort {:default 1 :spec (a/num-in 0 1) :doc "How hard the body's own passes look, 0-1: the share of grid rays between effort-min and all."}
   :engine.perception/effort-min {:default 0.15 :spec (a/num-in 0.01 1) :doc "The share of grid rays a pass at effort 0 casts."}
   :engine.perception/ground-from {:default 8 :spec (a/num-in 0 nil) :doc "Ground rows start this many blocks ahead."}
   :engine.perception/ground-to {:default 32 :spec (a/num-in 0 nil) :doc "Ground rows end this many blocks ahead."}
   :engine.perception/ground-across {:default 0.7 :spec (a/num-in 0.1 nil) :doc "Blocks between the rays of a ground row."}
   :engine.perception/wall-from {:default 4 :spec (a/num-in 0 nil) :doc "Wall rows (along a tunnel's walls and ceiling) start this many blocks ahead."}
   :engine.perception/wall-to {:default 32 :spec (a/num-in 0 nil) :doc "Wall rows end this many blocks ahead."}
   :engine.perception/wall-lanes {:default [0 0.5 1 1.7] :spec (a/coll-of (a/num-in 0 nil)) :doc "Blocks to the side (both sides) a wall row aims at."}
   :engine.perception/wall-heights {:default [-1.1 -0.1 0.4 1.4] :spec (a/coll-of (a/num-in nil nil)) :doc "Heights above the eye a wall row aims at, in blocks."}
   :engine.perception/pass-ms {:default 1500 :spec (a/int-in 1 nil) :doc "How long one looking pass may take, ms of wall clock."}
   :engine.perception/step-ms {:default 50 :spec (a/int-in 1 nil) :doc "How long one looking step may take, ms of wall clock."}
   :engine.perception/idle-ms {:default 3000 :spec (a/int-in 1 nil) :doc "A still body looks again this often when the world around it changed, ms."}
   :engine.perception/still-ms {:default 30000 :spec (a/int-in 1 nil) :doc "A still body looks again this often when nothing changed (no block or chunk change, same daylight and torch), ms."}
   :engine.perception/move-blocks {:default 0.5 :spec (a/num-in 0 nil) :doc "Blocks the body moves before it looks again."}
   :engine.perception/turn-deg {:default 2 :spec (a/num-in 0 nil) :doc "Degrees the body turns before it looks again."}
   :engine.perception/seeing-min {:default 0.2 :spec (a/num-in 0 nil) :doc "The light level below which a block cannot be made out."}
   :engine.perception/near {:default 4 :spec (a/num-in 0 nil) :doc "Blocks within which light is not needed to see."}
   :engine.perception/near-torch {:default 7 :spec (a/num-in 0 nil) :doc "The same, while a torch (or soul torch) is held in either hand."}
   :engine.perception/cap-bytes {:default (* 44 1024 1024) :spec (a/int-in 12288 nil) :doc "Memory the seen-world sections may use, bytes."}
   :engine.perception/stats-ms {:default 60000 :spec (a/int-in 1 nil) :doc "How often perception statistics are logged, ms."}
   :engine.perception/error-every-ms {:default 60000 :spec (a/int-in 1 nil) :doc "How often one repeated perception error is logged, ms."}
   :engine.perception/mob-ms {:default 250 :spec (a/int-in 1 nil) :doc "A mob sample is taken this often, ms."}
   :engine.perception/mob-scan {:default 64 :spec (a/num-in 0 nil) :doc "Hostiles within this many blocks of the body are sampled."}
   :engine.perception/hearing {:default 16 :spec (a/num-in 0 nil) :doc "A mob within this many blocks of the eye is heard."}
   :engine.perception/mob-drift {:default 16 :spec (a/num-in 0 nil) :doc "A mob not sensed is forgotten once it could have walked this many blocks."}
   :engine.perception/mutable-max-ms {:default 10000 :spec (a/int-in 1 nil) :doc "A remembered fluid, door, gate, trapdoor or fire older than this reads unknown, ms."}})

(defn defaults
  "The perception options as set (:save-ms from engine.settings)."
  []
  (into {:save-ms (settings/get settings/settings :engine.perception/save-ms)}
        (map (fn [k] [(keyword (name k)) (settings/get settings k)]))
        (keys settings)))

;; ---- create

(defn create
  "A perception over a rawWorld reader. opts override `(defaults)` (plus :now, a clock in ms, and :rand, the random
  source in [0, 1) that jitters each pass's rays within their grid cells)."
  [raw opts]
  (let [o (merge (defaults) {:now #(js/Date.now) :rand #(js/Math.random)} opts)
        g (rays/grid o)]
    {:raw raw
     :opts o
     :grid g
     :st #js {:stores (js/Map.) :count 0 :cap (max 1 (js/Math.floor (/ (:cap-bytes o) store/section-bytes)))
              :writes 0 :agingSec -1 :anyFluid false :stamp 0 :lastKey -1 :lastSec nil :minSec nil :minNow nil :minVal 0 :dim "overworld" :sight nil :visible nil
              :pass nil :passRays nil :lastStart nil :unsubscribe nil
              :mobs (js/Map.) :mobSource nil
              :infoOf (fn [id] (.stateInfo ^js raw id)) :kinds nil :sensedKey nil :sensed (js/Map.) :blockBases (js/Map.) :jsBases (js/Map.)
              :nowFn (:now o) :tickMs (:step-ms o) :mutableMaxMs (:mutable-max-ms o) :radius (:radius o) :hx (:hx g) :hy (:hy g)
              :passes 0 :rays 0 :cells 0 :steps 0 :stepMs 0 :stepMsMax 0}}))

(defn stats [{:keys [st] :as per}]
  (let [^js st st]
    {:passes (.-passes st) :rays (.-rays st) :cells (.-cells st) :steps (.-steps st)
     :step-ms-mean (if (pos? (.-steps st)) (/ (.-stepMs st) (.-steps st)) 0) :step-ms-max (.-stepMsMax st)
     :sections (.-count st) :bytes (* store/section-bytes (.-count st))
     :steps-per-pass (rays/steps-per-pass per) :rays-per-pass (rays/rays-per-pass per)
     :why (into {} (map (fn [k] [(keyword k) (js->clj (aget (.-why st) k) :keywordize-keys true)])) (js/Object.keys (or (.-why st) #js {})))}))

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
        off (rays/start-listening! per)
        timer (fn [ms f] (doto (js/setInterval (fn [] (try (f) (catch :default e (report! e)))) ms) (.unref)))
        timers [(timer (:step-ms opts) #(rays/step! per))
                (timer (:save-ms opts) save)
                (timer (:mob-ms opts) #(when-let [src (.-mobSource st)] (mobs/sense-mobs! per src)))
                (timer (:stats-ms opts) #(on-event (merge {:kind :perception.stats :source :body :level :info}
                                                          (stats per))))]]
    (fn []
      (run! js/clearInterval timers)
      (off)
      (save))))

(defn block-base
  "The answer for state id without its place (name, properties, age, full-cube), kept per id: the same for every cell."
  [{:keys [raw st]} id]
  (let [^js cache (.-blockBases ^js st)]
    (or (.get cache id)
        (let [^js info (.stateInfo ^js raw id)
              props (store/properties-of info)
              base (cond-> {:name (.-name info) :state-id id}
                     props (assoc :properties props)
                     (:age props) (assoc :age (js/Number (:age props)))
                     (.-fullCube info) (assoc :full-cube true))]
          (.set cache id base)
          base))))

(defn block-of
  "A blockAt-like answer for state id at pos: {:name :pos :properties :age (a crop's, as a number) :full-cube :state-id}."
  [per id pos]
  (assoc (block-base per id) :pos pos))

(defn glance!
  "The cell's blockAt-like answer with :visible true when the body sees it now (one ray, the pass's rules; memory takes
  it), else nil. No head turn: outside the view cone it is nil."
  [per pos]
  (when-let [id (rays/glance! per pos)]
    (assoc (block-of per id pos) :age-ms 0 :visible true)))

(defn feel!
  "The cell's blockAt-like answer with :felt true when it touches the body (memory takes it), else nil."
  [per pos]
  (when-let [id (rays/feel! per pos)]
    (assoc (block-of per id pos) :age-ms 0 :felt true)))

(defn remembered-age
  "The age in ms of the remembered state id of cell (x y z), nil when it reads unknown: never seen, or a mutable state,
  or a non-solid cell beside a remembered fluid, older than :mutable-max-ms."
  [^js st x y z]
  (when-let [^js sec (store/section-at st x y z)]
    (let [i (store/cell-index x y z)
          v (aget (.-ids sec) i)]
      (when (pos? v)
        (let [id (dec v)
              age (- ((.-nowFn st)) (store/cell-seen sec i))]
          (when-not (and (> age (.-mutableMaxMs st)) (or (store/mutable-id? st id) (store/beside-fluid? st x y z id)))
            age))))))

(defn memory-epoch
  "A key (compare with =) that changes whenever what remembered-age answers can: a store write, or (while a cell that can
  age was seen within :mutable-max-ms) the next second of the clock. The key of anything kept from a read of the
  remembered world: it holds still while nothing can age."
  [^js st]
  [(.-writes st) (js/Math.min (js/Math.floor (/ ((.-nowFn st)) 1000)) (inc (.-agingSec st)))])

(defn remembered
  "The remembered answer for pos with :age-ms and :visible false; unknown when it reads unknown (remembered-age)."
  [{:keys [st] :as per} [x y z :as pos]]
  (if-let [age (remembered-age st x y z)]
    (assoc (block-of per (store/remembered-id st x y z) pos) :age-ms age :visible false)
    {:unknown true :pos pos}))

(defn sense-fresh!
  "Clears the sensed cache when what it depends on (the tick, the raw world's epoch and the eye) changed since the last
  call. The eye, nil offline. Compares the parts in place: a key string per call was the cost."
  [{:keys [raw st]}]
  (when-let [^js eye (.eye ^js raw)]
    (let [^js st st
          ^js last (.-sensedKey st)
          tick (js/Math.floor (/ ((.-nowFn st)) (.-tickMs st)))
          epoch (rays/epoch-of raw)]
      (when-not (and last
                     (identical? tick (aget last 0)) (identical? epoch (aget last 1)) (identical? (.-dimension eye) (aget last 2))
                     (identical? (.-x eye) (aget last 3)) (identical? (.-y eye) (aget last 4)) (identical? (.-z eye) (aget last 5))
                     (identical? (.-yaw eye) (aget last 6)) (identical? (.-pitch eye) (aget last 7)))
        (set! (.-sensedKey st) #js [tick epoch (.-dimension eye) (.-x eye) (.-y eye) (.-z eye) (.-yaw eye) (.-pitch eye)])
        (set! (.-keyX st) (js/Math.floor (.-x eye)))
        (set! (.-keyZ st) (js/Math.floor (.-z eye)))
        (.clear ^js (.-sensed st)))
      eye)))

(defn memo-key
  "[k writes] that stay equal while every sensed answer does (same tick, raw-world epoch and eye, and no change to the remembered blocks); k is identical, not just equal; nil offline."
  [per]
  (when (sense-fresh! per)
    (let [^js st (:st per)] [(.-sensedKey st) (.-writes st)])))

(defn sense-cell
  "What the body knows of cell pos now, seen from eye, as #js [kind state-id age-ms]: kind 0 unloaded, 1 felt, 2 visible,
  3 remembered, 4 unknown. Slots 3 and 4 keep its answer as sensed and as sensed-js once asked."
  [{:keys [raw st] :as per} ^js eye [x y z :as pos]]
  (let [id (.stateAt ^js raw x y z)]
    (if (< id 0)
      #js [0 -1 0]
      (if-let [felt (rays/feel-cell! raw st eye pos)]
        #js [1 felt 0]
        (if-let [seen (rays/glance-cell! per st eye x y z id)]
          #js [2 seen 0]
          (if-let [age (remembered-age st x y z)]
            #js [3 (store/remembered-id st x y z) age]
            #js [4 -1 0]))))))

(def key-span "Blocks either side of the eye that sensed's numeric cell keys tell apart (a string key past them)." 524288)

(defn sensed-cell
  "sense-cell of pos, cached for one tick while the eye and the world stay the same; nil offline."
  [{:keys [st] :as per} [x y z :as pos]]
  (when-let [eye (sense-fresh! per)]
    (let [^js st st
          dx (+ (- x (.-keyX st)) key-span) dz (+ (- z (.-keyZ st)) key-span)
          ck (if (and (< -1 dx (* 2 key-span)) (< -1 dz (* 2 key-span)) (< -2049 y 2048))
               (+ (* (+ (* dx 2 key-span) dz) 4096) (+ y 2048))
               (str x "," y "," z))
          ^js m (.-sensed st)
          hit (.get m ck)]
      (if (undefined? hit)
        (let [c (sense-cell per eye pos)] (.set m ck c) c)
        hit))))

(defn sensed
  "What the body knows of the cell pos [x y z] now: nil (unloaded or offline), felt, visible, remembered or unknown (see
  the namespace doc). Cached for one tick while the eye and the world stay the same."
  [per pos]
  (when-let [^js c (sensed-cell per pos)]
    (let [hit (aget c 3)]
      (if (undefined? hit)
        (let [id (aget c 1)
              answer (case (aget c 0)
                       0 nil
                       1 (assoc (block-of per id pos) :age-ms 0 :felt true)
                       2 (assoc (block-of per id pos) :age-ms 0 :visible true)
                       3 (assoc (block-of per id pos) :age-ms (aget c 2) :visible false)
                       {:unknown true :pos pos})]
          (aset c 3 answer)
          answer)
        hit))))

(defn pos-js [[x y z]] #js {:x x :y y :z z})

(defn answer-js
  "A sensed answer as JS: blockAt's keys (pos {x y z}, fullCube) plus ageMs, felt, visible, unknown. Built key by key:
  only the properties map needs clj->js."
  [m]
  (when m
    (let [o #js {}]
      (doseq [[k v] m]
        (case k
          :state-id nil
          :pos (aset o "pos" (pos-js v))
          :age-ms (aset o "ageMs" v)
          :full-cube (aset o "fullCube" v)
          :properties (aset o "properties" (clj->js v))
          (aset o (name k) v)))
      o)))

(defn base-js
  "answer-js of block-base: the JS keys of state id that are the same for every cell, kept per id."
  [{:keys [st] :as per} id]
  (let [^js cache (.-jsBases ^js st)]
    (or (.get cache id)
        (let [b (answer-js (block-base per id))] (.set cache id b) b))))

(defn cell-js
  "answer-js of the answer sense-cell c gives for cell x y z, built from base-js (properties copied: each answer owns its own)."
  [per ^js c x y z]
  (let [kind (aget c 0)]
    (case kind
      0 nil
      4 #js {:unknown true :pos #js {:x x :y y :z z}}
      (let [^js o (js/Object.assign #js {} (base-js per (aget c 1)))]
        (when-let [props (.-properties o)] (set! (.-properties o) (js/Object.assign #js {} props)))
        (set! (.-pos o) #js {:x x :y y :z z})
        (set! (.-ageMs o) (aget c 2))
        (case kind
          1 (set! (.-felt o) true)
          2 (set! (.-visible o) true)
          (set! (.-visible o) false))
        o))))

(defn sensed-js
  "sensed as JS, one object per cell and tick (sensed's cache window), shared between callers."
  [per [x y z :as pos]]
  (when-let [^js c (sensed-cell per pos)]
    (let [hit (aget c 4)]
      (if (undefined? hit)
        (let [o (cell-js per c x y z)] (aset c 4 o) o)
        hit))))

(defn cell-of [^js a]
  (when-let [^js pos (some-> a .-pos)]
    (when (every? number? [(.-x pos) (.-y pos) (.-z pos)])
      (mapv #(js/Math.floor %) [(.-x pos) (.-y pos) (.-z pos)]))))

(defn looking
  "The look primitive f (called on p) that, once the head turned to a pos, glances its cell and the cell's 6 neighbours
  and adds the cell's sensed answer to the result as `sensed`."
  [per ^js p ^js f]
  (fn [token ^js a]
    (-> (.call f p token a)
        (.then (fn [r]
                 (if-let [cell (cell-of a)]
                   (do (run! #(rays/glance! per (mapv + cell %)) [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])
                       (js/Object.assign #js {} r #js {:sensed (answer-js (sensed per cell))}))
                   r))))))

(defn wrap
  "The primitives object p with every primitive as it is (blocks and entities stay raw; blockAt is sensedAt; dig, place, jumpPlace and useOn also let memory
  take the true state of their cell, see touching; look glances where it turned, see looking), plus
  seenBlockAt({x,y,z}) and seenBlocks({radius, names, max}) over memory, knownMobs() (known-mobs: the hostiles the body
  has seen or heard, sampled from p's entities), sensedAt, feel and glance ({x,y,z}: answer-js of sensed, feel!,
  glance!), and the perception itself."
  [p per]
  (let [out (js/Object.assign #js {} p)
        cell (fn [^js a] [(js/Math.floor (.-x a)) (js/Math.floor (.-y a)) (js/Math.floor (.-z a))])]
    (aset out "perception" per)
    (when-not (.-rawWorld out) (aset out "rawWorld" (:raw per)))
    (set! (.-mobSource ^js (:st per)) p)
    (aset out "knownMobs" (fn [] (mobs/known-mobs per p)))
    (run! #(when-let [f (aget p %)] (aset out % (rays/touching per p f))) touching-primitives)
    (when-let [f (aget p "look")] (aset out "look" (looking per p f)))
    (aset out "seenBlockAt" (fn [^js a]
                              (let [b (store/seen-block per (cell a))]
                                (clj->js (-> b (dissoc :state-id) (update :pos pos-js))))))
    (aset out "sensedAt" (fn [a] (sensed-js per (cell a))))
    (aset out "blockAt" (aget out "sensedAt"))
    (aset out "feel" (fn [a] (answer-js (feel! per (cell a)))))
    (aset out "glance" (fn [a] (answer-js (glance! per (cell a)))))
    (aset out "seenBlocks" (fn [^js a]
                             (let [q (js->clj (or a #js {}) :keywordize-keys true)]
                               (clj->js (mapv #(update % :pos pos-js) (store/seen-blocks per q))))))
    out))
