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
    A new pass starts when the eye moved or turned, every `:idle-ms` if the world changed (a block or chunk update, the
    daylight, a torch in hand), else every `:still-ms`.
    What is behind the body is seen only once it turns.

  Block memory. Per dimension and 16^3 section: a Uint16Array of stateId + 1 (0 = unknown), a Uint8Array of each
  cell's seen time (whole minutes after the section's :base) and the section's last-seen time.
    A cell more than 255 minutes older than the newest write reads as 255 minutes older.
    Capped at `:cap-bytes` (32 MB, 2730 sections of 12 KB) by forgetting the least recently seen section.
    Saved to and loaded from a file (engine/js/seen-file.mjs).
    A block change in view (cone, line of sight, light) updates memory at once.
    A change out of view leaves the old state, a true memory error.

  Mob memory. The hostile mobs the body has seen or heard, by entity id. The danger checks (jobs.lib.reach and the
  callers of jobs.survival.danger) take their candidates from it, not from every mob the server tracks.
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
  dig, place, jumpPlace and useOn let memory take the true state of their cell once they settle.

  The parts: engine.perception.light (light curve), .store (block memory and reading it), .rays (view cone, sight pass,
  keeping memory current), .mobs (mob memory), .persist (file). This namespace creates, runs and wraps them."
  (:require [engine.perception.light :as light]
            [engine.perception.mobs :as mobs]
            [engine.perception.persist :as persist]
            [engine.perception.rays :as rays]
            [engine.perception.store :as store]))

(def sky-darken light/sky-darken)
(def seeing light/seeing)
(def section-bytes store/section-bytes)
(def seen-block store/seen-block)
(def seen-blocks store/seen-blocks)
(def line-clear? rays/line-clear?)
(def pass! rays/pass!)
(def step! rays/step!)
(def start-listening! rays/start-listening!)
(def touch! rays/touch!)
(def touching-primitives rays/touching-primitives)
(def sense-thing mobs/sense-thing)
(def save! persist/save!)
(def load! persist/load!)

(def defaults
  {:radius 48
   :fov 70                ; vertical field of view, degrees (vanilla's default)
   :aspect (/ 16 9)
   :ray-deg 1             ; ray spacing at the centre of the view
   :pass-ms 1500
   :step-ms 50
   :idle-ms 3000          ; a still body looks again this often, when the world around it changed
   :still-ms 30000        ; ... and this often when nothing did (no block or chunk change, same daylight and torch)
   :move-blocks 0.5
   :turn-deg 2
   :seeing-min 0.2
   :near 4
   :near-torch 7          ; the same, while a torch (or soul torch) is held in either hand
   :cap-bytes (* 32 1024 1024)
   :save-ms 60000
   :stats-ms 60000
   :error-every-ms 60000
   :mob-ms 250            ; a mob sample this often
   :mob-scan 64           ; hostiles within this of the body are sampled
   :hearing 16            ; a mob within this of the eye is heard
   :dark-sight 4          ; a mob standing in the dark (light too low to make out) is seen only within this
   :mob-drift 16})        ; a mob not sensed is forgotten once it could have walked this far

;; ---- create

(defn create
  "A perception over a rawWorld reader. opts override `defaults` (plus :now, a clock in ms)."
  [raw opts]
  (let [o (merge defaults {:now #(js/Date.now)} opts)
        g (rays/grid o)]
    {:raw raw
     :opts o
     :grid g
     :st #js {:stores (js/Map.) :count 0 :cap (max 1 (js/Math.floor (/ (:cap-bytes o) store/section-bytes)))
              :stamp 0 :lastKey -1 :lastSec nil :dim "overworld" :sight nil :visible nil
              :pass nil :lastStart nil :unsubscribe nil
              :mobs (js/Map.) :mobSource nil
              :passes 0 :rays 0 :cells 0 :steps 0 :stepMs 0 :stepMsMax 0}}))

(defn stats [{:keys [st] :as per}]
  (let [^js st st]
    {:passes (.-passes st) :rays (.-rays st) :cells (.-cells st) :steps (.-steps st)
     :step-ms-mean (if (pos? (.-steps st)) (/ (.-stepMs st) (.-steps st)) 0) :step-ms-max (.-stepMsMax st)
     :sections (.-count st) :bytes (* store/section-bytes (.-count st))
     :steps-per-pass (rays/steps-per-pass per) :rays-per-pass (rays/rays-per-pass per)}))

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
    (aset out "knownMobs" (fn [] (mobs/known-mobs per p)))
    (run! #(when-let [f (aget p %)] (aset out % (rays/touching per p f))) touching-primitives)
    (aset out "seenBlockAt" (fn [^js a]
                              (let [b (store/seen-block per [(.-x a) (.-y a) (.-z a)])]
                                (clj->js (update b :pos pos-js)))))
    (aset out "seenBlocks" (fn [^js a]
                             (let [q (js->clj (or a #js {}) :keywordize-keys true)]
                               (clj->js (mapv #(update % :pos pos-js) (store/seen-blocks per q))))))
    out))
