(ns jobs.lib.shelter
  "What the night jobs (night, sleep, log-out, dig-in) and the night trigger share: night and roof tests, the bed to
  use, who sleeps on the server, the shelter entry."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.world-files :as world]))

(def ms-per-day
  "One in-game day in engine-clock milliseconds (20 minutes). \"A day\" in memory policies and :max-days-awake is
  this long."
  1200000)

(def default-roof-height 4)

(def default-bed-radius 48)

(def default-player-radius 128)

(def non-solid
  "Block names that are not a roof: air, fluids, and plants and fixtures a body can stand in."
  #{"air" "cave_air" "void_air" "water" "lava" "short_grass" "grass" "tall_grass" "fern" "large_fern"
    "torch" "wall_torch" "snow" "vine" "dead_bush" "seagrass" "tall_seagrass" "fire"})

(def walk-through
  "Blocks a mob or body walks through or over (signs, banners, rails, plates, buttons, levers, carpets, tripwire,
  cobweb). Not solid: neither roof, wall nor floor."
  #"(_sign|_banner|_carpet|_pressure_plate|_button|_rail)$|^(rail|lever|tripwire|tripwire_hook|string|cobweb|redstone_wire)$")

(defn solid?
  "Whether a block name counts as solid: not nil (unloaded), not non-solid or walk-through, and not leaves or a
  sapling."
  [name]
  (and (some? name)
       (not (non-solid name))
       (not (re-find walk-through name))
       (not (.endsWith name "_leaves"))
       (not (.endsWith name "_sapling"))))

(defn cell
  "The block cell {:x :y :z} containing a position."
  [{:keys [x y z]}]
  {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)})

(defn feet [p] (cell (u/self-pos {:primitives p})))


(defn solid-at? [p pos] (solid? (u/block-name p pos)))

(defn roofed?
  "Whether a solid block lies within height blocks straight above the feet cell."
  [p height]
  (let [{:keys [x y z]} (feet p)]
    (boolean (some #(solid-at? p {:x x :y (+ y %) :z z}) (range 1 (inc height))))))

(def buried-scan 32)

(def buried-thickness 3)

(defn sky-light-at
  "Sky light (0-15, before the night's darkening) of the cell, or nil when no light data is loaded there."
  [p {:keys [x y z]}]
  (let [raw (some-> (aget p "perception") :raw)]
    (when (and raw (<= 0 (.stateAt ^js raw x y z)))
      (bit-shift-right (.lightAt ^js raw x y z) 4))))

(defn buried-by-column?
  "Fallback without light data: at least buried-thickness solid blocks in the column within buried-scan above the feet."
  [p]
  (let [{:keys [x y z]} (feet p)]
    (<= buried-thickness
        (count (take buried-thickness
                     (filter #(solid-at? p {:x x :y (+ y %) :z z}) (range 1 (inc buried-scan))))))))

(defn buried?
  "The body is underground: no sky light at its feet and head cells. A cell at 0 is sealed from the sky, so the night
  changes nothing there. A ravine, shaft or cave mouth keeps sky light and stays unsafe. Without light data,
  buried-by-column? decides."
  [p]
  (let [{:keys [x y z] :as f} (feet p)
        lights (keep #(sky-light-at p %) [f {:x x :y (inc y) :z z}])]
    (if (empty? lights)
      (buried-by-column? p)
      (every? zero? lights))))

(defn night? [p] (not (.-isDay (.self p))))

(defn sleeping? [p] (boolean (.-isSleeping (.self p))))

(defn unsafe-night?
  "Night, awake, not under a roof and not buried underground."
  [p roof-height]
  (and (night? p) (not (sleeping? p)) (not (roofed? p roof-height)) (not (buried? p))))

(defn bed-in-view
  "The remembered bed position when it is within radius of the body, else nil."
  [p view radius]
  (let [pos (mem/place view :bed)]
    (when (and pos (<= (u/dist (u/self-pos {:primitives p}) pos) radius))
      pos)))

(def slept-tonight-ms
  "A :slept entry younger than this counts as \"slept tonight\" (half an in-game day)."
  (/ ms-per-day 2))

(defn walkable-at?
  "Whether the cell at pos is one a body stands or walks in: not solid, or a bed."
  [p pos]
  (let [n (u/block-name p pos)]
    (and (some? n) (or (not (solid? n)) (.endsWith n "_bed")))))

(defn doorway?
  "Whether the cell is a gap in a wall: solid on both sides along one horizontal axis and free on both ends of the other."
  [p {:keys [x y z]}]
  (let [solid (fn [dx dz] (solid-at? p {:x (+ x dx) :y y :z (+ z dz)}))
        free (fn [dx dz] (not (solid dx dz)))]
    (boolean (or (and (solid 1 0) (solid -1 0) (free 0 1) (free 0 -1))
                 (and (solid 0 1) (solid 0 -1) (free 1 0) (free -1 0))))))

(def seen-bed-radius 6)

(defn bed-occupied?
  "Whether the bed at pos is occupied as a player sees it: the bed block's occupied state, or another player lying within
  a block of it (a bed is two cells)."
  [p {:keys [x y z] :as pos}]
  (let [b (u/block-at p pos)
        props (when b (or (.-properties b) (some-> (.-getProperties b) (.call b))))
        me (.-username (.self p))]
    (boolean (or (true? (some-> props .-occupied))
                 (= "true" (some-> props .-occupied str))
                 (some #(and (.-sleeping %) (not= me (.-username %))
                             (<= (js/Math.abs (- (.-x (.-pos %)) x)) 1.5)
                             (<= (js/Math.abs (- (.-z (.-pos %)) z)) 1.5)
                             (<= (js/Math.abs (- (.-y (.-pos %)) y)) 1.5))
                       (array-seq (.entities p #js {:radius 6 :kind "player" :max 32})))))))

(defn in-own-zone?
  "Whether pos lies in a zone the body owns. kn is the engine world (nil: no zone known)."
  [p kn {:keys [x y z]}]
  (let [me (.-username (.self p))]
    (boolean (some #(and (zones/in-box? [x y z] %) (zones/same-owner? (:owner %) me)) (when kn (world/zones kn))))))

(defn bed-permit
  "A predicate (fn [pos]) for whether the body may use the bed at pos: any bed, in anyone's zone or claim (sleeping sets
  only the sleeper's own spawn), unless it is occupied; a bed in a zone the body owns is always a candidate (the server
  refuses an occupied one, which is a failed sleep). kn is the engine world (nil: no zone known)."
  [p kn _now]
  (fn [pos]
    (or (in-own-zone? p kn pos)
        (not (bed-occupied? p pos)))))

(defn seen-bed
  "A bed block the body can see in its room: found by a flood fill from its feet over free cells at feet height, within
  seen-bed-radius (a bed behind a wall is not seen), skipping beds permit? refuses (default all permitted). The nearest
  cell of it, or nil."
  ([p] (seen-bed p (constantly true)))
  ([p permit?]
  (let [{:keys [x y z] :as start} (feet p)]
    (loop [queue [start] seen #{start}]
      (when-let [pos (first queue)]
        (let [named (u/block-name p pos)]
          (if (and (some? named) (.endsWith named "_bed") (not= pos start) (permit? pos))
            pos
            (let [next (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                             :let [n {:x (+ (:x pos) dx) :y y :z (+ (:z pos) dz)}]
                             :when (and (not (seen n))
                                        (<= (max (js/Math.abs (- (:x n) x)) (js/Math.abs (- (:z n) z))) seen-bed-radius)
                                        (walkable-at? p n))]
                         n)]
              (recur (into (subvec (vec queue) 1) next) (into seen next))))))))))

(def sleep-failed-policy
  "The :sleep-failed entry the night job writes when a sleep ended without sleeping (bed taken, monsters near, ...):
  one, five minutes, so the night trigger does not refire on the same bed every cooldown."
  {:cap 1 :ttl 300000})

(def urgent-bed-radius 128)

(def max-days-awake 3)

(defn days-awake-in
  "In-game days since the latest :slept entry in view, or nil when none is remembered."
  [view]
  (when-let [t (:t (mem/latest view :slept))]
    (/ (- (:now view) t) ms-per-day)))

(defn bed-radius
  "How far a remembered bed may be: :bed-radius, or :urgent-bed-radius once the latest :slept entry is :max-days-awake
  in-game days old (phantoms come). No :slept entry is never overdue."
  [view {:keys [bed-radius urgent-bed-radius max-days-awake]
         :or {bed-radius default-bed-radius urgent-bed-radius urgent-bed-radius max-days-awake max-days-awake}}]
  (let [days (days-awake-in view)]
    (if (and days (>= days max-days-awake)) urgent-bed-radius bed-radius)))

(defn bed-to-use
  "The bed a body awake at night sleeps in: one it sees (seen-bed), else the remembered :bed within radius, under any
  roof or none. Never one permit? refuses (occupied) or one in an unexpired :bed-unreachable entry; none after a sleep
  tonight (:slept within half an in-game day) or while a :sleep-failed entry is unexpired. nil otherwise."
  [p view radius permit?]
  (when (and (night? p) (not (sleeping? p))
             (zero? (mem/count-in view :slept slept-tonight-ms))
             (empty? (mem/entries view :sleep-failed)))
    (let [ok? (fn [pos] (and (permit? pos) (not-any? #(= pos (:pos (:data %))) (mem/entries view :bed-unreachable))))]
      (or (seen-bed p ok?)
          (when-let [pos (bed-in-view p view radius)]
            (when (ok? pos) pos))))))

(def bed-place-failed-policy
  "Policy of the :bed-place-failed entry the night job writes when it could not set up a carried bed: one, ten
  minutes."
  {:cap 1 :ttl 600000})

(def bed-placed-policy
  "Policy of the :bed-placed entries: a bed the night job put down outside the body's own zones, picked up by day."
  {:cap 4 :ttl (* 7 ms-per-day)})

(defn carried-bed
  "The name of a bed item the body carries (any colour), or nil."
  [p]
  (some #(when (.endsWith (:name %) "_bed") (:name %)) (u/inventory p)))

(defn near-failed-place?
  "Whether a :bed-place-failed entry still holds: the body is within 6 blocks of where it failed (no position: always)."
  [p entry]
  (let [pos (:pos (:data entry))]
    (or (nil? pos) (< (u/dist (u/self-pos {:primitives p}) pos) 6))))

(defn bed-place-wanted?
  "Whether to put a carried bed down: night, awake, a bed item carried, no bed-to-use, no sleep tonight, and no
  :bed-place-failed or :sleep-failed entry. Roofed or in the open."
  [p view radius permit?]
  (boolean (and (night? p) (not (sleeping? p))
                (some? (carried-bed p))
                (zero? (mem/count-in view :slept slept-tonight-ms))
                (not-any? #(near-failed-place? p %) (mem/entries view :bed-place-failed))
                (empty? (mem/entries view :sleep-failed))
                (nil? (bed-to-use p view radius permit?)))))

(defn bed-to-collect
  "By day: the latest :bed-placed position while a bed block still stands there, else nil."
  [p view]
  (when-not (night? p)
    (let [pos (:pos (:data (mem/latest view :bed-placed)))]
      (when (and pos (.endsWith (or (u/block-name p pos) "") "_bed"))
        pos))))

(defn bed
  "The remembered bed position when it is within radius of the body, else nil."
  [c radius]
  (bed-in-view (:primitives c) (ctx/view c) radius))

(defn sleeping-players
  "Other players within radius the body sees asleep (the primitives report a sleeping pose only in sight)."
  [p radius]
  (let [me (.-username (.self p))]
    (filterv #(and (.-sleeping %) (not= me (.-username %)))
             (array-seq (.entities p #js {:radius radius :kind "player" :max 32})))))

(defn log-out-unsupported?
  "Whether the latest :log-out entry says the server does not support logging out."
  [view]
  (= "unsupported" (:status (:data (mem/latest view :log-out)))))

(def morning-tick
  "The first time of day (ticks) that self().isDay counts as day again after a night."
  23461)

(def dusk-tick
  "The first time of day (ticks) that self().isDay counts as night."
  12542)

(def ms-per-tick 50)

(def morning-margin-ms
  "Extra time away, so a body that logged out until morning comes back after the night has ended."
  2000)

(defn ms-until-morning
  "Milliseconds until the night ends at time of day t (ticks), plus morning-margin-ms; 0 by day."
  [t]
  (if (or (< t dusk-tick) (>= t morning-tick))
    0
    (+ (* ms-per-tick (- morning-tick t)) morning-margin-ms)))

(defn ms-since-dusk
  "Milliseconds since this night began at time of day t (ticks); 0 by day."
  [t]
  (if (or (< t dusk-tick) (>= t morning-tick)) 0 (* ms-per-tick (- t dusk-tick))))

(defn tonight
  "The engine time this night began: entries written since are tonight's."
  [p view]
  (- (:now view) (ms-since-dusk (.-timeOfDay (.self p)))))

(defn latest-since [view kind t]
  (let [e (mem/latest view kind)]
    (when (and e (>= (:t e) t)) e)))

(defn others-asleep?
  "Whether someone on the server sleeps tonight, as a player can tell: the latest :sleep-status entry tonight (the
  action bar's count, sent when it changes) counts a sleeper or says the night is skipped; or a sleeping player is
  in sight; or the body is back from a log-out tonight and no count has come since its return (:online): unknown is
  taken as still asleep."
  [p view]
  (let [since (tonight p view)
        status (latest-since view :sleep-status since)
        out (latest-since view :log-out since)
        back (:t (mem/latest view :online))]
    (boolean (or (and out back (#{"ok" "cut"} (:status (:data out))) (or (nil? status) (< (:t status) back)))
                 (and status (or (:skipping (:data status)) (pos? (:sleeping (:data status) 0))))
                 (seq (sleeping-players p default-player-radius))))))

(defn log-out-for-sleepers?
  "Night, awake, someone else asleep (others-asleep?), and no log-out tonight that failed (unsupported, closed)."
  [p view]
  (let [out (latest-since view :log-out (tonight p view))]
    (and (night? p) (not (sleeping? p))
         (not (log-out-unsupported? view))
         (or (nil? out) (contains? #{"ok" "cut"} (:status (:data out))))
         (others-asleep? p view))))

(defn days-awake
  "In-game days since the latest :slept entry, or nil when none is remembered."
  [c]
  (days-awake-in (ctx/view c)))

(defn shut-in?
  "Whether the body is still shut in by its shelter entry. Never for a mended :room (its door is the way out). With a
  pit :start, when below that height. With a :door, when a door cell is solid. With neither, when under a solid
  block within default-roof-height."
  [p {:keys [start door room]}]
  (cond
    room false
    start (< (:y (feet p)) (:y start))
    door (boolean (some #(solid-at? p %) door))
    :else (roofed? p default-roof-height)))

(defn in-own-shelter
  "The shelter entry data when the body stands in its :pos and is shut in it, else nil."
  [p entry]
  (when (and entry (= (:pos entry) (feet p)) (shut-in? p entry))
    entry))

(defn sheltered-in
  "The body's latest :shelter entry when the body stands in its :pos and is shut in it, else nil."
  [c]
  (in-own-shelter (:primitives c) (:data (ctx/latest c :shelter))))

(def trapped-policy
  "Policy of the :shelter-trapped entries written when leave! found no way out: up to 3, five minutes."
  {:cap 3 :ttl 300000})

(defn shut-in-by-day?
  "By day: the body is shut in its own shelter (data) and no :shelter-trapped entry (data) says leave! found no way
  out of this cell. Never at night."
  [p shelter trapped]
  (boolean (and (not (night? p))
                (in-own-shelter p shelter)
                (not= (:pos shelter) (:pos trapped)))))

(defn shelter-hint
  "{:inside-own-shelter pos :hint text} when the body is sealed in its own shelter (a go-to, attack or hunt that cannot
  reach its target says so), else nil."
  [c]
  (when-let [{{:keys [x y z]} :pos :as entry} (sheltered-in c)]
    {:inside-own-shelter (:pos entry)
     :hint (str "the body is sealed in its own shelter at [" x " " y " " z "]: the shelter job lets it out by day "
                "(jobs.survival.night), or run jobs.survival.dig-in leave")}))
