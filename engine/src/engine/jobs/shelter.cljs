(ns engine.jobs.shelter
  "What the night-survival jobs (shelter, sleep, log-out, dig-in) and the night-unsafe and player-sleeping-nearby
  triggers share: night and roof tests, the known bed, the log-out condition, the shelter entry."
  (:require [engine.ctx :as ctx]
            [engine.access.zones :as zones]
            [engine.jobs.util :as u]
            [engine.memory :as mem]
            [engine.world :as world]))

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

(def hut-radius
  "How near a bed must be for a roofed body to count it as the bed of the room it stands in (blocks)."
  12)

(defn walkable-at?
  "Whether the cell at pos is one a body stands or walks in: not solid, or a bed."
  [p pos]
  (let [n (u/block-name p pos)]
    (and (some? n) (or (not (solid? n)) (.endsWith n "_bed")))))

(defn in-room?
  "Whether the body stands in a room, not a one-wide tunnel: its cell is one of a 2x2 square of free cells at feet
  height. A one-wide tunnel (a miner's), its bends and junctions have no such square; a hut has."
  [p]
  (let [{:keys [x y z]} (feet p)]
    (boolean (some (fn [[ox oz]]
                     (every? #(walkable-at? p {:x (+ x (first %)) :y y :z (+ z (second %))})
                             [[ox oz] [(+ ox 1) oz] [ox (+ oz 1)] [(+ ox 1) (+ oz 1)]]))
                   [[0 0] [-1 0] [0 -1] [-1 -1]]))))

(defn doorway?
  "Whether the cell is a gap in a wall: solid on both sides along one horizontal axis and free on both ends of the other."
  [p {:keys [x y z]}]
  (let [solid (fn [dx dz] (solid-at? p {:x (+ x dx) :y y :z (+ z dz)}))
        free (fn [dx dz] (not (solid dx dz)))]
    (boolean (or (and (solid 1 0) (solid -1 0) (free 0 1) (free 0 -1))
                 (and (solid 0 1) (solid 0 -1) (free 1 0) (free -1 0))))))

(def seen-bed-radius 6)

(defn bed-permit
  "A predicate (fn [pos]) for whether the body may use the bed at pos: false inside another owner's zone that does not
  allow :take, or another owner's claim (zones and claims are a rule jobs consult, see engine.access.zones); true
  otherwise, also when no zone list was read. kn is the engine world (nil: everything is permitted), now the clock (ms)."
  [p kn now]
  (if (nil? kn)
    (constantly true)
    (let [in {:zones (world/zones kn)
              :claims (world/live-claims (world/area-claims kn) now)
              :footprints {}
              :plan-cells #{}
              :self (.-username (.self p))
              :now now
              :action :take}]
      (fn [{:keys [x y z]}]
        (not (#{:zone :claim} (:reason (zones/verdict (assoc in :cell [x y z])))))))))

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

(defn hut-bed
  "The bed of the room the body stands in: roofed within roof-height and in a room (not a tunnel), then the remembered
  :bed within hut-radius, else a bed it sees in the room; nil otherwise. A bed the body does not remember is found
  here, so a second one is not put down beside it. A bed permit? refuses (another owner's zone or claim, see
  bed-permit; default all permitted) is not the bed of the room."
  ([p view roof-height] (hut-bed p view roof-height (constantly true)))
  ([p view roof-height permit?]
   (when (and (roofed? p roof-height) (in-room? p))
     (or (let [pos (mem/place view :bed)]
           (when (and pos (<= (u/dist (u/self-pos {:primitives p}) pos) hut-radius) (permit? pos)) pos))
         (seen-bed p permit?)))))

(def sleep-failed-policy
  "The :sleep-failed entry a shelter writes when a roofed sleep ended without sleeping (bed taken, monsters near, ...):
  one, five minutes, so night-unsafe does not refire on the same bed every cooldown."
  {:cap 1 :ttl 300000})

(defn sleep-wanted
  "The bed of the room a roofed body at night, awake, should sleep in (see hut-bed): none slept in tonight (no :slept
  entry within half an in-game day), not given up on (no unexpired :bed-unreachable at it) and no unexpired
  :sleep-failed entry; else nil. permit? as in hut-bed."
  ([p view roof-height] (sleep-wanted p view roof-height (constantly true)))
  ([p view roof-height permit?]
   (when (and (night? p) (not (sleeping? p)))
     (let [bed (hut-bed p view roof-height permit?)]
       (when (and bed
                  (zero? (mem/count-in view :slept slept-tonight-ms))
                  (not-any? #(= bed (:pos (:data %))) (mem/entries view :bed-unreachable))
                  (empty? (mem/entries view :sleep-failed)))
         bed)))))

(def bed-place-failed-policy
  "Policy of the :bed-place-failed entry the shelter writes when it could not set up a carried bed: one, ten minutes."
  {:cap 1 :ttl 600000})

(defn carried-bed
  "The name of a bed item the body carries (any colour), or nil."
  [p]
  (some #(when (.endsWith (:name %) "_bed") (:name %)) (u/inventory p)))

(defn bed-place-wanted?
  "Whether a sheltered body should put a carried bed down: night, awake, roofed within roof-height and in a room, no bed
  of the room (remembered near or seen), a bed item carried, no sleep tonight, and no :bed-place-failed or
  :sleep-failed entry. permit? as in hut-bed."
  ([p view roof-height] (bed-place-wanted? p view roof-height (constantly true)))
  ([p view roof-height permit?]
  (boolean (and (night? p) (not (sleeping? p))
                (roofed? p roof-height)
                (some? (carried-bed p))
                (zero? (mem/count-in view :slept slept-tonight-ms))
                (empty? (mem/entries view :bed-place-failed))
                (empty? (mem/entries view :sleep-failed))
                (in-room? p)
                (nil? (hut-bed p view roof-height permit?))))))

(defn bed
  "The remembered bed position when it is within radius of the body, else nil."
  [c radius]
  (bed-in-view (:primitives c) (ctx/view c) radius))

(defn sleeping-players
  "Other players within radius that are asleep."
  [p radius]
  (let [me (.-username (.self p))]
    (filterv #(and (.-sleeping %) (not= me (.-username %)))
             (array-seq (.entities p #js {:radius radius :kind "player" :max 32})))))

(defn log-out-unsupported?
  "Whether the latest :log-out entry says the server does not support logging out."
  [view]
  (= "unsupported" (:status (:data (mem/latest view :log-out)))))

(defn other-players-online
  "Usernames of the other players in the server's player list (the tab list a player sees), from self().players."
  [p]
  (let [s (.self p)
        me (.-username s)]
    (filterv #(not= me %) (array-seq (or (.-players s) #js [])))))

(def morning-tick
  "The first time of day (ticks) that self().isDay counts as day again after a night."
  23461)

(def ms-per-tick 50)

(def morning-margin-ms
  "Extra time away, so a body that logged out until morning comes back after the night has ended."
  2000)

(defn ms-until-morning
  "Milliseconds until the night ends at time of day t (ticks), plus morning-margin-ms; 0 by day. A whole night is
  about 9 minutes, under the offline primitive's ten-minute cap."
  [t]
  (if (or (< t 12542) (>= t morning-tick))
    0
    (+ (* ms-per-tick (- morning-tick t)) morning-margin-ms)))

(defn log-out-wanted?
  "Whether logging out is wanted. All of: night; no bed remembered within :bed-radius; :offline-allowed not false;
  the last log-out was not unsupported; and :others holds. :asleep-nearby (default) wants another player within
  :player-radius asleep. :online wants another player in the server's player list. A roof does not matter.
  Args missing keys take their defaults."
  [p view args]
  (boolean (and (not= false (:offline-allowed args))
                (night? p)
                (nil? (bed-in-view p view (:bed-radius args default-bed-radius)))
                (not (log-out-unsupported? view))
                (seq (if (= :online (:others args))
                       (other-players-online p)
                       (sleeping-players p (:player-radius args default-player-radius)))))))

(defn days-awake
  "In-game days since the latest :slept entry, or nil when none is remembered."
  [c]
  (when-let [t (:t (ctx/latest c :slept))]
    (/ (- (ctx/now c) t) ms-per-day)))

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
                "(jobs.survival.shelter), or run jobs.survival.dig-in leave")}))
