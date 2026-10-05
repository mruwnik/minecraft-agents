(ns engine.jobs.shelter
  "What the night-survival jobs (shelter, sleep, log-out, dig-in) and the
  night-unsafe and player-sleeping-nearby triggers share: night and roof
  tests, the known bed, the log-out condition, the shelter entry. See README.md, Job library."
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.memory :as mem]))

(def ms-per-day
  "One in-game day in engine-clock milliseconds (20 minutes). The engine has
  no world day counter, so \"a day\" in memory policies and in
  :max-days-awake is this long."
  1200000)

(def default-roof-height 4)

(def default-bed-radius 48)

(def default-player-radius 128)

(def non-solid
  "Block names that are not a roof: air, fluids and the plants and fixtures
  a body can stand in."
  #{"air" "cave_air" "void_air" "water" "lava" "short_grass" "grass" "tall_grass" "fern" "large_fern"
    "torch" "wall_torch" "snow" "vine" "dead_bush" "seagrass" "tall_seagrass" "fire"})

(def walk-through
  "Names of blocks a mob or body walks through or over: signs and banners, rails, pressure plates, buttons, levers,
  carpets, tripwire and cobweb. Not solid, so neither a roof, a wall nor a floor."
  #"(_sign|_banner|_carpet|_pressure_plate|_button|_rail)$|^(rail|lever|tripwire|tripwire_hook|string|cobweb|redstone_wire)$")

(defn solid?
  "Whether a block name counts as solid: not nil (an unloaded chunk), not in
  non-solid, not walk-through, and not leaves or a sapling or flower."
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
  "A solid block within height blocks straight above the feet cell (the
  cells y+1 .. y+height)."
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
  "The body is underground: no sky light at its feet and head cells. Sky light reaches a cell through any opening to
  the open sky (straight down a shaft undimmed, one less per step sideways or diagonal), and the night's mobs spawn
  or wander in there; a cell at 0 is sealed from the sky, so the night changes nothing (mobs spawn by block light).
  A ravine, a shaft or a cave mouth keeps sky light and stays unsafe. Without light data, the column heuristic
  (buried-by-column?) decides."
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
  "The remembered bed position in a memory view when it is within radius of
  the body, else nil."
  [p view radius]
  (let [pos (mem/place view :bed)]
    (when (and pos (<= (u/dist (u/self-pos {:primitives p}) pos) radius))
      pos)))

;; a :slept entry younger than this counts as "slept tonight" (the night is under half an in-game day)
(def slept-tonight-ms (/ ms-per-day 2))

(defn sleep-wanted
  "The remembered bed within radius when the body, awake at night, has not slept tonight (no :slept entry within half an
  in-game day) and the bed was not given up on (no unexpired :bed-unreachable entry at it); else nil. The roof is not
  asked: this is for a body sheltered already, which should set its respawn point in its own bed."
  [p view radius]
  (when (and (night? p) (not (sleeping? p)))
    (let [bed (bed-in-view p view radius)]
      (when (and bed
                 (zero? (mem/count-in view :slept slept-tonight-ms))
                 (not-any? #(= bed (:pos (:data %))) (mem/entries view :bed-unreachable)))
        bed))))

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
  "Whether the latest :log-out entry in a memory view says the server does not
  support logging out."
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
  "Whether logging out is wanted: it is night, no bed is remembered within :bed-radius, :offline-allowed is not
  false, the last log-out was not unsupported, and :others holds: :asleep-nearby (the default) wants another player
  within :player-radius asleep (logging out lets them skip the night); :online wants another player in the server's
  player list (the night shelter's step 2: away for the night instead of digging in). Takes the primitives, a memory
  view and the args map (missing keys take their defaults). Being roofed does not matter."
  [p view args]
  (boolean (and (not= false (:offline-allowed args))
                (night? p)
                (nil? (bed-in-view p view (:bed-radius args default-bed-radius)))
                (not (log-out-unsupported? view))
                (seq (if (= :online (:others args))
                       (other-players-online p)
                       (sleeping-players p (:player-radius args default-player-radius)))))))

(defn days-awake
  "In-game days since the latest :slept entry, or nil when none is
  remembered (the body has no known last sleep)."
  [c]
  (when-let [t (:t (ctx/latest c :slept))]
    (/ (- (ctx/now c) t) ms-per-day)))

(defn shut-in?
  "Whether the body is still in shelter entry: never for a mended room (:room, its own door is the way out); below a
  pit's :start height (a stair stopped part way counts), behind a solid :door cell, or, with neither (a roof over a
  shaft that was walled already), under a solid block within default-roof-height."
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
  "The :shelter-trapped entries a shelter writes when leave! found no way out: one per failed attempt, kept 5 minutes."
  {:cap 3 :ttl 300000})

(defn shut-in-by-day?
  "By day, the body is shut in its own latest shelter entry (data) and no :shelter-trapped entry (data) says leave!
  found no way out of this very cell. Never at night: the shelter holds then."
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
