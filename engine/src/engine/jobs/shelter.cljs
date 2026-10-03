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

(defn solid?
  "Whether a block name counts as solid: not nil (an unloaded chunk), not in
  non-solid, and not leaves or a sapling or flower."
  [name]
  (and (some? name)
       (not (non-solid name))
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

(defn night? [p] (not (.-isDay (.self p))))

(defn sleeping? [p] (boolean (.-isSleeping (.self p))))

(defn unsafe-night?
  "Night, awake and not under a roof."
  [p roof-height]
  (and (night? p) (not (sleeping? p)) (not (roofed? p roof-height))))

(defn bed-in-view
  "The remembered bed position in a memory view when it is within radius of
  the body, else nil."
  [p view radius]
  (let [pos (mem/place view :bed)]
    (when (and pos (<= (u/dist (u/self-pos {:primitives p}) pos) radius))
      pos)))

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

(defn log-out-wanted?
  "Whether logging out would let a sleeping player skip the night: it is
  night, no bed is remembered within :bed-radius, :offline-allowed is not
  false, the last log-out was not unsupported, and another player within
  :player-radius is asleep. Takes the primitives, a memory view and the args
  map (missing keys take their defaults). Being roofed does not matter."
  [p view args]
  (boolean (and (not= false (:offline-allowed args))
                (night? p)
                (nil? (bed-in-view p view (:bed-radius args default-bed-radius)))
                (not (log-out-unsupported? view))
                (seq (sleeping-players p (:player-radius args default-player-radius))))))

(defn days-awake
  "In-game days since the latest :slept entry, or nil when none is
  remembered (the body has no known last sleep)."
  [c]
  (when-let [t (:t (ctx/latest c :slept))]
    (/ (- (ctx/now c) t) ms-per-day)))
