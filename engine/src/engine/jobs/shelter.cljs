(ns engine.jobs.shelter
  "What the night-survival jobs (shelter, sleep, log-out, dig-in) and the
  night-unsafe trigger share: night and roof tests, the known bed, the
  shelter entry. See README.md, Job library."
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

(defn bed
  "The remembered bed position when it is within radius of the body, else nil."
  [c radius]
  (let [pos (mem/place (ctx/view c) :bed)]
    (when (and pos (<= (u/dist (u/self-pos c) pos) radius))
      pos)))

(defn days-awake
  "In-game days since the latest :slept entry, or nil when none is
  remembered (the body has no known last sleep)."
  [c]
  (when-let [t (:t (ctx/latest c :slept))]
    (/ (- (ctx/now c) t) ms-per-day)))
