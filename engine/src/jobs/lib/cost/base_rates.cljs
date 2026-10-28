(ns jobs.lib.cost.base-rates
  "Base rates: how much of a resource the ground holds before a body has looked, an input to costs (not a cost).
  The numbers are the game's own placements, in base_rates.json beside this file, which tools/base-rates.mjs writes
  from the server jar; nothing here counts blocks of the loaded world. Ores (coal, iron, copper, gold, redstone, lapis,
  diamond, emerald, the json's keys) and the farm animals (animal-pack-density, animal-ground-share). Pure.

  Every fn takes the body: a map of what the body knows, as jobs.lib.look/beliefs gives it. Today only :biome-at is
  read, a fn of a pos -> the biome the body saw there, nil for ground it has not seen."
  (:require [shadow.resource :as rc]))

(def ^:private data (js->clj (js/JSON.parse (rc/inline "jobs/lib/cost/base_rates.json")) :keywordize-keys true))

(def biomes "The biomes that carry any ore: the overworld's." (:biomes data))

(def ^:private chunk-area 256)

(defn- height-share
  "The share of a placement's veins that start at height y, as the game draws it: uniform over min..max, or for a
  trapezoid the sum of two even draws that together span it."
  [{:keys [shape] low :min high :max} y]
  (let [span (- high low)
        k (- y low)]
    (cond
      (not (<= 0 k span)) 0
      (= shape "uniform") (/ 1 (inc span))
      :else (let [m (quot span 2)
                  n (- span m)]
              (/ (inc (min k m (- span k))) (* (inc m) (inc n)))))))

(defn density
  "Expected blocks of resource per block of ground at pos's height in biome: over the resource's placements there,
  veins per chunk x the vein's size x the share of veins at that height, per chunk layer. The size is the game's
  nominal one and the ground is taken as all stone, so caves, air and other fill make the real count lower.
  With :exposed? the ground is a face that was open to air when the ore was placed (a cave wall), where the game drops
  each vein block with the placement's :air chance. 0 for a resource or biome with no placement. body is not read yet."
  [resource pos biome _body & {:keys [exposed?]}]
  (let [y (js/Math.floor (:y pos))]
    (->> (get (:ores data) (keyword resource))
         (filter #(some #{biome} (or (:biomes %) biomes)))
         (map #(/ (* (:count %) (:size %) (height-share % y) (if exposed? (- 1 (:air %)) 1)) chunk-area))
         (reduce + 0))))

(def ^:private hosts (into {} (map (fn [[k v]] [(name k) (set v)])) (:hosts data)))

(defn host?
  "Whether resource's veins replace block (stone, granite, diorite, andesite, deepslate, tuff: the game's ore targets):
  other ground (dirt, sand, gravel) holds none."
  [resource block]
  (contains? (get hosts resource) block))

(defn host-blocks "The blocks resource's veins replace." [resource] (get hosts resource #{}))

(defn biome-chance
  "The chance (0..1) that the ground at pos is biome. 1 or 0 where the body has seen that ground; for unseen ground
  a very simple prior: every ore-carrying biome alike."
  [biome pos body]
  (let [seen (when-let [biome-at (:biome-at body)] (biome-at pos))]
    (cond
      seen (if (= seen biome) 1 0)
      (some #{biome} biomes) (/ 1 (count biomes))
      :else 0)))

(defn expected-density
  "density at pos when its biome is not given: each biome's density weighed by its biome-chance, so it holds for
  seen and unseen ground alike. Option :exposed? as density's."
  [resource pos body & {:keys [exposed?]}]
  (reduce + 0 (map #(* (biome-chance % pos body) (density resource pos % body :exposed? exposed?)) biomes)))

(def ^:private biome-set (set biomes))

(defn- unseen-density
  "expected-density at height y for a body that has seen no biome: each placement's veins times the share of the
  ore-carrying biomes it covers, in one pass over the placements."
  [placements y]
  (let [n (count biomes)]
    (reduce (fn [acc p]
              (let [share (height-share p y)]
                (if (zero? share)
                  acc
                  (+ acc (/ (* (:count p) (:size p) share (count (filter biome-set (or (:biomes p) biomes))))
                            (* chunk-area n))))))
            0 placements)))

(defn peak-density
  "The most expected-density of resource at any height from -64 to 64, where mining is done, the figure of the best layer to look
  in; 0 for a resource with no placement."
  [resource body]
  (if (:biome-at body)
    (apply max 0 (for [y (range -64 65)] (expected-density resource {:y y} body)))
    (let [placements (get (:ores data) (keyword resource))]
      (reduce max 0 (map #(unseen-density placements %) (range -64 65))))))

(def lava-level
  "The lowest feet height whose cave air the game leaves dry: its aquifers fill open cells below y -54 with lava (the
  overworld's fluid picker in NoiseBasedChunkGenerator, a constant of the game's code, not of the worldgen data)."
  -54)

(defn best-level
  "The height from lava-level to 64 where a body that has seen no biome expects the most of resource (the lowest of
  ties), where a tunnel for it pays best; nil when it has no placement or its most lies at 64 or above (coal, emerald):
  no tunnel height beats where the body stands."
  [resource]
  (let [placements (get (:ores data) (keyword resource))
        ys (range lava-level 65)
        best (reduce (fn [a y] (if (> (unseen-density placements y) (unseen-density placements a)) y a)) ys)]
    (when (and (seq placements) (pos? (unseen-density placements best)) (< best 64)) best)))

(def animal-pack-density
  "Expected packs (of 4 adults) of a farm animal per block of ground where its kind spawns, from the game's
  chunk-generation spawns in plains: a creature pack in about 0.11 of chunks (a 0.1 chance, drawn again while it holds),
  its kind drawn by spawn weight out of 42 (sheep 12, pig 10, chicken 10, cow 8, horse 1, donkey 1)."
  (into {} (map (fn [[k w]] [k (/ (* (/ 0.1 0.9) (/ w 42)) chunk-area)])) {"sheep" 12 "pig" 10 "chicken" 10 "cow" 8}))

(def animal-ground-share
  "The share of the land a body walks whose biome spawns farm animals (plains, forests, taiga, hills, swamp, savanna,
  jungle) against the land that spawns none (desert, badlands, snowy plains, mushroom fields, beaches): a rough prior."
  0.6)

(defn- table-of [json] (js->clj (js/JSON.parse json) :keywordize-keys true))

(def ^:private solid-data (table-of (rc/inline "jobs/lib/cost/solid_shares.json")))

(def ^:private solid-shares (vec (:sections solid-data)))

(def ^:private reach (mapv vec (:reach solid-data)))

(def ^:private sky-shares (vec (:sections (table-of (rc/inline "jobs/lib/cost/sky_shares.json")))))

(defn- between
  "[i f]: height y lies f of the way from the centre of table's section i to that of i + 1, the end ones past the table's
  ends."
  [table y]
  (let [t (-> (/ (- y (:y (first table)) 8) 16) (max 0) (min (dec (count table))))
        i (min (js/Math.floor t) (- (count table) 2))]
    [i (- t i)]))

(defn- mix "x-of section index, linear between sections i and i + 1 at f." [[i f] x-of] (+ (* (- 1 f) (x-of i)) (* f (x-of (inc i)))))

(defn- prior-at
  "{:a :b} of a table of Betas by section height at height y, a-of and b-of reading a section's: linear in y between the
  sections' centres, the end ones past the table's ends."
  [table y a-of b-of]
  (let [w (between table y)]
    {:a (mix w #(a-of (table %))) :b (mix w #(b-of (table %)))}))

(defn solid-prior
  "{:a :b} the Beta prior on the share of solid among the blocks sight meets round height y (the open blocks and the
  solid ones beside them) before a body has looked: fitted to the 3x3x3-section windows of the recorded terrain
  (solid_shares.json, tools/solid-shares.mjs)."
  [y]
  (prior-at solid-shares y :a :b))

(defn sky-share
  "The chance an open block round height y has sky light `level` (1-15) or more, with open blocks seen there and lit of
  them at that sky light: the posterior mean over a Beta fitted to the 3x3x3-section windows of the recorded terrain
  (sky_shares.json, tools/sky-shares.mjs)."
  [y level lit open]
  (let [{:keys [a b]} (prior-at sky-shares y #(nth (:a %) (dec level)) #(nth (:b %) (dec level)))]
    (/ (+ a lit) (+ a b open))))

(defn solid-share
  "The share of solid expected among the blocks sight meets round height y, with solid and open those it has met in the
  window round height seen's section: their posterior mean over that section's prior, carried to y by the slope of the
  share y - seen away on the share at seen's section in the recorded terrain (reach in solid_shares.json, in [0, 1]):
  the posterior at seen, the prior at y where the two heights' shares are unrelated."
  [y seen solid open]
  (let [s (-> (js/Math.floor (/ (- seen (:y (first solid-shares))) 16)) (max 0) (min (dec (count solid-shares))))
        {sa :a sb :b} (solid-shares s)
        {:keys [a b]} (solid-prior y)
        slope (mix (between solid-shares (+ y (- (:y (solid-shares s)) seen -8))) (reach s))]
    (-> (+ (/ a (+ a b)) (* slope (- (/ (+ sa solid) (+ sa sb solid open)) (/ sa (+ sa sb))))) (max 0) (min 1))))

(defn open-run-chance
  "The chance every block of a column from height y0 up to y1 (not counted) is open, each at 1 - solid-share at its own
  height with solid and open those sight has met round height seen; under 1e-6 reads 0."
  [y0 y1 seen solid open]
  (loop [y y0 c 1]
    (cond (< c 1e-6) 0
          (>= y y1) c
          :else (recur (inc y) (* c (- 1 (solid-share y seen solid open)))))))
