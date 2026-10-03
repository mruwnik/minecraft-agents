(ns dashboard.tiles
  "Terrain tiles for the map: one dumped chunk column -> a 16x16 picture, one pixel per block. The JS glue
  (js/worldblocks.mjs createWorldTiles) decodes a column and reports the top of every (x, z); the colours, the shading and
  the cache are here. Shading is the old Minecraft map's: lighter the higher the ground, lighter where it rises from the
  block to the north (the sun is in the north-west), darker for deeper water."
  (:require [dashboard.blockcolour :as bc]))

(def size 16)

;; blocks the eye looks through: the top of a column is the first block below them
(def skipped-blocks
  ["air" "cave_air" "void_air" "short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "dandelion" "poppy" "blue_orchid" "allium"
   "azure_bluet" "red_tulip" "orange_tulip" "white_tulip" "pink_tulip" "oxeye_daisy" "cornflower" "lily_of_the_valley" "sunflower"
   "lilac" "rose_bush" "peony" "torch" "wall_torch" "light" "structure_void" "wheat" "carrots" "potatoes" "beetroots" "sweet_berry_bush"
   "brown_mushroom" "red_mushroom" "hanging_roots" "glow_lichen" "leaf_litter"])

(def rgb
  (memoize
   (fn [hex] (mapv #(js/parseInt (subs hex % (+ % 2)) 16) [1 3 5]))))

(defn clamp [lo hi n] (max lo (min hi n)))

(def sea-level 63)

(defn height-light
  "0.78 at y 50 and below, 1.10 from y 160."
  [y]
  (+ 0.78 (* 0.32 (clamp 0 1 (/ (- y 50) 110)))))

(defn slope-light
  "1 on level ground, 0.06 more per block the ground rises from the one to the north (at most 4), less per block it falls."
  [y north-y]
  (+ 1 (* 0.06 (clamp -4 4 (- y north-y)))))

(defn scale-rgb [[r g b] k] (mapv #(js/Math.round (clamp 0 255 (* % k))) [r g b]))

(defn mix-rgb [a b t] (mapv (fn [x y] (+ (* x (- 1 t)) (* y t))) a b))

(defn water-rgb [{:keys [depth floor-name]}]
  (let [water (rgb (bc/terrain-colour "water"))
        floor (scale-rgb (rgb (bc/terrain-colour (or floor-name "water"))) 0.8)
        see-through (min 0.85 (+ 0.45 (* 0.06 depth)))]
    (scale-rgb (mix-rgb floor water see-through) (clamp 0.35 1 (- 1 (* 0.045 depth))))))

(defn cell-rgb
  "[r g b] of one cell: {:name top block, :y its height, :north-y the height one block north, :depth water over a
  floor of :floor-name}."
  [{:keys [name y north-y depth] :as cell}]
  (if (pos? (or depth 0))
    (water-rgb cell)
    (scale-rgb (rgb (bc/terrain-colour name)) (* (height-light y) (slope-light y north-y)))))

(defn tile-rgba
  "The 16x16 RGBA bytes of a column {:palette :top :floor :y :depth} (typed arrays, cell = z * 16 + x)."
  [{:keys [palette top floor y depth]}]
  (let [out (js/Uint8Array. (* 4 size size))]
    (dotimes [cell (* size size)]
      (let [north (if (< cell size) cell (- cell size))
            [r g b] (cell-rgb {:name (nth palette (aget top cell)) :y (aget y cell) :north-y (aget y north)
                               :depth (aget depth cell) :floor-name (nth palette (aget floor cell))})
            at (* 4 cell)]
        (aset out at r) (aset out (+ at 1) g) (aset out (+ at 2) b) (aset out (+ at 3) 255)))
    out))

;; ---------------------------------------------------------------- what is dumped
(defn parse-column-file
  "\"<cx>.<cz>.bin\" -> [cx cz], or nil."
  [file]
  (when-let [[_ cx cz] (re-matches #"(-?\d+)\.(-?\d+)\.bin" file)]
    [(js/parseInt cx 10) (js/parseInt cz 10)]))

(defn index-entries
  "[[file mtime-ms]] -> [[cx cz mtime-ms]] of the column files, mtimes whole milliseconds."
  [files]
  (vec (for [[file mtime] files
             :let [cell (parse-column-file file)]
             :when cell]
         (conj cell (js/Math.floor mtime)))))

(defn newer-than [entries since]
  (if-not since
    entries
    (filterv (fn [[_ _ mtime]] (> mtime since)) entries)))

;; ---------------------------------------------------------------- bounded cache
;; A JS Map keeps insertion order: the first key is the least recently used, a touch re-inserts.
(defn lru [] (js/Map.))

(defn lru-get! [m k]
  (when (.has m k)
    (let [v (.get m k)]
      (.delete m k)
      (.set m k v)
      v)))

(defn lru-put! [m cap k v]
  (.delete m k)
  (.set m k v)
  (loop []
    (when (> (.-size m) cap)
      (.delete m (.-value (.next (.keys m))))
      (recur)))
  m)

(defn lru-keys [m] (vec (es6-iterator-seq (.keys m))))
