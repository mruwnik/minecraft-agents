(ns jobs.forestry.prepare-rule
  "The rule of jobs.forestry.prepare, pure: what state a planned tree cell is in, from the blocks around it and what
  the body carries."
  (:require [clojure.string :as str]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.trees :as forestry]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.tidy-rules :as tidy]
            [jobs.forestry.maintain :as maintain]))

(def headroom-table
  {"oak" 7 "birch" 8 "spruce" 9 "jungle" 13 "acacia" 10 "dark_oak" 10 "pale_oak" 10 "cherry" 9})

(defn headroom-of
  "Cells of growth space for species (the planted cell included): the override, else the table, nil for a species
  not supported."
  [species over]
  (or (get over species) (get headroom-table species)))

(def tree-plants
  #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow" "vine" "glow_lichen" "dandelion" "poppy"
    "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip" "white_tulip" "pink_tulip" "oxeye_daisy"
    "cornflower" "lily_of_the_valley" "torchflower" "sunflower" "lilac" "rose_bush" "peony" "pink_petals"
    "wildflowers" "leaf_litter" "short_dry_grass" "tall_dry_grass" "bush" "firefly_bush"})

(defn tree-free?
  "Whether a tree's growth may take the place of the block: air, leaves, saplings, plants, snow, vines."
  [n]
  (boolean (or (rules/air n) (str/ends-with? n "_leaves") (str/ends-with? n "_sapling") (tree-plants n))))

(def natural-names
  #{"stone" "cobblestone" "deepslate" "cobbled_deepslate" "andesite" "diorite" "granite" "tuff" "calcite" "sand"
    "red_sand" "gravel" "sandstone" "red_sandstone" "clay" "mud" "snow_block" "mycelium" "terracotta" "dirt_path"})

(defn natural-ground?
  "Whether the block is ground nobody built: replaced by dirt when it is under a planted cell."
  [n]
  (contains? natural-names n))

(def soil-preference ["dirt" "grass_block" "coarse_dirt" "podzol"])

(defn soil-item
  "The soil block of species' soil carried (carried: a set of names), dirt first, or nil."
  [carried species]
  (first (filter #(and (carried %) ((maintain/soil-for species) %)) soil-preference)))

(defn plant-like? [n]
  (boolean (re-find #"(_sapling|_propagule|_fungus)$" n)))

(defn own-cell
  "{:state ...} when the block in the planned cell settles it (unloaded, growing, grown, wrong), else nil."
  [p pos species]
  (let [n (u/block-name p pos)]
    (cond
      (nil? n) {:state :unloaded}
      (= n (forestry/sapling-of species)) {:state :growing}
      (= n (str species "_log")) {:state :grown}
      (or (plant-like? n) (forestry/log-name? n) (tidy/keep-why n)) {:state :wrong :found n})))

(defn growth-space
  "{:state :cramped :at :block} for the lowest block above the planted cell a tree cannot grow through, {:state
  :unloaded} when a cell of it is not loaded, else nil."
  [p pos species over]
  (some (fn [dy]
          (let [at (update pos :y + dy)
                n (u/block-name p at)]
            (cond
              (nil? n) {:state :unloaded}
              (not (tree-free? n)) {:state :cramped :at (maintain/cell-vec at) :block n})))
        (range 1 (headroom-of species over))))

(defn soil-state
  "{:state ...} for the ground under the planted cell: nil when it is soil the species takes, else :soil-dig (natural
  ground, dirt carried), :soil-place (the hole this job dug, dirt carried), :no-soil with :why, or :unloaded."
  [p pos species {:keys [planned holes carried]}]
  (let [under (maintain/down pos)
        n (u/block-name p under)
        dirt (soil-item carried species)
        no-dirt {:state :no-soil :why :no-dirt}]
    (cond
      (nil? n) {:state :unloaded}
      ((maintain/soil-for species) n) nil
      (and (contains? holes (maintain/cell-vec under)) (rules/air n)) (if dirt {:state :soil-place :item dirt} no-dirt)
      (planned (maintain/cell-vec under)) {:state :no-soil :why :planned}
      (rules/air n) {:state :no-soil :why :hollow}
      (rules/fluids n) {:state :no-soil :why :fluid}
      (tidy/keep-why n) {:state :no-soil :why :kept}
      (not (natural-ground? n)) {:state :no-soil :why :other-block}
      dirt {:state :soil-dig :block n :item dirt}
      :else no-dirt)))

(def max-upstream "How many cells upstream the walk to a source goes." 16)

(defn upstream-cells
  "The water cells the cell at level l is fed from, the best first: the cell above for falling water, else the
  neighbours on its level with a lower level (a falling one too, for level 1), lowest first."
  [level-at [x y z] l]
  (if (>= l 8)
    (filter level-at [[x (inc y) z]])
    (->> rules/neighbour-deltas
         (filter (fn [[_ dy _]] (zero? dy)))
         (keep (fn [[dx _ dz]]
                 (let [q [(+ x dx) y (+ z dz)]
                       ql (level-at q)]
                   (when (and ql (or (< ql l) (and (= l 1) (>= ql 8)))) [ql q]))))
         (sort-by first)
         (map second))))

(defn upstream
  "The [x y z] of the source reached by walking upstream from start, or nil: level-at maps an [x y z] to the level of
  the water there (0 source, 1-7 flowing, 8+ falling), nil for anything else; at most max-upstream steps, no cell twice."
  [level-at start]
  (loop [pos (vec start) n 0 seen #{}]
    (let [l (level-at pos)
          next-pos (when (and l (pos? l) (< n max-upstream) (not (seen pos))) (first (upstream-cells level-at pos l)))]
      (cond
        (nil? l) nil
        (zero? l) pos
        next-pos (recur next-pos (inc n) (conj seen pos))))))

(defn water-level
  "The level of the water at pos (a number), nil for any other block, unloaded, or water without a level."
  [p pos]
  (let [b (u/block-at p pos)]
    (when (= "water" (some-> b .-name))
      (let [l (some-> b .-properties .-level)]
        (when (number? l) l)))))

(defn wet-state
  "{:state ...} of a planned cell holding water: :fill (the source is in the cell and no source lies beside it),
  :dam (the source is at :target: upstream, or beside a source cell, which would flood back once dug), both with the
  :item to place and the :source, or :wet with :why :untraced / :no-dirt."
  [p pos species carried]
  (let [l (water-level p pos)
        cell (maintain/cell-vec pos)
        level-at (fn [[x y z]] (water-level p {:x x :y y :z z}))
        beside (when (and l (zero? l))
                 (let [[x y z] cell]
                   (->> rules/neighbour-deltas
                        (filter (fn [[_ dy _]] (zero? dy)))
                        (map (fn [[dx _ dz]] [(+ x dx) y (+ z dz)]))
                        (filter #(= 0 (level-at %)))
                        first)))
        source (cond (nil? l) nil
                     beside beside
                     (zero? l) cell
                     :else (upstream level-at cell))
        dirt (soil-item carried species)]
    (cond
      (nil? source) {:state :wet :why :untraced}
      (nil? dirt) {:state :wet :why :no-dirt :source source}
      (= source cell) {:state :fill :target pos :source source :item dirt}
      :else {:state :dam :target (zipmap [:x :y :z] source) :source source :item dirt})))

(declare assess-open)

(defn assess
  "{:state ...} of one planned cell: :unsupported, :unloaded, :growing, :grown, :wrong (:found), :cramped (:at :block),
  :fill / :dam / :wet (:why :source) for water in the cell, :clear (:block: a stray to dig), :soil-dig, :soil-place,
  :no-soil (:why), :no-tool (:block: a dig no carried tool harvests; snow, whose drop is a snowball, is dug anyway), :plant (:item) or :short."
  [p pos species world]
  (let [r (assess-open p pos species world)
        dug (when (#{:clear :soil-dig} (:state r)) (:block r))]
    (if (and dug (not= "snow" dug) (not (tools/can-harvest? p dug)))
      {:state :no-tool :block dug}
      r)))

(defn assess-open
  "assess before the tool rule."
  [p pos species {:keys [carried over] :as world}]
  (let [n (u/block-name p pos)
        sapling (forestry/sapling-of species)]
    (or (when-not (headroom-of species over) {:state :unsupported})
        (when (= "water" n) (wet-state p pos species carried))
        (own-cell p pos species)
        (growth-space p pos species over)
        (when-not (rules/air n) {:state :clear :block n})
        (soil-state p pos species world)
        (if (carried sapling) {:state :plant :item sapling} {:state :short}))))

(def steps #{:fill :dam :clear :soil-dig :soil-place :plant})
