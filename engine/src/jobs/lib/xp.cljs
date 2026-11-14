(ns jobs.lib.xp
  "Experience: the game's level curve, the experience points each act gives (game data, the wiki's means: no
  minecraft-data file lists them), and the ways a body knows to earn points, each priced in seconds with jobs.lib.cost.
  A way is {:way :smelt|:mine|:fight :key :points :price :job :args}: :points the mean it earns, :price its seconds,
  :job and :args the child that does it (jobs.items.smelt, jobs.blocks.dig, jobs.combat.attack)."
  (:require [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.fuel :as fuel]
            [jobs.lib.game-facts :as facts]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.threats :as threats]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

;; ------------------------------------------------------------------ the level curve

(defn level-points
  "Points from level `level` to the next."
  [level]
  (cond (<= level 15) (+ (* 2 level) 7)
        (<= level 30) (- (* 5 level) 38)
        :else (- (* 9 level) 158)))

(defn total-points
  "Points a body at the start of `level` has earned in all."
  [level]
  (cond (<= level 16) (+ (* level level) (* 6 level))
        (<= level 31) (+ (* 2.5 level level) (* -40.5 level) 360)
        :else (+ (* 4.5 level level) (* -162.5 level) 2220)))

(defn level-of [p] (or (some-> (.self p) .-experience .-level) 0))

(defn points-of
  "The body's points in all: its level's and the progress bar's share of the next."
  [p]
  (let [level (level-of p)]
    (+ (total-points level) (* (or (some-> (.self p) .-experience .-progress) 0) (level-points level)))))

(defn points-short
  "Points the body lacks to reach `levels`, 0 when it has them."
  [p levels]
  (max 0 (- (total-points levels) (points-of p))))

;; ------------------------------------------------------------------ what each act gives (game data)

(def smelt-points
  "Mean points one item smelted gives when taken from the furnace, by input."
  (merge {"raw_iron" 0.7 "raw_gold" 1 "raw_copper" 0.7 "ancient_debris" 2
          "iron_ore" 0.7 "deepslate_iron_ore" 0.7 "gold_ore" 1 "deepslate_gold_ore" 1 "nether_gold_ore" 1
          "copper_ore" 0.7 "deepslate_copper_ore" 0.7 "diamond_ore" 1 "deepslate_diamond_ore" 1
          "emerald_ore" 1 "deepslate_emerald_ore" 1 "lapis_ore" 0.2 "deepslate_lapis_ore" 0.2
          "redstone_ore" 0.7 "deepslate_redstone_ore" 0.7 "coal_ore" 0.1 "deepslate_coal_ore" 0.1 "nether_quartz_ore" 0.2
          "cactus" 1 "clay_ball" 0.3 "clay" 0.35 "kelp" 0.1 "wet_sponge" 0.15 "chorus_fruit" 0.1 "sea_pickle" 0.1}
         (zipmap ["beef" "porkchop" "chicken" "mutton" "rabbit" "cod" "salmon" "potato"] (repeat 0.35))
         (zipmap ["sand" "red_sand" "cobblestone" "stone" "cobbled_deepslate" "netherrack" "sandstone" "red_sandstone"
                  "stone_bricks" "deepslate_bricks" "deepslate_tiles" "nether_bricks" "quartz_block"] (repeat 0.1))))

(defn smelt-points-of
  "Mean points one item smelted gives: smelt-points, 0.15 for a log or wood (charcoal), else 0."
  [item]
  (or (smelt-points item) (if (re-find #"_(log|wood)$" item) 0.15 0)))

(def ore-points
  "Mean points mining one ore block with a tool that harvests it gives (none with silk touch)."
  (merge (zipmap ["coal_ore" "deepslate_coal_ore"] (repeat 1))
         (zipmap ["lapis_ore" "deepslate_lapis_ore" "nether_quartz_ore"] (repeat 3.5))
         (zipmap ["redstone_ore" "deepslate_redstone_ore"] (repeat 3))
         (zipmap ["diamond_ore" "deepslate_diamond_ore" "emerald_ore" "deepslate_emerald_ore"] (repeat 5))
         {"nether_gold_ore" 0.5}))

(def mob-points
  "Points a kill of a hostile mob gives, where it is not 5 (most hostiles)."
  {"blaze" 10 "guardian" 10 "elder_guardian" 10 "evoker" 10 "ravager" 20 "piglin_brute" 20 "endermite" 3 "slime" 2 "magma_cube" 2
   "warden" 5})

(defn mob-points-of [name] (get mob-points name 5))

(def ore-radius "How far a seen ore counts: all that perception remembers." look/memory-radius)

;; ------------------------------------------------------------------ the ways a body knows

(defn walk-s [c pos] (:cost (cost/walk-cost {:distance (u/dist (u/self-pos c) pos)})))

(defn fetch-price
  "Seconds getting n of item costs: 0 when carried, else the obtain's price (need/item-price) when the call's ways fetch,
  else ##Inf."
  [c item n]
  (cond (<= n (u/carried-count (:primitives c) item)) 0
        (need/fetch-opts c) (need/item-price c item n (need/fetch-opts c) nil)
        :else ##Inf))

(defn smelt-way
  "Smelting n of input (enough for short points, at most have of it): the furnace's cook, the worth of the coal it
  burns, a furnace when none is seen (fetch-price) and the input not carried."
  [c input have short]
  (let [each (smelt-points-of input)
        n (min have (js/Math.ceil (/ short each)))
        furnace (first (look/seen-blocks (:primitives c) {:names ["furnace"] :radius ore-radius :max 1 :live? true}))]
    (when (pos? n)
      {:way :smelt :key [:smelt input] :points (* n each)
       :price (+ (/ (settings/ticks->ms (* n (facts/cook-ticks "furnace"))) 1000)
                 (:value (cost/goal-value [{:name "coal" :count (js/Math.ceil (/ n (fuel/fuel-per-unit "furnace" "coal")))}]))
                 (if furnace (* 2 (walk-s c (:pos furnace))) (fetch-price c "furnace" 1))
                 (fetch-price c input n))
       :job 'jobs.items.smelt :args {:item input :count n}})))

(defn smelt-ways
  "A smelt way for each carried input that gives points, and, when the call fetches, for each raw ore not carried
  (the ore a body smelts for its ingots)."
  [c short]
  (let [carried (->> (u/inventory (:primitives c)) (group-by :name) (map (fn [[k v]] [k (reduce + (map :count v))])))
        fetched (when (need/fetch-opts c)
                  (for [ore ["raw_iron" "raw_copper" "raw_gold"] :when (not (some #(= ore (first %)) carried))]
                    [ore ##Inf]))]
    (keep (fn [[item have]] (when (pos? (smelt-points-of item)) (smelt-way c item have short)))
          (concat carried fetched))))

(defn mine-ways
  "A dig of each seen ore that gives points (nearest first): the walk, the dig and, with no carried tool that
  harvests it, the cheapest such tool's fetch-price."
  [c]
  (let [p (:primitives c)
        inv (map :name (u/inventory p))
        tool-s (memoize (fn [block] (if-let [t (tools/harvest-need inv (some-> (.harvestTools p block) js->clj))] (fetch-price c t 1) 0)))]
    (for [{:keys [name pos]} (look/seen-blocks p {:names (keys ore-points) :radius ore-radius :live? true})]
      {:way :mine :key [:mine pos] :points (ore-points name)
       :price (+ (walk-s c pos) (or (cost/dig-cost name) 0) (tool-s name))
       :job 'jobs.blocks.dig :args {:pos pos :collect true :need-drop true}})))

(defn fight-ways
  "A kill of each hostile the body senses now: cost/kill-price and the walk to it."
  [c]
  (let [p (:primitives c)
        version (game/version-of p)
        body (threats/death-body c)
        self (u/self-pos c)]
    (for [{:keys [id name pos]} (threats/sensed-mobs p {:way? false :any-range? true})
          :when (and id pos (cost/hostile? version {:name name}))]
      {:way :fight :key [:fight id] :points (mob-points-of name)
       :price (+ (walk-s c pos) (cost/kill-price body name self))
       :job 'jobs.combat.attack :args {:targets [id]}})))

(defn ways
  "Every way c's body knows to earn points toward `levels`, with a finite price, minus those whose :key is in skip,
  cheapest a point first."
  [c levels skip]
  (let [short (points-short (:primitives c) levels)]
    (->> (concat (smelt-ways c short) (mine-ways c) (fight-ways c))
         (remove #(contains? skip (:key %)))
         (filter #(and (pos? (:points %)) (js/isFinite (:price %))))
         (sort-by #(/ (:price %) (:points %))))))

(defn levels-price
  "Seconds reaching `levels` costs c's body by the ways it knows, cheapest a point first, each taken whole until the
  points are earned: 0 when it has them, ##Inf when the ways known do not earn enough."
  ([c levels] (levels-price c levels (ways c levels #{})))
  ([c levels ws]
   (loop [short (points-short (:primitives c) levels) [w & more] ws s 0]
     (cond (<= short 0) s
           (nil? w) ##Inf
           :else (recur (- short (:points w)) more (+ s (:price w)))))))
