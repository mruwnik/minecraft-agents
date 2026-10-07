(ns jobs.lib.cost.value
  "What a list of items is worth (item-value) and what fetching them costs (fetch-cost), in one unit: about a second
  of a player's work. recover-drops compares the two; any job may ask what a pile, a chest or a kit is worth.

  The worth of one item is the cheapest way to obtain it, from minecraft-data for the body's version. Every constant
  and the few small tables (ore rarity, natural-and-craftable blocks, passive-mob health, rare drop chances) live in
  default-basis, overridable by :basis (deep-merged). The tables are defaults, not data minecraft-data has.

    block        A block's drops (blockLoot, no silk touch, chance at least :min-drop-chance): (dig + find) / drops.
                 dig = hardness x :hardness-s / the speed of the stone tool (the block's own tier if higher; the base assumes a
                 tool, never the hand);
                 find = :terrain, :wood for a log, stem or bamboo block, or :ores {ore [find drops]}, or :find-s for a block. Crafted blocks are no source
                 except :natural ones (granite, clay ...); a block that is no item (redstone_wire) is none.
    crop         A block with an age state: :farm-s / the age-7 yield.
    mob          entityLoot of the mobs in weapon/mob-max-health and passive-max-health: (kill + hurt + find) / expected
                 drops; kill = the stone sword's hits, hurt = :hurt-hp-per-hp x health x :danger hp, each at health/hp-seconds, find by hostile or passive.
    made         The cheapest recipe: the ingredients plus :craft-step, divided by the result count; or smelting by
                 name rule (raw_X -> X_ingot, X -> cooked_X, ancient_debris -> netherite_scrap, cobblestone -> stone,
                 sand -> glass, clay_ball -> brick): the input plus :smelt-step. The table is a fixpoint.
    unused       A sourced item that is no tool, armour, edible food, block or ingredient is worth :junk-share of that.
    food         An edible food is worth at least its points x :food-point-s.
    no source    :no-source-each (4) x 64 / stack size (a 64-stack 4, a 16-stack 16, an unstackable 256).
    unknown      A name minecraft-data does not list: :unknown-each (1).

  Per stack: worth x count (nil counts 1; 0, negative or not a number counts 0), x the durability left (:durability
  against maxDurability, at least :min-durability-share), plus each enchantment (:enchants [{:name :lvl|:level}]) at
  :per-enchant-level x level x 10 / the enchantment's weight. Walking a block costs about 0.3 (fetch-cost).

  Overrides ({name-or-group value}): a number is the worth of one item, {:times n} multiplies it. A name override
  goes into the table, so what is made of the item follows ({\"iron_ingot\" 0} makes an iron pickaxe worth its
  sticks). Group overrides apply to the final worth of their members: \"ore\" (what an ore block drops), \"tool\"
  (has durability, not armour), \"armor\", \"food\", \"block\" (placeable), \"unknown\". A name override beats a
  group one.

  Prices ([[regex price] ...], first match wins) set the worth of one item by name pattern, with :else the price of
  items no pattern matches, and :enchanted the worth of a stack with :enchants or :nbt: a job that ranks items its own
  way passes its own table, and the durability and enchantment terms do not apply to a priced
  item. A row [regex price :per-stack] prices the whole stack, not each item."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.foods :as foods]
            [engine.game :as game]
            [engine.settings :as settings]))

(def settings
  {::trip {:default 10 :type :number :min 0
           :doc "Any fetch: turning round, finding the pile, the risk of the place one died at, in seconds."}
   ::per-block {:default 0.3 :type :number :min 0.01
                :doc "One block walked, there and back about a second of a player's time per 3 blocks."}
   ::per-dark {:default 0.3 :type :number :min 0
               :doc "One block walked in the dark, on top of per-block: a dark block costs twice a lit one."}
   ::walk-blocks-per-s {:default 2.9 :type :number :min 0.1
                        :doc "Walking speed with slack for detours (4.3 flat out, 1.5x the way), blocks a second."}})

(def default-basis
  "Every constant of the item base; item-value's :basis is deep-merged over it."
  {:unknown-each 1 ;; the worth of one item minecraft-data does not know
   :no-source-each 4 ;; one item of a 64-stack that no block, mob or recipe yields
   :per-enchant-level 30
   :min-durability-share 0.1
   :craft-step 1
   :smelt-step 2
   :junk-share 0.1
   :food-point-s 2
   :farm-s 4
   :min-drop-chance 0.05
   :drop-chance {"apple" 0.005 "wheat_seeds" 0.125}
   :dig {:hardness-s 1.5 :max-hardness 5 :min-s 0.25 :default-tier 2 :speed {1 2 2 4 3 6 4 8}}
   :find {:terrain 0.1
          :wood 12 ;; a log, stem, hyphae, wood or bamboo block: the walk to a tree
          :block {"gilded_blackstone" 300}
          :ore-by-tier {1 10 2 30 3 300 4 300}
          :ores {"coal" [10 1.5] "copper" [15 3.5] "iron" [30 1] "lapis" [60 6.5] "redstone" [40 4.5] "gold" [90 1]
                 "diamond" [300 1.5] "emerald" [900 1] "nether_quartz" [40 1.5] "nether_gold" [60 4]
                 "ancient_debris" [900 1]}}
   :natural #{"granite" "diorite" "andesite" "coarse_dirt" "sandstone" "red_sandstone" "mossy_cobblestone" "snow"
              "clay" "glowstone" "melon" "packed_ice" "blue_ice" "prismarine" "dark_prismarine"}
   :mob {:weapon "stone_sword" :hurt-hp-per-hp 0.025 :danger {"creeper" 3 "enderman" 3} :find {:hostile 20 :passive 15}}})

(defn deep-merge [a b]
  (cond (nil? b) a
        (and (map? a) (map? b)) (merge-with deep-merge a b)
        :else b))

(defn smelt-sources
  "{output name input name}: smelting minecraft-data does not describe, by name rule over the item names it has."
  [names]
  (let [has? (set names)
        by-rule (keep (fn [n]
                        (cond
                          (and (str/starts-with? n "raw_") (has? (str (subs n 4) "_ingot"))) [(str (subs n 4) "_ingot") n]
                          (has? (str "cooked_" n)) [(str "cooked_" n) n]))
                      names)]
    (into {} (concat by-rule
                     (filter (fn [[o i]] (and (has? o) (has? i)))
                             [["netherite_scrap" "ancient_debris"] ["stone" "cobblestone"] ["glass" "sand"]
                              ["brick" "clay_ball"]])))))

(defn ore-block? [name] (or (str/ends-with? name "_ore") (= "ancient_debris" name)))

(defn ore-key
  "The :ores key of an ore block (deepslate_diamond_ore -> diamond)."
  [name]
  (-> name (str/replace #"^deepslate_" "") (str/replace #"_ore$" "")))

(defn block-tier
  "The weakest harvest tool's tier of block b (0 when it needs none)."
  [md b]
  (let [tools (some-> (.-harvestTools b) js/Object.keys array-seq)]
    (if (empty? tools)
      0
      (apply min (map (fn [id] (weapon/tool-tier (.-name (aget (.-items md) id)))) tools)))))

(defn mean-range [r] (if r (/ (+ (aget r 0) (aget r 1)) 2) 1))

(defn dig-seconds [md basis b]
  (let [{:keys [hardness-s max-hardness min-s default-tier speed]} (:dig basis)
        tier (max default-tier (block-tier md b))]
    (max min-s (/ (* hardness-s (min max-hardness (.-hardness b))) (get speed tier (get speed default-tier))))))

(defn wood-block? [name] (boolean (re-find #"(_log|_stem|_wood|_hyphae|^bamboo_block)$" name)))

(defn find-seconds [md basis b]
  (let [{:keys [terrain wood block ores ore-by-tier]} (:find basis)
        name (.-name b)]
    (cond
      (contains? block name) (get block name)
      (wood-block? name) wood
      (ore-block? name) (first (get ores (ore-key name) [(get ore-by-tier (block-tier md b) (apply max (vals ore-by-tier)))]))
      :else terrain)))

(defn crop? [b loot]
  (and (some #(= "age" (.-name %)) (some-> (.-states b) array-seq))
       (some #(some? (.-blockAge %)) (some-> loot .-drops array-seq))))

(defn block-yields
  "[[item name expected drops per break] ...] of block b: its blockLoot without silk touch (the age-7 entries of a
  crop, none of a crop's others), or the plain drops; ores take the drop count of the :ores table."
  [md basis b loot crop]
  (let [item-name (fn [id] (some-> (aget (.-items md) (if (number? id) id (.-id id))) .-name))
        ore ((:ores (:find basis)) (ore-key (.-name b)))
        entries (if loot
                  (->> (array-seq (.-drops loot))
                       (remove #(or (.-silkTouch %) (not= (boolean crop) (some? (.-blockAge %)))))
                       (map (fn [e] [(.-item e) (if crop 1 (get-in basis [:drop-chance (.-item e)] (.-dropChance e)))
                                     (mean-range (.-stackSizeRange e))])))
                  (map (fn [id] [(item-name id) 1 1]) (some-> (.-drops b) array-seq)))]
    (for [[item chance mean] entries
          :when (and item (>= chance (:min-drop-chance basis)))]
      [item (if (and ore (ore-block? (.-name b))) (second ore) (* chance mean))])))

(defn block-sources
  "{item name worth} of what blocks drop: the cheapest way, per item."
  [md basis crafted]
  (let [item? #(some? (aget (.-itemsByName md) %))]
    (reduce (fn [acc b]
              (let [name (.-name b)
                    loot (aget (.-blockLoot md) name)
                    crop (crop? b loot)]
                (if (or (not (number? (.-hardness b))) (neg? (.-hardness b))
                        (and (crafted name) (not crop) (not (contains? (:natural basis) name)))
                        (not (or crop (item? name))))
                  acc
                  (let [cost (if crop
                               (:farm-s basis)
                               (+ (dig-seconds md basis b) (find-seconds md basis b)))]
                    (reduce (fn [acc [item n]] (if (pos? n) (update acc item #(min (or % js/Infinity) (/ cost n))) acc))
                            acc (block-yields md basis b loot crop))))))
            {} (array-seq (.-blocksArray md)))))

(defn mob-sources
  "{item name worth} of what mobs drop (entityLoot): the kill, the hurt and the finding, per expected drop."
  [md basis]
  (let [{:keys [weapon hurt-hp-per-hp danger find]} (:mob basis)
        passive weapon/passive-max-health
        mobs (merge passive weapon/mob-max-health)
        gap (/ (weapon/attack-gap-ms weapon) 1000)
        damage (weapon/weapon-damage weapon)]
    (reduce (fn [acc [mob hp]]
              (let [kill (* gap (js/Math.ceil (/ hp damage)))
                    hurt (if (passive mob) 0 (* health/hp-seconds hurt-hp-per-hp hp (get danger mob 1)))
                    cost (+ kill hurt (if (passive mob) (:passive find) (:hostile find)))]
                (reduce (fn [acc e]
                          (let [n (* (.-dropChance e) (mean-range (.-stackSizeRange e)))]
                            (if (and (>= (.-dropChance e) (:min-drop-chance basis)) (pos? n))
                              (update acc (.-item e) #(min (or % js/Infinity) (/ cost n)))
                              acc)))
                        acc (some-> (aget (.-entityLoot md) mob) .-drops array-seq))))
            {} mobs)))

(defn used?
  "An item that is something: a tool, armour, an edible food, a placeable block or an ingredient."
  [md ingredients version name]
  (let [item (aget (.-itemsByName md) name)]
    (boolean (or (ingredients name)
                 (number? (some-> item .-maxDurability))
                 (and (contains? (foods/table-for version) name) (not (contains? foods/harmful name)))
                 (aget (.-blocksByName md) name)))))

(defn recipe-list
  "[[result-name result-count [ingredient-name ...]] ...] of every crafting recipe."
  [md]
  (let [item-name (fn [id] (some-> (aget (.-items md) (if (number? id) id (some-> id .-id))) .-name))]
    (for [[_ rs] (js/Object.entries (.-recipes md))
          r (array-seq rs)
          :let [result (.-result r)
                ids (if (.-inShape r)
                      (mapcat array-seq (array-seq (.-inShape r)))
                      (array-seq (.-ingredients r)))
                names (keep #(when (some? %) (item-name %)) ids)
                out (item-name result)]
          :when (and out (seq names))]
      [out (max 1 (or (.-count result) 1)) (vec names)])))

(defn override-fn
  "(fn [worth]) for override o: a number replaces the worth, {:times n} multiplies it; nil leaves it."
  [o]
  (cond
    (number? o) (constantly o)
    (map? o) (let [t (or (:times o) (get o "times"))] (if (number? t) #(* t %) identity))
    :else identity))

(defn normal-overrides [overrides] (into {} (map (fn [[k o]] [(if (keyword? k) (name k) (str k)) o])) overrides))

(defn build-table
  "{item name worth} for minecraft-data version with name overrides pinned (overrides: {name override}) and the basis
  (default-basis with the deep-merged :basis)."
  [version overrides basis]
  (let [md (minecraft-data version)
        basis (deep-merge default-basis basis)
        items (array-seq (.-itemsArray md))
        names (map #(.-name %) items)
        recipes (recipe-list md)
        crafted (set (map first recipes))
        ingredients (set (mapcat #(nth % 2) recipes))
        smelts (smelt-sources names)
        foods-table (foods/table-for version)
        food-floor (fn [n] (if (contains? foods/harmful n) 0 (* (:food-point-s basis) (get-in foods-table [n :points] 0))))
        sourced (merge-with min (block-sources md basis crafted) (mob-sources md basis))
        natural (into {} (map (fn [[n w]] [n (if (or (crafted n) (contains? smelts n) (used? md ingredients version n))
                                                  w
                                                  (* w (:junk-share basis)))]))
                       sourced)
        by-result (group-by first recipes)
        pinned (into {} (map (fn [[n o]] [n (override-fn o)])) overrides)
        pin (fn [n w] ((get pinned n identity) w))
        no-source (into {} (map (fn [i] [(.-name i) (* (:no-source-each basis) (/ 64 (max 1 (.-stackSize i))))])) items)
        step (fn [table]
               (into {} (map (fn [n]
                               (let [via-recipe (some->> (by-result n)
                                                         (map (fn [[_ k ins]] (/ (+ (:craft-step basis) (reduce + (map #(get table % js/Infinity) ins))) k)))
                                                         (apply min))
                                     via-smelt (some-> (smelts n) (#(+ (:smelt-step basis) (get table % js/Infinity))))
                                     w (min (get natural n js/Infinity) (or via-recipe js/Infinity) (or via-smelt js/Infinity))]
                                 [n (pin n (if (js/isFinite w) (max w (food-floor n)) w))])))
                     names))
        start (into {} (map (fn [n] [n (pin n (get natural n js/Infinity))])) names)
        table (loop [t start i 0]
                (let [t' (step t)]
                  (if (or (= t t') (>= i 60)) t' (recur t' (inc i)))))]
    (into {} (map (fn [[n w]] [n (if (js/isFinite w) w (pin n (max (no-source n) (food-floor n))))])) table)))

(def table-for (memoize build-table))

(defn groups-of
  "The override groups item name belongs to."
  [version name]
  (let [md (minecraft-data version)
        item (aget (.-itemsByName md) name)
        cats (set (some-> item .-enchantCategories array-seq))
        ore-drop? (some (fn [b] (and (ore-block? (.-name b))
                                     (some #(= (.-id item) (if (number? %) % (.-id %))) (array-seq (.-drops b)))))
                        (array-seq (.-blocksArray md)))]
    (if-not item
      ["unknown"]
      (cond-> []
        ore-drop? (conj "ore")
        (cats "armor") (conj "armor")
        (and (number? (.-maxDurability item)) (not (cats "armor"))) (conj "tool")
        (foods/food? name) (conj "food")
        (aget (.-blocksByName md) name) (conj "block")))))

(def groups-for (memoize groups-of))

(defn count-of [{:keys [count]}]
  (cond (nil? count) 1
        (and (number? count) (js/isFinite count) (pos? count)) count
        :else 0))

(defn enchant-worth [version basis enchants]
  (let [md (minecraft-data version)]
    (reduce + 0 (map (fn [e]
                       (let [ench (some->> (or (:name e) (:id e)) str (aget (.-enchantmentsByName md)))
                             weight (or (some-> ench .-weight) 10)
                             level (let [l (or (:lvl e) (:level e) 1)] (if (number? l) l 1))]
                         (* (:per-enchant-level basis) level (/ 10 (max 1 weight)))))
                     enchants))))

(defn enchanted?
  "Whether the entry carries a non-empty :enchants or :nbt."
  [entry]
  (boolean (some #(seq (get entry %)) [:enchants :nbt])))

(defn price-of
  "[each stack?] of the first prices row matching name ([regex price] or [regex price :per-stack]), else nil."
  [prices name]
  (some (fn [[re p mode]] (when (re-find re name) [p (= :per-stack mode)])) prices))

(defn stack-worth
  "{:name :count :each :value} of one entry; group overrides skip a name that has its own (named, a map)."
  [version basis table group-overrides named {:keys [prices else enchanted]} {:keys [name durability enchants] :as entry}]
  (let [n (count-of entry)
        [price per-stack] (or (when (and enchanted (enchanted? entry)) [enchanted true])
                              (price-of prices name)
                              (when else [else false]))
        md (minecraft-data version)
        item (aget (.-itemsByName md) name)
        base (get table name ((override-fn (get group-overrides "unknown")) (:unknown-each basis)))
        grouped (reduce (fn [w g] ((override-fn (get group-overrides g)) w))
                        base (if (and (contains? table name) (not (contains? named name))) (groups-for version name) []))
        max-d (some-> item .-maxDurability)
        share (if (and (number? durability) (number? max-d) (pos? max-d))
                (max (:min-durability-share basis) (min 1 (/ durability max-d)))
                1)
        each (cond (nil? price) (+ (* grouped share) (enchant-worth version basis enchants))
                   per-stack (/ price (max 1 n))
                   :else price)]
    {:name name :count n :each each :value (* n each)}))

(defn item-value
  "{:value total :items [{:name :count :each :value} ...]} of items ([{:name :count ...}], inventory or drop shape),
  main contributors first. Stacks of one name with the same durability, enchantments and :nbt are added. See the ns doc.
  Options: :overrides, :prices, :else and :enchanted (see the ns doc), :basis (over default-basis), :version
  (minecraft-data, default the body's). Never throws on odd entries: no name is worth 0, an unknown name :unknown-each."
  [items & {:keys [overrides prices else enchanted version basis]}]
  (let [version (or version @game/version)
        overrides (normal-overrides overrides)
        group-names #{"ore" "tool" "armor" "food" "block" "unknown"}
        group-overrides (select-keys overrides group-names)
        name-overrides (apply dissoc overrides group-names)
        table (table-for version name-overrides basis)
        basis (deep-merge default-basis basis)
        named (filter #(string? (:name %)) items)
        merged (map (fn [[k stacks]] (assoc k :count (reduce + 0 (map count-of stacks))))
                    (group-by #(select-keys % [:name :durability :enchants :nbt]) named))
        opts {:prices prices :else else :enchanted enchanted}
        rows (->> merged
                  (map #(stack-worth version basis table group-overrides name-overrides opts %))
                  (filter #(pos? (:count %)))
                  (sort-by :value >)
                  vec)]
    {:value (reduce + 0 (map :value rows)) :items rows}))

;; ---------------------------------------------------------------- the cost

(defn trip [] (settings/get settings ::trip))
(defn per-block [] (settings/get settings ::per-block))
(defn per-dark [] (settings/get settings ::per-dark))
(defn dark-factor
  "The planner's extra cost of a dark cell as a share of its own seconds (options.dark.factor): per-dark over per-block."
  []
  (/ (per-dark) (per-block)))
(def per-danger "One expected point of damage (jobs.lib.cost/route-danger): what an hp costs (jobs.lib.cost.health)." health/hp-seconds)
(defn walk-blocks-per-s [] (settings/get settings ::walk-blocks-per-s))
(def lethal-causes ["lava" "fire" "burn" "void" "out_of_world"])

(defn lethal-cause? [cause]
  (let [c (str/lower-case (if (keyword? cause) (name cause) (str cause)))]
    (boolean (some #(str/includes? c %) lethal-causes))))

(defn walk-cost
  "{:cost :parts {:walk :danger}} of walking `distance` blocks with `danger` (route-danger's number) on the way: the
  per-block and per-danger prices every trip is costed with."
  [{:keys [distance danger]}]
  (let [parts {:walk (* (per-block) distance) :danger (* per-danger (or danger 0))}]
    {:cost (reduce + (vals parts)) :parts parts}))

(defn fetch-cost
  "{:cost :parts {:trip :walk :danger}} of fetching a pile `distance` blocks off, with `danger` (route-danger's number)
  on the way, `elapsed-ms` after it dropped.
  Or {:cost js/Infinity :reason}: :no-position (distance nil), :window-closed (the 5 min despawn passed), :too-far
  (the walk would end after the despawn), :lethal-cause (lava, fire or the void took the items)."
  [{:keys [distance danger elapsed-ms cause]}]
  (let [elapsed (or elapsed-ms 0)
        despawn-ms (game/despawn-ms)
        walk-ms (when (number? distance) (* 1000 (/ distance (walk-blocks-per-s))))
        reason (cond
                 (nil? walk-ms) :no-position
                 (>= elapsed despawn-ms) :window-closed
                 (and (some? cause) (lethal-cause? cause)) :lethal-cause
                 (>= (+ elapsed walk-ms) despawn-ms) :too-far)]
    (if reason
      {:cost js/Infinity :reason reason}
      (let [w (walk-cost {:distance distance :danger danger})]
        {:cost (+ (trip) (:cost w)) :parts (assoc (:parts w) :trip (trip))}))))
