(ns engine.jobs.value
  "What a list of items is worth (item-value) and what fetching them costs (fetch-cost), in one unit: about a second
  of a player's work. recover-drops compares the two; any job may ask what a pile, a chest or a kit is worth.

  The worth of one item is built from minecraft-data for the body's version, never from a hand list:

    natural items  An item a block drops, from a block no crafting recipe makes: hardness (0.1..5) x tool tier x
                   rarity, taking the cheapest such block.
                   Tool tier: the weakest tool in the block's harvestTools. None, wood or gold x1, stone or copper x4,
                   iron x16, diamond or netherite x64.
                   Rarity: x10 for an ore block (*_ore, ancient_debris).
                   Examples: dirt 0.5, cobblestone 1.5, a log 2, coal 30, raw iron 120, diamond 480.
    made items     The cheapest crafting recipe: the ingredients plus 1 for the craft, divided by the result count.
                   Or smelting, which minecraft-data does not describe, so by name rule (raw_X -> X_ingot,
                   X -> cooked_X, ancient_debris -> netherite_scrap, cobblestone -> stone, sand -> glass,
                   clay_ball -> brick): the input plus 2 for fuel and time.
                   Recipes loop (ingot, block, nugget), so the table is a fixpoint and the cheapest way wins.
                   Examples: stone pickaxe 6.75, iron ingot 122, diamond pickaxe 1443.
    no source      Neither dropped nor made (mob and chest loot): 4 x 64 / stack size. A 64-stack is 4 each, a
                   16-stack 16, an unstackable 256.
    unknown        A name minecraft-data does not list: unknown-each (1).

  Per stack: worth x count (nil counts 1; 0, negative or not a number counts 0), x the durability left (:durability
  against maxDurability, at least 0.1), plus each enchantment (:enchants [{:name :lvl|:level}]) at
  30 x level x 10 / the enchantment's weight. Walking a block costs about 0.3 (fetch-cost).

  Overrides ({name-or-group value}): a number is the worth of one item, {:times n} multiplies it. A name override
  goes into the table, so what is made of the item follows ({\"iron_ingot\" 0} makes an iron pickaxe worth its
  sticks). Group overrides apply to the final worth of their members: \"ore\" (what an ore block drops), \"tool\"
  (has durability, not armour), \"armor\", \"food\", \"block\" (placeable), \"unknown\". A name override beats a
  group one."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [engine.foods :as foods]
            [engine.value :as value]))

(def unknown-each "The worth of one item minecraft-data does not know." 1)
(def no-source-each "The worth of one item of a 64-stack that no block drops and no recipe makes." 4)
(def craft-step "Added to a recipe's ingredients: the craft itself." 1)
(def smelt-step "Added to a smelted item: fuel and furnace time." 2)
(def ore-rarity 10)
(def max-hardness 5)
(def min-hardness 0.1)
(def per-enchant-level 30)
(def min-durability-share 0.1)

(def tool-tiers
  "Tier of a harvest tool by its material prefix."
  {"wooden" 1 "golden" 1 "stone" 2 "copper" 2 "iron" 3 "diamond" 4 "netherite" 4})

(defn tier-factor [tier] (if (<= tier 1) 1 (js/Math.pow 4 (dec tier))))

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

(defn block-tier
  "The weakest harvest tool's tier of block b (0 when it needs none)."
  [md b]
  (let [tools (some-> (.-harvestTools b) js/Object.keys array-seq)]
    (if (empty? tools)
      0
      (apply min (map (fn [id] (get tool-tiers (first (str/split (.-name (aget (.-items md) id)) #"_")) 1)) tools)))))

(defn natural-values
  "{item name worth} of what blocks no recipe makes drop: the cheapest such block."
  [md crafted]
  (reduce (fn [acc b]
            (let [hardness (.-hardness b)
                  drops (.-drops b)]
              (if (or (not (number? hardness)) (neg? hardness) (crafted (.-name b)) (nil? drops))
                acc
                (let [w (* (max min-hardness (min max-hardness hardness))
                           (tier-factor (block-tier md b))
                           (if (ore-block? (.-name b)) ore-rarity 1))]
                  (reduce (fn [acc id]
                            (if-let [item (aget (.-items md) (if (number? id) id (.-id id)))]
                              (update acc (.-name item) #(min (or % js/Infinity) w))
                              acc))
                          acc (array-seq drops))))))
          {} (array-seq (.-blocksArray md))))

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
  "{item name worth} for minecraft-data version with name overrides pinned (overrides: {name override})."
  [version overrides]
  (let [md (minecraft-data version)
        items (array-seq (.-itemsArray md))
        names (map #(.-name %) items)
        recipes (recipe-list md)
        crafted (set (map first recipes))
        natural (natural-values md crafted)
        smelts (smelt-sources names)
        by-result (group-by first recipes)
        pinned (into {} (map (fn [[n o]] [n (override-fn o)])) overrides)
        pin (fn [n w] ((get pinned n identity) w))
        no-source (into {} (map (fn [i] [(.-name i) (* no-source-each (/ 64 (max 1 (.-stackSize i))))])) items)
        step (fn [table]
               (into {} (map (fn [n]
                               (let [via-recipe (some->> (by-result n)
                                                         (map (fn [[_ k ins]] (/ (+ craft-step (reduce + (map #(get table % js/Infinity) ins))) k)))
                                                         (apply min))
                                     via-smelt (some-> (smelts n) (#(+ smelt-step (get table % js/Infinity))))
                                     w (min (get natural n js/Infinity) (or via-recipe js/Infinity) (or via-smelt js/Infinity))]
                                 [n (pin n w)])))
                     names))
        start (into {} (map (fn [n] [n (pin n (get natural n js/Infinity))])) names)
        table (loop [t start i 0]
                (let [t' (step t)]
                  (if (or (= t t') (>= i 60)) t' (recur t' (inc i)))))]
    (into {} (map (fn [[n w]] [n (if (js/isFinite w) w (pin n (no-source n)))])) table)))

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

(defn enchant-worth [version enchants]
  (let [md (minecraft-data version)]
    (reduce + 0 (map (fn [e]
                       (let [ench (some->> (or (:name e) (:id e)) str (aget (.-enchantmentsByName md)))
                             weight (or (some-> ench .-weight) 10)
                             level (let [l (or (:lvl e) (:level e) 1)] (if (number? l) l 1))]
                         (* per-enchant-level level (/ 10 (max 1 weight)))))
                     enchants))))

(defn stack-worth
  "{:name :count :each :value} of one entry; group overrides skip a name that has its own (named, a map)."
  [version table group-overrides named {:keys [name durability enchants] :as entry}]
  (let [n (count-of entry)
        md (minecraft-data version)
        item (aget (.-itemsByName md) name)
        base (get table name ((override-fn (get group-overrides "unknown")) unknown-each))
        grouped (reduce (fn [w g] ((override-fn (get group-overrides g)) w))
                        base (if (and (contains? table name) (not (contains? named name))) (groups-for version name) []))
        max-d (some-> item .-maxDurability)
        share (if (and (number? durability) (number? max-d) (pos? max-d))
                (max min-durability-share (min 1 (/ durability max-d)))
                1)
        each (+ (* grouped share) (enchant-worth version enchants))]
    {:name name :count n :each each :value (* n each)}))

(defn item-value
  "{:value total :items [{:name :count :each :value} ...]} of items ([{:name :count ...}], inventory or drop shape),
  main contributors first. Stacks of one name with the same durability and enchantments are added. See the ns doc.
  Options: :overrides, :version (minecraft-data, default the body's). Never throws on odd entries: no name is
  worth 0, an unknown name unknown-each."
  [items & {:keys [overrides version]}]
  (let [version (or version @foods/selected)
        overrides (normal-overrides overrides)
        group-names #{"ore" "tool" "armor" "food" "block" "unknown"}
        group-overrides (select-keys overrides group-names)
        name-overrides (apply dissoc overrides group-names)
        table (table-for version name-overrides)
        named (filter #(string? (:name %)) items)
        merged (map (fn [[k stacks]] (assoc k :count (reduce + 0 (map count-of stacks))))
                    (group-by #(select-keys % [:name :durability :enchants]) named))
        rows (->> merged
                  (map #(stack-worth version table group-overrides name-overrides %))
                  (filter #(pos? (:count %)))
                  (sort-by :value >)
                  vec)]
    {:value (reduce + 0 (map :value rows)) :items rows}))

;; ---------------------------------------------------------------- the cost

(def despawn-ms value/despawn-ms)
(def trip "Any fetch: turning round, finding the pile, the risk of the place one died at." 10)
(def per-block "One block walked, there and back about a second of a player's time per 3 blocks." 0.3)
(def per-danger "One expected point of damage (engine.jobs.danger/route-danger): about 10 s of healing and risk." 10)
(def walk-blocks-per-s "Walking speed with slack for detours (4.3 flat out, 1.5x the way)." 2.9)
(def lethal-causes ["lava" "fire" "burn" "void" "out_of_world"])

(defn lethal-cause? [cause]
  (let [c (str/lower-case (if (keyword? cause) (name cause) (str cause)))]
    (boolean (some #(str/includes? c %) lethal-causes))))

(defn fetch-cost
  "{:cost :parts {:trip :walk :danger}} of fetching a pile `distance` blocks off, with `danger` (route-danger's number)
  on the way, `elapsed-ms` after it dropped.
  Or {:cost js/Infinity :reason}: :no-position (distance nil), :window-closed (the 5 min despawn passed), :too-far
  (the walk would end after the despawn), :lethal-cause (lava, fire or the void took the items)."
  [{:keys [distance danger elapsed-ms cause]}]
  (let [elapsed (or elapsed-ms 0)
        walk-ms (when (number? distance) (* 1000 (/ distance walk-blocks-per-s)))
        reason (cond
                 (nil? walk-ms) :no-position
                 (>= elapsed despawn-ms) :window-closed
                 (and (some? cause) (lethal-cause? cause)) :lethal-cause
                 (>= (+ elapsed walk-ms) despawn-ms) :too-far)]
    (if reason
      {:cost js/Infinity :reason reason}
      (let [parts {:trip trip :walk (* per-block distance) :danger (* per-danger (or danger 0))}]
        {:cost (reduce + (vals parts)) :parts parts}))))
