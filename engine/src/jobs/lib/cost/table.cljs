(ns jobs.lib.cost.table
  "The item worth table of jobs.lib.cost.value: the default basis and the table built from minecraft-data (see that namespace's doc)."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [jobs.lib.cost.base-rates :as base-rates]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.gear :as gear]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.precious :as precious]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.foods :as foods]
            [jobs.lib.fuel :as fuel]
            [jobs.lib.game-facts :as facts]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.game :as game]))

(a/defargs settings
  {::craft-timeouts {:default {:k 21 :n 1165 :s 12.39} :spec (a/pred "{:k timeouts :n started (> 0) :s seconds each, k <= n}" (fn [{:keys [k n s]}] (and (number? k) (number? n) (number? s) (<= 0 k n) (pos? n) (<= 0 s))))
                     :doc "Craft actions that timed out: k of n started, taking s seconds each (same counts as ::craft-step, `node tools/event-counts.mjs craft-steps`). jobs.lib.cost.value/craft-step-s prices them into every craft step."}
   ::craft-step {:default 0.905 :spec (a/num-in 0 nil)
                 :doc "Seconds one craft action takes beyond its ingredients, the walk to the table not in it. Counted over n = 1144 finished craft actions of the trial bodies' logs (21 timeouts left out), 0.905 s each (recount: `node tools/event-counts.mjs craft-steps`, dashboard/src/world_test/craft_steps.cljs)."}
   ::smelt-step {:default 10.3 :spec (a/num-in 0 nil)
                 :doc "Seconds one item takes in a furnace beyond its input, fuel and the furnace apart: vanilla cooks it in 10 s; 18 raw iron in one smelt job (furnace placed) took 185.4 s wall, 10.3 each (trial logs to 2026-10-30; the 2-item probe runs of 51-74 s include making the furnace)."}})

(defn fuel-each
  "Seconds of the cheapest fuel one smelted item burns: a fuel's worth in table over the items it cooks."
  [table]
  (transduce (keep (fn [[n w]] (when-let [per (fuel/fuel-per-unit :furnace n)] (/ w per)))) min js/Infinity table))

(def default-basis
  "Every constant of the item base; item-value's :basis is deep-merged over it."
  {:unknown-each 1 ;; the worth of one item minecraft-data does not know
   :no-source-each 4 ;; one item of a 64-stack that no block, mob or recipe yields
   :per-enchant-level 30
   :min-durability-share 0.1
   :unsourced-food-health 10 ;; the health a precious food that nothing yields is priced at: what it heals a body there
   :gear {:ref-mob "zombie" :ref-block "stone"} ;; one use of a tool, weapon or armour piece saves, against the stone tool or the bare body: the zombie (its hit rate is the one counted) hits and has hp, the kit trial digs stone most (gear/use-saved)
   :junk-share 0.1
   :food-point-s 2
   :farm-s 4
   :min-drop-chance 0.05
   :drop-chance {"apple" 0.005 "wheat_seeds" 0.125 "stick" 0.02} ;; vanilla's, where minecraft-data lists 1 (leaves' sticks)
   :dig {:hardness-s 1.5 :unfit-s 5 :min-s 0.25 :default-tier 2} ;; seconds of dig per hardness at speed 1: 30 ticks with the block's tool tier, 100 without (vanilla)
   :find {:terrain 0.1
          :crop 0 ;; the walk to a ripe crop: none at the margin (the field is the farm's); a goal's is value/measured-worth's
          :wood 12 ;; a log, stem, hyphae, wood or bamboo block, or bamboo itself (jungles alone): the walk to a tree
          :block {"gilded_blackstone" 300}
          :ore-by-tier {1 10 2 30 3 300 4 300}
          ;; seconds of looking per unit of the best layer's ore density (base-rates/peak-density): find = this / density.
          ;; item-value fits it to the measured ores (value/measured-worth); nil: :ore-by-tier.
          :ore-scan-s nil
          :ores {"coal" [nil 1.5] "copper" [nil 3.5] "iron" [nil 1] "lapis" [nil 6.5] "redstone" [nil 4.5] "gold" [nil 1]
                 "diamond" [nil 1.5] "emerald" [nil 1] "nether_quartz" [40 1.5] "nether_gold" [60 4]
                 "ancient_debris" [900 1]}} ;; [find drops]: a nil find comes from the base rates, a number is the hand figure of an ore they do not cover
   :natural #{"granite" "diorite" "andesite" "coarse_dirt" "sandstone" "red_sandstone" "mossy_cobblestone" "snow"
              "clay" "glowstone" "melon" "packed_ice" "blue_ice" "prismarine" "dark_prismarine"}
   :mob {:weapon "stone_sword" :hurt-hp-per-hp 0.025 :danger {"creeper" 3 "enderman" 3} :find {:hostile 20 :passive 15}}})

(defn craft-step
  "Seconds one craft step takes beyond its ingredients (::craft-step)."
  [] (settings/get settings ::craft-step))

(defn craft-timeouts
  "{:k :n :s} of the measured craft timeouts (::craft-timeouts)."
  [] (settings/get settings ::craft-timeouts))

(defn smelt-step
  "Seconds one smelt step takes per item beyond its input (::smelt-step)."
  [] (settings/get settings ::smelt-step))

(defn deep-merge [a b]
  (cond (nil? b) a
        (and (map? a) (map? b)) (merge-with deep-merge a b)
        :else b))

(defn full-basis
  "default-basis with the measured :craft-step and :smelt-step, the deep-merged basis over it."
  [basis]
  (deep-merge (assoc default-basis :craft-step (craft-step) :smelt-step (smelt-step)) basis))

(defn smelt-sources
  "{output [input names]} of what the furnaces cook (facts/smelt-inputs), over the item
  names minecraft-data has."
  [names]
  (let [has? (set names)
        inputs facts/smelt-inputs]
    (into {} (keep (fn [[o ins]]
                     (let [ins (filterv has? ins)]
                       (when (and (has? o) (seq ins)) [o ins]))))
          inputs)))

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
  (let [dig (:dig basis)]
    (gear/dig-time dig (gear/tier-speed dig (block-tier md b)) (.-hardness b))))

(def drops-table
  "{block name [item name ..]} of what minecraft-data lists each block drops for a version (memoised)."
  (memoize
   (fn [version]
     (let [data (minecraft-data version)]
       (into {} (map (fn [b] [(.-name b) (vec (keep #(some-> (aget data "items" %) .-name) (array-seq (aget b "drops"))))]))
             (array-seq (aget data "blocksArray")))))))

(defn dig-cost
  "Seconds digging the block name takes, the item base's dig term (dig-seconds); nil for a block never dug (bedrock) or
  one minecraft-data does not know. Options :version and :basis as item-value's."
  [name & {:keys [version basis]}]
  (let [md (minecraft-data (or version @game/version))
        b (aget (.-blocksByName md) name)]
    (when (and b (number? (.-hardness b)) (not (neg? (.-hardness b))))
      (dig-seconds md (deep-merge default-basis basis) b))))

(defn kill-drops
  "Items of name a kill of mob is expected to drop (entityLoot: chance x mean stack), nil when it drops none. Option
  :version as item-value's."
  [mob name & {:keys [version]}]
  (let [md (minecraft-data (or version @game/version))]
    (some (fn [e] (when (= name (.-item e)) (* (.-dropChance e) (mean-range (.-stackSizeRange e)))))
          (some-> (aget (.-entityLoot md) mob) .-drops array-seq))))

(defn wood-block? [name] (boolean (re-find #"(_log|_stem|_wood|_hyphae|^bamboo_block|^bamboo|^bamboo_sapling)$" name)))

(defn ore-find-seconds
  "Seconds to find ore: its hand figure, else :ore-scan-s over the density of its best layer (nil: no placement or no scan)."
  [{:keys [ores ore-scan-s]} ore]
  (or (first (get ores ore))
      (let [density (base-rates/peak-density ore nil)]
        (when (and ore-scan-s (pos? density)) (/ ore-scan-s density)))))

(defn find-seconds [md basis b]
  (let [{:keys [terrain wood block ores ore-by-tier] :as found} (:find basis)
        name (.-name b)]
    (cond
      (contains? block name) (get block name)
      (wood-block? name) wood
      (ore-block? name) (or (ore-find-seconds found (ore-key name)) (get ore-by-tier (block-tier md b) (apply max (vals ore-by-tier))))
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

(defn crop-yields
  "[{:name :count}] a ripe crop block of name drops when cut (block-yields of its age-7 loot), nil for a block minecraft-data
  does not know. Options :version and :basis as item-value's."
  [name & {:keys [version basis]}]
  (let [md (minecraft-data (or version @game/version))
        b (aget (.-blocksByName md) name)
        loot (when b (aget (.-blockLoot md) name))]
    (when b
      (mapv (fn [[item n]] {:name item :count n}) (block-yields md (deep-merge default-basis basis) b loot (crop? b loot))))))

(defn block-sources
  "{item name worth} of what blocks drop: the cheapest way, per item. tool-price is the seconds of the pickaxe a block of that
  tier needs, beyond the stone one every body is assumed to have (0 when none)."
  [md basis crafted tool-price]
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
                               (+ (:farm-s basis) (get-in basis [:find :crop]))
                               (+ (dig-seconds md basis b) (find-seconds md basis b) (tool-price (block-tier md b))))]
                    (reduce (fn [acc [item n]] (if (pos? n) (update acc item #(min (or % js/Infinity) (/ cost n))) acc))
                            acc (block-yields md basis b loot crop))))))
            {} (array-seq (.-blocksArray md)))))

(defn mob-sources
  "{item name worth} of what mobs drop (entityLoot): the kill, the hurt and the finding, per expected drop."
  [md basis]
  (let [{:keys [weapon hurt-hp-per-hp danger find]} (:mob basis)
        passive weapon/passive-max-health
        mobs (merge passive weapon/mob-max-health)]
    (reduce (fn [acc [mob hp]]
              (let [kill (weapon/kill-seconds weapon hp)
                    hurt (if (passive mob) 0 (* (health/walk-hp-seconds) hurt-hp-per-hp hp (get danger mob 1)))
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

(defn tier-floors
  "table with each tool worth at least the next weaker material's tool of its kind plus a craft step: a stone pickaxe
  is the upgrade of a wooden one, however cheap cobble is next to a walk to a tree."
  [basis overrides tools table]
  (let [ranked (sort-by (comp :rank val) (dissoc weapon/tool-material "golden"))
        kinds (distinct (keep #(second (str/split % #"_" 2)) tools))]
    (reduce (fn [t kind]
              (reduce (fn [t [prev [mat _]]]
                        (let [floor (+ (:craft-step basis) (get t (str (key prev) "_" kind) js/Infinity))
                              n (str mat "_" kind)]
                          (if (and (js/isFinite floor) (contains? tools n) (contains? t n) (not (contains? overrides n)))
                            (update t n max floor)
                            t)))
                      t (map vector ranked (rest ranked))))
            table kinds)))

(defn ref-gear-basis
  "basis with the hardness, tool tag and harvest tier of the block gear is measured digging (:gear :ref-block) read from md."
  [md basis]
  (let [b (aget (.-blocksByName md) (get-in basis [:gear :ref-block]))]
    (update basis :gear assoc :ref-hardness (.-hardness b) :ref-material (.-material b) :ref-tier (block-tier md b))))

(defn build-table-with
  "build-table's table, the blocks priced with tool-price (tier -> seconds, see block-sources)."
  [version overrides basis tool-price]
  (let [md (minecraft-data version)
        basis (full-basis basis)
        items (array-seq (.-itemsArray md))
        names (map #(.-name %) items)
        recipes (recipe-list md)
        crafted (set (map first recipes))
        ingredients (set (mapcat #(nth % 2) recipes))
        smelts (smelt-sources names)
        foods-table (foods/table-for version)
        food-floor (fn [n] (if (contains? foods/harmful n) 0 (* (:food-point-s basis) (get-in foods-table [n :points] 0))))
        sourced (merge-with min (block-sources md basis crafted tool-price) (mob-sources md basis))
        natural (into {} (map (fn [[n w]] [n (if (or (crafted n) (contains? smelts n) (used? md ingredients version n))
                                                  w
                                                  (* w (:junk-share basis)))]))
                       sourced)
        by-result (group-by first recipes)
        pinned (into {} (map (fn [[n o]] [n (override-fn o)])) overrides)
        pin (fn [n w] ((get pinned n identity) w))
        ;; nothing yields it, so it cannot be replaced: a precious food is worth what eating it gives a body at the reference health, a tool or armour piece what its uses save
        gear-basis (ref-gear-basis md basis)
        no-source (into {} (map (fn [i] (let [n (.-name i)]
                                          [n (cond (contains? foods/precious n)
                                                (precious/hp-worth {:health (:unsourced-food-health basis) :absorption 0} n precious/table-heals?)
                                                (number? (.-maxDurability i)) (gear/use-worth gear-basis n (.-maxDurability i))
                                                :else (* (:no-source-each basis) (/ 64 (max 1 (.-stackSize i)))))])))
                         items)
        step (fn [natural table]
               (let [smelt-s (+ (:smelt-step basis) (fuel-each table))]
               (into {} (map (fn [n]
                               (let [via-recipe (some->> (by-result n)
                                                         (map (fn [[_ k ins]] (/ (+ (:craft-step basis) (reduce + (map #(get table % js/Infinity) ins))) k)))
                                                         (apply min))
                                     via-smelt (some->> (smelts n) (map #(+ smelt-s (get table % js/Infinity))) (apply min))
                                     w (min (get natural n js/Infinity) (or via-recipe js/Infinity) (or via-smelt js/Infinity))]
                                 [n (pin n (if (js/isFinite w) (max w (food-floor n)) w))])))
                     names)))
        fixpoint (fn [natural]
                   (loop [t (into {} (map (fn [n] [n (pin n (get natural n js/Infinity))])) names) i 0]
                     (let [t' (step natural t)]
                       (if (or (= t t') (>= i 60)) t' (recur t' (inc i))))))
        unsourced (into {} (keep (fn [[n w]] (when-not (or (js/isFinite w) (by-result n) (smelts n)) [n (max (no-source n) (food-floor n))])))
                        (fixpoint natural))
        ;; again with the items nothing yields at their floor, so a recipe of one (a golden apple of an apple) is priced by its ingredients
        table (fixpoint (merge natural unsourced))]
    (tier-floors basis overrides (set (keep #(when (number? (.-maxDurability %)) (.-name %)) items))
                 (into {} (map (fn [[n w]] [n (if (js/isFinite w) w (pin n (max (no-source n) (food-floor n))))])) table))))

(defn tool-price-fn
  "tier -> worth per block of the cheapest pickaxe of that harvest tier (its table worth over its durability, one use per
  block), 0 up to the :dig basis's default tier. Obtaining the tool itself is priced where it is obtained."
  [md basis table]
  (let [pickaxes (filter #(and (str/ends-with? (.-name %) "_pickaxe") (pos? (or (.-maxDurability %) 0))) (array-seq (.-itemsArray md)))
        floor (get-in basis [:dig :default-tier])]
    (fn [tier]
      (if (<= tier floor)
        0
        (let [w (->> pickaxes (filter #(<= tier (weapon/tool-tier (.-name %)))) (keep #(let [w (get table (.-name %))] (when (and w (js/isFinite w)) (/ w (.-maxDurability %))))) (apply min js/Infinity))]
          (if (js/isFinite w) w 0))))))

(defn build-table
  "{item name worth} for minecraft-data version with name overrides pinned (overrides: {name override}) and the basis
  (default-basis with the deep-merged :basis): built again with the pickaxes' prices of the table before, once per harvest
  tier above the default (a diamond pickaxe is made of ore an iron pickaxe digs, so one pass would price it without the iron
  pickaxe; three passes settle the diamond tier). The extra args only key table-for's memo."
  [version overrides basis & _]
  (let [md (minecraft-data version)
        basis* (full-basis basis)]
    (nth (iterate #(build-table-with version overrides basis (tool-price-fn md basis* %))
                  (build-table-with version overrides basis (constantly 0)))
         2)))

(def table-for
  "build-table, memoized on what it reads: its arguments and the settings the hp and fall prices come from."
  (let [built (memoize build-table)]
    (fn [version overrides basis]
      (built version overrides basis (health/walk-hp-seconds) (health/max-scale) (health/heal-s-per-hp) (armour/max-fall) (craft-step) (smelt-step)))))

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
