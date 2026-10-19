(ns jobs.items.obtain.table
  "Which job gathers which raw item (gather-needs), the item families it knows, and the recipe options of the sources."
  (:require [engine.game :as game]
            [jobs.items.obtain.base :refer [seen-radius sheep-radius]]
            [jobs.items.recipes :as recipes]
            [jobs.items.smelt.plan :as smelt-plan]
            [jobs.lib.blocks :as b]
            [jobs.lib.trees :as trees]
            [clojure.string]))

(def ore-drops
  "The items mined from ores (raw_iron, coal, diamond ..): gathered, never crafted."
  (set (vals b/ores)))

(def fish-items "Raw fish: caught with a rod (jobs.food.fish), never crafted." #{"cod" "salmon"})

(def mob-drops
  "The items a farm animal drops, and the animal: gathered by hunting it (a kept pair and penned animals are spared)."
  {"leather" "cow" "feather" "chicken"})

(defn source-opts
  "The recipes/plan options of the gather and smelt sources: ore drops count as gathered, and every furnace product
  (jobs.items.smelt.plan/made-from; charcoal from any overworld log) is also made by a smelt step."
  []
  {:raw (conj (into ore-drops (concat fish-items (keys mob-drops))) "obsidian")
   :smelts (assoc (into {} (map (fn [[out in]] [out [in]])) smelt-plan/made-from) "charcoal" trees/log-names)})

(defn leaves-of
  "The leaf block that drops this sapling (oak_sapling: oak_leaves; mangrove_propagule: mangrove_leaves), or nil."
  [name]
  (cond
    (clojure.string/ends-with? name "_sapling") (str (subs name 0 (- (count name) (count "_sapling"))) "_leaves")
    (= "mangrove_propagule" name) "mangrove_leaves"))

(def grass-seeds
  "The seed items broken out of grass, and the grass blocks that drop them."
  {"wheat_seeds" #{"short_grass" "tall_grass"}})

(def stalks "The stalks cut from a wild stand (jobs.gather.get-seeds), the base left to grow again." #{"sugar_cane" "bamboo"})

(defn material-blocks
  "The blocks mining which gives a stone-material (stone: cobblestone), or nil for any other item. A block
  that drops itself (blackstone) is its own."
  ([item] (material-blocks game/default-version item))
  ([version item]
   (when (contains? (recipes/stone-materials version) item)
    (let [bs (set (for [[b d] b/drop-item :when (= d item)] b))]
      (if (seq bs) bs #{item})))))

(def dirt-blocks
  "The seen blocks digging which by hand gives dirt (grass drops it)."
  #{"dirt" "grass_block"})

(defn animal?
  "Whether a gather need is hunted (wool from sheep, leather from cows ..), not mined or felled."
  [need]
  (boolean (or (:sheep need) (:mob need))))

(defn wool? [name] (clojure.string/ends-with? name "_wool"))

(defn wool-color [name] (subs name 0 (- (count name) (count "_wool"))))

(def sapling-leaf-limit
  "Leaves broken for one sapling at most (a drop is about 1 in 20); one run, no retries."
  60)

(defn ore-blocks
  "The ore blocks that drop item (iron_ore and deepslate_iron_ore for raw_iron), or nil."
  [item]
  (when (contains? ore-drops item)
    (not-empty (set (for [[blk d] b/drop-item :when (and (= d item) (re-find #"_ore$" blk))] blk)))))

(def mine-fetch
  "The :fetch of a gather mine: its pickaxe from a chest or a craft only, so a mine never starts another gather."
  {:what #{:tool} :how #{:chest :craft}})

(def log-fetch
  "The :fetch of a gather of logs: a replant sapling from a chest only; breaking leaves for one is another gather, of a drop the felling may leave out of reach."
  {:what #{:item} :how #{:chest}})

(defn gather-needs
  "The raw needs of a plan's :gather map, logs first: [{:key k :count n :job sym :args {..} :seen fn of a block name: what must have been seen}]. A new gather source
  is one more case here. An ore drop (coal, raw_iron ..) is mined from a plain or a deepslate ore (:ore? true: :block
  is set by with-block to the one seen); one other than coal is :tunnel? (unseen with :explore, the mine child
  tunnels and descends for it rather than a surface search); so is a stone material (it stairs down to stone)."
  ([gather] (gather-needs gather game/default-version))
  ([gather version]
  (let [logs (get gather "log" 0)]
    (vec (concat
          (for [[sapling n] gather :when (leaves-of sapling)]
            {:key sapling :count n :job 'jobs.gather.get-seeds :seen #{(leaves-of sapling)} :max-runs 1
             :args {:item sapling :count n :sources [(leaves-of sapling)] :dry-digs sapling-leaf-limit}})
          (for [[seed n] gather :when (grass-seeds seed)]
            {:key seed :count n :job 'jobs.gather.get-seeds :seen (grass-seeds seed) :max-runs 1
             :args {:item seed :count n}})
          (for [[k n] gather :when (stalks k)]
            {:key k :count n :job 'jobs.gather.get-seeds :seen #{k} :max-runs 1
             :args {:item k :count n}})
          (when-let [n (get gather "water_bucket")]
            [{:key "water_bucket" :count n :job 'jobs.blocks.use-on :seen #{"water"} :args {:item "bucket"}}])
          (for [[k n] gather :when (mob-drops k)]
            {:key k :count n :job 'jobs.combat.hunt :mob (mob-drops k) :max-runs 1
             :args {:mob (mob-drops k) :count n :radius (sheep-radius) :drops [k]}})
          (for [[k n] gather :when (wool? k)]
            {:key k :count n :job 'jobs.combat.hunt :sheep (wool-color k) :max-runs 1
             :args {:mob "sheep" :color (wool-color k) :count n :radius (sheep-radius)}})
          (when (pos? logs) [{:key "log" :count logs :job 'jobs.forestry.harvest-wood :seen trees/log-name? :args {:fetch log-fetch}}])
          (for [[k n] gather :when (trees/log-name? k)]
            {:key k :count n :job 'jobs.forestry.harvest-wood :seen #{k} :args {:species (trees/species-of k) :fetch log-fetch}})
          (for [[k n] gather :when (material-blocks version k)]
            (let [blocks (material-blocks version k)]
              {:key k :count n :job 'jobs.gather.mine :seen blocks :tunnel? true
               :args {:block (first (sort blocks)) :item k :count n :fetch mine-fetch :radius (seen-radius)}}))
          (for [[k n] gather :when (fish-items k)]
            {:key k :count n :job 'jobs.food.fish :seen #{"water"} :args {:count n :fetch #{:item}}})
          (when-let [n (get gather "obsidian")]
            [{:key "obsidian" :count n :job 'jobs.gather.mine :seen #{"obsidian" "lava"}
              :args {:block "obsidian" :item "obsidian" :count n :mend false :fetch mine-fetch :radius (seen-radius)}}])
          (when-let [n (get gather "dirt")]
            [{:key "dirt" :count n :job 'jobs.gather.mine :seen dirt-blocks
              :args {:block "dirt" :item "dirt" :count n :fetch mine-fetch :radius (seen-radius)}}])
          (for [[k n] gather :when (ore-blocks k)]
            (let [blocks (ore-blocks k)]
              {:key k :count n :job 'jobs.gather.mine :seen blocks :ore? true :tunnel? (not= "coal" k)
               :args {:block (first (sort-by count blocks)) :item k :count n :fetch mine-fetch :radius (seen-radius)}})))))))

(defn gather-need
  "The first of gather-needs."
  ([gather] (first (gather-needs gather)))
  ([gather version] (first (gather-needs gather version))))
