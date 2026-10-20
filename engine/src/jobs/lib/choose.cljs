(ns jobs.lib.choose
  "Choosers: what a body may use for a purpose, in order. (tools-for c [:dig block] amount) answers
  {:list [{:item name :uses-left n}] :short n}: the carried tools that harvest block and are not reserved, cheapest
  material first (opts {:fast true}: best first; [:attack] always best first, swords over axes), and the uses the amount still lacks once the list is used up (the
  caller's :fetch). `reserved` is the register's held units (jobs.lib.reserve) and a body's :choose/override entries {:chooser :tools :avoid [names] :prefer [names]}.
  (fillers-for c purpose amount) answers {:list [{:item name :count n}] :short n} the same way for the blocks a
  purpose places (:build :shelter :pillar :climb :cover :seal), in the purpose's order (common first). Zones judge cells, not items: the caller asks them for the cell it digs."
  (:require [jobs.lib.blocks :as blocks]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.tool-kind :as kind]
            [jobs.lib.trees :as trees]
            [jobs.lib.util :as u]))

(def whole "Uses of a tool with no known durability: counts as whole." 1000000)

(def overrides reserve/overrides)

(defn reserved
  "{name count}: how much of each carried item is off limits to a chooser (jobs.lib.reserve/held)."
  [c chooser]
  (reserve/held c chooser))

(def kind-purposes
  "Tool purposes besides [:dig block]: the tool kinds that serve each."
  {:attack ["sword" "axe"] :till ["hoe"] :shovel ["shovel"] :shear ["shears"]})

(defn tools-for
  "See the ns doc. The purposes are [:dig block] and the kind purposes of kind-purposes ([:attack], [:till], [:shovel],
  [:shear]); any other throws."
  ([c purpose amount] (tools-for c purpose amount nil))
  ([c [what block :as purpose] amount opts]
   (when-not (or (and (= :dig what) (string? block)) (kind-purposes what))
     (throw (js/Error. (str "tools-for: unknown purpose " (pr-str purpose)))))
   (let [p (:primitives c)
         dig? (= :dig what)
         ok (if dig? (set (some-> (.harvestTools p block) js->clj)) #{})
         wanted (if dig? [(kind/tool-kind block)] (kind-purposes what))
         held (reserved c :tools)
         prefer (:prefer (overrides c :tools))
         rank-in (fn [name] (let [i (.indexOf prefer name)] (if (neg? i) (count prefer) i)))
         order (fn [t] [(rank-in (:name t))
                        (cond (= :attack what) (- (weapon/weapon-score (:name t)))
                              (:fast opts) (- (weapon/weapon-rank (:name t)))
                              :else (weapon/cheap-rank (:name t)))
                        (or (:durability t) whole)])
         fit (fn [ks] (->> (u/inventory p)
                          (filter #(and (some (partial kind/of-kind? (:name %)) ks) (or (empty? ok) (ok (:name %)))))
                          (remove #(contains? held (:name %)))
                          (remove #(zero? (or (:durability %) whole)))))
         usable (sort-by order (or (seq (fit wanted)) (some-> (kind/fallback-kind (first wanted)) vector fit)))
         uses (mapv (fn [t] {:item (:name t) :uses-left (or (:durability t) whole)}) usable)]
     {:list uses
      :short (max 0 (- amount (transduce (map :uses-left) + 0 uses)))})))

(defn dig-wear
  "{tool [n n n]} for the carried tools that dig the blocks (names), one use a block, each by the tool tools-for lists first."
  [c blocks]
  (->> blocks
       (keep #(:item (first (:list (tools-for c [:dig %] 1)))))
       frequencies
       (into {} (map (fn [[tool n]] [tool [n n n]])))))

(def building-blocks
  ["dirt" "cobblestone" "cobbled_deepslate" "stone" "andesite" "diorite" "granite" "netherrack"
   "oak_planks" "spruce_planks" "birch_planks" "jungle_planks" "acacia_planks" "dark_oak_planks"
   "mangrove_planks" "cherry_planks"])

(def filler-purposes
  "Filler blocks per purpose, in order. :shelter is :build then logs (worth more, a last resort); :climb is what an
  escape pillars with (no sand or gravel: they fall); :cover and :seal are solid blocks that do not fall."
  {:build building-blocks
   :shelter (into building-blocks trees/log-names)
   :pillar ["dirt" "cobblestone"]
   :climb ["dirt" "cobblestone" "cobbled_deepslate" "stone" "deepslate" "netherrack" "andesite" "diorite" "granite"
           "oak_planks" "spruce_planks" "birch_planks"]
   :cover ["cobblestone" "stone" "dirt" "netherrack" "cobbled_deepslate" "deepslate" "andesite" "diorite" "granite"]
   :seal ["cobblestone" "cobbled_deepslate" "deepslate" "stone" "dirt"]})

(defn fillers
  "The ordered block names for purpose, reserves ignored (a default for a job's :blocks arg). Unknown purpose throws."
  [purpose]
  (or (filler-purposes purpose)
      (throw (js/Error. (str "fillers: unknown purpose " (pr-str purpose))))))

(def danger-purposes
  "Purposes a body in danger places for: they may spend soft-reserved blocks it can dig back out (jobs.lib.blocks/carried)."
  #{:shelter :pillar :climb :cover :seal})

(defn fillers-for
  "See the ns doc."
  [c purpose amount]
  (let [held (reserved c :fillers)
        have (into {} (map (juxt :name :count)) (u/inventory (:primitives c)))
        carried (if (danger-purposes purpose)
                  (blocks/carried c (fillers purpose))
                  (for [b (fillers purpose) :let [n (- (get have b 0) (get held b 0))] :when (pos? n)] {:name b :count n}))
        listed (mapv (fn [{:keys [name count]}] {:item name :count count}) carried)]
    {:list listed
     :short (max 0 (- amount (transduce (map :count) + 0 listed)))}))

(defn filler-names
  "The block names for purpose to fetch or list, without the ones a :choose/override :avoids (what the body carries is
  fillers-for's)."
  [c purpose]
  (let [avoid (:avoid (overrides c :fillers))]
    (filterv (complement avoid) (fillers purpose))))

(defn filler
  "The first block name fillers-for lists for purpose, or nil."
  [c purpose]
  (:item (first (:list (fillers-for c purpose 1)))))

(defn cheap-blocks
  "The blocks worth at most max-value each by jobs.lib.cost/item-value."
  [blocks max-value]
  (filterv #(<= (:value (cost/item-value [{:name % :count 1}])) max-value) blocks))

(defn use-spec?
  "Whether {purpose spec} names a chooser default-use knows: {:tools :dig}, {:tools <kind purpose>} or
  {:blocks <filler purpose>}."
  [purpose spec]
  (case purpose
    :tools (boolean (or (= :dig spec) (kind-purposes spec)))
    :blocks (contains? filler-purposes spec)
    false))

(defn default-use
  "The item names a call's :use gets for purpose when the job's defaults name chooser spec for it (the :use/default
  hook; see use-spec?). {:tools :dig} is the tools for the args' :block, else the block at :pos, nil when
  the args give neither (the job's own args check ends the call); {:tools :attack|:till|:shovel|:shear} the carried tools of that purpose; {:blocks :build|...} the
  carried fillers of that purpose. nil when the list is not known yet (the block at :pos is hidden). An unknown
  spec throws."
  [c purpose spec]
  (when-not (use-spec? purpose spec)
    (throw (js/Error. (str "default :use: unknown chooser " (pr-str {purpose spec})))))
  (if (= :blocks purpose)
    (mapv :item (:list (fillers-for c spec 1)))
    (if (not= :dig spec)
      (mapv :item (:list (tools-for c [spec] 1)))
      (let [{:keys [block pos]} (:args c)]
        (when-let [block (or block (when pos (blocks/target-name (:primitives c) pos blocks/hidden-guess)))]
          (mapv :item (:list (tools-for c [:dig block] 1))))))))
