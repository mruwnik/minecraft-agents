(ns jobs.items.obtain
  (:require [jobs.lib.child :as child]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.animals :as animals]
            [jobs.lib.blocks :as b]
            [jobs.lib.body :as body]
            [jobs.lib.declined :as declined]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.look :as look]
            [jobs.items.recipes :as recipes]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.items.craft :as craft]
            [jobs.items.get-tool :as get-tool]
            [jobs.items.smelt :as smelt]
            [jobs.lib.storage :as storage]
            [engine.game :as game]
            [jobs.lib.tools :as tools]
            [jobs.lib.trees :as trees]
            [clojure.string]))

(def doc
  "Get :count more of :item (or of any of :any-of, in that order of preference) than were carried at the first round.
  The target is booked once, so a cut or restart goes on toward the same count. One call is the whole attempt: it yields (:continue) only while a child waits on the world.

  Sources, in order (carried, chests, crafting, gathering):
  - Carried: the target is met, done.
  - :chest (in :how): the containers the body has seen (perception's seenBlocks, never x-ray) within 32 blocks,
    still there, whose zone or claim allows :take (an open, unzoned chest does; jobs.lib.access). A chest known
    to hold a wanted name (body memory :fetch/stock) is withdrawn from (jobs.storage.withdraw child :take); one of
    unknown stock is walked to and looked into, nearest first, at most 4 per obtain; one known not to hold any is
    skipped. Each chest is withdrawn from once. With none usable seen at the first round, one look around first.
  - :craft (in :how): a chain of crafts planned from the recipes (jobs.items.recipes) over what is carried: logs to
    planks to sticks to the tool, and a crafting table when the chain needs one and none is seen (crafted, then put
    down on a free cell beside the body with a jobs.blocks.place child). Each step is a jobs.items.craft child; a
    furnace product (iron_ingot from raw_iron, charcoal from a log, cooked food; jobs.items.smelt/made-from) is a step
    run by a jobs.items.smelt child, which fetches its own fuel and furnace one :depth deeper. The names are tried in
    order; the first with a plan is made, but one with no smelt step before one with. Three fruitless steps stop it (:tried :craft).
  - :gather (in :how): coal comes from a plain or deepslate ore seen; dirt is dug from seen dirt or grass; every raw item a chain lacks must be seen before it starts. A sapling (no recipe) comes from the leaves of its tree the body has seen, broken by
    jobs.gather.get-seeds (at most 60 leaves, one run; the drop is picked up). What a craft chain lacks that has no recipe (logs, coal, stone-tool material; recipes/gatherable?, stone-materials) is
    felled or mined as one child per round, only what the body has seen: logs by jobs.forestry.harvest-wood (of any species, but one only its species makes, oak_log for oak_planks, from that species' trees),
    raw fish (cod, salmon: also the input of a cooked fish's smelt step) by jobs.food.fish from water seen (its rod from a chest or a craft; string has no source, so no rod ends :no-source),
    cobblestone and ore drops (coal, raw_iron, raw_gold, diamond ..: from the plain or deepslate ore seen) by jobs.gather.mine (its pickaxe only from a chest or a craft, never gathered: no mine inside a mine's fetch, so no loop back into obtain). The crafts follow once the chain is whole (a chain that is craftable now
    is crafted first). A child that brings in nothing three times stops it (:tried :gather). A mined need (seen, or
    with :explore) whose block no carried tool harvests gets the cheapest tool that does first (an obtain child, slot
    :pickaxe: wooden_pickaxe for stone and coal, stone_pickaxe for iron); a mine that ends :no-tool (its pickaxe broke) does not count as a nothing run, that tool is got again the same way.
    With :explore, an unseen stone material or ore other than coal is left to the mine child (it tunnels for ore); any other unseen raw need is searched for (jobs.explore.search child, slot :explore, at most 3 per need;
    a notice :obtain.exploring says so); none found stops it (:tried {:gather {:searched ..}}).
  - Nothing else: stopped :no-source with :tried.

  The check waits {:reason :no-source :why text :item|:any-of} (:why: what each source lacks) before the first round (once the round has looked around, when chests are allowed) when nothing is carried enough, no
  usable chest that might hold it is seen and no craft chain makes it from what is carried, and nothing seen to gather what a chain lacks (with :explore, a chain whose raw needs are unseen passes: the search child solves it; when every raw need is seen and the gather child's own check waits (night in a shelter), that wait stands instead). Ends {:status :done :got n :item name} or {:status :stopped
  :reason r :got n :tried {..}}, r :no-source, :cycle (a wanted name is on :chain), :timeout (:minutes without what is
  carried changing) or :bad-args. :depth is the nesting left for sources that need fetches of their own (not used yet: crafting
  plans its whole chain; a gather child fetches its own tools).")

(a/defargs args
  {:item {:doc "the item to get" :spec a/item? :default nil}
   :any-of {:doc "items, any one will do, the first preferred (instead of :item)" :spec (a/coll-of a/item?) :default nil}
   :count {:doc "how many more than carried at the start (counts above 64 are taken as 64)" :spec (a/int-in 1 nil) :default 1}
   :how {:doc "sources to use, a subset of #{:chest :craft :gather}; nil: all" :spec (a/coll-of #{:chest :craft :gather}) :default nil}
   :depth {:doc "nested fetches left; nil: the fetch limits (jobs.lib.fetch)" :spec (a/int-in 0 nil) :default nil}
   :minutes {:doc "minutes without a change in what is carried before it stops :timeout (a long chain keeps going while it makes progress); nil: the fetch limits" :spec (a/num-in 0 nil) :default nil}
   :fail-minutes {:doc "passed on to nested fetches; nil: the fetch limits" :spec (a/num-in 0 nil) :default nil}
   :chain {:doc "items being fetched above this one (a cycle stops)" :spec (a/coll-of a/item?) :default []}
   :explore {:doc "walk and look for a raw need not seen: false, true (the ::explore setting) or jobs.explore.search options over it" :spec (a/or-of boolean? map?) :default false}})

(a/defargs settings
  {::max-inspections {:default 4 :doc "Looks at a place before the search for it gives up." :spec (a/int-in 0 nil)}
   ::craft-radius {:default 32 :doc "Blocks within which a crafting table is looked for." :spec (a/int-in 0 nil)}
   ::max-fruitless {:default 3 :doc "Rounds without progress before the job gives up." :spec (a/int-in 0 nil)}
   ::max-gap-ms {:default 60000 :doc "Longest gap between two steps that counts toward :minutes; a longer one (a preemption) counts this much." :spec (a/num-in 0 nil)}
   ::seen-radius {:default 48 :doc "Blocks within which a seen raw block (ore, dirt, leaves) counts as a source; the gather gets the same radius." :spec (a/int-in 0 64)}
   ::sheep-radius {:default 24 :doc "Blocks within which sheep are looked for when wool is gathered." :spec (a/num-in 0 nil)}
   ::explore {:default {:max-distance 64 :max-legs 12 :timeout-s 300} :doc "jobs.explore.search options of a search for a raw need (:explore true)." :spec map?}})
(defn max-inspections [] (settings/get settings ::max-inspections))
(defn max-gap-ms [] (settings/get settings ::max-gap-ms))
(defn sheep-radius [] (settings/get settings ::sheep-radius))
(defn seen-radius [] (settings/get settings ::seen-radius))

(defn names-of [{:keys [item any-of]}]
  (vec (if item [item] any-of)))

(defn wanted-fields
  "{:item name} or {:any-of [..]} of the args, for reasons and results."
  [a]
  (into {} (remove (comp nil? val)) (select-keys a [:item :any-of])))

(defn args-error [a]
  (let [names (names-of a)]
    (cond
      (not (and (seq names) (every? string? names))) (str "needs :item, or :any-of [names], got :item " (pr-str (:item a)) " :any-of " (pr-str (:any-of a)))
      (not (and (int? (:count a)) (pos? (:count a)))) (str ":count is a positive whole number, got " (pr-str (:count a))))))

(defn carried-of [inv names]
  (reduce + 0 (map #(storage/carried inv %) names)))

(defn limits
  "The fetch limits of the call; :how given as a list (a JSON array) is read as a set."
  [c]
  (cond-> (fetch/merge-limits 'jobs.items.obtain nil (fetch/body-limits (ctx/view c)) (:args c))
    (:how (:args c)) (update :how set)))

(defn holds [items names]
  (first (filter #(pos? (get items % 0)) names)))

(defn candidates
  "Usable seen chests not yet withdrawn from or given up: [{:pos :stock items-or-nil}]."
  [c names]
  (let [stock (fetch/stock-of (ctx/view c))
        done (:done-chests (ctx/mem c) #{})]
    (keep (fn [pos]
            (let [cell (fetch/cell-of pos)
                  items (get stock cell)]
              (when-not (or (done cell) (and items (not (holds items names))))
                {:pos pos :stock items})))
          (fetch/usable-chests c))))

(defn craft-radius [] (settings/get settings ::craft-radius))

(def ore-drops
  "The items mined from ores (raw_iron, coal, diamond ..): gathered, never crafted."
  (set (vals b/ores)))

(def fish-items "Raw fish: caught with a rod (jobs.food.fish), never crafted." #{"cod" "salmon"})

(defn source-opts
  "The recipes/plan options of the gather and smelt sources: ore drops count as gathered, and every furnace product
  (jobs.items.smelt/made-from; charcoal from any overworld log) is also made by a smelt step."
  []
  {:raw (into ore-drops fish-items)
   :smelts (assoc (into {} (map (fn [[out in]] [out [in]])) smelt/made-from) "charcoal" trees/log-names)})

(defn furnace-free
  "Of plans in order of preference, the first with no smelt step (a furnace needs fuel and time), else the first."
  [plans]
  (or (first (remove (fn [pl] (some #(= :smelt (:op %)) (:steps pl))) plans)) (first plans)))

(defn carried-counts [p]
  (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} (u/inventory p)))

(defn craft-plan
  "{:item name :steps [..]} for the first of names a craft chain makes n more of from what is carried, else nil."
  [c names n]
  (let [p (:primitives c)
        have (carried-counts p)
        mem (ctx/mem c)
        table? (and (not (get-in mem [:craft :table-unreachable]))
                    (boolean (or (:table mem) (craft/nearest-table p (craft-radius)))))
        version (game/version-of p)]
    (furnace-free
     (keep (fn [name]
             (when-let [pl (recipes/plan version have name n (assoc (source-opts) :table? table?))]
               (assoc pl :item name)))
           names))))

(defn leaves-of
  "The leaf block that drops this sapling (oak_sapling: oak_leaves; mangrove_propagule: mangrove_leaves), or nil."
  [name]
  (cond
    (clojure.string/ends-with? name "_sapling") (str (subs name 0 (- (count name) (count "_sapling"))) "_leaves")
    (= "mangrove_propagule" name) "mangrove_leaves"))

(def grass-seeds
  "The seed items broken out of grass, and the grass blocks that drop them."
  {"wheat_seeds" #{"short_grass" "tall_grass"}})

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

(defn wool? [name] (clojure.string/ends-with? name "_wool"))

(defn wool-color [name] (subs name 0 (- (count name) (count "_wool"))))

(defn sheep-in-range
  "The adult unsheared sheep within the sheep radius, as JS entities."
  [c]
  (remove #(or (true? (.-baby %)) (true? (.-sheared %))) (animals/herd (:primitives c) "sheep" (sheep-radius))))

(def wool-keep "Sheep of a colour combat.hunt spares (its :keep default for animals); a colour with no more is no source." 2)

(defn pick-plan
  "Of the gather plans (one per wanted name) the one that gathers the least wool, so the colour the body carries
  most of; a plan whose sheep are not in range comes after one whose are. {:wool-fallback {:wanted :taking}} on
  the plan when the colour the body carries most of has no sheep in range."
  [c plans]
  (let [wool-of (fn [pl] (into {} (filter (comp wool? key)) (:gather pl)))]
    (if-not (some (comp seq wool-of) plans)
      (furnace-free plans)
      (let [sorted (sort-by #(reduce + 0 (vals (wool-of %))) plans)
            colors (set (keep (fn [[k n]] (when (> n wool-keep) k)) (frequencies (keep #(.-color %) (sheep-in-range c)))))
            colors-of (fn [pl] (map wool-color (keys (wool-of pl))))
            ok? (fn [pl] (every? colors (colors-of pl)))
            chosen (or (first (filter ok? sorted)) (first sorted))
            wanted (first sorted)]
        (cond-> chosen
          (and (not= chosen wanted) (seq (wool-of chosen)) (seq (wool-of wanted)))
          (assoc :wool-fallback {:wanted (first (colors-of wanted)) :taking (first (colors-of chosen))}))))))

(declare gather-needs seen-of)

(defn raw-seen?
  "Whether every raw need of a gather plan (but wool, left to pick-plan) has a block seen: of several names, the plans
  whose blocks are seen come before one that would have to look for them (coal unseen, logs seen: charcoal)."
  [c plan]
  (every? #(or (:sheep %) (seen-of c %)) (some-> plan :gather (gather-needs (game/version-of (:primitives c))))))

(defn log? [name] (clojure.string/ends-with? name "_log"))

(defn any-log
  "The gather map with the logs any species would do as \"log\" (summed); a log only its own species makes (oak_log
  for oak_planks) keeps its name: it stays the plan's even with plenty of another species carried."
  [gather version have name n opts]
  (let [free? (fn [k] (let [other (first (remove #{k} trees/log-names))
                            pl (recipes/plan version (assoc have other 1000) name n opts)]
                        (not (pos? (get-in pl [:gather k] 0)))))
        free (filter (fn [[k _]] (and (log? k) (free? k))) gather)]
    (cond-> (apply dissoc gather (map key free))
      (seq free) (update "log" (fnil + 0) (reduce + 0 (vals free))))))

(defn gather-plan
  "The craft chain for the first of names that has one once the raw items it lacks are gathered, with :gather
  {name n} when it lacks any, else nil. A sapling has no recipe: its plan is just :gather {name n}, when its leaves are seen (or its child is running).
  Of several names that lack wool, the one gathering the least (pick-plan)."
  [c names n]
  (let [p (:primitives c)
        have (carried-counts p)
        version (game/version-of p)
        seen? #(seq (look/seen-blocks p {:names (vec (material-blocks version %)) :radius (seen-radius) :max 1}))
        seen (set (filter seen? (recipes/stone-materials version)))
        materials (if (seq seen) seen (recipes/stone-materials version))
        plan-of (fn [name]
                  (if-let [seen-of-name (cond (= "dirt" name) dirt-blocks
                                              (leaves-of name) #{(leaves-of name)}
                                              (grass-seeds name) (grass-seeds name))]
                    (when (or (= name (get-in (ctx/mem c) [:gather :item]))
                              (seq (look/seen-blocks p {:names (vec seen-of-name) :radius (seen-radius) :max 1})))
                      {:steps [] :gather {name n} :item name})
                    (let [opts (merge (source-opts)
                                      {:table? (boolean (or (:table (ctx/mem c)) (craft/nearest-table p (craft-radius))))
                                       :gather? true :materials materials})]
                      (when-let [pl (recipes/plan version have name n opts)]
                        (-> pl
                            (update :gather any-log version have name n opts)
                            (assoc :item name))))))]
    (if (= 1 (count names))
      (plan-of (first names))
      (let [plans (keep plan-of names)]
        (pick-plan c (or (not-empty (filter #(raw-seen? c %) plans)) plans))))))

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
          (for [[k n] gather :when (wool? k)]
            {:key k :count n :job 'jobs.combat.hunt :sheep (wool-color k)
             :args {:mob "sheep" :color (wool-color k) :count n :radius (sheep-radius)}})
          (when (pos? logs) [{:key "log" :count logs :job 'jobs.forestry.harvest-wood :seen log? :args {:fetch #{:item}}}])
          (for [[k n] gather :when (log? k)]
            {:key k :count n :job 'jobs.forestry.harvest-wood :seen #{k} :args {:species (trees/species-of k) :fetch #{:item}}})
          (for [[k n] gather :when (material-blocks version k)]
            (let [blocks (material-blocks version k)]
              {:key k :count n :job 'jobs.gather.mine :seen blocks :tunnel? true
               :args {:block (first (sort blocks)) :item k :count n :fetch mine-fetch :radius (seen-radius)}}))
          (for [[k n] gather :when (fish-items k)]
            {:key k :count n :job 'jobs.food.fish :seen #{"water"} :args {:count n :fetch #{:item}}})
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

(defn gather-carried [p {:keys [key]}]
  (reduce + 0 (for [[k v] (carried-counts p) :when (if (= "log" key) (log? k) (= key k))] v)))

(defn cut-off?
  "Whether the block b stands in a column of a tree an earlier gather found cut off."
  [c b]
  (contains? (get (ctx/mem c) :cut-off #{}) [(:x (:pos b)) (:z (:pos b))]))

(defn seen-blocks-of
  "The seen blocks of a need (within ::seen-radius), nearest first, with those a zone, claim or plan footprint bars
  taking left out unless all? is true."
  [c need all?]
  (when (:seen need)
    (let [in (access/rules-input c)
          blocks (look/seen-blocks (:primitives c) {:match #((:seen need) %) :radius (seen-radius) :max (if all? 1 64)})]
      (if all? blocks (filter #(and (:ok (access/may? in :harvest (:pos %))) (not (cut-off? c %))) blocks)))))

(defn seen-of
  "The nearest seen block of a need that may be taken, or for a wool need a sheep of its colour in range, else nil."
  [c need]
  (if-let [color (:sheep need)]
    (first (filter #(= color (.-color %)) (sheep-in-range c)))
    (first (seen-blocks-of c need false))))

(defn with-block
  "The need with :block of its args the kind of block seen when it could be either (a coal ore, dirt or grass); a wool need shears when shears are carried, else hunts."
  [c need]
  (cond
    (and (:sheep need) (pos? (storage/carried (u/inventory (:primitives c)) "shears")))
    (assoc need :job 'jobs.animals.shear :args {:color (:sheep need) :radius (sheep-radius)})

    :else
    (if-let [name (when (or (:ore? need) (= "dirt" (:key need))) (:name (seen-of c need)))]
      (assoc-in need [:args :block] name)
      need)))

(defn with-count
  "The need with the logs to carry in all (harvest-wood's :count) in its args: those carried and those it lacks."
  [p need]
  (if (= 'jobs.forestry.harvest-wood (:job need))
    (assoc-in need [:args :count] (+ (gather-carried p need) (:count need)))
    need))

(defn with-avoid
  "The need with the columns of trees found cut off in its args when it fells trees."
  [c need]
  (if-let [cut (seq (:cut-off (ctx/mem c)))]
    (cond-> need (= 'jobs.forestry.harvest-wood (:job need)) (assoc-in [:args :avoid] (vec cut)))
    need))

(defn gather-slot
  "The child slot of a gather need: one per source, since a slot's memory outlives its job while it runs and another job must not start over it."
  [need]
  (keyword (str "gather-" (:key need))))

(declare tools-wanted explorable? gather-viable?)

(defn tool-viable?
  "Whether a source of the obtain child that gets a tool for need would start: a usable seen chest, a craft plan, or a
  gather of its own (recursion ends at the wooden pickaxe, whose logs need no tool)."
  [c names need]
  (let [tools (tools-wanted c names need)
        how (:how (limits c))]
    (boolean (or (and (contains? how :chest) (seq (candidates c tools)))
                 (and (contains? how :craft) (craft-plan c tools 1))
                 (and (contains? how :gather) (or (gather-viable? c tools 1) (explorable? c tools 1)))))))

(defn gather-viable?
  "Whether a chain is craftable once raw items are gathered, and a block for every raw need has been seen and
  the child of the first would run (or, when no carried tool harvests its block, the tool's obtain child would)."
  [c names n]
  (when-let [needs (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))) seq)]
    (and (every? #(seen-of c %) needs)
         (let [need (with-block c (first needs))]
           (boolean (if (tools-wanted c names need)
                      (tool-viable? c names need)
                      (ctx/check-child c (gather-slot need) (:job need) (:args need))))))))

(defn explore-opts
  "The search options of :explore, or nil when it is off."
  [c]
  (let [e (:explore (:args c))]
    (cond (map? e) (merge (settings/get settings ::explore) e)
          e (settings/get settings ::explore))))

(defn search-names
  "The block names a search for a need looks for."
  [need]
  (if (set? (:seen need)) (vec (sort (:seen need))) trees/log-names))

(defn explorable?
  "Whether :explore is on and a raw need of the chain is not seen: the search child solves it, when the tool that
  harvests its block (an unseen ore is mined by the pickaxe for its block) can be had (tool-viable?)."
  [c names n]
  (boolean (and (explore-opts c)
                (some #(and (not (:sheep %)) (not (seen-of c %))
                            (or (not (tools-wanted c names %)) (tool-viable? c names %)))
                      (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))))))))

(defn unseen-why
  "Text for the gather source: the raw needs of the chain whose blocks are not seen, those seen but not taken (a zone,
  claim or plan footprint refuses them), or the general text."
  [c names n]
  (let [needs (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))))
        seen (filter #(and (not (:sheep %)) (not (seen-of c %)) (seq (seen-blocks-of c % true))) needs)
        unseen (remove #(or (seen-of c %) (seq (seen-blocks-of c % true))) needs)
        label (fn [{:keys [key seen sheep]}]
                (if sheep (str key " (from " sheep " sheep within " (sheep-radius) " blocks)") (if (and (set? seen) (not= #{key} seen)) (str key " (from " (clojure.string/join "/" (sort seen)) ")") key)))]
    (str "nothing seen to gather what a craft lacks"
         (when (seq unseen) (str ": " (clojure.string/join ", " (map label unseen))))
         (when (seq seen)
           (str "; seen but not taken (a zone, claim or plan footprint may refuse it): "
                (clojure.string/join ", " (for [need seen :let [b (first (seen-blocks-of c need true))]]
                                            (str (label need) (when-let [p (some-> b :pos)] (str " at " (:x p) " " (:y p) " " (:z p)))))))))))

(defn pickaxe-why
  "For a stone material wanted with no pickaxe carried: that it needs one, and what crafting the cheapest lacks; else nil."
  [version have names]
  (when (and (some #(material-blocks version %) names)
             (not-any? (fn [[k v]] (and (pos? v) (clojure.string/ends-with? k "_pickaxe"))) have))
    (str "mining it needs a pickaxe, none carried; " (recipes/lacking version have "wooden_pickaxe" 1))))

(defn no-source-why
  "Text for the no-source wait: what each allowed source lacks."
  [c names n]
  (let [how (:how (limits c))
        have (carried-counts (:primitives c))
        version (game/version-of (:primitives c))]
    (clojure.string/join
     "; "
     (concat (when (contains? how :chest) [(str "no seen chest that may hold " (clojure.string/join "/" names))])
             (when (contains? how :craft) [(recipes/lacking version have (first names) n)])
             (when (contains? how :gather) [(unseen-why c names n)])
             (when-let [w (and (contains? how :gather) (pickaxe-why version have names))] [w])))))

(defn tried-text
  "What a gather that was tried and failed adds to the no-source text (\"; gathering was tried and ended ..\", with the
  child's reason when it gave one), or nil when none was tried."
  ([tried] (tried-text tried nil))
  ([tried why]
   (when-let [g (:gather tried)]
     (str "; gathering was tried and ended " (pr-str g) (when why (str " (" why ")"))))))

(defn gather-held?
  "Whether every raw need of the chain is seen and the first one's child declined with a wait reason of its own
  (night in a shelter ..), left standing as the job's: the source is there, only not now."
  [c names n]
  (let [needs (some-> (gather-plan c names n) :gather (gather-needs (game/version-of (:primitives c))) seq)]
    (boolean (and needs (every? #(seen-of c %) needs) (some-> (:wait c) deref)))))

(defn check [c]
  (let [a (:args c)
        names (names-of a)]
    (cond
      (args-error a) true
      (declined/key* (ctx/mem c)) (declined/check c)
      (:start (ctx/mem c)) true
      (and (contains? (:how (limits c)) :chest) (seq (candidates c names))) true
      (and (contains? (:how (limits c)) :craft) (craft-plan c names (:count a))) true
      (and (contains? (:how (limits c)) :gather) (gather-viable? c names (:count a))) true
      (and (contains? (:how (limits c)) :gather) (explorable? c names (:count a)))
      (if-let [hurt (body/too-hurt (:primitives c))] (ctx/wait c hurt) true)
      (and (contains? (:how (limits c)) :gather) (gather-held? c names (:count a))) false
      :else (let [reason (merge {:reason :no-source :why (no-source-why c names (:count a))} (wanted-fields a))]
              (if (contains? (:how (limits c)) :chest)
                (look/wait-unless-surveyed c reason)
                (ctx/wait c reason))))))

(defn survey-first?
  "Before the first round, with chests allowed, none usable seen that might hold one of names and no look around yet:
  the round looks around first, so a chest behind the body is preferred to a craft or gather."
  [c names]
  (and (not (:start (ctx/mem c)))
       (not (look/surveyed? c))
       (contains? (:how (limits c)) :chest)
       (empty? (candidates c names))))

(defn stop! [c reason extra]
  (let [a (:args c)
        res (merge {:status :stopped :reason reason :got (:got extra 0)} (wanted-fields a) extra)]
    (ctx/result! c res)
    :done))

(defn tried! [c k v] (ctx/update-mem! c assoc-in [:tried k] v) :again)

(defn ^:async withdraw! [c pos items names have]
  (let [name (holds items names)
        need (- (:target (:start (ctx/mem c))) have)
        r (await (ctx/call-child c :take 'jobs.storage.withdraw
                                 {:chest pos :items {name (+ (storage/carried (u/inventory (:primitives c)) name) need)}}))]
    (when (#{:done :declined} r)
      (ctx/update-mem! c update :done-chests (fnil conj #{}) (fetch/cell-of pos)))
    (if (= :continue r) :continue :again)))

(defn ^:async inspect!
  "Walk to within 3 of the chest (a go-to child), look into it and book its stock. :continue while go-to waits, else :again."
  [c pos]
  (let [cell (fetch/cell-of pos)
        mark! #(ctx/update-mem! c (fn [m] (-> m (update :done-chests (fnil conj #{}) cell) (update :inspected (fnil inc 0)))))
        w (when-not (u/within? (u/self-pos c) pos 3)
            (await (child/call-walk c :walk {:pos pos :range 3 :escalate false :warn false :retry false :zone-tolls true})))]
    (cond
      (= :continue w) :continue
      (and w (not (:arrived (ctx/child-result c :walk)))) (do (mark!) :again)
      :else
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos pos})))]
        (if (= "ok" (.-status seen))
          (do (fetch/note-stock! c pos (.-items seen))
              (ctx/update-mem! c update :inspected (fnil inc 0)))
          (mark!))
        :again))))

(defn max-fruitless [] (settings/get settings ::max-fruitless))

(defn fruitless! [c why]
  (ctx/update-mem! c update-in [:craft :fruitless] (fnil inc 0))
  (ctx/update-mem! c assoc-in [:craft :why] why)
  :again)

(defn smelt-fetch
  "The :fetch limits of a smelt child (its fuel and furnace): this obtain's, one level deeper."
  [c]
  (let [o (limits c)]
    (into {} (remove (comp nil? val))
          {:depth (max 0 (dec (:depth o))) :how (:how o) :minutes (:minutes o) :fail-minutes (:fail-minutes o)
           :explore (:explore (:args c))})))

(defn wood-fuel
  "The planks kind to burn: the first of the smelt's wood fuels carried, else the first."
  [inv]
  (let [fuels (smelt/fetch-wood-fuels)]
    (or (first (filter #(pos? (carried-of inv [%])) fuels)) (first fuels))))

(defn ^:async smelt-step!
  "Run a smelt step of the plan as a jobs.items.smelt child (it fetches its own fuel and furnace) until it ends.
  While it cooks the child declines (:cooking): the round ends :declined and the check waits with its reason.
  Charcoal burns wood (planks) as fuel: coal or charcoal as its fuel would ask for charcoal again."
  [c step]
  (ctx/update-mem! c assoc-in [:craft :step] step)
  (let [r (await (declined/call-child! c :smelt 'jobs.items.smelt
                                                 (cond-> {:item (:item step) :count (:count step) :fetch (smelt-fetch c)}
                                                   (= "charcoal" (:item step)) (assoc :fuel (wood-fuel (u/inventory (:primitives c)))))))]
    (if (#{:continue :declined} r)
      r
      (let [res (ctx/child-result c :smelt)]
        (ctx/update-mem! c update :craft dissoc :step)
        (if (and (= :done r) (pos? (:smelted res 0)))
          :again
          (fruitless! c (or (:reason res) :declined)))))))

(defn ^:async craft-step!
  "One round of the craft source: run the current step of the plan as a child, until it ends."
  [c names have target]
  (let [mem (:craft (ctx/mem c))
        step (or (:step mem)
                 (some-> (craft-plan c names (- target have)) :steps first))]
    (cond
      (>= (:fruitless mem 0) (max-fruitless))
      (tried! c :craft (or (:why mem) :failed))

      ;; no plan without the seen table: try that table again (it may be reachable now); fruitless counts bound the retries
      (and (nil? step) (:table-unreachable mem))
      (do (ctx/update-mem! c update :craft dissoc :table-unreachable)
          (fruitless! c :table-unreachable))

      (nil? step)
      (tried! c :craft :no-plan)

      (= :place (:op step))
      (let [spot (or (:spot mem) (await (craft/spot! c)))]
        (cond
          (= :continue spot) :continue
          (not spot) (fruitless! c :no-table-spot)
          :else
          (do (ctx/update-mem! c assoc-in [:craft :step] step)
              (ctx/update-mem! c assoc-in [:craft :spot] spot)
              (let [place #(ctx/call-child c :place 'jobs.blocks.place {:item (:item step) :pos spot :fetch false})
                    r (await (if (= "crafting_table" (:item step))
                               (ctx/atomic c :urgent :place-table (fn ^:async place-table [] (await (place))))
                               (place)))]
                (if (= :continue r)
                  :continue
                  (let [res (ctx/child-result c :place)]
                    (ctx/update-mem! c update :craft dissoc :step :spot)
                    (if (and (= :done r) (:placed res))
                      (do (ctx/update-mem! c assoc :table spot)
                          (ctx/update-mem! c update :craft dissoc :table-unreachable)
                          :again)
                      (fruitless! c (or (:reason res) :place-declined)))))))))

      (= :smelt (:op step))
      (await (smelt-step! c step))

      :else
      (do (ctx/update-mem! c assoc-in [:craft :step] step)
          (let [r (await (ctx/call-child c :craft 'jobs.items.craft (cond-> {:item (:item step) :count (:count step) :fetch false}
                                                                        (and (:table? step) (:table (ctx/mem c))) (assoc :table (:table (ctx/mem c))))))]
            (if (= :continue r)
              :continue
              (let [res (ctx/child-result c :craft)]
                (ctx/update-mem! c update :craft dissoc :step)
                (cond
                  (and (= :done r) (pos? (:made res 0))) :again

                  ;; the seen table cannot be reached: plan again without it, so a carried or new table is put down
                  (and (#{"unreachable" "no-table"} (:reason res)) (not (:table-unreachable mem)))
                  (do (ctx/update-mem! c assoc-in [:craft :table-unreachable] true) :again)

                  :else
                  (fruitless! c (or (:reason res) (when (:short res) {:short (:short res)}) :declined))))))))))

(defn fruitless-limit
  "Fruitless runs that end the gather source: the need's :max-runs, but only for a run that happened; a child that
  was declined (:declined: it could not start) broke nothing and counts against the default."
  [need r]
  (if (= :declined r) (max-fruitless) (:max-runs need (max-fruitless))))

(defn tools-wanted
  "For a mined need (dirt is dug by hand) whose block the carried tools do not harvest, with :craft allowed: the tools
  that do, cheapest first (wooden_pickaxe for stone, stone_pickaxe for iron_ore), less those being fetched above; else nil."
  [c names need]
  (let [p (:primitives c)
        block (:block (:args need))
        tools (when (and (= 'jobs.gather.mine (:job need)) (not= "dirt" (:key need)) block
                         (contains? (:how (limits c)) :craft) (not (tools/can-harvest? p block)))
                (get-tool/tools-for p {:block block}))]
    (when (vector? tools)
      (not-empty (vec (remove (set (concat names (:chain (:args c)))) tools))))))

(defn pickaxe-first? [c names need] (boolean (tools-wanted c names need)))

(defn gather-failed!
  "A gather run that brought nothing. Trees seen but unreachable (:no-tree) with :explore on send the body to look
  elsewhere (bounded by [:explored key]); a mine that lost its pickaxe (:no-tool) that the pickaxe step can replace
  goes to that step; neither counts as a fruitless run."
  [c names need why r]
  (cond
    (and (= :no-tree why) (explore-opts c) (< (get-in (ctx/mem c) [:explored (:key need)] 0) (max-fruitless)))
    (do (ctx/update-mem! c assoc-in [:gather :unreachable (:key need)] true) :again)

    (and (= :no-tool why) (pickaxe-first? c names need)) :again

    :else
    (let [k (inc (get-in (ctx/mem c) [:gather :fruitless] 0))]
      (ctx/update-mem! c assoc-in [:gather :fruitless] k)
      (when (>= k (fruitless-limit need r)) (tried! c :gather (or why :failed)))
      :again)))

(defn pickaxe-args
  "The args of the obtain child that gets a tool for need."
  [c names need]
  (let [a (:args c)
        o (limits c)]
    {:any-of (tools-wanted c names need) :count 1 :how (:how o) :depth (max 0 (dec (:depth o)))
     :minutes (:minutes o) :fail-minutes (:fail-minutes o) :chain (into (vec (:chain a)) names)
     :explore (:explore a)}))

(defn ^:async pickaxe-step!
  "Get the cheapest tool that harvests the need's block (a jobs.items.obtain child): :again once it is carried, :declined
  while the tool's smelt cooks, else the gather source fails."
  [c names need]
  (let [args (pickaxe-args c names need)
        r (await (declined/call-child! c :pickaxe 'jobs.items.obtain args))]
    (cond
      (= :continue r) :continue
      (and (= :declined r) (= :cooking (:reason (b/child-wait c :pickaxe 'jobs.items.obtain args)))) :declined
      (pickaxe-first? c names need)
      (tried! c :gather {:pickaxe (or (:reason (ctx/child-result c :pickaxe)) r)})
      :else :again)))

(defn ^:async explore-walk!
  "Walk and look for need (a jobs.explore.search child): :again once it is seen; a search that finds none stops the
  gather source, as does the third search for one need."
  [c need]
  (let [targets (search-names need)
        o (explore-opts c)
        here (.-pos (.self (:primitives c)))
        _ (ctx/emit! c :obtain.exploring :info
                     {:message (str "nothing seen to get " (clojure.string/join "/" targets) ": walking to look for it, up to " (:max-distance o)
                                    " blocks out and " (:max-legs o) " legs, from " (js/Math.floor (.-x here)) " "
                                    (js/Math.floor (.-y here)) " " (js/Math.floor (.-z here)))
                      :attention :notice
                      :for targets :max-distance (:max-distance o) :max-legs (:max-legs o)
                      :from (mapv #(js/Math.floor %) [(.-x here) (.-y here) (.-z here)])})
        r (await (ctx/call-child c :explore 'jobs.explore.search
                                 (merge (explore-opts c) {:target targets :count 1 :scan-radius 16 :use-notes false :takeable true :avoid (vec (:cut-off (ctx/mem c)))})))]
    (if (= :continue r)
      :continue
      (let [res (ctx/child-result c :explore)
            k (inc (get-in (ctx/mem c) [:explored (:key need)] 0))]
        (ctx/update-mem! c assoc-in [:explored (:key need)] k)
        (ctx/update-mem! c update :gather update :unreachable dissoc (:key need))
        (if (and (= :found (:reason res)) (seen-of c need))
          :again
          (tried! c :gather (cond-> {:searched (:key need) :for targets :result (or (:reason res) r)}
                              (:why res) (assoc :why (:why res))
                              (:coverage res) (assoc :legs (:legs (:coverage res)) :farthest (:farthest (:coverage res))))))))))

(defn ^:async explore-step!
  "explore-walk! unless the body is too hurt (jobs.lib.body/too-hurt): then :continue until it has healed."
  [c need]
  (if (body/too-hurt (:primitives c))
    :continue
    (await (explore-walk! c need))))

(defn ^:async gather-step!
  "One round of the gather source: the child for the first raw item the chain lacks, until it ends."
  [c names have target]
  (let [p (:primitives c)
        pl (gather-plan c names (- target have))
        need (some->> (some-> pl :gather (gather-need (game/version-of p))) (with-block c) (with-count p) (with-avoid c))]
    (cond
      (nil? need) (tried! c :gather :no-plan)
      (and (not (get-in (ctx/mem c) [:gather :before])) (pickaxe-first? c names need) (or (explore-opts c) (seen-of c need)))
      (await (pickaxe-step! c names need))
      (and (not (get-in (ctx/mem c) [:gather :before])) (explore-opts c) (not (:sheep need)) (not (:tunnel? need))
           (or (not (seen-of c need)) (get-in (ctx/mem c) [:gather :unreachable (:key need)]))
           (< (get-in (ctx/mem c) [:explored (:key need)] 0) (max-fruitless)))
      (await (explore-step! c need))
      (and (not (get-in (ctx/mem c) [:gather :before]))
           (not (if (explore-opts c)
                  (and (or (:tunnel? need) (seen-of c need)) (ctx/check-child c (gather-slot need) (:job need) (:args need)))
                  (gather-viable? c names (- target have)))))
      (tried! c :gather :none-seen)
      :else
      (do (when-not (get-in (ctx/mem c) [:gather :before])
            (ctx/update-mem! c assoc-in [:gather :before] (gather-carried p need))
            (ctx/update-mem! c assoc-in [:gather :item] (:key need))
            (when (= 'jobs.forestry.harvest-wood (:job need))
              (ctx/update-mem! c assoc-in [:gather :tree] (select-keys need [:key :job :args])))
            (when-let [{:keys [wanted taking]} (:wool-fallback pl)]
              (ctx/emit! c :obtain.wool-colour :info
                         {:message (str "no " wanted " sheep within " (sheep-radius) " blocks; taking " taking " wool instead")
                          :wanted wanted :taking taking})))
          (let [r (await (ctx/call-child c (gather-slot need) (:job need) (:args need)))]
            (if (= :continue r)
              :continue
              (let [res (ctx/child-result c (gather-slot need))
                    before (get-in (ctx/mem c) [:gather :before])]
                (ctx/update-mem! c update :gather dissoc :before :item :tree)
                (if (> (gather-carried p need) before)
                  (do (ctx/update-mem! c assoc-in [:gather :fruitless] 0) :again)
                  (do (when-let [w (:why res)] (ctx/update-mem! c assoc-in [:gather :why] w))
                      (when-let [cols (seq (:columns res))] (ctx/update-mem! c update :cut-off (fnil into #{}) (map vec) cols))
                      (gather-failed! c names need (or (:reason res) :nothing-gathered) r))))))))))

(defn ^:async finish-tree!
  "The log child of an earlier round was cut before it ended: runs it to its end even when the logs carried are enough
  (a tree it started is felled whole). :continue while it waits, else :again."
  [c]
  (let [{:keys [key job args]} (get-in (ctx/mem c) [:gather :tree])
        slot (gather-slot {:key key})
        r (if (ctx/check-child c slot job args) (await (ctx/call-child c slot job args)) :done)]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c update :gather dissoc :before :item :tree) :again))))

(defn progress
  "The stall clock: prev ({:inv carried-counts :t ms :at ms of the last look}) kept while what is carried is the same, else
  restarted at now. A gap since the last look beyond max-gap (a preemption, a long wait) does not count."
  [prev inv now max-gap]
  (if (= inv (:inv prev))
    {:inv inv :at now :t (+ (:t prev) (max 0 (- now (:at prev) max-gap)))}
    {:inv inv :t now :at now}))

(defn ^:async step! [c]
  (let [a (:args c)
        names (names-of a)
        inv (u/inventory (:primitives c))
        have (carried-of inv names)
        now (ctx/now c)]
    (if-let [e (args-error a)]
      (do (ctx/emit! c :obtain.declined :warn {:reason :bad-args :text (str "items.obtain " e)})
          (stop! c :bad-args {:why e}))
      (if (survey-first? c names)
        (do (await (look/survey! c)) (if (check c) :again :continue))
        (let [_ (when-not (:start (ctx/mem c))
                  (ctx/update-mem! c update :craft dissoc :table-unreachable)
                  (ctx/update-mem! c assoc :start {:have have :target (+ have (min 64 (:count a)))}))
              {:keys [target] :as start} (:start (ctx/mem c))
              seen-inv (carried-counts (:primitives c))
              prog (progress (:progress (ctx/mem c)) seen-inv now (max-gap-ms))
              _ (when (not= prog (:progress (ctx/mem c))) (ctx/update-mem! c assoc :progress prog))
              t (:t prog)
              got (max 0 (- have (:have start)))
              o (limits c)
              m (ctx/mem c)]
          (cond
            (some (set (:chain a)) names) (stop! c :cycle {:chain (:chain a)})
            (get-in m [:gather :tree]) (await (finish-tree! c))
            (>= have target) (do (ctx/result! c {:status :done :got got :item (or (holds (into {} (map (juxt :name :count)) inv) names) (first names))})
                                 :done)
            (> (- now t) (* 60000 (:minutes o))) (stop! c :timeout {:got got :tried (:tried m)})
            (and (contains? (:how o) :chest) (not (get-in m [:tried :chest])))
            (let [cs (candidates c names)
                  known (first (filter :stock cs))
                  unknown (first (remove :stock cs))]
              (cond
                known (await (withdraw! c (:pos known) (:stock known) names have))
                (and unknown (< (:inspected m 0) (max-inspections))) (await (inspect! c (:pos unknown)))
                :else (tried! c :chest (if (or (seq (:done-chests m)) (pos? (:inspected m 0))) :lacking :none-seen))))
            (and (contains? (:how o) :craft) (not (get-in m [:tried :craft]))
                 (or (not (contains? (:how o) :gather)) (get-in m [:craft :step]) (get-in m [:craft :table-unreachable]) (craft-plan c names (- target have))))
            (await (craft-step! c names have target))
            (and (contains? (:how o) :gather) (not (get-in m [:tried :gather])))
            (await (gather-step! c names have target))
            :else (let [unreachable (= :table-unreachable (get-in m [:tried :craft]))]
                    (stop! c (if unreachable :table-unreachable :no-source)
                           (cond-> {:got got :tried (:tried m)}
                             (not unreachable) (assoc :why (str "no source for " (clojure.string/join "/" names) ": "
                                                                (no-source-why c names (:count a))
                                                                (tried-text (:tried m) (get-in m [:gather :why])))))))))))))

(defn ^:async round
  "The whole attempt in one call: step! again until the count is carried or it stops; :continue only while a child waits on the world, :declined while a smelt child cooks."
  [c]
  (declined/begin! c)
  (await (pace/steps! c (fn ^:async obtain-step []
                                      (let [r (await (step! c))]
                                        (when (:progress (ctx/mem c)) (ctx/update-mem! c assoc-in [:progress :at] (ctx/now c)))
                                        r)))))
