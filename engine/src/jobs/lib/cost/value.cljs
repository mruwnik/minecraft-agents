(ns jobs.lib.cost.value
  "What a list of items is worth (item-value) and what fetching them costs (fetch-cost), in one unit: about a second
  of a player's work. recover-drops compares the two; any job may ask what a pile, a chest or a kit is worth.

  The worth of one item is the cheapest way to obtain it, from minecraft-data for the body's version. Every constant
  and the few small tables (ore rarity, natural-and-craftable blocks, passive-mob health, rare drop chances) live in
  default-basis (jobs.lib.cost.table), overridable by :basis (deep-merged). The tables are defaults, not data minecraft-data has.

    block        A block's drops (blockLoot, no silk touch, chance at least :min-drop-chance): (dig + find) / drops.
                 dig = hardness x :hardness-s / the speed of the stone tool (the block's own tier if higher; the base assumes a
                 tool, never the hand);
                 find = :terrain, :wood for a log, stem, bamboo or bamboo block, or :ores {ore [find drops]} (a nil find: :ore-scan-s, fitted to the measured ores (measured-worth), over the base-rates density of the ore's best layer), or :find-s for a block. Crafted blocks are no source
                 except :natural ones (granite, clay ...); a block that is no item (redstone_wire) is none.
    crop         A block with an age state: (:farm-s + :find :crop) / the age-7 yield.
    mob          entityLoot of the mobs in weapon/mob-max-health and passive-max-health: (kill + hurt + find) / expected
                 drops; kill = the stone sword's hits, hurt = :hurt-hp-per-hp x health x :danger hp, each at health/walk-hp-seconds, find by hostile or passive.
    made         The cheapest recipe: the ingredients plus :craft-step, divided by the result count; or smelting by
                 name rule (raw_X -> X_ingot, X -> cooked_X, ancient_debris -> netherite_scrap, cobblestone -> stone,
                 sand -> glass, clay_ball -> brick): the input plus :smelt-step and the cheapest fuel's share (fuel-each). The table is a fixpoint, then each tool
                 is raised to at least the next weaker material's tool of its kind plus :craft-step.
    goal         made-value: a goal's count at least made from nothing, whole crafts and each station once, smelts with their fuel.
    unused       A sourced item that is no tool, armour, edible food, block or ingredient is worth :junk-share of that.
    food         An edible food is worth at least its points x :food-point-s.
    no source    :no-source-each (4) x 64 / stack size (a 64-stack 4, a 16-stack 16, an unstackable 256); a precious food nothing yields: jobs.lib.cost.precious/hp-worth at :unsourced-food-health (10). A tool, weapon or armour piece nothing yields: its durability x what a use saves (jobs.lib.cost.gear, :gear).
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
  (:require [engine.args :as a]
            ["minecraft-data" :as minecraft-data]
            [clojure.set :as set]
            [clojure.string :as str]
            [jobs.lib.recipes :as recipes]
            [jobs.lib.cost.table :as tbl]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.health :as health]
            [jobs.lib.foods :as foods]
            [engine.game :as game]
            [engine.settings :as settings]))

(a/defargs settings
  {::trip {:default 10 :spec (a/num-in 0 nil)
           :doc "Any fetch: turning round, finding the pile, the risk of the place one died at, in seconds."}
   ::per-block {:default 0.3 :spec (a/num-in 0.01 nil)
                :doc "One block walked, there and back about a second of a player's time per 3 blocks."}
   ::own-place-blocks {:default 8 :spec (a/num-in 0 nil)
                       :doc "Blocks of walk the placing of a furnace of one's own counts for, on top of making it (jobs.lib.cost/walk-beats-making)."}
   ::per-climb {:default 2 :spec (a/num-in 0 nil)
                :doc "One block climbed past the first on a detour (stairs or a pillar back out of a drop, or up to a ledge), in seconds."}
   ::per-dark {:default 0 :spec (a/num-in 0 nil)
               :doc "Floor of one block walked in the dark, on top of per-block; the hp the spawned mobs would cost the kit (jobs.lib.cost/dark-factor) comes on top, so a bare body pays a dark block most and an armed one less."}
   ::spawn-chance-per-s {:default 0.005 :spec (a/num-in 0 nil)
                         :doc "Chance a mob spawns and reaches the body each second it stands on a cell where one can spawn (jobs.lib.cost/spawn-chance)."}
   ::detour-factor {:default 1.27 :spec (a/num-in 1 nil)
                    :doc "Times the straight line a walk's way is taken to be (detours), for walk-blocks-per-s: 30255 blocks of steers over 23766 of straight line in 1235 walks (go-to walk jobs of 4 blocks or more), trial logs to 2026-10-28; a lower bound (steer chords). Recount: tools/event-counts.mjs walk-detour (:ratio)."}
   ::per-frontier-walk {:default 1 :spec (a/num-in 0 nil)
                        :doc "Each walk of a tour to the edge of loaded land that brought the body no nearer its goal, on top of its blocks, in seconds."}
   ::guess-rock-counts {:default {:layer-64 {:bad 51456 :ok 0} :layer-63 {:bad 202.135 :ok 50.428} :layer-62 {:bad 164.126 :ok 108.634}
                                  :layer-61 {:bad 115.175 :ok 172.317} :layer-60 {:bad 50.618 :ok 199.494}
                                  :placed {:bad 6 :ok 72} :apart {:bad 0.003 :ok 562.539}}
                        :spec (a/pred "a map of the escape/guess-classes to {:bad n :ok n} pseudo-counts >= 0"
                                      (fn [m] (and (map? m) (every? (fn [k] (let [c (get m k)] (and (map? c) (every? #(and (number? (get c %)) (<= 0 (get c %))) [:bad :ok]))))
                                                                    [:layer-64 :layer-63 :layer-62 :layer-61 :layer-60 :placed :apart]))))
                        :doc "The Beta prior per class (escape/guess-class) of a go-to dig route's guess (an unseen cell read as rock) being rock an escalation never digs (bedrock and the like), the call's own looks on top (answer/guess-bad-chance): the solid cells of 1860 sections of 16 blocks of recorded chunk files (every 400th, the fixture grid left out, 2026-10-30). In the overworld's bedrock layer (below y -59) one class per y, :layer-64 to :layer-60, 1.0, 0.80, 0.60, 0.40 and 0.20 of 51k cells each whatever is beside it; above it :placed a protected neighbour but bedrock (a build: 6 of 78 cells, too few for a spread, pooled) and :apart the rest (3.6e-6; a bedrock neighbour above the layer too, 0 of 10k). Spread fitted by moments over the sections with 20 cells of the class, as a call digs in one place (effective n 51456, 253, 273, 288, 250, 78, 563). Recount: node tools/event-counts.mjs guess-rock --stride 400; the bodies' own looks: guess-looks."}
   ::unseen-floor-counts {:default {:level {:absent 2.97 :present 9.84} :down {:absent 1.69 :present 5.71}
                                    :up {:absent 0.06 :present 1.68} :drop {:absent 1.4 :present 0.99}}
                          :spec (a/pred "a map of step kind (:level :down :up :drop) to {:absent n :present n} pseudo-counts >= 0"
                                        (fn [m] (and (map? m) (every? (fn [k] (let [c (get m k)] (and (map? c) (every? #(and (number? (get c %)) (<= 0 (get c %))) [:absent :present]))))
                                                                      [:level :down :up :drop]))))
                          :doc "The Beta prior per step kind (jobs.lib.walk.sight/floor-kind) of an unseen floor being there, a call's own floors of the kind found there or not on top (answer/floor-absent-chance): from a stand whose floor is there, the next stand's floor one block over in 2009 recorded chunk files (every 40th, the fixture grid left out, 2026-10-29), level 0.23 absent, down 0.26, up 0.03, drop (2 or 3 lower) 0.59; of a stand under sky and one under none, the worse; spread fitted by moments over the chunks, as a call walks one place (effective n 12.8, 7.4, 1.7, 2.4). Recount: node tools/event-counts.mjs floor-neighbours --stride 40; the bodies' own outcomes: unseen-floors."}
   ::unseen-edge-dark-counts {:default {:roofed {:far 2.947 :near 0.681} :sky {:far 3.838 :near 0.071}}
                              :spec (a/pred "a map of :roofed and :sky to {:far n :near n} pseudo-counts >= 0"
                                            (fn [m] (and (map? m) (every? (fn [k] (let [c (get m k)] (and (map? c) (every? #(and (number? (get c %)) (<= 0 (get c %))) [:far :near]))))
                                                                          [:roofed :sky]))))
                              :doc "The Beta prior, by the sky light where the body stands (:sky over 0, :roofed 0), that a dark place whose walkers' region ends at an unseen edge is walked to from dark floor farther than 24 blocks (answer/hall-chance): of dark stands (block light 0) in 2009 recorded chunk files (every 40th, the fixture grid left out, 2026-10-29), the share a walker reaches from such floor over the true blocks, roofed 0.80 (7214 of 8990), under sky 0.98 (3154 of 3218); a walk into land never recorded is left out, so both are low if anything; spread fitted by moments over the chunks (effective n 3.6, 3.9). Recount: node tools/event-counts.mjs dark-reach --stride 40."}
   ::unseen-spot-counts {:default {:plantable 2.417 :unfit 0.634}
                         :spec (a/pred "a map {:plantable n :unfit n} of pseudo-counts >= 0"
                                       (fn [m] (and (map? m) (every? #(and (number? (get m %)) (<= 0 (get m %))) [:plantable :unfit]))))
                         :doc "The Beta prior that a replant spot the body has not seen the soil, room or light of can be planted (answer/spot-plantable-chance): of the bases of felled trees (a log on soil) in 2009 recorded chunk files (every 40th, the fixture grid left out, 2026-10-29), 3513 of 4448 had the species' growth space free of anything a tree cannot grow through (logs counted felled) and an open side lit 9 or more, 0.79; spread fitted by moments over 482 chunks (effective n 3.1). Recount: node tools/event-counts.mjs tree-spots --stride 40."}
   ::retry-counts {:default {:craft {:win 0.141 :fail 0.013 :try-s 3.677} :dig {:win 0.018 :fail 0.135 :try-s 10.764} :furnace {:win 1 :fail 1 :try-s 0.287} :place {:win 0.095 :fail 0.106 :try-s 8.97} :walk {:win 0.032 :fail 0.074 :try-s 9.46} :walk-round {:win 0.001 :fail 0.222 :try-s 0.613} :respond {:win 0.054 :fail 0.08 :try-s 9.054} :risky-stay {:win 0.054 :fail 0.08 :try-s 5} :comb-look {:win 0.054 :fail 0.08 :try-s 5} :kit-later {:win 0.054 :fail 0.08 :try-s 30}}
                   :spec (a/pred "a map of retry kind to {:win n :fail n :try-s s}, pseudo-counts >= 0 not both 0 and seconds >= 0"
                                 (fn [m] (and (map? m) (every? (fn [{:keys [win fail try-s]}] (and (number? win) (number? fail) (number? try-s) (<= 0 win) (<= 0 fail) (pos? (+ win fail)) (<= 0 try-s)))
                                                               (vals m)))))
                   :doc "The Beta prior per retry kind that a try after a failed one wins (retry-chance; cost/try-again?), and :try-s the seconds that try takes: of the tries after a failed one in the same job in the trial bodies' logs (probes left out, 2026-10-29), :craft a craft action after a timeout, 17 of 20 won in 18 calls; :furnace a furnace action after a busy slot, none counted (the uniform prior, a visit's own seconds); :dig a dig action after a timeout, a failure or an unreachable block, 11 of 92 won in 2 calls (too few calls to fit a spread: the mean 0.12 at the mean pseudo-count total of the kinds that did fit); :place a place action after an occupied cell, no support, a failure or a timeout, or a jumpPlace action (the pillar's) after a partial, a failure or a timeout, 120 of 4856 in 116 calls; :walk a go-to after one that did not arrive, 93 of 882 in 183 calls; :walk-round a round inside one go-to after a fruitless one that planned again (go-to.retrying; walk/learn-again?, reach/pick!), counted per go-to call (a job id holds many; the first retrying is the condition), a later retrying failed and the go-to's result after one the call's last round (won when it arrived; a call that ends unseen adds none), 30 of 6175 in 5896 calls (2026-10-30), left out of :pooled; spread fitted by moments over the calls (effective n 0.17, 0.22: a call's own failures soon outweigh the pool). :respond (respond-to-hostile's call after calls that changed nothing, :hostile.call): none counted yet, so :pooled, all kinds' retries fitted together, 241 of 5850 in 319 calls; its seconds come from the caller. :risky-stay (jobs.lib.near/risky-wait!'s re-check after a stay refused for its price, :near.risky-wait and :near.risky-over): none counted yet, so :pooled; its seconds are the caller's pause. :comb-look (jobs.apiary.harvest's look for comb after one that took none, :apiary.comb-wait and :apiary.comb-over): none counted yet, so :pooled; its seconds are the comb-wait-s pause. :kit-later (jobs.items.get-kit's bill run again after a :later stop or a gathering that ended, :kit.later-wait, :kit.later-over and :kit.retry): none counted yet, so :pooled; its seconds are the rest. Recount: node tools/event-counts.mjs retry-wins."}
   ::gather-counts {:default {"birch_log" {:s 34.417, :got 6, :calls 3}, "birch_sapling" {:s 342.572, :got 9, :calls 7}, "coal" {:s 104.443, :got 7, :calls 6}, "cobblestone" {:s 1408.92, :got 202, :calls 32}, "dirt" {:s 181.444, :got 69, :calls 4}, "log" {:s 5073.127, :got 0, :calls 58}, "oak_log" {:s 65.975, :got 10, :calls 6}, "oak_sapling" {:s 339.004, :got 12, :calls 9}, "raw_iron" {:s 5873.701, :got 28, :calls 21}, "white_wool" {:s 101.411, :got 3, :calls 5}}
                    :spec (a/pred "a map of item name to {:s seconds >= 0 :got items >= 0}"
                                  (fn [m] (and (map? m) (every? (fn [{:keys [s got]}] (and (number? s) (number? got) (<= 0 s) (<= 0 got))) (vals m)))))
                    :doc "Seconds the trial bodies' gathers of an item held the body and the items of it picked up meanwhile, fruitless gathers and their child jobs' seconds counted, a reflex's or an offline body's not (logs to 2026-10-29, probes left out, gathers logged at debug alone: 21 raw iron gathers got 28 in 5874 s, 32 cobblestone 202 in 1409 s, 6 coal 7 in 104 s). A name minecraft-data has not (log: any log) counts for nothing. measured-worth weighs them with the recipe tree. Recount: node tools/event-counts.mjs item-costs (:gather-counts)."}
   ::gather-prior-items {:default 1.516 :spec (a/num-in 0 nil)
                         :doc "Items a measured item's recipe-tree price counts as beside its gathers (measured-worth): the shape of the gathers' rates about their item's, fitted by moments over the items with two or more gathers and pooled. Recount: node tools/event-counts.mjs item-costs (:prior-items)."}
   ::food-worth-per-point {:default 30 :spec (a/num-in 0 nil)
                           :doc "Seconds reaching food is worth per food point the body is under ::food-worth-from."}
   ::food-worth-from {:default 10 :spec (a/num-in 0 20)
                      :doc "Food at or over which reaching food is worth nothing; it rises as the bar falls under it."}
   ::torch-place-s {:default 1.5 :spec (a/num-in 0 nil)
                    :doc "Placing a torch to light a dark floor and waiting for the light to show it, in seconds."}})

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
        base (get table name ((tbl/override-fn (get group-overrides "unknown")) (:unknown-each basis)))
        grouped (reduce (fn [w g] ((tbl/override-fn (get group-overrides g)) w))
                        base (if (and (contains? table name) (not (contains? named name))) (tbl/groups-for version name) []))
        max-d (some-> item .-maxDurability)
        share (if (and (number? durability) (number? max-d) (pos? max-d))
                (max (:min-durability-share basis) (min 1 (/ durability max-d)))
                1)
        each (cond (nil? price) (+ (* grouped share) (enchant-worth version basis enchants))
                   per-stack (/ price (max 1 n))
                   :else price)]
    {:name name :count n :each each :value (* n each)}))

(declare item-value)

(defn- tree-each
  "Seconds one `n` is worth by the recipe tree alone, its find seconds from `find` (over :find in default-basis)."
  [version n find]
  (or (:each (first (:items (item-value [{:name n :count 1}] :version version :basis {:find find})))) 0))

(defn- fit
  "Seconds per unit of find, fitted pooled to rows {:s :got :a :b} whose tree price is a + b x the unit: nil with none."
  [rows]
  (let [per (reduce + (map #(* (:got %) (:b %)) rows))]
    (when (pos? per) (max 0 (/ (reduce + (map #(- (:s %) (* (:got %) (:a %))) rows)) per)))))

(def ^:private measured
  (memoize
   (fn [version counts k]
     (let [md (minecraft-data version)
           terrain (get-in tbl/default-basis [:find :terrain])
           tree #(tree-each version %1 {:ore-scan-s %2 :terrain %3})
           rows (for [[n {:keys [s got]}] counts
                      :when (aget (.-itemsByName md) n)
                      :let [a (tree n 0 terrain) at (tree n 0 0)]]
                  {:n n :s s :got got :a a :b (- (tree n 1 terrain) a) :at at :bt (- (tree n 0 1) at)})
           ores (filter #(pos? (:b %)) rows)
           others (remove #(pos? (:b %)) rows)
           scan (fit ores)
           find (fit (map #(assoc % :a (:at %) :b (:bt %)) (filter #(< 0 (:bt %) 1.001) others)))
           worth (fn [{:keys [s got]} tree-s] (/ (+ s (* k tree-s)) (+ got k)))]
       {:basis (if scan {:find {:ore-scan-s scan}} {})
        :goal-basis (if find {:find {:terrain find :crop find}} {})
        :ores (into {} (map (fn [r] [(:n r) (worth r (+ (:a r) (* (:b r) (or scan 0))))])) ores)
        :goals (into {} (map (fn [r] [(:n r) (worth r (+ (:at r) (* (:bt r) (or find terrain))))])) others)}))))

(defn measured-worth
  "{:basis :goal-basis :ores :goals} of ::gather-counts for minecraft-data version. Each measured item is worth (s + k x
  tree) / (got + k), its gathers' seconds and items with the recipe tree's price at k = ::gather-prior-items items.
  :basis's :ore-scan-s is fitted to the measured ores (an item whose tree price grows with it) pooled, so every ore
  follows by density at the margin too (item-value); :ores are those ores' worths. :goal-basis's :terrain
  is the find fitted so to the measured terrain blocks one of which gives an item (cobblestone, dirt; not a sapling of
  leaves, dug many at one find), a goal's walk to the block, a ripe crop's (:crop) too; :goals, the
  other measured items' worths with it. Both are the goal's alone (goal-value): at the margin a block is its dig and
  its find (an ore's at the fitted scan), its fruitless runs not counted."
  [version]
  (measured version (settings/get settings ::gather-counts) (settings/get settings ::gather-prior-items)))

(defn measured-worths
  "{item seconds} of the measured items (measured-worth :ores and :goals), as item-value :overrides for a goal;
  cobblestone's for every stone tool material (jobs.lib.recipes/stone-materials), as one gather takes whichever is seen."
  [version]
  (let [{:keys [ores goals]} (measured-worth version)
        m (merge ores goals)]
    (if-let [cobble (m "cobblestone")]
      (merge (zipmap (recipes/stone-materials version) (repeat cobble)) m)
      m)))

(def ^:private group-names #{"ore" "tool" "armor" "food" "block" "unknown"})

(defn counted-chance
  "The chance of a measured share, k of n counted {:k :n}, under a Beta(a, b) prior of pseudo-counts :prior [a b] (a of the
  share, b of the rest; default the uniform [1 1]): (k + a) / (n + a + b)."
  [{:keys [k n prior]}]
  (let [[a b] (or prior [1 1])]
    (/ (+ k a) (+ n a b))))

(defn retry-chance
  "The chance the next try of kind wins after fails tries of the call failed in a row: counted-chance of no win in the
  fails after the first over ::retry-counts' pseudo-counts of kind. Those are counted over tries that followed a
  failed one, so the first failure is their condition, not evidence: the first retry wins at the prior's mean."
  [kind fails]
  (let [{:keys [win fail]} (get (settings/get settings ::retry-counts) kind)]
    (counted-chance {:k 0 :n (max 0 (dec fails)) :prior [win fail]})))

(defn retry-s
  "The measured seconds one try of kind takes (::retry-counts :try-s)."
  [kind]
  (:try-s (get (settings/get settings ::retry-counts) kind)))

(defn craft-step-s
  "Expected seconds one craft step takes: a finished one's tbl/craft-step, or with the measured timeout share, a timeout's
  seconds and the retries after it (the :craft ::retry-counts :try-s over the chance each wins, retry-chance :craft after the first failure)."
  []
  (let [{:keys [k n s]} (tbl/craft-timeouts)
        p (/ k n)]
    (+ (* (- 1 p) (tbl/craft-step)) (* p (+ s (/ (retry-s :craft) (retry-chance :craft 1)))))))

(defn- table-basis
  "[table basis] item-value prices by: the worth table of the name overrides and the whole basis."
  [version name-overrides basis]
  (let [basis (tbl/deep-merge {:craft-step (craft-step-s)} (tbl/deep-merge (when-not (get-in basis [:find :ore-scan-s]) (:basis (measured-worth version))) basis))]
    [(tbl/table-for version name-overrides basis) (tbl/full-basis basis)]))

(def ^:private smelts-of (memoize (fn [version] (tbl/smelt-sources (map #(.-name %) (array-seq (.-itemsArray (minecraft-data version))))))))

(def ^:private fuel-each (memoize tbl/fuel-each))

(defn smelt-price
  "Seconds one smelted item takes: ::smelt-step and the cheapest fuel's share of it (fuel-each). Options :version, :basis."
  [& {:keys [version basis]}]
  (let [[table basis] (table-basis (or version @game/version) {} basis)]
    (+ (:smelt-step basis) (fuel-each table))))

(defn- made
  "{:s :stations} of n of name made from nothing at the table's worths (m: :version :table :basis, :stations-s the
  seconds of a set of stations): an item cheaper got than made (its table worth below every way's) n x its worth, else
  the way (recipe or smelt) whose whole crafts, a craft step each, and stations (crafting table, furnace) cost least,
  a smelt costing its step and its fuel per item;
  Infinity for an item made of itself up the chain (an ingot of nuggets of ingots)."
  [{:keys [version table basis stations-s] :as m} name n chain]
  (let [unit (get table name js/Infinity)
        step (:craft-step basis)
        smelt-s (+ (:smelt-step basis) (fuel-each table))
        ways (filter #(js/isFinite (:per %))
                     (concat (for [r (get (recipes/recipes-for version) name)]
                               {:per (/ (+ step (reduce + (map (fn [[k v]] (* v (get table k js/Infinity))) (:needs r)))) (:count r)) :r r})
                             (for [i (get (smelts-of version) name)] {:per (+ smelt-s (get table i js/Infinity)) :smelt i})))
        within (conj chain name)
        made-way (fn [{:keys [r smelt]}]
                   (if smelt
                     (let [in (made m smelt n within)]
                       {:s (+ (* n smelt-s) (:s in)) :stations (conj (:stations in) "furnace")})
                     (let [ins (map (fn [[k v]] (made m k (* v (js/Math.ceil (/ n (:count r)))) within)) (:needs r))]
                       {:s (+ step (reduce + (map :s ins)))
                        :stations (cond-> (reduce set/union #{} (map :stations ins)) (:table? r) (conj recipes/table-item))})))]
    (cond
      (contains? chain name) {:s js/Infinity :stations #{}}
      (or (empty? ways) (< unit (* (apply min (map :per ways)) (- 1 1e-9)))) {:s (* n unit) :stations #{}}
      :else (apply min-key #(+ (:s %) (stations-s (:stations %))) (map made-way ways)))))

(def ^:private station-base
  "made of station st, its own stations priced at their table worths, so pricing one never needs itself."
  (memoize (fn [version table basis st] (made {:version version :table table :basis basis :stations-s #(reduce + 0 (map table %))} st 1 #{}))))

(defn- stations-closure
  "Stations st and the stations making them needs."
  [version table basis st]
  (let [more (reduce set/union st (map #(:stations (station-base version table basis %)) st))]
    (if (= more st) st (recur version table basis more))))

(def ^:private made-memo
  (memoize (fn [version table basis name n]
             (made {:version version :table table :basis basis
                    :stations-s (fn [st] (reduce + 0 (map #(:s (station-base version table basis %)) (stations-closure version table basis st))))}
                   name n #{}))))

(declare item-value)

(defn made-value
  "item-value of items as a goal from a reference start, nothing carried and no station near: each plain stack (no
  durability, enchantments or nbt) at least what making its count takes (made), and each crafting table or furnace
  those crafts and smelts need, and what those need, once, unless the items hold it. Options as item-value's but :prices."
  [items & {:keys [overrides version basis] :as opts}]
  (let [version (or version @game/version)
        [table basis] (table-basis version (apply dissoc (tbl/normal-overrides overrides) group-names) basis)
        named (group-by :name (filter #(string? (:name %)) items))
        plain (set (keep (fn [[n es]] (when (every? (fn [e] (not-any? #(some? (get e %)) [:durability :enchants :nbt])) es) n)) named))
        rows (map (fn [{:keys [name count value] :as row}]
                    (if (plain name)
                      (let [{:keys [s stations]} (made-memo version table basis name count) v (max value s)]
                        (assoc row :value v :each (/ v count) :stations stations))
                      row))
                  (:items (apply item-value items (mapcat identity (dissoc opts :prices)))))
        stations (stations-closure version table basis (reduce set/union #{} (keep :stations rows)))]
    {:value (+ (reduce + 0 (map :value rows)) (reduce + 0 (map #(:s (station-base version table basis %)) (remove (set (keys named)) stations))))
     :items (vec (sort-by :value > (map #(dissoc % :stations) rows)))}))

(defn item-value
  "{:value total :items [{:name :count :each :value} ...]} of items ([{:name :count ...}], inventory or drop shape),
  main contributors first. Stacks of one name with the same durability, enchantments and :nbt are added. See the ns doc.
  Options: :overrides, :prices, :else and :enchanted (see the ns doc), :basis (over default-basis in jobs.lib.cost.table), :version
  (minecraft-data, default the body's). Never throws on odd entries: no name is worth 0, an unknown name :unknown-each."
  [items & {:keys [overrides prices else enchanted version basis]}]
  (let [version (or version @game/version)
        overrides (tbl/normal-overrides overrides)
        group-overrides (select-keys overrides group-names)
        name-overrides (apply dissoc overrides group-names)
        [table basis] (table-basis version name-overrides basis)
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

(defn wear-saved
  "Seconds of wear on tool `dear` that digging n blocks with tool `spare` instead saves: dear's whole item-value over
  its maxDurability for each use, at most spare's maxDurability uses (n nil: all of them). 0 when either has none."
  [dear spare n & {:keys [version]}]
  (let [md (minecraft-data (or version @game/version))
        uses #(some-> (aget (.-itemsByName md) %) .-maxDurability)
        dear-uses (uses dear)
        spare-uses (uses spare)]
    (if (and (pos? dear-uses) (pos? spare-uses))
      (* (/ (:value (item-value [{:name dear :count 1}] :version version)) dear-uses) (min spare-uses (or n spare-uses)))
      0)))

;; ---------------------------------------------------------------- the cost

(defn reserve-cost
  "What using one unit worth value costs against a reservation of strength s (0..1): value x s/(1-s), so 0.5 counts the
  item twice and 0.9 ten times; strength 1 is ##Inf (never)."
  [value s]
  (if (>= s 1) ##Inf (* value (/ s (- 1 s)))))

(defn chance-times
  "Chance p of seconds s, the expected seconds: 0 when p is 0, though s be ##Inf (a worth of arrival), where p x s is NaN."
  [p s]
  (if (zero? p) 0 (* p s)))

(defn affords?
  "Whether a price of `s` seconds is within `worth`: never a certain failure (##Inf), at any worth, ##Inf (arrive) too."
  [s worth]
  (and (js/isFinite s) (<= s worth)))

(defn trip [] (settings/get settings ::trip))
(defn per-block [] (settings/get settings ::per-block))
(defn per-dark [] (settings/get settings ::per-dark))
(defn cells-of
  "The cells of a place: a cell [x y z], a collection of cells, or a box {:min [x y z] :max [x y z]} (inclusive)."
  [place]
  (cond
    (map? place) (let [[x0 y0 z0] (:min place) [x1 y1 z1] (:max place)]
                   (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [x y z]))
    (number? (first place)) [place]
    :else place))

(defn spawn-share
  "The share of place's cells (a cell, a set of cells or a {:min :max} box) a mob spawns at. :light (cell -> {:block :sky} as
  the body saw it, the sky light already less the sky's darkening and 0 where it no longer lights; {:dark} the chance an
  unseen cell is dark, jobs.lib.look's guess; nil or missing: unseen, dark). A cell spawns only at block light 0 and sky
  light 0."
  [place {:keys [light]}]
  (let [cells (cells-of place)
        spawns (fn [cell] (let [{:keys [block sky dark] :or {block 0 sky 0}} (when light (light cell))]
                            (or dark (if (and (zero? block) (zero? sky)) 1 0))))]
    (/ (transduce (map spawns) + 0 cells) (max 1 (count cells)))))

(defn spawn-chance
  "The chance per second a mob spawns at place and reaches a body standing there: ::spawn-chance-per-s x spawn-share."
  ([place] (spawn-chance place nil))
  ([place opts] (* (settings/get settings ::spawn-chance-per-s) (spawn-share place opts))))

(defn dark-factor
  "The floor of the planner's extra cost of a dark cell as a share of its own seconds: per-dark over per-block
  (jobs.lib.cost/dark-factor adds a kit's spawn hurt)."
  [] (/ (per-dark) (per-block)))
(defn per-danger "One expected point of damage (jobs.lib.cost/route-danger): what an hp costs (jobs.lib.cost.health)." [] (health/walk-hp-seconds))
(defn own-place-blocks [] (settings/get settings ::own-place-blocks))
(defn walk-blocks-per-s
  "Blocks of straight line a second a walk covers: fight/walk-speed over ::detour-factor."
  []
  (/ fight/walk-speed (settings/get settings ::detour-factor)))
(def lethal-causes ["lava" "fire" "burn" "void" "out_of_world"])

(defn lethal-cause? [cause]
  (let [c (str/lower-case (if (keyword? cause) (name cause) (str cause)))]
    (boolean (some #(str/includes? c %) lethal-causes))))

(defn walk-cost
  "{:cost :parts {:walk :danger}} of walking `distance` blocks at the per-block price every trip is costed with, and
  `danger-s` seconds of danger on the way (jobs.lib.cost/risk-s)."
  [{:keys [distance danger-s]}]
  (let [parts {:walk (* (per-block) distance) :danger (or danger-s 0)}]
    {:cost (reduce + (vals parts)) :parts parts}))

(defn survived-worth
  "Seconds a find worth `worth` is worth to a body whose trip to it ends in death with `chance`: a dead body does not bring it
  back. The trip's death cost (carried kit, walk back) is priced beside it, in the walk's danger."
  [worth chance]
  (chance-times (- 1 (min 1 (max 0 (or chance 0)))) worth))

(defn walk-beats-making
  "Whether walking `distance` blocks to something that exists costs no more than making one's own (cost `make`, in seconds)
  and walking the own-place-blocks setting to put it down."
  [{:keys [distance make]}]
  (<= (:cost (walk-cost {:distance distance}))
      (+ make (:cost (walk-cost {:distance (settings/get settings ::own-place-blocks)})))))

(defn tour-cost
  "{:cost :parts {:walk :walks}} of a tour of frontiers (walks to the edge of loaded land): the blocks `explored` since
  the body last came nearer its goal at the walk price, and per-frontier-walk for each of the `walks` since then."
  [{:keys [explored walks]}]
  (let [parts {:walk (:cost (walk-cost {:distance explored})) :walks (* (settings/get settings ::per-frontier-walk) walks)}]
    {:cost (reduce + (vals parts)) :parts parts}))

(defn cover-tour-s
  "Seconds (walk-cost) of a greedy walk from point `from` ([x y z]) that brings every one of `points` within `reach`
  blocks: each leg walks to reach of the nearest point still beyond it; the points within reach of a stop are done."
  [from points reach]
  (let [d (fn [a b] (apply js/Math.hypot (map - a b)))]
    (loop [at from left points s 0]
      (let [left (remove #(<= (d at %) reach) left)]
        (if (empty? left)
          s
          (let [p (apply min-key #(d at %) left)
                gap (d at p)]
            (recur (mapv #(+ %2 (* (- %1 %2) (/ reach gap))) at p) (remove #{p} left) (+ s (:cost (walk-cost {:distance (- gap reach)}))))))))))

(defn tour-worth
  "Seconds of tour a goal `straight` blocks off is worth to a caller counting arrival `worth` seconds (##Inf: arrive):
  that less the straight walk, 0 at least. A goal go-to is given is known: a search nets its own trip in its worth."
  [straight worth]
  (max 0 (- worth (:cost (walk-cost {:distance straight})))))

(defn turn-back-cost
  "{:cost :parts {:look :back}} of a way over unseen `floors` [{:look :back :p :extra-s :fall-s}] in walk order, each not there at
  chance p: the way reaches a floor while those before it held, pays its look (seconds), and when it is not there walks
  back to the way's start (:back blocks) and takes the next-best way, :extra-s seconds dearer; a step down that falls
  further first climbs out of where it lands, :fall-s seconds."
  [floors]
  (let [parts (:parts (reduce (fn [{:keys [reach] :as acc} {:keys [look back p extra-s fall-s]}]
                                (-> acc
                                    (update-in [:parts :look] + (* reach look))
                                    (update-in [:parts :back] + (chance-times (* reach p) (+ (:cost (walk-cost {:distance back})) (or extra-s 0) (or fall-s 0))))
                                    (assoc :reach (* reach (- 1 p)))))
                              {:reach 1 :parts {:look 0 :back 0}}
                              floors))]
    {:cost (+ (:look parts) (:back parts)) :parts parts}))

(defn food-point-worth
  "Seconds one hunger point of food is worth while the pack lacks the food reserve."
  []
  (settings/get settings ::food-worth-per-point))

(defn hunger-s
  "Seconds the hunger that losing damage hp from health brings a body with food costs: each food point its hungry line
  (jobs.lib.foods/hungry-line) rises over the food it is short already, at food-point-worth."
  [food health damage]
  (let [short #(max 0 (- (foods/hungry-line % {}) food))]
    (* (food-point-worth) (- (short (- health damage)) (short health)))))

(defn food-search-worth
  "Seconds a search for food is worth while the pack lacks `short` hunger points: food-point-worth each, less one trip."
  [short]
  (max 0 (- (* (food-point-worth) short) (trip))))

(defn food-worth
  "Seconds reaching food is worth to a body with `food` points and `health`: rising as the bar falls, 0 from
  ::food-worth-from up; under foods/top-up-food, where only food lets it heal, also the hp it lacks, each at what it is
  worth over healing it by waiting (cost.health/gain-worth without and with waiting)."
  [food health]
  (let [missing (if (< food foods/top-up-food) (js/Math.floor (- 20 health)) 0)]
    (+ (* (settings/get settings ::food-worth-per-point) (max 0 (- (settings/get settings ::food-worth-from) food)))
       (- (health/gain-worth health missing false) (health/gain-worth health missing true)))))

(def meal-points
  "{item hunger points} per item of the items a crafting recipe of that item alone makes a food of (3 wheat a bread: 5/3 a wheat), memoised per version."
  (memoize
   (fn [version]
     (let [food (foods/table-for version)]
       (reduce (fn [m [out n ins]]
                 (let [pts (get-in food [out :points])]
                   (if (and pts (apply = ins)) (update m (first ins) (fnil max 0) (/ (* pts n) (count ins))) m)))
               {} (tbl/recipe-list (minecraft-data version)))))))

(defn food-points
  "Hunger points one item of name gives at best: eaten (none when harmful), cooked or made into a food alone (meal-points); 0 when none."
  [name]
  (let [version @game/version
        own #(if (contains? foods/harmful %) 0 (get-in (foods/table-for version) [% :points] 0))]
    (max (own name) (if-let [c (foods/cooked-of name)] (own c) 0) (get (meal-points version) name 0))))

(def crop-points-for
  (memoize (fn [version crop] (reduce + 0 (map (fn [{:keys [name count]}] (* count (food-points name))) (tbl/crop-yields crop :version version))))))

(defn crop-points
  "Hunger points a ripe crop of block name gives when cut: food-points of what it drops (table/crop-yields)."
  [crop]
  (crop-points-for @game/version crop))

(defn crop-worth
  "Seconds a ripe crop of block name is worth when cut: its crop-points at food-point-worth."
  [crop]
  (* (food-point-worth) (crop-points crop)))

(def expected-drops "Meat items a kill is counted to give when the game data lists none." 2)

(defn animal-worth
  "Seconds an animal of kind `mob` is worth: the kill-drops of its raw meat at their measured gathers (the animal's own name for a kind with none)."
  [mob]
  (let [meat (get (set/map-invert game/meat-animal) mob mob)]
    (:value (item-value [{:name meat :count (or (tbl/kill-drops mob meat) expected-drops)}]
                        :overrides (measured-worths @game/version)))))

(defn per-climb [] (settings/get settings ::per-climb))

(defn detour-cost
  "{:cost :parts {:walk :climb}} of stepping off the way to pick something up `distance` blocks off and `climb` blocks
  above or below the feet: per-block a block (there and back) plus per-climb for each block of climb past the first.
  opts :per-block and :per-climb override the settings."
  ([spot] (detour-cost spot {}))
  ([{:keys [distance climb]} opts]
   (let [block (or (:per-block opts) (per-block))
         up (or (:per-climb opts) (per-climb))
         parts {:walk (* block distance) :climb (* up (max 0 (dec (abs (or climb 0)))))}]
     {:cost (reduce + (vals parts)) :parts parts})))

(defn torch-place-s [] (settings/get settings ::torch-place-s))

(defn light-cost
  "{:cost :parts {:torch :place :trip}} of one torch go-to places to light a dark floor (the planner's lightCost,
  seconds): the torch's worth, torch-place-s, and a trip when none is carried (it is fetched first)."
  [{:keys [carried]}]
  (let [parts (cond-> {:torch (:value (item-value [{:name "torch" :count 1}])) :place (torch-place-s)}
                (not (pos? carried)) (assoc :trip (trip)))]
    {:cost (reduce + (vals parts)) :parts parts}))

(defn fetch-cost
  "{:cost :parts {:trip :walk :danger :collect}} of fetching a pile `distance` blocks off, with `danger-s` seconds of danger
  on the way, `elapsed-ms` after it dropped; `back?` walks the distance twice (the despawn check is for the way there and the stay), `stay-s`
  seconds are spent standing at the pile.
  Or {:cost js/Infinity :reason}: :no-position (distance nil), :window-closed (the 5 min despawn passed), :too-far
  (the walk would end after the despawn), :lethal-cause (lava, fire or the void took the items)."
  [{:keys [distance danger-s elapsed-ms cause back? stay-s]}]
  (let [elapsed (or elapsed-ms 0)
        despawn-ms (game/despawn-ms)
        walk-ms (when (number? distance) (* 1000 (/ distance (walk-blocks-per-s))))
        reason (cond
                 (nil? walk-ms) :no-position
                 (>= elapsed despawn-ms) :window-closed
                 (and (some? cause) (lethal-cause? cause)) :lethal-cause
                 (>= (+ elapsed walk-ms (* 1000 (or stay-s 0))) despawn-ms) :too-far)]
    (if reason
      {:cost js/Infinity :reason reason}
      (let [w (walk-cost {:distance (* distance (if back? 2 1)) :danger-s danger-s})
            stay (or stay-s 0)]
        {:cost (+ (trip) stay (:cost w)) :parts (cond-> (assoc (:parts w) :trip (trip)) (pos? stay) (assoc :collect stay))}))))
