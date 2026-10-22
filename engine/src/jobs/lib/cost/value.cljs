(ns jobs.lib.cost.value
  "What a list of items is worth (item-value) and what fetching them costs (fetch-cost), in one unit: about a second
  of a player's work. recover-drops compares the two; any job may ask what a pile, a chest or a kit is worth.

  The worth of one item is the cheapest way to obtain it, from minecraft-data for the body's version. Every constant
  and the few small tables (ore rarity, natural-and-craftable blocks, passive-mob health, rare drop chances) live in
  default-basis (jobs.lib.cost.table), overridable by :basis (deep-merged). The tables are defaults, not data minecraft-data has.

    block        A block's drops (blockLoot, no silk touch, chance at least :min-drop-chance): (dig + find) / drops.
                 dig = hardness x :hardness-s / the speed of the stone tool (the block's own tier if higher; the base assumes a
                 tool, never the hand);
                 find = :terrain, :wood for a log, stem or bamboo block, or :ores {ore [find drops]} (a nil find: :ore-scan-s over the base-rates density of the ore's best layer), or :find-s for a block. Crafted blocks are no source
                 except :natural ones (granite, clay ...); a block that is no item (redstone_wire) is none.
    crop         A block with an age state: :farm-s / the age-7 yield.
    mob          entityLoot of the mobs in weapon/mob-max-health and passive-max-health: (kill + hurt + find) / expected
                 drops; kill = the stone sword's hits, hurt = :hurt-hp-per-hp x health x :danger hp, each at health/hp-seconds, find by hostile or passive.
    made         The cheapest recipe: the ingredients plus :craft-step, divided by the result count; or smelting by
                 name rule (raw_X -> X_ingot, X -> cooked_X, ancient_debris -> netherite_scrap, cobblestone -> stone,
                 sand -> glass, clay_ball -> brick): the input plus :smelt-step. The table is a fixpoint, then each tool
                 is raised to at least the next weaker material's tool of its kind plus :craft-step.
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
  (:require [engine.args :as a]
            ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [jobs.lib.cost.table :as tbl]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.health :as health]
            [engine.game :as game]
            [engine.settings :as settings]))

(a/defargs settings
  {::trip {:default 10 :spec (a/num-in 0 nil)
           :doc "Any fetch: turning round, finding the pile, the risk of the place one died at, in seconds."}
   ::per-block {:default 0.3 :spec (a/num-in 0.01 nil)
                :doc "One block walked, there and back about a second of a player's time per 3 blocks."}
   ::per-climb {:default 2 :spec (a/num-in 0 nil)
                :doc "One block climbed past the first on a detour (stairs or a pillar back out of a drop, or up to a ledge), in seconds."}
   ::per-dark {:default 0 :spec (a/num-in 0 nil)
               :doc "Floor of one block walked in the dark, on top of per-block; the hp a zombie would cost the kit (dark-factor) comes on top, so a bare body pays a dark block twice a lit one and an armed one less."}
   ::spawn-chance-per-s {:default 0.005 :spec (a/num-in 0 nil)
                         :doc "Chance a mob spawns and reaches the body each second it stands on a cell where one can spawn (jobs.lib.cost/spawn-chance)."}
   ::dark-hurt-cap {:default 20 :spec (a/num-in 0 nil)
                    :doc "Most hp a zombie is counted to cost the kit in the dark price: a body that loses more is no worse off priced."}
   ::walk-blocks-per-s {:default 2.9 :spec (a/num-in 0.1 nil)
                        :doc "Walking speed with slack for detours (4.3 flat out, 1.5x the way), blocks a second."}
   ::per-frontier-walk {:default 1 :spec (a/num-in 0 nil)
                        :doc "Each walk of a tour to the edge of loaded land that brought the body no nearer its goal, on top of its blocks, in seconds."}
   ::tour-ratio {:default 3 :spec (a/num-in 0 nil)
                 :doc "Times its straight walk a goal whose worth the caller did not say is worth a tour of loaded land on top of a trip: a near goal is worth a short tour."}
   ::tour-floor-blocks {:default 32 :spec (a/num-in 0 nil)
                        :doc "Straight blocks a goal counts as at least when worth a tour (about 100 blocks of tour): a near goal behind a ravine is still reached."}
   ::tour-worth {:default false :spec (a/or-of false? (a/num-in 0 nil))
                 :doc "Seconds a tour of frontiers may cost whatever its goal; false: from the goal's straight blocks (tour-ratio, tour-floor-blocks)."}
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

(defn item-value
  "{:value total :items [{:name :count :each :value} ...]} of items ([{:name :count ...}], inventory or drop shape),
  main contributors first. Stacks of one name with the same durability, enchantments and :nbt are added. See the ns doc.
  Options: :overrides, :prices, :else and :enchanted (see the ns doc), :basis (over default-basis in jobs.lib.cost.table), :version
  (minecraft-data, default the body's). Never throws on odd entries: no name is worth 0, an unknown name :unknown-each."
  [items & {:keys [overrides prices else enchanted version basis]}]
  (let [version (or version @game/version)
        overrides (tbl/normal-overrides overrides)
        group-names #{"ore" "tool" "armor" "food" "block" "unknown"}
        group-overrides (select-keys overrides group-names)
        name-overrides (apply dissoc overrides group-names)
        table (tbl/table-for version name-overrides basis)
        basis (tbl/deep-merge tbl/default-basis basis)
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

(defn reserve-cost
  "What using one unit worth value costs against a reservation of strength s (0..1): value x s/(1-s), so 0.5 counts the
  item twice and 0.9 ten times; strength 1 is ##Inf (never)."
  [value s]
  (if (>= s 1) ##Inf (* value (/ s (- 1 s)))))

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

(defn spawn-chance
  "The chance per second a mob spawns at place (a cell, a set of cells or a {:min :max} box: the mean over its cells).
  :light (cell -> {:block :sky} as the body saw it, the sky light already less the sky's darkening and 0 where it no
  longer lights; nil or missing: unseen, dark). A cell spawns only at block light 0 and sky light 0."
  ([place] (spawn-chance place nil))
  ([place {:keys [light]}]
   (let [cells (cells-of place)
         spawns? (fn [cell] (let [{:keys [block sky] :or {block 0 sky 0}} (when light (light cell))]
                              (and (zero? block) (zero? sky))))]
     (* (settings/get settings ::spawn-chance-per-s)
        (/ (count (filter spawns? cells)) (max 1 (count cells)))))))

(defn dark-factor
  "The planner's extra cost of a dark cell as a share of its own seconds (options.dark.factor): per-dark over per-block
  (the floor). With a kit {:weapon :equipment} the hp a zombie would cost it (fight/fight-damage, at most
  ::dark-hurt-cap) times spawn-chance of a dark cell and hp-seconds is added: a bare body pays more than an armed,
  armoured one."
  ([] (/ (per-dark) (per-block)))
  ([kit]
   (let [hurt (min (settings/get settings ::dark-hurt-cap) (fight/fight-damage (assoc kit :mobs [{:name "zombie" :distance 0 :hits 0}])))]
     (+ (dark-factor) (* hurt (spawn-chance [0 0 0]) health/hp-seconds)))))
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

(defn tour-cost
  "{:cost :parts {:walk :walks}} of a tour of frontiers (walks to the edge of loaded land): the blocks `explored` since
  the body last came nearer its goal at the walk price, and per-frontier-walk for each of the `walks` since then."
  [{:keys [explored walks]}]
  (let [parts {:walk (:cost (walk-cost {:distance explored})) :walks (* (settings/get settings ::per-frontier-walk) walks)}]
    {:cost (reduce + (vals parts)) :parts parts}))

(defn tour-worth
  "Seconds of tour a goal `straight` blocks off is worth. The setting ::tour-worth, when set, is the worth. Else with the
  caller's `worth` of arrival (seconds), that less the trip and the straight walk (0 at least). Else a trip, and tour-ratio
  times the walk of its straight blocks, at least tour-floor-blocks."
  ([straight] (tour-worth straight nil))
  ([straight worth]
   (cond
     (settings/get settings ::tour-worth) (settings/get settings ::tour-worth)
     worth (max 0 (- worth (trip) (:cost (walk-cost {:distance straight}))))
     :else (+ (trip) (* (settings/get settings ::tour-ratio) (:cost (walk-cost {:distance (max straight (settings/get settings ::tour-floor-blocks))})))))))

(defn food-worth
  "Seconds reaching food is worth to a body with `food` points: rising as the bar falls, 0 from ::food-worth-from up."
  [food]
  (* (settings/get settings ::food-worth-per-point) (max 0 (- (settings/get settings ::food-worth-from) food))))

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
