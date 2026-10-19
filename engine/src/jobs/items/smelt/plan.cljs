(ns jobs.items.smelt.plan
  "What a furnace cooks and burns, and what to load into it: pure functions over a furnace read and the carried items."
  (:require [jobs.lib.fuel :as fuel]
            [jobs.lib.game-facts :as facts]
            [jobs.lib.storage :as storage]))

(def cook-ticks {"furnace" 200 "blast_furnace" 100 "smoker" 100})

(def foods #{"beef" "porkchop" "chicken" "mutton" "rabbit" "cod" "salmon" "potato" "kelp"})
(def furnace-only #{"cobblestone" "cobbled_deepslate" "stone" "sand" "red_sand" "clay_ball" "clay" "netherrack"
                    "sandstone" "red_sandstone" "stone_bricks" "quartz_block" "sea_pickle" "cactus" "chorus_fruit"})

(defn ore? [name]
  (boolean (or (re-find #"_ore$" name) (re-find #"^raw_(iron|gold|copper)$" name) (= "ancient_debris" name))))

(defn smelts?
  "Whether a kind of furnace (the block name) cooks the item."
  [kind name]
  (boolean
   (case kind
     "smoker" (contains? foods name)
     "blast_furnace" (ore? name)
     "furnace" (or (ore? name) (contains? foods name) (contains? furnace-only name)
                   (boolean (re-find #"_(log|wood)$" name)))
     false)))

(def made-from
  "{output input} of what the furnaces cook, for naming the input of a product. The vanilla recipes are in no
  minecraft-data file."
  {"cooked_beef" "beef" "cooked_porkchop" "porkchop" "cooked_chicken" "chicken" "cooked_mutton" "mutton"
   "cooked_rabbit" "rabbit" "cooked_cod" "cod" "cooked_salmon" "salmon" "baked_potato" "potato" "dried_kelp" "kelp"
   "iron_ingot" "raw_iron" "gold_ingot" "raw_gold" "copper_ingot" "raw_copper" "netherite_scrap" "ancient_debris"
   "glass" "sand" "smooth_stone" "stone" "brick" "clay_ball" "terracotta" "clay"
   "nether_brick" "netherrack" "smooth_sandstone" "sandstone" "smooth_red_sandstone" "red_sandstone"
   "cracked_stone_bricks" "stone_bricks" "cracked_deepslate_bricks" "deepslate_bricks"
   "cracked_deepslate_tiles" "deepslate_tiles" "cracked_nether_bricks" "nether_bricks" "smooth_quartz" "quartz_block" "lime_dye" "sea_pickle" "green_dye" "cactus"
   "popped_chorus_fruit" "chorus_fruit"})

(declare smeltable?)

(defn input-of
  "The item to load for :item: itself when a furnace cooks it, else the input it is made from (iron_ingot: raw_iron).
  Charcoal comes from any log: the carried one there is most of, else oak_log."
  ([item] (input-of item nil))
  ([item carried]
   (cond
     (nil? item) item
     (= "charcoal" item) (or (->> carried (filter #(re-find #"_(log|wood)$" (:name %))) (sort-by :count >) first :name)
                             "oak_log")
     (smeltable? item) item
     :else (get made-from item item))))

(defn smeltable?
  "Whether some kind of furnace cooks the item."
  [item]
  (boolean (some #(smelts? % item) (keys cook-ticks))))

(defn fuel-rank
  "Lower burns first: coal and charcoal, then wood, then sticks, then the rest."
  [name]
  (cond (#{"coal" "charcoal"} name) 0
        (fuel/wood-fuel? name) 1
        (= "stick" name) 2
        :else 3))


(defn existing-capacity
  "How many items the fuel already in the furnace will cook: the burning fuel's bar and the fuel slot."
  [kind state]
  (let [slot (:fuel state)]
    (+ (js/Math.floor (/ (get-in state [:burn :left] 0) (cook-ticks kind)))
       (if slot (js/Math.floor (* (or (fuel/fuel-per-unit kind (:name slot)) 0) (:count slot))) 0))))

(defn spare
  "How many of name the carried list has beyond what m's :held ({name n}, the register's) keeps back."
  [{:keys [carried held]} name]
  (max 0 (- (storage/carried carried name) (get held name 0))))

(defn pick-fuel
  "The fuel item to load: the given one, else the one already in the slot when carried, else the best carried.
  Never the item being smelted. {:give-up reason} when there is none."
  [{:keys [kind carried item fuel state] :as m}]
  (let [usable (fn [name] (and (not= name item) (fuel/fuel-per-unit kind name) (pos? (spare m name))))
        slot-name (:name (:fuel state))]
    (cond
      fuel (cond (nil? (fuel/fuel-per-unit kind fuel)) {:give-up "unknown-fuel"}
                 (usable fuel) {:fuel fuel}
                 :else {:give-up "no-fuel"})
      (and slot-name (usable slot-name)) {:fuel slot-name}
      :else (if-let [best (->> carried (map :name) distinct (filter usable) (sort-by fuel-rank) first)]
              {:fuel best}
              {:give-up "no-fuel"}))))

(defn fuel-plan
  "Fuel for n more items on top of e already in the slot. {:count n :fuel {:item :count}|nil}, with n cut to what
  the fuel covers (n 0 is a load already made that only needs its fire fed), or {:give-up reason}."
  [{:keys [kind state carried] :as m} n e]
  (let [existing (existing-capacity kind state)
        deficit (- (+ e n) existing)]
    (if (<= deficit 0)
      {:count n :fuel nil}
      (let [picked (pick-fuel m)
            name (:fuel picked)
            slot (:fuel state)]
        (cond
          (and (:give-up picked) (> existing e)) {:count (min n (- existing e)) :fuel nil}
          (:give-up picked) picked
          (and slot (not= (:name slot) name)) {:give-up "fuel-busy"}
          :else (let [per (fuel/fuel-per-unit kind name)
                      units (min (js/Math.ceil (/ deficit per)) (spare m name) (- facts/slot-max (:count slot 0)))
                      covered (- (js/Math.floor (+ existing (* units per))) e)
                      n' (max 0 (min n covered))]
                  (if (and (pos? n) (zero? n'))
                    {:give-up "no-fuel"}
                    {:count n' :fuel {:item name :count (min units (js/Math.ceil (/ (- (+ e n') existing) per)))}})))))))

(defn plan-load
  "What to put in the furnace: {:item :count :fuel {:item :count}|nil}, or {:give-up reason}.
  m: :kind and :state (a read of the furnace), :carried ({:name :count} each), :held ({name n} the fuel may not use), :item :count :fuel from the args,
  :target (the input the furnace must hold, set once a load was planned, so a resumed load adds only the rest)."
  [{:keys [state carried item count target] :as m}]
  (let [kind (:kind state)
        item (or item (some #(when (smelts? kind (:name %)) (:name %)) carried))
        have (storage/carried carried item)
        input (:input state)
        e (if (= item (:name input)) (:count input) 0)
        room (- facts/slot-max e)
        wanted (cond target (max 0 (- target e)) :else (min (or count have) have room))
        n (if target (min wanted have) wanted)]
    (cond
      (nil? item) {:give-up "nothing-smeltable"}
      (and (not target) (zero? have)) {:give-up "no-item"}
      (not (smelts? kind item)) {:give-up "not-smeltable"}
      (and input (not= item (:name input))) {:give-up "furnace-busy"}
      (and (not target) (<= room 0)) {:give-up "furnace-full"}
      :else (let [r (fuel-plan (assoc m :item item :kind kind) (if target (max n 0) n) e)]
              (if (:give-up r) r (assoc r :item item))))))

(defn judge
  "What a visit found: :take (output to collect), :wait (input cooking), :not-smeltable (input, fuel, no fire),
  :out-of-fuel (input, nothing to burn) or :done."
  [state]
  (cond
    (:output state) :take
    (and (:input state) (:lit state)) :wait
    (and (:input state) (:fuel state)) :not-smeltable
    (:input state) :out-of-fuel
    :else :done))

(def products
  "{input output} of made-from, for what a load yields."
  (into {} (map (fn [[out in]] [in out])) made-from))

(defn product-of
  "What the furnace makes of item (a log makes charcoal), or nil when it is not known."
  [item]
  (if (re-find #"_(log|wood)$" item) "charcoal" (get products item)))

(defn load-figures
  "What a load from the pockets comes to, for a quote: plan-load into an empty furnace, plus :product; or {:give-up r}.
  m: :carried, :item :count :fuel as the args give them."
  [{:keys [carried item] :as m}]
  (let [state {:kind "furnace" :input nil :fuel nil :output nil :burn {:left 0 :total 0}}
        input (input-of item carried)
        r (plan-load (assoc m :state state :item input))]
    (cond-> r (not (:give-up r)) (assoc :product (product-of (:item r))))))
