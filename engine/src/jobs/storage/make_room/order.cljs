(ns jobs.storage.make-room.order
  "The pure side of jobs.storage.make-room: what is kept, what goes to a chest, what is thrown, and in which order."
  (:require [clojure.string :as str]
            [engine.memory :as mem]
            [jobs.lib.choose :as choose]
            [jobs.lib.cost :as cost]
            [jobs.lib.foods :as foods]
            [jobs.lib.storage :as storage]))

(defn item-worth
  "The worth of one item of an inventory entry ({:name :count? ...}): its jobs.lib.cost/item-value per item."
  [item]
  (/ (:value (cost/item-value [item])) (max 1 (or (:count item) 1))))

(def tool-like #{"bucket" "water_bucket" "lava_bucket"})

(def junk-blocks
  "Plain blocks a player throws first when the bag is full."
  #{"dirt" "coarse_dirt" "cobblestone" "cobbled_deepslate" "deepslate" "stone" "granite" "diorite" "andesite"
    "tuff" "gravel" "netherrack" "sand" "red_sand" "calcite" "dripstone_block"})

(defn stack-worth
  "item-worth, but 0 for a plain junk block: crafted-only ones (cobbled_deepslate) have no source in the item base and
  would price above the toss line."
  [item]
  (if (contains? junk-blocks (:name item)) 0 (item-worth item)))

(defn protected?
  "Never put away, never thrown: tools, weapons, armour, the buckets, torches and golden apples."
  [name]
  (boolean (or (storage/tool? name) (tool-like name) (= "torch" name) (contains? foods/precious name))))

;; what shelter, mend, furnace and tool jobs spend
(def shelter-blocks
  (into #{"dirt" "cobblestone"} (filter #(str/ends-with? % "_planks")) (choose/fillers :build)))

(defn shared-floors
  "Floors for names, in priority order, that share one budget: each keeps
  what it carries, up to what is left of the budget."
  [names totals budget]
  (first (reduce (fn [[floors left] n]
                   (let [k (min (get totals n 0) left)]
                     [(assoc floors n k) (- left k)]))
                 [{} budget]
                 names)))

(defn keep-counts
  "{name n}: how many of each carried name stay carried. Protected names keep
  their whole total; food names share carry-food, best food-points first;
  shelter-blocks keep up to carry-blocks each, seeds up to carry-seeds each, carry-items whole, held ({name n}, the reserve
  register) at least n; the rest 0."
  [inventory {:keys [carry-food carry-blocks carry-seeds carry-items]} held]
  (let [totals (storage/carried-totals inventory)
        names (keys totals)
        food (->> names
                  (filter #(and (not (protected? %)) (foods/edible? %)))
                  (sort-by (juxt #(- (foods/points %)) identity)))
        food-floors (if carry-food
                      (shared-floors food totals carry-food)
                      (select-keys (cost/food-reserve inventory) food))
        capped (fn [pred n] (into {} (for [[k v] totals :when (pred k)] [k (min v (or n 0))])))]
    (merge-with max
                (merge (zipmap names (repeat 0))
                       (select-keys totals (filter protected? names))
                       food-floors
                       (capped shelter-blocks carry-blocks)
                       (capped #(str/ends-with? % "_seeds") carry-seeds)
                       (select-keys totals carry-items))
                (into {} (for [[k n] held :when (totals k)] [k (min n (totals k))])))))

(defn recency
  "{item-name t}: the time of the newest :picked-up entry per item."
  [view]
  (reduce (fn [m {:keys [t data]}] (update m (:item data) (fnil max 0) t))
          {}
          (mem/entries view :picked-up)))

(defn toss-order
  "The stacks that may be thrown, first to throw first: [{:name :count :slot
  :worth :junk}]: plain junk blocks (junk-blocks) first, the biggest stack
  first (whole stacks before partial), then by worth, then when the name was
  last picked up (never is 0, oldest), then count, then slot. A stack qualifies when its name is not
  protected or food, its worth is below max-worth, and its whole count fits in
  what the name carries above its keep, after the stacks of the name that come
  before it in the order: a toss frees a whole slot and never cuts into a floor."
  [inventory keep recency max-worth]
  (let [totals (storage/carried-totals inventory)
        budgets (into {} (map (fn [[n total]] [n (- total (get keep n 0))])) totals)
        candidates (->> inventory
                        (map #(assoc (select-keys % [:name :count :slot]) :worth (stack-worth %) :junk (contains? junk-blocks (:name %))))
                        (remove #(or (protected? (:name %)) (foods/edible? (:name %))))
                        (filter #(< (:worth %) max-worth))
                        (sort-by (fn [s] (let [kept (if (pos? (get keep (:name s) 0)) 1 0)]
                                           (if (:junk s)
                                             [kept 0 0 0 0 (- (:count s)) (:slot s)]
                                             [kept 1 (:worth s) 0 (get recency (:name s) 0) (:count s) (:slot s)])))))]
    (first (reduce (fn [[chosen left] s]
                     (if (<= (:count s) (left (:name s)))
                       [(conj chosen s) (update left (:name s) - (:count s))]
                       [chosen left]))
                   [[] budgets]
                   candidates))))

(defn deposit-names
  "Distinct carried names, not protected, whose total exceeds their keep, in
  the order to put them away: least worth keeping first. Names without a floor
  (keep 0) before names with one (food, building blocks), then the lowest
  item-worth among the name's stacks, then when the name was last
  picked up (recency {name t}; never is 0, oldest first), then its lowest slot
  (the stack's index when it has no :slot)."
  [inventory keep recency]
  (let [totals (storage/carried-totals inventory)
        stacks (map-indexed (fn [i s] (assoc s :slot (or (:slot s) i))) inventory)
        order-key (fn [[n ss]]
                    [(if (pos? (get keep n 0)) 1 0)
                     (apply min (map stack-worth ss))
                     (get recency n 0)
                     (apply min (map :slot ss))])]
    (->> stacks
         (remove #(protected? (:name %)))
         (filter #(> (totals (:name %)) (get keep (:name %) 0)))
         (group-by :name)
         (sort-by order-key)
         (mapv first))))
