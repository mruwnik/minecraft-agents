(ns plan.adopt
  "A plan from what a body has seen: the cells of a box, each judged as the plan would want it, grouped by want into
  area parts (plans adopt). Leaves out the unseen, air, what a plan does not own (natural terrain, flowing fluid, a crop's
  farmland: tend owns it) and the state a swing or a neighbour changes (open, powered, waterlogged, connections).
  Pure: the caller reads the world and says what is natural terrain."
  (:require [clojure.string :as str]
            [plan.shape :as shape]))

(def kept-state "The state a want keeps: how a block is set, not what it does now." [:facing :half :axis :type :hinge :rotation :part])

(def crop-blocks #{"wheat" "carrots" "potatoes" "beetroots" "melon_stem" "pumpkin_stem" "nether_wart"})

(def fluid-names #{"water" "lava"})

(def stalk-names "Plants a farm job tends from the plan, kept though they grow wild: only the base of each column is a want." #{"sugar_cane" "bamboo"})

(defn crop-of
  "The crop a block is (any growth stage), nil for other blocks."
  [name]
  (let [stem (some-> (re-matches #"attached_(melon|pumpkin)_stem" name) second (str "_stem"))]
    (cond (crop-blocks name) name stem stem)))

(defn tree-of
  "The tree species a sapling is, nil for other blocks."
  [name]
  (or (some-> (re-matches #"(.+)_sapling" name) second)
      (some-> (re-matches #"(.+)_fungus" name) second)
      (when (= "mangrove_propagule" name) "mangrove")))

(defn want-of
  "The want of a seen block (:name :state), nil when the plan leaves the cell out. natural? is the caller's test of terrain.
  A source fluid is its name and a flowing one is left out."
  [{:keys [name state] :as block} {:keys [natural? terrain?]}]
  (cond
    (nil? name) nil
    (shape/air? name) nil
    (crop-of name) {:crop (crop-of name)}
    (tree-of name) {:tree (tree-of name)}
    (fluid-names name) (when-not (shape/flowing-fluid? block) name)
    (stalk-names name) name
    (and (not terrain?) (natural? name)) nil
    :else (let [kept (into {} (keep (fn [k] (when-let [v (shape/state-of state k)]
                                              (when-not (contains? (shape/server-decided-state name) k) [k (shape/state-text v)]))))
                           kept-state)]
            (if (empty? kept) name (assoc kept :block name)))))

(defn family
  "A part id's stem for a want: the plant, or the block's family (fence, gate, bed, door, wall-stone_bricks, ...)."
  [want]
  (let [name (cond (string? want) want (:crop want) (:crop want) (:tree want) (str "tree-" (:tree want)) :else (:block want))]
    (cond
      (str/ends-with? name "_fence_gate") "gate"
      (str/ends-with? name "_fence") "fence"
      (str/ends-with? name "_bed") "bed"
      (str/ends-with? name "_door") "door"
      (str/ends-with? name "_wall") (str "wall-" (str/replace name #"_wall$" ""))
      :else name)))

(defn part-ids
  "The wants in order, each with a part id: its family, a number after the first of a family."
  [wants]
  (second (reduce (fn [[seen out] want]
                    (let [f (family want) n (inc (get seen f 0))]
                      [(assoc seen f n) (conj out [(if (= 1 n) f (str f "-" n)) want])]))
                  [{} []] wants)))

(defn adopt
  "{:id :box [[x y z] [x y z]] :block-at (fn [[x y z]] -> {:name :state} or nil unseen) :natural? (fn [name]) :terrain? :body :date
  :pen {:box [[x y z] [x y z]] :mob :target} (adds the part \"pen\" animals.maintain reads)}
  -> {:plan {:id :note :parts [{:id :cells :want}]} :cells n :unseen n :parts {id n}}."
  [{:keys [id box block-at body date pen] :as opts}]
  (let [[[x1 y1 z1] [x2 y2 z2]] box
        poses (for [y (shape/span y1 y2) x (shape/span x1 x2) z (shape/span z1 z2)] [x y z])
        seen (into {} (keep (fn [pos] (when-let [b (block-at pos)] [pos b]))) poses)
        crop-ground (into #{} (comp (filter (fn [[_ b]] (crop-of (:name b)))) (map (fn [[[x y z] _]] [x (dec y) z]))) seen)
        wants (into {} (keep (fn [[[x y z :as pos] b]]
                               (when-not (or (and (= "farmland" (:name b)) (crop-ground pos))
                                             (and (stalk-names (:name b)) (= (:name b) (:name (seen [x (dec y) z])))))
                                 (when-let [w (want-of b opts)] [pos w]))))
                    seen)
        placed (filterv wants poses)
        by-want (group-by wants placed)
        parts (cond-> (mapv (fn [[part-id want]] {:id part-id :want want :cells (by-want want)}) (part-ids (distinct (map wants placed))))
                pen (conj {:id "pen" :box (:box pen) :want :any :herd (select-keys pen [:mob :target])}))]
    {:plan {:id id :note (str "adopted from " body "'s sight " date) :parts parts}
     :cells (count wants)
     :unseen (- (count poses) (count seen))
     :parts (into {} (map (juxt :id (comp count :cells))) parts)}))
