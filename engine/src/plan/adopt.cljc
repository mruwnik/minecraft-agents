(ns plan.adopt
  "A plan from what a body has seen: every seen cell of a box, as it is, grouped by want into parts (plans adopt); a want
  whose cells fill a box is one box part. Leaves out the unseen, air (bodies pass through it, crops grow into it), flowing
  fluid, the cells of an :any box (whatever), and the state a swing or a neighbour changes (open, powered, waterlogged,
  connections). Pure: the caller reads the world."
  (:require [clojure.string :as str]
            [plan.shape :as shape]))

(def kept-state "The state a want keeps: how a block is set, not what it does now." [:facing :half :axis :type :hinge :rotation :part])

(def crop-blocks #{"wheat" "carrots" "potatoes" "beetroots" "melon_stem" "pumpkin_stem" "nether_wart" "sugar_cane" "bamboo"})

(def fluid-names #{"water" "lava"})

(defn crop-of
  "The crop a block is (any growth stage; a melon or pumpkin is its stem's crop), nil for other blocks."
  [name]
  (let [stem (some-> (re-matches #"(?:attached_)?(melon|pumpkin)(?:_stem)?" name) second (str "_stem"))]
    (cond (crop-blocks name) name stem stem)))

(defn tree-of
  "The tree species a sapling is, nil for other blocks."
  [name]
  (or (some-> (re-matches #"(.+)_sapling" name) second)
      (some-> (re-matches #"(.+)_fungus" name) second)
      (when (= "mangrove_propagule" name) "mangrove")))

(defn want-of
  "The want of a seen block (:name :state), nil when the plan leaves the cell out. A source fluid is its name and a
  flowing one is left out."
  [{:keys [name state] :as block}]
  (cond
    (nil? name) nil
    (shape/air? name) nil
    (crop-of name) {:crop (crop-of name)}
    (tree-of name) {:tree (tree-of name)}
    (fluid-names name) (when-not (shape/flowing-fluid? block) name)
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

(defn fills-box
  "[min max] when two or more cells are exactly the cells of their bounding box, else nil."
  [cells]
  (let [lo (apply mapv min cells) hi (apply mapv max cells)]
    (when (and (next cells) (= (count cells) (reduce * (map (fn [a b] (inc (- b a))) lo hi)))) [lo hi])))

(defn adopt
  "{:id :box [[x y z] [x y z]] :block-at (fn [[x y z]] -> {:name :state} or nil unseen) :body :date
  :any [box ...] (areas that stay whatever: no cell of them is recorded; each is a part with want :any)
  :pen {:box [[x y z] [x y z]] :mob :target} (adds the part \"pen\" animals.maintain reads)}
  -> {:plan {:id :note :parts [{:id :cells|:box :want}]} :cells n :unseen n :parts {id n}}."
  [{:keys [id box block-at body date pen any] :as opts}]
  (let [[[x1 y1 z1] [x2 y2 z2]] box
        in-any? (fn [[x y z]] (some (fn [[[ax ay az] [bx by bz]]] (and (<= (min ax bx) x (max ax bx)) (<= (min ay by) y (max ay by)) (<= (min az bz) z (max az bz)))) any))
        poses (for [y (shape/span y1 y2) x (shape/span x1 x2) z (shape/span z1 z2)] [x y z])
        seen (into {} (keep (fn [pos] (when-let [b (block-at pos)] [pos b]))) poses)
        wants (into {} (keep (fn [[pos b]] (when-not (in-any? pos) (when-let [w (want-of b)] [pos w])))) seen)
        placed (filterv wants poses)
        by-want (group-by wants placed)
        counted (mapv (fn [[part-id want]]
                        (let [cells (by-want want) area (fills-box cells)]
                          [(count cells) (if area {:id part-id :want want :box area} {:id part-id :want want :cells cells})]))
                      (part-ids (distinct (map wants placed))))
        any-parts (map-indexed (fn [i b] [0 {:id (if (zero? i) "any" (str "any-" (inc i))) :box b :want :any}]) any)
        parts (cond-> (into (mapv second counted) (map second) any-parts)
                pen (conj {:id "pen" :box (:box pen) :want :any :herd (select-keys pen [:mob :target])}))]
    {:plan {:id id :note (str "adopted from " body "'s sight " date) :parts parts}
     :cells (count wants)
     :unseen (- (count poses) (count seen))
     :parts (into {} (map (fn [[n p]] [(:id p) n])) (into counted any-parts))}))
