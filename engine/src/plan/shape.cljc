(ns plan.shape
  "Plans and blueprints (docs/design.md, \"Plans and blueprints\"): the ONE namespace that knows what they are. Plain data
  in, plain data out, no IO and no host interop (.cljc, so the dashboard and the engine's jobs share it):
    1. checking     plan-errors, blueprint-errors, want-error
    2. where        part-cells (:box, :outline, :cells)
    3. blueprints   place, turn-want, front-after
    4. expansion    expand: a plan and its blueprints -> cells [{:pos :want :part}], spots, part summaries, errors
    5. plan minus world  judge, plan-minus-world, assignment-answers
  Parsing file text is plan.parse; reading the files is dashboard.plan and engine.world; counting and grids over the
  answers is dashboard.plan-compare.

  A world block is nil (nobody has seen the cell), {:name n} (names only) or {:name n :state {k v}} (state keys and
  values as keywords or strings). Turning a blueprint turns facing, axis, rotation (signs, banners, heads) and the
  north/east/south/west connection keys (fences, panes, walls, vines). Not turned yet: rail shape (north_south,
  ascending_east, south_east ...), orientation (jigsaw, crafter), and any other state naming a side in its value."
  (:require [clojure.string :as str]))

(def turns #{0 90 180 270})
(def sides [:north :east :south :west])
(def max-cells 200000)

(def plan-keys #{:id :parts :assign :note :kind :at :metadata})
(defn author
  "The body or agent that made a plan, :metadata :by; nil when it names none."
  [plan]
  (get-in plan [:metadata :by]))

(defn with-author
  "The plan with :metadata :by set to by when it names no maker yet; an existing maker is kept (the plan tools stamp the first writer; the engine reads it back)."
  [plan by]
  (cond-> plan (nil? (author plan)) (assoc-in [:metadata :by] by)))

(def where-keys [:box :outline :cells])
(def area-part-keys #{:id :box :outline :cells :want :note})
(def blueprint-part-keys #{:id :blueprint :at :turn :note})
(def blueprint-keys #{:id :front :key :layers :spots :note})

(def air-names #{"air" "cave_air" "void_air"})

(defn air? [name] (contains? air-names name))

;; ---------------------------------------------------------------- checking
(defn coords? [v] (and (vector? v) (= 3 (count v)) (every? integer? v)))

(defn finite-number? [v]
  (and (number? v) #?(:clj (Double/isFinite (double v)) :cljs (js/Number.isFinite v))))

(defn anchor? [v]
  (and (vector? v) (= 3 (count v)) (every? finite-number? v)))

(defn block-want? [want]
  (and (map? want) (string? (:block want)) (every? keyword? (keys want))))

(defn want-error
  "nil when want is one of: a block name, a block with state, [:any name-or-block ...], {:crop c}, {:tree s}, :clear."
  [want]
  (cond
    (= :clear want) nil
    (string? want) (when (str/blank? want) "want: a block name must not be empty")
    (vector? want) (when-not (and (= :any (first want)) (seq (rest want))
                                  (every? #(or (string? %) (block-want? %)) (rest want)))
                     "want: an any-of list is [:any name-or-block ...]")
    (and (map? want) (contains? want :block)) (when-not (block-want? want) "want: {:block name ...state} needs a name")
    (and (map? want) (= #{:crop} (set (keys want)))) (when-not (string? (:crop want)) "want: {:crop name}")
    (and (map? want) (= #{:tree} (set (keys want)))) (when-not (string? (:tree want)) "want: {:tree species}")
    :else (str "want: not a block, block with state, any-of, crop, tree or :clear: " (pr-str want))))

(defn corners? [v] (and (vector? v) (= 2 (count v)) (every? coords? v)))

(defn box-size [[[x1 y1 z1] [x2 y2 z2]]]
  (* (inc (abs (- x2 x1))) (inc (abs (- y2 y1))) (inc (abs (- z2 z1)))))

(defn unknown-key-error [part allowed]
  (when-let [k (first (remove allowed (keys part)))]
    (str "unknown key " k)))

(defn area-part-error [part]
  (let [wheres (filter #(contains? part %) where-keys)
        where (first wheres)]
    (cond
      (not= 1 (count wheres)) "a part needs exactly one of :box :outline :cells (or :blueprint)"
      (and (#{:box :outline} where) (not (corners? (where part)))) (str where " needs two corners [[x y z] [x y z]]")
      (and (#{:box :outline} where) (> (box-size (where part)) max-cells))
      (str where " too large (" (box-size (where part)) " cells, at most " max-cells ")")
      (and (= :cells where) (not (and (vector? (:cells part)) (seq (:cells part)) (every? coords? (:cells part)))))
      ":cells needs a non-empty list of [x y z] integers"
      (not (contains? part :want)) "a part needs a :want"
      :else (or (want-error (:want part)) (unknown-key-error part area-part-keys)))))

(defn blueprint-part-error [part]
  (cond
    (not (string? (:blueprint part))) ":blueprint must be the name of a blueprint"
    (not (coords? (:at part))) "a placed blueprint needs :at [x y z], its north-west bottom corner after turning"
    (not (contains? turns (get part :turn 0))) ":turn must be 0, 90, 180 or 270"
    :else (unknown-key-error part blueprint-part-keys)))

(defn part-id? [id] (and (string? id) (re-matches #"[A-Za-z0-9_.-]+" id)))

(defn part-error [part]
  (cond
    (not (map? part)) "a part must be a map"
    (contains? part :blueprint) (blueprint-part-error part)
    :else (area-part-error part)))

(defn plan-errors
  "Every problem of a parsed plan as [{:part id :error text}]; :part is absent for problems of the plan as a whole or
  of a part without a usable id. Empty when it is valid. file-id is the file name without .edn."
  [plan file-id]
  (if-not (map? plan)
    [{:error "a plan file must hold one map"}]
    (let [parts (:parts plan)
          ids (keep #(when (map? %) (:id %)) (when (vector? parts) parts))]
      (vec (concat
            (for [e [(when-not (= file-id (:id plan)) (str ":id must equal the file name, " (pr-str file-id)))
                     (when-not (vector? parts) ":parts must be a vector of parts")
                     (when (and (contains? plan :kind) (not (keyword? (:kind plan)))) ":kind must be a keyword")
                     (when (and (contains? plan :at) (not (anchor? (:at plan)))) ":at must be three finite coordinates [x y z]")
                     (when (and (contains? plan :metadata) (not (map? (:metadata plan)))) ":metadata must be a map")
                     (when-not (or (nil? (:assign plan)) (and (vector? (:assign plan)) (every? #(string? (:spot %)) (:assign plan))))
                       ":assign must be a vector of {:spot name ...}")
                     (when-not (or (empty? ids) (apply distinct? ids)) "part ids must be unique")
                     (unknown-key-error plan plan-keys)]
                  :when e]
              {:error e})
            (for [[i part] (map-indexed vector (when (vector? parts) parts))
                  :let [id (when (map? part) (:id part))
                        e (if (part-id? id) (part-error part) (str "part " i ": :id must be letters, digits, . - _"))]
                  :when e]
              (if (part-id? id) {:part id :error e} {:error e})))))))

(defn layer-size
  "[width depth] of a blueprint: letters per row (x, west to east) and rows per layer (z, north to south)."
  [{:keys [layers]}]
  [(count (ffirst layers)) (count (first layers))])

(defn key-error [letter want]
  (cond
    (not (and (string? letter) (= 1 (count letter)))) (str "key " (pr-str letter) ": a key is one letter")
    (and (map? want) (contains? want :blueprint)) (str "key " (pr-str letter) ": a blueprint cannot contain a blueprint")
    (not (or (= :clear want) (and (string? want) (not (str/blank? want))) (block-want? want)))
    (str "key " (pr-str letter) ": a letter stands for a block, a block with state, or :clear")))

(defn layers-error [{:keys [layers] :as bp}]
  (if-not (and (vector? layers) (seq layers) (every? vector? layers) (every? #(every? string? %) layers))
    ":layers must be a non-empty vector of layers, each a vector of row strings"
    (let [[w d] (layer-size bp)]
      (when-not (and (pos? w) (every? #(= d (count %)) layers) (every? #(= w (count %)) (apply concat layers)))
        "every layer must have the same number of rows, every row the same number of letters"))))

(defn blueprint-errors
  "Every problem of a parsed blueprint as [{:error text}]; empty when it is valid."
  [bp file-id]
  (if-not (map? bp)
    [{:error "a blueprint file must hold one map"}]
    (let [layers-problem (layers-error bp)
          [w d] (when-not layers-problem (layer-size bp))
          h (count (:layers bp))
          inside? (fn [at] (and (coords? at) (let [[x y z] at] (and (< -1 x w) (< -1 y h) (< -1 z d)))))
          letters (when-not layers-problem (set (map str (apply concat (apply concat (:layers bp))))))]
      (vec (for [e (concat
                    [(when-not (= file-id (:id bp)) (str ":id must equal the file name, " (pr-str file-id)))
                     (when-not (some #{(:front bp)} sides) ":front must be :north, :east, :south or :west")
                     (when-not (map? (:key bp)) ":key must map letters to blocks")
                     layers-problem
                     (when-not (or (nil? (:spots bp)) (map? (:spots bp))) ":spots must map names to [x y z]")
                     (when-let [k (first (remove blueprint-keys (keys bp)))]
                       (str "unknown key " k " (a blueprint cannot contain a blueprint or parts)"))]
                    (when (map? (:key bp)) (for [[letter want] (sort-by key (:key bp))] (key-error letter want)))
                    (when (map? (:key bp))
                      (for [letter (sort (remove #(contains? (:key bp) %) letters))]
                        (str "letter " (pr-str letter) " is not in the key")))
                    (when (and (map? (:spots bp)) (not layers-problem))
                      (for [[spot at] (sort-by key (:spots bp))
                            :when (not (inside? at))]
                        (str "spot " (pr-str spot) " must be [x y z] inside the blueprint"))))
                     :when e]
                 {:error e})))))

;; ---------------------------------------------------------------- where
(defn span [a b] (range (min a b) (inc (max a b))))

(defn part-cells
  "The cells a :box, :outline or :cells part names, each once. An outline is the plan-view ring of its box at every y."
  [{:keys [box outline cells]}]
  (cond
    cells (vec (distinct cells))
    :else (let [[[x1 y1 z1] [x2 y2 z2]] (or box outline)
                ring? (fn [x z] (or (= x (min x1 x2)) (= x (max x1 x2)) (= z (min z1 z2)) (= z (max z1 z2))))]
            (vec (for [y (span y1 y2) z (span z1 z2) x (span x1 x2)
                       :when (or box (ring? x z))]
                   [x y z])))))

;; ---------------------------------------------------------------- blueprints
(defn steps [turn] (quot (mod turn 360) 90))

(defn turn-side
  "A side turned clockwise by turn degrees; up, down and anything else unchanged. Keeps keyword or string."
  [side turn]
  (let [i (.indexOf sides (keyword side))]
    (if (neg? i)
      side
      (let [turned (nth sides (mod (+ i (steps turn)) 4))]
        (if (keyword? side) turned (name turned))))))

(defn turn-axis [axis turn]
  (if (and (odd? (steps turn)) (#{"x" "z"} (name axis)))
    (let [turned (if (= "x" (name axis)) :z :x)]
      (if (keyword? axis) turned (name turned)))
    axis))

(defn turn-want
  "A block want as it stands after turning clockwise by turn degrees (see the ns doc for the state that turns)."
  [want turn]
  (if-not (block-want? want)
    want
    (let [connections (select-keys want sides)]
      (cond-> (apply dissoc want sides)
        (contains? want :facing) (update :facing turn-side turn)
        (contains? want :axis) (update :axis turn-axis turn)
        (integer? (:rotation want)) (update :rotation #(mod (+ % (* 4 (steps turn))) 16))
        true (merge (into {} (map (fn [[k v]] [(turn-side k turn) v])) connections))))))

(defn front-after [bp turn] (turn-side (:front bp) turn))

(defn turn-offset
  "A blueprint offset [x y z] (turn 0: x west to east, z north to south) after turning, still measured from the
  north-west bottom corner of the turned building. size = [width depth] at turn 0."
  [[x y z] [w d] turn]
  (case (steps turn)
    1 [(- d 1 z) y x]
    2 [(- w 1 x) y (- d 1 z)]
    3 [z y (- w 1 x)]
    [x y z]))

(defn blueprint-cells
  "Every cell of a blueprint at turn 0: [{:offset [x y z] :want w}], floor first."
  [{:keys [key layers]}]
  (vec (for [[y layer] (map-indexed vector layers)
             [z row] (map-indexed vector layer)
             [x letter] (map-indexed vector row)]
         {:offset [x y z] :want (get key (str letter))})))

(defn place
  "A blueprint placed by a plan part {:id :at :turn}: {:cells [{:pos :want}] :spots {\"id/spot\" [x y z]}}."
  [bp {:keys [id at turn]}]
  (let [turn (or turn 0)
        size (layer-size bp)
        world (fn [offset] (mapv + at (turn-offset offset size turn)))]
    {:cells (mapv (fn [{:keys [offset want]}] {:pos (world offset) :want (turn-want want turn)}) (blueprint-cells bp))
     :spots (into {} (map (fn [[spot offset]] [(str id "/" spot) (world offset)])) (:spots bp))}))

;; ---------------------------------------------------------------- expansion
(defn where-of [part] (or (some #(when (contains? part %) %) (conj where-keys :blueprint)) :none))

(defn expand-part
  "-> {:cells [{:pos :want :part}] :spots {..}} or {:error text}"
  [blueprints part]
  (if (contains? part :blueprint)
    (if-let [bp (get blueprints (:blueprint part))]
      (update (place bp part) :cells (fn [cells] (mapv #(assoc % :part (:id part)) cells)))
      {:error (str "no blueprint called " (pr-str (:blueprint part)))})
    {:cells (mapv (fn [pos] {:pos pos :want (:want part) :part (:id part)}) (part-cells part))}))

(defn later-wins
  "Cells with each position kept once, from the last part that names it, in their first order otherwise."
  [cells]
  (let [last-index (into {} (map-indexed (fn [i c] [(:pos c) i])) cells)]
    (vec (keep-indexed (fn [i c] (when (= i (last-index (:pos c))) c)) cells))))

(defn expand
  "A plan (already checked by plan-errors) with its blueprints {name blueprint} ->
  {:cells [{:pos [x y z] :want w :part id}]   one per position, the later part winning a shared cell
   :spots {\"placement/spot\" [x y z]}
   :parts [{:id :where :want :blueprint :at :turn :count :error}]   :count = cells the part still holds
   :errors [{:part id :error text} | {:assign spot :error text}]}"
  [plan blueprints]
  (let [results (mapv #(expand-part blueprints %) (:parts plan))
        cells (later-wins (mapcat :cells results))
        held (frequencies (map :part cells))
        spots (apply merge {} (map :spots results))
        names (into (set (map :id (:parts plan))) (keys spots))]
    {:cells cells
     :spots spots
     :parts (mapv (fn [part {:keys [error]}]
                    (cond-> (merge {:id (:id part) :where (where-of part) :count (get held (:id part) 0)}
                                   (select-keys part [:want :blueprint :at]))
                      (contains? part :blueprint) (assoc :turn (get part :turn 0))
                      error (assoc :error error)))
                  (:parts plan) results)
     :errors (vec (concat (for [[part {:keys [error]}] (map vector (:parts plan) results) :when error]
                            {:part (:id part) :error error})
                          (for [{:keys [spot]} (:assign plan) :when (not (contains? names spot))]
                            {:assign spot :error (str "no part or spot called " (pr-str spot))})))}))

;; ---------------------------------------------------------------- plan minus world
(defn crop-names
  "The blocks that count as this crop: the block itself; melon and pumpkin (or their stems) count as the stem and the
  attached stem."
  [crop]
  (let [base (str/replace crop #"_stem$" "")]
    (if (#{"melon" "pumpkin"} base)
      #{(str base "_stem") (str "attached_" base "_stem")}
      #{crop})))

(defn tree-names
  "The blocks that hold a tree spot of this species: its sapling and its trunk."
  [species]
  (case species
    "mangrove" #{"mangrove_propagule" "mangrove_log"}
    ("crimson" "warped") #{(str species "_fungus") (str species "_stem")}
    #{(str species "_sapling") (str species "_log")}))

(defn state-text [v] (if (keyword? v) (name v) (str v)))

(defn state-of [state k]
  (if (contains? state k) (get state k) (get state (name k))))

(defn judge-block
  "A block want (name or block with state) against a non-air block: :match, :wrong, or :unknown when the name
  matches, the want names state and the world gave names only."
  [want {:keys [name state]}]
  (let [{wanted :block :as want} (if (string? want) {:block want} want)
        wanted-state (dissoc want :block)]
    (cond
      (not= wanted name) :wrong
      (empty? wanted-state) :match
      (nil? state) :unknown
      (every? (fn [[k v]] (= (state-text v) (some-> (state-of state k) state-text))) wanted-state) :match
      :else :wrong)))

(defn judge
  "One planned cell: :match (the want holds), :missing (air where something is wanted), :wrong (something else),
  :extra (something where :clear is wanted) or :unknown (nobody has seen the cell, or the state decides and the world
  gave names only). A crop matches at any growth stage; the ground below it is not judged."
  [want block]
  (let [name (:name block)]
    (cond
      (nil? block) :unknown
      (= :clear want) (if (air? name) :match :extra)
      (air? name) :missing
      (string? want) (judge-block want block)
      (vector? want) (let [answers (set (map #(judge-block % block) (rest want)))]
                       (cond (answers :match) :match (answers :unknown) :unknown :else :wrong))
      (contains? want :crop) (if (contains? (crop-names (:crop want)) name) :match :wrong)
      (contains? want :tree) (if (contains? (tree-names (:tree want)) name) :match :wrong)
      :else (judge-block want block))))

(defn plan-minus-world
  "Every expanded cell with :answer (judge) and :found (the world's block name, nil when unseen). block-at takes a
  position [x y z] and gives a world block (see the ns doc) or nil."
  [cells block-at]
  (mapv (fn [{:keys [pos want] :as cell}]
          (let [block (block-at pos)]
            (assoc cell :answer (judge want block) :found (:name block))))
        cells))

(defn assignment-answers
  "Each assignment of the plan with its :answer. A villager's profession or trade and who uses a bed or a chest are
  wishes blocks cannot show, so every assignment answers :unknown until a body that has looked reports otherwise."
  [plan]
  (mapv #(assoc % :answer :unknown) (:assign plan)))

(defn want-text [want]
  (cond
    (= :clear want) "clear"
    (string? want) want
    (vector? want) (str/join " | " (map want-text (rest want)))
    (contains? want :crop) (str "crop " (:crop want))
    (contains? want :tree) (str "tree " (:tree want))
    :else (let [state (dissoc want :block)]
            (if (empty? state)
              (:block want)
              (str (:block want) "[" (str/join "," (for [[k v] (sort-by key state)] (str (name k) "=" (state-text v)))) "]")))))

(defn want-block
  "The block a want is drawn as: its name, the first choice of an :any, the plant of a crop, the leaves of a tree;
  nil for :clear."
  [want]
  (cond
    (= :clear want) nil
    (string? want) want
    (vector? want) (want-block (second want))
    (contains? want :crop) (first (remove #(str/starts-with? % "attached_") (crop-names (:crop want))))
    (contains? want :tree) (case (:tree want)
                             "crimson" "nether_wart_block"
                             "warped" "warped_wart_block"
                             (str (:tree want) "_leaves"))
    :else (:block want)))
