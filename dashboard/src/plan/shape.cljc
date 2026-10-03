(ns plan.shape
  "The plan shape (DRAFT, see dashboard README \"Plans (draft)\"): the ONE namespace that knows what a plan is. Plain
  data in, plain data out, no IO and no host interop (.cljc, so the dashboard and later the engine's jobs share it):
    1. validation        plan-errors, element-error, content-error, region-error
    2. child resolution  children, child-cells, expand-plan (cycle guard)
    3. expansion         element-cells -> [{:pos [x y z] :want {...}}], blueprint cells placed and rotated
    4. cell predicates   judge / matches? / want-text: what counts as match, missing, wrong, extra
  Reading files and parsing EDN text is dashboard.plan; counting and grids over statuses is dashboard.plan-compare.

  World tolerance (the world moves by itself; block names only, the column decoder gives no block state):
  - a crop matches the crop block in any growth stage (age is state, never judged); farmland where a crop is wanted is
    missing (tilled, not yet planted), not wrong
  - the cell below a farmland crop (wheat, carrots, potatoes, beetroots, melon/pumpkin stems, torchflower, pitcher_crop)
    is judged as farmland: farmland matches; dirt, grass_block, coarse_dirt, dirt_path, air (dried back, trampled) are
    missing; anything else wrong
  - orientation (facing, half, axis, ...) is not judged: a block matches by name only"
  (:require [clojure.string :as str]))

(def plan-statuses #{:proposed :active :done :abandoned})
(def rotations #{0 90 180 270})
(def max-cells 200000)

;; ---------------------------------------------------------------- validation
(defn coords? [v] (and (vector? v) (= 3 (count v)) (every? integer? v)))

(defn region-error [region]
  (cond
    (not (map? region)) "region must be {:min [x y z] :max [x y z]}"
    (not (and (coords? (:min region)) (coords? (:max region)))) "region :min and :max must be [x y z] integers"
    (some true? (map > (:min region) (:max region))) "region :min must not exceed :max"))

(defn content-error [content]
  (let [ks (set (keys (when (map? content) content)))]
    (cond
      (not (map? content)) "content must be a map"
      (not= 1 (count ks)) "content must have exactly one of :crop :block :palette :blueprint :air"
      (contains? ks :crop) (when-not (string? (:crop content)) ":crop must be a block name string")
      (contains? ks :block) (when-not (string? (:block content)) ":block must be a block name string")
      (contains? ks :palette) (when-not (and (vector? (:palette content)) (seq (:palette content)) (every? string? (:palette content)))
                                ":palette must be a non-empty vector of block names")
      (contains? ks :blueprint) (when-not (string? (:blueprint content)) ":blueprint must be a blueprint name string")
      (contains? ks :air) (when-not (true? (:air content)) ":air must be true")
      :else "content must have exactly one of :crop :block :palette :blueprint :air")))

(defn element-error [element]
  (cond
    (not (map? element)) "element must be a map"
    (not (and (string? (:id element)) (re-matches #"[A-Za-z0-9_-]+" (:id element)))) "element :id must be a string of letters, digits, - and _"
    (not (keyword? (:kind element))) "element :kind must be a keyword"
    (= :plan (:kind element)) (when-not (string? (:ref element)) ":plan element needs :ref, the id of a plan")
    (= :structure (:kind element)) (or (when-not (coords? (:at element)) ":structure element needs :at [x y z]")
                                       (when-not (contains? rotations (get element :rotation 0)) ":rotation must be 0, 90, 180 or 270")
                                       (content-error (:content element))
                                       (when-not (contains? (:content element) :blueprint) ":structure content must be {:blueprint name}"))
    :else (or (region-error (:region element))
              (content-error (:content element))
              (when (contains? (:content element) :blueprint) ":blueprint content belongs to a :structure element"))))

(defn plan-errors
  "Every problem of a parsed plan, as strings; empty when it is valid. file-id is the file name without .edn."
  [plan file-id]
  (if-not (map? plan)
    ["a plan file must hold one map"]
    (let [elements (:elements plan)
          ids (map :id (filter map? elements))]
      (vec (remove nil?
                   (concat
                    [(when-not (= file-id (:id plan)) (str ":id must equal the file name, " (pr-str file-id)))
                     (when-not (or (nil? (:status plan)) (plan-statuses (:status plan)))
                       (str ":status must be one of " (str/join " " (sort plan-statuses))))
                     (when-not (or (nil? (:kind plan)) (keyword? (:kind plan))) ":kind must be a keyword")
                     (region-error (:region plan))
                     (when-not (or (nil? elements) (vector? elements)) ":elements must be a vector")
                     (when-not (apply distinct? (or (seq ids) [nil])) "element ids must be unique")]
                    (for [[i el] (map-indexed vector (when (vector? elements) elements))
                          :let [e (element-error el)] :when e]
                      (str "element " (or (when (map? el) (:id el)) i) ": " e))))))))

;; ---------------------------------------------------------------- cell predicates
(def air-names #{"air" "cave_air" "void_air"})
(def liquid-names #{"water" "lava"})
(def farmland-crops #{"wheat" "carrots" "potatoes" "beetroots" "melon" "pumpkin" "melon_stem" "pumpkin_stem"
                      "torchflower" "pitcher_crop"})
(def dried-ground-names #{"dirt" "grass_block" "coarse_dirt" "dirt_path"})

(defn air? [name] (contains? air-names name))

(defn crop-names
  "The blocks that count as this crop: the block itself; a melon or pumpkin crop (melon, melon_stem) also counts as
  its stem and its attached stem."
  [crop]
  (let [stem? (str/ends-with? crop "_stem")
        base (if stem? (subs crop 0 (- (count crop) 5)) crop)]
    (if (or (#{"melon" "pumpkin"} crop) stem?)
      #{(str base "_stem") (str "attached_" base "_stem")}
      #{crop})))

(defn matches?
  "Does this (non-air) block satisfy the want?"
  [want actual]
  (case (:kind want)
    :crop (contains? (crop-names (:crop want)) actual)
    :farmland (= "farmland" actual)
    :block (= (:block want) actual)
    :palette (contains? (set (:blocks want)) actual)
    :solid (not (contains? liquid-names actual))
    false))

(defn absent?
  "Does this non-matching block still count as 'not there yet' (missing) rather than wrong?"
  [want actual]
  (case (:kind want)
    :crop (= "farmland" actual)
    :farmland (contains? dried-ground-names actual)
    false))

(defn judge
  "match | missing (air where something is wanted, or the world's own pre-state, see ns doc) | wrong (another block)
  | extra (a block where air is wanted) | unknown (actual is nil: no column dumped)."
  [want actual]
  (cond
    (nil? actual) :unknown
    (= :air (:kind want)) (if (air? actual) :match :extra)
    (air? actual) :missing
    (matches? want actual) :match
    (absent? want actual) :missing
    :else :wrong))

(defn want-text [want]
  (case (:kind want)
    :crop (:crop want)
    :farmland "farmland"
    :block (:block want)
    :palette (str/join " | " (:blocks want))
    :solid "any solid block"
    :air "air"
    (str "?" (name (or (:kind want) :none)))))

;; ---------------------------------------------------------------- expansion
(defn content-text
  "A short summary of an element for people."
  [{:keys [kind content ref rotation]}]
  (cond
    (= :plan kind) (str "plan " ref)
    (contains? content :crop) (str "crop " (:crop content))
    (contains? content :block) (:block content)
    (contains? content :palette) (str "one of " (str/join ", " (:palette content)))
    (contains? content :blueprint) (str "blueprint " (:blueprint content) (when (pos? (or rotation 0)) (str " rot " rotation)))
    (:air content) "keep clear"
    :else ""))

(defn want-of
  "The comparison a content asks of each of its cells."
  [content]
  (cond
    (contains? content :crop) {:kind :crop :crop (:crop content)}
    (contains? content :block) {:kind :block :block (:block content)}
    (contains? content :palette) {:kind :palette :blocks (:palette content)}
    :else {:kind :air}))

(defn region-size [{[x1 y1 z1] :min [x2 y2 z2] :max}]
  (* (inc (- x2 x1)) (inc (- y2 y1)) (inc (- z2 z1))))

(defn region-cells [{[x1 y1 z1] :min [x2 y2 z2] :max} border?]
  (for [y (range y1 (inc y2)) z (range z1 (inc z2)) x (range x1 (inc x2))
        :when (or (not border?) (#{x1 x2} x) (#{z1 z2} z))]
    [x y z]))

(defn rotate
  "Offset (dx, dz) turned clockwise (seen from above, x east, z south) by rotation degrees about the anchor."
  [rotation [dx dz]]
  (case rotation
    90 [(- dz) dx]
    180 [(- dx) (- dz)]
    270 [dz (- dx)]
    [dx dz]))

(defn blueprint-want [{:keys [names air]}]
  (cond
    air {:kind :air}
    (= ["@solid"] (vec names)) {:kind :solid}
    (= 1 (count names)) {:kind :block :block (first names)}
    :else {:kind :palette :blocks (vec names)}))

(defn structure-cells
  "cells = blueprint cells [{:dx :dy :dz :names [..] :air bool}], placed with offset (0, 0, 0) at :at, rotated."
  [{[ax ay az] :at :keys [rotation]} cells]
  (for [{:keys [dx dy dz] :as c} cells
        :let [[rx rz] (rotate (or rotation 0) [dx dz])]]
    {:pos [(+ ax rx) (+ ay dy) (+ az rz)] :want (blueprint-want c)}))

(defn with-ground
  "A crop cell on farmland also wants the farmland below it; any other cell stands alone."
  [{[x y z] :pos {:keys [kind crop]} :want :as cell}]
  (if (and (= :crop kind) (contains? farmland-crops crop))
    [cell {:pos [x (dec y) z] :want {:kind :farmland}}]
    [cell]))

(defn element-cells
  "-> {:cells [{:pos :want}]} or {:error string}"
  [{:keys [kind content region] :as element} blueprint-fn]
  (cond
    (= :structure kind)
    (if-let [bp (blueprint-fn (:blueprint content))]
      {:cells (vec (structure-cells element bp))}
      {:error (str "unknown blueprint " (pr-str (:blueprint content)))})

    (> (region-size region) max-cells)
    {:error (str "region too large (" (region-size region) " cells, at most " max-cells ")")}

    :else
    (let [want (want-of content)]
      {:cells (vec (mapcat with-ground (for [pos (region-cells region (= :border kind))] {:pos pos :want want})))})))

(declare expand-plan)

(defn child-cells
  "A :plan element: the child's cells, relabelled as this element's. Cycles and unknown refs are errors."
  [plans blueprint-fn trail {:keys [ref]}]
  (cond
    (some #{ref} trail) {:error (str "cycle: " (str/join " -> " (concat trail [ref])))}
    (not (contains? plans ref)) {:error (str "unknown plan " (pr-str ref))}
    :else (let [{:keys [cells errors]} (expand-plan plans ref blueprint-fn (conj (vec trail) ref))]
            {:cells (mapv #(dissoc % :element) cells) :child-errors errors})))

(defn expand-plan
  "A plan with its children resolved: {:cells [{:pos :want :element}] :elements [{:id :kind :content :cells? :error}]
  :errors [{:element id :error string}]}. trail = the plan ids being expanded (cycle guard)."
  ([plans id blueprint-fn] (expand-plan plans id blueprint-fn [id]))
  ([plans id blueprint-fn trail]
   (let [expanded (for [el (:elements (get plans id))
                        :let [r (if (= :plan (:kind el))
                                  (child-cells plans blueprint-fn trail el)
                                  (element-cells el blueprint-fn))]]
                    {:element {:id (:id el) :kind (:kind el) :content (content-text el)
                               :ref (:ref el) :at (:at el) :rotation (:rotation el)
                               :count (count (:cells r))
                               :error (or (:error r)
                                          (when (seq (:child-errors r))
                                            (str (count (:child-errors r)) " problem(s) inside plan " (:ref el))))}
                     :cells (mapv #(assoc % :element (:id el)) (:cells r))
                     :errors (concat (when (:error r) [{:element (:id el) :error (:error r)}])
                                     (for [e (:child-errors r)] (update e :element #(str (:id el) "/" %))))})]
     {:cells (vec (mapcat :cells expanded))
      :elements (mapv :element expanded)
      :errors (vec (mapcat :errors expanded))})))

(defn children
  "Ids of the plans this plan nests."
  [plan]
  (vec (keep #(when (= :plan (:kind %)) (:ref %)) (:elements plan))))
