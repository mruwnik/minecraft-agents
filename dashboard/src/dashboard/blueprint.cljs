(ns dashboard.blueprint
  "The blueprint library page's pure half: list rows, the cells of one layer, the legend, the bill, lint lines,
  material role cards, and the isometric preview's faces. Ported from tools/dashboard/blueprint.mjs.
  Input is the JSON /api/blueprints serves, keywordized: a detail has :name :hash :bp :bill :lint :errors :builds."
  (:require [clojure.string :as str]
            [dashboard.blockcolour :as bc]))

(defn kname [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn primary [spec] (first (:alts spec)))

(defn spec-for [bp token] (get (:legend bp) (keyword token)))

;; ---------------------------------------------------------------- cells, legend, hover
(defn states-text [states]
  (if (empty? states)
    ""
    (str "[" (str/join "," (for [[k v] states] (str (kname k) "=" v))) "]")))

;; the block a token stands for, written the way the legend line wrote it; @solid in words
(defn cell-label [spec]
  (let [alt (primary spec)]
    (if (= "@solid" (:name alt)) "any solid block" (str (:name alt) (states-text (:states alt))))))

(defn row-tokens [row] (vec (js/Array.from row)))

;; every cell of layer y that is part of the blueprint (a _ cell is not), west to east then north to south
(defn layer-cells [bp y]
  (if-let [layer (first (filter #(= y (:y %)) (:layers bp)))]
    (vec (for [[dz row] (map-indexed vector (:grid layer))
               [dx token] (map-indexed vector (row-tokens row))
               :when (not= "_" token)
               :let [spec (spec-for bp token)
                     alt (primary spec)]]
           {:dx dx :dz dz :y y :token token :name (:name alt) :label (cell-label spec)
            :colour (bc/alt-colour alt) :air (bc/air? (:name alt))}))
    []))

;; what the page says under the cursor: the cell's offset from the anchor (x east, z south), then the block
(defn hover-text [{:keys [dx y dz label token]}]
  (str "x+" dx " y" y " z+" dz " · " label " (" token ")"))

;; the tokens the layers actually use, first met from the lowest layer up, with how many cells each fills
(defn legend-rows [bp]
  (let [cells (mapcat #(layer-cells bp (:y %)) (sort-by :y (:layers bp)))
        step (fn [{:keys [order rows]} {:keys [token name label colour air]}]
               (if air
                 {:order order :rows rows}
                 {:order (if (contains? rows token) order (conj order token))
                  :rows (update rows token
                                #(update (or % {:token token :name name :label label :colour colour
                                                :tags (or (:tags (spec-for bp token)) []) :count 0})
                                         :count inc))}))
        {:keys [order rows]} (reduce step {:order [] :rows {}} cells)]
    (mapv rows order)))

;; ---------------------------------------------------------------- the bill and the list
(defn bill-rows [items]
  (let [rows (for [[k v] items] {:item (kname k) :count v})
        mx (apply max 0 (map :count rows))]
    (->> rows
         (sort-by (juxt (comp - :count) :item))
         (mapv #(assoc % :share (/ (:count %) mx))))))

(defn block-count [bill] (reduce + 0 (vals (:total bill))))

(defn layer-height [bp] (inc (- (:y (last (:layers bp))) (:y (first (:layers bp))))))

(defn footprint [bp] (str (:width bp) "x" (:depth bp) "x" (layer-height bp)))

(defn status-of [{:keys [bp lint]}]
  (let [n (count (:warnings lint))]
    (cond
      (nil? bp) "does not parse"
      (seq (:errors lint)) "build refuses"
      (zero? n) "ok"
      :else (str n " warning" (when-not (= 1 n) "s")))))

(defn status-level [status]
  (case status
    "ok" "ok"
    "build refuses" "error"
    "does not parse" "parse"
    "warning"))

;; one row of the list: a file that does not parse still gets one
(defn blueprint-row [{:keys [name bp bill builds] :as detail}]
  (let [n (count builds)]
    (if-not bp
      {:name name :title "" :kind "" :footprint "" :layers 0 :blocks 0 :status "does not parse" :builds n}
      {:name name :title (:title bp) :kind (str/join ", " (:tags bp)) :footprint (footprint bp)
       :layers (count (:layers bp)) :blocks (block-count bill) :status (status-of detail) :builds n})))

(defn bill-note [{:keys [bill]}]
  (let [per (str/join " · " (for [l (:layers bill)] (str "y" (:y l) ": " (reduce + 0 (vals (:items l))))))
        extras (concat (when (seq (:tools bill)) [(str "tools: " (str/join ", " (:tools bill)))])
                       (when (seq (:scaffold bill))
                         [(str "scaffold: " (str/join ", " (for [[k v] (:scaffold bill)] (str v " " (kname k)))))]))]
    (str/join " · " (concat [(str (block-count bill) " items in all") (str "per layer " per)] extras))))

;; what the parser and lint said, as lines with a level; one ok line when there is nothing to say
(defn lint-lines [{:keys [bp errors lint]}]
  (if-not bp
    (mapv (fn [text] {:level "parse" :text text}) errors)
    (let [lines (concat (map (fn [text] {:level "error" :text text}) (:errors lint))
                        (map (fn [text] {:level "warning" :text text}) (:warnings lint)))]
      (if (seq lines) (vec lines) [{:level "ok" :text "lint has nothing to say: build accepts it"}]))))

(defn params-text [params]
  (str/join " " (for [[k v] params] (str (kname k) "=" v))))

(defn meta-facts [{:keys [bp hash]}]
  (let [clearance (:clearance bp)]
    (vec (filter (fn [[_ v]] (not (str/blank? (str v))))
                 [["tags" (str/join ", " (:tags bp))] ["front" (:front bp)] ["foundation" (:foundation bp)]
                  ["clearance" (when (some? clearance) (str clearance " air layer" (when-not (= 1 clearance) "s")))]
                  ["difficulty" (:difficulty bp)] ["params" (params-text (:params bp))] ["by" (:by bp)]
                  ["hash" hash] ["notes" (:notes bp)]]))))

(defn build-text [{:keys [place x y z facing params by]}]
  (str place " (" x "," y "," z
       (when facing (str ", facing " facing))
       (when (seq params) (str ", " (params-text params)))
       (when by (str ", by " by)) ")"))

(defn older-version? [build] (not (:current build)))

(defn find-detail [details name] (first (filter #(= name (:name %)) details)))

;; ---------------------------------------------------------------- material roles
(def relation-labels {"material" "same block" "family" "matching family" "color" "matching color"})

(defn camel->words [s] (str/lower-case (str/replace s #"([A-Z])" " $1")))

(defn slot-required [{:keys [kind familyClass requires candidates acceptExisting]}]
  (concat [(str "shape: " (str/replace kind "_" " "))]
          (when familyClass [(str "family: " familyClass)])
          (for [[k v] requires] (str (camel->words (kname k)) ": " (if v "required" "excluded")))
          (when candidates [(str "allowed: " (str/join ", " candidates))])
          (when (seq acceptExisting) [(str "existing blocks also allowed: " (str/join ", " acceptExisting))])))

(defn relation-lines [relationships role]
  (for [{:keys [members relation strength]} relationships
        :when (some #{role} members)
        :let [peers (remove #{role} members)
              required? (= "required" strength)]]
    [required?
     (str (get relation-labels relation relation)
          (if (seq peers) (str " with " (str/join ", " peers)) " throughout this role")
          (if required? " required" " preferred; mixing allowed"))]))

(defn material-objects [{:keys [materialObjects document]}]
  (or materialObjects (concat (vals (get-in document [:structure :legend])) (get-in document [:structure :objects]))))

;; Required capabilities kept apart from preferences and from the preview's concrete palette. Roles are sorted by
;; name: the JSON object's own order does not survive keywordizing.
(defn material-role-rows [{:keys [materials relationships allocation] :as detail}]
  (let [objects (material-objects detail)
        rows (for [[k slot] (sort-by (comp kname key) materials)
                   :let [role (kname k)
                         relations (relation-lines relationships role)]]
               {:role role
                :required (vec (concat (slot-required slot) (for [[req? text] relations :when req?] text)))
                :preferences (vec (concat (when (seq (:preferences slot))
                                            [(str "preferred examples: " (str/join ", " (:preferences slot))
                                                  " (other allowed blocks can be selected)")])
                                          (for [[req? text] relations :when (not req?)] text)))
                :palette (vec (distinct (keep #(get-in allocation [:assignments (keyword (:id %))])
                                              (filter #(= role (:material %)) objects))))})
        pins (distinct (keep :block objects))]
    (vec (concat rows
                 (when (seq pins)
                   [{:role "exact functional blocks" :required [(str "must use: " (str/join ", " pins))] :preferences [] :palette []}])))))

;; ---------------------------------------------------------------- the isometric preview
(defn project [cos sin [x y z]]
  {:x (- (* cos x) (* sin z))
   :y (- (* 0.45 (+ (* sin x) (* cos z))) (* 0.85 y))
   :depth (+ (* 0.85 (+ (* sin x) (* cos z))) (* 0.45 y))})

(defn box-sides [cos sin [x y z X Y Z]]
  [{:points [[x Y z] [X Y z] [X Y Z] [x Y Z]] :light 1.1}
   {:points (if (>= cos 0) [[x y Z] [x Y Z] [X Y Z] [X y Z]] [[X y z] [X Y z] [x Y z] [x y z]]) :light 0.72}
   {:points (if (>= sin 0) [[X y Z] [X Y Z] [X Y z] [X y z]] [[x y z] [x Y z] [x Y Z] [x y Z]]) :light 0.88}])

(defn cell-faces [cos sin {:keys [x y z name states shapes] :as cell}]
  (let [colour (bc/alt-colour {:name name :states states})]
    (for [[a b c d e f] shapes
          side (box-sides cos sin [(+ x a) (+ y b) (+ z c) (+ x d) (+ y e) (+ z f)])
          :let [points (mapv #(project cos sin %) (:points side))]]
      {:cell cell :points points
       :depth (/ (reduce + (map :depth points)) 4)
       :colour (bc/shade (or colour "#4b5462") (:light side))})))

;; Orthographic whole-building preview. Faces carry their source cell for hover; the layer cutoff exposes rooms.
(defn preview-faces [cells {:keys [angle max-y] :or {angle (/ js/Math.PI 4) max-y js/Infinity}}]
  (let [cos (js/Math.cos angle) sin (js/Math.sin angle)]
    (->> cells
         (filter #(<= (:y %) max-y))
         (mapcat #(cell-faces cos sin %))
         (sort-by :depth)
         vec)))

(defn preview-bounds [faces]
  (let [ps (for [f faces p (:points f)] p)]
    (if (empty? ps)
      {:x1 0 :x2 1 :y1 0 :y2 1}
      {:x1 (apply min (map :x ps)) :x2 (apply max (map :x ps))
       :y1 (apply min (map :y ps)) :y2 (apply max (map :y ps))})))

;; faces with their points in canvas pixels, the whole building fitted to w x h
(defn fit-faces [faces {:keys [x1 x2 y1 y2]} w h]
  (let [scale (min (/ (- w 40) (max 1 (- x2 x1))) (/ (- h 40) (max 1 (- y2 y1))))
        px (fn [p] {:x (+ (/ w 2) (* (- (:x p) (/ (+ x1 x2) 2)) scale))
                    :y (+ (/ h 2) (* (- (:y p) (/ (+ y1 y2) 2)) scale))})]
    (mapv #(update % :points (fn [ps] (mapv px ps))) faces)))

(defn inside-polygon? [points x y]
  (let [n (count points)]
    (loop [i 0 j (dec n) inside false]
      (if (= i n)
        inside
        (let [a (nth points i) b (nth points j)
              crosses (and (not= (> (:y a) y) (> (:y b) y))
                           (< x (+ (/ (* (- (:x b) (:x a)) (- y (:y a))) (- (:y b) (:y a))) (:x a))))]
          (recur (inc i) i (if crosses (not inside) inside)))))))

(defn layer-range [bp]
  (let [ys (map :y (:layers bp))] {:min (apply min ys) :max (apply max ys)}))

(defn preview-layer-text [max-y top]
  (if (= max-y top) (str " y" max-y " (whole building)") (str " y" max-y)))

(defn face-hover-text [{:keys [cell]}]
  (str "x+" (:x cell) " y" (:y cell) " z+" (:z cell) " · " (:name cell)))

(defn palette-label [{:keys [palette]}]
  (if (= palette "resolved-declared-stock")
    "drag to rotate · allocated from declared stock; site/scaffolds not checked"
    "drag to rotate · illustrative material palette, not inventory selection"))
