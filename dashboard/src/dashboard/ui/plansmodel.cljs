(ns dashboard.ui.plansmodel
  "Pure rules of the Plans page: the plan tree, the completion bar, the layer grid geometry and cell texts.
  Input is the JSON /api/plans and /api/plan/<id> serve, keywordized."
  (:require [clojure.string :as str]
            [dashboard.blockcolour :as bc]))

(def green "#3fb950")
(def red "#f85149")
(def amber "#d29922")
(def purple "#a371f7")
(def grey "#6e7681")

(def status-colors {"match" green "wrong" red "missing" amber "extra" purple "unknown" grey})
(def status-order ["match" "wrong" "missing" "extra" "unknown"])

(def min-cell 3)
(def max-cell 40)

(defn sort-plans [plans] (vec (sort-by :name plans)))

(defn plan-tree
  "The plans as a flat pre-order list of {:plan p :depth n}: plans nobody nests first (by name), each followed by
  the plans it nests. A plan reached twice (nested by two parents, or a cycle) is listed once."
  [plans]
  (let [by-id (into {} (map (juxt :id identity)) plans)
        nested (set (mapcat :children plans))
        roots (sort-by :name (remove #(contains? nested (:id %)) plans))
        walk (fn walk [seen depth p]
               (if (contains? @seen (:id p))
                 []
                 (do (swap! seen conj (:id p))
                     (into [{:plan p :depth depth}]
                           (mapcat #(when-let [child (by-id %)] (walk seen (inc depth) child)))
                           (:children p)))))
        seen (atom #{})
        walked (vec (mapcat #(walk seen 0 %) roots))
        leftover (remove #(contains? @seen (:id %)) (sort-by :name plans))]
    (into walked (mapcat #(walk seen 0 %)) leftover)))

(defn bar-widths
  "Percent of the bar each status takes; a plan with nothing counted is all unknown."
  [{:keys [total match wrong missing extra unknown]}]
  (if-not (pos? (or total 0))
    {:match 0 :wrong 0 :missing 0 :extra 0 :unknown 100}
    (let [pct #(* 100 (/ (or % 0) total))]
      {:match (pct match) :wrong (pct wrong) :missing (pct missing) :extra (pct extra) :unknown (pct unknown)})))

(defn completion-color
  "One colour for a plan outline: green at least 90% matching, amber at least half, red less, grey when most of it was never seen."
  [{:keys [total match unknown]}]
  (cond
    (not (pos? (or total 0))) grey
    (>= (* 10 (or match 0)) (* 9 total)) green
    (>= (* 2 (or unknown 0)) total) grey
    (>= (* 2 (or match 0)) total) amber
    :else red))

(defn layer-weight [layer]
  (count (remove nil? (apply concat (:rows layer)))))

(defn default-layer-y
  "The layer with the most cells (the first on a tie)."
  [layers]
  (:y (reduce (fn [best l] (if (> (layer-weight l) (layer-weight best)) l best)) (first layers) (rest layers))))

(defn percent-text [{:keys [percent]}] (str (or percent 0) "%"))

(defn cells-text [n] (str n (if (= 1 n) " cell" " cells")))

(defn conflict-text
  "The mark on a plan that conflicts with another active plan; {:with plan :count cells}."
  [{:keys [with count]}]
  (str "conflicts with " with " in " (cells-text count)))

(defn conflicts-label
  "One short line for a plan's conflicts (the map's plan label), nil for none."
  [conflicts]
  (when (seq conflicts)
    (str "conflicts with " (str/join ", " (map (fn [{:keys [with count]}] (str with " (" count ")")) conflicts)))))

(defn pair-text [{[a b] :plans :keys [count]}] (str a " x " b ": " (cells-text count)))

(defn region-text [{[x1 y1 z1] :min [x2 y2 z2] :max}]
  (str x1 ", " y1 ", " z1 "  to  " x2 ", " y2 ", " z2))

(defn region-size [{[x1 y1 z1] :min [x2 y2 z2] :max}]
  (str (inc (- x2 x1)) " x " (inc (- y2 y1)) " x " (inc (- z2 z1))))

(defn counts-text [{:keys [match wrong missing extra unknown total]}]
  (str/join " · " [(str match "/" total " match") (str missing " missing") (str wrong " wrong")
                   (str extra " extra") (str unknown " unknown")]))

(defn world-pos
  "World [x y z] of a grid cell [col row] on layer y."
  [{:keys [min-x min-z]} y [c r]]
  [(+ min-x c) y (+ min-z r)])

(defn cell-text
  "What the tooltip says under the cursor, in every mode: the answer, what the plan wants and what was found (nothing
  was found where the chunk was never dumped); cell is nil where the plan wants nothing."
  [[x y z] cell]
  (let [where (str x " " y " " z)]
    (if-not cell
      (str where ": not part of the plan")
      (let [{:keys [s e a el x]} cell]
        (str where ": " s ", wanted " e (if a (str ", found " a) ", chunk not dumped") " (" el ")"
             (when x ", CONFLICT with another active plan"))))))

;; ---------------------------------------------------------------- modes and views
(def modes ["plan" "world" "diff"])
(def default-mode "diff")
(def default-view :bird)

(defn cell-fill
  "The colour a cell is drawn in under a mode: the plan's block, the block found, or the colour of the answer. nil
  draws nothing (a clear want, air)."
  [mode {:keys [s w a]}]
  (case mode
    "plan" (some-> w bc/block-colour)
    "world" (some-> a bc/block-colour)
    (get status-colors s grey)))

(defn hatched?
  "Cells nobody dumped are marked in world mode."
  [mode {:keys [a]}]
  (and (= "world" mode) (nil? a)))

(def worst-rank {"wrong" 0 "missing" 1 "extra" 2 "unknown" 3 "match" 4})

(defn pick-in-column
  "The cell a column shows from above under a mode; cells are listed top first."
  [mode cells]
  (case mode
    "plan" (or (first (filter :w cells)) (first cells))
    "world" (or (first (filter #(and (:a %) (not (bc/air? (:a %)))) cells)) (first cells))
    (first (sort-by #(get worst-rank (:s %) 5) cells))))

(defn bird-rows
  "The layers seen from straight above, as one grid of cells (each with its :y), by the column rule of the mode (see
  bird-rule). Only the cells the plan has are looked at; a column the plan has nothing in is nil."
  [layers mode]
  (if (empty? layers)
    []
    (let [top-first (sort-by :y > layers)
          rows (count (:rows (first layers)))
          cols (count (first (:rows (first layers))))]
      (vec (for [r (range rows)]
             (vec (for [c (range cols)]
                    (pick-in-column mode (keep (fn [{:keys [y rows]}] (some-> (get-in rows [r c]) (assoc :y y))) top-first)))))))))

(defn bird-rule [mode]
  (case mode
    "plan" "Bird's eye, plan: each column shows its topmost wanted block (clear cells never cover one)."
    "world" "Bird's eye, world: each column shows the topmost block found within the plan's own cells of that column (air skipped)."
    "Bird's eye, diff: each column shows its worst answer: wrong over missing over extra over unknown over match, so a problem under a roof stays visible."))

(defn age-text [ms]
  (cond
    (< ms 1000) "just now"
    (< ms 60000) (str (quot ms 1000) " s ago")
    (< ms 3600000) (str (quot ms 60000) " min ago")
    (< ms 86400000) (str (quot ms 3600000) " h ago")
    :else (str (quot ms 86400000) " d ago")))

(defn checked-text
  "When the world under the plan was last seen: the age of the oldest and newest chunk dump ({:chunks :dumped :oldest
  :newest :now}, times in ms)."
  [{:keys [chunks dumped oldest newest now] :as checked}]
  (cond
    (nil? checked) "last check unknown"
    (zero? (or dumped 0)) "no chunk of this plan was ever dumped"
    :else (let [new-age (age-text (- now newest))
                old-age (age-text (- now oldest))]
            (str "checked " (if (= new-age old-age) new-age (str new-age " to " old-age))
                 (when (< dumped chunks) (str ", " dumped " of " chunks " chunks dumped"))))))

(defn cell-size [cols rows width height]
  (if-not (and (pos? cols) (pos? rows))
    max-cell
    (-> (js/Math.floor (min (/ width cols) (/ height rows)))
        (max min-cell)
        (min max-cell))))

(defn cell-at
  "[col row] under a pixel of the grid, or nil outside it."
  [size cols rows x y]
  (let [c (js/Math.floor (/ x size)) r (js/Math.floor (/ y size))]
    (when (and (<= 0 c) (< c cols) (<= 0 r) (< r rows))
      [c r])))

(defn crop-grid
  "The rows and grid geometry limited to {:min [x y z] :max [x y z]} (clamped to the grid); the whole grid for no bounds."
  [rows {:keys [min-x min-z cols] :as grid} bounds]
  (if-not bounds
    {:rows rows :grid grid}
    (let [{[x1 _ z1] :min [x2 _ z2] :max} bounds
          c1 (max 0 (- x1 min-x)) c2 (min (dec cols) (- x2 min-x))
          r1 (max 0 (- z1 min-z)) r2 (min (dec (count rows)) (- z2 min-z))]
      (if (or (> c1 c2) (> r1 r2))
        {:rows [] :grid grid}
        {:rows (mapv #(subvec (vec %) c1 (inc c2)) (subvec (vec rows) r1 (inc r2)))
         :grid {:min-x (+ min-x c1) :min-z (+ min-z r1) :cols (inc (- c2 c1)) :rows (inc (- r2 r1))}}))))

(defn dimmed?
  "With an element selected, the cells of other elements fade."
  [selected-element cell]
  (boolean (and selected-element cell (not= selected-element (:el cell)))))

(defn element-rows
  "The elements table: each element with a label for what it is."
  [{:keys [elements]}]
  (mapv (fn [{:keys [id kind content counts error at ref bounds] n :count}]
          {:id id :kind kind :bounds bounds :content content :counts counts :cells n :error error
           :ref ref :where (when at (str/join ", " at))})
        elements))

(defn keyword-text [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn spot-rows
  "The spots of a plan (name -> [x y z]) as table rows sorted by name."
  [spots]
  (->> spots
       (map (fn [[k [x y z]]] {:name (keyword-text k) :pos (str x ", " y ", " z)}))
       (sort-by :name)
       vec))

(defn assign-rows
  "The assignments of a plan as table rows: the spot or part, who takes it (a body, or a villager's profession and
  trade), what for, and the answer."
  [assign]
  (mapv (fn [{:keys [spot body profession trade use answer]}]
          {:spot spot
           :who (or body (when profession (str (keyword-text profession) (when trade (str " (" trade ")")))) "")
           :use (if use (keyword-text use) "")
           :answer (when answer (keyword-text answer))})
        assign))
