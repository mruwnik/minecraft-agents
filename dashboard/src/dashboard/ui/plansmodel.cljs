(ns dashboard.ui.plansmodel
  "Pure rules of the Plans page: the plan tree, the completion bar, the layer grid geometry and cell texts.
  Input is the JSON /api/plans and /api/plan/<id> serve, keywordized."
  (:require [clojure.string :as str]))

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
  "What the tooltip says under the cursor; cell is nil where the plan wants nothing."
  [[x y z] cell]
  (let [where (str x " " y " " z)]
    (if-not cell
      (str where ": not part of the plan")
      (let [{:keys [s e a el]} cell]
        (case s
          "match" (str where ": match, " a " (" el ")")
          "unknown" (str where ": unknown, wanted " e ", chunk not dumped (" el ")")
          (str where ": " s ", wanted " e ", found " a " (" el ")"))))))

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
