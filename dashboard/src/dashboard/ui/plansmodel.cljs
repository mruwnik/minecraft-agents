(ns dashboard.ui.plansmodel
  "Pure rules of the Plans page: ordering, the completion bar, the layer grid geometry and cell texts."
  (:require [clojure.string :as str]))

(def green "#3fb950")
(def red "#f85149")
(def amber "#d29922")
(def grey "#6e7681")

(def status-colors {"match" green "wrong" red "missing" amber "unknown" grey})

(def min-cell 6)
(def max-cell 40)

(defn sort-plans [plans] (vec (sort-by :name plans)))

(defn bar-widths
  "Percent of the bar each status takes; a plan with nothing counted is all unknown."
  [{:keys [total match wrong missing unknown]}]
  (if-not (pos? (or total 0))
    {:match 0 :wrong 0 :missing 0 :unknown 100}
    (let [pct #(* 100 (/ (or % 0) total))]
      {:match (pct match) :wrong (pct wrong) :missing (pct missing) :unknown (pct unknown)})))

(defn completion-color
  "One colour for a plan outline: green done, amber at least half done, red less, grey when most of it was never seen."
  [{:keys [total match unknown] :as summary}]
  (cond
    (not (pos? (or total 0))) grey
    (= match total) green
    (>= (* 2 (or unknown 0)) total) grey
    (>= (* 2 (or match 0)) total) amber
    :else red))

(defn layer-weight [layer]
  (count (remove #(= "free" (:status %)) (apply concat (:rows layer)))))

(defn default-layer-y
  "The layer with the most constrained cells (the first on a tie)."
  [layers]
  (:y (reduce (fn [best l] (if (> (layer-weight l) (layer-weight best)) l best)) (first layers) (rest layers))))

(defn world-y [place-y layer-y] (+ place-y layer-y))

(defn bill-rows [bill]
  (->> bill
       (map (fn [[k n]] [(name k) n]))
       (sort-by (fn [[item n]] [(- n) item]))
       vec))

(defn cell-text [{:keys [status expected actual]}]
  (case status
    "free" "not part of the plan"
    "match" (str "match: " actual)
    "unknown" (str "unknown: wanted " expected ", chunk not dumped")
    (str status ": wanted " expected ", found " actual)))

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

(defn shown-char
  "The legend character drawn in a cell: free cells draw none, private-use legend keys a dot."
  [ch]
  (cond
    (= "_" ch) ""
    (re-find #"[-]" (str ch)) "."
    :else (str ch)))

(defn percent-text [{:keys [percent]}] (str (or percent 0) "%"))

(defn bounds-text [{:keys [x1 y1 z1 x2 y2 z2]}]
  (str x1 ", " y1 ", " z1 "  to  " x2 ", " y2 ", " z2))

(defn counts-text [{:keys [match wrong missing unknown total]}]
  (str/join " · " [(str match "/" total " match") (str wrong " wrong") (str missing " missing") (str unknown " unknown")]))
