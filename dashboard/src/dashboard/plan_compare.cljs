(ns dashboard.plan-compare
  "Judges the cells a plan expands to (plan.shape/expand-plan) against the blocks actually in the world, and counts and
  lays out the statuses (plan.shape/judge decides each one). Pure: the world comes in as (block-at x y z) -> block name,
  or nil when no chunk column was dumped there."
  (:require [plan.shape :as shape]))

(def zero-counts {:match 0 :missing 0 :wrong 0 :extra 0 :unknown 0 :total 0 :percent 0})

(defn counts
  "{:match :missing :wrong :extra :unknown :total :percent} of judged cells (each with a :status); percent is
  match / total (unknown cells count in the total), 0 for no cells."
  [cells]
  (let [freq (frequencies (map :status cells))
        total (count cells)
        match (get freq :match 0)]
    (merge zero-counts
           (select-keys freq [:match :missing :wrong :extra :unknown])
           {:total total :percent (if (pos? total) (js/Math.round (* 100 (/ match total))) 0)})))

(defn judge-cell [block-at {[x y z] :pos want :want :as cell}]
  (let [actual (block-at x y z)]
    (assoc cell :actual actual :status (shape/judge want actual))))

(defn bounds
  "{:min [x y z] :max [x y z]} of cells, nil for none."
  [cells]
  (when (seq cells)
    (let [ps (map :pos cells)
          axis (fn [i f] (apply f (map #(nth % i) ps)))]
      {:min [(axis 0 min) (axis 1 min) (axis 2 min)]
       :max [(axis 0 max) (axis 1 max) (axis 2 max)]})))

(defn grid-cell [{:keys [status want actual element]}]
  {:s (name status) :e (shape/want-text want) :a actual :el element})

(defn layers
  "One top-down grid per y that holds cells: {:y y :rows [[cell-or-nil ...]]} over the x/z bounds of all the cells
  ([:min-x :min-z :cols :rows] in the second value). A cell wanted twice shows the later element's."
  [judged]
  (let [{[x1 _ z1] :min [x2 _ z2] :max} (bounds judged)]
    (when x1
      (let [cols (inc (- x2 x1)) rows (inc (- z2 z1))
            by-y (group-by (comp second :pos) judged)]
        [(vec (for [y (sort (keys by-y))
                    :let [at (into {} (map (fn [c] [[(first (:pos c)) (nth (:pos c) 2)] (grid-cell c)])) (get by-y y))]]
                {:y y
                 :rows (vec (for [z (range z1 (inc z2))]
                              (vec (for [x (range x1 (inc x2))] (get at [x z])))))}))
         {:min-x x1 :min-z z1 :cols cols :rows rows}]))))

(defn compare-plan
  "expansion = what dashboard.plan/expand returned. -> {:counts :elements :layers :grid :errors}: each element with its
  own counts and bounds, the plan's counts over all its cells (children included), the per-layer grids."
  [{:keys [cells elements errors]} block-at]
  (let [judged (mapv #(judge-cell block-at %) cells)
        by-element (group-by :element judged)
        [layer-grids grid] (layers judged)]
    {:counts (counts judged)
     :elements (mapv (fn [{:keys [id] :as el}]
                       (let [own (get by-element id [])]
                         (-> (dissoc el :cells)
                             (assoc :counts (counts own) :bounds (bounds own)))))
                     elements)
     :layers (or layer-grids [])
     :grid grid
     :errors (or errors [])}))
