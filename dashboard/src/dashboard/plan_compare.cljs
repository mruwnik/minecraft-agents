(ns dashboard.plan-compare
  "Judges the cells a plan expands to (plan.shape/expand) against the blocks in the world (plan.shape/plan-minus-world),
  and counts and lays out the answers. Pure: the world comes in as (block-at [x y z]) -> {:name n} or nil when no chunk
  column was dumped there."
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

(defn bounds
  "{:min [x y z] :max [x y z]} of cells, nil for none."
  [cells]
  (when (seq cells)
    (let [ps (map :pos cells)
          axis (fn [i f] (apply f (map #(nth % i) ps)))]
      {:min [(axis 0 min) (axis 1 min) (axis 2 min)]
       :max [(axis 0 max) (axis 1 max) (axis 2 max)]})))

(defn grid-cell [{:keys [status want found part]}]
  {:s (name status) :e (shape/want-text want) :w (shape/want-block want) :a found :el part})

(defn chunk-of [[x _ z]] [(bit-shift-right x 4) (bit-shift-right z 4)])

(defn checked
  "When the world was last seen under the cells: the chunk columns they stand in, how many of those were dumped, and
  the oldest and newest dump time (mtime-of cx cz -> ms or nil when not dumped)."
  [cells mtime-of]
  (let [chunks (distinct (map (comp chunk-of :pos) cells))
        times (keep (fn [[cx cz]] (mtime-of cx cz)) chunks)]
    {:chunks (count chunks) :dumped (count times)
     :oldest (when (seq times) (apply min times)) :newest (when (seq times) (apply max times))}))

(def stale-ms "A chunk dump older than this is reported stale by a plan check." 30000)

(defn stale-note
  "A short line when the oldest dump under the cells is older than limit-ms, naming its age; nil otherwise (or when no
  chunk was dumped). checked is the map of `checked` plus :now."
  [{:keys [oldest now]} limit-ms]
  (when (and oldest (> (- now oldest) limit-ms))
    (str "stale: oldest chunk dump is " (quot (- now oldest) 1000) " s old (limit " (quot limit-ms 1000)
         " s); the body re-dumps columns it changes within seconds, so check again shortly")))

(defn layers
  "One top-down grid per y that holds cells: {:y y :rows [[cell-or-nil ...]]} over the x/z bounds of all the cells
  ([:min-x :min-z :cols :rows] in the second value)."
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

(defn content-text
  "A short summary of a part for people."
  [{:keys [where want blueprint turn]}]
  (if (= :blueprint where)
    (str "blueprint " blueprint ", turn " turn)
    (shape/want-text want)))

(defn element
  "A part as the Plans page lists it."
  [{:keys [id where at turn error] n :count :as part}]
  (cond-> {:id id :kind where :content (content-text part) :count n}
    at (assoc :at at :rotation turn)
    error (assoc :error error)))

(defn compare-plan
  "expansion = what plan.shape/expand returned. -> {:counts :elements :layers :grid :errors}: each part (as an element)
  with its own counts and bounds, the plan's counts over all its cells, the per-layer grids."
  [{:keys [cells parts errors]} block-at]
  (let [judged (mapv #(assoc % :status (:answer %)) (shape/plan-minus-world cells block-at))
        by-part (group-by :part judged)
        [layer-grids grid] (layers judged)]
    {:counts (counts judged)
     :elements (mapv (fn [{:keys [id] :as part}]
                       (let [own (get by-part id [])]
                         (assoc (element part) :counts (counts own) :bounds (bounds own))))
                     parts)
     :layers (or layer-grids [])
     :grid grid
     :errors (mapv (fn [{:keys [part assign error]}] {:element (or part assign "plan") :error error}) errors)}))
