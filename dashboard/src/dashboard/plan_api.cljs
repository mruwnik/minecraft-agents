(ns dashboard.plan-api
  "What /api/plans and /api/plan/<id> answer: the plans of a world compared with the dumped blocks. Plain data in and
  out (the server supplies the plan directory, the blueprint directory and the block lookup (block-at x y z) -> a JS
  {name, state: {property value}} or nil where no column was dumped)."
  (:require [dashboard.plan :as plan]
            [dashboard.plan-compare :as cmp]
            [plan.conflicts :as conflicts]
            [plan.shape :as shape]))

(def max-conflict-cells "Conflicting cells sent per pair of plans; :count says how many there are." 3000)

(defn world-blocks
  "The shape's block lookup over the JS one: [x y z] -> {:name n :state {\"property\" value}}, nil where nothing was dumped."
  [block-at]
  (fn [[x y z]]
    (when-let [block (block-at x y z)]
      {:name (.-name block) :state (js->clj (.-state block))})))

(defn compare-one
  "mtime-of (cx cz -> ms or nil), when given, adds :checked: the age of the chunk dumps under the plan."
  ([p blueprints block-at] (compare-one p blueprints block-at nil))
  ([p blueprints block-at mtime-of]
   (let [expansion (shape/expand p blueprints)]
     (cond-> (assoc (cmp/compare-plan expansion (world-blocks block-at))
                    :region (cmp/bounds (:cells expansion))
                    :spots (:spots expansion))
       mtime-of (assoc :checked (assoc (cmp/checked (:cells expansion) mtime-of) :now (js/Date.now)))))))

(defn header [id p]
  (merge {:id id :name id :status (:status p) :note (:note p) :children []}
         (select-keys p [:kind :at])
         (when (:metadata p) {:metadata (select-keys (:metadata p) [:geometry])})))

(defn read-all [{:keys [dir blueprint-dir]}]
  (assoc (plan/read-dir dir) :blueprints (:blueprints (plan/read-blueprints blueprint-dir))))

(defn capped
  "A conflict pair with at most limit of its cells and :shown, how many came."
  [limit pair]
  (let [cells (vec (take limit (:cells pair)))]
    (assoc pair :cells cells :shown (count cells))))

(defn flag-conflicts
  "The layers of a plan's grid with :x true on the cells in conflict (a set of [x y z])."
  [layers {:keys [min-x min-z]} cells]
  (if (empty? cells)
    layers
    (mapv (fn [{:keys [y] :as layer}]
            (update layer :rows
                    (fn [rows]
                      (vec (map-indexed (fn [r row]
                                          (vec (map-indexed (fn [c cell]
                                                              (cond-> cell
                                                                (and cell (contains? cells [(+ min-x c) y (+ min-z r)])) (assoc :x true)))
                                                            row)))
                                        rows)))))
          layers)))

(defn list-item [blueprints block-at per-plan [id p]]
  (let [{:keys [counts elements region]} (compare-one p blueprints block-at)]
    (assoc (header id p)
           :conflicts (get per-plan id [])
           :region region
           :counts counts
           :percent (:percent counts)
           :elements (mapv #(select-keys % [:id :kind :bounds :counts]) elements))))

(defn summaries
  "{:plans [...] :errors [{:file :errors}] :conflicts [pair ...]}: every readable plan with its totals, the bounds of its
  parts and its :conflicts, and every pair of active plans wanting different things of a cell (plan.conflicts, the cells
  capped)."
  [{:keys [block-at] :as opts}]
  (let [{:keys [plans errors blueprints]} (read-all opts)
        pairs (conflicts/active-conflicts plans blueprints)
        per-plan (conflicts/per-plan pairs)]
    {:plans (mapv #(list-item blueprints block-at per-plan %) (sort-by key plans))
     :errors errors
     :conflicts (mapv #(capped max-conflict-cells %) pairs)}))

(defn detail
  "The full comparison of one plan (parts as elements with content text, layers, grid, spots, assignments, errors),
  nil for an unknown id. :column-mtime (cx cz -> ms or nil) adds :checked. :conflicts lists the active plans this one
  conflicts with, and the cells in conflict are flagged :x in the layers."
  [{:keys [block-at column-mtime] :as opts} id]
  (let [{:keys [plans blueprints]} (read-all opts)]
    (when-let [p (get plans id)]
      (let [result (compare-one p blueprints block-at column-mtime)
            pairs (conflicts/active-conflicts plans blueprints)
            mine (filter #(some #{id} (:plans %)) pairs)]
        (merge (header id p)
               (select-keys p [:metadata])
               (select-keys result [:region :counts :elements :errors :spots :checked])
               {:layers (flag-conflicts (:layers result) (:grid result) (into #{} (mapcat :cells) mine))
                :grid (:grid result)
                :conflicts (get (conflicts/per-plan pairs) id [])
                :percent (get-in result [:counts :percent])
                :assign (shape/assignment-answers p)})))))
