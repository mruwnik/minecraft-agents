(ns dashboard.plan-api
  "What /api/plans and /api/plan/<id> answer: the plans of a world compared with the dumped blocks. Plain data in and
  out (the server supplies the plan directory, the blueprint directory and the block lookup (block-at x y z) -> a JS
  {name, state: {property value}} or nil where no column was dumped)."
  (:require [dashboard.plan :as plan]
            [dashboard.plan-compare :as cmp]
            [plan.shape :as shape]))

(defn world-blocks
  "The shape's block lookup over the JS one: [x y z] -> {:name n :state {\"property\" value}}, nil where nothing was dumped."
  [block-at]
  (fn [[x y z]]
    (when-let [block (block-at x y z)]
      {:name (.-name block) :state (js->clj (.-state block))})))

(defn compare-one [p blueprints block-at]
  (let [expansion (shape/expand p blueprints)]
    (assoc (cmp/compare-plan expansion (world-blocks block-at))
           :region (cmp/bounds (:cells expansion))
           :spots (:spots expansion))))

(defn header [id p]
  {:id id :name id :status (:status p) :note (:note p) :children []})

(defn read-all [{:keys [dir blueprint-dir]}]
  (assoc (plan/read-dir dir) :blueprints (:blueprints (plan/read-blueprints blueprint-dir))))

(defn list-item [blueprints block-at [id p]]
  (let [{:keys [counts elements region]} (compare-one p blueprints block-at)]
    (assoc (header id p)
           :region region
           :counts counts
           :percent (:percent counts)
           :elements (mapv #(select-keys % [:id :kind :bounds :counts]) elements))))

(defn summaries
  "{:plans [...] :errors [{:file :errors}]}: every readable plan with its totals and the bounds of its parts."
  [{:keys [block-at] :as opts}]
  (let [{:keys [plans errors blueprints]} (read-all opts)]
    {:plans (mapv #(list-item blueprints block-at %) (sort-by key plans))
     :errors errors}))

(defn detail
  "The full comparison of one plan (parts as elements with content text, layers, grid, spots, assignments, errors),
  nil for an unknown id."
  [{:keys [block-at] :as opts} id]
  (let [{:keys [plans blueprints]} (read-all opts)]
    (when-let [p (get plans id)]
      (let [result (compare-one p blueprints block-at)]
        (merge (header id p)
               (select-keys result [:region :counts :elements :layers :grid :errors :spots])
               {:percent (get-in result [:counts :percent])
                :assign (shape/assignment-answers p)})))))
