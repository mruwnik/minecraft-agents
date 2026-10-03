(ns dashboard.plan-api
  "What /api/plans and /api/plan/<id> answer: the plans of a world compared with the dumped blocks. Plain data in and
  out (the server supplies the directory, the blueprint lookup and the block lookup)."
  (:require [dashboard.plan :as plan]
            [dashboard.plan-compare :as cmp]
            [plan.shape :as shape]))

(defn compare-one [plans id blueprint-fn block-at]
  (cmp/compare-plan (shape/expand-plan plans id blueprint-fn) block-at))

(defn header [id p]
  {:id id :name (or (:name p) id) :owner (:owner p) :kind (:kind p) :status (or (:status p) :proposed)
   :region (:region p) :note (:note p) :children (shape/children p)})

(defn list-item [plans blueprint-fn block-at [id p]]
  (let [{:keys [counts elements]} (compare-one plans id blueprint-fn block-at)]
    (assoc (header id p)
           :counts counts
           :percent (:percent counts)
           :elements (mapv #(select-keys % [:id :kind :bounds :counts]) elements))))

(defn summaries
  "{:plans [...] :errors [{:file :errors}]}: every readable plan with its totals and the bounds of its elements."
  [{:keys [dir blueprint-fn block-at]}]
  (let [{:keys [plans errors]} (plan/read-dir dir)]
    {:plans (mapv #(list-item plans blueprint-fn block-at %) (sort-by key plans))
     :errors errors}))

(defn detail
  "The full comparison of one plan (elements with content text, layers, grid, errors), nil for an unknown id."
  [{:keys [dir blueprint-fn block-at]} id]
  (let [{:keys [plans]} (plan/read-dir dir)]
    (when-let [p (get plans id)]
      (let [result (compare-one plans id blueprint-fn block-at)]
        (assoc (header id p)
               :counts (:counts result) :percent (get-in result [:counts :percent])
               :elements (:elements result) :layers (:layers result) :grid (:grid result) :errors (:errors result))))))
