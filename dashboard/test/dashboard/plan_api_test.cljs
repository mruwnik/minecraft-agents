(ns dashboard.plan-api-test
  (:require ["path" :as path]
            [cljs.test :refer [deftest are is]]
            [dashboard.plan-api :as api]))

(def dir (.resolve path js/__dirname ".." "test" "fixtures" "plans"))

(def blueprints {"villager-house-10" [{:dx 0 :dy 0 :dz 0 :names ["cobblestone"]} {:dx 1 :dy 0 :dz 0 :air true}]
                 "watchtower" [{:dx 0 :dy 0 :dz 0 :names ["cobblestone"]}]})

(defn opts [block-at] {:dir dir :blueprint-fn blueprints :block-at block-at})

(def wheat-everywhere (fn [_ _ _] "wheat"))
(def nothing-dumped (fn [_ _ _] nil))
(defn by-id [items id] (first (filter #(= id (:id %)) items)))

(deftest summaries-list-every-plan-with-totals
  (let [{:keys [plans errors]} (api/summaries (opts nothing-dumped))
        farm (by-id plans "jizo-farm")]
    (is (= [] errors))
    (is (= ["claude-village" "jizo-farm" "spawn-clear"] (map :id plans)))
    (are [path expected] (= expected (get-in farm path))
      [:name] "Jizo's farm"
      [:owner] "Jizo"
      [:status] :active
      [:children] []
      [:counts :match] 0
      [:counts :unknown] (get-in farm [:counts :total])
      [:percent] 0
      [:region :min] [-17 62 -97]
      [:elements 0 :id] "potatoes"
      [:elements 0 :bounds] {:min [-11 63 -97] :max [-10 63 -96]})))

(deftest a-village-rolls-up-its-child-farm
  (let [plans (:plans (api/summaries (opts wheat-everywhere)))
        village (by-id plans "claude-village")
        farm (by-id plans "jizo-farm")]
    (is (= ["jizo-farm"] (:children village)))
    (is (= (get-in farm [:counts :total])
           (get-in (by-id (:elements village) "farm") [:counts :total])))
    (is (= (+ (get-in farm [:counts :total]) 2 1 23) (get-in village [:counts :total])))))

(deftest detail-has-elements-layers-and-errors
  (let [d (api/detail (opts wheat-everywhere) "jizo-farm")]
    (are [path expected] (= expected (get-in d path))
      [:id] "jizo-farm"
      [:elements 0 :content] "crop potatoes"
      [:elements 0 :counts :match] 0
      [:elements 0 :counts :wrong] 4
      [:elements 8 :id] "wheat-a"
      [:elements 8 :counts :percent] 100
      [:errors] []
      [:grid :min-x] -17
      [:grid :cols] 32
      [:layers 0 :y] 62
      [:layers 1 :y] 63)))

(deftest detail-of-a-village-lists-unresolved-blueprints
  (let [d (api/detail {:dir dir :blueprint-fn {} :block-at nothing-dumped} "claude-village")]
    (is (= ["hall" "tower"] (map :element (:errors d))))))

(deftest detail-of-an-unknown-plan-is-nil
  (is (nil? (api/detail (opts nothing-dumped) "nope"))))
