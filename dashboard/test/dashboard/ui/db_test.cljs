(ns dashboard.ui.db-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.db :as db]))

(def world
  {:places [{:name "home" :x 0 :z 0} {:name "farm" :x 100 :z 50}]
   :zones []
   :humans []
   :bodies [{:name "Near" :up true :state {:pos {:x 20 :z 20}}}
            {:name "Far" :up true :state {:pos {:x 5000 :z -4000}}}]})

(defn model [] {:canvas {:w 1000 :h 500} :state {:worlds [world]}})

(defn visible-blocks [view w]
  (/ w (:scale view)))

(deftest default-view-is-the-home-cluster
  (let [home (db/effective-view (model))
        all (db/all-view (model))]
    (is (< (visible-blocks home 1000) 400))
    (is (> (visible-blocks all 1000) 5000))))

(deftest home-falls-back-to-everything-without-places
  (let [m (assoc-in (model) [:state :worlds 0 :places] [])]
    (is (> (visible-blocks (db/effective-view m) 1000) 5000))))

(deftest user-view-wins
  (is (= {:scale 2} (db/effective-view (assoc (model) :user-view {:scale 2})))))

(deftest home-includes-plans
  (let [m (assoc-in (model) [:plans :items] [{:bounds {:x1 400 :z1 0 :x2 600 :z2 10}}])]
    (is (> (visible-blocks (db/effective-view m) 1000) 600))))
