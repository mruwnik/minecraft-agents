(ns dashboard.ui.db-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.db :as db]))

(def world
  {:places [{:name "home" :x 0 :z 0} {:name "farm" :x 100 :z 50}]
   :zones []
   :humans []})

(def bodies [{:name "Near" :up true :state {:pos {:x 20 :z 20}}}
             {:name "Far" :up true :state {:pos {:x 5000 :z -4000}}}])

(defn model [] {:canvas {:w 1000 :h 500} :state {:bodies bodies :worlds [world]}})

(deftest bodies-come-from-the-top-level-list-only
  (is (= bodies (db/all-bodies (model))))
  (is (= [] (db/all-bodies {:state {:worlds [{:bodies bodies}]}}))))

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

(deftest outstanding-attention-is-available-from-offline-state
  (let [inbox {"r1" {:request-id "r1"}}
        state {:bodies [{:name "Offline" :up false :outstanding inbox}]}]
    (is (= inbox (db/body-outstanding state "Offline")))
    (is (= {} (db/body-outstanding state "Missing")))))

(deftest home-includes-plans
  (let [m (assoc-in (model) [:plans :items] [{:region {:min [400 60 0] :max [599 70 9]}}])]
    (is (> (visible-blocks (db/effective-view m) 1000) 600))))

;; ---------------------------------------------------------------- show on map
(defn with-show [m name] (assoc m :show-body name))

(deftest focus-body-centres-zooms-and-selects
  (let [m (db/focus-body (model) "Near")
        view (:user-view m)]
    (is (= {:kind :body :name "Near"} (:selected m)))
    (is (>= (:scale view) db/show-scale))
    (is (= [20 20] [(+ (:origin-x view) (/ 1000 2 (:scale view))) (+ (:origin-z view) (/ 500 2 (:scale view)))]))))

(deftest focus-body-keeps-a-closer-zoom
  (let [m (db/focus-body (assoc (model) :user-view {:scale 8 :origin-x 0 :origin-z 0}) "Near")]
    (is (= 8 (:scale (:user-view m))))))

(deftest focus-body-without-a-position-or-canvas-changes-nothing
  (let [no-pos (assoc-in (model) [:state :bodies] [{:name "Lost" :up false}])]
    (are [m name] (= m (db/focus-body m name))
      no-pos "Lost"
      (model) "Nobody"
      (dissoc (model) :canvas) "Near")))

(deftest pending-show-waits-for-state-and-canvas-then-clears
  (let [waiting (with-show (dissoc (model) :canvas) "Near")
        ready (db/apply-pending-show (assoc waiting :canvas {:w 1000 :h 500}))
        no-state (db/apply-pending-show (with-show {:canvas {:w 1000 :h 500}} "Near"))]
    (is (= waiting (db/apply-pending-show waiting)))
    (is (= no-state (with-show {:canvas {:w 1000 :h 500}} "Near")))
    (is (nil? (:show-body ready)))
    (is (= {:kind :body :name "Near"} (:selected ready)))
    (is (= (model) (db/apply-pending-show (model))))))

(deftest initial-db-reads-show-from-the-address
  (are [search expected] (= expected (:show-body (db/initial-db search "who")))
    "" nil
    "?show=Near" "Near"
    "?world=claude&show=Probe_1" "Probe_1")
  (is (= true (:places-open? (db/initial-db "" "who"))))
  (is (= false (:players-open? (db/initial-db "" "who")))))

(def mixed-bodies
  [{:name "Work" :up true :state {:pos {:x 0 :z 0}} :engine {:job {:id "j" :name "dig"}}}
   {:name "Idle" :up true :state {:pos {:x 10 :z 10}} :engine {}}
   {:name "Gone" :up false :engine {}}])

(defn mixed [extra] (merge {:canvas {:w 1000 :h 500} :state {:at 5 :bodies mixed-bodies :worlds [{}]}} extra))

(defn shown-names [m] (mapv :name (db/shown-bodies m)))

(deftest no-status-filter-shows-all-bodies
  (is (= ["Work" "Idle" "Gone"] (shown-names (mixed {}))))
  (is (= ["Work" "Idle" "Gone"] (shown-names (mixed {:status-filter #{}})))))

(deftest a-status-filter-hides-the-other-states
  (are [pressed expected] (= expected (shown-names (mixed {:status-filter pressed})))
    #{:working} ["Work"]
    #{:idle :offline} ["Idle" "Gone"]))

(deftest the-selected-and-detail-bodies-stay-shown
  (are [extra expected] (= expected (shown-names (mixed (assoc extra :status-filter #{:working}))))
    {:selected {:kind :body :name "Gone"}} ["Work" "Gone"]
    {:selected {:kind :place :name "Gone"}} ["Work"]
    {:detail-body "Idle"} ["Work" "Idle"]
    {:selected {:kind :body :name "Gone"} :detail-body "Idle"} ["Work" "Idle" "Gone"]))

(deftest fit-bodies-fits-the-shown-bodies
  (let [all (db/bodies-view (mixed {}))
        only-work (db/bodies-view (mixed {:status-filter #{:working}}))]
    (is (some? only-work))
    (is (> (:scale only-work) (:scale all)))))
