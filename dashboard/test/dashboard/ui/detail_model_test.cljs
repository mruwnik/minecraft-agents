(ns dashboard.ui.detail-model-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.detail-model :as m]))

(def now 1000000)

(def online
  {:name "Bob" :up true :world "w1" :at (- now 2000)
   :engine {:up true :age-ms 2000 :at (- now 2000) :job {:id "j1" :name "jobs.gather"}
            :jobs [{:id "j1" :label "gather" :current? true :round 3}] :reflexes [{:id "r1" :job "flee"}]}
   :view {:poseMtimeMs 5 :dimension "overworld" :pos {:x 10.7 :y 64 :z -3.2}
          :hud {:health 18 :food 20 :inventory [] :held nil :effects []}}})

(def offline (-> online (assoc :up false) (assoc-in [:engine :up] false) (assoc :at (- now 2460000)) (assoc-in [:engine :age-ms] 2460000)))

(deftest model-fields
  (let [mdl (m/detail-model online now "wait")]
    (are [k expected] (= expected (k mdl))
      :name "Bob"
      :status :working
      :online? true
      :job "gather, round 3"
      :action "wait"
      :pos-text "10, 64, -4"
      :dimension "overworld"
      :world "w1"
      :last-seen "2s ago"
      :iframe-src "/view?agent=Bob&embed=1&who=dashboard"
      :offline-text nil)))

(deftest offline-model
  (let [mdl (m/detail-model offline now nil)]
    (are [k expected] (= expected (k mdl))
      :online? false
      :status :offline
      :last-seen "41m ago"
      :iframe-src nil)
    (is (re-find #"^offline since \d\d:\d\d:\d\d \(41m ago\)$" (:offline-text mdl)))))

(deftest missing-body
  (are [k expected] (= expected (k (m/detail-model nil now nil)))
    :name nil
    :online? false))

(deftest no-pos
  (is (= "-" (:pos-text (m/detail-model (assoc online :view nil :engine {:up true}) now nil)))))
