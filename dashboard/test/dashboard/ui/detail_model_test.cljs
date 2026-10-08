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
  (let [mdl (m/detail-model online now "wait" "dashboard-k3x9ab")]
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
      :iframe-src "/view?agent=w1/Bob&embed=1&who=dashboard-k3x9ab"
      :offline-text nil)))

(deftest offline-model
  (let [mdl (m/detail-model offline now nil "dashboard-k3x9ab")]
    (are [k expected] (= expected (k mdl))
      :online? false
      :status :offline
      :last-seen "41m ago"
      :iframe-src nil)
    (is (re-find #"^offline since \d\d:\d\d:\d\d \(41m ago\)$" (:offline-text mdl)))))

(deftest missing-body
  (are [k expected] (= expected (k (m/detail-model nil now nil "dashboard-k3x9ab")))
    :name nil
    :online? false))

(deftest no-pos
  (is (= "-" (:pos-text (m/detail-model (assoc online :view nil :engine {:up true}) now nil "me")))))

(deftest embed-css-hides-the-view-chrome
  (are [stats? expected-hidden] (= expected-hidden (m/hidden-selectors stats?))
    false ["#overlay" "#bar" "#drive-banner"]
    true ["#bar" "#drive-banner"])
  (are [stats? expected] (= expected (m/embed-css stats?))
    false "#overlay, #bar, #drive-banner { display: none !important; }"
    true "#bar, #drive-banner { display: none !important; }"))

(deftest attention-note-cases
  (are [error online? expected] (= expected (m/attention-note error online?))
    nil true nil
    nil false nil
    "engine event service unavailable: ENOENT" true {:kind :error :text "engine event service unavailable: ENOENT"}
    "engine event service unavailable: ENOENT" false {:kind :offline :text "offline: outstanding requests cannot be read"}
    "http 500" false {:kind :offline :text "offline: outstanding requests cannot be read"}))

(deftest iframe-src-encodes-world-and-name
  (let [odd (assoc online :name "a&b#c" :world "w 1")]
    (is (= "/view?agent=w%201/a%26b%23c&embed=1&who=me" (:iframe-src (m/detail-model odd now nil "me"))))))

(deftest the-goal-wait-reaches-the-model
  (let [mdl (m/detail-model (assoc online :goal {:text "t" :wait "waiting: x" :since (- now 1000)}) now "wait" "k")]
    (is (= "waiting: x" (:goal-wait mdl)))))
