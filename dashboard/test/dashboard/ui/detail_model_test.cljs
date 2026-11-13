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
      :iframe-src "/view?agent=w1/Bob&embed=1"
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

(deftest embed-css-hides-the-view-chrome
  (are [stats? expected-hidden] (= expected-hidden (m/hidden-selectors stats?))
    false ["#overlay" "#bar"]
    true ["#bar"])
  (are [stats? expected] (= expected (m/embed-css stats?))
    false "#overlay, #bar { display: none !important; }"
    true "#bar { display: none !important; }"))

(deftest attention-note-cases
  (are [error online? expected] (= expected (m/attention-note error online?))
    nil true nil
    nil false nil
    "engine event service unavailable: ENOENT" true {:kind :error :text "engine event service unavailable: ENOENT"}
    "engine event service unavailable: ENOENT" false {:kind :offline :text "offline: outstanding requests cannot be read"}
    "http 500" false {:kind :offline :text "offline: outstanding requests cannot be read"}))

(deftest iframe-src-encodes-world-and-name
  (let [odd (assoc online :name "a&b#c" :world "w 1")]
    (is (= "/view?agent=w%201/a%26b%23c&embed=1" (:iframe-src (m/detail-model odd now nil))))))

(deftest the-goal-wait-reaches-the-model
  (let [mdl (m/detail-model (assoc online :goal {:text "t" :wait "waiting: x" :since (- now 1000)}) now "wait")]
    (is (= "waiting: x" (:goal-wait mdl)))))

(deftest takeover-lines-name-every-pid-and-flag-an-unclean-kill
  (let [p {:pid 7 :role :body :argv ["node" "out/body-release.cjs"]}]
    (are [state expected] (= expected (m/takeover-lines state))
      {:pending? true} {:kind :pending :lines ["killing..."]}
      {:error "http 403"} {:kind :error :lines ["takeover failed: http 403"]}
      {:result {:killed [] :ok? true}} {:kind :ok :lines ["no process of this body was running"]}
      {:result {:killed [p] :ok? true :launcher {:pid 3 :argv ["bash"]}}}
      {:kind :ok :lines ["killed 1, all gone" "killed 7 (body): node out/body-release.cjs" "started from 3: bash (left running)"]}
      {:result {:killed [p] :respawned [(assoc p :pid 8)] :survivors [p] :ok? false}}
      {:kind :error :lines ["NOT CLEAN: see below" "killed 7 (body): node out/body-release.cjs"
                            "respawned and killed 8 (body): node out/body-release.cjs" "STILL ALIVE 7 (body): node out/body-release.cjs"]})))
