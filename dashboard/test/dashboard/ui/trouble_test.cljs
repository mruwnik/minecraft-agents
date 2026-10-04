(ns dashboard.ui.trouble-test
  (:require [cljs.test :refer [deftest is testing are]]
            [dashboard.ui.trouble :as t]))

(def now 1000000)

(defn body
  ([] (body {}))
  ([{:keys [signals hud job up] :or {up true}}]
   {:name "A" :up up :engine {:signals signals :job job} :view {:hud hud}}))

(deftest reasons-table
  (doseq [[title b expected]
          [["calm" (body) []]
           ["hurt just now" (body {:signals {:hurt-t (- now 3000)}}) [{:severity :warn :text "hurt 3s ago"}]]
           ["hurt a while ago" (body {:signals {:hurt-t (- now 31000)}}) []]
           ["died 2 minutes ago" (body {:signals {:died-t (- now 120000)}}) [{:severity :danger :text "died 2m ago"}]]
           ["died long ago" (body {:signals {:died-t (- now 301000)}}) []]
           ["a job in backoff" (body {:signals {:backoffs {"go-to" (- now 10000)}}}) [{:severity :warn :text "backoff: go-to"}]]
           ["backoff keys mixing symbols and keywords" (body {:signals {:backoffs {'jobs.gather.mine 1 :suffocating 2 "go-to" 3}}}) [{:severity :warn :text "backoff: :suffocating, go-to, jobs.gather.mine"}]]
           ["several backoffs list all" (body {:signals {:backoffs {"a" 1 "b" 2}}}) [{:severity :warn :text "backoff: a, b"}]]
           ["stuck reflex still open" (body {:signals {:stuck-open? true :stuck-t (- now 900000)}}) [{:severity :warn :text "stuck"}]]
           ["unstick gave up recently" (body {:signals {:stuck-t (- now 100000)}}) [{:severity :warn :text "stuck"}]]
           ["unstick gave up long ago" (body {:signals {:stuck-t (- now 400000)}}) []]
           ["low health" (body {:hud {:health 6 :food 20}}) [{:severity :danger :text "health 6"}]]
           ["health just above" (body {:hud {:health 7 :food 20}}) []]
           ["fractional health rounds up for display" (body {:hud {:health 5.5 :food 20}}) [{:severity :danger :text "health 6"}]]
           ["low food" (body {:hud {:health 20 :food 6}}) [{:severity :warn :text "food 6"}]]
           ["manual takeover is not a trouble" (body {:signals {:takeover? true}}) []]
           ["offline bodies are never in trouble" (body {:up false :signals {:takeover? true :died-t now}}) []]
           ["no hud, no signals" {:name "A" :up true :engine {}} []]
           ["order: died, health, hurt, backoff, stuck, food"
            (body {:signals {:takeover? true :died-t now :hurt-t now :backoffs {"x" 1} :stuck-open? true} :hud {:health 2 :food 1}})
            [{:severity :danger :text "died 0s ago"} {:severity :danger :text "health 2"}
             {:severity :warn :text "hurt 0s ago"} {:severity :warn :text "backoff: x"} {:severity :warn :text "stuck"} {:severity :warn :text "food 1"}]]]]
    (testing title
      (is (= expected (t/reasons b now))))))

(deftest status-table
  (doseq [[title b expected]
          [["working" (body {:job {:id "j1" :name "dig"}}) :working]
           ["idle" (body) :idle]
           ["trouble beats working" (body {:job {:id "j1"} :signals {:hurt-t now}}) :trouble]
           ["manual beats trouble and working" (body {:job {:id "j1"} :signals {:takeover? true :died-t now}}) :manual]
           ["manual alone" (body {:signals {:takeover? true}}) :manual]
           ["takeover ended" (body {:signals {:takeover? false}}) :idle]
           ["offline beats manual" (body {:up false :signals {:takeover? true}}) :offline]
           ["offline beats all" (body {:up false :job {:id "j1"}}) :offline]]]
    (testing title
      (is (= expected (t/status b now))))))

(deftest sort-bodies-by-status-then-name
  (let [mk (fn [n o] (assoc (body o) :name n))
        bodies [(mk "Zed" {:up false}) (mk "Amy" {}) (mk "Bob" {:job {:id "j"}}) (mk "Cat" {:job {:id "j"}})
                (mk "Ann" {:signals {:takeover? true}}) (mk "Abe" {:signals {:hurt-t now}}) (mk "Eve" {:signals {:takeover? true}})]]
    (is (= ["Ann" "Eve" "Abe" "Bob" "Cat" "Amy" "Zed"] (map :name (t/sort-bodies bodies now))))))

(deftest counts
  (let [mk (fn [o] (body o))]
    (is (= {:manual 1 :working 2 :idle 1 :trouble 1 :offline 1}
           (t/counts [(mk {:job {:id "j"}}) (mk {:job {:id "k"}}) (mk {}) (mk {:signals {:takeover? true}}) (mk {:signals {:hurt-t now}}) (mk {:up false})] now)))
    (is (= {:manual 0 :working 0 :idle 0 :trouble 0 :offline 0} (t/counts [] now)))))

(deftest short-job-text
  (doseq [[job edn-job expected]
          [[nil nil nil]
           [{:id "j1" :name "(repeat jobs.movement.look-around)"} nil "repeat movement.look-around"]
           [{:id "j1" :name "jobs.combat.attack"} {:label "attack" :round 7} "attack, round 7"]
           [{:id "j1" :name "dig"} {:label "dig"} "dig"]]]
    (is (= expected (t/job-text job edn-job)))))

(deftest offline-label
  (doseq [[age expected] [[nil "offline"] [5000 "offline 5s"] [90000 "offline 1m"] [10800000 "offline 3h"] [172800000 "offline 2d"]]]
    (is (= expected (t/offline-text age)))))

(def full-body
  {:name "Hazel" :world "claude" :up true
   :engine {:age-ms 800 :job {:id "j1" :name "jobs.combat.attack"} :recent [{:t (- now 12000) :level "warn" :text "went wrong"}]
            :jobs [{:id "j1" :label "attack" :round 3 :current? true}] :signals {:hurt-t (- now 3000)}}
   :view {:poseMtimeMs 555 :hud {:health 14 :food 20}}})

(deftest card-model-cases
  (doseq [[title b expected]
          [["a working body in trouble"
            full-body
            {:name "Hazel" :world "claude" :status :trouble :reason "hurt 3s ago" :severity :warn :thumb "/api/thumb/claude/Hazel.png?v=555" :pose-mtime 555
             :health 14 :food 20 :job "attack, round 3" :event "went wrong" :event-age "12s ago" :event-attention :none :offline nil}]
           ["an idle body without a view or events"
            {:name "Bob" :up true :engine {:recent []}}
            {:name "Bob" :world nil :status :idle :reason nil :severity nil :thumb nil :pose-mtime nil :health nil :food nil :job nil :event nil :event-age nil :event-attention :none :offline nil}]
           ["an offline body"
            (assoc full-body :up false :engine {:age-ms 10800000 :recent [] :job nil})
            {:name "Hazel" :world "claude" :status :offline :reason nil :severity nil :thumb "/api/thumb/claude/Hazel.png?v=555"
             :health 14 :food 20 :job nil :event nil :event-age nil :event-attention :none :offline "offline 3h"}]
           ]]
    (testing title
      (is (= expected (select-keys (t/card-model b now) (keys expected)))))))

(deftest card-without-pose-has-no-thumb
  (is (nil? (:thumb (t/card-model (assoc-in full-body [:view :poseMtimeMs] nil) now)))))

(deftest thumb-age-mark-table
  (doseq [[title up mtime expected]
          [["fresh" true (- now 9000) nil]
           ["exactly 10 s" true (- now 10000) nil]
           ["just over 10 s" true (- now 10001) "10 s old"]
           ["12 s" true (- now 12000) "12 s old"]
           ["59 s" true (- now 59900) "59 s old"]
           ["a minute" true (- now 60000) "1 min old"]
           ["3 minutes" true (- now 200000) "3 min old"]
           ["2 hours" true (- now 7200000) "120 min old"]
           ["offline keeps its badge" false (- now 120000) nil]
           ["no view" true nil nil]
           ["clock ahead" true (+ now 5000) nil]]]
    (testing title
      (is (= expected (t/thumb-age-mark up mtime now))))))

(deftest card-model-carries-the-mark
  (is (= "12 s old" (:thumb-age (t/card-model {:name "A" :up true :engine {} :view {:poseMtimeMs (- now 12000)}} now))))
  (is (nil? (:thumb-age (t/card-model {:name "A" :up false :engine {} :view {:poseMtimeMs (- now 12000)}} now)))))

(deftest since-text-table
  (are [t expected] (= expected (t/since-text t))
    nil nil
    "x" nil
    (.getTime (js/Date. 2026 9 3 7 5 0)) "07:05"
    (.getTime (js/Date. 2026 9 3 23 59 59)) "23:59"
    (.getTime (js/Date. 2026 9 3 0 0 0)) "00:00"))

(deftest manual-text-table
  (let [at (.getTime (js/Date. 2026 9 3 14 30 0))]
    (are [b expected] (= expected (t/manual-text b))
      (body {:signals {:takeover? true :takeover-who "dan" :takeover-t at}}) "driven by dan since 14:30"
      (body {:signals {:takeover? true :takeover-who "dan"}}) "driven by dan"
      (body {:signals {:takeover? true}}) "driven by someone"
      (body {:signals {:takeover? false :takeover-who "dan" :takeover-t at}}) nil
      (body {:up false :signals {:takeover? true :takeover-who "dan"}}) nil
      (body) nil)))

(deftest card-model-manual
  (let [at (.getTime (js/Date. 2026 9 3 14 30 0))
        card (t/card-model (body {:signals {:takeover? true :takeover-who "dan" :takeover-t at :hurt-t now}}) now)]
    (are [k expected] (= expected (k card))
      :status :manual
      :manual "driven by dan since 14:30"
      :reason "hurt 0s ago")))

(deftest card-model-mine
  (let [mine (fn [who] (:mine? (t/card-model (body {:signals {:takeover? true :takeover-who "dashboard-k3x9ab"}}) now who)))]
    (are [who expected] (= expected (mine who))
      "dashboard-k3x9ab" true
      "dashboard-other1" false
      "dashboard" false
      nil false))
  (are [b expected] (= expected (:mine? (t/card-model b now "dashboard-k3x9ab")))
    (body {:signals {:takeover? false :takeover-who "dashboard-k3x9ab"}}) false
    (body {:up false :signals {:takeover? true :takeover-who "dashboard-k3x9ab"}}) false
    (body) false))

(deftest manual-for-any-driver-is-counted-and-sorted-first
  (let [mk (fn [name who] {:name name :up true :engine {:signals {:takeover? true :takeover-who who}} :view {}})
        bodies [(body) (mk "Z" "someone-else") (mk "Y" "dashboard-k3x9ab")]]
    (is (= [:manual :manual :idle] (mapv #(t/status % now) (t/sort-bodies bodies now))))
    (is (= 2 (:manual (t/counts bodies now))))))
