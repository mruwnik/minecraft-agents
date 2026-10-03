(ns dashboard.ui.eventlog-test
  (:require [cljs.test :refer [deftest is are testing]]
            [dashboard.ui.eventlog :as log]))

(defn ev [seq source kind data & [extra]]
  (merge {:seq seq :generation-id "g1" :time-ms (* 1000 seq)
          :source source :kind kind :context {} :data data}
         extra))

(deftest canonical-category-cases
  (are [e expected] (= expected (log/category e))
    (ev 1 :chat :said {:text "hi"}) :chat
    (ev 2 :body :hurt {}) :combat
    (ev 3 :body :died {}) :combat
    (ev 4 :reflex :fired {} {:context {:reflex-id "flee"}}) :combat
    (ev 5 :reflex :fired {} {:context {:reflex-id "stuck"}}) :movement
    (ev 6 :job :unreachable {}) :movement
    (ev 7 :action :started {:name "go-to"}) :movement
    (ev 8 :action :started {:name "dig"}) :jobs
    (ev 9 :job :completed {}) :jobs
    (ev 10 :system :started {}) :system))

(deftest canonical-summary-cases
  (are [e expected] (= expected (log/summary e))
    (ev 1 :job :completed {:job-name "jobs.gather"}) "jobs.gather"
    (ev 2 :body :chat {} {:message "hello"}) "hello"
    (ev 3 :action :started {:name "wait" :args {:ms 3000}}) "wait {:ms 3000}"
    (ev 4 :action :done {:name "wait" :ms 12.7}) "wait 13ms"
    (ev 5 :job :failed {:error "boom"}) "boom"
    (ev 6 :system :started {}) "started"))

(deftest attention-is-distinct-from-severity
  (let [required (ev 1 :job :failed {:reason "needs recovery"} {:attention :required :request-id "r1"})
        notice (ev 2 :system :stopping {} {:attention :notice})
        resolved (ev 3 :attention :resolved {:reason :handled} {:request-id "r1"})
        outstanding {"r1" {:request-id "r1"}}]
    (is (= :required (:attention (log/row required outstanding))))
    (is (= :resolved (:attention (log/row required {}))))
    (is (= :notice (:attention (log/row notice {}))))
    (is (= :resolved (:attention (log/row resolved outstanding))))
    (is (not (contains? (log/row required outstanding) :level)))
    (is (= "needs recovery · job-1 · failed" (log/request-text {:event required :reason :failed :job-id "job-1"})))
    (is (= [1] (mapv :seq (log/rows [required notice resolved] outstanding {:chip :required}))))
    (is (= [2] (mapv :seq (log/rows [required notice resolved] outstanding {:chip :notices}))))))

(deftest current-action-cases
  (are [evs expected] (= expected (log/current-action evs))
    [] nil
    [(ev 1 :action :started {:name "wait"})] "wait"
    [(ev 1 :action :started {:name "wait"}) (ev 2 :action :done {:name "wait"})] nil
    [(ev 1 :action :started {:name "wait"}) (ev 2 :job :yielded {})] "wait"))
