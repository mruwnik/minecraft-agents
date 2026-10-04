(ns dashboard.ui.statusfilter-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.statusfilter :as sf]
            [dashboard.ui.trouble :as trouble]))

(def now 1000000)

(defn body [name {:keys [signals job up] :or {up true}}]
  {:name name :up up :engine {:signals signals :job job}})

(def bodies
  [(body "Off" {:up false})
   (body "Man" {:signals {:takeover? true}})
   (body "Work" {:job {:id "j1" :name "dig"}})
   (body "Idle" {})
   (body "Hurt" {:signals {:died-t now}})
   (body "Work2" {:job {:id "j2" :name "dig"}})])

(defn names [bs] (mapv :name bs))

(deftest the-chips-use-the-five-states-in-order
  (is (= [[:manual "manual"] [:working "working"] [:idle "idle"] [:trouble "in trouble"] [:offline "offline"]]
         sf/states)))

(deftest only-states-keeps-the-pressed-states
  (are [pressed expected] (= expected (names (sf/only-states bodies pressed now)))
    #{} ["Off" "Man" "Work" "Idle" "Hurt" "Work2"]
    nil ["Off" "Man" "Work" "Idle" "Hurt" "Work2"]
    #{:working} ["Work" "Work2"]
    #{:working :idle} ["Work" "Idle" "Work2"]
    #{:offline :trouble :manual} ["Off" "Man" "Hurt"]
    #{:idle :working :manual :trouble :offline} ["Off" "Man" "Work" "Idle" "Hurt" "Work2"]))

(deftest a-state-with-no-bodies-shows-none
  (is (= [] (sf/only-states [(body "Idle" {})] #{:working} now))))

(deftest keep-names-survive-the-filter
  (are [pressed keep expected] (= expected (names (sf/only-states bodies pressed now keep)))
    #{:working} #{"Off"} ["Off" "Work" "Work2"]
    #{:working} #{"Work"} ["Work" "Work2"]
    #{:working} #{"nobody"} ["Work" "Work2"]
    #{} #{"Off"} ["Off" "Man" "Work" "Idle" "Hurt" "Work2"]))

(deftest shown-bodies-match-the-counts
  (let [counts (trouble/counts bodies now)]
    (are [k] (= (get counts k) (count (sf/only-states bodies #{k} now)))
      :manual :working :idle :trouble :offline)))

(deftest toggle-adds-and-removes
  (are [pressed k expected] (= expected (sf/toggle pressed k))
    #{} :idle #{:idle}
    nil :idle #{:idle}
    #{:idle} :idle #{}
    #{:idle} :working #{:idle :working}
    #{:idle :working} :idle #{:working}))

(deftest toggle-fx-updates-the-db-and-stores-the-set
  (is (= {:db {:status-filter #{:idle} :x 1} :store-status-filter #{:idle}}
         (sf/toggle-fx {:status-filter #{} :x 1} :idle)))
  (is (= {:db {:status-filter #{}} :store-status-filter #{}}
         (sf/toggle-fx {:status-filter #{:idle}} :idle))))

(def counts {:manual 1 :working 2 :idle 0 :trouble 3})

(defn chip-of [chips k] (first (filter #(= k (:state %)) chips)))

(deftest chips-list-every-state-with-the-full-counts
  (are [pressed] (= [[:manual 1] [:working 2] [:idle 0] [:trouble 3] [:offline 0]]
                    (mapv (juxt :state :count) (sf/chips counts pressed)))
    #{} #{:working} #{:idle :offline}))

(deftest chips-flag-pressed-and-dim
  (are [pressed k pressed? dim?] (let [c (chip-of (sf/chips counts pressed) k)]
                                   (and (= pressed? (:pressed? c)) (= dim? (:dim? c))))
    #{} :idle false false
    #{:working} :working true false
    #{:working} :idle false true
    #{:working :idle} :idle true false
    #{:working :idle} :offline false true))

(deftest chips-title-the-click
  (are [pressed k expected] (= expected (:title (chip-of (sf/chips counts pressed) k)))
    #{} :working "show only working bodies"
    #{} :manual "show only manual bodies"
    #{} :trouble "show only bodies in trouble"
    #{} :offline "show only offline bodies"
    #{:working} :working "stop filtering by working"
    #{:trouble} :trouble "stop filtering by trouble"
    #{:working} :idle "show only idle bodies"))

(deftest chips-carry-the-label
  (is (= ["manual" "working" "idle" "in trouble" "offline"] (mapv :label (sf/chips {} #{})))))

(deftest empty-text-names-the-pressed-states
  (are [pressed expected] (= expected (sf/empty-text pressed))
    #{:working} "no body is working"
    #{:trouble} "no body is in trouble"
    #{:idle :working} "no body is working or idle"
    #{:manual :working :idle} "no body is manual, working or idle"
    #{:offline :manual :trouble :working} "no body is manual, working, in trouble or offline"))

(deftest rubbish-stored-text-gives-no-filter
  (are [text expected] (= expected (sf/parse-stored text))
    nil #{}
    "" #{}
    "not json" #{}
    "{" #{}
    "null" #{}
    "42" #{}
    "\"idle\"" #{}
    "{\"idle\":true}" #{}
    "[]" #{}
    "[\"idle\"]" #{:idle}
    "[\"idle\",\"nonsense\",3,null,\"working\"]" #{:idle :working}
    "[\"nonsense\"]" #{}))

(deftest the-stored-text-round-trips-in-state-order
  (are [pressed text] (and (= text (sf/stored-text pressed))
                           (= pressed (sf/parse-stored text)))
    #{} "[]"
    #{:idle} "[\"idle\"]"
    #{:offline :manual :idle} "[\"manual\",\"idle\",\"offline\"]"))
