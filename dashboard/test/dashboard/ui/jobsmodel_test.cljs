(ns dashboard.ui.jobsmodel-test
  (:require [cljs.test :refer [deftest is are]]
            [clojure.string :as str]
            [dashboard.ui.jobsmodel :as jm]))

(def jobs
  [{:id "jobs.movement.pace" :category "movement" :name "pace" :doc "Walk a, b." :args "{:laps {}}" :running ["ProbeView"] :reflex []}
   {:id "jobs.combat.attack" :category "combat" :name "attack" :doc "Kill things." :args nil :running [] :reflex ["ProbeDrive"] :backoff true}
   {:id "jobs.movement.go-to" :category "movement" :name "go-to" :doc nil :ns-doc "Go." :args nil :running [] :reflex []}])

(deftest paragraph-splitting
  (are [doc expected] (= expected (jm/paragraphs doc))
    nil []
    "" []
    "one two\n  three" ["one two three"]
    "a\n\n  b\n  c\n\n\nd" ["a" "b c" "d"]))

(deftest filtering
  (are [needle ids] (= ids (mapv :id (mapcat :jobs (jm/grouped jobs needle))))
    "" ["jobs.combat.attack" "jobs.movement.go-to" "jobs.movement.pace"]
    nil ["jobs.combat.attack" "jobs.movement.go-to" "jobs.movement.pace"]
    "PACE" ["jobs.movement.pace"]
    "kill" ["jobs.combat.attack"]
    "probeview" ["jobs.movement.pace"]
    "probedrive" ["jobs.combat.attack"]
    ":laps" ["jobs.movement.pace"]
    "go." ["jobs.movement.go-to"]
    "zzz" []))

(deftest grouping
  (is (= [["combat" 1] ["movement" 2]]
         (mapv (juxt :category (comp count :jobs)) (jm/grouped jobs "")))))

(deftest badges
  (are [job expected] (= expected (jm/badges job))
    {:running [] :reflex []} []
    {:running ["A" "B"] :reflex []} [{:kind :running :text "running on: A, B"}]
    {:running [] :reflex ["A"]} [{:kind :reflex :text "reflex on: A"}]
    {:running ["A"] :reflex ["B"] :backoff true} [{:kind :running :text "running on: A"} {:kind :reflex :text "reflex on: B"} {:kind :backoff :text "backoff"}]))

(deftest summary-text
  (are [needle shown total expected] (= expected (jm/count-text shown total needle))
    "" 28 28 "28 jobs"
    "x" 3 28 "3 of 28 jobs"
    "" 1 1 "1 job"))

(deftest body-text
  (are [job expected] (= expected (jm/body-paragraphs job))
    {:doc "A\n\nB" :ns-doc "N"} ["A" "B"]
    {:doc nil :ns-doc "N"} ["N"]
    {:doc nil :ns-doc nil} []))

(deftest long-docs
  (are [doc expected] (= expected (jm/long-doc? {:doc doc}))
    nil false
    "short" false
    (apply str (repeat 421 "x")) true
    (str (apply str (repeat 300 "x")) "\n\n" (apply str (repeat 300 "y"))) true))

(def long-sentence (str (str/join " " (repeat 60 "word")) "."))

(deftest summaries
  (are [job expected] (= expected (jm/summary job))
    {:doc nil :ns-doc nil} {:text "" :more? false}
    {:doc "Short."} {:text "Short." :more? false}
    {:doc "No full stop"} {:text "No full stop" :more? false}
    {:doc "First one. Second one."} {:text "First one." :more? true}
    {:doc "First!\n\nSecond"} {:text "First!" :more? true}
    {:doc nil :ns-doc "Go. Far."} {:text "Go." :more? true}
    {:doc "Pi is 3.14 ok. More"} {:text "Pi is 3.14 ok." :more? true}
    {:doc long-sentence} {:text (str (str/join " " (repeat 40 "word")) "…") :more? true}))

(deftest arg-counts
  (are [args n] (= n (jm/arg-count args))
    nil 0
    "{}" 0
    "{:laps {:doc \"x\", :default 1}}" 1
    "{:mob {:doc \"a\\nb\"}\n :count {:doc \"c\"}\n :radius {:doc \"r\"}}" 3))
