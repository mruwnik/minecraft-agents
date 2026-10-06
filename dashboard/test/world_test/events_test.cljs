(ns world-test.events-test
  (:require [cljs.test :refer [deftest is testing]]
            [world-test.events :as ev]))

(def expect-fail {:status :fail :expect {:event {:type "x"} :within-s 5} :evidence "nothing seen"})
(def expect-ok {:status :pass :expect {:event {:type "y"} :within-s 5}})

(deftest plan-counts-cases-times-repeat
  (is (= {:event "plan" :total 6} (ev/plan [{} {} {}] 2))))

(deftest phases-are-named
  (is (= {:event "phase" :name "setup"} (ev/phase :setup))))

(deftest result-names-the-case-and-run
  (is (= "f/c#2" (:name (ev/result {:id "f/c" :run 2 :status :pass})))))

(deftest outcome-follows-status
  (is (= ["passed" "failed" "error" "skipped"]
         (mapv #(:outcome (ev/result {:id "f/c" :run 1 :status %})) [:pass :fail :error :skipped]))))

(deftest a-pass-has-no-message
  (is (not (contains? (ev/result {:id "f/c" :run 1 :status :pass :expects [expect-ok]}) :message))))

(deftest the-message-is-the-first-unmet-expectation
  (let [m (:message (ev/result {:id "f/c" :run 1 :status :fail :expects [expect-ok expect-fail]}))]
    (is (re-find #"nothing seen" m))
    (is (not (re-find #"\"y\"|:type \"y\"" m)))))

(deftest the-message-falls-back-to-a-failed-after-check-then-why
  (is (re-find #"after.*wrong" (:message (ev/result {:id "f/c" :run 1 :status :fail :expects [expect-ok]
                                                       :afters [{:pass? true} {:pass? false :check "c" :evidence "wrong"}]}))))
  (is (= "boom" (:message (ev/result {:id "f/c" :run 1 :status :error :why "boom"})))))

(deftest progress-counts-fixtures
  (is (= {:event "progress" :done 1 :total 3 :unit "fixtures"} (ev/progress 1 3))))

(deftest progress-per-fixture-fires-when-a-fixtures-last-run-ends
  (testing "fixture stem done once all its runs reported"
    (let [expected {"a" 2 "b" 1}]
      (is (= [] (ev/fixtures-done expected [{:file "a" :run 1}])))
      (is (= ["b"] (ev/fixtures-done expected [{:file "a" :run 1} {:file "b" :run 1}])))
      (is (= ["a" "b"] (sort (ev/fixtures-done expected [{:file "a" :run 1} {:file "a" :run 2} {:file "b" :run 1}])))))))
