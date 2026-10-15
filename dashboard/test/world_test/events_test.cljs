(ns world-test.events-test
  (:require [cljs.test :refer [deftest is testing]]
            [world-test.events :as ev]))

(def expect-fail {:status :fail :expect {:event {:type "x"} :within-s 5} :evidence "nothing seen"})
(def expect-ok {:status :pass :expect {:event {:type "y"} :within-s 5}})

(deftest plan-counts-cases-times-repeat
  (is (= {:event "plan" :total 6} (ev/plan [{} {} {}] 2))))

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

(deftest a-flaky-result-is-its-own-outcome-with-its-why
  (let [m (ev/result {:id "f/c" :run 1 :status :flaky :why "passed on a retry"})]
    (is (= "flaky" (:outcome m)))
    (is (= "passed on a retry" (:message m)))))

(deftest the-message-names-the-real-failure-before-a-not-judged-one
  (let [not-judged {:status :fail :not-judged true :evidence "not judged: the run stopped at the first failure" :expect {:event {:kind :a} :within-s 5}}
        real {:status :fail :evidence "unwanted job/no_air" :expect {:no-event {:kind :no_air} :for-s 30}}]
    (is (re-find #"no_air" (:message (ev/result {:id "f/c" :run 1 :status :fail :expects [not-judged real]}))))
    (is (re-find #"not judged" (:message (ev/result {:id "f/c" :run 1 :status :fail :expects [not-judged]}))))
    (is (re-find #"after.*wrong" (:message (ev/result {:id "f/c" :run 1 :status :fail :expects [not-judged]
                                                       :afters [{:check [:x] :pass? false :evidence "wrong"}]})))
        "a failed after check outranks a not-judged expectation")))

(deftest a-failed-case-always-carries-a-why-naming-its-first-failure
  (is (re-find #"after.*wrong" (:why (ev/with-why {:status :fail :expects [expect-ok] :afters [{:check [:x] :pass? false :evidence "wrong"}]}))))
  (is (= "kept" (:why (ev/with-why {:status :fail :why "kept"}))))
  (is (not (contains? (ev/with-why {:status :pass :expects [expect-ok]}) :why))))
