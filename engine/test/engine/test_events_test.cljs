(ns engine.test-events-test
  (:require [cljs.test :refer [deftest is]]
            [engine.test-events :as ev]))

(deftest line-is-marked-json
  (is (= "@@test {\"event\":\"plan\",\"total\":3}" (ev/line {:event "plan" :total 3}))))

(deftest var-name-drops-the-var-quote
  (is (= "engine.a-test/one" (ev/var-name "#'engine.a-test/one"))))

(deftest result-passed-without-failures
  (is (= {:event "result" :name "engine.a-test/one" :outcome "passed"}
         (ev/result "#'engine.a-test/one" []))))

(deftest result-failed-carries-expected-and-actual
  (let [r (ev/result "#'engine.a-test/one" [{:type :fail :message "sum" :expected '(= 1 2) :actual '(not (= 1 2))}])]
    (is (= "failed" (:outcome r)))
    (is (re-find #"sum" (:message r)))
    (is (re-find #"expected \(= 1 2\)" (:message r)))
    (is (re-find #"got \(not \(= 1 2\)\)" (:message r)))))

(deftest result-error-wins-over-failed
  (is (= "error" (:outcome (ev/result "#'x/y" [{:type :fail :expected 1 :actual 2} {:type :error :expected 1 :actual (js/Error. "boom")}]))))
  (is (re-find #"boom" (:message (ev/result "#'x/y" [{:type :error :expected 1 :actual (js/Error. "boom")}])))))

(deftest result-message-is-capped
  (is (<= (count (:message (ev/result "#'x/y" [{:type :fail :expected (apply str (repeat 5000 "x")) :actual 1}]))) 2000)))
