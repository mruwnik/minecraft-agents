(ns world-test.trace-test
  (:require [cljs.test :refer [deftest is]]
            [clojure.string :as str]
            [world-test.trace :as t]))

(deftest file-name-from-case-run-and-attempt
  (is (= "breathe-pond__sink-and-rise__r1-a0.edn" (t/file-name "breathe-pond/sink-and-rise" 1 0)))
  (is (= "a__b_c__r2-a1.edn" (t/file-name "a/b c" 2 1)))
  (is (= "x__x__r1-a0.edn" (t/file-name "x" 1 0))))

(deftest tail-keeps-the-end-within-the-cap
  (is (= "abc" (t/tail-text "abc" 10)))
  (is (= "fghij" (t/tail-text "abcdefghij" 5))))

(deftest events-are-capped-keeping-the-latest
  (let [evs (mapv (fn [i] {:kind :e :n i}) (range 100))
        {:keys [events dropped]} (t/cap-events evs 400)]
    (is (pos? dropped))
    (is (= 100 (+ dropped (count events))))
    (is (= 99 (:n (peek events))))
    (is (<= (count (pr-str events)) 400)))
  (is (= 0 (:dropped (t/cap-events [{:a 1}] 400)))))

(deftest doc-carries-the-case-and-capped-parts
  (let [d (t/trace-doc {:id "f/c" :run 1 :attempt 0 :status :fail :why "w" :events [{:a 1}] :log "line\n"})]
    (is (= {:id "f/c" :run 1 :attempt 0 :status :fail :why "w" :events [{:a 1}] :events-dropped 0 :body-log-tail "line\n"}
           d))))

(deftest doc-carries-the-step-tail-only-when-the-body-traced-steps
  (is (= "9 2 T 1,2,3" (:steps-tail (t/trace-doc {:id "f/c" :steps "9 2 T 1,2,3"}))))
  (is (not (contains? (t/trace-doc {:id "f/c" :steps nil}) :steps-tail))))

(deftest write-puts-the-doc-in-the-dir-and-returns-its-path
  (let [fs (js/require "fs") os (js/require "os") path (js/require "path")
        dir (.mkdtempSync fs (.join path (.tmpdir os) "trace-test-"))
        sub (.join path dir "cases")
        p (t/write! sub "f/c" 1 0 {:id "f/c" :status :error})]
    (is (= (.join path sub "f__c__r1-a0.edn") p))
    (is (str/includes? (.readFileSync fs p "utf8") ":status :error"))
    (.rmSync fs dir #js {:recursive true :force true})))
