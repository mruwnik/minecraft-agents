(ns plan.parse-test
  (:require [cljs.test :refer [deftest are is]]
            [clojure.string :as str]
            [plan.parse :as parse]))

(def good {:id "p" :status :active :parts [{:id "a" :box [[0 64 0] [1 64 1]] :want {:crop "wheat"}}]})
(def shed {:id "shed" :front :south :key {"S" "stone"} :layers [["SS"]]})

(deftest parse-never-throws
  (are [text expected-fragment] (some #(str/includes? % expected-fragment) (:errors (parse/parse text "p")))
    "{:id " "unreadable EDN"
    "[1 2]" "must hold one map"
    "{:id \"q\"}" "must equal the file name"
    "" "must hold one map"))

(deftest parse-names-the-part-in-its-errors
  (is (= ["part a: a part needs a :want"]
         (:errors (parse/parse (pr-str (assoc-in good [:parts 0] {:id "a" :cells [[0 64 0]]})) "p")))))

(deftest parse-reads-a-plan
  (is (= good (:plan (parse/parse (pr-str good) "p")))))

(deftest parse-blueprint-reads-and-checks
  (are [text id expected] (= expected (parse/parse-blueprint text id))
    (pr-str shed) "shed" {:blueprint shed}
    (pr-str shed) "barn" {:errors [":id must equal the file name, \"barn\""]}))
