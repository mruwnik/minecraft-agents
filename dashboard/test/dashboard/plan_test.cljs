(ns dashboard.plan-test
  (:require ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.test :refer [deftest are is]]
            [clojure.string :as str]
            [dashboard.plan :as plan]))

(def fixtures-dir (.resolve path js/__dirname ".." "test" "fixtures" "plans"))

(def region {:min [0 64 0] :max [1 64 1]})
(def good {:id "p" :region region :elements [{:id "a" :kind :plot :region region :content {:crop "wheat"}}]})

(deftest parse-never-throws
  (are [text expected-fragment] (some #(str/includes? % expected-fragment) (:errors (plan/parse text "p")))
    "{:id " "unreadable EDN"
    "[1 2]" "must hold one map"
    "{:id \"q\"}" "must equal the file name"
    "" "must hold one map"))

(deftest parse-reads-a-plan
  (is (= good (:plan (plan/parse (pr-str good) "p")))))

(deftest read-dir-lists-valid-plans-and-the-errors-of-the-rest
  (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "plans-"))]
    (.writeFileSync fs (.join path dir "p.edn") (pr-str good))
    (.writeFileSync fs (.join path dir "broken.edn") "{:id ")
    (.writeFileSync fs (.join path dir "wrongid.edn") (pr-str good))
    (.writeFileSync fs (.join path dir "notes.txt") "ignored")
    (let [{:keys [plans errors]} (plan/read-dir dir)]
      (is (= ["p"] (keys plans)))
      (is (= ["broken.edn" "wrongid.edn"] (map :file errors)))
      (is (every? seq (map :errors errors))))
    (is (= {:plans {} :errors []} (plan/read-dir (.join path dir "missing"))))))

(deftest the-fixture-plans-are-valid
  (let [{:keys [plans errors]} (plan/read-dir fixtures-dir)]
    (is (= [] errors))
    (is (= ["claude-village" "jizo-farm" "spawn-clear"] (sort (keys plans))))))

