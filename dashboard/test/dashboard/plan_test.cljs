(ns dashboard.plan-test
  (:require ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.test :refer [deftest are is]]
            [clojure.string :as str]
            [dashboard.plan :as plan]))

(def fixtures-dir (.resolve path js/__dirname ".." "test" "fixtures" "plans"))
(def blueprints-dir (.resolve path js/__dirname ".." "test" "fixtures" "blueprints"))

(def good {:id "p" :status :active :parts [{:id "a" :box [[0 64 0] [1 64 1]] :want {:crop "wheat"}}]})
(def shed {:id "shed" :front :south :key {"S" "stone"} :layers [["SS"]]})

(defn temp-dir [] (.mkdtempSync fs (.join path (.tmpdir os) "plans-")))

(deftest read-dir-lists-valid-plans-and-the-errors-of-the-rest
  (let [dir (temp-dir)]
    (.writeFileSync fs (.join path dir "p.edn") (pr-str good))
    (.writeFileSync fs (.join path dir "broken.edn") "{:id ")
    (.writeFileSync fs (.join path dir "wrongid.edn") (pr-str good))
    (.writeFileSync fs (.join path dir "notes.txt") "ignored")
    (let [{:keys [plans errors]} (plan/read-dir dir)]
      (is (= ["p"] (keys plans)))
      (is (= ["broken.edn" "wrongid.edn"] (map :file errors)))
      (is (every? seq (map :errors errors))))
    (is (= {:plans {} :errors []} (plan/read-dir (.join path dir "missing"))))))

(deftest read-blueprints-skips-the-legacy-json-documents
  (let [dir (temp-dir)]
    (.writeFileSync fs (.join path dir "shed.edn") (pr-str shed))
    (.writeFileSync fs (.join path dir "hut.blueprint.json") "{}")
    (is (= {:blueprints {"shed" shed} :errors []} (plan/read-blueprints dir)))))

(deftest the-fixture-plans-and-blueprints-are-valid
  (let [{:keys [plans errors]} (plan/read-dir fixtures-dir)]
    (is (= [] errors))
    (is (= ["claude-village" "jizo-farm" "spawn-clear"] (sort (keys plans)))))
  (let [{:keys [blueprints errors]} (plan/read-blueprints blueprints-dir)]
    (is (= [] errors))
    (is (= ["hut"] (keys blueprints)))))
