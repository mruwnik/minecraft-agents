(ns dashboard.village-data-cache-test
  (:require [cljs.test :refer [deftest is]]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [dashboard.edn-data :as data]
            [dashboard.village-data :as village-data]))

(defn plan-text [id x] (str "{:id \"" id "\" :kind :village :at [" x " 64 0] :parts []}"))

(defn with-plans [f]
  (let [root (.mkdtempSync fs (.join path (os/tmpdir) "village-cache-test-"))
        dir (.join path root "w" "plans")]
    (.mkdirSync fs dir #js {:recursive true})
    (try (f dir (fn [file] {:worlds ["w"] :worlds-dir root :file file}))
         (finally (.rmSync fs root #js {:recursive true :force true})))))

(defn write! [dir name text seconds-ahead]
  (let [file (.join path dir name) t (+ (/ (js/Date.now) 1000) seconds-ahead)]
    (.writeFileSync fs file text)
    (.utimesSync fs file t t)))

(defn snap [root-opts] (village-data/snapshot "unused" [] (dissoc root-opts :file)))

(deftest unchanged-plan-files-are-parsed-once
  (with-plans
    (fn [dir opts]
      (write! dir "a.edn" (plan-text "a" 1) 0)
      (write! dir "b.edn" (plan-text "b" 2) 0)
      (let [reads (atom 0) orig data/read-file o (opts nil)]
        (with-redefs [data/read-file (fn [f] (swap! reads inc) (orig f))]
          (let [first-poll (snap o)]
            (is (= 2 (count (:villages first-poll))))
            (is (= 2 @reads))
            (is (= first-poll (snap o)))
            (is (= 2 @reads) "second poll parses nothing")))))))

(deftest changed-added-and-removed-plan-files-are-seen
  (with-plans
    (fn [dir opts]
      (write! dir "a.edn" (plan-text "a" 1) 0)
      (write! dir "b.edn" (plan-text "b" 2) 0)
      (let [o (opts nil) xs #(mapv :x (:villages (snap o)))]
        (is (= [1 2] (xs)))
        (write! dir "a.edn" (plan-text "a" 5) 10)
        (is (= [5 2] (xs)) "a changed file is read again")
        (write! dir "c.edn" (plan-text "c" 9) 0)
        (is (= [5 2 9] (xs)) "a new file is read")
        (.unlinkSync fs (.join path dir "b.edn"))
        (is (= [5 9] (xs)) "a removed file is gone")
        (write! dir "a.edn" "{:id \"a\" :kind :village" 20)
        (is (some? (:error (snap o))) "a broken file reports its error, never a stale result")))))
