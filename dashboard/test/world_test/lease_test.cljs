(ns world-test.lease-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.lease :as l]))

(defn fake-dir
  "files: atom of {index pid}. create! is exclusive like fs 'wx'."
  [files alive-pids]
  {:create! (fn [i pid] (if (contains? @files i) false (do (swap! files assoc i pid) true)))
   :holder (fn [i] (get @files i))
   :reclaim! (fn [i] (swap! files dissoc i))
   :alive? (fn [pid] (contains? alive-pids pid))})

(deftest a-free-plot-is-taken-from-the-first-index
  (let [files (atom {})]
    (is (= 0 (l/acquire (fake-dir files #{}) {:pid 10 :first 0 :total 400})))
    (is (= {0 10} @files))))

(deftest two-runners-never-get-the-same-plot
  (let [files (atom {})
        a (l/acquire (fake-dir files #{10 11}) {:pid 10 :first 0 :total 400})
        b (l/acquire (fake-dir files #{10 11}) {:pid 11 :first 0 :total 400})]
    (is (= [0 1] [a b]))))

(deftest a-lease-of-a-dead-pid-is-reclaimed
  (let [files (atom {0 99 1 10})]
    (is (= 0 (l/acquire (fake-dir files #{10 11}) {:pid 11 :first 0 :total 400})))
    (is (= {0 11 1 10} @files))))

(deftest a-lease-held-by-a-live-pid-is-skipped
  (let [files (atom {0 10})]
    (is (= 1 (l/acquire (fake-dir files #{10 11}) {:pid 11 :first 0 :total 400})))))

(deftest a-lease-that-vanishes-while-checking-is-retried
  (let [files (atom {})
        d (assoc (fake-dir files #{}) :create! (let [n (atom 0)]
                                                  (fn [i pid] (if (zero? (swap! n inc)) false (do (swap! files assoc i pid) true))))
                 :holder (fn [_] nil))]
    (is (= 0 (l/acquire d {:pid 5 :first 0 :total 400})))))

(deftest no-free-plot-is-an-error
  (let [files (atom {0 10 1 10})]
    (is (thrown? js/Error (l/acquire (fake-dir files #{10}) {:pid 11 :first 0 :total 2})))))

(deftest the-time-log-stamp-is-local-iso-with-offset
  (is (= "2026-10-05T15:04:09.007+01:00" (l/local-iso [2026 10 5 15 4 9 7] 60)))
  (is (= "2026-10-05T03:04:09.120-05:30" (l/local-iso [2026 10 5 3 4 9 120] -330)))
  (is (= "2026-01-02T00:00:00.000+00:00" (l/local-iso [2026 1 2 0 0 0 0] 0))))
