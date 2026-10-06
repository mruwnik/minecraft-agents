(ns engine.util-fail-test
  "jobs.lib.util failure counting: failures count in a row, progress! resets them."
  (:require [cljs.test :refer [deftest is]]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]))

(defn with-mem
  "Run (f ctx) with job memory in an atom; returns [result mem emitted]."
  [f]
  (let [m (atom {})
        out (atom [])
        c {:update-mem (fn [g args] (apply swap! m g args))
           :emit (fn [kind level data] (swap! out conj [kind level data]))}]
    (with-redefs [ctx/mem (fn [_] @m)]
      (let [r (f c)]
        [r @m @out]))))

(deftest fail-counts-in-a-row
  (let [[rs _ out] (with-mem (fn [c] (mapv (fn [_] (u/fail! c :x.gave-up "no")) (range 3))))]
    (is (= [:continue :continue :done] rs))
    (is (= [[:x.gave-up :warn {:tries 3 :text "no"}]] out))))

(deftest progress-resets-the-row
  (let [[rs m out] (with-mem (fn [c]
                               [(u/fail! c :x "a") (u/fail! c :x "a")
                                (u/progress! c)
                                (u/fail! c :x "a") (u/fail! c :x "a")]))]
    (is (= [:continue :continue nil :continue :continue] rs))
    (is (= 2 (:failures m)))
    (is (empty? out))))

(deftest count-fail-says-when-the-row-is-full
  (let [[rs] (with-mem (fn [c] (mapv (fn [_] (u/count-fail! c)) (range 3))))]
    (is (= [false false true] rs))))
