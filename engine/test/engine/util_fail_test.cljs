(ns engine.util-fail-test
  "jobs.lib.util failure counting: failures count in a row, progress! resets them; cost/try-again? prices another try."
  (:require [cljs.test :refer [deftest is]]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.value :as value]
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
    (is (= 2 (:jobs.lib.util/failures m)))
    (is (empty? out))))

(deftest count-fail-says-when-the-row-is-full
  (let [[rs] (with-mem (fn [c] (mapv (fn [_] (u/count-fail! c)) (range 3))))]
    (is (= [false false true] rs))))

(deftest another-try-is-worth-it-while-its-chance-times-the-worth-beats-its-price
  (let [s (get-in (settings/get value/settings ::value/retry-counts) [:walk :try-s])
        tries (fn [w] (first (with-mem (fn [c] (with-redefs [ctx/worth (constantly w)]
                                                 (count (take-while (fn [_] (u/failed! c) (cost/try-again? c :walk)) (range 100))))))))]
    (is (= 0 (tries 0)))
    (is (= 0 (tries s)) "a goal worth one try's price is not worth it at under a sure chance")
    (is (< 0 (tries (* 100 s)) (tries (* 10000 s))) "each failure lowers the chance, so a dearer goal tries more")
    (is (= 100 (tries ##Inf)) "an arrive tries on: only its :spend stops it")))

(deftest a-caller-that-counts-and-prices-its-own-tries-passes-them-as-opts
  (let [[rs] (with-mem (fn [c] (with-redefs [ctx/worth (constantly 0)]
                                 (mapv #(cost/try-again? c :respond %)
                                       [{:fails 1 :worth 30 :try-s 5} {:fails 4 :worth 30 :try-s 5} {:fails 1 :worth 30 :try-s 500} {:fails 1 :worth 0 :try-s 0.05}]))))]
    (is (= [true false false false] rs))))
