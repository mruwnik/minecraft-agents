(ns engine.fish-test
  "jobs.food.fish against the fake world: a pond with a bank, casts that bite after some misses."
  (:require [cljs.test :refer [deftest is async]]
            [engine.library-test :as lt]
            [engine.test-util :as tu]))

(def pond (into {} (for [x (range 0 9) z (range 0 9) y [62 63]] [(str x "," y "," z) "water"])))
(def bank (into {} (for [z (range 0 9) y [62 63]] [(str "9," y "," z) "stone"])))
(def rod {:name "fishing_rod" :count 1})

(defn world [inventory fish]
  {:blocks (merge pond bank) :inventory inventory :fish fish :self {:pos [9.5 64 4.5]}})

(defn ^:async run [w args]
  (let [{:keys [eng p]} (lt/setup w)
        res (await (tu/child-outcome eng 'jobs.food.fish args 120))]
    {:res res :p p}))

(defn carried [p name] (reduce + (map #(.-count %) (filter #(= name (.-name %)) (array-seq (.-inventory (.self p)))))))

(deftest fishes-until-the-count-is-caught
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run (world [rod] {:catch ["cod" "salmon"] :misses 2}) {:count 3}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 3 (:got res)))
          (is (= 3 (+ (carried p "cod") (carried p "salmon")))))))))

(deftest no-rod-and-no-fetch-ends-no-rod
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (world [] {}) {:count 1 :fetch false}))]
          (is (= :stopped (:status res)))
          (is (= :no-rod (:reason res))))))))

(deftest casts-that-never-bite-end-no-bite-with-the-count-got
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (world [rod] {:misses 100}) {:count 2 :max-misses 4}))]
          (is (= :stopped (:status res)))
          (is (= :no-bite (:reason res)))
          (is (= 0 (:got res))))))))

(deftest no-water-in-sight-ends-no-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks bank :inventory [rod]} {:count 1}))]
          (is (= :stopped (:status res)))
          (is (= :no-water (:reason res))))))))
