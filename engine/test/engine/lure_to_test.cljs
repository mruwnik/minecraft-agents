(ns engine.lure-to-test
  "jobs.animals.lure-to against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]))

(defn cow [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(def wheat [{:name "wheat" :count 4}])
(def goal {:x 30 :y 64 :z 0})

(defn ^:async scenario [args world n]
  (let [{:keys [eng clock] :as s} (h/setup (merge {:floor tu/walk-floor} world) 50)]
    (core/submit! eng (list 'jobs.animals.lure-to (merge {:mob "cow" :pos goal} args)) {})
    (dotimes [_ n]
      (swap! clock + 700)
      (await (core/tick! eng)))
    s))

(defn done-event [{:keys [seen]}] (first (filter #(= :lure-to.done (:kind %)) @seen)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn stopped [{:keys [seen]}] (filterv #(= :stopped (:kind %)) @seen))
(defn cow-of [{:keys [p]} id] (first (filter #(= id (:id %)) (fake/entities p))))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))

(deftest lures-the-cow-to-the-spot-and-puts-the-food-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory wheat :entities [(cow 1 3)]} 12))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :arrived (:reason (done-event s))))
          (is (= "u1" (:animal (done-event s))))
          (is (<= (js/Math.abs (- 30 (first (:pos c)))) 5) "the cow stands near the spot")
          (is (= 4 (count-of s "wheat")) "no food is used")
          (is (nil? (.-held (.self (:p s)))) "the food is put away at the end"))))))

(deftest one-call-is-the-whole-attempt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory wheat :entities [(cow 1 3)]} 1))]
          (is (finished? s))
          (is (= :arrived (:reason (done-event s)))))))))

(deftest no-food-ends-no-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory [] :entities [(cow 1 3)]} 12))]
          (is (finished? s))
          (is (= :no-food (:reason (done-event s))))
          (is (= 1 (count (stopped s)))))))))

(deftest no-animal-ends-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory wheat :entities []} 12))]
          (is (= :none (:reason (done-event s))))
          (is (= 1 (count (stopped s)))))))))

(deftest an-animal-on-a-lead-is-not-lured
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory wheat :entities [(cow 1 3 {:leashed true})]} 12))]
          (is (= :none (:reason (done-event s)))))))))
