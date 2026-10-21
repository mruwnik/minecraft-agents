(ns engine.trip-test
  "jobs.lib.trip/ready!: a hunt that leaves home deposits first when the bag is fullish, against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.memory :as mem]
            [engine.test-util :as tu]))

(def cow-drops [{:name "beef" :count 1}])
(defn cow [id x] {:id id :name "cow" :kind "passive" :pos {:x x :y 64 :z 0} :drops cow-drops})
(def junk (mapv (fn [i] {:name (str "junk_" i) :count 1}) (range 30)))

(defn ^:async hunt
  "Run a one-cow hunt with a nearly full bag, a chest known at chest (or none); [setup transfers attacks event-kinds]."
  [chest args]
  (let [s (h/setup {:inventory (into h/sword junk) :containers {"10,64,0" []} :entities [(cow 1 3) (cow 2 5) (cow 3 7)]} 20)]
    (when chest (mem/write! (:store (:eng s)) :chest {:pos chest} mem/place-policy))
    (core/submit! (:eng s) (list 'jobs.combat.hunt (merge {:item "beef" :count 1 :keep 0} args)) {})
    (await (tu/run-ticks (:eng s) (:clock s) 40 700))
    [s (count (h/calls (:p s) "transfer")) (count (h/calls (:p s) "attack")) (set (map :kind @(:seen s)))]))

(deftest a-fullish-bag-with-a-chest-near-deposits-then-hunts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ transfers attacks kinds] (await (hunt {:x 10 :y 64 :z 0} {}))]
          (is (pos? transfers))
          (is (pos? attacks))
          (is (contains? kinds :trip.deposited)))))))

(deftest a-fullish-bag-with-no-chest-hunts-with-an-info-event
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ transfers attacks kinds] (await (hunt nil {}))]
          (is (zero? transfers))
          (is (pos? attacks))
          (is (contains? kinds :trip.no-chest)))))))

(deftest a-chest-too-far-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ transfers attacks kinds] (await (hunt {:x 400 :y 64 :z 0} {}))]
          (is (zero? transfers))
          (is (pos? attacks))
          (is (contains? kinds :trip.far)))))))

(defn ^:async cull
  "Run a one-cow cull with a nearly full bag and a chest known at 10,64,0; [transfers attacks event-kinds]."
  [args]
  (let [s (h/setup {:inventory (into h/sword junk) :containers {"10,64,0" []} :entities [(cow 1 3) (cow 2 5) (cow 3 7)]} 20)]
    (mem/write! (:store (:eng s)) :chest {:pos {:x 10 :y 64 :z 0}} mem/place-policy)
    (core/submit! (:eng s) (list 'jobs.animals.cull (merge {:mob "cow" :count 1 :keep 0} args)) {})
    (await (tu/run-ticks (:eng s) (:clock s) 40 700))
    [(count (h/calls (:p s) "transfer")) (count (h/calls (:p s) "attack")) (set (map :kind @(:seen s)))]))

(deftest cull-deposits-then-culls
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[transfers attacks kinds] (await (cull {}))]
          (is (pos? transfers))
          (is (pos? attacks))
          (is (contains? kinds :trip.deposited)))))))

(deftest the-deposit-before-a-trip-keeps-the-units-the-register-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory (into (into h/sword [{:name "raw_iron" :count 5} {:name "coal" :count 3}]) junk)
                          :containers {"10,64,0" []} :entities [(cow 1 3)]} 20)]
          (mem/write! (:store (:eng s)) :chest {:pos {:x 10 :y 64 :z 0}} mem/place-policy)
          (mem/write! (:store (:eng s)) :reserve {:id "kit-raw" :item "raw_iron" :count 5 :strength 0.9} {:cap 100 :ttl :forever})
          (core/submit! (:eng s) '(jobs.combat.hunt {:item "beef" :count 1 :keep 0}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 40 700))
          (let [carried (reduce (fn [m i] (update m (.-name i) (fnil + 0) (.-count i))) {} (.-inventory (.self (:p s))))]
            (is (contains? (set (map :kind @(:seen s))) :trip.deposited))
            (is (= 5 (get carried "raw_iron")))
            (is (nil? (get carried "junk_0")) "the rest went into the chest")))))))
