(ns engine.pen-entry-sight-test
  "A fence does not stop sight: the body facing a fenced pen from outside sees the floor and the animals behind it, so
  jobs.lib.pen-entry finds the pen."
  (:require [cljs.test :refer [deftest is async]]
            [engine.test-util :as tu]
            [engine.fake :as fake]
            [engine.herd-test :as ht]
            [engine.hostile-test :as h]
            [engine.perception :as perception]
            [engine.sense-test :as st]
            [jobs.lib.sense :as sense]
            [jobs.lib.pen-entry :as pen-entry]))

(def ring
  (into {} (for [x (range 18 25) z (range 13 20) :when (or (#{18 24} x) (#{13 19} z))] [(str x ",64," z) "oak_fence"])))

(def ground (into {} (for [x (range 5 30) z (range 5 28)] [(str x ",63," z) "stone"])))

(def world
  {:self {:pos {:x 14.5 :y 64 :z 16.5}}
   :ray-deg 2
   :blocks (merge ground ring {"18,64,16" "oak_fence_gate"})
   :states {"18,64,16" {:open true :facing "east"}}
   :entities [(ht/cow 1 21.5 15.5) (ht/cow 2 22.5 17.5)]})

(deftest a-fenced-pen-is-found-from-outside
  (let [s (ht/clock-on-wait! (h/setup-seeing world nil 700))]
    (swap! (fake/state (:p s)) assoc :yaw 270 :pitch 0)
    (perception/pass! (aget (:p s) "perception"))
    (is (= [[{:x 18 :y 64 :z 16}]] (map :gates (pen-entry/pens {:primitives (:p s)}))))))

(defn seeing-body
  "[p ctx] of world, facing the pen."
  [w]
  (let [p (:p (ht/clock-on-wait! (h/setup-seeing w nil 700)))]
    ((aget p "setOwner") "t1")
    (swap! (fake/state p) assoc :yaw 270 :pitch 0)
    (perception/pass! (aget p "perception"))
    [p (st/stub-ctx p)]))

(def walled-world
  "A tall stone wall inside the pen hides its far half from the body: a look never settles those cells."
  (-> world
      (assoc :entities [(ht/cow 1 19.5 15.5) (ht/cow 2 20.5 17.5)])
      (update :blocks merge (into {} (for [y (range 64 71) z (range 14 19)] [(str "21," y "," z) "stone"])))))

(deftest a-look-that-settles-nothing-is-not-repeated
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[p c] (seeing-body walled-world)
              _ (await (pen-entry/pens! c))
              n (st/looks p)]
          (is (pos? n) "the first call looks at the hidden floor")
          (is (<= n (sense/max-looks)))
          (await (pen-entry/pens! c))
          (is (= n (st/looks p)) "the second call does not look again"))))))

(deftest an-animal-in-the-open-is-not-looked-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[p c] (seeing-body (update walled-world :entities conj (ht/cow 3 17.5 22.5)))
              _ (await (pen-entry/pens! c))]
          (is (seq (st/look-points p)) "the hidden half is looked at")
          (is (every? (fn [[x _ z]] (<= z 19)) (st/look-points p))
              "no look round the cow in the open (z 22)"))))))
