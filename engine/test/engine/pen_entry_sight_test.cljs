(ns engine.pen-entry-sight-test
  "A fence does not stop sight: the body facing a fenced pen from outside sees the floor and the animals behind it, so
  jobs.lib.pen-entry finds the pen."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.herd-test :as ht]
            [engine.hostile-test :as h]
            [engine.perception :as perception]
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
    (is (= [{:gates [{:x 18 :y 64 :z 16}] :animals 2}]
           (map #(select-keys % [:gates :animals]) (pen-entry/pens {:primitives (:p s)}))))))
