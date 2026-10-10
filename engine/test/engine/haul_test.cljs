(ns engine.haul-test
  "jobs.storage.haul against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [child-outcome]]
            [engine.triggers :as triggers]
            [jobs.storage.haul :as haul]))

(defn setup [world]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (mem/write! (:store eng) :chest {:pos {:x 20 :y 64 :z 0}} mem/place-policy)
    {:eng eng :p p}))

(defn stacks [prefix n] (mapv (fn [i] {:name (str prefix i) :count 5}) (range n)))
(defn containers [p] (:containers @(fake/state p)))
(defn transfers [p] (filterv #(= "transfer" (.-name %)) (.-calls (.-world p))))
(defn direction [call] (.-direction (.-args call)))

(deftest load-plan-takes-whole-stacks-up-to-the-free-slots-and-leaves-kept-names
  (let [stacks [{:name "a" :count 3} {:name "keep" :count 9} {:name "b" :count 4} {:name "a" :count 2} {:name "c" :count 1}]]
    (is (= {"a" 5 "b" 4} (haul/load-plan stacks ["keep"] 3 [])))
    (is (= {"a" 13 "b" 4} (haul/load-plan stacks ["keep"] 3 [{:name "a" :count 8}])))
    (is (= {} (haul/load-plan stacks [] 0 [])))))

(deftest haul-moves-two-chests-in-three-bag-loads
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" (stacks "a" 40) "10,64,4" (stacks "b" 40) "20,64,0" []}})
              result (await (child-outcome eng 'jobs.storage.haul {:from [[10 64 0] [10 64 4]]} 120))
              cs (containers p)]
          (is (= {:gave-up false :trips 3} (select-keys result [:gave-up :trips])))
          (is (= 400 (reduce + (vals (:moved result)))))
          (is (empty? (get cs [10 64 0])))
          (is (empty? (get cs [10 64 4])))
          (is (= 80 (count (get cs [20 64 0])))))))))

(deftest haul-with-a-full-bag-deposits-before-it-takes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (stacks "x" 35) :containers {"10,64,0" (stacks "a" 5) "20,64,0" []}})
              result (await (child-outcome eng 'jobs.storage.haul {:from [[10 64 0]]} 60))]
          (is (= "deposit" (direction (first (transfers p)))))
          (is (= 40 (count (get (containers p) [20 64 0]))))
          (is (= 2 (:trips result))))))))

(deftest haul-leaves-the-kept-names-in-the-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" [{:name "a" :count 5} {:name "keepme" :count 7}] "20,64,0" []}})
              result (await (child-outcome eng 'jobs.storage.haul {:from [[10 64 0]] :keep ["keepme"]} 60))]
          (is (= false (:gave-up result)))
          (is (= [{:name "keepme" :count 7}] (map #(select-keys % [:name :count]) (get (containers p) [10 64 0]))))
          (is (= ["a"] (map :name (get (containers p) [20 64 0])))))))))
