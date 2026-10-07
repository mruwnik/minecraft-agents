(ns engine.boat-land-test
  "jobs.movement.boat-land against the fake world: bring the boat to a shore and step off."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
            [jobs.lib.shore :as shore]))

(defn pond
  "Water at y 63 over x in [x0, x1] and z in [z0, z1]."
  [x0 x1 z0 z1]
  (into {} (for [x (range x0 (inc x1)) z (range z0 (inc z1))] [(str x ",63," z) "water"])))

(def lake (pond 0 20 0 20))

(def east-bank "Stone at y 63 along x 21 (the only land: the fake world has no floor)." (into {} (for [z (range 0 21)] [(str "21,63," z) "stone"])))

(defn world-with
  "The lake with the east bank, with the boat at x z; a dismount lands at the stand cell land."
  [x z land]
  {:blocks (merge lake east-bank)
   :entities [{:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x x :y 63.5 :z z} :yaw 0 :health 1 :drops [{:name "oak_boat" :count 1}] :dismountAt land}]
   :self {:vehicle 9 :pos [x 63.5 z]}})

(def world (world-with 18.5 10.5 {:x 21 :y 64 :z 10}))

(defn ^:async run [world args]
  (let [{:keys [eng p seen]} (lt/setup world)
        res (await (tu/child-outcome eng 'jobs.movement.boat-land args 90))]
    {:res res :p p :seen seen :eng eng}))

(defn aboard? [p] (some? (:vehicle (js->clj (.self p) :keywordize-keys true))))

(deftest lands-on-the-nearest-shore-and-steps-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p seen]} (await (run world {}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= {:x 21 :y 64 :z 10} (:land res)))
          (is (false? (:recovered res)))
          (is (not (aboard? p)))
          (is (some #(= :boat.landed (:kind %)) @seen)))))))

(deftest a-given-land-cell-is-the-shore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run (world-with 18.5 10.5 {:x 21 :y 64 :z 12}) {:pos [21 64 12]}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= {:x 21 :y 64 :z 12} (:land res))))))))

(deftest stops-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label w args reason]
                [["on foot" (dissoc world :self) {} :not-aboard]
                 ["no shore in sight" world {:radius 1} :no-shore]
                 ["the given cell is not a shore" world {:pos [10 64 10]} :no-shore]
                 ["the given shore is across land"
                  (update world :blocks merge (pond 0 10 30 34) (into {} (for [x (range 0 11)] [(str x ",63,35") "stone"])))
                  {:pos [5 64 35]} :blocked]]]
          (let [{:keys [res]} (await (run w args))]
            (is (= {:status :stopped :reason reason} (select-keys res [:status :reason])) (pr-str [label res]))))))))

(deftest shore-needs-seen-cells
  (let [{:keys [p]} (lt/setup (assoc world :unloaded ["21,64,3"]))]
    (is (nil? (shore/spot-at p {:x 21 :y 64 :z 3})) "an unseen stand cell is no shore")
    (is (some? (shore/spot-at p {:x 21 :y 64 :z 4})))))

(deftest an-unseen-given-cell-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup (assoc world :unloaded ["21,64,12"]))]
          (core/submit! eng '(jobs.movement.boat-land {:pos [21 64 12]}) {})
          (await (tu/run-until-empty eng 3))
          (is (= [:unseen] (mapv :reason (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest recover-takes-the-boat-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run world {:recover true}))]
          (is (= :done (:status res)) (pr-str res))
          (is (true? (:recovered res)))
          (is (empty? (filter #(= "oak_boat" (:name %)) (fake/entities p)))))))))


