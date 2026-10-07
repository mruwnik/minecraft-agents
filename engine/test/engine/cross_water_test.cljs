(ns engine.cross-water-test
  "jobs.movement.cross-water against the fake world: launch, drive to the far shore, land and take the boat back."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
            [jobs.movement.cross-water :as cw]))

(def lake
  "A channel of water at y 63 at x 4, z -3..3 in the stone floor: the near bank is x 3, the far bank x 5 (the fake dismount steps one block on
  from the boat, so a wider lake would drop the body into water)."
  (into {} (for [z (range -3 4)] [(str "4,63," z) "water"])))

(def boat-item {:name "oak_boat" :count 1})
(def far {:x 5 :y 64 :z 0})

(def world {:blocks lake :inventory [boat-item]})

(defn ^:async run [world args]
  (let [{:keys [eng p seen]} (lt/setup world)
        res (await (tu/child-outcome eng 'jobs.movement.cross-water args 200))]
    {:res res :p p :seen seen :eng eng}))

(defn aboard? [p] (some? (:vehicle (js->clj (.self p) :keywordize-keys true))))

(def wide "Water at y 63 at x 4..6, z -3..3: the far bank is x 7."
   (into {} (for [x (range 4 7) z (range -3 4)] [(str x ",63," z) "water"])))

(def aboard-world
  "The body in a boat on the wide lake (a placed boat in the fake has no dismount cell: the recover leg needs one on the bank)."
  {:blocks wide
   :entities [{:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 4.5 :y 63.5 :z 0.5} :yaw 0 :health 1 :drops [{:name "oak_boat" :count 1}]
               :dismountAt [7.5 64 0.5]}]
   :self {:vehicle 9 :pos [4.5 63.5 0.5]}})

(deftest aboard-it-skips-the-launch-lands-and-takes-the-boat-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p seen]} (await (run aboard-world {:pos [7 64 0]}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= {:x 7 :y 64 :z 0} (:land res)))
          (is (true? (:recovered res)))
          (is (not (aboard? p)))
          (is (= 1 (get (lt/inv p) "oak_boat")) "the boat came along")
          (is (not-any? #(= :boat.launched (:kind %)) @seen))
          (is (some #(= :boat.landed (:kind %)) @seen)))))))

(deftest recover-false-leaves-the-boat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p seen]} (await (run world {:pos [5 64 0] :recover false}))]
          (is (= :done (:status res)) (pr-str res))
          (is (false? (:recovered res)))
          (is (= far (:land res)))
          (is (some #(= :boat.launched (:kind %)) @seen))
          (is (nil? (get (lt/inv p) "oak_boat"))))))))

(deftest picks-a-far-shore-when-none-is-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run world {:min-cross 4}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 5 (:x (:land res))) "a cell on the far side"))))))

(deftest stops-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label w args reason]
                [["a given cell that is no shore" world {:pos [9 64 0]} :no-far-shore]
                 ["no shore far enough" world {:min-cross 60} :no-far-shore]
                 ["no water within :radius of the body" world {:pos [5 64 0] :radius 1} :not-water]
                 ["no boat and none to fetch" (dissoc world :inventory) {:pos [5 64 0]} :no-boat]]]
          (let [{:keys [res]} (await (run w args))]
            (is (= {:status :stopped :reason reason} (select-keys res [:status :reason])) (pr-str [label res]))))))))

(deftest an-unseen-given-cell-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup (assoc world :unloaded ["5,64,0"]))]
          (core/submit! eng '(jobs.movement.cross-water {:pos [5 64 0]}) {})
          (await (tu/run-until-empty eng 3))
          (is (= [:unseen] (mapv :reason (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest far-shore-prefers-the-far-side
  (let [{:keys [p]} (lt/setup world)]
    (is (= 5 (:x (:land (cw/far-spot p {:x 0 :y 64 :z 0} {:radius 24 :min-cross 4})))))))
