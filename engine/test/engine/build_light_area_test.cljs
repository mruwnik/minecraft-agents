(ns engine.build-light-area-test
  "jobs.build.light-area against the seeing fake world: dark rooms and corridors are lit with the fewest torches."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.test-util :as tu]))

(def job 'jobs.build.light-area)
(def torches [{:name "torch" :count 16}])

(defn key-of [[x y z]] (str x "," y "," z))

(defn world
  "Stone with the open cells (feet and head levels y 64 and 65, floor y 63) carved in; their light is 14 where lit says, else 0."
  [open lit inventory]
  (let [cells (for [[x z] open y [64 65]] [x y z])
        env (bd/setup-seeing
             {:self {:pos {:x (+ 0.5 (ffirst open)) :y 64 :z (+ 0.5 (second (first open)))}}
              :blocks (apply dissoc (tu/box -3 62 -3 40 67 40 "stone") (map key-of cells))
              :inventory inventory :drops {"torch" "torch"}})]
    (swap! (.. (:p env) -world -state) merge {:light (into {} (for [c cells] [c (if (lit c) [14 0] [0 0])]))})
    env))

(def room (for [x (range 0 9) z (range 0 9)] [x z]))
(def l-corridor (concat (for [x (range 0 21)] [x 0]) (for [z (range 1 21)] [20 z])))

(defn ^:async run [open lit args & [inventory]]
  (let [env (world open lit (or inventory torches))
        r (await (tu/child-outcome (:eng env) job args 400))]
    [env r]))

(defn torch-cells [p open]
  (for [[x z] open y [64 65] :when (re-find #"torch" (or (bd/block-at p {:x x :y y :z z}) ""))] [x y z]))

(deftest a-dark-room-takes-one-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run room (constantly false) {:boxes [[[0 64 0] [8 64 8]]]}))]
          (is (= :done (:status r)) (pr-str r))
          (is (= 1 (count (torch-cells p room))))
          (is (every? (fn [[x _ z]] (and (<= 1 x 7) (<= 1 z 7))) (torch-cells p room)) "a corner torch would leave the far corner unlit"))))))

(deftest a-dark-l-corridor-takes-two-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run l-corridor (constantly false)
                                          {:boxes [[[0 64 0] [20 64 0]] [[20 64 1] [20 64 20]]]}))]
          (is (= :done (:status r)) (pr-str r))
          (is (= 2 (count (torch-cells p l-corridor))) (pr-str r (torch-cells p l-corridor))))))))

(deftest a-lit-room-hangs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run room (constantly true) {:boxes [[[0 64 0] [8 64 8]]]}))]
          (is (= :done (:status r)) (pr-str r))
          (is (empty? (torch-cells p room))))))))

(deftest no-torch-and-none-to-get-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run room (constantly false) {:boxes [[[0 64 0] [8 64 8]]]} []))]
          (is (not= :done (:status r)) (pr-str r))
          (is (empty? (torch-cells p room))))))))
