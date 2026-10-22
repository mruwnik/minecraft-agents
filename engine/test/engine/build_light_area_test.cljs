(ns engine.build-light-area-test
  "jobs.build.light-area against the seeing fake world: dark rooms and corridors are lit with the fewest torches."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.test-util :as tu]
            [jobs.build.light-area :as light-area]
            [jobs.lib.util :as u]))

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

(deftest near-lights-the-cells-round-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run room (constantly false) {:near 3}))]
          (is (= :done (:status r)) (pr-str r))
          (is (= 1 (count (torch-cells p room)))))))))

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

(deftest a-lit-seen-part-with-an-unseen-dark-part-is-looked-at-before-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [corridor (for [x (range 0 41)] [x 0])
              [{:keys [p]} r] (await (run corridor (fn [[x _ _]] (<= x 30)) {:boxes [[[0 64 0] [40 64 0]]]}))]
          (is (= :done (:status r)) (pr-str r))
          (is (seq (torch-cells p corridor)) "the dark far end got a torch, not an instant done")
          (is (every? (fn [[x _ _]] (> x 30)) (torch-cells p corridor))))))))

(deftest an-open-cell-with-its-head-or-floor-unseen-is-not-decided-yet
  (let [seen (fn [known] (fn [_ {:keys [y]}] (known y)))
        half? #(with-redefs [u/seen-name (seen %)] (light-area/half-seen? nil [0 64 0]))]
    (is (true? (half? {64 "air" 65 nil 63 "stone"})) "head unseen")
    (is (true? (half? {64 "air" 65 "air" 63 nil})) "floor unseen")
    (is (false? (half? {64 "air" 65 "air" 63 "stone"})))
    (is (false? (half? {64 "stone" 65 nil 63 nil})) "rock is not a room cell")
    (is (false? (half? {})) "an unseen cell is the other frontier clause")))

(deftest a-field-box-is-lit-to-its-level-with-torches-beside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [box [[3 64 3] [5 64 5]]
              [{:keys [p]} r] (await (run room (constantly false) {:boxes [box] :level 9 :beside? true}))
              hung (torch-cells p room)
              inside? (fn [[x _ z]] (and (<= 3 x 5) (<= 3 z 5)))]
          (is (= :done (:status r)) (pr-str r))
          (is (seq hung))
          (is (not-any? inside? hung) "the field's own cells stay free for crops")
          (is (every? (fn [[x _ z]] (and (<= 2 x 6) (<= 2 z 6))) hung) "only the ring round the field"))))))
