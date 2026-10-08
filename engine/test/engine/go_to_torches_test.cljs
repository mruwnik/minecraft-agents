(ns engine.go-to-torches-test
  "go-to :place-torches (with :unknown): a dark floor the body cannot see from the takeoff is lit by a torch it places
  there (carried, or fetched once), then walked once seen; the planner prices the torch (jobs.lib.cost/light-cost, a
  caller's :unknown :light-cost overrides). Without the option nothing is placed or fetched."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.go-to-sensed-test :as gs]
            [engine.path.blocks :as blocks]
            [engine.path.planner.base :as base]
            [engine.test-util :as tu]
            [engine.walk-sight-test :as wst]
            [jobs.lib.cost :as cost]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.sight :as ws]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to :as go-to]))

;; ---------------------------------------------------------------- the gate hands a :light step to the walk

(deftest a-light-step-given-up-unseen-asks-for-a-torch
  (let [steps (-> (ws/mark-steps (subvec wst/road 2) (fn [x y z] (= [4 61 0] [x y z]))) (assoc-in [1 :light] true))
        e (assoc (wst/env (constantly nil)) :steps steps)
        s0 (walk/start-state steps {:x 3.5 :z 0.5})
        [_ out] (wst/run-ticks e s0 (assoc wst/still :x 3.5) 200)]
    (is (= {:status :light :step 1 :cells [[4 61 0]]} (select-keys (:done out) [:status :step :cells])) (pr-str out))))

(deftest a-takeoff-where-no-torch-went-joins-the-masks-no-light-set
  (let [pw (wst/masked-pw [[0 63 0 9 63 0 "stone" {}]] #{[5 63 0]})
        u (.-unknown (.-snapshot pw))]
    (set! (.-noLight u) (js/Set.))
    (ws/note-missed! pw {:status :replan :why :no-light :cells [[4 64 0]]})
    (is (= #{(base/cell-key 4 64 0)} (set (.-noLight u))))
    (is (= 0 (.-size (.-missed u))) "not a missed floor")))

;; ---------------------------------------------------------------- the price and the planner's inputs

(deftest a-torch-costs-the-torch-the-placing-and-a-trip-when-none-is-carried
  (let [{carried :cost cp :parts} (cost/light-cost {:carried 3})
        {none :cost np :parts} (cost/light-cost {:carried 0})]
    (is (= #{:torch :place} (set (keys cp))))
    (is (= #{:torch :place :trip} (set (keys np))))
    (is (pos? (:torch cp)))
    (is (= (+ carried (cost/trip)) none))))

(deftest a-torch-stands-on-a-sturdy-floor-or-a-fence-top
  (let [t (blocks/default-state-table)
        base (wworld/torch-base)
        on? (fn [n] (aget base (first (blocks/state-ids t n))))]
    (is (= [1 1 1 0 0 0] (mapv on? ["stone" "oak_fence" "glass" "air" "oak_slab" "water"])))))

(deftest the-unknown-arg-takes-a-light-cost
  (is (nil? (wworld/unknown-problem {:light-cost 3})))
  (is (some? (wworld/unknown-problem {:light-cost -1}))))

;; ---------------------------------------------------------------- go-to down a dark ledge

(def ledge
  "Rock (stone x -4..10, y 59..67, z -2..2) with a corridor (x -2..0, feet y 64, z 0, the body's) whose end is a ledge over
  a low corridor 3 below (x 1..9, feet y 61, open up to y 65)."
  (apply dissoc (tu/box -4 59 -2 10 67 2 "stone")
         (concat (for [x (range -2 1) y [64 65]] (str x "," y ",0"))
                 (for [x (range 1 10) y (range 61 66)] (str x "," y ",0")))))

(defn torch-light
  "Lights the fake world as a torch does once one stands in it: block light 14 less the blocks (x+y+z) from it, round
  the ledge."
  [w]
  (let [torches (keep (fn [[pos n]] (when (#{"torch" "wall_torch"} n) pos)) (:blocks w))
        light (into {} (for [x (range -4 11) y (range 59 68) z (range -2 3)
                             :let [l (apply max 0 (map (fn [[tx ty tz]] (- 14 (+ (js/Math.abs (- x tx)) (js/Math.abs (- y ty)) (js/Math.abs (- z tz))))) torches))]
                             :when (pos? l)]
                         [[x y z] [0 l]]))]
    (if (= light (:light w {})) w (assoc w :light light))))

(defn dark-with-torch-light
  "gs/dark-and-looked (the low corridor's floor is past near, 4, and unseen), and a torch placed lights round it (torch-light)."
  [{:keys [p] :as world}]
  (gs/dark-and-looked world)
  (add-watch (fake/state p) :torch-light (fn [_ ref _ w] (let [w' (torch-light w)] (when-not (identical? w w') (reset! ref w'))))))

(def shielded
  "The body on the ledge, one block west of its rim, a shield in the off hand (no off-hand torch to see deeper), torches carried."
  {:self {:pos {:x -0.5 :y 64 :z 0.5}} :blocks ledge
   :inventory [{:name "torch" :count 4}] :equipment {"offHand" {:name "shield" :count 1}}})

(def below "The goal on the low ground." [9 61 0])

(defn torches-left [p] (reduce + (map :count (filter #(= "torch" (:name %)) (:inventory @(fake/state p))))))
(defn torch-cells [p] (keep (fn [[pos n]] (when (#{"torch" "wall_torch"} n) pos)) (:blocks @(fake/state p))))

(deftest go-to-lights-the-dark-drop-with-a-carried-torch-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (gs/run-go-to shielded {:pos below :range 1 :escalate false :unknown true
                                                                  :place-torches true :ignore-zones? true}
                                                        dark-with-torch-light))]
          (is (= {:arrived true} out) (pr-str out))
          (is (= [[0 65 0]] (torch-cells p)) "one torch on the takeoff, on the wall at the head")
          (is (= 3 (torches-left p)))
          (is (= 1 (count (filter #(= :walk.torch-placed (:kind %)) seen))))
          (is (empty? (filter #(= :gate-breach (:kind %)) seen))))))))

(deftest go-to-without-place-torches-places-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gs/run-go-to shielded {:pos below :range 1 :escalate false :unknown true :ignore-zones? true}
                                                   dark-with-torch-light))]
          (is (= :unseen-floor-dark (:why out)) (pr-str out))
          (is (empty? (torch-cells p)))
          (is (= 4 (torches-left p))))))))

(deftest go-to-with-no-torch-and-no-fetch-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (gs/run-go-to (assoc shielded :inventory [])
                                                        {:pos below :range 1 :escalate false :unknown true
                                                         :place-torches true :fetch false :ignore-zones? true}
                                                        dark-with-torch-light))]
          (is (= :unseen-floor-dark (:why out)) (pr-str out))
          (is (re-find #"no torch" (str (:text out))) (str (:text out)))
          (is (empty? (torch-cells p)))
          (is (empty? (filter #(= :fetch.started (:kind %)) seen))))))))

(deftest go-to-with-no-torch-fetches-torches-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (gs/run-go-to (assoc shielded :inventory [])
                                                      {:pos below :range 1 :escalate false :unknown true
                                                       :place-torches true :ignore-zones? true}
                                                      dark-with-torch-light))
              started (filter #(= :fetch.started (:kind %)) seen)]
          (is (= 1 (count started)) (pr-str (map :kind seen)))
          (is (= "torch" (get-in (first started) [:args :item])))
          (is (= :unseen-floor-dark (:why out)) (str "nothing to fetch torches from " (pr-str out)))
          (is (re-find #"could not fetch" (str (:text out))) (str (:text out))))))))

(deftest go-to-takes-a-place-torches-arg
  (is (contains? go-to/args :place-torches))
  (is (contains? go-to/args :fetch)))
