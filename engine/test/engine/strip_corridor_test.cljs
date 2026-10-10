(ns engine.strip-corridor-test
  "jobs.strip.corridor: one mineshaft corridor segment stripped, in the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [jobs.lib.strip :as strip]))

(def job 'jobs.strip.corridor)
(def seg {:y 64 :axis :x :line 0 :from 0 :to 12 :open-ends #{}})
(def args {:segment (assoc seg :id (strip/seg-id seg)) :entry [-3 64 0]})
(def kit [{:name "iron_pickaxe" :count 1} {:name "iron_axe" :count 1} {:name "shears" :count 1}])
(def drops {"rail" "rail" "cobweb" "string" "oak_fence" "oak_fence" "oak_planks" "oak_planks" "torch" "torch" "wall_torch" "torch"})
(def bridge [8 63 0])
(def web [4 65 0])
(def wood-cells
  (concat (for [x [2 6 10] z [-1 1] y [64 65]] [x y z])
          (for [x [2 6 10] z [-1 0 1]] [x 66 z])))
(def rail-cells (for [x (range 0 13)] [x 64 0]))

(def corridor
  "A corridor along x in rock, 3 wide and 3 high, supports at x 2 6 10, rails on the centre line, a cobweb, and one floor plank
  with air under it (a bridge)."
  (let [air (set (for [x (range -5 16) y [64 65 66] z [-1 0 1]] (str x "," y "," z)))
        key (fn [[x y z]] (str x "," y "," z))]
    (merge (apply dissoc (tu/box -6 63 -2 16 67 2 "stone") air)
           (into {} (map (fn [c] [(key c) (if (= 66 (second c)) "oak_planks" "oak_fence")])) wood-cells)
           (into {} (map (fn [c] [(key c) "rail"])) rail-cells)
           {(key web) "cobweb" (key bridge) "oak_planks"})))

(defn world [& {:keys [inventory entities zones] :or {inventory kit}}]
  (bd/setup {:self {:pos {:x -3 :y 64 :z 0}} :blocks corridor :inventory inventory :drops drops :entities (or entities [])
             :zones (or zones [])}))

(defn dark-world
  "A seeing world whose floor level in the corridor is dark (light 0) after the first sight pass; the rest stays lit so the corridor is seen."
  [inventory]
  (let [env (bd/setup-seeing {:self {:pos {:x -3 :y 64 :z 0}} :blocks corridor :drops drops :inventory inventory})]
    (swap! (.. (:p env) -world -state) merge {:light (into {} (for [x (range -5 16) z [-1 0 1]] [[x 64 z] [0 0]]))})
    env))

(defn run! [{:keys [eng]} a] (bd/child-outcome eng job a 400))
(defn gone? [p cells] (every? #(#{"air" "cave_air"} (bd/block-at p %)) cells))
(defn cart [id pos] {:id id :name "chest_minecart" :kind "other" :pos pos :health 5
                     :items [{:name "iron_ingot" :count 3}] :drops [{:name "chest_minecart" :count 1}]})

(deftest supports-rails-and-web-go-the-bridge-plank-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world)
              p (:p env)
              r (await (run! env args))]
          (is (= {:status :done :taken {:wood 21 :rail 13 :cobweb 1}} (select-keys r [:status :taken])))
          (is (gone? p (concat wood-cells rail-cells [web])))
          (is (= "oak_planks" (bd/block-at p bridge)) "the plank that floors a gap stays")
          (is (= [{:pos {:x 8 :y 63 :z 0} :why :bridge}] (:kept r))))))))

(deftest torches-in-the-segment-are-taken-back-after-the-rest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self {:pos {:x -3 :y 64 :z 0}} :blocks (assoc corridor "3,64,1" "torch" "9,65,-1" "wall_torch")
                             :inventory kit :drops drops})
              r (await (run! env args))]
          (is (= {:status :done :taken {:wood 21 :rail 13 :cobweb 1 :torch 2}} (select-keys r [:status :taken])))
          (is (= 2 (bd/carried (:p env) "torch"))))))))

(deftest a-zombie-in-the-corridor-declines-danger-with-nothing-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0} :health 20}])
              why (await (bd/waiting-after env (list job args) 3))]
          (is (= :danger (:reason why)))
          (is (empty? (bd/calls (:p env) "dig"))))))))

(deftest a-bag-at-want-free-stops-bag-full-after-the-first-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [i] {:name (str "filler_" i) :count 1}) (range 21))
              env (world :inventory (into kit filler))
              r (await (run! env args))]
          (is (= {:status :stopped :reason :bag-full :taken {:rail 1}} (select-keys r [:status :reason :taken])))
          (is (= 1 (count (bd/calls (:p env) "dig")))))))))

(deftest a-chest-minecart-in-the-segment-is-looted-by-the-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :entities [(cart 50 [5 64 1])])
              p (:p env)
              r (await (run! env args))]
          (is (= :done (:status r)))
          (is (empty? (filter #(= "chest_minecart" (:name %)) (fake/entities p))))
          (is (= 3 (bd/carried p "iron_ingot")))
          (is (= 1 (bd/carried p "chest_minecart"))))))))

(deftest without-shears-the-web-stops-no-tool-naming-the-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :inventory (vec (take 2 kit)))
              r (await (run! env args))]
          (is (= {:status :stopped :reason :no-tool :tool "shears"} (select-keys r [:status :reason :tool])))
          (is (= "cobweb" (bd/block-at (:p env) web))))))))

(deftest a-refused-dig-ends-the-trip-with-the-childs-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :zones [{:name "farm" :min [0 60 -3] :max [12 70 3] :owner "Miles"}])
              r (await (run! env args))]
          (is (= {:status :stopped :reason :refused} (select-keys r [:status :reason])))
          (is (empty? (bd/calls (:p env) "dig"))))))))

(deftest a-dark-corridor-gets-a-torch-before-the-first-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as env} (dark-world (into kit [{:name "torch" :count 8}]))
              r (await (run! env args))]
          (is (= {:status :done} (select-keys r [:status :reason :child :cell])))
          (is (pos? (count (filter #(= "torch" (.-item (.-args %))) (bd/calls p "place")))))
          (is (= 8 (bd/carried p "torch")) "the torches it hung are taken back at the end"))))))

(deftest no-torch-in-the-dark-stops-no-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as env} (dark-world kit)
              r (await (run! env args))]
          (is (= {:status :stopped :reason :no-torches} (select-keys r [:status :reason])))
          (is (empty? (bd/calls p "dig"))))))))

(deftest the-segment-may-be-given-as-its-id
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world)
              r (await (run! env (assoc args :segment (:id (:segment args)))))]
          (is (= {:status :done :taken {:wood 21 :rail 13 :cobweb 1}} (select-keys r [:status :taken]))))))))

(deftest no-segment-stops-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! (world) {:segment "nonsense"}))]
          (is (= {:status :stopped :reason :bad-args} (select-keys r [:status :reason]))))))))
