(ns engine.blocks-seal-opening-test
  "jobs.blocks.seal-opening: the openings a body's own stair broke into, and the job against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [engine.blocks-dig-test :as bd]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.blocks.seal-opening :as so]))

(def job 'jobs.blocks.seal-opening)

(def floors
  "A stair cut down east from feet [0 64 0]: the floors of its four steps."
  [[1 62 0] [2 61 0] [3 60 0] [4 59 0]])

(def own (set (for [[x y z] floors dy [1 2 3]] [x (+ y dy) z])))

(def cave "Air beside the lowest step that the stair broke into, and one cell of it further in." [[4 60 1] [4 61 1] [5 60 1]])

(defn rock?
  "The stone mass the stair is cut in: x -2..8, y 55..63, z -3..3."
  [[x y z]]
  (and (<= -2 x 8) (<= 55 y 63) (<= -3 z 3)))

(defn kind-at
  "The seal kind of the course: open in the air over the ground, the stair and the cave, else solid."
  [cell]
  (if (or (not (rock? cell)) (own cell) (some #{cell} cave)) :open :solid))

(deftest the-cave-beside-a-step-is-open-and-the-ground-at-the-stairs-top-is-the-way-back
  (let [seeds (into [[3 61 0] [3 62 0]] (mapcat so/beside) (for [dy [1 2 3]] [1 (+ 62 dy) 0]))
        {:keys [open unseen]} (so/openings kind-at own seeds [3 61 0] 8)]
    (is (= [[4 60 1] [4 61 1]] open) "the cave cells beside the step, not the one further in, nor the air at the top")
    (is (empty? unseen))))

(deftest the-cave-the-body-stands-in-is-the-way-back
  (let [{:keys [open]} (so/openings kind-at own [[5 60 1] [5 61 1]] [3 61 0] 8)]
    (is (= [] (filterv (set cave) open)))
    (is (seq open) "with no seed at the stair's top the air over the ground is no way back")))

(deftest an-unseen-cell-beside-an-own-cell-is-to-look-at-not-to-fill
  (let [unseen [4 60 -1]
        {:keys [open] :as f} (so/openings #(when-not (= unseen %) (kind-at %)) own [[3 64 0]] [3 61 0] 8)]
    (is (= [unseen] (:unseen f)))
    (is (not-any? #{unseen} open))))

(defn key-of [[x y z]] (str/join "," [x y z]))

(def world-blocks
  (into {} (for [x (range -2 9) y (range 55 64) z (range -3 4)
                 :let [cell [x y z]]
                 :when (= :solid (kind-at cell))]
             [(key-of cell) "stone"])))

(defn ^:async run [stair-way]
  (let [env (bd/setup {:self {:pos {:x 3.5 :y 61 :z 0.5}} :inventory [{:name "dirt" :count 8}] :blocks world-blocks})]
    (some->> stair-way (mem/write! (:store (:eng env)) :stair-way))
    [env (await (tu/child-outcome (:eng env) job {:use {:blocks ["dirt"]} :ways #{:carried}} 40))]))

(deftest the-cells-a-stair-broke-into-are-filled-and-its-top-stays-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} e] (await (run {:dim "overworld" :floors floors}))]
          (is (= :done (:status e)))
          (is (= 2 (:sealed e)))
          (is (= ["dirt" "dirt" "air"] (mapv #(bd/block-at p (zipmap [:x :y :z] %)) cave)))
          (is (= "air" (bd/block-at p {:x 0 :y 64 :z 0})) "the ground at the top is not walled"))))))

(deftest with-no-own-cells-near-it-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} e] (await (run nil))]
          (is (= :impossible (:kind (:ending e))))
          (is (= :no-own-cells (:reason e)))
          (is (empty? (bd/calls p "place"))))))))

(deftest with-no-blocks-it-ends-need-with-the-openings-left-and-the-call-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self {:pos {:x 3.5 :y 61 :z 0.5}} :inventory [] :blocks world-blocks})
              _ (mem/write! (:store (:eng env)) :stair-way {:dim "overworld" :floors floors})
              e (await (tu/child-outcome (:eng env) job {:use {:blocks ["dirt"]} :ways #{:carried}} 40))]
          (is (= :condition-failed (:kind (:ending e))))
          (is (= :need (:reason e)))
          (is (= 0 (:sealed e)))
          (is (= 2 (count (:left e))))
          (is (:again (last (:next e)))))))))
