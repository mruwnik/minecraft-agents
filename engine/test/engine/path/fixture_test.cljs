(ns engine.path.fixture-test
  "engine.path.fixture: the planner tests' hand-built worlds (ported from the JS fixture.test.mjs)."
  (:require [cljs.test :refer [deftest is are testing]]
            [engine.path.fixture :as fx]))

(def name-at (comp :name fx/block-at))

(deftest blocks-land-where-asked-air-elsewhere-in-the-touched-column
  (let [snap (fx/fixture-snapshot {:blocks [[3 64 5 "stone"]]})]
    (are [pos expected] (= expected (apply name-at snap pos))
      [3 64 5] "stone"
      [4 64 5] "air"
      [15 300 15] "air"
      [0 -64 0] "air")))

(deftest untouched-columns-are-unloaded
  (let [snap (fx/fixture-snapshot {:blocks [[3 64 5 "stone"]]})]
    (is (= fx/UNLOADED (.stateAt ^js snap 16 64 5)))
    (is (= fx/UNLOADED (.stateAt ^js snap 3 64 -1)))))

(deftest fill-boxes-apply-before-blocks-inclusive-across-columns
  (let [snap (fx/fixture-snapshot {:fill [[-2 63 -2 17 63 2 "stone"]] :blocks [[0 63 0 "dirt"]]})]
    (are [pos expected] (= expected (apply name-at snap pos))
      [-2 63 -2] "stone"
      [17 63 2] "stone"
      [0 63 0] "dirt"
      [18 63 0] "air")
    (is (.hasColumn ^js snap -1 0))
    (is (.hasColumn ^js snap 1 0))))

(deftest state-id-takes-properties
  (are [name props expected] (= expected (select-keys (:props (fx/block-at-id (fx/state-id name props))) (keys expected)))
    "oak_slab" {:type "bottom"} {:type "bottom"}
    "oak_slab" {:type "top"} {:type "top"}
    "snow" {:layers 3} {:layers "3"}
    "oak_trapdoor" {:facing "north" :open "true"} {:facing "north" :open true}
    "oak_trapdoor" {} {:facing "north" :half "bottom" :open false}))

(deftest unspecified-properties-take-the-default-state
  (is (= (fx/default-state "oak_slab") (fx/state-id "oak_slab" {:type "bottom"})))
  (is (= (fx/default-state "stone") (fx/state-id "stone"))))

(deftest unknown-block-name-throws
  (is (thrown-with-msg? js/Error #"unknown block" (fx/state-id "no_such_block"))))

(deftest layers-builds-blocks-from-ascii-bottom-layer-first
  (is (= [[10 60 20 "stone"] [11 60 20 "stone"] [10 60 21 "stone"] [11 60 21 "air"]
          [10 61 20 "air"] [11 61 20 "air"] [11 61 21 "oak_slab" {:type "top"}]]
         (fx/layers [["SS" "S."] [".." " w"]]
                    {"S" "stone" "w" ["oak_slab" {:type "top"}]}
                    {:x 10 :y 60 :z 20}))))

(deftest posts-join-their-neighbours
  (testing "two fences side by side join east-west only"
    (let [snap (fx/fixture-snapshot {:blocks [[0 64 0 "oak_fence"] [1 64 0 "oak_fence"]]})
          props (:props (fx/block-at snap 0 64 0))]
      (is (= [true false false false] ((juxt :east :west :south :north) props)))))
  (testing "a lone fence joins nothing"
    (let [props (:props (fx/block-at (fx/fixture-snapshot {:blocks [[0 64 0 "oak_fence"]]}) 0 64 0))]
      (is (= [false false false false] ((juxt :east :west :south :north) props)))))
  (testing "a fence joins a full block beside it, not a nether brick fence"
    (let [snap (fx/fixture-snapshot {:blocks [[0 64 0 "oak_fence"] [1 64 0 "stone"] [0 64 1 "nether_brick_fence"]]})
          props (:props (fx/block-at snap 0 64 0))]
      (is (= [true false false false] ((juxt :east :west :south :north) props))))))
