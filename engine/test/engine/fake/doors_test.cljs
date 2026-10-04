(ns engine.fake.doors-test
  "Doors, gates and trapdoors of the fake world over cljs world data; the cases of js/fake-doors.test.mjs."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake.doors :as doors]))

(defn world [blocks states] {:blocks blocks :states states})

(deftest only-an-openable-block-with-open-true-is-open
  (let [w (world {[0 64 0] "oak_fence_gate" [1 64 0] "oak_fence_gate" [2 64 0] "stone" [3 64 0] "iron_door"}
                 {[0 64 0] {:open true} [1 64 0] {:open false} [2 64 0] {:open true} [3 64 0] {:open true}})]
    (is (= [true false false true false] (mapv #(doors/open? w [% 64 0]) [0 1 2 3 4])))))

(deftest flipping-a-gate-toggles-it
  (let [w (world {[0 64 0] "oak_fence_gate"} {})
        opened (doors/flip-open w [0 64 0])
        shut (doors/flip-open opened [0 64 0])]
    (is (true? (get-in opened [:states [0 64 0] :open])))
    (is (false? (get-in shut [:states [0 64 0] :open])))))

(deftest flipping-either-half-of-a-door-moves-both
  (let [door (world {[0 64 0] "oak_door" [0 65 0] "oak_door"}
                    {[0 64 0] {:half "lower" :open false} [0 65 0] {:half "upper" :open false}})
        opens (fn [w] (mapv #(get-in w [:states % :open]) [[0 64 0] [0 65 0]]))]
    (is (= [[true true] [true true]] (mapv #(opens (doors/flip-open door %)) [[0 64 0] [0 65 0]])))))

(deftest planner-properties-of-a-door
  (let [w (world {[0 64 0] "oak_door" [1 64 0] "stone"}
                 {[0 64 0] {:half "lower" :open true :facing "east" :locked true} [1 64 0] {:open true}})]
    (is (= {:open true :half "lower" :facing "east"} (doors/path-props w [0 64 0])))
    (is (= {} (doors/path-props w [1 64 0])))))

(deftest planner-properties-of-a-button
  (let [w (world {[0 64 0] "stone_button"} {[0 64 0] {:face "wall" :facing "west" :powered false :other 1}})]
    (is (= {:facing "west" :face "wall" :powered false} (doors/path-props w [0 64 0])))))

(deftest planner-properties-of-a-ladder
  (let [w (world {[0 64 0] "ladder"} {[0 64 0] {:facing "south" :other 1}})]
    (is (= {:facing "south"} (doors/path-props w [0 64 0])))))

(deftest open-trapdoor-over-a-ladder-of-its-facing-is-climbed
  (let [w (world {[0 64 0] "ladder" [0 65 0] "oak_trapdoor"
                  [3 64 0] "stone" [3 65 0] "oak_trapdoor"
                  [6 64 0] "ladder" [6 65 0] "oak_trapdoor"
                  [9 64 0] "ladder" [9 65 0] "oak_trapdoor"}
                 {[0 64 0] {:facing "east"} [0 65 0] {:open true :facing "east"}
                  [3 65 0] {:open true :facing "east"}
                  [6 64 0] {:facing "east"} [6 65 0] {:open false :facing "east"}
                  [9 64 0] {:facing "east"} [9 65 0] {:open true :facing "west"}})]
    (is (= [true false false false] (mapv #(doors/climbs-through? w %) [[0 65 0] [3 65 0] [6 65 0] [9 65 0]])))))
