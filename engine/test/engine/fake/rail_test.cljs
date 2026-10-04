(ns engine.fake.rail-test
  "Rails of the fake world as pure functions over cljs world data; the cases of js/fake-rail.test.mjs."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.fake.rail :as rail]))

(def floor (into {} (for [x (range 30)] [[x 63 0] "stone"])))
(def big-floor (into {} (for [x (range 12) z (range 12)] [[x 63 z] "stone"])))

(defn world
  ([blocks] (world blocks {} [0 64 0]))
  ([blocks states body] {:blocks blocks :states states :self {:pos body}}))

(defn row [name xs] (into {} (for [x xs] [[x 64 0] name])))

(defn shape [w pos] (:shape (rail/rail-properties w pos)))
(defn powered [w pos] (:powered (rail/rail-properties w pos)))
(defn shapes [w cells] (mapv #(shape w %) cells))

(defn place-all
  "Place the cells one at a time, as the fake's place does: set the block, then let the rails settle."
  ([w cells] (place-all w cells "rail"))
  ([w cells item]
   (reduce (fn [w pos] (rail/rails-placed (assoc-in w [:blocks pos] item) [{:name item :pos pos}])) w cells)))

(defn dig [w pos] (-> w (update :blocks dissoc pos) (update :states dissoc pos)))

(defn placing
  ([] (placing {} [4 64 2]))
  ([extra body] (world (merge big-floor extra) {} body)))

(defn perms [xs]
  (if (< (count xs) 2)
    [xs]
    (for [i (range (count xs))
          r (perms (concat (take i xs) (drop (inc i) xs)))]
      (cons (nth xs i) r))))

(def straight [[2 64 0] [3 64 0] [4 64 0] [5 64 0] [6 64 0]])
(def corner [[1 64 0] [2 64 0] [3 64 0] [4 64 0] [4 64 1] [4 64 2] [4 64 3]])
(def corner-shapes ["east_west" "east_west" "east_west" "south_west" "north_south" "north_south" "north_south"])
(def climb [[1 64 0] [2 64 0] [3 65 0] [4 65 0]])
(def descend [[1 65 0] [2 65 0] [3 64 0] [4 64 0]])

(deftest rails-in-a-row-read-by-axis
  (let [w (world (merge (row "rail" [2 3 4]) {[10 64 5] "rail" [10 64 6] "powered_rail" [20 64 0] "rail"}))]
    (is (= ["east_west" "east_west" "east_west"] (mapv #(shape w [% 64 0]) [2 3 4])))
    (is (= "north_south" (shape w [10 64 5])))
    (is (= "north_south" (shape w [10 64 6])))
    (is (= "north_south" (shape w [20 64 0])))))

(deftest a-placed-rail-reports-its-shape-after-joining-its-neighbour
  (let [w (place-all (world (assoc floor [2 64 0] "rail") {} [3 64 2]) [[3 64 0]])]
    (is (= "east_west" (shape w [3 64 0])))
    (is (= "east_west" (shape w [2 64 0])))))

(deftest redstone-block-lights-eight-each-way-not-the-ninth
  (let [w (world (merge (row "powered_rail" (range 0 21)) {[10 63 0] "redstone_block"}))]
    (is (= (mapv #(<= (abs (- % 10)) 8) (range 0 21))
           (mapv #(powered w [% 64 0]) (range 0 21))))))

(deftest power-sources
  (doseq [[label source state lit] [["torch" "redstone_torch" nil true]
                                    ["lever on" "lever" {:powered true} true]
                                    ["lever off" "lever" {:powered false} false]
                                    ["stone" "stone" nil false]]]
    (let [w (world (merge (row "powered_rail" [5]) {[5 64 1] source}) (if state {[5 64 1] state} {}) [0 64 0])]
      (is (= lit (powered w [5 64 0])) label))))

(deftest a-normal-rail-breaks-the-run-of-power
  (let [w (world (merge (row "powered_rail" [0 1 3 4]) {[2 64 0] "rail" [0 63 0] "redstone_block"}))]
    (is (= [true true false false] (mapv #(powered w [% 64 0]) [0 1 3 4])))))

(deftest a-stored-state-wins-over-the-worked-out-one
  (let [w (world (row "rail" [2 3 4]) {[3 64 0] {:shape "north_south"}} [0 64 0])]
    (is (= "north_south" (shape w [3 64 0])))
    (is (= "east_west" (shape w [2 64 0])))))

(deftest a-straight-line-comes-out-straight-in-any-order
  (doseq [order [[2 0 4 1 3] [0 1 2 3 4] [4 3 2 1 0] [1 3 0 4 2]]]
    (let [w (place-all (placing) (mapv straight order))]
      (is (= (vec (repeat 5 "east_west")) (shapes w straight)) (pr-str order)))))

(deftest an-l-comes-out-right-in-every-order
  (doseq [order (take-nth 97 (perms (range 7)))]
    (let [w (place-all (placing) (mapv corner order))]
      (is (= corner-shapes (shapes w corner)) (pr-str order)))))

(deftest the-four-corners-take-their-shapes-from-the-arms
  (doseq [[cells expected] [[[[2 64 5] [3 64 5] [4 64 5] [4 64 6] [4 64 7]] "south_west"]
                            [[[2 64 5] [3 64 5] [4 64 5] [4 64 4] [4 64 3]] "north_west"]
                            [[[6 64 5] [5 64 5] [4 64 5] [4 64 6] [4 64 7]] "south_east"]
                            [[[6 64 5] [5 64 5] [4 64 5] [4 64 4] [4 64 3]] "north_east"]]]
    (let [w (place-all (placing {} [4 64 5]) cells)]
      (is (= expected (shape w (nth cells 2)))))))

(deftest a-slope-comes-out-right-in-every-order
  (doseq [order (perms (range 4))]
    (let [w (place-all (placing {[3 64 0] "stone" [4 64 0] "stone"} [4 64 2]) (mapv climb order))]
      (is (= ["east_west" "ascending_east" "east_west" "east_west"] (shapes w climb)) (pr-str order)))))

(deftest a-slope-down-is-the-same-climb-the-other-way
  (doseq [order (perms (range 4))]
    (let [w (place-all (placing {[1 64 0] "stone" [2 64 0] "stone"} [4 64 2]) (mapv descend order))]
      (is (= ["east_west" "east_west" "ascending_west" "east_west"] (shapes w descend)) (pr-str order)))))

(deftest a-rail-beside-the-end-bends-it-and-beside-the-middle-changes-nothing
  (let [end (place-all (place-all (placing) straight) [[6 64 1]])
        middle (place-all (place-all (placing) straight) [[4 64 1]])]
    (is (= "south_west" (shape end [6 64 0])))
    (is (= "north_south" (shape end [6 64 1])))
    (is (= (vec (repeat 5 "east_west")) (shapes middle straight)))
    (is (= "north_south" (shape middle [4 64 1])))))

(deftest a-lone-rail-takes-its-first-shape-from-where-the-body-stands
  (doseq [[body expected] [[[5 64 3] "north_south"] [[8 64 0] "east_west"] [[2 64 0] "east_west"]]]
    (let [w (place-all (placing {} body) [[5 64 0]])]
      (is (= expected (shape w [5 64 0])) (pr-str body)))))

(deftest a-powered-rail-never-takes-a-corner
  (let [w (-> (placing)
              (place-all [[1 64 0] [2 64 0]])
              (place-all [[3 64 0]] "powered_rail")
              (place-all [[3 64 1] [3 64 2]]))]
    (is (contains? #{"north_south" "east_west"} (shape w [3 64 0])))))

(deftest a-broken-rail-leaves-neighbours-and-placing-it-again-joins-them
  (let [w (place-all (placing) corner)
        broken (dig w [4 64 0])]
    (is (= ["east_west" "north_south"] (shapes broken [[3 64 0] [4 64 1]])))
    (is (= corner-shapes (shapes (place-all broken [[4 64 0]]) corner)))))

(deftest rails-written-into-the-world-read-as-placed-in-line-order
  (let [w (world {[0 64 0] "rail" [1 64 0] "rail" [2 64 0] "rail" [2 64 1] "rail" [2 64 2] "rail"
                  [5 64 5] "rail" [6 65 5] "rail" [7 65 5] "rail"})]
    (is (= ["east_west" "east_west" "south_west" "north_south" "north_south"]
           (shapes w [[0 64 0] [1 64 0] [2 64 0] [2 64 1] [2 64 2]])))
    (is (= ["ascending_east" "east_west" "east_west"] (shapes w [[5 64 5] [6 65 5] [7 65 5]])))))

(deftest power-runs-along-slopes-and-stops-at-a-normal-rail
  (let [w (world {[3 64 0] "powered_rail" [4 65 0] "powered_rail" [5 66 0] "powered_rail" [6 66 0] "powered_rail"
                  [7 66 0] "rail" [8 66 0] "powered_rail" [4 65 -1] "redstone_torch"})]
    (is (= [true true true true false]
           (mapv #(powered w %) [[3 64 0] [4 65 0] [5 66 0] [6 66 0] [8 66 0]])))))

(deftest non-rails-have-no-properties
  (is (= {} (rail/rail-properties (world {[0 64 0] "stone"}) [0 64 0]))))

(deftest a-rail-with-two-live-connections-is-full
  (let [w (-> (placing)
              (place-all straight)
              (place-all [[5 64 1]])
              (place-all [[4 64 1]]))]
    (is (= "east_west" (shape w [4 64 1])))))

(deftest a-rail-pointing-at-neighbours-that-do-not-point-back-is-not-full
  (let [w (-> (world (merge big-floor (row "rail" [2 3 4]))
                     {[2 64 0] {:shape "north_south"} [3 64 0] {:shape "east_west"} [4 64 0] {:shape "north_south"}}
                     [7 64 1])
              (place-all [[3 64 1]]))]
    (is (= "north_south" (shape w [3 64 1])))))
