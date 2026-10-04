(ns engine.fake.placing-test
  "The forward placement rule of the fake world over cljs world data; the cases of js/placing.test.mjs."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.fake.placing :as placing]))

(def pi js/Math.PI)
(def yaw {:north 0 :west (/ pi 2) :south pi :east (* 1.5 pi)})
(def up [0 1 0])
(def down [0 -1 0])
(def pos [0 64 0])
(def floor {[0 63 0] "stone"})
(def top [0.5 1 0.5])

(defn place
  [item {:keys [face cursor yaw pitch cells] :or {face up cursor top yaw 0 pitch 0 cells floor}}]
  (placing/placed-blocks {:blocks cells} {:item item :pos pos :face face :cursor cursor :yaw yaw :pitch pitch}))

(defn props [r] (:properties (first (:blocks r))))

(def cases
  [["stairs face the look, the top face gives the bottom half" "oak_stairs" {:yaw (yaw :east)} {:facing "east" :half "bottom"}]
   ["stairs clicked on the underside are top" "oak_stairs" {:face down :yaw (yaw :west)} {:facing "west" :half "top"}]
   ["stairs on a side above the middle are top" "oak_stairs" {:face [-1 0 0] :cursor [0 0.75 0.5] :yaw (yaw :north)} {:facing "north" :half "top"}]
   ["stairs on a side below the middle are bottom" "oak_stairs" {:face [-1 0 0] :cursor [0 0.25 0.5] :yaw (yaw :south)} {:facing "south" :half "bottom"}]
   ["a slab on a side above the middle is top" "oak_slab" {:face [1 0 0] :cursor [1 0.75 0.5]} {:type "top"}]
   ["a slab on the top face is bottom whatever the cursor" "oak_slab" {:cursor [0.5 1 0.5]} {:type "bottom"}]
   ["a log takes the axis of the face, x" "oak_log" {:face [1 0 0]} {:axis "x"}]
   ["a log takes the axis of the face, z" "oak_log" {:face [0 0 -1]} {:axis "z"}]
   ["a log takes the axis of the face, y" "oak_log" {} {:axis "y"}]
   ["a gate faces the look" "oak_fence_gate" {:yaw (yaw :east)} {:facing "east" :open false}]
   ["a chest faces the placer" "chest" {:yaw (yaw :north)} {:facing "south" :type "single"}]
   ["a furnace faces the placer" "furnace" {:yaw (yaw :west)} {:facing "east"}]
   ["a trapdoor on a side faces that side, half by cursor" "oak_trapdoor" {:face [-1 0 0] :cursor [0 0.75 0.5] :yaw (yaw :north)} {:facing "west" :half "top"}]
   ["a trapdoor on a top face faces the placer, bottom" "oak_trapdoor" {:yaw (yaw :east)} {:facing "west" :half "bottom"}]
   ["a trapdoor under a block faces the placer, top" "oak_trapdoor" {:face down :yaw (yaw :south)} {:facing "north" :half "top"}]])

(deftest placing-cases
  (doseq [[label item click want] cases]
    (testing label
      (is (= want (select-keys (props (place item click)) (keys want)))))))

(deftest waterlogged-slab
  (is (true? (:waterlogged (props (place "oak_slab" {:cells (assoc floor [0 64 0] "water")})))))
  (is (false? (:waterlogged (props (place "oak_slab" {}))))))

(deftest door-is-two-blocks
  (let [r (place "oak_door" {:yaw (yaw :north)})]
    (is (= [[64 "lower" "north"] [65 "upper" "north"]]
           (mapv (fn [b] [(get-in b [:pos 1]) (get-in b [:properties :half]) (get-in b [:properties :facing])]) (:blocks r)))))
  (testing "no room for the upper half"
    (is (some? (:refused (place "oak_door" {:cells (assoc floor [0 65 0] "stone")})))))
  (testing "no floor"
    (is (some? (:refused (place "oak_door" {:face [1 0 0] :cells {[-1 64 0] "stone"}}))))))

(deftest bed-is-foot-and-head
  (let [r (place "white_bed" {:yaw (yaw :south)})]
    (is (= [[[0 64 0] "foot" "south"] [[0 64 1] "head" "south"]]
           (mapv (juxt :pos #(get-in % [:properties :part]) #(get-in % [:properties :facing])) (:blocks r)))))
  (is (some? (:refused (place "white_bed" {:yaw (yaw :south) :cells (assoc floor [0 64 1] "stone")})))))

(deftest torches
  (let [cells (assoc floor [1 64 0] "stone")]
    (is (= {:pos pos :name "torch" :properties {}} (first (:blocks (place "torch" {:pitch (- (/ pi 2)) :cells cells})))))
    (is (= {:pos pos :name "wall_torch" :properties {:facing "west"}} (first (:blocks (place "torch" {:yaw (yaw :east) :pitch -0.3 :cells cells})))))
    (is (= "soul_wall_torch" (:name (first (:blocks (place "soul_torch" {:face [-1 0 0] :yaw (yaw :east) :cells {[1 64 0] "stone"}}))))))
    (is (some? (:refused (place "torch" {:yaw (yaw :east) :cells {}}))))))

(deftest ladders
  (is (= {:facing "west" :waterlogged false}
         (props (place "ladder" {:face [-1 0 0] :yaw (yaw :east) :cells {[1 64 0] "stone"}}))))
  (is (some? (:refused (place "ladder" {:yaw (yaw :east) :cells floor})))))

(deftest other-blocks-have-no-state
  (is (= {:blocks [{:pos pos :name "oak_fence" :properties {}}]} (place "oak_fence" {:yaw (yaw :east)}))))

(deftest place-writes-the-world
  (let [[w r] (placing/place {:blocks floor :states {}} {:item "oak_door" :pos pos :face up :cursor top :yaw 0 :pitch 0})]
    (is (= 2 (count (:blocks r))))
    (is (= ["oak_door" "oak_door"] (mapv #(get-in w [:blocks %]) [[0 64 0] [0 65 0]])))
    (is (= "upper" (get-in w [:states [0 65 0] :half]))))
  (let [blocked (assoc floor [0 65 0] "stone")
        [w r] (placing/place {:blocks blocked :states {}} {:item "oak_door" :pos pos :face up :cursor top :yaw 0 :pitch 0})]
    (is (some? (:refused r)))
    (is (= blocked (:blocks w)) "refused places nothing")))
