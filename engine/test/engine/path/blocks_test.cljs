(ns engine.path.blocks-test
  "engine.path.blocks: the planner's block table (ported from the JS blocks.test.mjs)."
  (:require ["prismarine-registry" :as prismarine-registry]
            [cljs.test :refer [deftest is testing]]
            [engine.path.blocks :as b]
            [engine.path.fixture :as fx]))

(def registry (prismarine-registry b/MC-VERSION))
(def table (b/build-state-table registry))

(defn- at
  "the table's fields of one array at the state id of block name with props"
  [field name props]
  (aget (aget table field) (fx/state-id name props)))

;; [block props top base kind hazard]; heights in 1/16 block
(def state-cases
  [
  ["air" {} 0 16 b/OPEN b/HAZARD-NONE]
  ["stone" {} 16 0 b/SOLID b/HAZARD-NONE]
  ["oak_slab" {:type "bottom"} 8 0 b/SOLID b/HAZARD-NONE]
  ["oak_slab" {:type "top"} 16 8 b/SOLID b/HAZARD-NONE]
  ["oak_slab" {:type "bottom" :waterlogged true} 8 0 b/SOLID b/HAZARD-NONE]
  ["snow" {:layers 1} 0 16 b/OPEN b/HAZARD-NONE]
  ["snow" {:layers 2} 2 0 b/SOLID b/HAZARD-NONE]
  ["snow" {:layers 8} 14 0 b/SOLID b/HAZARD-NONE]
  ["oak_fence" {} 24 0 b/NARROW b/HAZARD-NONE]
  ["cobblestone_wall" {} 24 0 b/NARROW b/HAZARD-NONE]
  ["oak_fence_gate" {:open false} 24 0 b/OPENABLE b/HAZARD-NONE]
  ["oak_fence_gate" {:open true} 0 16 b/OPENABLE b/HAZARD-NONE]
  ["oak_trapdoor" {:half "bottom" :open false} 3 0 b/OPENABLE b/HAZARD-NONE]
  ["oak_trapdoor" {:half "bottom" :open true} 16 0 b/OPENABLE b/HAZARD-NONE]
  ["glass_pane" {} 16 0 b/NARROW b/HAZARD-NONE]
  ["iron_bars" {} 16 0 b/NARROW b/HAZARD-NONE]
  ["bamboo" {} 16 0 b/NARROW b/HAZARD-NONE]
  ["ladder" {} 16 0 b/CLIMB b/HAZARD-NONE]
  ["vine" {} 0 16 b/CLIMB b/HAZARD-NONE]
  ["water" {} 0 16 b/WATER b/HAZARD-NONE]
  ["bubble_column" {} 0 16 b/WATER b/HAZARD-NONE]
  ["seagrass" {} 0 16 b/WATER b/HAZARD-NONE]
  ["oak_sign" {:waterlogged true} 0 16 b/WATER b/HAZARD-NONE]
  ["oak_sign" {:waterlogged false} 0 16 b/OPEN b/HAZARD-NONE]
  ["lava" {} 0 16 b/LAVA b/HAZARD-AVOID]
  ["fire" {} 0 16 b/OPEN b/HAZARD-AVOID]
  ["magma_block" {} 16 0 b/SOLID b/DAMAGE-STAND]
  ["soul_sand" {} 14 0 b/SOLID b/SLOW]
  ["honey_block" {} 15 0 b/SOLID b/SLOW]
  ["campfire" {:lit true} 7 0 b/SOLID b/DAMAGE-STAND]
  ["campfire" {:lit false} 7 0 b/SOLID b/HAZARD-NONE]
  ["sweet_berry_bush" {} 0 16 b/OPEN b/DAMAGE-TOUCH]
  ["wither_rose" {} 0 16 b/OPEN b/DAMAGE-TOUCH]
  ["cactus" {} 15 0 b/SOLID b/DAMAGE-TOUCH]
  ["cobweb" {} 0 16 b/OPEN b/HAZARD-AVOID]
  ["powder_snow" {} 0 16 b/OPEN b/HAZARD-AVOID]
  ["short_grass" {} 0 16 b/OPEN b/HAZARD-NONE]
  ["white_carpet" {} 1 0 b/SOLID b/HAZARD-NONE]
  ["dirt_path" {} 15 0 b/SOLID b/HAZARD-NONE]
  ["farmland" {} 15 0 b/SOLID b/HAZARD-NONE]
  ["mud" {} 14 0 b/SOLID b/HAZARD-NONE]
  ["nether_portal" {:axis "x"} 0 16 b/OPEN b/PORTAL]
  ["end_portal" {} 0 16 b/OPEN b/PORTAL]
  ["end_gateway" {} 0 16 b/OPEN b/PORTAL]
  ["tall_seagrass" {} 0 16 b/WATER b/HAZARD-NONE]
  ["kelp" {} 0 16 b/WATER b/HAZARD-NONE]
  ["big_dripleaf" {:tilt "none" :waterlogged false} 15 0 b/SOLID b/HAZARD-NONE]
])

(deftest state-table
  (doseq [[name props top base kind hazard] state-cases]
    (testing (str name " " props)
      (is (= {:top top :base base :kind kind :hazard hazard}
             {:top (at "top" name props) :base (at "base" name props) :kind (at "kind" name props) :hazard (at "hazard" name props)})))))

;; walking direction that climbs a bottom straight stairs block: 1 east, 2 west, 3 south, 4 north; 0 for anything else
(def stair-cases
  [
  ["oak_stairs" {:facing "east" :half "bottom" :shape "straight"} 1]
  ["oak_stairs" {:facing "west" :half "bottom" :shape "straight"} 2]
  ["oak_stairs" {:facing "south" :half "bottom" :shape "straight"} 3]
  ["oak_stairs" {:facing "north" :half "bottom" :shape "straight"} 4]
  ["stone_brick_stairs" {:facing "north" :half "bottom" :shape "straight"} 4]
  ["oak_stairs" {:facing "north" :half "top" :shape "straight"} 0]
  ["oak_stairs" {:facing "north" :half "bottom" :shape "inner_left"} 0]
  ["oak_stairs" {:facing "north" :half "bottom" :shape "outer_right"} 0]
  ["oak_slab" {:type "bottom"} 0]
  ["stone" {} 0]
])

(deftest stair-up
  (doseq [[name props expected] stair-cases]
    (testing (str name " " props)
      (is (= expected (at "stairUp" name props))))))

(deftest state-table-is-sized-to-the-highest-state-id
  (is (= (inc (reduce (fn [m blk] (max m (.-maxStateId blk))) 0 (.-blocksArray registry))) (.-length (.-top table)))))

(defn- boxes-of [id]
  (mapv (fn [i] (vec (.subarray (.-boxes table) (* 6 (+ (aget (.-boxStart table) id) i)) (+ 6 (* 6 (+ (aget (.-boxStart table) id) i))))))
        (range (aget (.-boxCount table) id))))

;; [block props partial offsetMax]
(def partial-cases
  [
  ["stone" {} 0 0]
  ["air" {} 0 0]
  ["oak_slab" {:type "bottom"} 0 0]
  ["oak_slab" {:type "top"} 0 0]
  ["oak_stairs" {:facing "north" :half "bottom" :shape "straight"} 0 0]
  ["white_carpet" {} 0 0]
  ["oak_trapdoor" {:half "bottom" :open false} 0 0]
  ["oak_trapdoor" {:half "bottom" :open true} 1 0]
  ["cocoa" {:age 2} 1 0]
  ["bamboo" {} 1 0.25]
  ["pointed_dripstone" {} 1 0.125]
  ["glass_pane" {} 1 0]
  ["iron_bars" {} 1 0]
  ["oak_fence" {} 1 0]
  ["cobblestone_wall" {} 1 0]
  ["ladder" {} 1 0]
  ["oak_door" {:open false} 1 0]
  ["lantern" {} 1 0]
  ["iron_chain" {} 1 0]
  ["end_rod" {} 1 0]
  ["lightning_rod" {} 1 0]
  ["candle" {} 1 0]
  ["flower_pot" {} 1 0]
  ["skeleton_skull" {} 1 0]
  ["oak_fence_gate" {:open true} 0 0]
])

(deftest partial-and-offset-max
  (doseq [[name props partial offset-max] partial-cases]
    (testing (str name " " props)
      (is (= {:partial partial :offset-max offset-max}
             {:partial (at "partial" name props) :offset-max (at "offsetMax" name props)})))))

;; [block props boxes] block-local
(def box-cases
  [
  ["air" {} []]
  ["stone" {} [[0 0 0 1 1 1]]]
  ["oak_slab" {:type "bottom"} [[0 0 0 1 0.5 1]]]
  ["cocoa" {:age 2 :facing "west"} [[0.0625 0.1875 0.25 0.5625 0.75 0.75]]]
  ["bamboo" {} [[0.40625 0 0.40625 0.59375 1 0.59375]]]
  ["oak_fence" {:north false :south false :east false :west false} [[0.375 0 0.375 0.625 1.5 0.625]]]
])

(deftest boxes
  (doseq [[name props expected] box-cases]
    (testing (str name " " props)
      (is (= expected (boxes-of (fx/state-id name props)))))))

;; [block props climb climbName facing floor]: climb 1 climbable, 2 open trapdoor (climbable over a ladder of its
;; facing), 3 closed trapdoor a hand opens; climbName 1 ladder, 2 vines, 3 scaffolding; floor: what a body can stand on
(def climb-cases
  [
  ["stone" {} 0 0 0 16]
  ["oak_slab" {:type "bottom"} 0 0 0 8]
  ["ladder" {:facing "west"} 1 1 2 0]
  ["vine" {:east true} 1 2 0 0]
  ["twisting_vines_plant" {} 1 2 0 0]
  ["weeping_vines" {} 1 2 0 0]
  ["cave_vines" {} 1 2 0 0]
  ["scaffolding" {} 1 3 0 16]
  ["oak_trapdoor" {:half "top" :open true :facing "west"} 2 0 2 16]
  ["oak_trapdoor" {:half "top" :open false :facing "east"} 3 0 1 16]
  ["iron_trapdoor" {:half "top" :open false :facing "east"} 0 0 1 16]
  ["iron_trapdoor" {:half "top" :open true :facing "east"} 2 0 1 16]
])

(deftest climb-data
  (doseq [[name props climb climb-name facing floor] climb-cases]
    (testing (str name " " props)
      (is (= {:climb climb :climb-name climb-name :facing facing :floor floor}
             {:climb (at "climb" name props) :climb-name (at "climbName" name props) :facing (at "facing" name props) :floor (at "floor" name props)})))))

(deftest scaffolding-has-no-collision-inside-but-is-a-floor
  (let [id (fx/state-id "scaffolding")]
    (is (= {:top 0 :boxes [] :kind b/CLIMB} {:top (aget (.-top table) id) :boxes (boxes-of id) :kind (aget (.-kind table) id)}))))

;; [block props flowing bubble]: currents push where the water is not a source; a bubble column lifts (drag=false) or drags (drag=true)
(def fluid-cases
  [
  ["water" {:level 0} 0 0]
  ["water" {:level 1} 1 0]
  ["water" {:level 8} 1 0]
  ["seagrass" {} 0 0]
  ["bubble_column" {:drag false} 0 1]
  ["bubble_column" {:drag true} 0 2]
  ["stone" {} 0 0]
])

(deftest fluid-tables
  (doseq [[name props flowing bubble] fluid-cases]
    (testing (str name " " props)
      (is (= {:flowing flowing :bubble bubble} {:flowing (at "flowing" name props) :bubble (at "bubble" name props)})))))

;; [tilt a-floor?]: a big dripleaf leaf holds a body until it tilts
(deftest big-dripleaf-tilt
  (doseq [[tilt floor] [["none" 1] ["unstable" 1] ["partial" 1] ["full" 0]]]
    (testing tilt
      (is (= floor (at "dripleaf" "big_dripleaf" {:tilt tilt :waterlogged false}))))))

(deftest state-ids-lists-every-state-of-a-block-by-name
  (is (= [(fx/state-id "hay_block" {:axis "y"}) (fx/state-id "hay_block" {:axis "x"}) (fx/state-id "hay_block" {:axis "z"})]
         (map #(nth (b/state-ids table "hay_block") %) [1 0 2]))) 
  (is (= 3 (count (b/state-ids table "hay_block"))))
  (is (= [(fx/state-id "stone" {})] (b/state-ids table "stone")))
  (is (nil? (b/state-ids table "no_such_block"))))
