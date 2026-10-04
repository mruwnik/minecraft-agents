(ns engine.path.courses-test
  "engine.path.courses: the live tester's courses replayed into fixture snapshots (ported from the JS courses.test.mjs)."
  (:require [cljs.test :refer [deftest is are testing]]
            [engine.path.courses :as courses]
            [engine.path.fixture :as fx]))

(defn found-at [snapshot [x y z]]
  (let [{:keys [name props]} (fx/block-at snapshot x y z)]
    (assoc props :name name)))

(defn matches? [found expected] (= expected (select-keys found (keys expected))))

(defn replay [cmds]
  (fx/fixture-snapshot {:fill (courses/apply-commands [[0 0 0 15 8 15 "stone"]] cmds)}))

(deftest apply-commands-parses
  (are [cmds pos expected] (matches? (found-at (replay cmds) pos) expected)
    ["setblock 3 4 5 minecraft:cocoa[age=2,facing=north]"] [3 4 5] {:name "cocoa" :age "2" :facing "north"}
    ["setblock 3 4 5 oak_planks"] [3 4 5] {:name "oak_planks"}
    ["setblock 3 4 5 oak_door[facing=east,half=lower,open=true]"] [3 4 5] {:name "oak_door" :open true :half "lower" :facing "east"}
    ["fill 5 6 7 2 4 3 dirt"] [3 5 5] {:name "dirt"}
    ["fill 1 1 1 2 2 2 glass replace"] [2 2 2] {:name "glass"}
    ["fill 1 1 1 4 4 4 glass" "setblock 2 2 2 air"] [2 2 2] {:name "air"}
    ["setblock 2 2 2 glass" "fill 1 1 1 4 4 4 air"] [2 2 2] {:name "air"}
    ["summon minecart 1.5 2 3.5" "kill @e[type=item]" "setblock 1 1 1 glass"] [1 1 1] {:name "glass"}))

(deftest apply-commands-refuses
  (are [cmd why] (thrown-with-msg? js/Error why (courses/apply-commands [] [cmd]))
    "fill 1 1 1 2 2 2 air keep" #"keep"
    "fill 1 1 1 4 4 4 glass hollow" #"hollow"
    "fill 1 1 1 4 4 4 glass outline" #"outline"
    "fill 1 1 1 4 4 4 glass replace stone" #"replace"
    "setblock 1 1 1 glass keep" #"keep"
    "fill ~ 1 1 2 2 2 air" #"relative"
    "clone 1 1 1 2 2 2 5 5 5" #"clone"))

(deftest apply-commands-keeps-its-entries
  (let [before [[0 0 0 1 1 1 "stone"]]
        after (courses/apply-commands before ["setblock 1 1 1 glass"])]
    (is (= 1 (count before)))
    (is (= [0 0 0 1 1 1 "stone"] (first after)))
    (is (= 2 (count after)))))

(deftest course-cells
  (are [course pos expected] (matches? (found-at (:snapshot (courses/course-snapshot course)) pos) expected)
    "cocoa-a2-both-feethead" [2880 161 3216] {:name "cocoa" :age "2" :facing "north"}
    "cocoa-a2-both-feethead" [2880 162 3217] {:name "cocoa" :age "2" :facing "south"}
    "cocoa-a2-both-feethead" [2880 161 3215] {:name "jungle_log"}
    "cocoa-a2-both-feethead" [2880 163 3218] {:name "jungle_log"}
    "cocoa-a2-both-feethead" [2880 164 3218] {:name "air"}
    "cocoa-a2-both-feethead" [2880 163 3216] {:name "air"}
    "cocoa-a0-both-feet" [2880 162 3216] {:name "air"}
    "cocoa-a0-both-feet" [2880 161 3217] {:name "cocoa" :age "0" :facing "south"}
    "cocoa-a2-one-feethead" [2880 161 3217] {:name "air"}
    "cocoa-open-a2-both-feethead" [2880 161 3214] {:name "air"}
    "cocoa-farm-across" [2874 162 3209] {:name "jungle_log"}
    "cocoa-farm-across" [2873 162 3208] {:name "glass"}
    "cocoa-farm-across" [2875 162 3209] {:name "cocoa" :facing "west"}
    "checker-we" [2875 161 3209] {:name "bamboo"}
    "checker-we" [2875 165 3209] {:name "bamboo"}
    "checker-we" [2875 166 3209] {:name "air"}
    "checker-we" [2876 161 3209] {:name "air"}
    "checker-we" [2875 160 3209] {:name "dirt"}
    "checker-we" [2870 160 3209] {:name "stone"}
    "full-walled" [2889 165 3223] {:name "bamboo"}
    "full-walled" [2857 161 3224] {:name "glass"}
    "full-open" [2857 161 3224] {:name "air"}
    "full-open" [2853 161 3216] {:name "glass"}
    "door-open" [2880 161 3216] {:name "oak_door" :open true :half "lower"}
    "door-open" [2880 163 3216] {:name "stone"}
    "tunnel-stairs" [2879 161 3216] {:name "stone_stairs" :facing "east"}
    "tunnel-stairs" [2875 161 3216] {:name "air"}
    "trap-ceil-top" [2878 162 3216] {:name "oak_trapdoor" :half "top" :open false}
    "fence-diag" [2880 161 3216] {:name "oak_fence"}
    "fence-diag" [2881 161 3217] {:name "oak_fence"}
    "fence-diag" [2881 161 3216] {:name "air"}
    "stream3" [2879 159 3216] {:name "water"}
    "stream3" [2879 160 3216] {:name "air"}
    "stream3" [2879 160 3213] {:name "stone"}
    "stream3" [2879 161 3213] {:name "glass"}
    "stream3" [2870 160 3207] {:name "stone"}
    ;; water as the server settles it: a source at the top spills into a waterfall, spreads 7 blocks over the floor, bubble columns
    "waterfall-up" [2880 170 3216] {:name "water" :level "8"}
    "waterfall-up" [2880 180 3216] {:name "water" :level "0"}
    "waterfall-up" [2879 180 3216] {:name "water" :level "1"}
    "waterfall-up" [2879 170 3216] {:name "water" :level "8"}
    "waterfall-up" [2872 161 3216] {:name "water" :level "7"}
    "waterfall-up" [2871 161 3216] {:name "air"}
    "waterfall-up" [2881 180 3216] {:name "stone"}
    "dropshaft-1deep" [2878 161 3216] {:name "water" :level "1"}
    "dropshaft-1deep" [2872 161 3216] {:name "water" :level "7"}
    "dropshaft-1deep" [2871 161 3216] {:name "air"}
    "water20-up" [2880 170 3216] {:name "water" :level "0"}
    "bubble-up" [2880 170 3216] {:name "bubble_column" :drag false}
    "bubble-up" [2880 161 3216] {:name "bubble_column" :drag false}
    "magma-down" [2880 170 3216] {:name "bubble_column" :drag true}
    "bubble-up" [2879 161 3216] {:name "oak_sign"}))

(deftest course-start-and-goal-cells
  (are [course from goal] (let [c (courses/course-snapshot course)]
                            (and (= from (select-keys (:from c) [:x :y :z])) (= goal (:goal c))))
    "cocoa-a2-both-feethead" {:x 2857 :y 161 :z 3217} {:kind "near" :x 2902 :y 161 :z 3216 :range 1}
    "cocoa-a2-both-feethead-diag" {:x 2870 :y 161 :z 3210} {:kind "near" :x 2890 :y 161 :z 3223 :range 1}
    "checker-we" {:x 2857 :y 161 :z 3216} {:kind "near" :x 2902 :y 161 :z 3216 :range 1}
    "lad-shaft20-up" {:x 2857 :y 161 :z 3216} {:kind "near" :x 2885 :y 181 :z 3216 :range 1}
    "lake20-high" {:x 2857 :y 161 :z 3216} {:kind "near" :x 2902 :y 162 :z 3216 :range 1}))

(deftest the-start-keeps-its-exact-position
  (let [{:keys [from]} (courses/course-snapshot "cocoa-a2-both-feethead")]
    (is (= [2857.5 3217] ((juxt :px :pz) from)))))

(deftest every-course-builds
  (let [names (courses/course-names)]
    (is (= (count names) (count (set names))))
    (is (< 100 (count names)))
    (doseq [name names]
      (is (some? (.stateAt ^js (:snapshot (courses/course-snapshot name)) 2860 160 3216)) name))))

(deftest an-unknown-course-is-an-error
  (is (thrown-with-msg? js/Error #"unknown course" (courses/course-snapshot "no-such-course"))))

;; attached blocks the server drops when their support is gone: replayed commands must drop them too
(deftest replay-drops-unsupported
  (are [what cmds spots expected]
       (= expected (mapv (fn [[x y]] (:name (found-at (courses/lane-snapshot :tricky cmds) [x y 3216]))) spots))
    "a ladder whose wall was filled with air"
    ["fill 2871 161 3216 2871 165 3216 stone" "fill 2870 161 3216 2870 164 3216 ladder[facing=west]" "fill 2871 162 3216 2871 163 3216 air"]
    [[2870 161] [2870 162] [2870 163] [2870 164]] ["ladder" "air" "air" "ladder"]
    "a ladder facing north, support at z+1"
    ["setblock 2870 161 3217 stone" "setblock 2870 161 3216 ladder[facing=north]" "setblock 2870 162 3216 ladder[facing=north]"]
    [[2870 161] [2870 162]] ["ladder" "air"]
    "a ladder against glass is kept"
    ["setblock 2871 161 3216 glass" "setblock 2870 161 3216 ladder[facing=west]"] [[2870 161]] ["ladder"]
    "a ladder against a fence is not supported"
    ["setblock 2871 161 3216 oak_fence" "setblock 2870 161 3216 ladder[facing=west]"] [[2870 161]] ["air"]
    "a wall vine whose block is gone"
    ["setblock 2871 161 3216 stone" "setblock 2870 161 3216 vine[east=true]" "setblock 2871 161 3216 air"] [[2870 161]] ["air"]
    "a wall vine on its block"
    ["setblock 2871 161 3216 stone" "setblock 2870 161 3216 vine[east=true]"] [[2870 161]] ["vine"]
    "a vine hanging under a supported vine"
    ["setblock 2871 163 3216 stone" "setblock 2870 163 3216 vine[east=true]" "setblock 2870 162 3216 vine[east=true]" "setblock 2870 161 3216 vine[east=true]"]
    [[2870 161] [2870 162] [2870 163]] ["vine" "vine" "vine"]
    "a top vine under a block"
    ["setblock 2870 162 3216 stone" "setblock 2870 161 3216 vine[up=true]"] [[2870 161]] ["vine"]
    "cocoa whose log is gone (facing north: z-1)"
    ["setblock 2870 161 3215 jungle_log" "setblock 2870 161 3216 cocoa[age=2,facing=north]" "setblock 2870 161 3215 air"] [[2870 161]] ["air"]
    "cocoa on its log"
    ["setblock 2870 161 3215 jungle_log" "setblock 2870 161 3216 cocoa[age=2,facing=north]"] [[2870 161]] ["cocoa"]
    "cocoa on a stone block is not supported"
    ["setblock 2870 161 3215 stone" "setblock 2870 161 3216 cocoa[age=2,facing=north]"] [[2870 161]] ["air"]
    "a torch with nothing under it"
    ["setblock 2870 162 3216 torch" "setblock 2870 161 3216 stone" "setblock 2870 161 3216 air"] [[2870 162]] ["air"]
    "a torch on the lane floor"
    ["setblock 2870 161 3216 torch"] [[2870 161]] ["torch"]
    "a wall torch whose wall is gone"
    ["setblock 2871 161 3216 stone" "setblock 2870 161 3216 wall_torch[facing=west]" "setblock 2871 161 3216 air"] [[2870 161]] ["air"]
    "a wall button whose wall is gone"
    ["setblock 2871 161 3216 stone" "setblock 2870 161 3216 stone_button[face=wall,facing=west]" "setblock 2871 161 3216 air"] [[2870 161]] ["air"]
    "a wall button on its wall"
    ["setblock 2871 161 3216 stone" "setblock 2870 161 3216 stone_button[face=wall,facing=west]"] [[2870 161]] ["stone_button"]
    "a floor lever on the lane floor"
    ["setblock 2870 161 3216 lever[face=floor,facing=north]"] [[2870 161]] ["lever"]
    "a ceiling lever with no ceiling"
    ["setblock 2870 163 3216 lever[face=ceiling,facing=north]"] [[2870 163]] ["air"]))
