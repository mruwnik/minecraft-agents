(ns plan.shape-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest are is]]
            [plan.shape :as shape]))

;; ---------------------------------------------------------------- reading and checking a plan
(def wheat-plan {:id "p" :parts [{:id "wheat" :box [[0 65 0] [1 65 1]] :want {:crop "wheat"}}]})

(defn errors-of [plan] (shape/plan-errors plan "p"))
(defn parts-erring [plan] (mapv :part (errors-of plan)))
(defn with-part [part] (assoc wheat-plan :parts [part]))

(deftest a-good-plan-has-no-errors
  (are [plan] (= [] (errors-of plan))
    wheat-plan
    (assoc wheat-plan :note "a note")
    (with-part {:id "fence" :outline [[0 64 0] [5 64 5]] :want "oak_fence"})
    (with-part {:id "gate" :cells [[1 64 0] [2 64 0]] :want {:block "oak_fence_gate" :facing :north}})
    (with-part {:id "wall" :box [[0 64 0] [0 66 4]] :want [:any "cobblestone" "stone_bricks"]})
    (with-part {:id "wood" :cells [[3 70 3]] :want {:tree "oak"}})
    (with-part {:id "yard" :box [[0 65 0] [3 66 3]] :want :clear})
    (with-part {:id "hut" :blueprint "hut" :at [10 64 20] :turn 90})
    (with-part {:id "hut" :blueprint "hut" :at [10 64 20]})
    (assoc wheat-plan :assign [{:spot "wheat" :body "Jizo" :use :field}])))

(deftest plan-level-errors-name-no-part
  (are [plan fragment] (some #(and (nil? (:part %)) (str/includes? (:error %) fragment)) (errors-of plan))
    [1 2] "one map"
    (assoc wheat-plan :id "q") "file name"
    (assoc wheat-plan :status :active) "unknown key :status"
    (assoc wheat-plan :status :retired) "unknown key :status"
    (assoc wheat-plan :parts {}) ":parts"
    (assoc wheat-plan :elements []) ":elements"
    (assoc wheat-plan :assign {}) ":assign"
    (assoc wheat-plan :parts [{:id "a" :cells [[0 0 0]] :want "stone"} {:id "a" :cells [[1 0 0]] :want "stone"}]) "unique"))

(deftest a-village-marker-has-an-anchor-and-intent-without-geometric-claims
  (let [p {:id "p" :kind :village :at [1.5 64 -3]
           :metadata {:population {:target 8} :legacy-place {:name "home"}}
           :parts []}]
    (is (= [] (errors-of p)))
    (is (= [] (:cells (shape/expand p {}))))
    (is (= [] (:parts (shape/expand p {}))))
    (are [changed fragment] (some #(str/includes? (:error %) fragment) (errors-of changed))
      (assoc p :at [0 #?(:clj Double/POSITIVE_INFINITY :cljs js/Infinity) 0]) ":at"
      (assoc p :at [1 2]) ":at"
      (assoc p :at nil) ":at"
      (assoc p :kind "village") ":kind"
      (assoc p :kind nil) ":kind"
      (assoc p :metadata []) ":metadata")))

(deftest part-errors-name-the-part
  (are [part fragment] (= [{:part "x" :fragment true}]
                          (mapv #(hash-map :part (:part %) :fragment (str/includes? (:error %) fragment)) (errors-of (with-part part))))
    {:id "x" :want "stone"} "one of :box :outline :cells"
    {:id "x" :box [[0 0 0] [1 1 1]] :cells [[0 0 0]] :want "stone"} "one of :box :outline :cells"
    {:id "x" :box [[0 0 0]] :want "stone"} "two corners"
    {:id "x" :outline [[0 0] [1 1]] :want "stone"} "two corners"
    {:id "x" :cells [] :want "stone"} ":cells"
    {:id "x" :cells [[0 0.5 0]] :want "stone"} ":cells"
    {:id "x" :box [[0 0 0] [1 0 1]]} ":want"
    {:id "x" :box [[0 0 0] [1 0 1]] :want 7} "want"
    {:id "x" :box [[0 0 0] [1 0 1]] :want {:crop "wheat" :harvest true}} "want"
    {:id "x" :box [[0 0 0] [1 0 1]] :want {:tree "oak" :keep :small}} "want"
    {:id "x" :box [[0 0 0] [1 0 1]] :want [:any]} "want"
    {:id "x" :box [[0 0 0] [1 0 1]] :want [:any "stone" :clear]} "want"
    {:id "x" :box [[0 0 0] [1 0 1]] :want {:block 3}} "want"
    {:id "x" :box [[0 0 0] [1 0 1]] :want "stone" :harvest true} ":harvest"
    {:id "x" :box [[0 0 0] [1 0 1]] :want "stone" :yield 3} ":yield"
    {:id "x" :box [[0 0 0] [500 0 500]] :want "stone"} "too large"
    {:id "x" :blueprint "hut"} ":at"
    {:id "x" :blueprint "hut" :at [0 64 0] :turn 45} ":turn"
    {:id "x" :blueprint "hut" :at [0 64 0] :want "stone"} ":want"
    {:id "x" :blueprint "hut" :at [0 64 0] :box [[0 0 0] [1 1 1]]} ":box"))

(deftest a-part-id-is-a-name-without-a-slash
  (are [id] (= [nil] (parts-erring (with-part {:id id :cells [[0 0 0]] :want "stone"})))
    "a/b" "" nil 7))

;; ---------------------------------------------------------------- where: box, outline, cells
(defn cells-of [part] (set (shape/part-cells part)))

(deftest a-box-is-every-cell-whatever-the-corner-order
  (are [box] (= #{[0 64 0] [1 64 0] [0 64 1] [1 64 1] [0 65 0] [1 65 0] [0 65 1] [1 65 1]} (cells-of {:box box}))
    [[0 64 0] [1 65 1]]
    [[1 65 1] [0 64 0]]
    [[0 65 1] [1 64 0]]))

(deftest an-outline-three-blocks-high-is-the-ring-at-every-y
  (let [cells (cells-of {:outline [[0 64 0] [3 66 2]]})]
    (are [expected actual] (= expected actual)
      30 (count cells)                                   ; a 4 x 3 ring is 10 cells, three times
      #{64 65 66} (set (map second cells))
      10 (count (filter #(= 65 (second %)) cells)))
    (are [cell inside?] (= inside? (contains? cells cell))
      [0 64 0] true
      [3 66 2] true
      [1 65 0] true
      [3 65 1] true
      [1 65 1] false                                    ; the inside of the ring
      [2 66 1] false
      [0 67 0] false)))

(deftest an-outline-one-block-wide-is-all-its-cells
  (is (= 4 (count (cells-of {:outline [[0 64 0] [0 64 3]]})))))

(deftest cells-are-listed-once
  (is (= [[0 64 0] [1 64 0]] (shape/part-cells {:cells [[0 64 0] [1 64 0] [0 64 0]]}))))

;; ---------------------------------------------------------------- blueprints
;; Three wide (west to east), two deep (north to south), a floor and one layer above it. The door in the south row
;; faces south at turn 0, the log lies along x.
(def shed
  {:id "shed" :front :south
   :key {"S" "stone" "A" "andesite" "B" "brick" "C" "clay" "D" "dirt" "." :clear
         "E" {:block "oak_door" :facing :south :half :lower}
         "F" {:block "oak_log" :axis :x}}
   :layers [["SSS" "SSS"]
            ["ABC" "DEF"]]
   :spots {"seat" [2 1 1] "corner" [0 0 0]}})

(def blueprints {"shed" shed})

(deftest a-good-blueprint-has-no-errors
  (is (= [] (shape/blueprint-errors shed "shed"))))

(deftest blueprint-errors-say-what-is-wrong
  (are [bp fragment] (some #(str/includes? (:error %) fragment) (shape/blueprint-errors bp "shed"))
    [1] "one map"
    (assoc shed :id "barn") "file name"
    (assoc shed :front :up) ":front"
    (assoc shed :layers []) ":layers"
    (assoc shed :layers [["SSS" "SS"]]) "rows"
    (assoc shed :layers [["SSS" "SSS"] ["SSS"]]) "rows"
    (assoc shed :layers [["SSZ" "SSS"]]) "\"Z\""
    (assoc-in shed [:key "Q"] {:blueprint "hut"}) "blueprint"
    (assoc-in shed [:key "Q"] {:crop "wheat"}) "\"Q\""
    (assoc-in shed [:key "QQ"] "stone") "one letter"
    (assoc shed :parts []) ":parts"
    (assoc shed :blueprint "hut") ":blueprint"
    (assoc-in shed [:spots "far"] [3 0 0]) "far"
    (assoc-in shed [:spots "low"] [0 -1 0]) "low"))

(defn placed [turn] (shape/place shed {:id "shed-1" :at [10 64 20] :turn turn}))
(defn want-at [turn pos] (:want (first (filter #(= pos (:pos %)) (:cells (placed turn))))))

(deftest the-first-layer-is-the-floor
  (are [pos want] (= want (want-at 0 pos))
    [10 64 20] "stone"
    [12 64 21] "stone"
    [10 65 20] "andesite"))

(deftest each-turn-lands-the-shed-in-the-right-cells-with-the-door-turned
  (are [turn pos want] (= want (want-at turn pos))
    0   [10 65 20] "andesite"
    0   [12 65 20] "clay"
    0   [11 65 21] {:block "oak_door" :facing :south :half :lower}
    0   [12 65 21] {:block "oak_log" :axis :x}
    90  [11 65 20] "andesite"                          ; the north row becomes the east column
    90  [11 65 22] "clay"
    90  [10 65 20] "dirt"
    90  [10 65 21] {:block "oak_door" :facing :west :half :lower}
    90  [10 65 22] {:block "oak_log" :axis :z}
    180 [12 65 21] "andesite"
    180 [10 65 21] "clay"
    180 [11 65 20] {:block "oak_door" :facing :north :half :lower}
    180 [10 65 20] {:block "oak_log" :axis :x}
    270 [10 65 22] "andesite"
    270 [10 65 20] "clay"
    270 [11 65 21] {:block "oak_door" :facing :east :half :lower}
    270 [11 65 20] {:block "oak_log" :axis :z}))

(deftest a-turned-shed-lies-east-and-south-of-its-corner
  (are [turn xs zs] (= [xs zs] (let [ps (map :pos (:cells (placed turn)))]
                                 [(set (map first ps)) (set (map last ps))]))
    0   #{10 11 12} #{20 21}
    90  #{10 11}    #{20 21 22}
    180 #{10 11 12} #{20 21}
    270 #{10 11}    #{20 21 22}))

(deftest the-front-turns-clockwise
  (are [turn front] (= front (shape/front-after shed turn))
    0 :south 90 :west 180 :north 270 :east))

(deftest named-spots-come-out-in-world-coordinates
  (are [turn spots] (= spots (:spots (placed turn)))
    0   {"shed-1/seat" [12 65 21] "shed-1/corner" [10 64 20]}
    90  {"shed-1/seat" [10 65 22] "shed-1/corner" [11 64 20]}
    180 {"shed-1/seat" [10 65 20] "shed-1/corner" [12 64 21]}
    270 {"shed-1/seat" [11 65 20] "shed-1/corner" [10 64 22]}))

(deftest turning-turns-directional-state
  (are [want turn turned] (= turned (shape/turn-want want turn))
    "stone" 90 "stone"
    :clear 90 :clear
    {:block "oak_stairs" :facing :north :half :top} 90 {:block "oak_stairs" :facing :east :half :top}
    {:block "oak_stairs" :facing "west" :shape "inner_left"} 90 {:block "oak_stairs" :facing "north" :shape "inner_left"}
    {:block "hopper" :facing :down} 90 {:block "hopper" :facing :down}
    {:block "oak_log" :axis :y} 90 {:block "oak_log" :axis :y}
    {:block "oak_log" :axis :x} 180 {:block "oak_log" :axis :x}
    {:block "oak_log" :axis :z} 270 {:block "oak_log" :axis :x}
    {:block "oak_sign" :rotation 0} 90 {:block "oak_sign" :rotation 4}
    {:block "oak_sign" :rotation 14} 90 {:block "oak_sign" :rotation 2}
    {:block "oak_fence" :north true :east false} 90 {:block "oak_fence" :east true :south false}))

;; ---------------------------------------------------------------- expanding a plan
(def hill-farm
  {:id "hill-farm"
   :parts [{:id "wheat" :box [[200 65 300] [202 65 301]] :want {:crop "wheat"}}
           {:id "carrots" :box [[200 66 303] [202 66 304]] :want {:crop "carrots"}}
           {:id "fence-low" :outline [[199 65 299] [203 65 302]] :want "oak_fence"}
           {:id "fence-high" :outline [[199 66 302] [203 66 305]] :want "oak_fence"}
           {:id "gate-low" :cells [[201 65 299]] :want {:block "oak_fence_gate" :facing :north}}]})

(defn expanded [plan] (shape/expand plan blueprints))
(defn cell-at [expansion pos] (first (filter #(= pos (:pos %)) (:cells expansion))))

(deftest a-terrace-keeps-both-heights
  (let [e (expanded hill-farm)]
    (are [pos part want] (= [part want] ((juxt :part :want) (cell-at e pos)))
      [200 65 300] "wheat" {:crop "wheat"}
      [200 66 303] "carrots" {:crop "carrots"}
      [199 65 302] "fence-low" "oak_fence"
      [199 66 302] "fence-high" "oak_fence")
    (is (nil? (cell-at e [200 65 303])))
    (is (= [] (:errors e)))))

(deftest a-gate-later-in-the-plan-takes-its-cell-from-the-fence
  (let [e (expanded hill-farm)
        parts (into {} (map (juxt :id identity)) (:parts e))]
    (are [expected actual] (= expected actual)
      ["gate-low" {:block "oak_fence_gate" :facing :north}] ((juxt :part :want) (cell-at e [201 65 299]))
      1 (count (filter #(= [201 65 299] (:pos %)) (:cells e)))
      13 (get-in parts ["fence-low" :count])            ; a 5 x 4 ring is 14 cells, less the gate
      1 (get-in parts ["gate-low" :count])
      (+ 6 6 13 14 1) (count (:cells e)))))

(deftest a-placed-blueprint-brings-its-cells-and-spots
  (let [e (expanded {:id "v"
                     :parts [{:id "shed-1" :blueprint "shed" :at [10 64 20] :turn 90}
                             {:id "lectern-1" :cells [[13 65 20]] :want "lectern"}]
                     :assign [{:spot "shed-1/seat" :body "Jizo" :use :seat}
                              {:spot "lectern-1" :profession :librarian :trade "mending"}]})]
    (are [expected actual] (= expected actual)
      [] (:errors e)
      13 (count (:cells e))
      "shed-1" (:part (cell-at e [10 65 21]))
      {"shed-1/seat" [10 65 22] "shed-1/corner" [11 64 20]} (:spots e)
      {:id "shed-1" :where :blueprint :blueprint "shed" :at [10 64 20] :turn 90 :count 12}
      (select-keys (first (:parts e)) [:id :where :blueprint :at :turn :count]))))

(deftest expansion-errors-name-the-part-or-the-assignment
  (are [plan errors] (= errors (:errors (expanded plan)))
    {:id "v" :parts [{:id "barn-1" :blueprint "barn" :at [0 64 0] :turn 0}]}
    [{:part "barn-1" :error "no blueprint called \"barn\""}]

    {:id "v" :parts [{:id "shed-1" :blueprint "shed" :at [0 64 0] :turn 0}]
     :assign [{:spot "shed-1/bed" :body "Jizo" :use :bed} {:spot "lectern-9" :profession :farmer}]}
    [{:assign "shed-1/bed" :error "no part or spot called \"shed-1/bed\""}
     {:assign "lectern-9" :error "no part or spot called \"lectern-9\""}]))

;; ---------------------------------------------------------------- plan minus world
(deftest judge-gives-each-of-the-five-answers
  (are [want block answer] (= answer (shape/judge want block))
    "oak_fence" {:name "oak_fence"} :match
    "oak_fence" {:name "air"} :missing
    "oak_fence" {:name "cave_air"} :missing
    "oak_fence" {:name "cobblestone"} :wrong
    :clear {:name "stone"} :extra
    :clear {:name "water"} :extra
    :clear {:name "air"} :match
    "oak_fence" nil :unknown
    :clear nil :unknown))

(deftest a-crop-matches-at-any-stage-and-the-ground-below-is-not-judged
  (are [want block answer] (= answer (shape/judge want block))
    {:crop "wheat"} {:name "wheat" :state {:age 0}} :match
    {:crop "wheat"} {:name "wheat" :state {:age 7}} :match
    {:crop "wheat"} {:name "wheat"} :match
    {:crop "wheat"} {:name "air"} :missing
    {:crop "wheat"} {:name "carrots"} :wrong
    {:crop "wheat"} {:name "farmland"} :wrong          ; the crop's own cell, a block too low
    {:crop "melon_stem"} {:name "attached_melon_stem"} :match
    {:crop "pumpkin"} {:name "pumpkin_stem"} :match))

(deftest a-tree-spot-holds-a-sapling-or-a-trunk-of-its-species
  (are [want block answer] (= answer (shape/judge want block))
    {:tree "oak"} {:name "oak_sapling"} :match
    {:tree "oak"} {:name "oak_log" :state {:axis "y"}} :match
    {:tree "oak"} {:name "birch_sapling"} :wrong
    {:tree "oak"} {:name "air"} :missing
    {:tree "mangrove"} {:name "mangrove_propagule"} :match
    {:tree "crimson"} {:name "crimson_fungus"} :match
    {:tree "crimson"} {:name "crimson_stem"} :match))

(deftest state-is-judged-only-where-the-want-names-it
  (are [want block answer] (= answer (shape/judge want block))
    {:block "oak_door" :facing :north} {:name "oak_door" :state {:facing "north" :half "upper" :open false}} :match
    {:block "oak_door" :facing :north} {:name "oak_door" :state {"facing" "north"}} :match
    {:block "oak_door" :facing :north} {:name "oak_door" :state {:facing "south"}} :wrong
    {:block "oak_door" :facing :north} {:name "oak_door"} :unknown         ; names only and the state decides
    {:block "oak_door" :facing :north} {:name "birch_door"} :wrong         ; the name decides already
    {:block "oak_door" :facing :north} {:name "air"} :missing
    {:block "oak_door" :open false} {:name "oak_door" :state {:open false}} :match
    {:block "oak_door"} {:name "oak_door"} :match
    "oak_door" {:name "oak_door" :state {:facing "south"}} :match))

(deftest any-of-matches-any-option
  (are [want block answer] (= answer (shape/judge want block))
    [:any "cobblestone" "stone_bricks"] {:name "stone_bricks"} :match
    [:any "cobblestone" "stone_bricks"] {:name "dirt"} :wrong
    [:any "cobblestone" "stone_bricks"] {:name "air"} :missing
    [:any {:block "oak_log" :axis :y} "stone"] {:name "oak_log"} :unknown
    [:any {:block "oak_log" :axis :y} "oak_log"] {:name "oak_log"} :match
    [:any {:block "oak_log" :axis :y} "stone"] {:name "oak_log" :state {:axis "x"}} :wrong))

(deftest plan-minus-world-answers-every-cell
  (let [world {[200 65 300] {:name "wheat" :state {:age 3}}
               [201 65 300] {:name "air"}
               [202 65 300] {:name "farmland"}
               [201 65 299] {:name "oak_fence_gate"}}
        cells (shape/plan-minus-world (:cells (expanded hill-farm)) world)
        by-pos (into {} (map (juxt :pos identity)) cells)]
    (are [pos answer found] (= [answer found] ((juxt :answer :found) (get by-pos pos)))
      [200 65 300] :match "wheat"
      [201 65 300] :missing "air"
      [202 65 300] :wrong "farmland"
      [201 65 299] :unknown "oak_fence_gate"            ; a gate wanted facing north, the world gave the name only
      [199 65 299] :unknown nil)
    (is (= (count (:cells (expanded hill-farm))) (count cells)))))

(deftest a-plan-says-nothing-about-harvesting
  (is (every? #(not-any? #{:harvest :yield} (keys %)) (:cells (expanded hill-farm)))))

(deftest assignments-blocks-cannot-show-answer-unknown
  (is (= [{:spot "lectern-1" :profession :librarian :trade "mending" :answer :unknown}
          {:spot "shed-1/seat" :body "Jizo" :use :seat :answer :unknown}]
         (shape/assignment-answers {:assign [{:spot "lectern-1" :profession :librarian :trade "mending"}
                                             {:spot "shed-1/seat" :body "Jizo" :use :seat}]}))))

(deftest want-text-for-people
  (are [want text] (= text (shape/want-text want))
    "oak_fence" "oak_fence"
    {:block "oak_door" :facing :north :half :lower} "oak_door[facing=north,half=lower]"
    [:any "cobblestone" "stone_bricks"] "cobblestone | stone_bricks"
    {:crop "wheat"} "crop wheat"
    {:tree "oak"} "tree oak"
    :clear "clear"))

(deftest want-block-is-the-block-a-want-is-drawn-as
  (are [want block] (= block (shape/want-block want))
    "oak_fence" "oak_fence"
    {:block "oak_door" :facing :north} "oak_door"
    [:any "cobblestone" {:block "stone_bricks"}] "cobblestone"
    [:any {:block "oak_stairs" :facing :east} "stone"] "oak_stairs"
    {:crop "wheat"} "wheat"
    {:crop "melon"} "melon_stem"
    {:tree "oak"} "oak_leaves"
    {:tree "crimson"} "nether_wart_block"
    :clear nil))

;; ---------------------------------------------------------------- who made a plan
(deftest a-plans-maker-lives-in-metadata-by
  (let [made (shape/with-author wheat-plan "Jizo")]
    (is (= "Jizo" (shape/author made)))
    (is (= [] (errors-of made)) "the shape accepts a plan carrying its maker")
    (is (= "Miles" (shape/author (shape/with-author made "Miles"))) "the last writer is the maker")
    (is (= {:geometry :planned :by "Jizo"} (:metadata (shape/with-author (assoc wheat-plan :metadata {:geometry :planned}) "Jizo"))))
    (is (nil? (shape/author wheat-plan)))
    (is (nil? (shape/author nil)))))
