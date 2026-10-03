(ns plan.shape-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest are is]]
            [plan.shape :as shape]))

(def region {:min [0 64 0] :max [1 64 1]})
(def good {:id "p" :region region :elements [{:id "a" :kind :plot :region region :content {:crop "wheat"}}]})

(defn errors-of [plan] (shape/plan-errors plan "p"))

(deftest valid-plans-have-no-errors
  (are [p] (empty? (errors-of p))
    good
    (assoc good :status :done :kind :farm :name "N" :owner "O")
    (assoc good :elements [])
    (dissoc good :elements)
    (assoc good :elements [{:id "w" :kind :structure :at [1 2 3] :rotation 90 :content {:blueprint "well"}}])
    (assoc good :elements [{:id "c" :kind :plan :ref "other"}])
    (assoc good :elements [{:id "b" :kind :border :region region :content {:palette ["a" "b"]}}
                           {:id "k" :kind :area :region region :content {:air true}}])))

(deftest invalid-plans-name-their-problem
  (are [p fragment] (some #(str/includes? % fragment) (errors-of p))
    (assoc good :id "other") "must equal the file name"
    (dissoc good :region) "region must be"
    (assoc good :region {:min [0 0 0] :max [-1 0 0]}) "must not exceed"
    (assoc good :region {:min [0 0] :max [1 1 1]}) ":min and :max"
    (assoc good :status :bogus) ":status must be one of"
    (assoc good :elements {:a 1}) ":elements must be a vector"
    (assoc good :elements [(first (:elements good)) (first (:elements good))]) "unique"
    (assoc good :elements [{:id "a" :kind :plot :content {:crop "wheat"}}]) "region must be"
    (assoc good :elements [{:id "a" :kind :plot :region region}]) "content must be a map"
    (assoc good :elements [{:id "a" :kind :plot :region region :content {:crop "wheat" :block "x"}}]) "exactly one"
    (assoc good :elements [{:id "a" :kind :plot :region region :content {:palette []}}]) ":palette must be"
    (assoc good :elements [{:id "a" :kind :plot :region region :content {:air false}}]) ":air must be true"
    (assoc good :elements [{:id "a" :kind :plot :region region :content {:blueprint "x"}}]) "belongs to a :structure"
    (assoc good :elements [{:id "a" :kind :structure :content {:blueprint "x"}}]) "needs :at"
    (assoc good :elements [{:id "a" :kind :structure :at [0 0 0] :rotation 45 :content {:blueprint "x"}}]) ":rotation must be"
    (assoc good :elements [{:id "a" :kind :structure :at [0 0 0] :content {:crop "x"}}]) "{:blueprint name}"
    (assoc good :elements [{:id "a" :kind :plan}]) "needs :ref"
    (assoc good :elements [{:id "a b" :kind :plot}]) ":id must be"
    (assoc good :elements [{:kind :plot}]) ":id must be"))

(deftest border-keeps-the-ring-of-every-layer
  (are [region border? n] (= n (count (shape/region-cells region border?)))
    {:min [0 0 0] :max [4 0 3]} false 20
    {:min [0 0 0] :max [4 0 3]} true 14
    {:min [0 0 0] :max [4 1 3]} true 28
    {:min [0 0 0] :max [0 0 0]} true 1
    {:min [0 0 0] :max [2 0 0]} true 3
    {:min [0 0 0] :max [2 0 2]} true 8))

(deftest rotation-is-clockwise-seen-from-above
  (are [rotation offset expected] (= expected (shape/rotate rotation offset))
    0 [3 1] [3 1]
    90 [3 1] [-1 3]
    180 [3 1] [-3 -1]
    270 [3 1] [1 -3]))

(deftest blueprint-cells-are-placed-at-the-anchor
  (let [cells [{:dx 0 :dy 0 :dz 0 :names ["cobblestone"]} {:dx 2 :dy 1 :dz 0 :names ["oak_planks" "birch_planks"]}
               {:dx 1 :dy 0 :dz 0 :air true} {:dx 1 :dy 1 :dz 0 :names ["@solid"]}]]
    (are [rotation expected] (= expected (map (juxt :pos :want) (shape/structure-cells {:at [10 70 20] :rotation rotation} cells)))
      0 [[[10 70 20] {:kind :block :block "cobblestone"}] [[12 71 20] {:kind :palette :blocks ["oak_planks" "birch_planks"]}]
         [[11 70 20] {:kind :air}] [[11 71 20] {:kind :solid}]]
      90 [[[10 70 20] {:kind :block :block "cobblestone"}] [[10 71 22] {:kind :palette :blocks ["oak_planks" "birch_planks"]}]
          [[10 70 21] {:kind :air}] [[10 71 21] {:kind :solid}]])))

(def blueprints {"hut" [{:dx 0 :dy 0 :dz 0 :names ["stone"]} {:dx 1 :dy 0 :dz 0 :names ["stone"]}]})
(def plans
  {"farm" {:id "farm" :region region
           :elements [{:id "plot" :kind :plot :region {:min [0 64 0] :max [1 64 0]} :content {:crop "wheat"}}
                      {:id "fence" :kind :border :region {:min [0 64 0] :max [2 64 2]} :content {:block "oak_fence"}}]}
   "town" {:id "town" :region region
           :elements [{:id "hut" :kind :structure :at [5 64 5] :content {:blueprint "hut"}}
                      {:id "farm" :kind :plan :ref "farm"}]}
   "bad" {:id "bad" :region region
          :elements [{:id "ghost" :kind :structure :at [0 0 0] :content {:blueprint "nope"}}
                     {:id "lost" :kind :plan :ref "nowhere"}
                     {:id "huge" :kind :plot :region {:min [0 0 0] :max [999 999 999]} :content {:air true}}]}
   "a" {:id "a" :region region :elements [{:id "to-b" :kind :plan :ref "b"}]}
   "b" {:id "b" :region region :elements [{:id "to-a" :kind :plan :ref "a"}]}})

(defn expand [id] (shape/expand-plan plans id blueprints))

(deftest expansion-of-a-plan
  (let [{:keys [cells elements errors]} (expand "farm")]
    (is (= [4 8] (map :count elements)) "two crop cells and the two farmland cells below")
    (is (= ["plot" "fence"] (map :id elements)))
    (is (= [] errors))
    (is (= 12 (count cells)))
    (is (= #{"plot" "fence"} (set (map :element cells))))))

(deftest children-roll-up-into-one-element
  (let [{:keys [cells elements]} (expand "town")]
    (is (= [2 12] (map :count elements)))
    (is (= "crop wheat" (:content (first (:elements (expand "farm"))))))
    (is (= "plan farm" (:content (second elements))))
    (is (= 14 (count cells)))
    (is (= {"hut" 2 "farm" 12} (frequencies (map :element cells))))))

(deftest broken-elements-report-errors-and-keep-the-rest
  (let [{:keys [elements errors]} (expand "bad")]
    (are [i fragment] (str/includes? (get-in elements [i :error]) fragment)
      0 "unknown blueprint"
      1 "unknown plan"
      2 "too large")
    (is (= ["ghost" "lost" "huge"] (map :element errors)))
    (is (= [0 0 0] (map :count elements)))))

(deftest cycles-are-an-error-not-a-hang
  (let [{:keys [elements errors]} (expand "a")]
    (is (= ["to-b/to-a"] (map :element errors)))
    (is (str/includes? (:error (first errors)) "cycle: a -> b -> a"))
    (is (str/includes? (get-in elements [0 :error]) "inside plan b"))
    (is (= [0] (map :count elements)))))

(deftest child-errors-surface-under-the-parent-element
  (let [p (assoc plans "top" {:id "top" :region region :elements [{:id "inner" :kind :plan :ref "bad"}]})
        {:keys [errors]} (shape/expand-plan p "top" blueprints)]
    (is (= ["inner/ghost" "inner/lost" "inner/huge"] (map :element errors)))))

(deftest children-lists-the-nested-plans
  (are [id expected] (= expected (shape/children (get plans id)))
    "town" ["farm"]
    "farm" []
    "bad" ["nowhere"]))

(deftest judge-cells
  (are [want actual status] (= status (shape/judge want actual))
    {:kind :crop :crop "wheat"} "wheat" :match
    {:kind :crop :crop "wheat"} "carrots" :wrong
    {:kind :crop :crop "wheat"} "air" :missing
    {:kind :crop :crop "wheat"} "farmland" :missing
    {:kind :crop :crop "wheat"} "dirt" :wrong
    {:kind :farmland} "farmland" :match
    {:kind :farmland} "dirt" :missing
    {:kind :farmland} "grass_block" :missing
    {:kind :farmland} "coarse_dirt" :missing
    {:kind :farmland} "air" :missing
    {:kind :farmland} "stone" :wrong
    {:kind :farmland} "wheat" :wrong
    {:kind :farmland} nil :unknown
    {:kind :crop :crop "wheat"} nil :unknown
    {:kind :crop :crop "melon_stem"} "melon_stem" :match
    {:kind :crop :crop "melon_stem"} "attached_melon_stem" :match
    {:kind :crop :crop "melon"} "attached_melon_stem" :match
    {:kind :crop :crop "pumpkin"} "pumpkin_stem" :match
    {:kind :crop :crop "melon"} "melon" :wrong
    {:kind :block :block "oak_fence"} "oak_fence" :match
    {:kind :block :block "oak_fence"} "birch_fence" :wrong
    {:kind :block :block "oak_fence"} "cave_air" :missing
    {:kind :palette :blocks ["water" "oak_slab"]} "oak_slab" :match
    {:kind :palette :blocks ["water" "oak_slab"]} "dirt" :wrong
    {:kind :palette :blocks ["water" "oak_slab"]} "void_air" :missing
    {:kind :solid} "stone" :match
    {:kind :solid} "lava" :wrong
    {:kind :solid} "air" :missing
    {:kind :air} "air" :match
    {:kind :air} "cave_air" :match
    {:kind :air} "void_air" :match
    {:kind :air} "oak_leaves" :extra
    {:kind :air} nil :unknown))

(deftest want-texts
  (are [want text] (= text (shape/want-text want))
    {:kind :crop :crop "wheat"} "wheat"
    {:kind :block :block "chest"} "chest"
    {:kind :palette :blocks ["a" "b"]} "a | b"
    {:kind :solid} "any solid block"
    {:kind :air} "air"))


(deftest crops-on-farmland-want-the-ground
  (are [crop n] (= n (count (:cells (shape/element-cells {:kind :plot :region {:min [0 64 0] :max [1 64 0]} :content {:crop crop}} {}))))
    "wheat" 4
    "melon_stem" 4
    "sugar_cane" 2
    "nether_wart" 2))

(deftest the-ground-cell-is-one-below
  (is (= [{:pos [3 64 5] :want {:kind :crop :crop "carrots"}} {:pos [3 63 5] :want {:kind :farmland}}]
         (:cells (shape/element-cells {:kind :plot :region {:min [3 64 5] :max [3 64 5]} :content {:crop "carrots"}} {})))))
