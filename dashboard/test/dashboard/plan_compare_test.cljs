(ns dashboard.plan-compare-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.plan-compare :as cmp]))

(deftest judge-cells
  (are [want actual status] (= status (cmp/judge want actual))
    {:kind :crop :crop "wheat"} "wheat" :match
    {:kind :crop :crop "wheat"} "carrots" :wrong
    {:kind :crop :crop "wheat"} "air" :missing
    {:kind :crop :crop "wheat"} "farmland" :wrong
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
  (are [want text] (= text (cmp/want-text want))
    {:kind :crop :crop "wheat"} "wheat"
    {:kind :block :block "chest"} "chest"
    {:kind :palette :blocks ["a" "b"]} "a | b"
    {:kind :solid} "any solid block"
    {:kind :air} "air"))

(deftest counts-and-percent
  (are [statuses expected] (= expected (cmp/counts (map (fn [s] {:status s}) statuses)))
    [] {:match 0 :missing 0 :wrong 0 :extra 0 :unknown 0 :total 0 :percent 0}
    [:match :match :missing :wrong] {:match 2 :missing 1 :wrong 1 :extra 0 :unknown 0 :total 4 :percent 50}
    [:match :unknown :unknown] {:match 1 :missing 0 :wrong 0 :extra 0 :unknown 2 :total 3 :percent 33}
    [:extra :extra :match] {:match 1 :missing 0 :wrong 0 :extra 2 :unknown 0 :total 3 :percent 33}))

(def expansion
  {:cells [{:pos [0 64 0] :want {:kind :crop :crop "wheat"} :element "a"}
           {:pos [1 64 0] :want {:kind :crop :crop "wheat"} :element "a"}
           {:pos [0 65 1] :want {:kind :air} :element "b"}
           {:pos [5 64 0] :want {:kind :block :block "stone"} :element "b"}]
   :elements [{:id "a" :kind :plot :content "crop wheat"} {:id "b" :kind :area :content "mixed"} {:id "c" :kind :plan :error "unknown plan"}]
   :errors [{:element "c" :error "unknown plan"}]})

(def world {[0 64 0] "wheat" [1 64 0] "air" [0 65 1] "oak_leaves"})

(deftest compare-plan-totals-and-elements
  (let [r (cmp/compare-plan expansion (fn [x y z] (get world [x y z])))]
    (are [path expected] (= expected (get-in r path))
      [:counts :total] 4
      [:counts :match] 1
      [:counts :missing] 1
      [:counts :extra] 1
      [:counts :unknown] 1
      [:counts :percent] 25
      [:elements 0 :counts :percent] 50
      [:elements 0 :bounds] {:min [0 64 0] :max [1 64 0]}
      [:elements 1 :counts :unknown] 1
      [:elements 2 :counts :total] 0
      [:elements 2 :bounds] nil
      [:errors 0 :element] "c"
      [:grid] {:min-x 0 :min-z 0 :cols 6 :rows 2})))

(deftest compare-plan-layers
  (let [r (cmp/compare-plan expansion (fn [x y z] (get world [x y z])))]
    (are [path expected] (= expected (get-in r path))
      [:layers 0 :y] 64
      [:layers 1 :y] 65
      [:layers 0 :rows 0 0] {:s "match" :e "wheat" :a "wheat" :el "a"}
      [:layers 0 :rows 0 1] {:s "missing" :e "wheat" :a "air" :el "a"}
      [:layers 0 :rows 0 5] {:s "unknown" :e "stone" :a nil :el "b"}
      [:layers 0 :rows 1 0] nil
      [:layers 1 :rows 1 0] {:s "extra" :e "air" :a "oak_leaves" :el "b"})))

(deftest empty-expansion
  (is (= {:counts cmp/zero-counts :elements [] :layers [] :grid nil :errors []}
         (cmp/compare-plan {:cells [] :elements []} (fn [_ _ _] nil)))))
