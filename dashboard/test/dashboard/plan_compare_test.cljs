(ns dashboard.plan-compare-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.plan-compare :as cmp]))

(deftest counts-and-percent
  (are [statuses expected] (= expected (cmp/counts (map (fn [s] {:status s}) statuses)))
    [] {:match 0 :missing 0 :wrong 0 :extra 0 :unknown 0 :total 0 :percent 0}
    [:match :match :missing :wrong] {:match 2 :missing 1 :wrong 1 :extra 0 :unknown 0 :total 4 :percent 50}
    [:match :unknown :unknown] {:match 1 :missing 0 :wrong 0 :extra 0 :unknown 2 :total 3 :percent 33}
    [:extra :extra :match] {:match 1 :missing 0 :wrong 0 :extra 2 :unknown 0 :total 3 :percent 33}))

;; What plan.shape/expand gives for a small plan.
(def expansion
  {:cells [{:pos [0 64 0] :want {:crop "wheat"} :part "a"}
           {:pos [1 64 0] :want {:crop "wheat"} :part "a"}
           {:pos [0 65 1] :want :clear :part "b"}
           {:pos [5 64 0] :want "stone" :part "b"}]
   :parts [{:id "a" :where :box :want {:crop "wheat"} :count 2}
           {:id "b" :where :cells :want "stone" :count 2}
           {:id "c" :where :blueprint :blueprint "barn" :at [9 64 0] :turn 90 :count 0 :error "no blueprint called \"barn\""}]
   :errors [{:part "c" :error "no blueprint called \"barn\""} {:assign "c/bed" :error "no part or spot called \"c/bed\""}]})

(def world {[0 64 0] {:name "wheat"} [1 64 0] {:name "air"} [0 65 1] {:name "oak_leaves"}})

(deftest compare-plan-totals-and-elements
  (let [r (cmp/compare-plan expansion world)]
    (are [path expected] (= expected (get-in r path))
      [:counts :total] 4
      [:counts :match] 1
      [:counts :missing] 1
      [:counts :extra] 1
      [:counts :unknown] 1
      [:counts :percent] 25
      [:elements 0 :counts :percent] 50
      [:elements 0 :bounds] {:min [0 64 0] :max [1 64 0]}
      [:elements 0 :kind] :box
      [:elements 0 :content] "crop wheat"
      [:elements 1 :counts :unknown] 1
      [:elements 2 :content] "blueprint barn, turn 90"
      [:elements 2 :at] [9 64 0]
      [:elements 2 :counts :total] 0
      [:elements 2 :bounds] nil
      [:elements 2 :error] "no blueprint called \"barn\""
      [:errors] [{:element "c" :error "no blueprint called \"barn\""} {:element "c/bed" :error "no part or spot called \"c/bed\""}]
      [:grid] {:min-x 0 :min-z 0 :cols 6 :rows 2})))

(deftest compare-plan-layers
  (let [r (cmp/compare-plan expansion world)]
    (are [path expected] (= expected (get-in r path))
      [:layers 0 :y] 64
      [:layers 1 :y] 65
      [:layers 0 :rows 0 0] {:s "match" :e "crop wheat" :a "wheat" :el "a"}
      [:layers 0 :rows 0 1] {:s "missing" :e "crop wheat" :a "air" :el "a"}
      [:layers 0 :rows 0 5] {:s "unknown" :e "stone" :a nil :el "b"}
      [:layers 0 :rows 1 0] nil
      [:layers 1 :rows 1 0] {:s "extra" :e "clear" :a "oak_leaves" :el "b"})))

(deftest empty-expansion
  (is (= {:counts cmp/zero-counts :elements [] :layers [] :grid nil :errors []}
         (cmp/compare-plan {:cells [] :parts []} (fn [_] nil)))))
