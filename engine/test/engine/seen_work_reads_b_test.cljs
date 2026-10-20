(ns engine.seen-work-reads-b-test
  "The build and items jobs read the cells they work from what the body sees or remembers: behind stone a cell is
  unknown and skipped or looked at; in view it is used. The primitives are wrapped; their blockAt throws."
  (:require [cljs.test :refer [deftest is]]
            [engine.seen-work-reads-test :refer [wrapped]]
            [engine.ctx :as ctx]
            [jobs.blocks.clear-cells :as clear-cells]
            [jobs.items.craft :as craft]
            [jobs.items.smelt :as smelt]))

(def far-cell {:x 0 :y 64 :z 4})

(deftest clear-cells-pending-skips-what-is-behind-stone
  (let [blocks {"0,64,4" "dirt"}
        args {:area {:min far-cell :max far-cell}}
        pending (fn [w] (clear-cells/pending {:primitives w :args args}))]
    (is (= [[far-cell nil]] (with-redefs [ctx/mem (constantly {})] (pending (wrapped blocks true)))) "behind stone: unknown, to be walked to")
    (is (= [[far-cell "dirt"]] (with-redefs [ctx/mem (constantly {})] (pending (wrapped blocks false)))) "in view: to dig")))

(deftest furnace-attention-needs-sight
  (let [blocks {"0,64,4" "furnace"}
        attention? (fn [w] (smelt/needs-attention? w far-cell))]
    (is (false? (attention? (wrapped blocks true))) "unseen: not judged")
    (is (true? (attention? (wrapped blocks false))) "seen unlit: needs attention")))

(deftest clear-cells-refuses-an-unseen-cell-in-a-zone
  (let [zone {:name "farm" :min [0 60 0] :max [0 70 5] :owner "Miles"}
        in {:block-at (constantly "stone") :floor-at (constantly nil) :feet [5 64 5] :ledger #{}
            :zones [zone] :footprints {} :plan-cells #{} :claims [] :self "Fake" :own-plans #{} :now 0 :ignore-zones? false}
        sorted (clear-cells/split-by-rules in #{:fluid-adjacent :falling-block} [[far-cell nil]])]
    (is (= [] (:allowed sorted)) "a zone refuses the cell whether or not it is in view")
    (is (= [far-cell] (map first (:refused sorted))))))
