(ns engine.seen-work-reads-b-test
  "The build and items jobs read the cells they work from what the body sees or remembers: behind stone a cell is
  unknown and skipped or looked at; in view it is used. The primitives are wrapped; their blockAt throws."
  (:require [cljs.test :refer [deftest is]]
            [engine.seen-work-reads-test :refer [wrapped]]
            [engine.ctx :as ctx]
            [jobs.build.clear-box :as clear-box]
            [jobs.items.craft :as craft]
            [jobs.items.smelt :as smelt]))

(def far-cell {:x 0 :y 64 :z 4})

(deftest clear-box-pending-skips-what-is-behind-stone
  (let [blocks {"0,64,4" "dirt"}
        args {:from far-cell :to far-cell}
        pending (fn [w] (clear-box/pending {:primitives w :args args}))]
    (is (= [[far-cell nil]] (with-redefs [ctx/mem (constantly {})] (pending (wrapped blocks true)))) "behind stone: unknown, to be walked to")
    (is (= [[far-cell "dirt"]] (with-redefs [ctx/mem (constantly {})] (pending (wrapped blocks false)))) "in view: to dig")))

(deftest furnace-attention-needs-sight
  (let [blocks {"0,64,4" "furnace"}
        attention? (fn [w] (smelt/needs-attention? w far-cell))]
    (is (false? (attention? (wrapped blocks true))) "unseen: not judged")
    (is (true? (attention? (wrapped blocks false))) "seen unlit: needs attention")))
