(ns engine.seen-work-reads-test
  "The work families (farm, forestry, apiary, animals, gather) read the cells they work from what the body sees:
  behind stone a cell is unknown and skipped; in view it is used. The primitives are wrapped; their blockAt throws."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.animals.lead-to :as lead-to]
            [jobs.farm.plant :as plant]
            [jobs.forestry.plant-sapling :as sapling]
            [jobs.gather.get-seeds :as seeds]
            [jobs.lib.apiary :as apiary]))

(defn wrapped
  "Wrapped fake primitives whose raw blockAt throws; wall? puts a stone wall between the body and the cells at z 4."
  [blocks wall?]
  (let [wall (when wall? (into {} (for [y (range 60 70) x (range -2 3)] [(str x "," y ",2") "stone"])))
        p (tu/fake {:blocks (merge blocks wall)})
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})
        w (perception/wrap p per)]
    (aset w "blockAt" (fn [_] (throw (js/Error. "raw blockAt"))))
    w))

(def far {"0,63,4" "stone" "0,64,4" "farmland" "0,65,4" "air"})

(deftest farm-bare-cells-skip-what-is-behind-stone
  (let [box {:min {:x 0 :y 64 :z 4} :max {:x 0 :y 64 :z 4}}]
    (is (= [] (plant/bare-cells (wrapped far true) box [])) "behind stone: unknown, skipped")
    (is (= [{:x 0 :y 64 :z 4}] (plant/bare-cells (wrapped far false) box [])) "in view: used")))

(deftest forestry-sapling-reading-needs-sight
  (let [blocks {"0,64,4" "oak_sapling"}]
    (is (false? (sapling/sapling-at? (wrapped blocks true) {:x 0 :y 64 :z 4})))
    (is (true? (sapling/sapling-at? (wrapped blocks false) {:x 0 :y 64 :z 4})))))

(deftest apiary-block-reader-is-nil-for-an-unseen-cell
  (let [blocks {"0,64,4" "beehive"}]
    (is (nil? ((apiary/seen-block-at-fn (wrapped blocks true)) {:x 0 :y 64 :z 4})))
    (is (= "beehive" (.-name ((apiary/seen-block-at-fn (wrapped blocks false)) {:x 0 :y 64 :z 4}))))))

(deftest animal-fence-reading-needs-sight
  (let [blocks {"0,64,4" "oak_fence"}
        fence? (fn [w] (lead-to/fence-block? {:primitives w :args {:fence {:x 0 :y 64 :z 4}}}))]
    (is (nil? (fence? (wrapped blocks true))))
    (is (true? (fence? (wrapped blocks false))))))

(deftest gather-stalk-cut-cell-needs-sight
  (let [blocks {"0,63,4" "sugar_cane" "0,64,4" "sugar_cane"}
        cut? (fn [w] (seeds/cut-cell? {:primitives w} "sugar_cane" {:x 0 :y 64 :z 4}))]
    (is (not (cut? (wrapped blocks true))))
    (is (true? (cut? (wrapped blocks false))))))
