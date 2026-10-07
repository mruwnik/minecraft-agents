(ns engine.seen-work-reads-test
  "The work families (farm, forestry, apiary, animals, gather) read the cells they work from what the body sees:
  behind stone a cell is unknown and skipped; in view it is used. The primitives are wrapped; their blockAt throws."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.animals.lead-to :as lead-to]
            [jobs.apiary.harvest :as apiary-harvest]
            [jobs.farm.harvest :as harvest]
            [jobs.farm.plant :as plant]
            [jobs.farm.tend :as tend]
            [jobs.farm.tend-plan :as tend-plan]
            [jobs.farm.till :as till]
            [jobs.forestry.maintain :as maintain]
            [jobs.forestry.prepare-rule :as prepare-rule]
            [jobs.forestry.plant-sapling :as sapling]
            [jobs.gather.get-seeds :as seeds]
            [jobs.lib.access :as access]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.look :as look]
            [jobs.lib.tidy-rules :as tidy-rules]
            [jobs.lib.util :as u]))

(defn wrapped
  "Wrapped fake primitives whose raw blockAt throws; wall? puts a stone wall between the body and the cells at z 4."
  [blocks wall?]
  (let [wall (when wall? (into {} (for [y (range 60 70) x (range -2 3)] [(str x "," y ",2") "stone"])))
        p (tu/fake {:blocks (merge blocks wall)})
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})
        w (perception/wrap p per)]
    (perception/pass! per)
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

(deftest sensed-is-nil-without-perception
  (let [bare #js {:blockAt (fn [_] (throw (js/Error. "x-ray blockAt")))}]
    (is (nil? (u/sensed bare {:x 0 :y 64 :z 4})) "no sensedAt: nothing known, blockAt is not read")
    (is (nil? (u/seen-name bare {:x 0 :y 64 :z 4})))))

(deftest rules-input-reads-what-the-body-senses
  (let [blocks {"0,64,4" "lava"}
        at (fn [w] ((:block-at (access/rules-input {:primitives w :args {} :root "j1" :slots [] :view (fn [] {:now 0 :data {}})})) [0 64 4]))]
    (is (= "stone" (at (wrapped blocks true))) "unsensed: taken for rock")
    (is (= "lava" (at (wrapped blocks false))) "in view: what it is")))

(deftest harvest-ripe-reading-needs-sight
  (let [blocks {"0,64,4" "wheat"}
        ripe (fn [w] (vec (harvest/planned-ripe w {} {{:x 0 :y 64 :z 4} "wheat"} [])))]
    (is (= [] (ripe (wrapped blocks true))) "behind stone: unknown, not ripe")))

(def cell {:x 0 :y 64 :z 4})

(defn job-ctx [w args]
  {:primitives w :args args :root "j1" :slots [] :view (fn [] {:now 0 :data {}})})

(deftest till-pending-needs-sight
  (let [pending (fn [w] (till/pending (job-ctx w {:from cell :to cell})))]
    (is (= [[cell nil]] (pending (wrapped far true))) "behind stone: unknown")
    (is (= [] (pending (wrapped far false))) "in view: farmland, nothing to till")))

(deftest tend-plan-reads-need-sight
  (let [blocks {"0,64,4" "farmland" "0,65,4" "wheat"}]
    (is (= {:pos cell :name nil :above nil} (tend-plan/ground-cell (wrapped blocks true) cell)))
    (is (= {:pos cell :name "farmland" :above "wheat"} (tend-plan/ground-cell (wrapped blocks false) cell)))
    (is (= [] (tend-plan/wrong-crops (wrapped {"0,64,4" "carrots"} true) {cell "wheat"})))
    (is (= 1 (count (tend-plan/wrong-crops (wrapped {"0,64,4" "carrots"} false) {cell "wheat"}))))
    (is (= [] (tend-plan/unripe-planned (wrapped {"0,64,4" "wheat"} true) {cell "wheat"})))))

(deftest tend-ground-and-unripe-need-sight
  (let [box {:min cell :max cell}
        blocks {"0,64,4" "farmland" "0,65,4" "wheat"}]
    (is (= [{:pos cell :name nil :above nil}] (tend/ground-layer (wrapped blocks true) box)))
    (is (= [{:pos cell :name "farmland" :above "wheat"}] (tend/ground-layer (wrapped blocks false) box)))
    (is (= [] (tend/unripe-in-box (wrapped {"0,64,4" "wheat"} true) box {:x 0 :y 64 :z 4} 8)))))

(deftest tidy-world-block-needs-sight
  (let [blocks {"0,64,4" "stone"}]
    (is (nil? (tidy-rules/world-block (wrapped blocks true) [0 64 4])))
    (is (= {:name "stone"} (tidy-rules/world-block (wrapped blocks false) [0 64 4])))))

(deftest forestry-classify-and-rule-reads-need-sight
  (let [blocks {"0,63,4" "dirt" "0,64,4" "air" "0,65,4" "air"}]
    (is (= :unloaded (maintain/classify (wrapped blocks true) cell "oak")))
    (is (= :bare (maintain/classify (wrapped blocks false) cell "oak")))
    (is (= {:state :unloaded} (prepare-rule/own-cell (wrapped {"0,64,4" "oak_log"} true) cell "oak")))
    (is (= {:state :grown} (prepare-rule/own-cell (wrapped {"0,64,4" "oak_log"} false) cell "oak")))
    (is (= {:state :unloaded} (prepare-rule/growth-space (wrapped {"0,65,4" "stone"} true) cell "oak" nil)))
    (is (= {:state :unloaded} (prepare-rule/soil-state (wrapped blocks true) {:x 0 :y 64 :z 4} "oak" {:carried #{}})))
    (is (nil? (prepare-rule/soil-state (wrapped blocks false) {:x 0 :y 64 :z 4} "oak" {:carried #{}})))))

(deftest apiary-harvest-classify-needs-sight
  (let [blocks {"0,64,4" "beehive" "0,63,4" "campfire"}
        classify (fn [w] (select-keys (apiary-harvest/classify (job-ctx w {}) [{:pos cell :ripe true}]) [:todo :declined]))]
    (is (= {:todo [] :declined {"0,64,4" :not-smoked}} (classify (wrapped blocks true))) "unseen fire: not smoked")
    (is (= {:todo [cell] :declined {}} (classify (wrapped blocks false))))))

(deftest live-seen-blocks-check-through-the-senses-not-raw-blockat
  (let [q {:names ["grass_block"] :radius 8 :live? true}]
    (is (= 1 (count (look/seen-blocks (wrapped {"0,63,4" "grass_block"} false) q))) "in view: still there")))
