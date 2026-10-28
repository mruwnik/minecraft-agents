(ns world-test.resight-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.resight :as r]))

(def names {1 "water" 2 "oak_door" 3 "oak_door" 4 "stone" 5 "kelp" 6 "fire"})

(defn- section
  "A section at cx sy cz seen at base ms with cells {index [state-id seen-s]}."
  [cx sy cz base cells]
  (let [ids (js/Uint16Array. 4096) times (js/Uint16Array. 4096)]
    (doseq [[i [id s]] cells] (aset ids i (inc id)) (aset times i s))
    #js {:dim "overworld" :cx cx :sy sy :cz cz :base base :ids ids :times times}))

(defn- at-from
  "at over a world {[x y z] state-id}, every column recorded at t-ms."
  [world t-ms]
  (fn [x y z] (when-let [id (get world [x y z])] {:name (names id) :id id :t t-ms})))

(deftest a-cell-counts-changed-when-its-block-or-door-state-differs-and-water-turned-to-plants-stays-water
  (let [;; index 0: x 0 y 0 z 0; index 1: x 1; index 2: x 2; index 3: x 3
        sec (section 0 0 0 0 {0 [1 0] 1 [1 0] 2 [2 0] 3 [6 0]})
        world {[0 0 0] 4 [1 0 0] 5 [2 0 0] 3 [3 0 0] 6}
        rows (r/rows "B" [sec] names (at-from world 100000))]
    (is (= {["B" 0 0 0 :fluid 0] {:n 2 :k 1 :age-s 100}
            ["B" 0 0 0 :door 0] {:n 1 :k 1 :age-s 100}
            ["B" 0 0 0 :fire 0] {:n 1 :k 0 :age-s 100}}
           rows)
        "water to stone changed, water to kelp not; the door's state id differs; the fire stays")))

(deftest a-record-under-a-minute-after-the-sighting-the-fixture-grid-and-unrecorded-columns-count-nothing
  (let [near (section 0 0 0 0 {0 [1 0]})
        grid (section (/ 19008 16) 0 0 0 {0 [1 0]})
        unrecorded (section 5 0 0 0 {0 [1 0]})
        world {[0 0 0] 1 [19008 0 0] 1}]
    (is (empty? (r/rows "B" [near] names (at-from world 60000))))
    (is (empty? (r/rows "B" [grid unrecorded] names (at-from world 600000))))))

(deftest a-row-folded-once-stays
  (is (= {:resight {:a {:n 1 :k 0 :age-s 100}}}
         (r/fold {:resight {:a {:n 1 :k 0 :age-s 100}}} {:a {:n 1 :k 1 :age-s 900}}))))

(deftest each-section-weighs-one-unit-in-the-jeffreys-gamma-posterior
  (let [rows {["A" 0 0 0 :fluid 0] {:n 3 :k 1 :age-s 100}
              ["B" 0 0 0 :fluid 50] {:n 1 :k 0 :age-s 300}
              ["A" 1 0 0 :fluid 0] {:n 10 :k 0 :age-s 1000}}
        {:keys [fluid]} (r/report rows)]
    (is (= {:cells 14 :changed 1 :sections 2} (select-keys fluid [:cells :changed :sections])))
    (is (= 0.75 (:a fluid)) "0.5 + the first section's 1 of 4 changed")
    (is (= 1150 (:b-s fluid)) "(3 * 100 + 300) / 4 + 1000")
    (let [[lo mid hi] (:rate-per-s fluid)]
      (is (< lo mid (/ 0.75 1150) hi)))))
