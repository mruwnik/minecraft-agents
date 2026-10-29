(ns engine.base-rates-test
  "jobs.lib.cost.base-rates over the checked-in worldgen numbers (base_rates.json)."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.cost.base-rates :as rates]))

(defn at [y] {:x 0 :y y :z 0})
(def unseen {:biome-at (constantly nil)})
(def in-plains {:biome-at (constantly "plains")})
(defn close? [a b] (< (abs (- a b)) 1e-9))

(deftest iron-is-densest-where-its-placements-say
  (let [iron #(rates/density "iron" (at %) "plains" unseen)]
    ;; ore_iron_middle (10 veins of 9, trapezoid -24..56) and ore_iron_small (10 of 4, uniform -64..72), per chunk layer
    (is (close? (/ (+ (* 10 9 (/ 41 1681)) (* 10 4 (/ 1 137))) 256) (iron 16)))
    (is (= 16 (apply max-key iron (range -64 80))) "the middle of the trapezoid under the mountain band")
    (is (= 232 (apply max-key iron (range -64 320))) "ore_iron_upper's peak")
    (is (= [0 0] [(iron -65) (iron 385)]))))

(deftest a-biome-without-the-placement-has-none
  (is (zero? (rates/density "emerald" (at 100) "plains" unseen)))
  (is (pos? (rates/density "emerald" (at 100) "meadow" unseen)))
  (is (zero? (rates/density "iron" (at 16) "the_end" unseen)))
  (is (zero? (rates/density "cheese" (at 16) "plains" unseen))))

(deftest biome-chance-is-certain-on-seen-ground-and-a-prior-elsewhere
  (is (= [1 0] [(rates/biome-chance "plains" (at 64) in-plains) (rates/biome-chance "desert" (at 64) in-plains)]))
  (is (close? 1 (reduce + (map #(rates/biome-chance % (at 64) unseen) rates/biomes))))
  (is (< 0 (rates/biome-chance "plains" (at 64) unseen) 1))
  (is (zero? (rates/biome-chance "the_end" (at 64) unseen))))

(deftest expected-density-weighs-each-biome-by-its-chance
  (let [meadow (rates/density "emerald" (at 100) "meadow" unseen)]
    (is (zero? (rates/expected-density "emerald" (at 100) in-plains)))
    (is (close? meadow (rates/expected-density "emerald" (at 100) {:biome-at (constantly "meadow")})))
    (is (< 0 (rates/expected-density "emerald" (at 100) unseen) meadow))
    (is (close? (rates/density "diamond" (at -55) "plains" unseen) (rates/expected-density "diamond" (at -55) unseen))
        "diamond is placed alike in every biome")))

(deftest peak-density-of-a-body-without-biomes-is-the-best-expected-density
  (doseq [r ["coal" "iron" "copper" "gold" "redstone" "lapis" "diamond" "emerald" "cheese"]]
    (is (close? (apply max 0 (for [y (range -64 65)] (rates/expected-density r {:y y} nil)))
                (rates/peak-density r nil))
        r)))

(deftest an-ore-tunnels-at-its-densest-layer-above-the-lava
  (is (= {"iron" 16 "copper" 48 "gold" -16 "lapis" 0 "redstone" -54 "diamond" -54 "coal" nil "emerald" nil "cheese" nil}
         (into {} (map (juxt identity rates/best-level)) ["iron" "copper" "gold" "lapis" "redstone" "diamond" "coal" "emerald" "cheese"]))
      "the placements' peaks; redstone and diamond grow denser down to the lava; coal and emerald are densest above 64"))

(deftest an-unseen-cell-is-likelier-rock-the-farther-it-lies-from-where-sight-was-cut
  (is (every? (fn [y] (apply < (map #(rates/unseen-solid y 0 % false nil) [1 3 6]))) [-40 0 30]) "a cut ray's next cell is likelier open than one further on")
  (is (> (rates/unseen-solid -40 0 4 false nil) 0.5) "deep down, 4 past a dark frontier: rock")
  (is (< (rates/unseen-solid 90 0 1 true nil) 0.5) "high up, the next cell past a lit cut: air")
  (is (< (rates/unseen-solid -20 0 4 false 0.25) (rates/unseen-solid -20 0 4 false 0.6)) "walls in reach round the eye: a cave, likelier rock")
  (is (< (js/Math.abs (- (rates/unseen-solid 63 0 3 false nil) (rates/unseen-solid 64 0 3 false nil))) 0.05) "no step at a section border")
  (is (= (rates/unseen-solid 0 0 rates/unseen-d-max false nil) (rates/unseen-solid 0 0 40 false nil)) "past the table's farthest, its farthest")
  (is (> (rates/unseen-solid 70 -4 4 true nil) (rates/unseen-solid 70 0 4 true nil)) "under the floor likelier rock than where the body walks"))

(deftest an-unseen-run-under-a-frontier-is-open-only-if-every-cell-is
  (is (= 1 (rates/open-run-chance 70 0 0 true nil)) "no cell between")
  (is (= (- 1 (rates/unseen-solid 70 0 1 true nil)) (rates/open-run-chance 70 0 1 true nil)) "one cell, next to the frontier")
  (is (< (rates/open-run-chance 42 0 30 false nil) 0.05) "a long dark shaft is unlikely open"))

(deftest the-sky-share-is-the-height-prior-until-the-seen-open-blocks-outweigh-it
  (is (< (rates/sky-share -40 8 0 0) 0.01) "nothing seen deep down: caves")
  (is (> (rates/sky-share 120 15 0 0) 0.9) "nothing seen high up: open air")
  (is (< (rates/sky-share 70 8 0 50) 0.1) "a hall seen shaded at the surface")
  (is (> (rates/sky-share 20 8 50 50) 0.9) "a ravine seen sky-lit deep down")
  (is (> (rates/sky-share 70 8 0 0) (rates/sky-share 70 15 0 0)) "more open blocks have some sky light than the open sky"))
