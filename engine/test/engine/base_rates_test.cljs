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

(deftest the-solid-share-is-the-depth-prior-until-the-seen-blocks-outweigh-it
  (is (> (rates/solid-share -40 0 0) 0.3) "nothing seen deep down: walls close all round")
  (is (< (rates/solid-share 120 0 0) 0.05) "nothing seen high up: open air")
  (is (< (rates/solid-share 0 400 2000) 0.2) "a hall seen open deep down")
  (is (> (rates/solid-share 120 300 100) 0.5) "a tunnel seen in rock high up")
  (is (< (js/Math.abs (- (rates/solid-share 63 0 0) (rates/solid-share 64 0 0))) 0.01) "no step at a section border")
  (is (= (rates/solid-prior -500) (rates/solid-prior -56)) "past the table's ends, its end window"))

(deftest the-sky-share-is-the-height-prior-until-the-seen-open-blocks-outweigh-it
  (is (< (rates/sky-share -40 8 0 0) 0.01) "nothing seen deep down: caves")
  (is (> (rates/sky-share 120 15 0 0) 0.9) "nothing seen high up: open air")
  (is (< (rates/sky-share 70 8 0 50) 0.1) "a hall seen shaded at the surface")
  (is (> (rates/sky-share 20 8 50 50) 0.9) "a ravine seen sky-lit deep down")
  (is (> (rates/sky-share 70 8 0 0) (rates/sky-share 70 15 0 0)) "more open blocks have some sky light than the open sky"))
