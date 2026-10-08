(ns engine.corridor-look-test
  "One sight pass down a 1x2 corridor: a lone ore block in its wall 13 cells down is seen in most passes when the corridor is
  lit (so mine's glance along its tunnel is enough) and not at all in the dark (no x-ray, as for a player)."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.rays :as rays]
            [engine.test-util :as tu]))

(defn hits [ore-pos passes & [dark?]]
  (let [blocks (merge (tu/box -3 62 -3 3 68 20 "stone")
                      (tu/box 0 64 0 0 65 20 "air")
                      {ore-pos "iron_ore"})
        clock (atom 1000000)
        p (tu/fake {:blocks blocks})
        _ (when dark? (swap! (fake/state p) assoc :light-default [0 0]))
        raw (fake-raw/create p)
        per (perception/create raw {:now #(deref clock)})]
    (count (filter true?
                   (for [_ (range passes)]
                     (do (swap! clock + 100000)
                         (rays/pass! per)
                         (= "iron_ore" (:name (perception/seen-block per (mapv js/parseInt (.split ore-pos ",")))))))))))

(deftest a-lit-corridor-shows-a-lone-wall-ore-13-cells-down-in-most-passes
  (doseq [o ["1,64,13" "1,65,13" "-1,64,13"]]
    (is (<= 20 (hits o 40)) o)))

(deftest a-dark-corridor-hides-it-as-it-would-from-a-player
  (is (zero? (hits "1,64,13" 10 true))))
