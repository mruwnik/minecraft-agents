(ns engine.access.rules-test
  "engine.access.rules: the verdict table over tiny hand-made block lookups."
  (:require [cljs.test :refer [deftest is are]]
            [engine.access.rules :as rules]))

(def feet [0 64 0])
(def farm {:name "farm" :min [4 60 4] :max [6 70 6]})

(defn world
  "A lookup over the cells given as alternating cell and name; everything else is not loaded (nil)."
  [& cells]
  (let [m (apply hash-map cells)]
    (fn [pos] (get m pos))))

(defn dig
  "may-dig? over the world, the cell and any extra input."
  [w cell & {:as extra}]
  (rules/may-dig? (merge {:block-at w :cell cell :feet feet :zones [] :footprints #{} :ledger #{}} extra)))

(defn place
  [w cell & {:as extra}]
  (rules/may-place? (merge {:block-at w :cell cell :feet feet :zones [] :footprints #{} :ledger #{}} extra)))

(defn verdict
  "The verdict cut to what a row compares: :ok and :reason."
  [v]
  (select-keys v [:ok :reason]))

(def plain (world [5 64 5] "stone"))
(def under-feet [0 63 0])

(deftest dig-verdicts
  (are [w cell extra expected] (= expected (verdict (apply dig w cell (mapcat identity extra))))
    ;; plain stone
    plain [5 64 5] {} {:ok true}
    ;; not loaded
    plain [9 9 9] {} {:ok false :reason :not-loaded}
    ;; fluids in any of the six neighbours
    (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5] {} {:ok false :reason :fluid-adjacent}
    (world [5 64 5] "stone" [4 64 5] "water") [5 64 5] {} {:ok false :reason :fluid-adjacent}
    (world [5 64 5] "stone" [5 65 5] "water") [5 64 5] {} {:ok false :reason :fluid-adjacent}
    (world [5 64 5] "stone" [5 63 5] "lava") [5 64 5] {} {:ok false :reason :fluid-adjacent}
    (world [5 64 5] "stone" [5 64 6] "bubble_column") [5 64 5] {} {:ok false :reason :fluid-adjacent}
    (world [5 64 5] "stone" [5 64 4] "bubble_column") [5 64 5] {} {:ok false :reason :fluid-adjacent}
    ;; a diagonal fluid is not a neighbour
    (world [5 64 5] "stone" [6 65 5] "water") [5 64 5] {} {:ok true}
    ;; falling blocks above, body under them in the same column
    (world [0 66 0] "stone" [0 67 0] "gravel") [0 66 0] {} {:ok false :reason :falling-block}
    (world [0 66 0] "stone" [0 67 0] "sand") [0 66 0] {} {:ok false :reason :falling-block}
    (world [0 66 0] "stone" [0 67 0] "red_sand") [0 66 0] {} {:ok false :reason :falling-block}
    (world [0 66 0] "stone" [0 67 0] "white_concrete_powder") [0 66 0] {} {:ok false :reason :falling-block}
    ;; the same gravel over a cell that is not over the body
    (world [3 66 3] "stone" [3 67 3] "gravel") [3 66 3] {} {:ok true}
    ;; a solid block between the cell and the body stops the fall
    (world [0 67 0] "stone" [0 68 0] "gravel" [0 66 0] "stone") [0 67 0] {} {:ok true}
    ;; gravel below the body's head does not matter
    (world [0 62 0] "stone" [0 63 0] "gravel") [0 62 0] {} {:ok true}
    ;; the cell under the feet
    (world under-feet "stone" [0 62 0] "stone") under-feet {} {:ok true}
    (world under-feet "stone" [0 62 0] "air") under-feet {} {:ok false :reason :under-feet}
    (world under-feet "stone" [0 62 0] "water") under-feet {} {:ok false :reason :fluid-adjacent}
    (world under-feet "stone" [0 62 0] "magma_block") under-feet {} {:ok false :reason :under-feet}
    (world under-feet "stone") under-feet {} {:ok false :reason :under-feet}
    (world under-feet "stone" [0 62 0] "air") under-feet {:ledger #{under-feet}} {:ok true}
    ;; a cell beside the feet is not the cell under them
    (world [1 63 0] "stone" [1 62 0] "air") [1 63 0] {} {:ok true}
    ;; zones
    plain [5 64 5] {:zones [farm]} {:ok false :reason :zone}
    plain [5 64 5] {:zones [(assoc farm :allow #{:place})]} {:ok false :reason :zone}
    plain [5 64 5] {:zones [(assoc farm :allow #{:dig})]} {:ok true}
    plain [5 64 5] {:zones [(assoc farm :allow #{:dig :place})]} {:ok true}
    plain [5 64 5] {:zones [(assoc farm :min [7 60 7] :max [8 70 8])]} {:ok true}
    ;; footprints
    plain [5 64 5] {:footprints #{[5 64 5]}} {:ok false :reason :footprint}
    plain [5 64 5] {:footprints #{[5 64 6]}} {:ok true}
    ;; no zone list
    plain [5 64 5] {:zones nil} {:ok false :reason :no-zones}
    ;; order: not loaded, footprint, zone, hazards, no-zones
    plain [9 9 9] {:zones nil :footprints #{[9 9 9]}} {:ok false :reason :not-loaded}
    (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5] {:footprints #{[5 64 5]} :zones [farm]} {:ok false :reason :footprint}
    (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5] {:zones [farm]} {:ok false :reason :zone}
    (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5] {:zones nil} {:ok false :reason :fluid-adjacent}))

(deftest dig-details
  (is (= {:ok false :reason :zone :zone "farm"} (dig plain [5 64 5] :zones [farm])))
  (is (= {:ok false :reason :fluid-adjacent :fluid "lava" :at [6 64 5]}
         (dig (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5])))
  (is (= {:ok false :reason :falling-block :block "gravel" :at [0 67 0]}
         (dig (world [0 66 0] "stone" [0 67 0] "gravel") [0 66 0])))
  (is (= {:ok true} (dig plain [5 64 5]))))

(def air-spot (world [5 64 5] "air" [0 64 0] "air" [0 65 0] "air"))

(deftest place-verdicts
  (are [w cell extra expected] (= expected (verdict (apply place w cell (mapcat identity extra))))
    air-spot [5 64 5] {} {:ok true}
    (world [5 64 5] "cave_air") [5 64 5] {} {:ok true}
    (world [5 64 5] "short_grass") [5 64 5] {} {:ok true}
    plain [5 64 5] {} {:ok false :reason :not-replaceable}
    ;; fluids are placed into like air
    (world [5 64 5] "water") [5 64 5] {} {:ok true}
    (world [5 64 5] "lava") [5 64 5] {} {:ok true}
    (world [5 64 5] "bubble_column") [5 64 5] {} {:ok true}
    ;; a fluid cell is still refused for the earlier reasons
    (world [5 64 5] "water") [5 64 5] {:zones [farm]} {:ok false :reason :zone}
    (world [5 64 5] "lava") [5 64 5] {:footprints #{[5 64 5]}} {:ok false :reason :footprint}
    (world [0 64 0] "water") [0 64 0] {} {:ok false :reason :own-body}
    (world [5 64 5] "water") [5 64 5] {:zones nil} {:ok false :reason :no-zones}
    air-spot [9 9 9] {} {:ok false :reason :not-loaded}
    ;; the body's own feet and head cells
    air-spot [0 64 0] {} {:ok false :reason :own-body}
    air-spot [0 65 0] {} {:ok false :reason :own-body}
    (world [0 66 0] "air") [0 66 0] {} {:ok true}
    ;; zones and footprints
    air-spot [5 64 5] {:zones [farm]} {:ok false :reason :zone}
    air-spot [5 64 5] {:zones [(assoc farm :allow #{:dig})]} {:ok false :reason :zone}
    air-spot [5 64 5] {:zones [(assoc farm :allow #{:place})]} {:ok true}
    air-spot [5 64 5] {:footprints #{[5 64 5]}} {:ok false :reason :footprint}
    air-spot [5 64 5] {:zones nil} {:ok false :reason :no-zones}
    ;; order: footprint before zone before body before replaceable before no-zones
    air-spot [5 64 5] {:footprints #{[5 64 5]} :zones [farm]} {:ok false :reason :footprint}
    plain [5 64 5] {:zones [farm]} {:ok false :reason :zone}
    air-spot [0 64 0] {:zones nil} {:ok false :reason :own-body}
    plain [5 64 5] {:zones nil} {:ok false :reason :not-replaceable}))

(deftest place-details
  (is (= {:ok false :reason :zone :zone "farm"} (place air-spot [5 64 5] :zones [farm])))
  (is (= {:ok false :reason :not-replaceable :block "stone"} (place plain [5 64 5])))
  (is (= {:ok true} (place air-spot [5 64 5]))))

(deftest fluids-are-no-floor
  (are [name expected] (= expected (rules/solid-floor? (world [0 62 0] name) [0 62 0]))
    "stone" true
    "water" false
    "lava" false
    "bubble_column" false
    "air" false
    "magma_block" false))
