(ns engine.access.rules-test
  "jobs.lib.access.rules: the verdict table over tiny hand-made block lookups."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.access.rules :as rules]))

(def feet [0 64 0])
(def farm {:name "farm" :min [4 60 4] :max [6 70 6] :owner "Miles"})

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
  "The verdict cut to what a row compares: :ok, :reason and the reasons of the hazards."
  [v]
  (cond-> (select-keys v [:ok :reason])
    (:hazards v) (assoc :hazards (mapv :reason (:hazards v)))))

(def plain (world [5 64 5] "stone"))
(def under-feet [0 63 0])

(deftest dig-verdicts
  (are [w cell extra expected] (= expected (verdict (apply dig w cell (mapcat identity extra))))
    ;; plain stone
    plain [5 64 5] {} {:ok true}
    ;; not loaded
    plain [9 9 9] {} {:ok false :reason :not-loaded}
    ;; fluids in any of the six neighbours
    (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5] {} {:ok true :hazards [:fluid-adjacent]}
    (world [5 64 5] "stone" [4 64 5] "water") [5 64 5] {} {:ok true :hazards [:fluid-adjacent]}
    (world [5 64 5] "stone" [5 65 5] "water") [5 64 5] {} {:ok true :hazards [:fluid-adjacent]}
    (world [5 64 5] "stone" [5 63 5] "lava") [5 64 5] {} {:ok true :hazards [:fluid-adjacent]}
    (world [5 64 5] "stone" [5 64 6] "bubble_column") [5 64 5] {} {:ok true :hazards [:fluid-adjacent]}
    (world [5 64 5] "stone" [5 64 4] "bubble_column") [5 64 5] {} {:ok true :hazards [:fluid-adjacent]}
    ;; a diagonal fluid is not a neighbour
    (world [5 64 5] "stone" [6 65 5] "water") [5 64 5] {} {:ok true}
    ;; falling blocks above, body under them in the same column
    (world [0 66 0] "stone" [0 67 0] "gravel") [0 66 0] {} {:ok true :hazards [:falling-block]}
    (world [0 66 0] "stone" [0 67 0] "sand") [0 66 0] {} {:ok true :hazards [:falling-block]}
    (world [0 66 0] "stone" [0 67 0] "red_sand") [0 66 0] {} {:ok true :hazards [:falling-block]}
    (world [0 66 0] "stone" [0 67 0] "white_concrete_powder") [0 66 0] {} {:ok true :hazards [:falling-block]}
    ;; the same gravel over a cell that is not over the body
    (world [3 66 3] "stone" [3 67 3] "gravel") [3 66 3] {} {:ok true}
    ;; a solid block between the cell and the body stops the fall
    (world [0 67 0] "stone" [0 68 0] "gravel" [0 66 0] "stone") [0 67 0] {} {:ok true}
    ;; gravel below the body's head does not matter
    (world [0 62 0] "stone" [0 63 0] "gravel") [0 62 0] {} {:ok true}
    ;; the cell under the feet
    (world under-feet "stone" [0 62 0] "stone") under-feet {} {:ok true}
    (world under-feet "stone" [0 62 0] "air") under-feet {} {:ok true :hazards [:under-feet]}
    (world under-feet "stone" [0 62 0] "water") under-feet {} {:ok true :hazards [:fluid-adjacent :under-feet]}
    (world under-feet "stone" [0 62 0] "magma_block") under-feet {} {:ok true :hazards [:under-feet]}
    (world under-feet "stone") under-feet {} {:ok true :hazards [:under-feet]}
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
    ;; two hazards at once, in check order
    (world [0 66 0] "stone" [0 67 0] "gravel" [1 66 0] "water") [0 66 0] {} {:ok true :hazards [:fluid-adjacent :falling-block]}
    ;; a hazard cell is still refused for the permission reasons
    (world [5 64 5] "stone" [6 64 5] "water") [5 64 5] {:zones [farm]} {:ok false :reason :zone}
    (world [5 64 5] "stone" [6 64 5] "water") [5 64 5] {:footprints #{[5 64 5]}} {:ok false :reason :footprint}
    (world [0 66 0] "stone" [0 67 0] "gravel") [0 66 0] {:zones nil} {:ok false :reason :no-zones}
    ;; order: not loaded, footprint, zone, no-zones; hazards are only reported
    plain [9 9 9] {:zones nil :footprints #{[9 9 9]}} {:ok false :reason :not-loaded}
    (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5] {:footprints #{[5 64 5]} :zones [farm]} {:ok false :reason :footprint}
    (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5] {:zones [farm]} {:ok false :reason :zone}
    (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5] {:zones nil} {:ok false :reason :no-zones}))

(deftest dig-details
  (is (= {:ok false :reason :zone :zone "farm" :owner "Miles"} (dig plain [5 64 5] :zones [farm] :self "Bot")))
  (is (= {:ok true :hazards [{:reason :fluid-adjacent :fluid "lava" :at [6 64 5]}]}
         (dig (world [5 64 5] "stone" [6 64 5] "lava") [5 64 5])))
  (is (= {:ok true :hazards [{:reason :falling-block :block "gravel" :at [0 67 0]}]}
         (dig (world [0 66 0] "stone" [0 67 0] "gravel") [0 66 0])))
  (is (nil? (:hazards (dig plain [5 64 5]))))
  (is (= {:ok true} (dig plain [5 64 5]))))

(deftest every-fluid-neighbour-is-a-hazard
  (is (= {:ok true :hazards [{:reason :fluid-adjacent :fluid "water" :at [6 64 5]}
                             {:reason :fluid-adjacent :fluid "lava" :at [5 64 6]}]}
         (dig (world [5 64 5] "stone" [6 64 5] "water" [5 64 6] "lava") [5 64 5])))
  (is (= {:ok true :hazards [{:reason :fluid-adjacent :fluid "lava" :at [6 64 5]}
                             {:reason :fluid-adjacent :fluid "water" :at [5 65 5]}
                             {:reason :fluid-adjacent :fluid "water" :at [5 64 4]}]}
         (dig (world [5 64 5] "stone" [6 64 5] "lava" [5 65 5] "water" [5 64 4] "water") [5 64 5])))
  (is (= [:fluid-adjacent :fluid-adjacent :falling-block]
         (mapv :reason (:hazards (dig (world [0 66 0] "stone" [1 66 0] "water" [0 66 1] "lava" [0 67 0] "gravel")
                                      [0 66 0]))))))

(deftest accepts-is-unchanged-by-several-fluids
  (let [v (dig (world [5 64 5] "stone" [6 64 5] "water" [5 64 6] "lava") [5 64 5])]
    (is (rules/accepts? v #{:fluid-adjacent}))
    (is (not (rules/accepts? v #{})))))

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
  (is (= {:ok false :reason :zone :zone "farm" :owner "Miles"} (place air-spot [5 64 5] :zones [farm])))
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

(deftest accepts-checks-ok-and-every-hazard
  (are [v accepted expected] (= expected (rules/accepts? v accepted))
    {:ok true} #{} true
    {:ok false :reason :zone} #{:fluid-adjacent} false
    {:ok true :hazards [{:reason :fluid-adjacent}]} #{} false
    {:ok true :hazards [{:reason :fluid-adjacent}]} #{:fluid-adjacent} true
    {:ok true :hazards [{:reason :fluid-adjacent} {:reason :under-feet}]} #{:fluid-adjacent} false
    {:ok true :hazards [{:reason :fluid-adjacent} {:reason :under-feet}]} #{:fluid-adjacent :under-feet} true))

(deftest a-footprint-map-names-the-plan-that-claims-the-cell
  (is (= {:ok false :reason :footprint :plan "pad"} (dig plain [5 64 5] :footprints {[5 64 5] "pad"})))
  (is (= {:ok false :reason :footprint :plan "pad"} (place (world [5 64 5] "air") [5 64 5] :footprints {[5 64 5] "pad"})))
  (is (= {:ok true} (dig plain [5 64 5] :footprints {[5 64 6] "pad"}))))

(deftest the-social-checks-follow-owner-claims-and-the-opt-out
  (are [w cell f extra expected] (= expected (verdict (apply f w cell (mapcat identity extra))))
    plain [5 64 5] dig {:zones [farm] :self "Miles"} {:ok true}
    plain [5 64 5] dig {:zones [farm] :self "miles"} {:ok true}
    plain [5 64 5] dig {:zones [farm] :self "Bot"} {:ok false :reason :zone}
    air-spot [5 64 5] place {:zones [farm] :self "Miles"} {:ok true}
    air-spot [5 64 5] place {:zones [farm] :self "Bot"} {:ok false :reason :zone}
    plain [5 64 5] dig {:zones [farm] :self "Bot" :ignore-zones? true} {:ok true}
    air-spot [5 64 5] place {:zones [farm] :self "Bot" :ignore-zones? true} {:ok true}
    plain [5 64 5] dig {:zones nil :ignore-zones? true} {:ok true}
    plain [5 64 5] dig {:footprints #{[5 64 5]} :ignore-zones? true} {:ok true}
    plain [5 64 5] dig {:zones [] :self "Bot" :now 10
                         :claims [{:id "c" :owner "Miles" :status :active :until 99 :min [4 60 4] :max [6 70 6]}]} {:ok false :reason :claim}
    plain [5 64 5] dig {:zones [] :self "Bot" :now 10 :ignore-zones? true
                         :claims [{:id "c" :owner "Miles" :status :active :until 99 :min [4 60 4] :max [6 70 6]}]} {:ok true}
    ;; physics still refuses under the opt-out
    plain [5 64 5] place {:ignore-zones? true :zones nil} {:ok false :reason :not-replaceable}
    plain [9 9 9] dig {:ignore-zones? true} {:ok false :reason :not-loaded}))
