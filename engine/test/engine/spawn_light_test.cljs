(ns engine.spawn-light-test
  "jobs.lib.spawn-light/exposure: the spawn chance a walk to a place meets, by chance-at, and what lowers it."
  (:require [cljs.test :refer [deftest is]]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spawn-light :as spawn-light]))

(def open-night-chance (cost/spawn-chance [0 0 0]))

(def pit
  "A 1x1 pit with a roof: dirt round the feet and head cells, a roof over the head."
  (merge (into {} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str dx "," y "," dz) "dirt"]))
         {"0,66,0" "dirt"}))

(deftest exposure-of-a-walk-out-of-shelter-is-the-open-sky-night-figure-lowered-by-dawn
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit}))]
    (let [e (spawn-light/exposure p {:x 15 :y 64 :z 0})]
      (is (= {:chance open-night-chance :lowered-by #{:light}} (dissoc e :light-in)))
      (is (pos? (:light-in e)) "and the seconds till light"))))

(deftest exposure-of-a-walk-out-of-shelter-judges-an-unseen-cell-only-where-it-could-be-seen
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit}))
        exposure (fn [glimpse? to] (with-redefs [sh-sense/seen-cell? (constantly false) sh-sense/block-light-at (constantly 14) look/glimpse? (constantly glimpse?)]
                                     (spawn-light/exposure p to)))]
    (is (= {:chance 0.0 :lowered-by #{}} (exposure true {:x 3 :y 64 :z 0})) "a lit cell in clear line is made out")
    (is (pos? (:chance (exposure false {:x 3 :y 64 :z 0}))) "a lit cell behind a wall stays unknown, so dark")))

(deftest exposure-of-a-seen-log-under-unseen-ones-is-judged-by-the-seen-air-over-the-pile
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))]
    (is (= [[3 67 0]] (with-redefs [sh-sense/solid-at? (fn [_ c] (= 64 (:y c)))
                                    sh-sense/seen-cell? (fn [_ c] (or (= 64 (:y c)) (= 67 (:y c))))
                                    look/glimpse? (constantly false)]
                        (:look-at (spawn-light/exposure p {:x 3 :y 64 :z 0})))))))

(deftest exposure-of-a-walk-in-the-open-is-the-spawn-chance-at-its-end
  (let [night (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))
        day (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/noon :self {:pos {:x 0 :y 64 :z 0}}}))
        seen (fn [p block sky] (with-redefs [sh-sense/seen-cell? (constantly true) sh-sense/block-light-at (constantly block) sh-sense/sky-light-at (constantly sky) sh-inside/walk-leaves-shelter? (constantly nil)]
                                 (spawn-light/exposure p {:x 15 :y 64 :z 0})))]
    (is (= {:chance (spawn-light/chance-at night {:x 15 :y 64 :z 0} false) :lowered-by #{:look} :look-at [[15 64 0]]}
           (spawn-light/exposure night {:x 15 :y 64 :z 0}))
        "an unseen cell is priced by its guessed light, and looking is what lowers that")
    (is (= {:chance 0.0 :lowered-by #{}} (seen night 8 15)) "a seen lit cell spawns nothing")
    (is (= #{} (:lowered-by (seen day 0 0))) "a seen dark cave cell by day: nothing foreseeable lowers it")))

(deftest exposure-takes-the-shelter-entry-of-the-body
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,67,0" "stone"}}))]
    (with-redefs [sh-inside/walk-leaves-shelter? (fn [_ shelter _ _] (when (= :entry shelter) :dug-in))]
      (is (= #{:light} (:lowered-by (spawn-light/exposure p {:x 15 :y 64 :z 0} :entry))))
      (is (= #{:look} (:lowered-by (spawn-light/exposure p {:x 15 :y 64 :z 0})))))))

(deftest exposure-floors-the-goal-and-takes-a-column-at-the-feet-height
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}}}))]
    (is (= {:chance (spawn-light/chance-at p {:x 15 :y 64 :z 0} false) :lowered-by #{:look} :look-at [[15 64 0]]}
           (spawn-light/exposure p {:x 15.5 :z 0.2})))))

(deftest exposure-of-a-solid-seen-goal-is-judged-by-the-air-over-it
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"15,64,0" "grass_block"}}))
        sky (fn [_ at] (if (= 64 (:y at)) 0 15))]
    (with-redefs [sh-sense/seen-cell? (constantly true) sh-sense/block-light-at (constantly 0)
                  sh-sense/sky-light-at sky sh-inside/walk-leaves-shelter? (constantly nil)]
      (is (= #{:light} (:lowered-by (spawn-light/exposure p {:x 15 :y 64 :z 0}))) "the grass cell is dark by day too; the air over it lowers at dawn")
      (is (= (spawn-light/exposure p {:x 15 :y 65 :z 0}) (spawn-light/exposure p {:x 15 :y 64 :z 0}))))))

(deftest exposure-of-a-dark-cell-is-lowered-by-dawn-where-the-sky-reaches-down-its-column
  (let [at (fn [sky blocks] (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks blocks}))]
                              (with-redefs [sh-sense/seen-cell? (constantly true) sh-sense/block-light-at (constantly 0)
                                            sh-sense/sky-light-at (constantly sky) sh-inside/walk-leaves-shelter? (constantly nil)]
                                (:lowered-by (spawn-light/exposure p {:x 15 :y 64 :z 0})))))]
    (is (= #{:light} (at 15 {})) "open sky: the night is lifted at dawn")
    (is (= #{:light} (at 13 {"15,70,0" "oak_leaves"})) "under a canopy the sky is dimmed but dawn still lights the floor")
    (is (= #{} (at 12 {"15,67,0" "stone"})) "a stair cell the sky reaches only round a corner: dawn is no cure, only threats count")
    (is (= #{} (at 0 {})))))

(deftest a-seen-cell-the-dusk-sky-leaves-dark-is-priced-dark
  (let [p (sky/with-sky (tu/fake {:floor tu/walk-floor :time st/noon :self {:pos {:x 0 :y 64 :z 0}}}))
        at (fn [f subtract] (with-redefs [sh-sense/sky-subtract-now (constantly subtract) sh-sense/seen-cell? (constantly true)
                                          sh-sense/block-light-at (constantly 0) sh-sense/sky-light-at (constantly 13)
                                          sh-inside/walk-leaves-shelter? (constantly nil)]
                              (f)))
        cell {:x 15 :y 64 :z 0}]
    (is (pos? (at #(spawn-light/chance-at p cell false) 6)) "a canopy floor, sky light 13, at dusk")
    (is (pos? (:chance (at #(spawn-light/exposure p cell) 6))) "and readiness prices it so")
    (is (zero? (at #(spawn-light/chance-at p cell false) 0)) "lit at noon")))
