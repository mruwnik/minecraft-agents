(ns engine.night-sleep-price-test
  "The night's ways through it priced against each other (bed-down/answers): an open bed, the carried bed's row shut in
  first, a log-out, sheltering here; and a body asleep in an open bed priced again when what it was priced on changes."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.night-support :refer [night sleep-count!]]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.lib.shelter.sense :as sh-sense]))

(def bed-and-dirt [{:name "red_bed" :count 1} {:name "dirt" :count 40}])

(defn ^:async tick-until [eng f n]
  (loop [i 0]
    (when (and (< i n) (not (f)))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn asleep? [p] (.-isSleeping (.self p)))

(defn placed [p item] (filterv #(= item (.-item (.-args %))) (st/calls p "place")))

(deftest a-body-asleep-in-an-open-bed-gets-up-when-the-night-can-no-longer-skip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks {"2,64,0" "red_bed"} :inventory st/wall-stack :skipNight false})
              asleep-at-hold (atom nil)]
          (st/know-bed! eng {:x 2 :y 64 :z 0})
          (st/dawn-after! p 40 (fn [n] (when (= n 3) (reset! asleep-at-hold (asleep? p)) (sleep-count! p 1 4))))
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-until eng #(seq (st/emitted seen :night.bed-left)) 60))
          (is (true? @asleep-at-hold) "alone, its sleep skips the night: it lay down and stayed")
          (is (= [:shelter] (mapv :answer (st/emitted seen :night.bed-left))) "three more must sleep: lying in the open all night is dearer than sheltering")
          (is (not (asleep? p)) "out of the bed"))))))

(deftest a-carried-bed-is-shut-in-before-it-is-lain-in-on-a-night-that-does-not-skip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks st/floor :inventory bed-and-dirt :players ["Alex" "Bo" "Cy"] :skipNight false})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-until eng #(asleep? p) 80))
          (is (= 1 (count (placed p "red_bed"))) "the bed goes down")
          (is (< 10 (count (placed p "dirt"))) "its row is walled and roofed")
          (is (asleep? p) "then lain in: no respawn point yet, and the row is sheltered")
          (is (sh-sense/shut-in-room? p)))))))

(deftest a-carried-bed-is-lain-in-the-open-when-the-sleep-skips-the-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks st/floor :inventory bed-and-dirt})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-until eng #(seq (st/calls p "sleep")) 60))
          (is (= 1 (count (placed p "red_bed"))))
          (is (empty? (placed p "dirt")) "alone, its sleep ends the night: walls would only cost time")
          (is (seq (st/calls p "sleep"))))))))

(deftest with-the-respawn-point-worth-nothing-the-body-shelters-rather-than-shut-its-bed-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks st/floor :inventory bed-and-dirt :players ["Alex" "Bo" "Cy"]})]
          (core/submit! eng '(jobs.survival.night {:settings {:jobs.lib.cost.death/spawn-horizon-s 0}}) {})
          (await (tick-until eng #(seq (placed p "dirt")) 60))
          (await (st/tick-n eng 20))
          (is (empty? (placed p "red_bed")) "the bed stays in the bag")
          (is (empty? (st/calls p "sleep")))
          (is (seq (placed p "dirt")) "walled in where it stands"))))))
