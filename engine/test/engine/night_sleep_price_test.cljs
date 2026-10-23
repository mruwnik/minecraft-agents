(ns engine.night-sleep-price-test
  "The night's ways through it priced against each other (bed-down/answers): an open bed, the carried bed's row shut in
  first, a log-out, sheltering here; and a body asleep in an open bed priced again when what it was priced on changes."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.night-support :refer [night noon sleep-count!]]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.survival.bed-down :as bed-down]))

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

(deftest a-bed-that-is-no-longer-there-is-seen-as-gone
  (let [{:keys [p]} (st/setup {:time night :blocks {"2,64,0" "red_bed" "3,64,0" "dirt"}})]
    (is (false? (bed-down/bed-gone? p {:x 2 :y 64 :z 0})))
    (is (true? (bed-down/bed-gone? p {:x 3 :y 64 :z 0})) "another block stands there")
    (is (true? (bed-down/bed-gone? p {:x 4 :y 64 :z 0})) "air")))

(def dark-bed-world {:time night :blocks {"2,64,0" "red_bed"} :inventory st/wall-stack :skipNight false :light-default [0 4] :light {[9 64 9] [0 4]}})

(defn ^:async asleep-until-answers
  "Alone in an open bed in dark-bed-world-like world; at hold 3 (f p) changes the world. The asleep :night.answers events (those with :inputs) once the
  body has held on to hold 6. `others` sleepers (of 4 needed) lie down before it starts."
  [world f & [others]]
  (let [{:keys [eng p seen]} (st/setup world)]
    (when others (sleep-count! p others 4))
    (st/know-bed! eng {:x 2 :y 64 :z 0})
    (st/dawn-after! p 40 (fn [n] (when (= n 3) (f p))))
    (core/submit! eng '(jobs.survival.night) {})
    (await (tick-until eng #(<= 6 (count (st/calls p "wait"))) 80))
    (filterv :inputs (st/emitted seen :night.answers))))

(deftest a-light-at-the-feet-while-asleep-prices-the-night-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lit (await (asleep-until-answers dark-bed-world #(swap! (fake/state %) assoc :light-default [15 4])))
              same (await (asleep-until-answers dark-bed-world (fn [_])))]
          (is (= 1 (count same)) "nothing changed: priced once")
          (is (= 2 (count lit)) "the bed's surroundings lit: what the stay risks is gone, priced again"))))))

(deftest health-prices-the-night-again-only-while-something-can-hurt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hurt! #(fake/swap-self! % assoc :health 12)
              open-world (dissoc dark-bed-world :light :light-default)
              dark (await (asleep-until-answers open-world hurt!))
              lit (await (asleep-until-answers (assoc dark-bed-world :light-default [15 4]) hurt!))]
          (is (= 2 (count dark)) "the dark bed risks hp: a lower health costs more per hp")
          (is (= 1 (count lit)) "no spawn chance and no mob near: the hp price is in no price"))))))

(deftest health-prices-the-night-again-while-a-log-out-is-open-in-a-lit-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lit-world (assoc dark-bed-world :light-default [15 4])
              hurt! #(fake/swap-self! % assoc :health 3 :food 2)
              open (await (asleep-until-answers lit-world hurt! 2))
              closed (await (asleep-until-answers lit-world hurt!))]
          (is (= 2 (count (distinct (map :inputs open)))) "back online at 3 hp is dearer than at 20: the log-out quote reads it, priced again")
          (is (= 1 (count closed)) "no log-out to price: health prices nothing in a lit bed"))))))

(deftest the-log-out-answer-is-taken-when-it-is-the-cheapest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks {"2,64,0" "red_bed"} :inventory st/wall-stack :skipNight false})]
          (st/know-bed! eng {:x 2 :y 64 :z 0})
          (st/dawn-after! p 40 (fn [n] (when (= n 3) (sleep-count! p 3 4))))
          (.override (.-world p) "look" (fn ^:async g [token a impl]
                                          (sleep-count! p 2 4)
                                          (await (impl token a))))
          (.override (.-world p) "offline" (fn ^:async g [token a impl]
                                             (.setTime (.-world p) noon)
                                             (await (impl token a))))
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-until eng #(seq (st/emitted seen :night.bed-left)) 60))
          (let [left (first (st/emitted seen :night.bed-left))]
            (is (true? (get-in left [:inputs :log-out])) "others are asleep: a log-out is open")
            (is (< (get-in left [:prices :log-out]) (min (get-in left [:prices :bed]) (get-in left [:prices :shelter]))))
            (is (number? (get-in left [:prices :log-out])))
            (is (= :log-out (:answer left)))))))))

(defn wake-cells-with
  "bed-down/wake-cells for a body asleep at the head cell 3,64,0 of a bed whose foot is 2,64,0, floor under only `floor` cells."
  [floor]
  (let [{:keys [p]} (st/setup {:time night :floor [100 100 100 100] :self {:pos {:x 3.5 :y 64 :z 0.5}}
                               :blocks (into {"2,64,0" "red_bed" "3,64,0" "red_bed"} (map #(vector % "stone")) floor)})]
    (mapv (juxt :x :z) (bed-down/wake-cells p))))

(deftest the-body-may-get-up-at-any-cell-round-the-bed-it-can-stand-in
  (is (= [[3 1] [2 1] [1 0]] (wake-cells-with ["3,63,1" "2,63,1" "1,63,0"])) "beside the head, beside the foot, behind it")
  (is (= [[1 0] [3 -1]] (wake-cells-with ["1,63,0" "3,63,-1"])) "behind the foot, then the other side")
  (is (= [[3 0]] (wake-cells-with [])) "nowhere to stand: on the bed"))

(deftest the-shelter-is-priced-from-the-worst-cell-the-body-may-get-up-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> dark-bed-world (dissoc :light :light-default) (assoc-in [:blocks "1,64,0"] "red_bed"))
              shelter (fn ^:async s [light]
                        (let [ev (await (asleep-until-answers (cond-> world (seq light) (assoc :light light)) (fn [_])))]
                          (get-in (first ev) [:prices :shelter])))
              dark (await (shelter {}))
              one-lit (await (shelter {[2 64 1] [15 4]}))]
          (is (number? dark))
          (is (= dark one-lit) "a lit cell to get up at changes nothing while another may be the one"))))))
