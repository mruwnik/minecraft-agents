(ns engine.stand-in-test
  "jobs.survival.stand-in: the nearest seen water, 2-high roof gap or dry stand, and the job that walks to it."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.stand-in :as stand-in]))

(deftest water-cell-is-the-nearest-seen-water-within-reach
  (let [water #{[3 64 0] [-2 64 1] [9 64 0]}
        at (fn [{:keys [x y z]}] (if (water [x y z]) "water" "air"))]
    (is (= {:x -2 :y 64 :z 1} (stand-in/water-cell at {:x 0.5 :y 64 :z 0.5} 6)))
    (is (nil? (stand-in/water-cell at {:x 20.5 :y 64 :z 0.5} 6)) "none within reach")))

(deftest roof-cell-is-the-nearest-seen-two-high-gap-under-a-roof
  (let [solid #{[0 63 0] [3 63 0] [3 66 0] [-2 63 1] [-2 65 1] [9 63 0] [9 66 0]}
        at (fn [{:keys [x y z]}] (if (solid [x y z]) "stone" "air"))]
    (is (= {:x 3 :y 64 :z 0} (stand-in/roof-cell at {:x 0.5 :y 64 :z 0.5} 8)) "a cell with a block at head height is no gap; one beyond reach is skipped")
    (is (nil? (stand-in/roof-cell at {:x 20.5 :y 64 :z 0.5} 8)) "none within reach")))

(deftest roof-cell-prefers-a-cell-no-enderman-can-stand-within-two-cells-of
  (let [roof (for [x (range 3 9) z (range -3 4)] [x 66 z])
        floor (for [x (range 0 9) z (range -3 4)] [x 63 z])
        solid (set (concat roof floor))
        at (fn [{:keys [x y z]}] (if (solid [x y z]) "stone" "air"))]
    (is (= {:x 5 :y 64 :z 0} (stand-in/roof-cell at {:x 0.5 :y 64 :z 0.5} 8))
        "the edge cell x=3 is open to a 3-high stand at x=2: it takes the first cell 2 in")))


(defn stand!
  "Run (jobs.survival.stand-in args) over world; steer teleports the body to the steer target (pos), as a walk would. {:p :seen}."
  [world args]
  (let [[seen sink] (tu/legacy-capture-sink)
        clock (atom 1000000)
        p (tu/fake-on-floor world)
        now (tu/act-clock clock p 1000)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    (.override (.-world p) "steer"
               (fn [token a impl] (-> (impl token a)
                                      (.then (fn [r] (when-let [t (some-> a .-pos)] (fake/swap-self! p assoc :pos [(.-x t) (.-y t) (.-z t)])) r)))))
    (core/submit! eng (list 'jobs.survival.stand-in args) {})
    {:eng eng :p p :seen seen}))

(deftest a-body-walks-to-the-nearest-water-and-ends-done-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (stand! {:blocks {"3,64,1" "water"}} {:where :water})]
          (await (tu/run-until-empty eng 10))
          (is (= {:x 3 :y 64 :z 1} (first (tu/walked-to eng))) "walks to the water cell")
          (is (= [3 1] (mapv #(js/Math.floor %) ((juxt :x :z) (st/pos-of p)))) "stands in it")
          (is (empty? (st/ended-because seen :no-place))))))))

(deftest no-water-within-reach-ends-no-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (stand! {:blocks {"30,64,1" "water"}} {:where :water})]
          (await (tu/run-until-empty eng 10))
          (is (zero? (count (tu/walked-to eng))) "no walk")
          (is (seq (st/ended-because seen :no-place)) "ends no-place"))))))
