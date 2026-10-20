(ns engine.shut-in-night-support-test
  "dig-in's walls place bottom-up: a wall cell with no block beside, above or below it gets a block under it first."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.shelter-support :as st]
            [jobs.survival.dig-in-cells :as dig-cells]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world-files]))

(def pillar
  "A one-block pillar over a ground two blocks lower: the body's side cells have nothing under or beside them."
  (into st/floor
        (for [x (range -4 5) z (range -4 5) :when (not= [0 0] [x z])] [(str x ",61," z) "stone"])))

(defn needs-support!
  "Make place answer no-support for a cell with no block beside, above or below it, as the real primitive does."
  [p]
  (.override (.-world p) "place"
             (fn ^:async f [token args impl]
               (let [x (.-x (.-pos args)) y (.-y (.-pos args)) z (.-z (.-pos args))
                     held? (some #(let [b (some-> (.blockAt p #js {:x (+ x (nth % 0)) :y (+ y (nth % 1)) :z (+ z (nth % 2))}) .-name)]
                                    (and b (not= "air" b)))
                                 [[0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])]
                 (if held? (await (impl token args)) #js {:status "no-support"})))))

(defn ^:async run-walls! [world]
  (let [{:keys [eng p seen]} (st/setup (assoc world :time st/night))
        _ (needs-support! p)]
    (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
    (await (run-until-empty eng 30))
    {:p p :seen seen}))

(deftest walls-around-a-pillar-place-support-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-walls! {:inventory [{:name "dirt" :count 40}] :blocks pillar :floor [0 0 0 0]}))]
          (is (empty? (st/emitted seen :shut-in.failed)) (pr-str (map :text (st/emitted seen :shut-in.failed))))
          (is (= 1 (count (st/emitted seen :shut-in.sealed)))))))))

(deftest a-support-block-in-another-zone-is-not-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              p (tu/fake {:offlineScale 0.0001 :floor [0 0 0 0] :blocks pillar :inventory [{:name "dirt" :count 40}] :time st/night})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                :world (world-files/of-data {} {} [{:name "keep-out" :min [-1 62 -1] :max [1 63 1]}])
                                :events (events/make {:body "Fake" :sinks [(second (tu/legacy-capture-sink))] :now #(deref clock)})})
              _ (needs-support! p)]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty eng 30))
          (is (every? #(contains? #{nil "air"} (some-> (.blockAt p (clj->js %)) .-name)) [{:x 1 :y 63 :z 0} {:x 1 :y 62 :z 0} {:x 0 :y 63 :z 1} {:x -1 :y 63 :z 0}])
              "nothing is placed under the wall cells inside the zone"))))))

(deftest walls-under-a-leaf-roof-do-not-count-as-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-walls! {:inventory [{:name "dirt" :count 40}] :blocks (assoc pillar "0,66,0" "oak_leaves") :floor [0 0 0 0]}))]
          (is (seq (st/emitted seen :shut-in.unsealed)) "leaves are no roof: rain and sky light come through")
          (is (empty? (st/emitted seen :shut-in.sealed))))))))

(defn refuses-under-the-body!
  "Make place fail for a cell the body's 0.62 wide, 1.8 high box overlaps, as the server does."
  [p]
  (.override (.-world p) "place"
             (fn ^:async f [token args impl]
               (let [x (.-x (.-pos args)) y (.-y (.-pos args)) z (.-z (.-pos args))
                     [bx by bz] (:pos (fake/self p))
                     overlaps? (and (< by (inc y)) (> (+ by 1.8) y) (< (- bx 0.31) (inc x)) (> (+ bx 0.31) x) (< (- bz 0.31) (inc z)) (> (+ bz 0.31) z))]
                 (if overlaps? #js {:status "failed"} (await (impl token args)))))))

(deftest walls-are-placed-from-the-middle-of-the-cell-so-the-body-never-overlaps-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:inventory [{:name "dirt" :count 40}] :blocks pillar :floor [0 0 0 0] :time st/night})
              _ (swap! (fake/state p) assoc-in [:self :pos] [0.15 64 0.5])
              _ (refuses-under-the-body! p)]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty eng 30))
          (is (empty? (st/emitted seen :shut-in.unsealed)))
          (is (= 1 (count (st/emitted seen :shut-in.sealed)))))))))

(def cell {:x 0 :y 64 :z 0})
(def avoid #{{:x 0 :y 63 :z 0} {:x 0 :y 64 :z 0}})

(defn chain [blocks depth]
  (dig-cells/support-chain (tu/fake {:blocks blocks}) cell #{} depth))

(deftest support-chain-is-the-cells-under-up-to-the-first-with-a-block-beside
  (is (= [] (chain {"1,64,0" "stone"} 3)) "placeable already")
  (is (= [{:x 0 :y 63 :z 0}] (chain {"1,63,0" "stone"} 3)))
  (is (= [{:x 0 :y 62 :z 0} {:x 0 :y 63 :z 0}] (chain {"1,62,0" "stone"} 3)) "lowest first")
  (is (nil? (chain {"1,61,0" "stone"} 2)) "a chain longer than the depth is none")
  (is (= [{:x 0 :y 61 :z 0} {:x 0 :y 62 :z 0} {:x 0 :y 63 :z 0}] (chain {"1,61,0" "stone"} 3))))

(deftest support-chain-stops-at-fluid-and-at-the-bodys-cells
  (is (nil? (chain {"0,63,0" "lava" "1,62,0" "stone"} 3)) "lava under")
  (is (nil? (chain {"0,63,0" "water" "1,62,0" "stone"} 3)) "water under")
  (is (nil? (dig-cells/support-chain (tu/fake {:blocks {"1,62,0" "stone"}}) {:x 0 :y 65 :z 0} avoid 3)) "never into the feet or head cells"))

(deftest walls-fill-the-support-depth-setting-cells-under-a-wall-cell
  (let [p (tu/fake {:blocks {"1,61,0" "stone"}})
        cells [cell]]
    (is (= 4 (count (dig-cells/bottom-up p cells {:x 5 :y 64 :z 5}))) "default 3 under plus the cell")
    (is (= [cell] (settings/with-settings {:jobs.survival.dig-in-cells/support-depth 1}
                    #(dig-cells/bottom-up p cells {:x 5 :y 64 :z 5}))) "too deep for 1: the cell alone, which the server then refuses")))
