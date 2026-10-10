(ns engine.dig-in-support-test
  "dig-in's walls place bottom-up: a wall cell with no block beside, above or below it gets a block under it first."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.shelter-test :as st]
            [engine.test-util :as tu :refer [run-until-empty]]))

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
    (core/submit! eng '(jobs.survival.dig-in {:enclose true}) {})
    (await (run-until-empty eng 30))
    {:p p :seen seen}))

(deftest walls-around-a-pillar-place-support-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-walls! {:inventory [{:name "dirt" :count 40}] :blocks pillar :floor [0 0 0 0]}))]
          (is (empty? (st/emitted seen :dig_in_failed)) (pr-str (map :text (st/emitted seen :dig_in_failed))))
          (is (= 1 (count (st/emitted seen :dig-in.sealed)))))))))

(deftest walls-under-a-leaf-roof-count-as-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-walls! {:inventory [{:name "dirt" :count 40}] :blocks (assoc pillar "0,66,0" "oak_leaves") :floor [0 0 0 0]}))]
          (is (empty? (st/emitted seen :dig-in.unsealed)) "leaves stop a mob: the roof the wall cells already count")
          (is (= 1 (count (st/emitted seen :dig-in.sealed)))))))))

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
          (core/submit! eng '(jobs.survival.dig-in {:enclose true}) {})
          (await (run-until-empty eng 30))
          (is (empty? (st/emitted seen :dig-in.unsealed)))
          (is (= 1 (count (st/emitted seen :dig-in.sealed)))))))))
