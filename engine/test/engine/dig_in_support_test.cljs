(ns engine.dig-in-support-test
  "dig-in's walls place bottom-up: a wall cell with no block beside, above or below it gets a block under it first."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
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
