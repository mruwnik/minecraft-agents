(ns engine.felt-reads-test
  "The body-mechanics reads (suffocating, wedged, breathe and extinguish footing, walk settle) answer from what the body
  feels, through perception's feel, never from the raw blockAt. The primitives are wrapped; their blockAt throws."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.lib.breath :as breath]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [jobs.survival.breathe :as breathe]
            [jobs.survival.extinguish :as extinguish]))

(defn felt-only
  "Wrapped fake primitives for spec, in the dark, whose raw blockAt throws."
  [spec]
  (let [p (tu/fake spec)
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})
        w (perception/wrap p per)]
    (swap! (fake/state p) assoc :light-default [0 0])
    (aset w "blockAt" (fn [_] (throw (js/Error. "raw blockAt"))))
    w))

(def pos {:x 0 :y 64 :z 0})

(deftest u-feel-answers-the-touched-cells-and-nothing-behind-stone
  (let [p (felt-only {:blocks {"0,64,0" "dirt" "0,63,0" "stone" "3,64,0" "gold_block"}})]
    (is (= "dirt" (u/feel-name p pos)))
    (is (= "stone" (u/feel-name p {:x 0 :y 63 :z 0})))
    (is (true? (.-fullCube (u/feel p pos))))
    (is (nil? (u/feel p {:x 3 :y 64 :z 0})) "not touching the body: unknown here, never raw")))

(deftest suffocating-and-wedged-are-felt-in-the-dark
  (let [p (felt-only {:blocks {"0,64,0" "sand" "0,65,0" "sand"}})]
    (is (= :enclosed (breath/situation p 12)))
    (is (= pos (breath/wedged-cell p)))
    (is (nil? (breath/only-feet-cell p)) "suffocating owns it"))
  (let [p (felt-only {:blocks {"0,64,0" "sand"}})]
    (is (= pos (breath/only-feet-cell p)))
    (is (nil? (breath/situation p 12)))))

(deftest footing-and-fire-at-the-feet-are-felt
  (let [p (felt-only {:blocks {"0,63,0" "stone" "0,64,0" "fire"}})]
    (is (true? (breathe/land-footing? p (.self p))))
    (is (= [{:name "fire" :pos pos}] (extinguish/feel-feet p pos [])))))

(deftest walk-settle-reads-the-feet-cell-by-feel
  (let [p (felt-only {:self {:onGround false} :blocks {"0,64,0" "ladder"}})]
    (is (true? (walk/grounded? p)))))
