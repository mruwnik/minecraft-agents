(ns engine.walk-dark-test
  "The walks' plans cost darkness (jobs.lib.walk.world/costed-world, with-dark, plan-options), and jobs.lib.targets passes it on:
  its answer's cost adds the dark seconds and a target may carry its own range."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.lib.targets :as targets]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.walk.plan :as wplan]))

(defn seen-world
  "Fake primitives over a floor with a perception at time t, one sight pass done."
  [t]
  (let [p (tu/fake-on-floor {:time t})
        wrapped (perception/wrap p (perception/create (fake-raw/create p) {:radius 16 :ray-deg 2}))]
    (.setOwner p "t1")
    (perception/pass! (aget wrapped "perception"))
    wrapped))

(defn options-of
  "plan-options over the pathWorld of p's costed-world with opts (no dangers: the ctx has no memory)."
  [p opts]
  (let [pw (wworld/costed-world {:primitives p} (wworld/path-world p) opts)]
    (wplan/plan-options pw walk/default-weight nil wplan/wide-box)))

(deftest plans-carry-the-dark-test
  (let [p (seen-world 18000)
        ^js dark (.-dark (options-of p {:dark? true}))]
    (is (fn? (.-at dark)))
    (is (= 1 (.-factor dark)) "a dark cell costs twice a lit one")
    (is (nil? (.-night dark)) "the planner no longer takes a night flag")))

(deftest no-dark-cost-when-off-or-without-perception
  (is (nil? (.-dark (options-of (seen-world 18000) {:dark? false}))))
  (is (nil? (.-dark (options-of (tu/fake-on-floor {:time 18000}) {:dark? true})))))

(deftest the-answer-adds-the-dark-seconds
  (let [r #js {:status "found" :goal 1 :path #js {:cost #js {:seconds 5 :risk 1 :darkSeconds 3}}}
        ts [{:x 0 :y 64 :z 0} {:x 5 :y 64 :z 0}]]
    (is (= 10 (:cost (targets/answer ts r))) "5 s + 2 x risk 1 + 3 s of dark")))

(deftest a-target-may-carry-its-own-range
  (let [c {:primitives (tu/fake-on-floor {:self {:pos {:x 2.5 :y 64 :z 2.5}}})}
        ^js q (targets/query c [{:x 6 :y 64 :z 2 :range 2} {:x 20 :y 64 :z 2}] 0)]
    (is (= [2 0] (mapv #(.-range ^js %) (array-seq (.-goals q)))))
    (is (= 2 (.-range (.-goal q))))))
