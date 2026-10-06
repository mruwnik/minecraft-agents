(ns engine.walk-alive-test
  "jobs.lib.walk's search loops stop when the round was cut (ctx/alive?): no slice runs after the cut."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.targets :as targets]
            [jobs.lib.walk :as walk]))

(def floor-blocks (box 0 63 0 47 63 47 "stone"))
(def goal [105 64 24])

(defn ^:async slices-after-cut
  "Run f (a function of a ctx whose alive? turns false after 3 yields) with 16-expansion slices; [yields outcome]."
  [f]
  (let [chunk walk/chunk-expansions
        yield walk/yield!
        yields (atom 0)
        p (tu/fake {:blocks floor-blocks :self {:pos {:x 44.5 :y 64 :z 24.5}}})
        c {:primitives p :alive? #(< @yields 3)}]
    (set! walk/chunk-expansions 16)
    (set! walk/yield! (fn [] (swap! yields inc) (yield)))
    (let [out (try (await (f c (walk/path-world p)))
                   (catch :default e e))]
      (set! walk/chunk-expansions chunk)
      (set! walk/yield! yield)
      [@yields out])))

(deftest a-cut-plan-from-stops-slicing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[yields out] (await (slices-after-cut (fn [c pw] (walk/plan-from! c pw goal 0 1.2 nil))))]
          (is (<= yields 3) "no slice after the cut")
          (is (core/cut? out) "the cut ends the call as an act would"))))))

(deftest a-cut-run-search-stops-slicing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[yields out]
              (await (slices-after-cut
                      (fn [c pw]
                        (let [policy (walk/body-policy c)
                              s (walk/new-search c (walk/with-walls pw nil) goal 0 1.2 policy :k nil)]
                          (walk/run-search! c s 100000 policy goal 0 1.2)))))]
          (is (<= yields 3) "no slice after the cut")
          (is (core/cut? out) "the cut ends the call as an act would"))))))

(deftest a-cut-nearest-stops-slicing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (reset! targets/searches {})
        (let [[yields out]
              (await (slices-after-cut
                      (fn [c _] (targets/nearest! c [{:x 105 :y 64 :z 24} {:x 106 :y 64 :z 24}] 0 {:budget 100000}))))]
          (reset! targets/searches {})
          (is (<= yields 3) "no slice after the cut")
          (is (core/cut? out) "the cut ends the call as an act would"))))))
