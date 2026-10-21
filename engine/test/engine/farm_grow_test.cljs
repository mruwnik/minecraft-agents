(ns engine.farm-grow-test
  "jobs.farm.grow against the fake world: a standing field is used, never started again; no field is made."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.triggers :as triggers]
            [engine.farm-tend-test :as ft]
            [engine.test-util :as tu]))

(defn setup
  "As ft/setup, the clock moving 5 s with every primitive call, so a wait ends by time."
  [spec]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor spec))
        now (tu/act-clock clock p 5000)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async scenario [args spec n]
  (let [s (setup spec)]
    (core/submit! (:eng s) (list 'jobs.farm.grow args) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
    s))

(def kit [(ft/item "iron_hoe" 1) (ft/item "wheat_seeds" 9)])

(deftest a-ripe-crop-in-sight-is-cut-and-the-count-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2} (ft/world (ft/farm "wheat" 7 [[2 2] [3 2]] [[2 2] [3 2]]) ft/wheat-drops {:inventory kit}) 40))]
          (is (<= 2 (get (ft/inv s) "wheat" 0)))
          (is (true? (ft/finished? s))))))))

(deftest a-ripe-crop-beyond-the-harvest-radius-of-the-body-is-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 1} (ft/world (ft/farm "wheat" 7 [[25 2]] [[25 2]]) ft/wheat-drops {:inventory kit}) 60))]
          (is (<= 1 (get (ft/inv s) "wheat" 0))))))))

(deftest a-growing-field-is-waited-for-and-no-field-is-started
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2 :spend {:job-time 120}} (ft/world (ft/farm "wheat" 2 [[2 2]] [[2 2]]) ft/wheat-drops {:inventory kit}) 60))]
          (is (some #(= :limit_reached (:kind %)) @(:seen s)) "waited for it to ripen until its time was up")
          (is (= 9 (get (ft/inv s) "wheat_seeds")) "no seed was sown")
          (is (empty? (ft/calls s "dig"))))))))

(deftest with-no-crop-in-sight-a-field-is-made-on-flat-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2 :spend {:job-time 600}}
                                 (ft/world (ft/ground "dirt" (for [x (range -2 8) z (range -2 8)] [x z]))
                                           {:inventory kit}) 120))]
          (is (some #(= "wheat" (apply ft/block-at s %)) (for [x (range -2 8) z (range -2 8)] [x 64 z])) "seeds were sown"))))))

(deftest a-dark-floor-is-lit-with-carried-torches-and-then-sown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (for [x (range -2 8) z (range -2 8)] [x z])
              s (await (scenario {:count 2 :spend {:job-time 600}}
                                 (ft/world (ft/ground "dirt" cells)
                                           {:light {} :light-default [0 0]
                                            :inventory (conj kit (ft/item "torch" 8))}) 160))]
          (is (seq (ft/calls s "place")) "a torch was placed")
          (is (some #(= "wheat" (apply ft/block-at s %)) (for [[x z] cells] [x 64 z])) "seeds were sown"))))))

(deftest a-ripe-crop-the-harvest-cannot-reach-is-given-up-and-a-field-is-made-elsewhere
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (for [x (range -2 8) z (range -2 8)] [x z])
              s (await (scenario {:count 2 :spend {:job-time 600}}
                                 (ft/world (ft/farm "wheat" 7 [[12 4]] [[12 4]]) (ft/ground "dirt" cells)
                                           {:unreachable [(ft/k 12 64 4)]} ft/wheat-drops {:inventory kit}) 200))]
          (is (= "wheat" (ft/block-at s 12 64 4)) "the unreachable crop stands")
          (is (some #(= "wheat" (apply ft/block-at s %)) (for [[x z] cells] [x 64 z])) "seeds were sown elsewhere"))))))
