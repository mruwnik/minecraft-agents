(ns engine.walk-void-search-test
  "A masked search (go-to :unknown) from the edge of a floor toward a goal floating over a big unseen void."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake.raw-world :as fake-raw]
            [engine.path.executor :as executor]
            [engine.path.planner-tuned :as planner]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]))

(def void-world
  {:self {:pos {:x 0.5 :y 64 :z 10.5}} :yaw 180 :pitch 0
   :blocks (merge (box -30 63 -10 40 63 10 "stone") (box 0 63 38 2 65 38 "iron_ore"))})

(deftest a-masked-search-over-a-void-stops-at-the-node-limit-and-runs-no-second-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake void-world)
              raw (fake-raw/create p)
              per (perception/create raw {:now (constantly 1000000) :radius 48 :ray-deg 4})
              w (perception/wrap p per)
              searches (atom 0)
              create planner/create-plan]
          (aset w "rawWorld" raw)
          (perception/pass! per)
          (with-redefs [planner/create-plan (fn [& args] (swap! searches inc) (apply create args))]
            (let [{:keys [r]} (await (settings/with-settings {:jobs.lib.walk.world/unknown-max-nodes 2000}
                                      #(wplan/plan-within! {:primitives w} (wworld/path-world w (wworld/unknown-spec)) [1 64 38] 4 1.2 executor/policy)))]
              (is (= "budget" (.-reason r)) "no seen route: the unseen cells were flooded to the limit")
              (is (<= (.-expanded r) 2200))
              (is (= 1 @searches) "the search without the walker's limits would flood the same cells again"))))))))
