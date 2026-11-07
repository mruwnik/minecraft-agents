(ns engine.far-quote-search-test
  "go-to's quote of a far walk at night past the land the body has seen (night-b06's hut bed 96 blocks off)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.path.planner-tuned :as planner]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [jobs.lib.walk.search :as wsearch]))

(def hut
  "night-b06's hut: a hollow stone box with a shut door on its north wall, a bed inside, torch-lit."
  (merge (box 94 64 10 106 67 22 "stone") (box 95 64 11 105 66 21 "air")
         {"95,65,11" "torch" "105,65,21" "torch"
          "100,64,10" "oak_door" "100,65,10" "oak_door" "100,64,16" "red_bed" "101,64,16" "red_bed"}))

(defn ^:async far-quote
  "Quote go-to to the hut's bed from x 4 at night, the body having looked round once (it sees 48 blocks): the quotes'
  routes until one is no longer :searching, and the planner searches made."
  []
  (let [raw-p (tu/fake {:blocks (merge (floor -16 -16 136 48) hut) :time 14000 :offlineScale 0.0001
                        :self {:pos {:x 4.5 :y 64 :z 16.5}}})
        _ (swap! (fake/state raw-p) merge {:light-default [15 0]})
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000) :rand (constantly 0.5)})
        p (perception/wrap raw-p per)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now (constantly 1000000)
                          :events (events/make {:body "Fake" :sinks [(second (tu/legacy-capture-sink))] :now (constantly 1000000)})})
        searches (atom [])
        create planner/new-search]
    (doseq [yaw [0 90 180 270]]
      (swap! (fake/state raw-p) assoc :yaw yaw)
      (perception/pass! per))
    (set! planner/new-search (fn [snap q o] (let [s (create snap q o)] (swap! searches conj s) s)))
    (try
      (loop [routes []]
        (let [route (get-in (await (event-api/quote-view eng "jobs.movement.go-to" (pr-str (merge {:worth tu/order-worth} {:pos [100 64 16] :range 1}))))
                            [:quote :route])
              routes (conj routes route)]
          (if (and (= :searching (:reason route)) (< (count routes) 12))
            (recur routes)
            {:routes routes :searches @searches})))
      (finally (set! planner/new-search create)))))

(deftest a-far-quote-past-the-seen-land-ends-at-one-rounds-nodes-priced-from-the-seen-edge
  ;; the rock guess walls the seen land in, so no search reaches the bed: the old quote flooded all the seen land (6920
  ;; nodes here, 11152 live, 6 times the ~100 ms a search may take) to the same end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [routes searches]} (await (far-quote))
              route (peek routes)]
          (is (= 1 (count searches)) "one search, gone on with over the quotes")
          (is (<= (.-n-nodes ^js (first searches)) wsearch/round-budget))
          (is (true? (:budget route)) "cut by its budget, the rest priced")
          (is (<= 50 (first (last (:waypoints (:partial route))))) "the partial ends at the edge of the land seen"))))))
