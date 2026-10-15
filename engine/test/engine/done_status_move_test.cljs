(ns engine.done-status-move-test
  "done? of the movement, access, explore and time jobs answers a status (docs/contract-conditions-design.md 2.2)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.contract :as contract]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(defn ctx-of [args]
  {:primitives (tu/fake {}) :root "j1" :slots [] :args args
   :view (fn [] {:now 0 :data {:entries {}}})})

(def cases
  [['jobs.movement.follow {:player "Steve"} :later]
   ['jobs.movement.linger-near {:pos [0 64 0] :range 3 :wait-s 5} :later]
   ['jobs.movement.mount {:id 9} :later]
   ['jobs.movement.leave-vehicle {} :ok]
   ['jobs.movement.boat-drive {:pos [10 63 2] :range 2} :later]
   ['jobs.movement.boat-launch {} :later]
   ['jobs.movement.boat-launch {:action :recover :id 7} :later]
   ['jobs.movement.boat-land {} :later]
   ['jobs.movement.cross-water {:pos [7 64 0]} :later]
   ['jobs.access.clear-path {:through [[1 64 0]]} :later]
   ['jobs.access.cleanup {:job "j"} :ok]
   ['jobs.access.toggle {:pos [3 64 0] :state :open} :later]
   ['jobs.access.pillar {:height 3} :later]
   ['jobs.access.bridge {:heading :east :length 3} :later]
   ['jobs.access.tunnel {:to [5 60 0]} :later]
   ['jobs.access.stair {:direction :down :steps 3} :later]
   ['jobs.explore.search {:target "diamond_ore" :count 1} :later]
   ['jobs.explore.look {} :later]
   ['jobs.time.noop {:ms 100} :later]])

(deftest every-done-answers-a-status
  (doseq [[sym args kind] cases
          :let [r ((:done? (get registry/jobs sym)) (ctx-of args))]]
    (is (contract/status? r) (str sym " done? is a status"))
    (is (= kind (first r)) (str sym))
    (is (every? #(contains? (nth r 2) %) [:units :done :left]) (str sym " carries its progress"))))
