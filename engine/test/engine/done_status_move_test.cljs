(ns engine.done-status-move-test
  "done? of the movement, access, explore and time jobs answers a status (docs/contract-conditions-design.md 2.2)."
  (:require [cljs.test :refer [deftest is]]
            [engine.contract :as contract]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(defn ctx-of
  ([args] (ctx-of args {}))
  ([args mem]
   {:primitives (tu/fake {}) :root "j1" :slots [] :args args
    :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data mem}]}}})}))

(def cases
  [['jobs.movement.follow {:player "Steve"} :later {}]
   ['jobs.movement.linger-near {:pos [0 64 0] :range 100 :wait-s 5} :later {:waited 4500}]
   ['jobs.movement.linger-near {:pos [0 64 0] :range 100 :wait-s 5} :ok {:waited 5000}]
   ['jobs.movement.mount {:id 9} :later {}]
   ['jobs.movement.leave-vehicle {} :ok {}]
   ['jobs.movement.boat-drive {:pos [10 63 2] :range 2} :later {}]
   ['jobs.movement.boat-launch {} :later {}]
   ['jobs.movement.boat-launch {:action :recover :id 7} :later {}]
   ['jobs.movement.boat-land {} :later {}]
   ['jobs.movement.cross-water {:pos [7 64 0]} :later {}]
   ['jobs.access.clear-path {:heading :east} :later {}]
   ['jobs.access.cleanup {:job "j"} :ok {}]
   ['jobs.access.toggle {:pos [3 64 0] :state :open} :later {}]
   ['jobs.access.pillar {:height 3} :later {}]
   ['jobs.access.bridge {:heading :east :length 3} :later {}]
   ['jobs.access.tunnel {:to [5 60 0]} :later {}]
   ['jobs.access.stair {:direction :down :steps 3} :later {}]
   ['jobs.explore.search {:target "diamond_ore" :count 2} :later {:found [{:what "diamond_ore" :pos [0 64 0]}]}]
   ['jobs.explore.search {:target "diamond_ore" :count 1} :ok {:found [{:what "diamond_ore" :pos [0 64 0]}]}]
   ['jobs.explore.look {} :later {}]
   ['jobs.time.noop {:seconds 5} :later {}]
   ['jobs.time.noop {:seconds 5} :ok {:until -1}]
   ['jobs.time.noop {:seconds 0} :ok {}]])

(deftest every-done-answers-a-status
  (doseq [[sym args kind mem] cases
          :let [r ((:done? (get registry/jobs sym)) (ctx-of args mem))]]
    (is (contract/status? r) (str sym " done? is a status"))
    (is (= kind (first r)) (str sym " " args))
    (is (every? #(contains? (nth r 2) %) [:units :done :left]) (str sym " carries its progress"))))

(def later-cases (filter #(= :later (nth % 2)) cases))

(deftest a-later-never-counts-its-goal-as-done
  (doseq [[sym args _ mem] later-cases
          :let [[_ _ {:keys [done left]}] ((:done? (get registry/jobs sym)) (ctx-of args mem))]]
    (is (not (and (pos? done) (zero? left))) (str sym " " args " counts nothing left yet is :later"))))
