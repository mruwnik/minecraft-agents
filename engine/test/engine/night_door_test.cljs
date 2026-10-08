(ns engine.night-door-test
  "The night shuts an open door of the closed room the body stands in instead of digging in behind it."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.hut-shelter-test :refer [hut-world door-states door-open?]]
            [engine.memory :as mem]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def open-door-states (update-vals door-states #(assoc % :open true)))

(defn open-door-hut [pos extra] (assoc (hut-world pos extra) :states open-door-states :time st/night))

(defn fires-seen? [world]
  (boolean ((:when (get triggers/all :night)) (tu/fake world) (mem/view (:store (:eng (st/setup {})))) {})))

(deftest night-holds-for-a-closed-room-with-an-open-door-until-the-door-is-shut
  (are [world held] (= held (fires-seen? world))
    (open-door-hut {:x 5 :y 64 :z 0} {}) true
    (assoc (open-door-hut {:x 5 :y 64 :z 0} {}) :states door-states) false
    (open-door-hut {:x 5 :y 64 :z 0} {"5,67,0" "air"}) true))

(deftest night-in-a-room-with-an-open-door-shuts-the-door-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (open-door-hut {:x 5 :y 64 :z 0} {}) {:inventory st/dirt-stack}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 8))
          (is (false? (door-open? p)) "the door is shut")
          (is (= [] (st/calls p "place")) "nothing placed")
          (is (= [] (st/calls p "dig")) "nothing dug")
          (is (= [] (st/entries eng :shelter)) "no dig-in shelter"))))))

(deftest night-in-a-room-with-an-open-door-and-a-hole-in-the-roof-digs-in-as-before
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (open-door-hut {:x 5 :y 64 :z 0} {"5,67,0" "air"}) {:inventory st/dirt-stack}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 8))
          (is (seq (st/calls p "place")) "dug in: the room cannot be closed"))))))
