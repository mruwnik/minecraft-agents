(ns engine.player-events-test
  "The player-joined and player-left body events as memory entries."
  (:require [cljs.test :refer [deftest is]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn engine-over [world]
  (let [[_ sink] (tu/legacy-capture-sink)
        p (tu/fake world)]
    {:p p
     :eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now (constantly 100000)
                        :events (events/make {:body "Fake" :sinks [sink] :now (constantly 100000)})})}))

(deftest player-events-become-memory-entries
  (let [{:keys [eng p]} (engine-over {})]
    (.emit (.-world p) #js {:kind "player-joined" :player "Ann"})
    (.emit (.-world p) #js {:kind "player-left" :player "Bob"})
    (is (= [{:player "Ann"}] (mapv :data (mem/entries (mem/view (:store eng)) :player-joined))))
    (is (= [{:player "Bob"}] (mapv :data (mem/entries (mem/view (:store eng)) :player-left))))))
