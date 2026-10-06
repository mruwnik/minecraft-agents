(ns engine.player-events-test
  "The player-joined and player-left body events as memory entries, and the :player-joined trigger."
  (:require [cljs.test :refer [deftest is are]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.triggers.player-joined :as player-joined]))

(defn entry-view
  "A memory view at :now 100000 holding :player-joined entries written at the given times."
  [times]
  {:data {:entries {:player-joined (mapv (fn [t] {:t t :data {:player "Ann"}}) times)}}
   :now 100000})

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

(deftest player-joined-holds-for-a-recent-join
  (are [label times args expected]
       (= expected (boolean (player-joined/player-joined nil (entry-view times) args)))
    "no entry" [] {} false
    "just now" [99000] {} true
    "inside the window" [91000] {} true
    "outside the window" [50000] {} false
    "a narrower :window-s" [91000] {:window-s 5} false))

(deftest player-joined-is-registered-with-notify
  (is (= player-joined/player-joined (:when (:player-joined triggers/all))))
  (is (= :player-joined (:name (:player-joined triggers/all))))
  (is (= 'jobs.debug.notify (first (:job (:player-joined triggers/all)))))
  (is (= :cooldown (:persistence (:player-joined triggers/all)))))
