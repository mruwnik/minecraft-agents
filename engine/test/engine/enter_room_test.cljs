(ns engine.enter-room-test
  "jobs.survival.enter-room: walk into the home (or stay in the roofed room the body is in), shut its open door, fill its
  doorway; end for what it cannot do."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hut-shelter-test :refer [hut-world door-states door-open?]]
            [engine.memory :as mem]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]))

(def open-door-states (update-vals door-states #(assoc % :open true)))

(defn ^:async enter!
  "Run (jobs.survival.enter-room args) over world, with :home at home when given: {:eng :p :seen}."
  [world args & [home]]
  (let [{:keys [eng p seen] :as s} (st/setup (assoc world :time st/night))]
    (when home (mem/write! (:store eng) :home {:pos home} mem/place-policy))
    (core/submit! eng (list 'jobs.survival.enter-room args) {})
    (await (tu/run-until-empty eng 30))
    s))

(deftest a-body-in-the-hut-shuts-its-open-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 0} {}) :states open-door-states) {}))]
          (is (false? (door-open? p)) "the door is shut")
          (is (empty? (st/calls p "place")) "nothing placed")
          (is (empty? (st/ended-because seen :door-open))))))))

(deftest a-door-that-stays-open-is-named-and-ends-the-job-door-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (hut-world {:x 5 :y 64 :z 0} {}) :states open-door-states)
              {:keys [eng p seen]} (st/setup (assoc world :time st/night))]
          (.override (.-world p) "useOn" (fn [token a impl] (impl token (clj->js {:pos {:x 3 :y 64 :z 0}}))))
          (core/submit! eng '(jobs.survival.enter-room {}) {})
          (await (tu/run-until-empty eng 30))
          (is (true? (door-open? p)) "the click did nothing")
          (is (= [{:door [5 64 2]}] (st/entries eng :night-door-failed)))
          (is (= 1 (count (st/emitted seen :shelter.door-failed))))
          (is (seq (st/ended-because seen :door-open))))))))

(deftest a-forgotten-home-and-no-roof-ends-no-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (enter! {:floor tu/walk-floor :self {:pos {:x 0 :y 64 :z 0}}} {}))]
          (is (seq (st/ended-because seen :no-room)))
          (is (empty? (st/calls p "place"))))))))

(deftest a-body-outside-walks-into-the-home-and-shuts-the-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 6} {}) :states open-door-states) {} {:x 5 :y 64 :z 0}))]
          (is (< (:z (st/pos-of p)) 2) "inside the hut")
          (is (false? (door-open? p)) "the door is shut"))))))
