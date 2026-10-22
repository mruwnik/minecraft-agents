(ns engine.go-to-risk-test
  "go-to's start check judges the walk's risk (jobs.movement.go-to.risk), not the clock: hostiles near a long dark way hold it
  with the figures in the reason; a way that goes mostly underground costs far less than the same distance over open ground."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hut-shelter-test :as hut]
            [engine.ready-test :as ready-test]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [engine.test-util :as tu]
            [jobs.movement.go-to.risk :as risk]))

(def inside {:x 5 :y 64 :z 0})
(def night-hut (merge (hut/hut-world inside {}) {:time st/night}))

(deftest a-way-down-from-the-hut-is-less-exposed-than-the-same-way-over-open-ground
  (let [c (ready-test/ctx-of night-hut)
        flat (risk/estimate c {:x 95 :y 64 :z 0})
        down (risk/estimate c {:x 5 :y 20 :z 90})]
    (is (pos? (:dark-s flat)))
    (is (< (:dark-s down) (/ (:dark-s flat) 2)))))

(deftest a-dark-way-by-day-is-not-exposed
  (let [c (ready-test/ctx-of (assoc night-hut :time st/noon))]
    (is (zero? (:hp (risk/estimate c {:x 95 :y 64 :z 0}))))))

(deftest hostiles-near-the-way-hold-a-walk-with-the-risk-in-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombies (for [i (range 3)] {:id (+ 7 i) :name "zombie" :kind "hostile" :pos {:x (+ 40 (* 4 i)) :y 64 :z 6}})
              {:keys [eng p]} (let [r (st/setup (assoc night-hut :entities (vec zombies)))] (sky/with-sky (:p r)) r)
              id (core/submit! eng '(jobs.movement.go-to {:pos [95 64 0]}) {})]
          (await (st/tick-n eng 6))
          (is (= inside (st/pos-of p)) "the body stays in")
          (let [w (core/waiting eng id)]
            (is (= :risky (:reason w)))
            (is (re-find #"hp" (str (:text w))))))))))
