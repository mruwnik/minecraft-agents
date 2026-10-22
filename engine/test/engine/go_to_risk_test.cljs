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

(def day-world (assoc night-hut :time st/noon))
(def far-goal {:x 95 :y 64 :z 0})

(defn with-death
  "ctx c remembering a death of `cause` at pos, `age-ms` ago (clock at 10 hours)."
  [c pos cause age-ms]
  (let [now 36000000]
    (assoc c :view (fn [] {:now now :data {:policies {:deaths core/deaths-policy} :entries {:deaths [{:t (- now age-ms) :data {:pos pos :cause cause :death-t (- now age-ms)}}]}}}))))

(deftest a-recent-death-near-the-goal-adds-its-fight-to-the-risk-even-by-day
  (let [c (ready-test/ctx-of day-world)
        on-way (with-death c {:x 90 :y 64 :z 5} "zombie_villager" 60000)
        off-way (with-death c {:x 50 :y 64 :z 5} "zombie_villager" 60000)]
    (is (zero? (:hp (risk/estimate c far-goal))))
    (is (pos? (:death-hp (risk/estimate on-way far-goal))))
    (is (zero? (:death-hp (risk/estimate off-way far-goal))))))

(deftest a-death-fades-with-time
  (let [c (ready-test/ctx-of day-world)
        at {:x 90 :y 64 :z 5}
        hp-at #(:death-hp (risk/estimate (with-death c at "zombie_villager" %) far-goal))]
    (is (> (hp-at 60000) (hp-at 3600000)))
    (is (pos? (hp-at 3600000)))
    (is (zero? (hp-at 8000000)))))

(deftest a-hungry-body-affords-less-risk-than-a-fed-one
  (let [status-for (fn [food] (let [c (-> (ready-test/ctx-of (assoc day-world :self {:pos inside :health 20 :food food}))
                                          (with-death {:x 90 :y 64 :z 5} "zombie" 6480000))]
                                (first (risk/status c far-goal))))]
    (is (= :ok (status-for 20)))
    (is (= :later (status-for 5)))))
