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
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.hide-spot :as hide-spot]
            [jobs.lib.lava-risk :as lava-risk]
            [jobs.lib.look :as look]
            [jobs.lib.price :as price]
            [jobs.lib.threats :as threats]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.movement.go-to.risk :as risk]))

(def inside {:x 5 :y 64 :z 0})
(def night-hut (merge (hut/hut-world inside {}) {:time st/night}))

(deftest a-way-down-from-the-hut-is-less-exposed-than-the-same-way-over-open-ground
  (let [c (ready-test/ctx-of night-hut)
        flat (risk/estimate c {:x 95 :y 64 :z 0})
        down (risk/estimate c {:x 5 :y 20 :z 90})]
    (is (pos? (:dark-s flat)))
    (is (< (:dark-s down) (/ (:dark-s flat) 2)))))

(deftest seen-lava-by-the-way-adds-hp-scaled-by-closeness-and-lava-far-from-it-adds-none
  (let [c (ready-test/ctx-of (assoc night-hut :time st/noon :blocks {"22,64,20" "lava"}))
        hp #(:lava-hp (risk/estimate c %))
        p (:primitives c)]
    (is (pos? (hp {:x 20 :y 64 :z 20})))
    (is (zero? (hp {:x 10 :y 64 :z 20})))
    (is (> (lava-risk/hp p [21 64 20]) (lava-risk/hp p [19 64 20]) 0))
    (is (= (hp {:x 20 :y 64 :z 20}) (lava-risk/hp p [20 64 20])) "the goal is the nearest cell to the lava")))

(deftest a-body-already-at-a-goal-beside-seen-lava-owes-no-lava-risk
  (let [c (ready-test/ctx-of (assoc night-hut :time st/noon :blocks {"22,64,20" "lava"}))
        here {:x 21 :y 64 :z 20}]
    (is (pos? (lava-risk/hp (:primitives c) [21 64 20])))
    (is (zero? (risk/route-lava-hp (:primitives c) here here 1)))))

(deftest a-dark-way-by-day-is-not-exposed
  (let [c (ready-test/ctx-of (assoc night-hut :time st/noon))]
    (is (zero? (:hp (risk/estimate c {:x 95 :y 64 :z 0}))))))

(deftest a-roofed-dark-way-prices-spawns-only-through-a-dark-hall
  (let [c (ready-test/ctx-of (assoc night-hut :time st/noon))]
    (with-redefs [sh-sense/sky-reaches-from-above? (constantly false)
                  sh-sense/block-light-at (constantly 0)]
      (is (zero? (:hp (with-redefs [ready/hall-at? (constantly false)] (risk/estimate c {:x 95 :y 64 :z 0})))) "a small dark tunnel is free")
      (is (pos? (:hp (with-redefs [ready/hall-at? (constantly true)] (risk/estimate c {:x 95 :y 64 :z 0})))) "a dark hall costs spawns"))))

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

(defn estimate-unseen
  "risk/estimate with the death spots never seen (the fake world sees every cell, which would clear them)."
  [c goal]
  (with-redefs [look/seen-block (constantly {:unknown true})] (risk/estimate c goal)))

(deftest a-recent-death-near-the-goal-adds-its-fight-to-the-risk-even-by-day
  (let [c (ready-test/ctx-of day-world)
        on-way (with-death c {:x 80 :y 64 :z 5} "zombie_villager" 60000)
        off-way (with-death c {:x 50 :y 64 :z 5} "zombie_villager" 60000)]
    (is (zero? (:hp (risk/estimate c far-goal))))
    (is (pos? (:death-hp (estimate-unseen on-way far-goal))))
    (is (zero? (:death-hp (estimate-unseen off-way far-goal))))))

(deftest the-planner-keeps-away-from-a-recent-death-until-a-mob-death-spot-is-seen-lit
  (let [dark (assoc day-world :light {[80 64 5] [0 0]})
        spots (fn [world cause seen]
                (with-redefs [look/seen-block (constantly seen)]
                  (mapv (juxt :x :mob) (js->clj (threats/planner-dangers (with-death (ready-test/ctx-of world) {:x 80 :y 64 :z 5} cause 60000)) :keywordize-keys true))))]
    (doseq [[world cause seen want why] [[day-world "zombie" {:unknown true} [[80 "zombie"]] "never seen"]
                                         [day-world "zombie" {:age-ms 1000} [] "seen lit since the death"]
                                         [dark "zombie" {:age-ms 1000} [[80 "zombie"]] "seen since but dark: the killer spawns there again"]
                                         [day-world "fall" {:age-ms 1000} [[80 "fall"]] "a fall death is not cleared by sight"]
                                         [day-world "drown" {:age-ms 1000} [[80 "drown"]] "a drowning is not cleared by sight"]]]
      (is (= want (spots world cause seen)) why))))

(deftest a-recent-hide-near-the-goal-adds-the-fight-with-its-mobs-to-the-risk
  (let [c (ready-test/ctx-of day-world)
        now 36000000
        with-hide (fn [pos] (assoc c :view (fn [] {:now now :data {:policies {:hid hide-spot/policy} :entries {:hid [{:t (- now 60000) :data {:pos pos :mobs ["zombie"]}}]}}})))]
    (is (pos? (:hide-hp (risk/estimate (with-hide {:x 80 :y 64 :z 5}) far-goal))))
    (is (zero? (:hide-hp (risk/estimate (with-hide {:x 50 :y 64 :z 5}) far-goal))))
    (is (zero? (:hide-hp (risk/estimate (with-hide inside) {:x 20 :y 64 :z 0}))) "the hide the body stands in is no threat on a walk away from it")))

(deftest only-seen-mobs-count-towards-repeated-hides
  (let [c (ready-test/ctx-of day-world)
        now 36000000
        with-hides (fn [n] (assoc c :view (fn [] {:now now :data {:policies {:hid hide-spot/policy} :entries {:hid (vec (repeat n {:t (- now 60000) :data {:pos {:x 0 :y 0 :z 0} :mobs ["zombie"]}}))}}})))
        at {:x 1 :y 0 :z 1}]
    (is (hide-spot/repeated? (with-hides 3) at ["zombie"]))
    (is (not (hide-spot/repeated? (with-hides 2) at ["zombie"])))
    (is (not (hide-spot/repeated? (with-hides 3) at ["spider"])))
    (is (not (hide-spot/repeated? (with-hides 3) at [])) "a heard mob has no name to count")))

(deftest a-death-fades-with-time
  (let [c (ready-test/ctx-of day-world)
        at {:x 80 :y 64 :z 5}
        hp-at #(:death-hp (estimate-unseen (with-death c at "zombie_villager" %) far-goal))]
    (is (> (hp-at 60000) (hp-at 3600000)))
    (is (pos? (hp-at 3600000)))
    (is (zero? (hp-at 8000000)))))

(deftest a-hungry-body-affords-less-risk-than-a-fed-one-unless-it-cannot-heal
  (let [status-for (fn [food inventory] (let [c (-> (ready-test/ctx-of (assoc day-world :self {:pos inside :health 20 :food food} :inventory inventory))
                                                    (with-death {:x 80 :y 64 :z 5} "witch" 6480000))]
                                          (first (risk/status c far-goal))))
        bread [{:name "bread" :count 3}]]
    (is (= :ok (status-for 20 [])))
    (is (= :later (status-for 5 bread)))
    (is (= :ok (status-for 5 [])) "nothing to eat: waiting never lowers the risk, so the walk goes")))

(deftest a-goal-worth-enough-lets-a-walk-take-a-risk-over-the-share-of-health
  (let [status-for (fn [worth] (let [c (-> (ready-test/ctx-of (assoc day-world :self {:pos inside :health 20 :food 5} :inventory [{:name "bread" :count 3}]))
                                           (with-death {:x 80 :y 64 :z 5} "witch" 6480000)
                                           (assoc-in [:args :worth] worth))]
                                 (first (risk/status c far-goal))))]
    (is (= :later (status-for nil)) "no worth said: the share of health decides")
    (is (= :later (status-for 5)) "a goal worth little buys little risk")
    (is (= :ok (status-for 400)))))

(deftest a-goal-worth-sets-the-allowance-not-just-raises-it
  (let [allowed (fn [self worth inv] (price/allowed (ready-test/ctx-of (assoc day-world :self self :inventory inv)) worth))
        fed {:pos inside :health 10 :food 20}
        starving {:pos inside :health 10 :food 3}]
    (is (= 5 (allowed fed nil [])) "no worth said: half the health")
    (is (= 9 (allowed starving nil [])) "no worth, cannot heal: all but 1")
    (is (< (allowed fed 5 []) 0.5) "a goal worth little buys little, under the share of health")
    (is (< (allowed starving 5 []) 0.5) "a body that cannot heal is no freer with a worthless goal")
    (is (= 9 (allowed fed 100000 [])) "a goal worth a lot: all but 1")))

(deftest a-death-by-terrain-adds-no-risk
  (let [c (ready-test/ctx-of day-world)]
    (doseq [cause ["fall" "lava" "drown" "starve" "a_new_cause" nil]]
      (is (zero? (:death-hp (estimate-unseen (with-death c {:x 80 :y 64 :z 5} cause 60000) far-goal))) cause))
    (is (pos? (:death-hp (estimate-unseen (with-death c {:x 80 :y 64 :z 5} "skeleton" 60000) far-goal))))))

(deftest a-far-goal-is-judged-on-a-bounded-number-of-reads
  (let [c (ready-test/ctx-of night-hut)
        reads (atom 0)
        counted (js/Proxy. (:primitives c)
                           #js {:get (fn [t k] (let [v (aget t k)]
                                                 (if (fn? v) (fn [& args] (swap! reads inc) (.apply v t (to-array args))) v)))})
        est (risk/estimate (assoc c :primitives counted) {:x 1000000 :y 64 :z 3})]
    (is (< @reads 20000))
    (is (pos? (:dark-s est)))))

(deftest a-death-or-hide-at-the-goal-adds-no-risk-but-one-further-off-does
  (let [c (ready-test/ctx-of day-world)
        now 36000000
        with-hide (fn [pos] (assoc c :view (fn [] {:now now :data {:policies {:hid hide-spot/policy} :entries {:hid [{:t (- now 60000) :data {:pos pos :mobs ["zombie"]}}]}}})))]
    (is (zero? (:death-hp (estimate-unseen (with-death c {:x 93 :y 64 :z 2} "zombie" 60000) far-goal))))
    (is (zero? (:hide-hp (risk/estimate (with-hide {:x 93 :y 64 :z 2}) far-goal))))
    (is (pos? (:death-hp (estimate-unseen (with-death c {:x 80 :y 64 :z 5} "zombie" 60000) far-goal))))))

(deftest a-walk-that-does-not-approach-a-death-pays-nothing-for-it
  (let [c (ready-test/ctx-of day-world)
        died (with-death c {:x 40 :y 64 :z 0} "enderman" 60000)]
    (is (pos? (:death-hp (estimate-unseen died {:x 20 :y 64 :z 0}))) "closer to the death than the body is")
    (is (zero? (:death-hp (estimate-unseen died inside))) "standing where the body is")
    (is (zero? (:death-hp (estimate-unseen died {:x 0 :y 64 :z 0}))) "walking away from the death")))

(deftest a-death-spot-seen-since-the-death-is-no-longer-a-threat
  (let [c (ready-test/ctx-of day-world)
        died (with-death c {:x 80 :y 64 :z 5} "enderman" 60000)
        hp-with (fn [seen] (with-redefs [look/seen-block (constantly seen)] (:death-hp (risk/estimate died far-goal))))]
    (is (pos? (hp-with {:unknown true})) "never seen")
    (is (pos? (hp-with {:name "air" :age-ms 120000})) "last seen before the death")
    (is (zero? (hp-with {:name "air" :age-ms 30000})) "seen after the death")
    (is (pos? (with-redefs [look/seen-block (constantly {:name "air" :age-ms 30000})]
                (:death-hp (risk/estimate (with-death (ready-test/ctx-of (assoc day-world :light {[80 64 5] [0 0]})) {:x 80 :y 64 :z 5} "enderman" 60000) far-goal))))
        "seen after the death but dark")))

(deftest a-death-spot-counts-as-lit-only-when-seen
  (let [p (:primitives (ready-test/ctx-of day-world))
        at {:x 80 :y 64 :z 5}
        lit-with (fn [seen] (with-redefs [look/seen-block (constantly seen)] (death-spot/lit? p at)))]
    (is (true? (lit-with {:name "air" :age-ms 1000})))
    (is (false? (lit-with {:unknown true})) "the light data of a cell never seen is no sight of it")))

(deftest a-walk-over-the-allowance-goes-when-staying-costs-more
  (let [zombies (for [i (range 3)] {:id (+ 7 i) :name "zombie" :kind "hostile" :pos {:x (+ 8 i) :y 64 :z 3}})
        status-for (fn [entities]
                     (first (risk/status (-> (ready-test/ctx-of (assoc day-world :self {:pos inside :health 2.3 :food 5} :entities (vec entities)))
                                             (with-death {:x 80 :y 64 :z 5} "zombie" 6480000))
                                         far-goal)))]
    (is (= :ok (status-for zombies)) "hostiles reach the body where it stands: the walk is the cheaper risk")
    (is (= :later (status-for [])) "nothing near the body: waiting costs nothing, so it waits")))

(deftest a-body-that-cannot-heal-and-is-no-safer-staying-ends-stranded-instead-of-waiting
  (let [zombies (for [i (range 3)] {:id (+ 7 i) :name "zombie" :kind "hostile" :pos {:x (+ 8 i) :y 64 :z 3}})
        c (ready-test/ctx-of (assoc night-hut :self {:pos inside :health 2.3 :food 5} :entities (vec zombies)))
        s (risk/status c far-goal)]
    (is (= :never (first s)))
    (is (= :stranded (:reason (nth s 2))))))

(deftest a-healing-body-on-dark-open-ground-goes-on-when-waiting-there-costs-as-much
  (let [c (ready-test/ctx-of (assoc ready-test/open-night :self {:pos {:x 0 :y 64 :z 0} :health 5 :food 15} :inventory [{:name "bread" :count 3}]))
        goal {:x 40 :y 64 :z 0}]
    (is (> (:hp (risk/estimate c goal)) (price/allowed c)) "over the half-of-health allowance")
    (is (= :ok (first (risk/status c goal))))))

(deftest a-hostile-near-the-far-end-of-a-way-does-not-make-staying-dear-enough-to-walk-past-lava
  (let [zombie {:id 9 :name "zombie" :kind "hostile" :pos {:x 20 :y 64 :z 40}}
        c (ready-test/ctx-of (assoc day-world :self {:pos inside :health 3 :food 5} :entities [zombie] :blocks {"22,64,20" "lava"}))]
    (is (> (:lava-hp (risk/estimate c {:x 20 :y 64 :z 20})) (price/allowed c)))
    (is (= :later (first (risk/status c {:x 20 :y 64 :z 20}))))))
