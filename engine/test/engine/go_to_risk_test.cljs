(ns engine.go-to-risk-test
  "go-to's start check judges the walk's risk (jobs.lib.walk-risk), not the clock: hostiles near a long dark way hold it
  with the figures in the reason; a way that goes mostly underground costs far less than the same distance over open ground."
  (:require [cljs.test :refer [are deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
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
            [jobs.lib.walk-risk :as risk]
            [jobs.movement.go-to.reprice :as reprice]))

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
(def dark-spot-world "day-world with the death spot of the tests, [80 64 5], dark (a cave): its killer may be there by day too." (assoc day-world :light {[80 64 5] [0 0]}))

(deftest a-hostile-that-catches-a-lit-walk-is-priced-one-the-walk-leaves-behind-is-not
  (are [zombie-pos priced?] (= priced? (pos? (:hostile-hp (risk/estimate (ready-test/ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}
                                                                                            :entities [{:id 7 :name "zombie" :kind "hostile" :pos zombie-pos}]})
                                                                         {:x 30 :y 64 :z 0}))))
    {:x 15 :y 64 :z 3} true
    {:x -10 :y 64 :z 0} false))

(deftest a-hostile-seen-mid-walk-asks-go-to-s-check-again-for-the-rest-of-it
  (let [c (ready-test/ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}})
        goal {:x 30 :y 64 :z 0}
        check (reprice/check c goal nil)
        replaced (reprice/check (assoc c :quote-call {:checks {:safe? '(always)}}) goal nil)]
    (is (= :ok (first (reprice/status c goal))) "the round start's check, cached")
    (is (nil? (check {} false)) "no hostile known")
    (fake/add-entity! (:primitives c) {:id 7 :name "zombie" :kind "hostile" :pos {:x 4 :y 64 :z 0}})
    (is (nil? (check {} true)) "the round's first plan: the engine has just asked")
    (is (= :later (first (:hostile (check {} false)))) "a zombie ahead on the way holds the rest of the walk")
    (is (nil? (replaced {} false)) "a caller's :checks :safe? replaces go-to's")))
(def far-goal {:x 95 :y 64 :z 0})

(defn with-death
  "ctx c remembering a death of `cause` at pos, `age-ms` ago (clock at 10 hours), and the entries {kind [entry]} of more."
  ([c pos cause age-ms] (with-death c pos cause age-ms {}))
  ([c pos cause age-ms more]
   (let [now 36000000]
     (assoc c :view (fn [] {:now now :data {:policies {:deaths core/deaths-policy} :entries (merge more {:deaths [{:t (- now age-ms) :data {:pos pos :cause cause :death-t (- now age-ms)}}]})}})))))

(defn estimate-unseen
  "risk/estimate with the death spots never seen (the fake world sees every cell, which would clear them)."
  [c goal]
  (with-redefs [look/seen-block (constantly {:unknown true})] (risk/estimate c goal)))

(defn with-hits
  "ctx c remembering :hurt entries [[health pos]] a minute ago, newest last (clock at 10 hours)."
  [c hits]
  (let [now 36000000]
    (assoc c :view (fn [] {:now now :data {:entries {:hurt (mapv (fn [[health pos]] {:t (- now 60000) :data {:health health :pos pos :attacker {:name "skeleton"}}}) hits)}}}))))

(deftest a-hit-down-to-near-death-prices-its-spot-like-a-death-one-spot-for-a-fight
  (let [c (ready-test/ctx-of day-world)
        spot {:x 80 :y 64 :z 5}
        hp-of #(:death-hp (estimate-unseen (with-hits c %) far-goal))]
    (is (zero? (hp-of [[12 spot]])) "a hit that leaves 12 hp is no near-death")
    (is (pos? (hp-of [[5 spot]])))
    (is (= 1 (count (death-spot/recent (with-hits c (for [h (range 5 0 -1)] [h (update spot :x + h)]))))) "a fight's hits are one spot")
    (is (= 2 (count (death-spot/recent (with-hits c [[5 spot] [5 (update spot :x + 30)]])))))))

(deftest a-near-death-fades-over-the-hurt-memory-ttl-and-counts-once-with-a-death
  (let [c (ready-test/ctx-of day-world)
        now 36000000
        spot {:x 80 :y 64 :z 5}
        view (fn [hurt-ttl deaths]
               (assoc c :view (fn [] {:now now
                                      :data {:policies {:hurt {:cap 50 :ttl hurt-ttl} :deaths core/deaths-policy}
                                             :entries {:hurt [{:t (- now 1800000) :data {:health 3 :pos spot :attacker {:name "skeleton"}}}]
                                                       :deaths deaths}}})))
        weights #(mapv :weight (death-spot/recent %))]
    (is (< 0.49 (first (weights (view 3600000 []))) 0.51) "half the 1 h hurt ttl gone")
    (is (< 0.74 (first (weights (view 7200000 []))) 0.76) "the ttl is read from the stored policy")
    (is (= ["zombie"] (mapv :cause (death-spot/recent (view 3600000 [{:t (- now 600000) :data {:pos (update spot :x + 3) :cause "zombie" :death-t (- now 600000)}}]))))
        "a hit beside a death is the death's spot")))

(deftest a-recent-death-near-the-goal-adds-its-fight-to-the-risk-even-by-day
  (let [c (ready-test/ctx-of day-world)
        on-way (with-death c {:x 80 :y 64 :z 5} "husk" 60000)
        off-way (with-death c {:x 50 :y 64 :z 5} "husk" 60000)]
    (is (zero? (:hp (risk/estimate c far-goal))))
    (is (pos? (:death-hp (estimate-unseen on-way far-goal))))
    (is (zero? (:death-hp (estimate-unseen off-way far-goal))))))

(deftest the-planner-keeps-away-from-a-recent-death-until-a-mob-death-spot-is-seen-lit
  ;; a death by a mob is a known threat (tolls over its killer's field), one by terrain a planner danger
  (let [dark dark-spot-world
        spots (fn [world cause seen]
                (with-redefs [look/seen-block (constantly seen)]
                  (let [c (with-death (ready-test/ctx-of world) {:x 80 :y 64 :z 5} cause 60000)]
                    (into (mapv (juxt :x :mob) (js->clj (threats/planner-dangers c) :keywordize-keys true))
                          (map (juxt (comp :x :pos) :mob)) (threats/known-threats c)))))]
    (doseq [[world cause seen want why] [[day-world "zombie" {:unknown true} [[80 "zombie"]] "never seen"]
                                         [day-world "zombie" {:age-ms 1000} [] "seen lit since the death"]
                                         [dark "zombie" {:age-ms 1000} [[80 "zombie"]] "seen since but dark: the killer spawns there again"]
                                         [day-world "fall" {:age-ms 1000} [[80 "fall"]] "a fall death is not cleared by sight"]
                                         [day-world "drown" {:age-ms 1000} [[80 "drown"]] "a drowning is not cleared by sight"]]]
      (is (= want (spots world cause seen)) why))))

(deftest a-recent-hide-near-the-goal-adds-the-fight-with-its-mobs-to-the-risk
  (let [c (ready-test/ctx-of day-world)
        now 36000000
        with-hide (fn [pos] (assoc c :view (fn [] {:now now :data {:policies {:hid hide-spot/policy} :entries {:hid [{:t (- now 60000) :data {:pos pos :mobs ["spider"]}}]}}})))]
    (is (pos? (:death-hp (risk/estimate (with-hide {:x 80 :y 64 :z 5}) far-goal))))
    (is (zero? (:death-hp (risk/estimate (with-hide {:x 50 :y 64 :z 5}) far-goal))))
    (is (zero? (:death-hp (risk/estimate (with-hide inside) {:x 20 :y 64 :z 0}))) "the hide the body stands in is no threat on a walk away from it")))

(deftest only-seen-mobs-count-towards-repeated-hides
  (let [c (ready-test/ctx-of day-world)
        now 36000000
        with-hides (fn [n] (assoc c :view (fn [] {:now now :data {:policies {:hid hide-spot/policy} :entries {:hid (vec (repeat n {:t (- now 60000) :data {:pos {:x 0 :y 64 :z 0} :mobs ["zombie"]}}))}}})))
        at {:x 1 :y 64 :z 1}]
    (is (hide-spot/repeated? (with-hides 3) at ["zombie"]))
    (is (not (hide-spot/repeated? (with-hides 2) at ["zombie"])))
    (is (not (hide-spot/repeated? (with-hides 3) at ["spider"])))
    (is (not (hide-spot/repeated? (with-hides 3) at [])) "a heard mob has no name to count")))

(deftest a-death-fades-with-time
  ;; seen lit before the death, within 32 blocks of the body: no new spawn, and its killer does not despawn
  (let [c (ready-test/ctx-of day-world)
        at {:x 30 :y 64 :z 2}
        hp-at #(with-redefs [look/seen-block (constantly {:name "air" :age-ms 1e8})]
                 (:death-hp (risk/estimate (with-death c at "husk" %) {:x 40 :y 64 :z 0})))]
    (is (> (hp-at 60000) (hp-at 3600000)))
    (is (pos? (hp-at 3600000)))
    (is (zero? (hp-at 8000000)))))

(def bed-world "day-world with a bed beside the body's place, which with-bed sets its respawn at: a death costs a short walk back." (update day-world :blocks assoc "5,64,1" "red_bed"))

(def with-bed "with-death's more for bed-world." {:spawn-bed [{:t 35000000 :data {:pos {:x 5 :y 64 :z 1}}}]})

(defn in-lit-cave
  "(f) with every cell seen lit before the deaths and roofed from the sky: no mob spawns there and none burns."
  [f]
  (with-redefs [look/seen-block (constantly {:name "air" :age-ms 1e8}) sh-sense/sky-reaches-from-above? (constantly false)] (f)))

(deftest a-hungry-body-affords-less-risk-than-a-fed-one-unless-it-cannot-heal
  ;; a skeleton death 105 s ago, 75 blocks off: about a sixth of a chance it has not despawned; a bed by the body
  (let [status-for (fn [food inventory] (let [c (-> (ready-test/ctx-of (assoc bed-world :self {:pos inside :health 20 :food food} :inventory inventory))
                                                    (with-death {:x 80 :y 64 :z 5} "skeleton" 105000 with-bed))]
                                          (in-lit-cave #(first (risk/status c far-goal)))))
        bread [{:name "bread" :count 3}]]
    (is (= :ok (status-for 20 [])))
    (is (= :later (status-for 5 bread)))
    (is (= :ok (status-for 5 [])) "nothing to eat: waiting never lowers the risk, so the walk goes")))

(deftest a-goal-worth-enough-lets-a-walk-take-a-risk-over-the-share-of-health
  (let [status-for (fn [worth] (let [c (-> (ready-test/ctx-of (assoc bed-world :self {:pos inside :health 20 :food 5} :inventory [{:name "bread" :count 3}]))
                                           (with-death {:x 80 :y 64 :z 5} "skeleton" 105000 with-bed)
                                           (assoc-in [:args :worth] worth))]
                                 (in-lit-cave #(first (risk/status c far-goal)))))]
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
    (is (pos? (:death-hp (estimate-unseen (with-death c {:x 80 :y 64 :z 5} "spider" 60000) far-goal))))))

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
    (is (zero? (:death-hp (risk/estimate (with-hide {:x 93 :y 64 :z 2}) far-goal))))
    (is (pos? (:death-hp (estimate-unseen (with-death c {:x 80 :y 64 :z 5} "husk" 60000) far-goal))))))

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
                (:death-hp (risk/estimate (with-death (ready-test/ctx-of dark-spot-world) {:x 80 :y 64 :z 5} "enderman" 60000) far-goal))))
        "seen after the death but dark")))

(deftest a-death-spot-costs-while-its-killer-may-still-be-there-or-a-like-one-spawn-there
  ;; the spot seen before the death (so not seen-clear?), 75 blocks from the body a minute ago; dark-spot-world has no light data: all dark
  (let [respawned-far {:respawned [{:t (- 36000000 50000) :data {:pos {:x 300 :y 64 :z 5}}}]}
        hp (fn [world cause more] (with-redefs [look/seen-block (constantly {:name "air" :age-ms 120000})]
                                    (:death-hp (risk/estimate (with-death (ready-test/ctx-of world) {:x 80 :y 64 :z 5} cause 60000 more) far-goal))))
        dark (hp dark-spot-world "skeleton" {})]
    (is (< (* 0.5 dark) (in-lit-cave #(hp day-world "skeleton" {})) dark) "a lit cave stops new spawns, not the old skeleton")
    (is (< (* 0.5 (hp dark-spot-world "creeper" {})) (hp day-world "creeper" {})) "a creeper does not burn by day")
    (is (zero? (hp day-world "zombie" {})) "a zombie burns under the day sky")
    (is (pos? (hp day-world "creeper" {})))
    (is (zero? (hp day-world "creeper" respawned-far)) "the body went 128 blocks off: it despawned")
    (is (zero? (with-redefs [look/seen-block (constantly {:name "air" :age-ms 120000})]
                 (:hp (risk/estimate (with-death (ready-test/ctx-of day-world) {:x 80 :y 64 :z 5} "creeper" 60000 respawned-far) far-goal))))
        "nor any death it risks")))

(deftest a-walk-for-an-unseen-fresh-death-spot-risks-the-death-of-meeting-its-killer
  ;; a fist body with no bed; the death risk alone is over half its health
  (let [c (with-death (ready-test/ctx-of day-world) {:x 80 :y 64 :z 5} "skeleton" 10000)
        {:keys [lethal-hp]} (estimate-unseen c far-goal)]
    (is (> lethal-hp (price/allowed c)))
    (is (= :later (first (with-redefs [look/seen-block (constantly {:unknown true})] (risk/status c far-goal)))))))

(deftest a-drowned-death-spot-holds-while-the-body-stays-near-its-water-and-goes-once-it-went-far
  ;; 3 h old, past the 2 h fade of a walker's spot; 25 blocks from the body
  (let [at {:x 30 :y 64 :z 2}
        share (fn [more] (let [c (with-death (ready-test/ctx-of day-world) at "drowned" 10800000 more)]
                           (mapv #(death-spot/present-share c (assoc % :mob (:cause %))) (death-spot/recent c))))]
    (is (= [1] (share {})))
    (is (= [0] (share {:respawned [{:t (- 36000000 60000) :data {:pos {:x 300 :y 64 :z 5}}}]})))))

(deftest a-death-spot-counts-as-lit-only-when-seen
  (let [p (:primitives (ready-test/ctx-of day-world))
        at {:x 80 :y 64 :z 5}
        lit-with (fn [seen] (with-redefs [look/seen-block (constantly seen)] (death-spot/lit? p at)))]
    (is (true? (lit-with {:name "air" :age-ms 1000})))
    (is (false? (lit-with {:unknown true})) "the light data of a cell never seen is no sight of it")))

(deftest a-walk-over-the-allowance-goes-when-staying-costs-more
  (let [zombies (for [i (range 3)] {:id (+ 7 i) :name "zombie" :kind "hostile" :pos {:x (- 2 i) :y 64 :z 3}})
        status-for (fn [entities]
                     (first (risk/status (-> (ready-test/ctx-of {:time st/noon :light {[80 64 5] [0 0]} :self {:pos inside :health 2.3 :food 5} :entities (vec entities)})
                                             (with-death {:x 80 :y 64 :z 5} "zombie" 20000))
                                         far-goal)))]
    (is (= :ok (status-for zombies)) "hostiles reach the body where it stands, the walk leaves them behind: the walk is the cheaper risk")
    (is (= :later (status-for [])) "nothing near the body: waiting costs nothing, so it waits")))

(deftest a-body-that-cannot-heal-and-is-no-safer-staying-ends-stranded-instead-of-waiting
  ;; on open ground, the zombies by the way 30 blocks off: their fields reach the body's cell at their edge
  (let [zombies (for [i (range 3)] {:id (+ 7 i) :name "zombie" :kind "hostile" :pos {:x (+ 30 i) :y 64 :z 3}})
        c (ready-test/ctx-of (assoc ready-test/open-night :self {:pos {:x 0 :y 64 :z 0} :health 2.3 :food 5} :entities (vec zombies)))
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

