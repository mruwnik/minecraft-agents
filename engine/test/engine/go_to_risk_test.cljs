(ns engine.go-to-risk-test
  "go-to's start check judges the walk's risk (jobs.lib.walk-risk), not the clock: hostiles near a long dark way hold it
  with the figures in the reason; unseen ground is priced at the worse case (Q16)."
  (:require [cljs.test :refer [are deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.hut-shelter-test :as hut]
            [engine.path.planner-tuned :as planner]
            [engine.ready-test :as ready-test]
            [engine.settings :as settings]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.hide-spot :as hide-spot]
            [jobs.lib.lava-risk :as lava-risk]
            [jobs.lib.look :as look]
            [jobs.lib.price :as price]
            [jobs.lib.reach :as reach]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.ready :as ready]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.walk-risk :as risk]
            [jobs.lib.walk.drive :as drive]
            [jobs.movement.go-to.reprice :as reprice]))

(def inside {:x 5 :y 64 :z 0})
(def night-hut (merge (hut/hut-world inside {}) {:time st/night}))

(defn plan-of "A found plan of go-to over cells." [cells] {:status "found" :steps (vec cells)})

(defn straight-plan "A found plan of the body of c on the straight way to goal, at the feet's height." [c goal]
  (let [{:keys [x y z]} (sh-sense/feet (:primitives c))
        n (max 1 (js/Math.abs (- (:x goal) x)) (js/Math.abs (- (:z goal) z)))]
    (plan-of (for [i (range 1 (inc n))] {:x (js/Math.round (+ x (* i (/ (- (:x goal) x) n)))) :y y :z (js/Math.round (+ z (* i (/ (- (:z goal) z) n))))}))))

(defn planned-status "risk/status of the walk to goal planned straight." [c goal] (risk/status c goal (straight-plan c goal)))

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
    (is (zero? (risk/route-lava-hp (:primitives c) [here])))))

(deftest each-mob-spawned-on-a-dark-way-hurts-a-bare-body-at-most-its-life
  ;; the death past the life is the lethal part's: a fist fight that would cost 65 hp is no more hurt than 20
  (let [c (ready-test/ctx-of (assoc ready-test/open-night :self {:pos {:x 0 :y 64 :z 0} :health 20 :food 20}))
        {:keys [spawn-hp dark-s]} (risk/estimate c {:x 95 :y 64 :z 0})]
    (is (pos? spawn-hp))
    (is (<= spawn-hp (* dark-s (cost/spawn-chance [0 0 0]) 20)))))

(deftest a-dark-way-by-day-is-not-exposed
  (let [c (ready-test/ctx-of (assoc night-hut :time st/noon))]
    (is (zero? (:hp (risk/estimate c {:x 95 :y 64 :z 0}))))))

(deftest a-roofed-dark-way-prices-spawns-only-through-a-dark-hall
  (let [c (ready-test/ctx-of (assoc night-hut :time st/noon))]
    (with-redefs [sh-sense/sky-reaches-from-above? (constantly false)
                  sh-sense/block-light-at (constantly 0)]
      (is (zero? (:hp (with-redefs [ready/hall-at? (constantly false)] (risk/estimate c {:x 95 :y 64 :z 0} {:plan (straight-plan c {:x 95 :y 64 :z 0})})))) "a small dark tunnel is free")
      (is (pos? (:hp (with-redefs [ready/hall-at? (constantly true)] (risk/estimate c {:x 95 :y 64 :z 0} {:plan (straight-plan c {:x 95 :y 64 :z 0})})))) "a dark hall costs spawns"))))

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

(def straight-cells (for [x (range 1 31)] {:x x :y 64 :z 0}))

(def detour-cells
  "A way from [0 64 0] to [30 64 0] out to z 10 and back."
  (concat (for [z (range 1 11)] {:x 0 :y 64 :z z}) (for [x (range 1 31)] {:x x :y 64 :z 10}) (for [z (range 9 -1 -1)] {:x 30 :y 64 :z z})))

(deftest a-longer-detour-pays-its-spawn-and-lava-parts-on-its-own-cells
  (let [c (ready-test/ctx-of {:time st/night :self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks {"15,64,12" "lava"}})
        goal {:x 30 :y 64 :z 0}
        straight (risk/estimate c goal {:plan (plan-of straight-cells)})
        detour (risk/estimate c goal {:plan (plan-of detour-cells)})]
    (is (zero? (:lava-hp straight)))
    (is (pos? (:lava-hp detour)) "lava beside the detour, 12 blocks off the line")
    (is (> (:dark-s detour) (* 1.5 (:dark-s straight))) "50 dark blocks against 30")
    (is (> (:spawn-hp detour) (:spawn-hp straight)))))

(deftest a-partial-plan-prices-its-unplanned-rest-on-the-straight-line
  (let [c (ready-test/ctx-of {:time st/noon :floor [0 -24 80 24] :self {:pos {:x 2.5 :y 64 :z 0.5}}
                              :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 36.5 :y 64 :z 2.5}}]})
        goal {:x 70 :y 64 :z 0}
        line (for [x (range 3 71)] {:x x :y 64 :z 0})
        round (concat (for [z (range -1 -21 -1)] {:x 2 :y 64 :z z}) (for [x (range 3 71)] {:x x :y 64 :z -20}) (for [z (range -19 1)] {:x 70 :y 64 :z z}))
        hurt #(:hostile-hp (risk/estimate c goal {:plan %}))]
    (is (> (hurt {:status "partial" :steps (take 8 line)}) (* 4 (hurt (plan-of round)))) "the rest past x 10 runs by the zombie; the found way keeps 22 blocks off")
    (is (= (hurt {:status "partial" :steps (take 8 line)}) (hurt (plan-of line))) "the rest is the line")))

(deftest a-partial-plan-prices-its-rest-by-what-the-body-sees-of-it
  (let [c (ready-test/ctx-of {:time st/night :floor [0 -24 80 24] :self {:pos {:x 2.5 :y 64 :z 0.5}}
                              :light (into {} (for [x (range 0 81)] [[x 64 0] [0 15]]))})
        goal {:x 70 :y 64 :z 0}
        dark-s #(:dark-s (risk/estimate c goal {:plan {:status "partial" :steps (for [x (range 3 11)] {:x x :y 64 :z 0})}}))]
    (is (zero? (dark-s)) "the rest is a seen, torch-lit line at night")
    (is (pos? (with-redefs [sh-sense/seen-cell? (constantly false) look/glimpse? (constantly false)] (dark-s))) "an unseen rest is open ground at night")))

(deftest unseen-ground-below-the-surface-band-or-round-a-body-underground-is-priced-as-a-dark-cave
  (let [c (ready-test/ctx-of day-world)]
    (with-redefs [sh-sense/seen-cell? (constantly false) look/glimpse? (constantly false)]
      (is (zero? (:dark-s (risk/estimate c {:x 95 :y 64 :z 0}))) "unseen open ground by day is lit by the sky")
      (is (zero? (:dark-s (risk/estimate c {:x 95 :y 84 :z 0}))) "a goal above the body makes no cave of the ground at its level")
      (is (pos? (:dark-s (risk/estimate c {:x 5 :y 20 :z 90}))) "unseen ground far below may be a cave")
      (is (pos? (with-redefs [sh-sense/buried? (constantly true)] (:dark-s (risk/estimate c {:x 95 :y 64 :z 0}))))
          "by day too, unseen ground level with a body underground may be a cave"))))

(deftest a-held-plan-is-kept-until-the-body-moves-or-a-block-it-knows-changes
  (let [m (atom {})
        writes (atom 3)
        clock (atom 0)
        c (assoc (ready-test/ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}}) :update-mem (fn [f args] (apply swap! m f args)))
        goal {:x 30 :y 64 :z 0}]
    (with-redefs [ctx/mem (fn [_] @m) ctx/now (fn [_] @clock) sense/known-writes (fn [_] @writes)]
      (risk/note-plan! c goal (plan-of straight-cells))
      (reset! clock 600000)
      (is (= goal (:goal (risk/planned c))) "kept while nothing changed, however long")
      (swap! writes inc)
      (is (nil? (risk/planned c)) "a block the body knows changed: a plan made now might differ")
      (risk/note-plan! c goal (plan-of straight-cells))
      (swap! (fake/state (:primitives c)) assoc-in [:self :pos] [3.5 64 0.5])
      (is (nil? (risk/planned c)) "the body moved from where it planned"))))

(deftest a-hostile-beside-the-straight-line-is-priced-on-go-to-s-plan-round-its-field-the-cells-it-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:time st/noon :floor [0 -24 80 24] :self {:pos {:x 2.5 :y 64 :z 0.5}}
                     :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 36.5 :y 64 :z 2.5}}]}
              c (ready-test/ctx-of world)
              goal {:x 70 :y 64 :z 0}
              searches (atom 0)
              priced (atom [])
              walked (atom [])
              create-plan planner/create-plan
              way risk/way
              follow! drive/follow!]
          (set! planner/create-plan (fn [snapshot query options] (swap! searches inc) (create-plan snapshot query options)))
          (is (= :ok (first (reprice/status c goal))) "the start check before a plan")
          (is (zero? @searches) "the start check searches nothing")
          (is (> (:hostile-hp (risk/estimate c goal)) 1) "the straight line runs through the zombie's field")
          (set! risk/way (fn [feet goal plan] (swap! priced conj (:steps plan)) (way feet goal plan)))
          (set! drive/follow! (fn [c plan opts] (swap! walked conj (:steps plan)) (follow! c plan opts)))
          (try
            (let [{:keys [eng p]} (let [r (st/setup world)] (sky/with-sky (:p r)) r)]
              (core/submit! eng '(jobs.movement.go-to {:pos [70 64 0] :unknown false}) {})
              (await (st/tick-n eng 40))
              (is (seq @walked) "go-to walks")
              (is (= (first @walked) (first @priced)) "its first plan is the way priced")
              (is (< (:hostile-hp (risk/estimate c goal {:plan {:status "found" :steps (first @walked)}})) 1) "round the field")
              (is (> (:x (st/pos-of p)) 30) "past the zombie"))
            (finally
              (set! planner/create-plan create-plan)
              (set! risk/way way)
              (set! drive/follow! follow!))))))))

(deftest a-round-s-first-plan-and-a-hostile-seen-mid-walk-ask-go-to-s-check-for-the-rest-of-it
  (let [m (atom {})
        c (assoc (ready-test/ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}}) :update-mem (fn [f args] (apply swap! m f args)))
        goal {:x 30 :y 64 :z 0}
        check (reprice/check c goal nil)
        replaced (reprice/check (assoc c :quote-call {:checks {:safe? '(always)}}) goal nil)
        plan (plan-of straight-cells)]
    (is (nil? (check plan true)) "the first plan, no hostile known")
    (is (nil? (check plan false)) "no new hostile")
    (fake/add-entity! (:primitives c) {:id 7 :name "zombie" :kind "hostile" :pos {:x 4 :y 64 :z 0}})
    (is (= :later (first (:risk (check plan false)))) "a zombie ahead on the way holds the rest of the walk")
    (is (= :later (first (with-redefs [ctx/mem (fn [_] @m)] (reprice/status c goal)))) "the start check holds on the plan the round held on")
    (let [kept (atom false)]
      (is (:searching ((reprice/check c goal nil) {:status "partial" :steps (take 10 straight-cells) :r #js {:reason "searching"} :search-on #(reset! kept true)} true))
          "an unfinished search searches on")
      (is @kept "its search kept to go on with"))
    (is (nil? (replaced plan false)) "a caller's :checks :safe? replaces go-to's")))

;; a walk is priced over the affordable when it ends past x 10 (a found plan's end, else the goal), but within it when
;; unseen cells are not guessed caves
(defn prefix-estimate [_ goal & [{:keys [plan caves-free?]}]]
  (let [end (if (= "found" (:status plan)) (peek (:steps plan)) goal)]
    {:hp (if (or caves-free? (<= (:x end) 10)) 0 100)
     :spawn-hp 0 :hostile-hp 0 :death-hp 0 :lethal-hp 0 :lava-hp 0 :dark-s 0 :hostiles 0}))

(deftest a-plan-let-through-as-a-safe-prefix-has-every-re-plan-after-it-priced
  (let [m (atom {})
        c (assoc (ready-test/ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}}) :update-mem (fn [f args] (apply swap! m f args)))
        goal {:x 30 :y 64 :z 0}
        prefix {:status "partial" :steps (vec (take 10 straight-cells))}]
    (with-redefs [risk/estimate prefix-estimate]
      (is (= :later (first (reprice/status c goal prefix))) "the whole walk is over the affordable")
      (is (true? (reprice/safe-prefix? c goal prefix)))
      (let [check (reprice/check c goal nil)]
        (is (nil? (check prefix true)) "the prefix is walked")
        (is (= :later (first (:risk (check (plan-of straight-cells) false)))) "a refresh past the prefix, no hostile new, holds"))
      (is (= :later (first (:risk ((reprice/check c goal nil) {:status "partial" :steps (vec (take 12 straight-cells))} true))))
          "a prefix itself over the affordable holds")
      (let [check (reprice/check c goal nil)]
        (is (nil? (check (plan-of (take 10 straight-cells)) true)) "a found plan within the affordable")
        (is (nil? (check (plan-of straight-cells) false)) "is not priced again without a hostile new")))))

(deftest the-stay-a-risky-walk-is-weighed-against-is-priced-where-the-body-stands-at-its-health
  (let [c (ready-test/ctx-of {:time st/noon :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        goal {:x 30 :y 64 :z 0}
        plan {:status "partial" :steps (vec (take 10 straight-cells))}
        stays (atom [])]
    (with-redefs [risk/estimate prefix-estimate
                  risk/stay-estimate (fn [c _ _] (swap! stays conj (mapv js/Math.floor ((juxt :x :y :z) (sh-sense/feet (:primitives c))))) {:hp 0 :hostiles 0})]
      (risk/status c goal plan)
      (swap! (fake/state (:primitives c)) assoc-in [:self :pos] [10.5 64 0.5])
      (risk/status c goal plan)
      (swap! (fake/state (:primitives c)) assoc-in [:self :health] 6)
      (risk/status c goal plan)
      (is (= [[0 64 0] [10 64 0] [10 64 0]] @stays) "at the prefix end the stay is priced there, not at the start, and again once the body is hurt"))))

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
  ;; a skeleton death 75 s ago, 75 blocks off: about a third of a chance it has not despawned; a bed by the body
  (let [status-for (fn [food inventory] (let [c (-> (ready-test/ctx-of (assoc bed-world :self {:pos inside :health 20 :food food} :inventory inventory))
                                                    (with-death {:x 80 :y 64 :z 5} "skeleton" 75000 with-bed))]
                                          (in-lit-cave #(first (planned-status c far-goal)))))
        bread [{:name "bread" :count 3}]]
    (is (= :ok (status-for 20 [])))
    (is (= :later (status-for 5 bread)))
    (is (= :ok (status-for 5 [])) "nothing to eat: waiting never lowers the risk, so the walk goes")))

(deftest a-goal-worth-enough-lets-a-walk-take-a-risk-over-the-share-of-health
  (let [status-for (fn [worth] (let [c (-> (ready-test/ctx-of (assoc bed-world :self {:pos inside :health 20 :food 5} :inventory [{:name "bread" :count 3}]))
                                           (with-death {:x 80 :y 64 :z 5} "skeleton" 75000 with-bed)
                                           (assoc-in [:args :worth] worth))]
                                 (in-lit-cave #(first (planned-status c far-goal)))))]
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

(deftest a-death-or-hide-near-the-goal-is-priced-like-one-further-off
  (let [c (ready-test/ctx-of day-world)
        now 36000000
        with-hide (fn [pos] (assoc c :view (fn [] {:now now :data {:policies {:hid hide-spot/policy} :entries {:hid [{:t (- now 60000) :data {:pos pos :mobs ["zombie"]}}]}}})))]
    (is (pos? (:death-hp (estimate-unseen (with-death c {:x 93 :y 64 :z 2} "zombie" 60000) far-goal))))
    (is (pos? (:death-hp (estimate-unseen (with-death c {:x 88 :y 64 :z 5} "husk" 60000) far-goal))) "5 blocks from the goal")
    (is (pos? (:death-hp (risk/estimate (with-hide {:x 93 :y 64 :z 2}) far-goal))))
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
    (is (pos? (hp (assoc day-world :entities [{:id 50 :name "Other" :kind "player" :pos {:x 82 :y 64 :z 5}}]) "creeper" respawned-far))
        "another player stands by the spot: the nearest player never went far")
    (is (zero? (with-redefs [look/seen-block (constantly {:name "air" :age-ms 120000})]
                 (:hp (risk/estimate (with-death (ready-test/ctx-of day-world) {:x 80 :y 64 :z 5} "creeper" 60000 respawned-far) far-goal))))
        "nor any death it risks")))

(deftest a-walk-for-an-unseen-fresh-death-spot-risks-the-death-of-meeting-its-killer
  ;; a fist body with no bed; the death risk alone is over half its health
  (let [c (with-death (ready-test/ctx-of day-world) {:x 80 :y 64 :z 5} "skeleton" 10000)
        {:keys [lethal-hp]} (estimate-unseen c far-goal)]
    (is (> lethal-hp (price/allowed c)))
    (is (= :later (first (with-redefs [look/seen-block (constantly {:unknown true})] (planned-status c far-goal)))))))

(deftest a-drowned-death-spot-holds-while-the-body-stays-near-its-water-and-goes-once-it-went-far
  ;; 3 h old, past the 2 h fade of a walker's spot; 25 blocks from the body
  (let [at {:x 30 :y 64 :z 2}
        share (fn [more] (let [c (with-death (ready-test/ctx-of day-world) at "drowned" 10800000 more)]
                           (mapv #(death-spot/present-share c (assoc % :mob (:cause %))) (death-spot/recent c))))]
    (is (= [1] (share {})))
    (is (= [5] (mapv #(js/Math.round (* 1000 %)) (share {:respawned [{:t (- 36000000 60000) :data {:pos {:x 300 :y 64 :z 5}}}]})))
        "only a chicken jockey's rider stays")))

(defn dark-share
  "present-share of each death or near-death spot by a mob in dark-spot-world (simulation distance 10), at difficulty (1 easy, 2 normal), its records `entries`;
  over its weight with :unfaded."
  [difficulty entries & [unfaded]]
  (with-redefs [threat/simulation-distance (constantly 10)]
    (settings/with-settings {::threat/difficulty difficulty}
      #(let [c (assoc (ready-test/ctx-of dark-spot-world) :view (fn [] {:now 36000000 :data {:policies {:deaths core/deaths-policy} :entries entries}}))]
         (mapv (fn [d] (/ (death-spot/present-share c (assoc d :mob (:cause d))) (if unfaded (:weight d) 1))) (filter :mob? (death-spot/recent c)))))))

(defn died [cause age-ms] {:deaths [{:t (- 36000000 age-ms) :data {:pos {:x 80 :y 64 :z 5} :cause cause :death-t (- 36000000 age-ms)}}]})
(defn pct [x] (js/Math.round (* 1000 x)))
(def respawned-far {:respawned [{:t (- 36000000 50000) :data {:pos {:x 300 :y 64 :z 5}}}]})

(deftest a-dark-death-spot-holds-a-spawned-mob-at-the-measured-share-falling-with-its-fade-and-none-once-the-body-went-far
  ;; the body 75 blocks off, the spot all dark
  (let [[ten] (dark-share 1 (died "creeper" 600000))
        [hundred] (dark-share 1 (died "creeper" 6000000))
        since-back #(* (- 1 (/ %1 7200000)) (threat/spawned-there %2))
        later-fall {:t 35999000 :data {:pos {:x 300 :y 64 :z 5} :cause "fall" :death-t 35999000}}]
    (is (< 0 hundred ten 0.5) "a dark spot is no sure mob, and the older record says less")
    (is (= [(pct (since-back 60000 50))] (mapv pct (dark-share 1 (merge (died "creeper" 60000) respawned-far))))
        "the respawn 128 blocks off despawned the creeper and any spawned there: only the 50 s since the body came back count")
    (is (= [(pct (since-back 600000 1))] (mapv pct (dark-share 1 (update (died "creeper" 600000) :deaths conj later-fall)))) "a later death far off is where the body was")))

(deftest a-spot-that-may-spawn-holds-a-spawned-mob-at-the-measured-rate-of-any-light-and-a-seen-lit-one-none
  ;; the measured rate already covers spots of any light: a half-lit field is priced as a dark one
  (let [shares #(mapv pct (with-redefs [death-spot/lit? %] (dark-share 1 (merge (died "creeper" 600000) respawned-far))))
        dark (shares (constantly false))]
    (is (= [(pct (* (- 1 (/ 600000 7200000)) (threat/spawned-there 50)))] dark))
    (is (= dark (shares (fn [_ {:keys [x]}] (< x 80)))))
    (is (= [0] (shares (constantly true))))))

(defn died-with
  "died, the :died entry of the death holding items (names)."
  [cause age-ms items]
  (assoc (died cause age-ms) :died [{:t (- 36000000 age-ms) :data {:pos {:x 80 :y 64 :z 5} :inventory (mapv #(hash-map :name %) items)}}]))

(deftest only-a-loot-picker-above-easy-that-could-pick-up-a-drop-it-equips-outlives-the-body-going-far
  (let [spawned (threat/spawned-there 50)]
    (are [difficulty entries kept] (= (pct (- 1 (* (- 1 kept) (- 1 spawned))))
                                      (pct (first (dark-share difficulty (merge entries respawned-far) :unfaded))))
      2 (died "zombie" 60000) 0.5522
      2 (died-with "zombie" 60000 ["dirt"]) 0.5522
      2 (died-with "skeleton" 60000 ["dirt" "oak_planks"]) 0.0099
      2 (died-with "skeleton" 60000 ["iron_boots"]) 0.5544
      1 (died "zombie" 60000) 0.0049
      2 (died "creeper" 60000) 0
      2 {:hurt [{:t (- 36000000 60000) :data {:health 3 :pos {:x 80 :y 64 :z 5} :attacker {:name "zombie"}}}]} 0.0049)))

(deftest a-death-spot-counts-as-lit-only-when-seen
  (let [p (:primitives (ready-test/ctx-of day-world))
        at {:x 80 :y 64 :z 5}
        lit-with (fn [seen] (with-redefs [look/seen-block (constantly seen)] (death-spot/lit? p at)))]
    (is (true? (lit-with {:name "air" :age-ms 1000})))
    (is (false? (lit-with {:unknown true})) "the light data of a cell never seen is no sight of it")))

(deftest a-walk-over-the-allowance-goes-when-staying-costs-more
  (let [zombies (for [i (range 3)] {:id (+ 7 i) :name "zombie" :kind "hostile" :pos {:x (- 2 i) :y 64 :z 3}})
        status-for (fn [entities]
                     (first (planned-status (-> (ready-test/ctx-of {:time st/noon :light {[80 64 5] [0 0]} :self {:pos inside :health 2.3 :food 5} :entities (vec entities)})
                                             (with-death {:x 80 :y 64 :z 5} "zombie" 20000))
                                         far-goal)))]
    (is (= :ok (status-for zombies)) "hostiles reach the body where it stands, the walk leaves them behind: the walk is the cheaper risk")
    (is (= :later (status-for [])) "nothing near the body: waiting costs nothing, so it waits")))

(deftest a-body-that-cannot-heal-and-is-no-safer-staying-ends-stranded-instead-of-waiting
  ;; on open ground: a zombie by the way 30 blocks off, whose field reaches the body's cell, and three 60 blocks off, whose fields do not; a notice rate at which the zombies get to the body on the walk as surely as at its cell; lava by the goal
  (let [zombies (for [[i x] (map-indexed vector [30 60 61 62])] {:id (+ 7 i) :name "zombie" :kind "hostile" :pos {:x x :y 64 :z 3}})
        c (ready-test/ctx-of (assoc ready-test/open-night :self {:pos {:x 0 :y 64 :z 0} :health 2.3 :food 5} :entities (vec zombies) :blocks {"94,64,2" "lava"}))
        s (settings/with-settings {:jobs.lib.cost.death/notice-rate {:other {:k 36 :n-s 100} :ranged {:k 36 :n-s 100}}} #(planned-status c far-goal))]
    (is (= :never (first s)))
    (is (= :stranded (:reason (nth s 2))))))

(deftest a-body-that-cannot-heal-among-zombies-at-the-measured-notice-rate-walks-when-the-walk-is-the-cheaper-risk
  ;; the stranded case's open night at the shipped rate, the zombies behind the body: staying meets them, the walk leaves them
  (let [zombies (for [[i x] (map-indexed vector [-4 -5 -6 -7])] {:id (+ 7 i) :name "zombie" :kind "hostile" :pos {:x x :y 64 :z 3}})
        c (ready-test/ctx-of (assoc ready-test/open-night :self {:pos {:x 0 :y 64 :z 0} :health 2.3 :food 5} :entities (vec zombies)))
        s (planned-status c far-goal)]
    (is (= :ok (first s)))
    (is (= :cheaper-than-staying (:reason (nth s 2))))))

(deftest a-healing-body-on-dark-open-ground-goes-on-when-waiting-there-costs-as-much
  (let [c (ready-test/ctx-of (assoc ready-test/open-night :self {:pos {:x 0 :y 64 :z 0} :health 5 :food 15} :inventory [{:name "bread" :count 3}]))
        goal {:x 40 :y 64 :z 0}]
    (is (> (:hp (risk/estimate c goal)) (price/allowed c)) "over the half-of-health allowance")
    (is (= :ok (first (planned-status c goal))))))

(deftest a-hostile-near-the-far-end-of-a-way-does-not-make-staying-dear-enough-to-walk-past-lava
  (let [zombie {:id 9 :name "zombie" :kind "hostile" :pos {:x 20 :y 64 :z 40}}
        c (ready-test/ctx-of (assoc day-world :self {:pos inside :health 3 :food 5} :entities [zombie] :blocks {"22,64,20" "lava"}))]
    (is (> (:lava-hp (risk/estimate c {:x 20 :y 64 :z 20})) (price/allowed c)))
    (is (= :later (first (planned-status c {:x 20 :y 64 :z 20}))))))


(deftest a-walk-round-a-never-seen-stone-cell-holding-a-fresh-skeleton-spot-holds
  ;; the cell's inside and its far faces unseen: a way out for the skeleton (Q16); the goal a table 6.5 blocks past its spot, the way a corridor along the cell
  (let [cx 21
        shell (into {} (for [x (range (dec cx) (+ cx 2)) y (range 63 66) z (range 15 18) :when (not= [x y z] [cx 64 16])] [(str x "," y "," z) "stone"]))
        corridor (into {} (for [x (range 12 31) y (range 63 68) z [13 19]] [(str x "," y "," z) "stone"]))
        c (-> (ready-test/ctx-of {:time st/noon :light {[cx 64 16] [0 0]} :blocks (merge shell corridor) :self {:pos {:x 13.5 :y 64 :z 18.5} :health 20 :food 20}})
              (with-death {:x cx :y 64 :z 16} "skeleton" 10000))
        real u/seen-block
        hidden? (fn [{:keys [x y z]}] (and (<= cx x (inc cx)) (<= 63 y 65) (<= 15 z 17)))
        s (with-redefs [u/seen-block (fn [p pos] (when-not (hidden? pos) (real p pos))) look/seen-block (constantly {:unknown true})]
            (planned-status c {:x 27.5 :y 64 :z 18.5}))]
    (is (= :later (first s)))
    (is (= :risky (:reason (nth s 2))))))

