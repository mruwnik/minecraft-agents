(ns engine.attack-test
  "jobs.combat.attack and its helpers against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.hunt-test :as hunt-test]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.fake.raw-world :as fake-raw]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [engine.events :as events]
            [engine.triggers :as triggers]
            [engine.test-util :as tu]
            [jobs.combat.attack :as attack]))

(defn spec [args] (list 'jobs.combat.attack args))

(defn ent
  ([id name kind x] (ent id name kind x {}))
  ([id name kind x more] (merge {:id id :name name :kind kind :pos {:x x :y 64 :z 0}} more)))

(defn zed [id x] (ent id "zombie" "hostile" x))

(defn ^:async run-ticks
  "Tick n times, the clock moving step ms before each."
  [{:keys [eng clock]} n step]
  (await (tu/run-ticks eng clock n step)))

(defn ^:async scenario-seeing
  "As scenario, over a body that sees through perception."
  [args world n]
  (let [s (h/setup-seeing world nil 20)]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n 700))
    s))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world 20)]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n 700))
    s))

(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))

(defn done-event
  "The fight's ending event (job.completed or job.stopped), which carries the data."
  [{:keys [seen]}]
  (first (filter #(#{:completed :stopped} (:kind %)) @seen)))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))

(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))

;; the target may be the danger: only the dark sky holds an attack
(deftest an-attack-on-a-zombie-in-reach-is-not-held-by-the-threat-and-waits-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [day (await (scenario {:targets [1]} {:inventory h/sword :entities [(zed 1 2)]} 6))
              night (await (scenario {:targets [1]} {:inventory h/sword :time st/night :entities [(zed 1 2)]} 6))]
          (is (seq (attacked day)) "a zombie next to the body is attacked by day")
          (is (empty? (attacked night)))
          (is (= [:dark] (distinct (map :reason (events-of night :waiting))))))))))

;; ------------------------------------------------------------ pure helpers

(deftest a-mob-type-target-is-found-past-more-than-64-nearer-entities
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sheep (mapv #(ent (+ 100 %) "sheep" "passive" 2 {:pos {:x 2 :y 64 :z (- (/ % 10) 3.5)}}) (range 70))
              s (await (scenario {:targets ["cow"]} {:inventory h/sword :entities (conj sheep (ent 500 "cow" "passive" 9))} 4))]
          (is (some #{500} (attacked s))))))))

(deftest an-id-target-is-found-past-more-than-64-nearer-entities
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sheep (mapv #(ent (+ 100 %) "sheep" "passive" 2 {:pos {:x 2 :y 64 :z (- (/ % 10) 3.5)}}) (range 70))
              s (await (scenario {:targets [500]} {:inventory h/sword :entities (conj sheep (ent 500 "cow" "passive" 9))} 4))]
          (is (some #{500} (attacked s))))))))

(deftest target-list-normalises-to-a-vector
  (doseq [[in out] [[nil []] [[] []] [7 [7]] ["zombie" ["zombie"]] [[7 "Alex"] [7 "Alex"]] ['(7 8) [7 8]]]]
    (is (= out (attack/target-list in)) (str in))))

(defn js-ent [kind name & [username id]]
  #js {:id (or id 1) :kind kind :name name :username username})

(deftest matches-by-id-username-and-mob-type
  (doseq [[targets e killed expected note]
          [[[7] (js-ent "hostile" "zombie" nil 7) #{} true "an id"]
           [[7] (js-ent "hostile" "zombie" nil 8) #{} false "another id"]
           [["zombie"] (js-ent "hostile" "zombie") #{} true "a mob type"]
           [["zombie"] (js-ent "passive" "cow") #{} false "another type"]
           [["Alex"] (js-ent "player" "Alex" "Alex") #{} true "a username"]
           [["Alex"] (js-ent "player" "Alex" "Alex") #{"Alex"} false "a killed player"]
           [["player"] (js-ent "player" "Alex" "Alex") #{} false "players never match by type"]
           [["item"] (js-ent "item" "item") #{} false "items never match"]
           [[3] (js-ent "item" "item" nil 3) #{} false "items never match, even by id"]
           [[3] (js-ent "player" "Fake" "Fake" 3) #{} false "never self by id"]
           [["Fake"] (js-ent "player" "Fake" "Fake") #{} false "never self by name"]]]
    (is (= expected (attack/matches? targets "Fake" killed e)) note)))

;; ------------------------------------------------------------------ the job

(deftest kills-an-id-then-clears
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 3)]} 1))]
          (is (= [7 7 7 7] (attacked s)) "four hits at 5 damage")
          (is (finished? s) "one call: the kill ends it")
          (is (= :cleared (:reason (done-event s))))
          (is (= [7] (:killed (done-event s)))))))))

(deftest kills-every-id-of-a-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7 8]} {:inventory h/sword :entities [(zed 7 3) (zed 8 2)]} 8))]
          (is (= (set [7 8]) (set (attacked s))))
          (is (= 8 (count (attacked s)))))))))

(deftest kills-a-player-by-username
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (scenario {:targets ["Alex"]} {:inventory h/sword :entities [(ent 9 "Alex" "player" 2)]} 4))]
          (is (= [9 9 9 9] (attacked s)))
          (is (empty? (fake/entities p)) "the player is dead"))))))

(deftest a-mob-type-takes-every-mob-of-it-and-no-cows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (scenario {:targets ["zombie"]}
                                                 {:inventory h/sword :entities [(zed 7 3) (zed 8 2) (ent 9 "cow" "passive" 1)]} 9))]
          (is (= (set [7 8]) (set (attacked s))))
          (is (= [9] (mapv :id (fake/entities p))) "the cow lives"))))))

(deftest a-mixed-list-takes-nearest-first-and-moves-on-when-one-dies
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [5 "zombie" "Alex"]}
                                 {:inventory h/sword
                                  :entities [(ent 9 "Alex" "player" 3) (zed 7 2) (ent 5 "cow" "passive" 1)]} 12))]
          (is (= (concat (repeat 4 5) (repeat 4 7) (repeat 4 9)) (attacked s))))))))

(deftest nearest-target-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets ["zombie"]} {:inventory h/sword :entities [(zed 7 3) (zed 8 1)]} 8))]
          (is (= (concat (repeat 4 8) (repeat 4 7)) (attacked s))))))))

(deftest no-target-in-radius-fails-the-check
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (h/setup {:inventory h/sword :entities [(zed 7 3) (zed 8 30)]})]
          (core/submit! eng (spec {:targets [8 "skeleton"] :absent :wait}) {})
          (is (nil? (core/tick! eng)))
          (is (zero? (count (h/calls p "attack"))))
          (is (zero? (count (h/calls p "equip")))))))))

(deftest never-attacks-itself-nor-matches-players-by-type
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [3 "Fake" "zombie"]}
                                 {:inventory h/sword :entities [(ent 3 "Fake" "player" 1) (zed 7 2)]} 4))]
          (is (= [7 7 7 7] (attacked s))))
        (doseq [targets [["player"] ["hostile"] [3 "Fake"]]]
          (let [{:keys [eng]} (h/setup {:entities [(ent 3 "Fake" "player" 1) (ent 9 "Alex" "player" 2)]})]
            (core/submit! eng (spec {:targets targets :absent :wait}) {})
            (is (nil? (core/tick! eng)) (str targets))))))))

(deftest gives-up-on-an-unreachable-target
  ;; 5 ticks: three blocked-walk rounds are fruitless and back off 1 s (ticks 4 is skipped), so the finishing round is tick 5.
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 10)] :unreachable ["10,64,0"]} 5))]
          (is (zero? (count (attacked s))))
          (is (= [] (h/calls (:p s) "moveTo")) "walks with the engine walker, not the raw pathfinder")
          (is (= "attack gave-up: killed 0, gave up on zombie 7: unreachable" (:text (done-event s))))
          (is (finished? s))
          (is (= :gave-up (:reason (done-event s))))
          (is (= {7 :unreachable} (:given-up (done-event s)))))))))

(defn wall-of [block xs] (into {} (for [x xs y [64 65 66]] [(str x "," y ",0") block])))

(deftest a-target-behind-glass-is-never-swung-at-and-is-given-up-as-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 3)] :blocks (wall-of "glass" [1 2]) :unreachable ["3,64,0"]} 8))]
          ;; the fake teleports a walker, so the walk to the walled-off cell is answered blocked, as a real one would be
          (is (zero? (count (attacked s))))
          (is (finished? s))
          (is (= :gave-up (:reason (done-event s)))))))))

(deftest a-target-the-line-passes-over-a-fence-is-swung-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 3)] :blocks {"1,64,0" "oak_fence"}} 2))]
          (is (= [7] (distinct (attacked s))))
          (is (empty? (:given-up (done-event s)))))))))

(deftest a-target-in-the-open-is-swung-at-and-a-missing-hittable-swings-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 3)]} 2))]
          (is (= [7] (distinct (attacked s)))))))))

(deftest gives-up-on-a-target-that-takes-no-damage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]}
                                 {:inventory h/sword :entities [(ent 7 "zombie" "hostile" 2 {:invulnerable true})]} 5))]
          (is (= 4 (count (attacked s))))
          (is (= {7 :no-damage} (:given-up (done-event s))))
          (is (finished? s))
          (is (= :gave-up (:reason (done-event s)))))))))

(deftest a-vehicle-that-shows-no-damage-is-hit-until-the-max-hits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7] :max-hits 6}
                                 {:inventory h/sword :entities [(ent 7 "chest_minecart" "vehicle" 2 {:invulnerable true})]} 8))]
          (is (= 6 (count (attacked s))))
          (is (= {7 :too-many-hits} (:given-up (done-event s)))))))))

(deftest gives-up-after-max-hits-without-a-kill
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7] :max-hits 3}
                                 {:inventory h/sword :entities [(ent 7 "zombie" "hostile" 2 {:health 1000})]} 4))]
          (is (= 3 (count (attacked s))))
          (is (= {7 :too-many-hits} (:given-up (done-event s))))
          (is (= :gave-up (:reason (done-event s)))))))))

(deftest the-engines-limit-ends-a-fight-past-its-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7] :spend {:job-time 3} :no-damage-hits 100 :max-hits 1000}
                                 {:inventory h/sword :entities [(ent 7 "zombie" "hostile" 2 {:invulnerable true})]} 7))]
          (is (finished? s))
          (is (= [:job-time] (mapv :reason (events-of s :stopped))) "the engine's limit ends it")
          (is (= [] (:killed (first (events-of s :stopped)))) "with done?'s data")
          (is (= [] (events-of s :attack.timeout))))))))

(deftest equips-the-weapon-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 2)]} 3))]
          (is (= 4 (count (attacked s))))
          (is (= 1 (count (h/calls p "equip")))))))))

(deftest a-killed-player-respawning-under-a-new-id-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p clock eng] :as s} (await (scenario {:targets ["Alex"]}
                                                           {:inventory h/sword :entities [(ent 9 "Alex" "player" 2)]} 4))]
          (fake/add-entity! p {:id 20 :name "Alex" :username "Alex" :kind "player" :health 20 :pos [2 64 0]})
          (await (run-ticks s 2 700))
          (is (= [9 9 9 9] (attacked s)))
          (swap! clock + 6000)
          (await (core/tick! eng))
          (is (= :cleared (:reason (done-event s))))
          (is (= [9] (:killed (done-event s)))))))))

(deftest at-most-one-swing-per-gap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (h/first-round-ms (spec {:targets [7]}) {:inventory h/sword :entities [(zed 7 2)]} 10))]
          (is (= 4 (count (attacked s))))
          (is (every? #(<= 585 % 625) (h/waited-between-swings p)) "the rest of the sword's 625 ms is waited out between swings"))))))

(deftest an-explicit-attack-gap-ms-beats-the-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (h/first-round-ms (spec {:targets [7] :attack-gap-ms 100}) {:inventory h/sword :entities [(zed 7 2)]} 10))]
          (is (= 4 (count (attacked s))))
          (is (every? #(<= 80 % 100) (h/waited-between-swings p)) "100 ms, not the sword's 625 nor the 500 floor"))))))

(deftest a-target-that-leaves-unkilled-is-looked-for-then-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (h/setup {:inventory h/sword :entities [(zed 7 2)]} 20)]
          (.override (.-world p) "attack" (fn [_ _ _] (swap! (fake/state p) assoc :entities []) (js/Promise.resolve #js {:status "hit" :hurt true})))
          (core/submit! (:eng s) (spec {:targets [7]}) {})
          (await (core/tick! (:eng s)))
          (is (finished? s) "nothing left to attack: the call looks round and ends")
          (is (= :lost (:reason (done-event s)))))))))

(deftest a-target-that-vanishes-after-a-hit-is-not-booked-as-killed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (h/setup {:inventory h/sword :entities [(zed 7 2)]} 20)]
          (.override (.-world p) "attack" (fn [_ _ _] (swap! (fake/state p) assoc :entities []) (js/Promise.resolve #js {:status "hit" :hurt true})))
          (core/submit! (:eng s) (spec {:targets [7]}) {})
          (await (core/tick! (:eng s)))
          (is (finished? s))
          (is (= :lost (:reason (done-event s))))
          (is (= [] (:killed (done-event s)))))))))

(deftest a-player-is-named-by-username-when-given-up-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]}
                                 {:inventory h/sword
                                  :entities [(ent 7 "player" "player" 2 {:username "Alex" :invulnerable true})]} 5))
              gave-up (done-event s)]
          (is (= {7 "Alex"} (:names gave-up)))
          (is (re-find #"Alex" (:text gave-up)))
          (is (not (re-find #"player" (:text gave-up)))))))))

(deftest a-killed-entity-that-stays-listed-is-not-attacked-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [targets [[9] ["Alex"]]]
          (let [{:keys [p clock eng] :as s} (h/setup {:inventory h/sword :entities [(ent 9 "Alex" "player" 2)]} 20)]
            (.override (.-world p) "attack"
                       (fn [_token _args _impl] (js/Promise.resolve #js {:status "killed" :health 0 :hurt true})))
            (core/submit! eng (spec {:targets targets}) {})
            (await (core/tick! eng))
            (is (= [9] (attacked s)) (str targets " one swing, then left alone"))
            (is (finished? s) (str targets))
            (is (= :cleared (:reason (done-event s))) (str targets))
            (is (= [9] (:killed (done-event s))) (str targets))))))))

(deftest an-absent-target-ends-the-job-absent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [targets [[123] ["skeleton"] ["Fake"]]]
          (let [{:keys [p] :as s} (await (h/first-round-ms (spec {:targets targets})
                                                           {:inventory h/sword :entities [(ent 3 "Fake" "player" 1) (zed 7 2)]} 20))]
            (is (finished? s) (str targets))
            (is (= :absent (:reason (done-event s))) (str targets))
            (is (empty? (:given-up (done-event s))) (str targets))
            (is (every? #(zero? (count (h/calls p %))) ["equip" "moveTo" "attack"]) (str targets))
            (is (pos? (count (h/calls p "look"))) (str targets " turned before ending absent"))))))))

(deftest a-target-heard-but-not-seen-is-stepped-toward-before-the-job-ends-absent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities [{:id 5 :name "cow" :kind "passive" :pos {:x 0 :y 64 :z -8}}]} 20)
              _ (hunt-test/hear-passives-until! (:p s) #(<= (:z %) -2))
              _ (core/submit! (:eng s) (spec {:targets [5]}) {})
              _ (await (run-ticks s 60 700))]
          (is (= [5] (take 1 (attacked s))) "attacked after stepping north to see it")
          (is (not= :absent (:reason (done-event s)))))))))

(deftest a-target-neither-seen-nor-heard-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities []} 20)
              _ (hunt-test/hear-passives-until! (:p s) (constantly false))
              _ (core/submit! (:eng s) (spec {:targets [5]}) {})
              _ (await (run-ticks s 60 700))]
          (is (= :absent (:reason (done-event s))))
          (is (zero? (count (h/calls (:p s) "steer")))))))))

(deftest absent-wait-declines-until-a-target-turns-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock] :as s} (h/setup {:inventory h/sword} 20)]
          (core/submit! eng (spec {:targets ["zombie"] :absent :wait}) {})
          (is (nil? (core/tick! eng)))
          (fake/add-entity! p {:id 7 :name "zombie" :kind "hostile" :health 20 :pos [2 64 0]})
          (swap! clock + 700)
          (await (core/tick! eng))
          (is (= [7] (distinct (attacked s)))))))))

(deftest lost-when-a-target-is-neither-killed-nor-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (h/setup {:inventory h/sword :entities [(zed 7 2) (zed 8 3)]} 20)]
          (.override (.-world p) "attack" (fn [t a impl]
                                            (swap! (fake/state p) assoc-in [:entities 1 :pos] [0 64 40])
                                            (impl t a)))
          (core/submit! (:eng s) (spec {:targets [7 8]}) {})
          (await (core/tick! (:eng s)))
          (is (= :lost (:reason (done-event s))))
          (is (= [7] (:killed (done-event s)))))))))

(deftest a-given-up-target-that-leaves-ends-gave-up-not-cleared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (h/setup {:inventory h/sword :entities [(ent 7 "zombie" "hostile" 2 {:invulnerable true})]} 20)
              n (atom 0)]
          (.override (.-world p) "attack" (fn [t a impl]
                                            (.then (impl t a) (fn [r]
                                                                (when (= 4 (swap! n inc)) (swap! (fake/state p) assoc-in [:entities 0 :pos] [0 64 40]))
                                                                r))))
          (core/submit! (:eng s) (spec {:targets [7]}) {})
          (await (core/tick! (:eng s)))
          (is (= :gave-up (:reason (done-event s))))
          (is (= {7 :no-damage} (:given-up (done-event s)))))))))

(defn ^:async scripted-attack
  "Submit an attack on id 7 (in reach) whose attack results follow statuses in order, then repeat the last; n ticks."
  [statuses n]
  (let [{:keys [p eng] :as s} (h/setup {:inventory h/sword :entities [(zed 7 2)]} 20)
        i (atom 0)]
    (.override (.-world p) "attack"
               (fn [_token _args _impl]
                 (let [st (nth statuses (min @i (dec (count statuses))))]
                   (swap! i inc)
                   (js/Promise.resolve #js {:status st :hurt true}))))
    (core/submit! eng (spec {:targets [7] :spend {:job-time 1000}}) {})
    (await (run-ticks s n 700))
    s))

(deftest a-landed-hit-resets-the-out-of-reach-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scripted-attack (conj (vec (take 12 (cycle ["out-of-reach" "hit"]))) "killed") 1))]
          (is (>= (count (attacked s)) 12))
          (is (empty? (:given-up (done-event s)))))))))

(deftest three-out-of-reach-swings-in-a-row-still-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scripted-attack ["out-of-reach"] 10))]
          (is (= {7 :unreachable} (:given-up (done-event s)))))))))

(deftest out-of-reach-right-after-an-arrived-walk-is-the-target-moving-not-a-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup {:inventory h/sword :entities [(zed 7 10)]} 20)
              i (atom 0)]
          ;; the target runs to the other side after each swing: every walk arrives, and the swing finds it gone on
          (.override (.-world p) "attack" (fn [_ _ _]
                                            (let [n (swap! i inc)]
                                              (swap! (fake/state p) assoc-in [:entities 0 :pos] [(if (even? n) 10 -4) 64 0])
                                              (js/Promise.resolve #js {:status (if (< n 8) "out-of-reach" "killed")}))))
          (core/submit! eng (spec {:targets [7] :spend {:job-time 1000}}) {})
          (await (core/tick! eng))
          (is (>= (count (attacked s)) 5) "swung well past three times")
          (is (empty? (:given-up (done-event s)))))))))

;; ------------------------------------------------------- looking round between swings (card 9970c377)

(defn watched [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :watched))

(deftest a-fight-with-a-hostile-looks-round-while-the-cooldown-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-seeing {:targets [7] :attack-gap-ms 3000} {:inventory h/sword :entities [(zed 7 3)]} 3))]
          (is (seq (watched s)) "a hostile known near: alert, so the cooldown is spent looking round")
          (is (= [7] (distinct (attacked s)))))))))

(deftest a-hunt-of-a-cow-in-a-lit-place-never-looks-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-seeing {:targets ["cow"] :attack-gap-ms 3000}
                                 {:inventory h/sword :entities [(ent 8 "cow" "passive" 3)]} 3))]
          (is (empty? (watched s)))
          (is (= [8] (distinct (attacked s)))))))))

;; ------------------------------------------------------- a far target out of the view cone (card 62a8df01)

(defn ^:async scenario-far-sight
  "As scenario-seeing, over a perception that sees 48 blocks (a far target is known only once it is in the view cone)."
  [args world n]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor (merge {:floor h/flight-floor} world))
        now (tu/act-clock clock raw 20)
        p (perception/wrap raw (perception/create (fake-raw/create raw) {:radius 48 :ray-deg 2 :now now}))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})
        s {:eng eng :p p :seen seen :clock clock}]
    (core/submit! eng (spec args) {})
    (await (run-ticks s n 700))
    s))

(deftest a-target-far-behind-the-view-cone-is-looked-for-before-the-job-ends-absent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario-far-sight {:targets ["zombie"] :radius 48} {:inventory h/sword :entities [(zed 7 30)]} 12))]
          (is (not= :absent (:reason (done-event s))))
          (is (seq (attacked s))))))))

;; ------------------------------------------------------- one fight per call (card 96dccacc)

(deftest one-call-fights-to-the-end-with-swings-a-gap-apart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (await (h/first-round-ms (spec {:targets [7]}) {:inventory h/sword :entities [(zed 7 3)]} 20))
              _ (is (= [] (:list (core/state eng))) "the first call killed it")
              calls (vec (.-calls (.-world p)))
              idx (keep-indexed (fn [i c] (when (= "attack" (.-name c)) i)) calls)]
          (is (= [7 7 7 7] (attacked s)))
          (is (= :cleared (:reason (done-event s))))
          (is (every? true? (map #(boolean (some (fn [c] (= "wait" (.-name c))) (subvec calls %1 %2))) idx (rest idx)))
              "between two swings the call waits out the gap"))))))

(deftest the-walks-to-a-target-are-short-legs-that-never-escalate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls* (atom [])
              s (await (tu/with-near-spy calls* (fn ^:async b [] (await (scenario {:targets [7]} {:floor tu/walk-floor :inventory h/sword :entities [(zed 7 10)]} 3)))))
              opts (map #(nth % 2) @calls*)]
          (is (seq opts))
          (is (every? #(and (= #{:walk :clear-seal} (:ways %)) (= 5 (get-in % [:spend :job-time])) (false? (:look-round %)) (false? (:dangers %)) (= :shut (:doors %))) opts)
              "a moving target is chased in legs, never dug or pillared toward"))))))

