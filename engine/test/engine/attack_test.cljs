(ns engine.attack-test
  "jobs.combat.attack and its helpers against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.jobs.combat :as combat]
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
  (dotimes [_ n]
    (swap! clock + step)
    (await (core/tick! eng))))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world)]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n 700))
    s))

(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))

(defn done-event [{:keys [seen]}] (first (filter #(= :attack.done (:kind %)) @seen)))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))

(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))

;; ------------------------------------------------------------ pure helpers

(deftest attack-gap-ms-is-the-held-weapons-cooldown
  (doseq [[item ms] [["wooden_sword" 625] ["diamond_sword" 625] ["netherite_sword" 625]
                     ["wooden_axe" 1250] ["stone_axe" 1250] ["iron_axe" 1112]
                     ["golden_axe" 1000] ["diamond_axe" 1000] ["netherite_axe" 1000]
                     ["iron_pickaxe" 500] ["stick" 500] [nil 500]]]
    (is (= ms (combat/attack-gap-ms item)) (str item))))

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

(deftest kills-an-id-then-clears-after-lost-s
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 3)]} 4))]
          (is (= [7 7 7 7] (attacked s)) "four hits at 5 damage")
          (is (= ["j1"] (:list (core/state eng))) "still running until nothing was seen for :lost-s")
          (swap! clock + 6000)
          (await (core/tick! eng))
          (is (finished? s))
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
          (is (empty? (.-entities (.-state (.-world p)))) "the player is dead"))))))

(deftest a-mob-type-takes-every-mob-of-it-and-no-cows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (scenario {:targets ["zombie"]}
                                                 {:inventory h/sword :entities [(zed 7 3) (zed 8 2) (ent 9 "cow" "passive" 1)]} 9))]
          (is (= (set [7 8]) (set (attacked s))))
          (is (= [9] (mapv #(.-id %) (.-entities (.-state (.-world p))))) "the cow lives"))))))

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
        (let [s (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 10)] :unreachable ["10,64,0"]} 5))
              gave-up (events-of s :attack.gave-up)]
          (is (zero? (count (attacked s))))
          (is (= 3 (count (h/calls (:p s) "moveTo"))))
          (is (= [[7 :unreachable]] (mapv (juxt :target :reason) gave-up)))
          (is (nil? (:level (first gave-up))))
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
          (is (= [[7 :unreachable]] (mapv (juxt :target :reason) (events-of s :attack.gave-up))))
          (is (finished? s))
          (is (= :gave-up (:reason (done-event s)))))))))

(deftest a-target-the-line-passes-over-a-fence-is-swung-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 3)] :blocks {"1,64,0" "oak_fence"}} 2))]
          (is (= [7 7] (attacked s)))
          (is (empty? (events-of s :attack.gave-up))))))))

(deftest a-target-in-the-open-is-swung-at-and-a-missing-hittable-swings-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 3)]} 2))]
          (is (= [7 7] (attacked s))))))))

(deftest gives-up-on-a-target-that-takes-no-damage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7]}
                                 {:inventory h/sword :entities [(ent 7 "zombie" "hostile" 2 {:invulnerable true})]} 5))]
          (is (= 4 (count (attacked s))))
          (is (= [[7 :no-damage]] (mapv (juxt :target :reason) (events-of s :attack.gave-up))))
          (is (finished? s))
          (is (= :gave-up (:reason (done-event s)))))))))

(deftest gives-up-after-max-hits-without-a-kill
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7] :max-hits 3}
                                 {:inventory h/sword :entities [(ent 7 "zombie" "hostile" 2 {:health 1000})]} 4))]
          (is (= 3 (count (attacked s))))
          (is (= [[7 :too-many-hits]] (mapv (juxt :target :reason) (events-of s :attack.gave-up))))
          (is (= :gave-up (:reason (done-event s)))))))))

(deftest times-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:targets [7] :timeout-s 3 :no-damage-hits 100 :max-hits 1000}
                                 {:inventory h/sword :entities [(ent 7 "zombie" "hostile" 2 {:invulnerable true})]} 7))]
          (is (finished? s))
          (is (= :timeout (:reason (done-event s))))
          (is (= 1 (count (events-of s :attack.timeout)))))))))

(deftest equips-the-weapon-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 2)]} 3))]
          (is (= 3 (count (attacked s))))
          (is (= 1 (count (h/calls p "equip")))))))))

(deftest a-killed-player-respawning-under-a-new-id-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p clock eng] :as s} (await (scenario {:targets ["Alex"]}
                                                           {:inventory h/sword :entities [(ent 9 "Alex" "player" 2)]} 4))]
          (.push (.-entities (.-state (.-world p)))
                 #js {:id 20 :name "Alex" :username "Alex" :kind "player" :health 20 :pos (tu/pos 2 64 0)})
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
        (let [{:keys [eng clock] :as s} (await (h/first-round (spec {:targets [7]}) {:inventory h/sword :entities [(zed 7 2)]}))]
          (is (= 1 (count (attacked s))))
          (await (core/tick! eng))
          (swap! clock + 100)
          (await (core/tick! eng))
          (is (= 1 (count (attacked s))) "inside the sword's 625 ms")
          (swap! clock + 600)
          (await (core/tick! eng))
          (is (= 2 (count (attacked s)))))))))

(deftest an-explicit-attack-gap-ms-beats-the-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (await (h/first-round (spec {:targets [7] :attack-gap-ms 100}) {:inventory h/sword :entities [(zed 7 2)]}))]
          (is (= 1 (count (attacked s))))
          (swap! clock + 100)
          (await (core/tick! eng))
          (is (= 2 (count (attacked s))) "100 ms later, not the sword's 625 nor the 500 floor"))))))

(deftest a-started-job-stays-until-lost-s-after-its-target-leaves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock p] :as s} (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 2)]} 1))]
          (set! (.. p -world -state -entities) #js [])
          (await (run-ticks s 2 700))
          (is (= ["j1"] (:list (core/state eng))) "nothing to attack, still started")
          (swap! clock + 6000)
          (await (core/tick! eng))
          (is (finished? s))
          (is (= :lost (:reason (done-event s)))))))))

(deftest a-target-that-vanishes-after-a-hit-is-not-booked-as-killed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p clock eng] :as s} (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 2)]} 1))]
          (set! (.. p -world -state -entities) #js [])
          (swap! clock + 700)
          (await (core/tick! eng))
          (swap! clock + 6000)
          (await (core/tick! eng))
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
              gave-up (first (events-of s :attack.gave-up))]
          (is (= "Alex" (:name gave-up)))
          (is (re-find #"Alex" (:text gave-up)))
          (is (not (re-find #"player" (:text gave-up)))))))))

(deftest a-killed-entity-that-stays-listed-is-not-attacked-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [targets [[9] ["Alex"]]]
          (let [{:keys [p clock eng] :as s} (h/setup {:inventory h/sword :entities [(ent 9 "Alex" "player" 2)]})]
            (.override (.-world p) "attack"
                       (fn [_token _args _impl] (js/Promise.resolve #js {:status "killed" :health 0 :hurt true})))
            (core/submit! eng (spec {:targets targets}) {})
            (await (run-ticks s 3 700))
            (is (= [9] (attacked s)) (str targets " one swing, then left alone"))
            (swap! clock + 6000)
            (await (core/tick! eng))
            (is (finished? s) (str targets))
            (is (= :cleared (:reason (done-event s))) (str targets))
            (is (= [9] (:killed (done-event s))) (str targets))))))))

(deftest waits-in-one-second-steps-while-nothing-is-in-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p clock eng seen] :as s} (await (scenario {:targets [7]} {:inventory h/sword :entities [(zed 7 2)]} 1))]
          (set! (.. p -world -state -entities) #js [])
          (await (run-ticks s 12 250))
          (is (pos? (count (h/calls p "wait"))))
          (is (= [1000] (distinct (mapv #(.. % -args -ms) (h/calls p "wait")))))
          (is (not-any? #(= :stalled (:kind %)) @seen))
          (is (not (finished? s)))
          (swap! clock + 6000)
          (await (core/tick! eng))
          (is (= :lost (:reason (done-event s)))))))))

(deftest an-absent-target-ends-the-job-after-the-grace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [targets [[123] ["skeleton"] ["Fake"]]]
          (let [{:keys [p] :as s} (await (h/first-round (spec {:targets targets})
                                                        {:inventory h/sword :entities [(ent 3 "Fake" "player" 1) (zed 7 2)]}))]
            (await (run-ticks s 1 1000))
            (await (run-ticks s 1 900))
            (is (not (finished? s)) (str targets " still inside the 2 s grace"))
            (await (run-ticks s 1 200))
            (is (finished? s) (str targets))
            (is (= :absent (:reason (done-event s))) (str targets))
            (is (= 1 (count (events-of s :attack.done))) (str targets))
            (is (every? #(zero? (count (h/calls p %))) ["equip" "moveTo" "look" "attack"]) (str targets))
            (is (pos? (count (h/calls p "wait"))) (str targets))))))))

(deftest a-target-arriving-within-the-grace-is_attacked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (await (h/first-round (spec {:targets ["zombie"]}) {:inventory h/sword}))]
          (.push (.-entities (.-state (.-world p)))
                 #js {:id 7 :name "zombie" :kind "hostile" :health 20 :pos (tu/pos 2 64 0)})
          (await (run-ticks s 1 1000))
          (is (= [7] (attacked s)))
          (is (not (finished? s)))
          (is (nil? (done-event s))))))))

(deftest absent-wait-declines-until-a-target-turns-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock] :as s} (h/setup {:inventory h/sword})]
          (core/submit! eng (spec {:targets ["zombie"] :absent :wait}) {})
          (is (nil? (core/tick! eng)))
          (is (zero? (count (events-of s :attack.done))))
          (.push (.-entities (.-state (.-world p)))
                 #js {:id 7 :name "zombie" :kind "hostile" :health 20 :pos (tu/pos 2 64 0)})
          (swap! clock + 700)
          (await (core/tick! eng))
          (is (= [7] (attacked s))))))))

(deftest lost-when-a-target-is-neither-killed-nor-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p clock eng] :as s} (await (scenario {:targets [7 8]} {:inventory h/sword :entities [(zed 7 2) (zed 8 3)]} 4))]
          (aset (aget (.. p -world -state -entities) 0) "pos" (tu/pos 0 64 40))
          (swap! clock + 6000)
          (await (core/tick! eng))
          (is (= :lost (:reason (done-event s))))
          (is (= [7] (:killed (done-event s)))))))))

(deftest a-given-up-target-that-leaves-ends-gave-up-not-cleared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p clock eng] :as s} (await (scenario {:targets [7]}
                                                           {:inventory h/sword
                                                            :entities [(ent 7 "zombie" "hostile" 2 {:invulnerable true})]} 4))]
          (aset (aget (.. p -world -state -entities) 0) "pos" (tu/pos 0 64 40))
          (swap! clock + 6000)
          (await (core/tick! eng))
          (is (= :gave-up (:reason (done-event s))))
          (is (= {7 :no-damage} (:given-up (done-event s)))))))))

(defn ^:async scripted-attack
  "Submit an attack on id 7 (in reach) whose attack results follow statuses in order, then repeat the last; n ticks."
  [statuses n]
  (let [{:keys [p eng] :as s} (h/setup {:inventory h/sword :entities [(zed 7 2)]})
        i (atom 0)]
    (.override (.-world p) "attack"
               (fn [_token _args _impl]
                 (let [st (nth statuses (min @i (dec (count statuses))))]
                   (swap! i inc)
                   (js/Promise.resolve #js {:status st :hurt true}))))
    (core/submit! eng (spec {:targets [7] :timeout-s 1000}) {})
    (await (run-ticks s n 700))
    s))

(deftest a-landed-hit-resets-the-out-of-reach-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scripted-attack (vec (take 12 (cycle ["out-of-reach" "hit"]))) 24))]
          (is (>= (count (attacked s)) 12))
          (is (empty? (events-of s :attack.gave-up))))))))

(deftest three-out-of-reach-swings-in-a-row-still-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scripted-attack ["out-of-reach"] 10))]
          (is (= [[7 :unreachable]] (mapv (juxt :target :reason) (events-of s :attack.gave-up)))))))))

(deftest out-of-reach-right-after-an-arrived-walk-is-the-target-moving-not-a-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup {:inventory h/sword :entities [(zed 7 10)]})]
          ;; the walk answers arrived but the body stays far; the swing finds the target gone on
          (.override (.-world p) "moveTo" (fn [_ _ _] (js/Promise.resolve #js {:status "arrived"})))
          (.override (.-world p) "attack" (fn [_ _ _] (js/Promise.resolve #js {:status "out-of-reach"})))
          (core/submit! eng (spec {:targets [7] :timeout-s 1000}) {})
          (await (run-ticks s 14 700))
          (is (>= (count (attacked s)) 5) "swung well past three times")
          (is (>= (count (h/calls p "moveTo")) 5) "walked each round")
          (is (empty? (events-of s :attack.gave-up))))))))
