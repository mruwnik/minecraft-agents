(ns engine.hostile-test
  "respond-to-hostile, fight-back, retreat and the hostile-near trigger
  against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.jobs.combat :as combat]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn hostile-entries [eng] (mapv :data (mem/entries (mem/view (:store eng)) :hostile)))

(defn zombie
  ([x z] (zombie 7 x z))
  ([id x z] {:id id :name "zombie" :kind "hostile" :pos {:x x :y 64 :z z}}))

(def sword [{:name "iron_sword" :count 1}])

(defn ^:async first-round
  "Submit spec in a world and run one tick; the setup map."
  [spec world]
  (let [s (setup world)]
    (core/submit! (:eng s) spec {})
    (await (core/tick! (:eng s)))
    s))

(defn fought? [{:keys [p]}] (pos? (count (calls p "attack"))))
(defn fled? [{:keys [p]}] (and (zero? (count (calls p "attack"))) (pos? (count (calls p "moveTo")))))

;; ------------------------------------------------------------- the chooser

(def respond '(jobs.survival.respond-to-hostile))

(deftest respond-picks-by-health-gear-mob-and-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (fought? (await (first-round respond {:self {:health 20} :inventory sword :entities [(zombie 3 0)]})))
            "sword and full health: fight")
        (is (fled? (await (first-round respond {:self {:health 10} :inventory sword :entities [(zombie 3 0)]})))
            "hurt: retreat")
        (is (fled? (await (first-round respond {:self {:health 20} :inventory [{:name "iron_pickaxe" :count 1}]
                                                :entities [(zombie 3 0)]})))
            "a pickaxe is not a weapon: retreat")
        (is (fled? (await (first-round respond {:self {:health 20} :inventory sword
                                                :entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]})))
            "a creeper: retreat")
        (is (fled? (await (first-round respond {:self {:health 20} :inventory sword
                                                :entities [(zombie 7 3 0) (zombie 8 3 1) (zombie 9 3 -1)]})))
            "outnumbered: retreat")
        (is (fought? (await (first-round respond {:self {:health 20} :inventory sword
                                                  :entities [(zombie 7 3 0) (zombie 8 3 1)]})))
            "two is still fightable")))))

(deftest respond-fights-a-hostile-beyond-melee-by-walking-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (first-round respond {:inventory sword :entities [(zombie 7 0)]}))]
          (is (fought? s) "the hostile at 7 blocks is walked up to and hit"))))))

(deftest respond-keeps-a-chosen-fight-down-to-min-health
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round respond {:inventory sword :entities [(zombie 100 3 0)]}))]
          (set! (.. p -world -state -self -health) 10)
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack"))) "10 is below fight-health 12 but above min-health 8: still fighting")
          (set! (.. p -world -state -self -health) 6)
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack"))) "below min-health: no more attacks")
          (is (pos? (count (calls p "moveTo"))) "it retreats instead"))))))

(deftest respond-writes-one-hostile-entry-per-encounter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock] :as s} (await (first-round respond {:inventory sword :entities [(zombie 100 3 0)]}))]
          (is (= [{:mob "zombie" :pos {:x 3 :y 64 :z 0} :decision :fight}] (hostile-entries eng)))
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 1 (count (hostile-entries eng))) "a second round of the same encounter writes nothing")
          (is (= {:cap 50 :ttl 3600000} (mem/policy (mem/view (:store eng)) :hostile))))))))

;; ------------------------------------------------------------- fight-back

(def fight '(jobs.survival.fight-back))

(deftest fight-back-equips-attacks-and-finishes-when-the-mob-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round fight {:inventory [{:name "wooden_sword" :count 1} {:name "iron_axe" :count 1}
                                                                           {:name "iron_pickaxe" :count 1}]
                                                               :entities [(zombie 7 3 0)]}))]
          (is (= ["iron_axe"] (mapv #(.-item (.-args %)) (calls p "equip"))) "the best weapon, not the pickaxe")
          (is (= 1 (count (calls p "attack"))))
          (is (= [7] (mapv #(.-id (.-args %)) (calls p "attack"))))
          (is (= ["j1"] (:list (core/state eng))) "the zombie has 20 health and survives one hit")
          (await (core/tick! eng))
          (is (= 1 (count (calls p "attack"))) "within the attack gap nothing is swung")
          (dotimes [_ 3]
            (swap! clock + 700)
            (await (core/tick! eng)))
          (is (= 4 (count (calls p "attack"))))
          (is (= [] (:list (core/state eng))) "four hits killed it: done"))))))

(deftest fight-back-walks-up-to-a-mob-beyond-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round '(jobs.survival.fight-back {:range 6}) {:inventory sword :entities [(zombie 5 0)]}))]
          (is (= 1 (count (calls p "moveTo"))))
          (is (= 1 (count (calls p "attack")))))))))

(deftest fight-back-declines-when-hurt-or-nothing-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 7} :inventory sword :entities [(zombie 3 0)]})]
          (core/submit! eng fight {})
          (is (nil? (core/tick! eng)) "health 7 is below min-health 8"))
        (let [{:keys [eng]} (setup {:inventory sword :entities [(zombie 6 0)]})]
          (core/submit! eng fight {})
          (is (nil? (core/tick! eng)) "6 blocks is beyond range 4"))))))

;; ----------------------------------------------------------------- retreat

(def retreat '(jobs.survival.retreat))

(defn last-move [p]
  (let [a (.-args (peek (calls p "moveTo")))]
    {:x (.. a -pos -x) :z (.. a -pos -z)}))

(deftest retreat-moves-away-and-finishes-after-the-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round retreat {:entities [(zombie 5 0)]}))]
          (is (= {:x -6 :z 0} (last-move p)) "a step of 6 directly away")
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 1 (count (calls p "moveTo"))) "the hostile is out of radius: no more walking")
          (is (= ["j1"] (:list (core/state eng))) "but not done before the cooldown")
          (swap! clock + 5000)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest retreat-resets-its-cooldown-whenever-a-hostile-is-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round retreat {:entities [(zombie 5 0)]}))]
          (swap! clock + 4000)
          (set! (.. p -world -state -entities) #js [(clj->js (assoc (zombie 5 0) :pos {:x -6 :y 64 :z 3}))])
          (await (core/tick! eng))
          (is (= 2 (count (calls p "moveTo"))) "seen again after 4 s: walks again")
          (swap! clock + 4000)
          (set! (.. p -world -state -entities) #js [])
          (await (core/tick! eng))
          (is (= ["j1"] (:list (core/state eng))) "only 4 s since it was last seen")
          (swap! clock + 1500)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(defn remember! [eng kind data] (mem/write! (:store eng) kind data mem/place-policy))

(deftest retreat-leans-towards-a-bed-or-home-unless-it-is-through-the-hostile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(zombie 0 5)]})]
          (remember! eng :bed {:pos {:x -20 :y 64 :z -20}})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (let [m (last-move p)]
            (is (< (:x m) -1) "pulled towards the bed's side")
            (is (< (:z m) -1) "still away from the hostile")))
        (let [{:keys [eng p]} (setup {:entities [(zombie 0 5)]})]
          (remember! eng :home {:pos {:x 0 :y 64 :z 20}})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= {:x 0 :z -6} (last-move p)) "a home beyond the hostile is ignored"))))))

(deftest retreat-avoids-hazard-entries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(zombie 5 0)]})]
          (mem/write! (:store eng) :hazard {:kind :lava :pos {:x -6 :y 64 :z 0}} {:cap 50 :ttl :forever})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (let [m (last-move p)]
            (is (not= 0 (:z m)) "turned off the straight line through the lava")
            (is (< (:x m) 0) "and still away")))))))

(deftest retreat-gives-up-when-every-walk-is-blocked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(zombie 5 0)]})]
          (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status "blocked"}))
          (core/submit! eng retreat {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng)))))))))

;; ----------------------------------------------------------------- trigger

(def when-hostile-near (:when triggers/hostile-near))

(defn holds? [world args]
  (when-hostile-near (tu/fake world) {:data mem/empty-data :now 0} args))

(deftest hostile-near-needs-a-hostile-mob-within-radius
  (is (holds? {:entities [(zombie 5 0)]} {:radius 8}))
  (is (not (holds? {:entities [(zombie 5 0)]} {:radius 4})) "beyond the radius")
  (is (not (holds? {:entities [{:id 1 :name "Steve" :kind "player" :pos {:x 2 :y 64 :z 0}}]} {:radius 8})) "a player")
  (is (not (holds? {:entities [{:id 2 :name "cow" :kind "passive" :pos {:x 2 :y 64 :z 0}}]} {:radius 8})) "a passive"))

(deftest hostile-near-fires-the-chooser
  (is (= '(jobs.survival.respond-to-hostile) (:job triggers/hostile-near))))

;; ------------------------------------------------------------- line of sight

(def wall
  (into {} (for [x [2 3] y [64 65 66]] [(str x "," y ",0") "stone"])))

(deftest hostile-near-ignores-a-hostile-behind-a-wall
  (is (not (holds? {:blocks wall :entities [(zombie 5 0)]} {:radius 8})) "behind two blocks of stone: silent")
  (is (holds? {:entities [(zombie 5 0)]} {:radius 8}) "the same hostile with no wall: fires")
  (is (holds? {:blocks wall :entities [(zombie 5 0)]} {:radius 8 :visible-only false}) "visible-only false counts it again"))

(deftest combat-hostiles-can-filter-or-prefer-visible
  (let [p (tu/fake {:blocks wall :entities [(zombie 1 3 3) (zombie 2 4 0)]})
        ids (fn [hs] (mapv #(.-id %) hs))]
    (is (= [2 1] (ids (combat/hostiles p 8))) "two arguments: unchanged, nearest first, sight ignored")
    (is (= [1] (ids (combat/hostiles p 8 {:sight :only}))))
    (is (= [1 2] (ids (combat/hostiles p 8 {:sight :prefer}))) "prefer: visible first, then the rest")))

;; ------------------------------------------------------- unreachable hostiles

(defn walks-to [p pos]
  (count (filter #(let [a (.. % -args -pos)] (and (= (:x pos) (.-x a)) (= (:z pos) (.-z a)))) (calls p "moveTo"))))

(deftest respond-retreats-from-a-hostile-it-cannot-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:inventory sword :entities [(zombie 6 0)] :unreachable ["6,64,0"]})]
          (core/submit! eng respond {})
          (dotimes [_ 3]
            (await (core/tick! eng))
            (swap! clock + 1000))
          (is (= 3 (walks-to p {:x 6 :z 0})) "three blocked walks towards the zombie")
          (is (< (:x (last-move p)) 0) "then it walks away instead")
          (dotimes [_ 3]
            (await (core/tick! eng))
            (swap! clock + 1000))
          (is (= 3 (walks-to p {:x 6 :z 0})) "the unreachable zombie is not walked at again")
          (is (zero? (count (calls p "attack")))))))))
