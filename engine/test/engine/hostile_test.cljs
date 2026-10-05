(ns engine.hostile-test
  "respond-to-hostile, fight-back, retreat and the hostile-near trigger
  against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.jobs.combat :as combat]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn set-entities!
  "Replace the fake's entities with the spec-style entity maps."
  [p es]
  (swap! (fake/state p) assoc :entities [])
  (doseq [e es] (fake/add-entity! p e)))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
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
(defn fled?
  "No swing, and a walk with the engine walker."
  [{:keys [p eng]}]
  (and (zero? (count (calls p "attack"))) (pos? (count (tu/walked-to eng)))))

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
                                                  :equipment {:torso {:name "iron_chestplate" :count 1}
                                                              :legs {:name "iron_leggings" :count 1}}
                                                  :entities [(zombie 7 3 0) (zombie 8 3 1)]})))
            "two are fightable in armour (11 points: 22.5 expected damage becomes 12.6)")))))

(deftest respond-fights-a-hostile-beyond-melee-by-walking-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (first-round respond {:inventory sword :entities [(zombie 7 0)]}))]
          (is (fought? s) "the hostile at 7 blocks is walked up to and hit"))))))

(deftest respond-keeps-a-fight-while-what-is-left-of-the-mob-is-worth-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round respond {:inventory sword :entities [(zombie 100 3 0)]}))]
          (fake/swap-self! p assoc :health 10)
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack"))) "health 10, the zombie at 15: 5.6 expected damage leaves the reserve of 4: still fighting")
          (fake/swap-self! p assoc :health 6)
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack"))) "health 6: 5.6 more would eat into the reserve: no more attacks")
          (is (pos? (count (tu/walked-to eng))) "it retreats instead"))))))

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
        (let [{:keys [p eng]} (await (first-round '(jobs.survival.fight-back {:range 6}) {:inventory sword :entities [(zombie 5 0)]}))]
          (is (= 1 (count (tu/walked-to eng))))
          (is (every? #(<= % 15) (map #(.-timeoutS (.-args %)) (calls p "steer"))) "a short walk: it aims again at the mob")
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

(defn last-move
  "The x and z of the latest walk's target."
  [eng]
  (select-keys (peek (tu/walked-to eng)) [:x :z]))

(defn remember! [eng kind data] (mem/write! (:store eng) kind data mem/place-policy))

(deftest retreat-leans-towards-a-bed-or-home-unless-it-is-through-the-hostile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(zombie 0 5)]})]
          (remember! eng :bed {:pos {:x -20 :y 64 :z -20}})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (let [m (last-move eng)]
            (is (< (:x m) -1) "pulled towards the bed's side")
            (is (< (:z m) -1) "still away from the hostile")))
        (let [{:keys [eng p]} (setup {:entities [(zombie 0 5)]})]
          (remember! eng :home {:pos {:x 0 :y 64 :z 20}})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= {:x 0 :z -6} (last-move eng)) "a home beyond the hostile is ignored"))))))

(deftest retreat-avoids-hazard-entries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(zombie 5 0)]})]
          (mem/write! (:store eng) :hazard {:kind :lava :pos {:x -6 :y 64 :z 0}} {:cap 50 :ttl :forever})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (let [m (last-move eng)]
            (is (not= 0 (:z m)) "turned off the straight line through the lava")
            (is (< (:x m) 0) "and still away")))))))

(def stone-box
  "Stone three thick around the cell [6 64 0] (feet and head height, below and above): a zombie there cannot be
  reached or hit."
  (into {} (for [x (range 3 10) y [63 64 65 66] z (range -3 4) :when (not (and (= x 6) (= z 0) (#{64 65} y)))]
             [(str x "," y "," z) "stone"])))

(deftest retreat-from-a-zombie-entombed-in-stone-is-no-flight-at-all
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:blocks stone-box :entities [(zombie 6 0)]})]
          (.override (.-world p) "steer" (fn [_ _ _] (js/Promise.resolve #js {:status "timeout" :pose #js {}})))
          (core/submit! eng retreat {})
          (dotimes [_ 8] (swap! clock + 60000) (await (core/tick! eng)))
          (is (zero? (count (filter #(= :retreat_blocked (:kind %)) @seen))) "it cannot reach the body: not a danger, no warn")
          (is (zero? (count (calls p "attack"))))
          (is (zero? (count (tu/walked-to eng))) "and no flight")
          (is (= [] (:list (core/state eng)))))))))

(defn stuck-walks!
  "Make every walk of the engine walker get no nearer: :blocked."
  [p]
  (.override (.-world p) "steer" (fn [_ _ _] (js/Promise.resolve #js {:status "timeout" :pose #js {}}))))

(deftest retreat-whose-every-walk-is-blocked-fights-with-the-fist
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(zombie 2 0)]})]
          (stuck-walks! p)
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "attack"))) "unarmed and nothing to seal with: it hits back")
          (is (= ["j1"] (:list (core/state eng)))))))))

;; ----------------------------------------------------------------- trigger

(def when-hostile-near (:when triggers/hostile-near))

(defn holds? [world args]
  (when-hostile-near (tu/fake-on-floor world) {:data mem/empty-data :now 0} args))

(deftest hostile-near-needs-a-hostile-mob-within-radius
  (is (holds? {:entities [(zombie 5 0)]} {:radius 8}))
  (is (not (holds? {:entities [(zombie 5 0)]} {:radius 4})) "beyond the radius")
  (is (not (holds? {:entities [{:id 1 :name "Steve" :kind "player" :pos {:x 2 :y 64 :z 0}}]} {:radius 8})) "a player")
  (is (not (holds? {:entities [{:id 2 :name "cow" :kind "passive" :pos {:x 2 :y 64 :z 0}}]} {:radius 8})) "a passive"))

(deftest hostile-near-sees-no-danger-while-the-body-is-dead
  (let [near (tu/fake-on-floor {:entities [(zombie 5 0)]})
        holds-with (fn [& entries]
                     (boolean (when-hostile-near near
                                                 {:data (reduce (fn [d [kind t]] (mem/add-entry d kind {:t t :data {}} nil)) mem/empty-data entries)
                                                  :now 3000}
                                                 {:radius 8})))]
    (is (false? (holds-with [:died 1000])) "died, not yet respawned")
    (is (true? (holds-with [:died 1000] [:respawned 2000])) "alive again")
    (is (false? (holds-with [:respawned 500] [:died 1000])) "an older respawn does not count")
    (is (true? (holds-with)) "never died")))

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
  (let [p (tu/fake-on-floor {:blocks wall :entities [(zombie 1 3 3) (zombie 2 4 0)]})
        ids (fn [hs] (mapv #(.-id %) hs))]
    (is (= [2 1] (ids (combat/hostiles p 8))) "two arguments: unchanged, nearest first, sight ignored")
    (is (= [1] (ids (combat/hostiles p 8 {:sight :only}))))
    (is (= [1 2] (ids (combat/hostiles p 8 {:sight :prefer}))) "prefer: visible first, then the rest")))

;; ------------------------------------------------------- unreachable hostiles

(defn walks-to
  "How many walks went to pos's x and z: walk-near! walks (their :moved entries) and moveTo calls."
  [{:keys [p eng]} pos]
  (+ (count (filter #(and (= (:x pos) (:x %)) (= (:z pos) (:z %))) (tu/walked-to eng)))
     (count (filter #(let [a (.. % -args -pos)] (and (= (:x pos) (.-x a)) (= (:z pos) (.-z a)))) (calls p "moveTo")))))

(deftest respond-retreats-from-a-hostile-it-cannot-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:inventory sword :entities [(zombie 6 0)] :unreachable ["6,64,0"]})]
          (core/submit! eng respond {})
          (dotimes [_ 3]
            (await (core/tick! eng))
            (swap! clock + 1000))
          (is (= 3 (walks-to {:p p :eng eng} {:x 6 :z 0})) "three blocked walks towards the zombie")
          (is (< (:x (last-move eng)) 0) "then it walks away instead")
          (dotimes [_ 3]
            (await (core/tick! eng))
            (swap! clock + 1000))
          (is (= 3 (walks-to {:p p :eng eng} {:x 6 :z 0})) "the unreachable zombie is not walked at again")
          (is (zero? (count (calls p "attack")))))))))

;; ------------------------------------------------------- ranged mobs count further out

(defn skeleton [id x z] {:id id :name "skeleton" :kind "hostile" :pos {:x x :y 64 :z z}})

(deftest hostile-near-counts-ranged-mobs-out-to-the-ranged-radius
  (is (holds? {:entities [(skeleton 1 13 0)]} {:radius 8 :ranged-radius 16}) "a skeleton at 13")
  (is (not (holds? {:entities [(zombie 13 0)]} {:radius 8 :ranged-radius 16})) "a zombie at 13 is beyond 8")
  (is (not (holds? {:entities [(skeleton 1 13 0)]} {:radius 8 :ranged-radius 10})) "beyond the ranged radius")
  (is (= 16 (:ranged-radius (:args triggers/hostile-near))) "16 by default"))

(defn arrow-wall
  "Blocks of name at x 3 across z -3..3 for each y in ys, except the cells in gaps ([y z])."
  [name ys & [gaps]]
  (into {} (for [y ys z (range -3 4) :when (not (contains? (set gaps) [y z]))] [(str "3," y "," z) name])))

(def slit "A 1-high gap at head height in a stone wall." (merge (arrow-wall "stone" [64 65 66] [[65 0]])))

(defn skeleton-at-6 [] [(skeleton 1 6 0)])

(defn skeleton-fires?
  "Whether hostile-near holds for a skeleton at x 6 with blocks in the way."
  [blocks]
  (boolean (holds? {:entities (skeleton-at-6) :blocks blocks} {:radius 8 :ranged-radius 16})))

(deftest hostile-near-needs-an-arrow-line-for-a-ranged-mob
  (doseq [[label blocks fires?]
          [["in the open" {} true]
           ["behind a glass wall" (arrow-wall "glass" [64 65 66]) false]
           ["behind a stone wall" (arrow-wall "stone" [64 65 66]) false]
           ["behind a leaves wall" (arrow-wall "oak_leaves" [64 65 66]) false]
           ["through a 1-high slit at head height" slit true]
           ["through a wall of poppies and torches" (merge (arrow-wall "poppy" [64]) (arrow-wall "torch" [65 66])) true]
           ["behind a wall of shut trapdoors" (arrow-wall "oak_trapdoor" [64 65 66]) false]]]
    (is (= fires? (skeleton-fires? blocks)) label)))

(deftest combat-hostiles-takes-a-ranged-radius
  (let [p (tu/fake-on-floor {:entities [(zombie 1 13 0) (skeleton 2 12 0) (zombie 3 5 0)]})
        ids (fn [hs] (mapv #(.-id %) hs))]
    (is (= [3] (ids (combat/hostiles p 8))))
    (is (= [3 2] (ids (combat/hostiles p 8 {:ranged-radius 16}))))))

(deftest respond-walks-up-to-a-skeleton-at-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (first-round respond {:inventory sword :entities [(skeleton 7 13 0)]}))]
          (is (pos? (walks-to s {:x 13 :z 0})) "armed and healthy: it closes in on the skeleton"))))))

;; ------------------------------------------------------- retreat keeps going and minds walls

(defn wall-cells
  "Stone at feet and head height on the given [x z] cells."
  [cells]
  (into {} (for [[x z] cells y [64 65]] [(str x "," y "," z) "stone"])))

(deftest retreat-turns-away-from-a-wall-behind-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:entities [(zombie 5 0)]
                                      :blocks (wall-cells (for [z (range -2 3)] [-3 z]))})]
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (let [m (last-move eng)]
            (is (>= (js/Math.abs (:z m)) 3) "not straight back into the wall")
            (is (<= (:x m) 0) "and not towards the zombie")))))))

(def box
  "A 3x3 pen around the origin, open only on the +x side."
  (wall-cells (concat (for [z (range -2 3)] [-2 z]) (for [x (range -2 2)] [x -2]) (for [x (range -2 2)] [x 2]))))

(deftest a-cornered-retreat-fights-when-armed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round retreat {:inventory sword :blocks box :entities [(zombie 2 0)]}))]
          (is (= 1 (count (calls p "attack"))) "nowhere to go: it hits back"))
        (let [{:keys [eng p]} (await (first-round retreat {:blocks box :entities [(zombie 2 0)]}))]
          (is (zero? (count (calls p "attack"))) "unarmed: first a step back into the far corner of the pen")
          (is (= -1 (:x (last-move eng))) "the far corner")
          (await (core/tick! eng))
          (is (= 1 (count (calls p "attack"))) "nowhere further and nothing to build with: the fist, not a failed round")
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= ["j1"] (:list (core/state eng))) "no giving up while the zombie can be hit"))))))

(def dead-end
  "A corridor five wide, closed behind the body (z -2), open towards +z."
  (wall-cells (concat (for [x (range -3 4)] [x -2]) (for [z (range -2 8)] [-3 z]) (for [z (range -2 8)] [3 z]))))

(deftest a-retreat-with-only-side-steps-left-fights-instead-of-shuffling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (first-round retreat {:inventory sword :blocks dead-end :entities [(zombie 0 3)]}))]
          (is (zero? (count (tu/walked-to eng))) "a two-block side step gains nothing on the zombie")
          (is (= 1 (count (calls p "attack")))))))))

;; ------------------------------------------------------- sight

(deftest respond-ignores-a-hostile-it-cannot-see
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory sword :blocks wall :entities [(zombie 5 0)]})]
          (core/submit! eng respond {})
          (is (nil? (core/tick! eng)) "only a zombie behind the wall: nothing to respond to")
          (is (zero? (count (tu/walked-to eng)))))))))

(deftest fight-back-goes-for-a-visible-hostile-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round '(jobs.survival.fight-back {:range 8})
                                              {:inventory sword :blocks wall :entities [(zombie 1 5 0) (zombie 2 0 6)]}))]
          (is (= [2] (mapv #(.-id (.-args %)) (calls p "attack")))
              "the nearer zombie is behind the wall; the visible one is fought"))))))

;; ------------------------------------------------------- finishing a fight, eating on the run

(deftest combat-estimates-what-is-left-of-a-mob
  (is (= 8 (combat/remaining-health {:name "zombie" :hits 2 :damage 6})) "20 less two iron sword hits")
  (is (= 3 (combat/remaining-health {:name "zombie" :hits 2 :damage 6 :health 3})) "a reported health wins")
  (is (= 16 (combat/remaining-health {:name "spider" :hits 0 :damage 6})))
  (is (= 20 (combat/remaining-health {:name "unknown_mob" :hits 0 :damage 6})) "unknown mobs count as 20")
  (is (= 6 (combat/weapon-damage "iron_sword")))
  (is (= 1 (combat/weapon-damage nil)) "a fist"))

(deftest respond-finishes-a-nearly-dead-hostile-below-min-health
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round respond {:inventory sword :entities [(assoc (zombie 100 3 0) :health 10)]}))]
          (is (= 1 (count (calls p "attack"))) "one hit of the fake's 5: the zombie is at 5")
          (fake/swap-self! p assoc :health 6)
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack"))) "below min-health, but one more hit kills it: it swings")
          (is (empty? (fake/entities p))))))))

(deftest respond-still-flees-below-min-health-from-a-healthy-hostile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round respond {:inventory sword :entities [(assoc (zombie 100 3 0) :health 20)]}))]
          (fake/swap-self! p assoc :health 6)
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 1 (count (calls p "attack"))) "15 left is not nearly dead"))))))

(deftest retreat-eats-once-when-the-gap-is-wide
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round retreat {:self {:food 10} :inventory [{:name "bread" :count 3}]
                                                                 :entities [(skeleton 1 5 0)]}))]
          (is (zero? (count (calls p "eat"))) "the skeleton is 5 away: no time to eat")
          (set-entities! p [(skeleton 1 8 0)])
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 1 (count (calls p "eat"))) "14 blocks of gap: it eats")
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 1 (count (calls p "eat"))) "once per flight"))))))

(deftest a-cornered-fight-is-kept-while-the-hostile-is-close
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round retreat {:inventory sword :blocks box :entities [(assoc (zombie 2 0) :health 20)]}))]
          (is (= 1 (count (calls p "attack"))))
          (swap! (fake/state p) update :blocks #(apply dissoc % (keys box)))
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack"))) "the pen opened up, but the zombie is still at 2: keep fighting")
          (is (zero? (count (tu/walked-to eng))) "no running back and forth"))))))

;; ------------------------------------------------- only a real danger fires hostile-near

(defn cells-of
  "Blocks named block-name at every [x y z] in cells."
  [block-name cells]
  (into {} (for [[x y z] cells] [(str x "," y "," z) block-name])))

(defn visible-zombie [x z] (assoc (zombie 7 x z) :visible true))

(def danger-args {:radius 10 :ranged-radius 16})

(def zombie-pen
  "A 3x3 pen of stone two high around (8 0), the zombie in the middle."
  (cells-of "stone" (for [x [7 8 9] z [-1 0 1] y [64 65] :when (not (and (= x 8) (= z 0)))] [x y z])))

(def dividing-wall
  "A wall at x 3 across the whole floor, three high."
  (cells-of "stone" (for [z (range -10 11) y [64 65 66]] [3 y z])))

(def trench
  "A trench two deep at x 3 and 4 across the whole floor."
  (cells-of "stone" (for [x [3 4] z (range -10 11)] [x 61 z])))

(deftest hostile-near-ignores-a-zombie-with-no-way-to-the-body
  (is (not (holds? {:blocks zombie-pen :entities [(visible-zombie 8 0)]} danger-args)) "walled into a pen")
  (is (not (holds? {:blocks dividing-wall :entities [(visible-zombie 8 0)]} danger-args)) "behind a wall across the floor")
  (is (not (holds? {:blocks trench :entities [(visible-zombie 8 0)]} danger-args)) "across a trench two deep: down is fine, up is not")
  (is (not (holds? {:blocks (cells-of "stone" (for [x [-1 0 1] z [-1 0 1] y [64 65] :when (not (and (= x 0) (= z 0)))] [x y z]))
                    :entities [(visible-zombie 5 0)]}
                   danger-args))
      "with the body sealed in"))

(deftest hostile-near-fires-for-a-zombie-that-can-walk-to-the-body
  (is (holds? {:entities [(visible-zombie 5 0)]} danger-args) "open ground")
  (is (holds? {:blocks (cells-of "stone" (for [z (range -9 11) y [64 65 66]] [3 y z])) :entities [(visible-zombie 8 0)]} danger-args)
      "a wall with a way round at z -10")
  (is (holds? {:blocks (cells-of "stone" (for [x [3 4] z (range -10 11)] [x 62 z])) :entities [(visible-zombie 8 0)]} danger-args)
      "across a trench one deep: it climbs out"))

(deftest hostile-near-needs-a-line-of-fire-for-a-skeleton
  (is (not (holds? {:blocks dividing-wall :entities [(skeleton 1 8 0)]} danger-args)) "behind a wall with no opening")
  (is (holds? {:blocks (dissoc dividing-wall "3,65,0") :entities [(skeleton 1 8 0)]} danger-args)
      "through a 1x1 window it can shoot"))

;; ------------------------------------------------- the odds: a response the body can survive

(def iron-armour
  {:head {:name "iron_helmet" :count 1} :torso {:name "iron_chestplate" :count 1}
   :legs {:name "iron_leggings" :count 1} :feet {:name "iron_boots" :count 1}})

(deftest combat-counts-armour-points-from-equipment
  (is (= 0 (combat/armour-points nil)))
  (is (= 15 (combat/armour-points (clj->js {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"}
                                            :legs {:name "iron_leggings"} :feet {:name "iron_boots"}}))))
  (is (= 5 (combat/armour-points (clj->js {:torso {:name "golden_chestplate"} :mainHand {:name "iron_sword"}})))
      "only worn slots count"))

(deftest combat-fight-damage-weighs-weapon-mobs-and-armour
  (let [z {:name "zombie" :distance 3}]
    (is (= 7.5 (combat/fight-damage {:weapon "iron_sword" :armour 0 :mobs [z]})) "4 hits at 0.625 s while it hits 3/s")
    (is (= 30 (combat/fight-damage {:weapon nil :armour 0 :mobs [z]})) "the fist: 20 hits at 0.5 s")
    (is (= 22.5 (combat/fight-damage {:weapon "iron_sword" :armour 0 :mobs [z z]})) "the second hits until it dies too")
    (is (= 3 (combat/fight-damage {:weapon "iron_sword" :armour 15 :mobs [z]})) "iron armour (15 points) takes 60% off")
    (is (= 3.75 (combat/fight-damage {:weapon "iron_sword" :armour 0 :mobs [(assoc z :hits 2)]})) "a struck mob has less left")
    (is (= 10 (combat/fight-damage {:weapon "iron_sword" :armour 0 :mobs [{:name "skeleton" :distance 13}]}))
        "a skeleton shoots while the body closes 10 blocks")))

(defn walked? [{:keys [eng]}] (pos? (count (tu/walked-to eng))))
(defn fled-by-walker? [{:keys [p] :as s}]
  (and (zero? (count (calls p "attack"))) (zero? (count (calls p "moveTo"))) (walked? s)))

(deftest respond-picks-a-response-the-body-can-survive
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[why spec want] [["no weapon vs one zombie" {:entities [(zombie 3 0)]} :flee]
                                 ["sword vs one zombie" {:inventory sword :entities [(zombie 3 0)]} :fight]
                                 ["no weapon vs three zombies" {:entities [(zombie 7 3 0) (zombie 8 3 1) (zombie 9 3 -1)]} :flee]
                                 ["sword vs a creeper" {:inventory sword :entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]} :flee]
                                 ["sword vs a skeleton at range" {:inventory sword :entities [(assoc (skeleton 7 12 0) :visible true)]} :fight]
                                 ["no weapon vs a skeleton at range" {:entities [(assoc (skeleton 7 12 0) :visible true)]} :flee]
                                 ["sword vs two zombies, no armour" {:inventory sword :entities [(zombie 7 3 0) (zombie 8 3 1)]} :flee]
                                 ["sword vs two zombies in iron armour" {:inventory sword :equipment iron-armour
                                                                         :entities [(zombie 7 3 0) (zombie 8 3 1)]} :fight]]]
          (let [s (await (first-round respond spec))
                got (:decision (first (hostile-entries (:eng s))))]
            (is (= want got) why)
            (is (= (= :flee want) (fled-by-walker? s)) (str why ": flees with the engine walker, no swing"))))))))

(deftest respond-fights-with-the-best-weapon-equipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round respond {:inventory [{:name "stone_sword" :count 1} {:name "diamond_sword" :count 1}]
                                                       :entities [(zombie 3 0)]}))]
          (is (= ["diamond_sword"] (mapv #(.-item (.-args %)) (calls p "equip"))))
          (is (= 1 (count (calls p "attack")))))))))

(def stair-exit
  "A dead-end corridor one wide along +z, closed behind the body (z -1): the way out of a dug-in cell, the zombie in it."
  (wall-cells (concat (for [z (range -1 4)] [-1 z]) (for [z (range -1 4)] [1 z]) [[0 -1]])))

(deftest a-cornered-unarmed-body-off-centre-seals-itself-in-instead-of-punching
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round retreat {:bodyHitbox true :self {:pos [0.5 64 0.75]}
                                                       :inventory [{:name "cobblestone" :count 16}]
                                                       :blocks stair-exit :entities [(zombie 0.5 2.5)]}))]
          (is (zero? (count (calls p "attack"))) "no fist fight")
          (is (= "cobblestone" (.-name (.blockAt p #js {:x 0 :y 64 :z 1})))
              "the side cell its hitbox overlapped is filled: it stood in the middle of its cell first"))))))

;; ------------------------------------------------- the hostile reflex: no cooldown, no backoff, a clear end

(defn reflex-ended [seen] (filterv #(and (= :reflex (:source %)) (= :ended (:kind %))) @seen))

(defn reflex-fired [seen] (filterv #(and (= :reflex (:source %)) (= :fired (:kind %))) @seen))

(defn reflex-jobs
  "The ids of the reflex job instances the engine holds."
  [eng]
  (keep (fn [[id inst]] (when (:reflex inst) id)) (:instances (core/state eng))))

(deftest hostile-near-defaults-to-no-cooldown
  (is (= :retry (:persistence triggers/hostile-near)))
  (is (zero? (:cooldown-s triggers/hostile-near 0))))

(deftest respond-to-hostile-is-never-backed-off
  (is (false? (:backoff (registry/jobs 'jobs.survival.respond-to-hostile)))))

(deftest a-far-walkable-zombie-does-not-hold-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:entities [(zombie 3 0)]})]
          (core/register-reflex! eng {:trigger :hostile-near})
          (await (core/tick! eng))
          (is (= 1 (count (reflex-fired seen))) "a zombie at 3: the reflex fires")
          (set-entities! p [(zombie 24 0)])
          (dotimes [_ 2] (swap! clock + 1000) (await (core/tick! eng)))
          (is (empty? (reflex-jobs eng)) "a zombie 30 blocks off, walkable or not, is outside the trigger's radius: the reflex ended")
          (is (= [:done] (mapv :outcome (reflex-ended seen)))))))))

(deftest a-retreat-is-done-the-first-round-no-danger-is-within-its-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (first-round retreat {:entities [(zombie 5 0)]}))]
          (is (= {:x -6 :z 0} (last-move eng)) "a step of 6 directly away")
          (set-entities! p [(zombie 30 0)])
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "no wait for a clear radius of 40 or a cooldown"))))))

(deftest a-blocked-retreat-with-the-danger-present-neither-ends-nor-backs-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:entities [(zombie 5 0)]})]
          (stuck-walks! p)
          (core/register-reflex! eng {:trigger :hostile-near})
          (dotimes [_ 12] (await (core/tick! eng)) (swap! clock + 1000))
          (is (= 1 (count (reflex-fired seen))) "fired once and held")
          (is (empty? (reflex-ended seen)) "never ended while the zombie stands")
          (is (seq (reflex-jobs eng)))
          (is (empty? (filter #(= :backoff (:kind %)) @seen)) "never backed off"))))))

(deftest an-explicit-cooldown-in-the-agents-entry-is-honoured
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[why entry fires-again?]
                [["the default: fired again at once" {:trigger :hostile-near :args {:radius 10}} true]
                 ["the agent's own cooldown" {:trigger :hostile-near :args {:radius 10} :persistence :cooldown :cooldown-s 30} false]]]
          (let [{:keys [eng seen clock]} (setup {:inventory sword
                                                 :entities [(assoc (zombie 1 3 0) :health 5 :visible true)
                                                            (assoc (zombie 2 0 7) :visible true)]})]
            (core/register-reflex! eng (assoc entry :job '(jobs.survival.respond-to-hostile {:radius 4})))
            (await (core/tick! eng))
            (is (= [:done] (mapv :outcome (reflex-ended seen))) (str why ": the near zombie killed, the one at 7 is beyond the job's 4"))
            (swap! clock + 1000)
            (await (core/tick! eng))
            (is (= (if fires-again? 2 1) (count (reflex-fired seen))) why)))))))

(deftest a-hostile-reflex-cut-by-a-higher-one-fires-again-while-the-danger-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:entities [(zombie 5 0)]})]
          (core/register-reflex! eng {:trigger :burning})
          (core/register-reflex! eng {:trigger :hostile-near})
          (await (core/tick! eng))
          (is (= [:hostile-near] (mapv :reflex (reflex-fired seen))))
          (fake/swap-self! p assoc :onFire true)
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= :burning (:reflex (peek (reflex-fired seen)))) "the fire cuts the flight")
          (fake/swap-self! p assoc :onFire false)
          (set-entities! p [(zombie (+ 3 (first (:pos (fake/self p)))) 0)])
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= :hostile-near (:reflex (peek (reflex-fired seen)))) "fired again, fresh, as soon as the fire is out"))))))
