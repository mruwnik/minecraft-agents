(ns engine.hostile-test
  "respond-to-hostile, retreat and the hostile-near trigger
  against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.flight-job :as fj]
            [engine.settings :as settings]
            [engine.hurt :as hurt]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [jobs.lib.body :as body]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.hostile-fight :as hostile-fight]
            [jobs.combat.attack :as attack]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops :as recover-drops]
            [jobs.survival.recover-drops.danger :as recover-drops-danger]
            [jobs.survival.respond-to-hostile :as respond-to-hostile]
            [jobs.survival.shut-in :as shut-in]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.pace :as pace]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.fake.raw-world :as fake-raw]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn set-entities!
  "Replace the fake's entities with the spec-style entity maps."
  [p es]
  (swap! (fake/state p) assoc :entities [])
  (doseq [e es] (fake/add-entity! p e)))

(def flight-floor
  "Ground wide enough for a whole flight from a mob that stands still: 35 blocks (a zombie's follow range) every way."
  [-50 -45 50 45])

(defn setup
  "An engine over a fake on the flight floor. Its clock moves ms (default 0) with every primitive call. Its world store
  is the given one (default: an empty zone list, so zone-checking jobs act)."
  ([world] (setup world 0))
  ([world ms] (setup world ms (ew/of-data {} {} [])))
  ([world ms store]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor (merge {:floor flight-floor} world))
        now (tu/act-clock clock p ms)
        eng (core/create {:primitives p :jobs fj/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now :world store
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :seen seen :clock clock})))

(defn zone-store
  "A world store whose only zone is a column of cells at x (owner as given; nil: no zone list was read)."
  [x owner]
  (if (nil? owner)
    (ew/of-data {} {} nil)
    (ew/of-data {} {} [{:name "farm" :owner owner :min [x 60 -5] :max [x 70 5]}])))

(defn setup-flight
  "As setup, the clock moving a second with every primitive call: a whole flight ends by time (out of line, its bound)."
  [world]
  (setup world 1000))

(defn setup-seeing
  "As setup, the body seeing through perception (engine.perception/wrap) as it does live; light is [sky block] for
  every cell when given (jobs that look round only in the dark); :radius in world sets the sight radius (default 16)."
  ([world light] (setup-seeing world light 0))
  ([world light ms]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor (merge {:floor flight-floor} (dissoc world :ray-deg :rand :radius)))
        _ (when light (swap! (fake/state raw) assoc :light-default light))
        now (tu/act-clock clock raw ms)
        p (perception/wrap raw (perception/create (fake-raw/create raw) (cond-> {:radius (:radius world 16) :ray-deg (:ray-deg world 2) :now now}
                                                                          (:rand world) (assoc :rand (:rand world)))))
        eng (core/create {:primitives p :jobs fj/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :raw raw :seen seen :clock clock})))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn hostile-entries [eng] (mapv #(dissoc (:data %) :health :run) (mem/entries (mem/view (:store eng)) :hostile)))

(defn zombie
  ([x z] (zombie 7 x z))
  ([id x z] {:id id :name "zombie" :kind "hostile" :pos {:x x :y 64 :z z}}))

(def sword [{:name "iron_sword" :count 1}])

(def diamond-armour
  {:head {:name "diamond_helmet" :count 1} :torso {:name "diamond_chestplate" :count 1}
   :legs {:name "diamond_leggings" :count 1} :feet {:name "diamond_boots" :count 1}})

(defn ^:async first-round-seeing
  "As first-round, over a body that sees through perception (the clock moving ms with every call, default 0)."
  ([spec world] (first-round-seeing spec world 0))
  ([spec world ms]
  (let [s (setup-seeing world nil ms)]
    (core/submit! (:eng s) spec {})
    (await (core/tick! (:eng s)))
    s)))

(defn ^:async first-round
  "Submit spec in a world and run one tick; the setup map."
  [spec world]
  (let [s (setup world)]
    (core/submit! (:eng s) spec {})
    (await (core/tick! (:eng s)))
    s))

(defn ^:async first-round-ms
  "As first-round, the clock moving ms with every primitive call, so a call that waits out a gap ends."
  [spec world ms]
  (let [s (setup world ms)]
    (core/submit! (:eng s) spec {})
    (await (core/tick! (:eng s)))
    s))

(defn ^:async flight-round
  "As first-round, over setup-flight."
  [spec world]
  (let [s (setup-flight world)]
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
        (is (fought? (await (flight-round respond {:self {:health 20} :inventory sword :entities [(zombie 3 0)]})))
            "sword and full health: fight")
        (is (fled? (await (flight-round respond {:self {:health 5} :inventory sword :entities [(zombie 3 0)]})))
            "hurt: retreat")
        (is (fled? (await (flight-round respond {:self {:health 20} :inventory [{:name "iron_pickaxe" :count 1}]
                                                :entities [(zombie 3 0)]})))
            "a pickaxe is not a weapon: retreat")
        (is (fought? (await (flight-round respond {:self {:health 20} :inventory sword
                                                  :entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]})))
            "a lone creeper within fuse range with a sword: hit-and-back")
        (is (fled? (await (flight-round respond {:self {:health 20} :inventory sword
                                                :entities [(zombie 7 3 0) (zombie 8 3 1) (zombie 9 3 -1)]})))
            "outnumbered: retreat")
        (is (fought? (await (flight-round respond {:self {:health 20} :inventory sword
                                                  :equipment {:head {:name "diamond_helmet" :count 1} :torso {:name "diamond_chestplate" :count 1}
                                                              :legs {:name "diamond_leggings" :count 1} :feet {:name "diamond_boots" :count 1}}
                                                  :entities [(zombie 7 3 0) (zombie 8 3 1)]})))
            "two are fightable in diamond: their blows cost less than walking away and meeting them again")))))

(defn fist-stance
  "What decide makes of a body at full health with weapon against one mob 2 blocks off (health: what is left of it, nil
  full): fight it, or walk away and meet it again."
  [weapon mob health]
  (let [f (cost/fight-answer {:weapon weapon :mobs [{:name mob :distance 2 :health health}]})]
    (:answer (cost/decide {:health 20 :food 20 :level 0 :carried 0 :respawn {:x 40 :y 64 :z 0}}
                          {:fight f :flee (cost/flee-answer {:food 20 :mobs [{:name mob :distance 2 :follow 35}] :later f})}
                          {:x 0 :y 64 :z 0}))))

(deftest an-unarmed-body-at-full-health-does-not-choose-a-fist-fight-it-can-walk-away-from
  (are [weapon mob health expected] (= expected (fist-stance weapon mob health))
    nil "zombie" nil :flee
    nil "zombie" 2 :fight
    "wooden_sword" "zombie" nil :fight))

(defn spider [x] {:id 5 :name "spider" :kind "hostile" :pos {:x x :y 64 :z 0}})

(deftest respond-with-bare-hands-and-nowhere-to-hide-fights-a-spider-and-never-runs-in-the-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [self [{:health 20 :food 20} {:health 18 :food 6} {:health 6 :food 20}]]
          (let [{:keys [eng] :as s} (await (flight-round respond {:self self :entities [(spider 2)]}))]
            (is (fought? s) (str "the fist is all that is left " self))
            (is (zero? (count (tu/walked-to eng))) (str "no flee step from a mob that outruns it " self))))))))

(deftest respond-with-bare-hands-digs-in-from-a-spider-where-it-can
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dirt (into {} (for [x (range -3 4) z (range -3 4) y (range 58 64)] [(str x "," y "," z) "dirt"]))
              {:keys [p seen]} (await (flight-round respond {:self {:health 20 :food 20} :blocks dirt :entities [(spider 2)]}))]
          (is (= [:pit] (keep :cover (filter #(= :hostile.decided (:kind %)) @seen))) "the pit is the cheapest answer")
          (is (zero? (count (calls p "attack"))) "no fist fight while a pit can be dug")
          (is (pos? (count (calls p "dig"))) "the flight begins with that pit"))))))

(deftest hits-taken-while-fleeing-are-counted-in-a-window
  (are [hits now before after expected] (= expected (respond-to-hostile/note-hits hits now before after 5000))
    [] 10000 20 18 [10000]
    [8000] 10000 18 16 [8000 10000]
    [2000 8000] 10000 16 16 [8000]
    [] 10000 16 18 []))

(deftest respond-fights-a-hostile-beyond-melee-by-walking-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (flight-round respond {:inventory sword :entities [(zombie 7 0)]}))]
          (is (fought? s) "the hostile at 7 blocks is walked up to and hit"))))))

(deftest respond-keeps-a-fight-while-what-is-left-of-the-mob-is-worth-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-flight {:inventory sword :entities [(zombie 100 3 0)]})
              health-after [16 1]
              n (atom 0)]
          (.override (.-world p) "attack"
                     (fn [token a impl]
                       (-> (impl token a)
                           (.then (fn [r] (fake/swap-self! p assoc :health (get health-after @n 6)) (swap! n inc) r)))))
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack")))
              "health 16, the zombie at 15: 2.8 expected damage costs less than walking away: a second swing; health 1 then: none")
          (is (pos? (count (tu/walked-to eng))) "it retreats instead"))))))

(deftest respond-writes-one-hostile-entry-per-encounter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (flight-round respond {:inventory sword :entities [(zombie 100 3 0)]}))]
          (is (= [{:mob "zombie" :pos {:x 3 :y 64 :z 0} :decision :fight}] (hostile-entries eng))
              "one entry for the whole fight, every swing in one round")
          (is (= [] (:list (core/state eng))))
          (is (= {:cap 50 :ttl 3600000} (mem/policy (mem/view (:store eng)) :hostile))))))))

(defn decided [seen] (mapv #(select-keys % [:mob :decision :pos :direction :band]) (filter #(= :hostile.decided (:kind %)) @seen)))

(deftest respond-says-once-what-it-answers-and-how-it-knows-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (flight-round respond {:inventory sword :entities [(zombie 100 3 0)]}))]
          (is (= [{:mob "zombie" :decision :fight :pos {:x 3 :y 64 :z 0}}] (decided seen))
              "a seen mob: its place, one event for the whole encounter"))
        (let [{:keys [eng seen]} (setup-seeing {:inventory sword :entities [(zombie 101 0.5 -5.5)]} [0 0])]
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (is (= [{:mob "zombie" :decision :fight :direction :north :band :near}] (decided seen))
              "a heard-only mob: what the sound tells, never its place"))))))

;; ------------------------------------------------------------- hostile attack

(deftest a-flee-after-a-fight-for-the-same-mob-says-what-changed
  (let [prev {:mob "zombie" :decision :fight :health 20}
        now {:mob "zombie" :decision :flee :health 6}]
    (is (= "; earlier fight at 20 hp, now 6 hp" (respond-to-hostile/changed-text prev now)))
    (is (nil? (respond-to-hostile/changed-text prev (assoc now :decision :fight))) "same decision: nothing changed")
    (is (nil? (respond-to-hostile/changed-text prev (assoc now :mob "spider"))) "another mob")
    (is (nil? (respond-to-hostile/changed-text nil now)))))

(defn waited-between-swings
  "The first ms waited (act :wait) after each swing but the last: what is left of the gap (the fake clock moves with calls
  and paces, not with waits)."
  [p]
  (let [calls (vec (.-calls (.-world p)))
        idx (vec (keep-indexed (fn [i c] (when (= "attack" (.-name c)) i)) calls))]
    (mapv (fn [a b] (some #(when (= "wait" (.-name %)) (.. % -args -ms)) (subvec calls a b)))
          idx (rest idx))))

(def fight '(jobs.combat.attack {:targets :hostiles :radius 4 :ranged-radius 16}))

(deftest hostile-attack-equips-attacks-and-finishes-when-the-mob-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (first-round-ms fight {:inventory [{:name "wooden_sword" :count 1} {:name "iron_axe" :count 1}
                                                                        {:name "iron_pickaxe" :count 1}]
                                                            :entities [(zombie 7 3 0)]} 20))]
          (is (= ["iron_axe"] (mapv #(.-item (.-args %)) (calls p "equip"))) "the best weapon, not the pickaxe")
          (is (= [7 7 7 7] (mapv #(.-id (.-args %)) (calls p "attack"))) "four hits kill the 20 health zombie")
          (is (= [] (:list (core/state eng))) "done in one call"))))))

(deftest hostile-attack-never-swings-at-a-calm-enderman
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[angry want] [[false [7 7 7 7]] [true [8]]]]
          (let [enderman {:id 8 :name "enderman" :kind "hostile" :angry angry :pos {:x 2 :y 64 :z 0}}
                {:keys [p]} (await (first-round-ms fight {:inventory sword :entities [enderman (zombie 7 3 0)]} 20))]
            (is (= want (take (count want) (mapv #(.-id (.-args %)) (calls p "attack"))))
                (str "angry " angry ": the nearer enderman is hit only when already angry"))))))))

(deftest hostile-attack-waits-out-the-held-weapons-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round-ms fight {:inventory [{:name "iron_axe" :count 1}] :entities [(zombie 7 3 0)]} 10))]
          (is (seq (waited-between-swings p)))
          (is (every? #(<= (- 1072 pace/pace-ms) % 1112) (waited-between-swings p)) "the iron axe's 1112 ms, so every hit lands at full strength"))))))

(deftest hostile-attack-looks-round-while-the-cooldown-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (first-round-seeing '(jobs.combat.attack {:targets :hostiles :radius 4 :ranged-radius 16 :settings {:jobs.lib.combat/attack-gap-ms 3000}}) {:inventory sword :entities [(zombie 7 3 0)]} 20))]
          (is (seq (mem/entries (mem/view (:store eng)) :watched)) "the call waits out the gap and looks round"))))))

(deftest hostile-attack-walks-up-to-a-mob-beyond-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (first-round-ms '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 6}) {:inventory sword :entities [(zombie 5 0)]} 20))]
          (is (pos? (count (tu/walked-to eng))))
          (is (every? #(<= % 15) (map #(.-timeoutS (.-args %)) (calls p "steer"))) "a short walk: it aims again at the mob")
          (is (pos? (count (calls p "attack")))))))))

(deftest hostile-attack-does-not-walk-down-a-ten-deep-hole-after-a-mob-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pit-mob {:id 7 :name "zombie" :kind "hostile" :pos {:x 5.5 :y 54 :z 0.5}}
              {:keys [eng]} (await (first-round-ms '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 16}) {:inventory sword :blocks {"5,53,0" "stone"} :entities [pit-mob]} 20))]
          (is (every? #(< 60 (:y %)) (tu/walked-to eng))
              "no walk was aimed at the pit's floor"))))))

(deftest hostile-attack-does-not-drop-into-a-pit-whose-fall-and-fight-it-cannot-afford
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pit-mob {:id 7 :name "skeleton" :kind "hostile" :pos {:x 7.5 :y 55 :z 0.5}}
              pit (into {} (mapcat (fn [x] [[(str x ",63,0") "air"] [(str x ",54,0") "stone"]]) [5 6 7]))
              {:keys [p eng]} (await (first-round-ms '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 16})
                                                     {:self {:pos {:x 4.5 :y 64 :z 0.5}} :blocks pit :entities [pit-mob]} 20))]
          (is (seq (tu/walked-to eng)) "the fight walked toward the mob")
          (is (empty? (calls p "steer"))
              "a 9-block fall (6 hp, within the walk's budget of 7) into a fist fight with a skeleton is more than the body may spend: no step down"))))))

(deftest hostile-attack-with-hold-waits-for-a-mob-beyond-reach-and-hits-it-when-it-comes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (first-round-ms '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 6 :hold true}) {:inventory sword :entities [(zombie 5 0)]} 20))]
          (is (empty? (tu/walked-to eng)) "it does not advance on the mob")
          (is (empty? (calls p "attack")) "and does not swing at one it cannot reach"))))))

(deftest hostile-attack-waits-for-a-mob-on-a-dark-cell-and-walks-to-one-in-the-light
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[light walks?] [[[0 0] false] [[15 0] true]]]
          (let [s (setup-seeing {:inventory sword :entities [(zombie 5 0)]} light 20)]
            (core/submit! (:eng s) '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 6}) {})
            (await (core/tick! (:eng s)))
            (is (= walks? (boolean (seq (tu/walked-to (:eng s))))) (str "light " light))))))))

(deftest the-fight-holds-position-in-the-dark-with-more-than-one-hostile-known
  (are [world held?] (= held? (respond-to-hostile/hold? {:primitives (tu/with-light-data (tu/fake world)) :view (fn [] {:data {} :now 0}) :args {}}))
    {:time 14000 :entities [(zombie 7 5 0) (zombie 8 6 3)]} true
    {:time 14000 :entities [(zombie 7 5 0)]} false
    {:time 1000 :entities [(zombie 7 5 0) (zombie 8 6 3)]} false))

(def closed-room
  "A 3x3 room around the body, walls to y 69, roof at y 70, floor at y 63."
  (into {} (for [x (range -2 3) z (range -2 3) y (range 63 71)
                 :let [wall? (or (= 2 (Math/abs x)) (= 2 (Math/abs z)))]
                 :when (or wall? (= y 63) (= y 70))]
             [(str x "," y "," z) "stone"])))

(deftest a-body-shut-in-a-room-in-the-dark-holds-against-one-hostile-unless-stay-inside-is-off
  (are [world overrides held?] (= held? (settings/with-settings overrides #(respond-to-hostile/hold? {:primitives (tu/with-light-data (tu/fake world)) :view (fn [] {:data {} :now 0}) :args {}})))
    {:time 14000 :blocks closed-room :entities [(zombie 7 5 0)]} {} true
    {:time 14000 :blocks closed-room :entities [(zombie 7 5 0)]} {::respond-to-hostile/stay-inside false} false
    {:time 1000 :blocks closed-room :entities [(zombie 7 5 0)]} {} false
    {:time 14000 :entities [(zombie 7 5 0)]} {} false))

(def breached-room
  "The closed room with a hole in its east wall and a low roof over the body."
  (into (dissoc closed-room "2,64,0" "2,65,0")
        (for [x (range -1 2) z (range -1 2)] [(str x ",67," z) "stone"])))

(def roofed-passage
  "The breached room with a hole in its west wall too: roofed, but walled two ways only, so no closed quarters (a groan counts)."
  (dissoc breached-room "-2,64,0" "-2,65,0"))

(deftest a-roofed-body-does-not-flee-for-a-mob-it-only-hears
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [heard (zombie 7 0.5 -5.5)
              stay '(jobs.survival.respond-to-hostile {:settings {:jobs.survival.respond-to-hostile/stay-inside false}})]
          (doseq [[world spec decision held? why]
                  [[{:time 14000 :entities [heard]} respond :flee false "no roof: it flees"]
                   [{:time 14000 :blocks roofed-passage :entities [heard]} respond :fight true "roofed, heard only: it stays"]
                   [{:time 14000 :blocks roofed-passage :entities [heard]} stay :flee false "stay-inside off: it flees"]
                   [{:time 1000 :blocks roofed-passage :entities [heard]} respond :fight true "day: it stays too (the roof, not the clock)"]]]
            (let [{:keys [eng p seen]} (setup-seeing (assoc world :self {:health 5} :inventory sword :pitch -80) nil 1000)]
              (perception/pass! (aget p "perception"))
              (swap! (fake/state p) assoc :pitch 0)
              (core/submit! eng spec {})
              (await (core/tick! eng))
              (is (= decision (:decision (first (hostile-entries eng)))) why)
              (is (= held? (boolean (:text (first (filter #(= :hostile.sealed-hold (:kind %)) @seen))))) (str why ": sealed-hold event with its reason"))
              (is (= held? (boolean (seq (filterv :mute-s @seen)))) (str why ": a held body with nothing to fight ends done, leaving the heard mob (not no-response)")))))))))

(deftest a-roofed-body-hit-lately-does-not-stay-for-a-mob-it-only-hears
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[hurt held? why] [[{:amount 3} false "hit by a mob lately: the roof is no reason to stay"]
                                  [{:amount 1 :cause "starvation"} true "starving is no hit: it stays under the roof"]]]
          (let [{:keys [eng p seen]} (setup-seeing (assoc {:time 14000 :blocks roofed-passage :entities [(zombie 7 0.5 -5.5)]} :self {:health 20} :inventory sword :pitch -80) nil 1000)]
            (perception/pass! (aget p "perception"))
            (swap! (fake/state p) assoc :pitch 0)
            (mem/write! (:store eng) :hurt hurt)
            (core/submit! eng respond {})
            (await (core/tick! eng))
            (is (= held? (boolean (seq (filter #(= :hostile.sealed-hold (:kind %)) @seen)))) why)
            (is (not-any? #(= :no-response (:reason %)) @seen) "the reflex does not end as if nothing could be done")))))))

(deftest a-body-looks-toward-a-heard-mob-before-leaving-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-seeing (assoc {:time 14000 :blocks roofed-passage :entities [(zombie 7 0.5 -5.5)]} :self {:health 20} :inventory sword :pitch -80) nil 1000)]
          (perception/pass! (aget p "perception"))
          (swap! (fake/state p) assoc :pitch 0)
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (is (seq (mem/entries (mem/view (:store eng)) :heard-mute)) "muted")
          (is (seq (calls p "look")) "after it turned to look"))))))

(defn looked-round!
  "One perception pass of p's body facing each of the four sides, up and down: the walls, roof and floor round it seen, as
  a look-around leaves them."
  [p]
  (doseq [[yaw pitch] [[0 0] [90 0] [180 0] [270 0] [0 -80] [0 80]]]
    (swap! (fake/state p) assoc :yaw yaw :pitch pitch)
    (perception/pass! (aget p "perception")))
  (swap! (fake/state p) assoc :yaw 0 :pitch 0))

(def wide-room
  "A 7x7 room around the body (interior -3..3), walls to y 66, roof at y 67, floor at y 63: too wide for closed quarters."
  (into {} (for [x (range -4 5) z (range -4 5) y (range 63 68)
                 :let [wall? (or (= 4 (Math/abs x)) (= 4 (Math/abs z)))]
                 :when (or wall? (= y 63) (= y 67))]
             [(str x "," y "," z) "stone"])))

(deftest a-zombie-heard-behind-the-wall-of-a-roofed-room-is-no-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [time [1000 14000]]
          (let [{:keys [p]} (setup-seeing {:time time :blocks wide-room :self {:health 20} :entities [(zombie 8 0)]} nil 1000)]
            (looked-round! p)
            (is (not (danger-q/danger-near? p 8 16 {})) (str "heard through the wall at time " time))))))))

(deftest a-roofed-body-stays-in-for-a-phantom-it-sees-but-not-for-a-seen-zombie
  (are [entity stays?] (= stays? (let [p (tu/fake {:time 14000 :blocks breached-room :entities [entity]})]
                                   (respond-to-hostile/stay-under-roof? {:primitives p :args {}}
                                                                        (danger-q/known-hostiles p 16 {}))))
    {:id 9 :name "phantom" :kind "hostile" :visible true :pos {:x 4 :y 72 :z 0}} true
    (assoc (zombie 4 0) :visible true) false))

(deftest hostile-attack-chases-in-legs-that-never-escalate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls* (atom [])
              _ (await (tu/with-near-spy calls* (fn ^:async b [] (await (first-round-ms '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 6}) {:inventory sword :entities [(zombie 5 0)]} 20)))))
              opts (map #(nth % 2) @calls*)]
          (is (seq opts))
          (is (every? #(and (= #{:walk :clear-seal} (:ways %)) (= 10 (:leg-s %)) (false? (:look-round %)) (nil? (:dangers %))) opts)
              "a moving mob is chased in legs, never dug or pillared toward, its dangers priced")
          (is (every? #(< 0 (:max-damage %) 7) opts) "a sword fight with a zombie leaves part of the budget of 7 for falls"))))))

(deftest hostile-attack-is-already-done-when-nothing-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory sword :entities [(zombie 6 0)]})]
          (core/submit! eng fight {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "6 blocks is beyond range 4: already done")
          (is (empty? (concat (calls p "attack") (calls p "equip"))) "no swing, nothing equipped"))))))

(deftest a-declined-fight-ending-carries-over-into-the-next-calls-memory
  (let [m {:killed [3] :given-up {7 :unreachable} :names {7 "zombie"} :hits {7 2} :health {7 9}
           :fails {7 2 8 1} :fail-health {7 12 8 12} :quiet {7 3}}
        data (assoc (select-keys m [:killed :given-up :names]) :struck (hostile-fight/struck m))]
    (is (= {7 {:name "zombie" :hits 2 :health 9 :blocked 2 :blocked-health 12 :quiet 3} 8 {:name nil :hits 0 :blocked 1 :blocked-health 12}}
           (:struck data)))
    (is (= (assoc m :hits {7 2 8 0}) (hostile-fight/carry-over data)))
    (is (nil? (hostile-fight/carry-over {:reason :no-target})) "a refused start has nothing to carry")))

(deftest hostile-attack-goes-on-from-the-prior-calls-give-ups
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round-ms '(jobs.combat.attack {:targets :hostiles :radius 12 :prior {:given-up {7 :unreachable} :struck {7 {:name "zombie" :hits 0 :blocked 2}}}})
                                                 {:inventory sword :entities [(zombie 7 3 0)]} 20))]
          (is (empty? (calls p "attack")) "the zombie the earlier call gave up on is not fought again"))))))

(deftest hostile-attack-leaves-a-mob-beyond-its-leash-from-the-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory sword :entities [(zombie 8 0)]})]
          (core/submit! eng '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 12 :settings {:jobs.lib.hostile-fight/leash 5}}) {})
          (is (nil? (core/tick! eng)) "8 blocks from the start is past the leash of 5: not chased, the job declines")
          (is (zero? (count (calls p "attack")))))
        (let [{:keys [p]} (await (first-round-ms '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 12 :settings {:jobs.lib.hostile-fight/leash 10}}) {:inventory sword :entities [(zombie 8 0)]} 20))]
          (is (pos? (count (calls p "attack"))) "inside the leash it is fought"))))))

(deftest hostile-attack-keeps-its-leash-anchor-when-resumed-after-a-hurt-yield
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:inventory sword :entities [(zombie 2 0)]} 2000)
              hurt (atom false)]
          (.override (.-world p) "attack"
                     (fn [token a impl]
                       (when-not @hurt
                         (reset! hurt true)
                         (swap! (fake/state p) update-in [:self :health] - 1))
                       (impl token a)))
          (core/submit! eng '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 8 :settings {:jobs.lib.hostile-fight/leash 5}}) {})
          (await (core/tick! eng))
          (swap! (fake/state p) assoc-in [:self :pos] [30.5 64 0])
          (set-entities! p [(zombie 33 0)])
          (dotimes [_ 3]
            (swap! clock + 3000)
            (await (core/tick! eng)))
          (is (= 1 (count (calls p "attack"))) "the resumed fight still measures its leash from the first round's start: a mob 30 blocks on is not chased"))))))

(deftest hostile-attack-is-already-done-for-a-hidden-ranged-mob
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory sword :entities [{:id 7 :name "skeleton" :kind "hostile" :visible false :pos {:x 10 :y 64 :z 0}}]})]
          (core/submit! eng '(jobs.combat.attack {:targets :hostiles :radius 4 :ranged-radius 16}) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "no line of fire: not a target, nothing in range")
          (is (empty? (concat (calls p "attack") (calls p "equip"))) "no swing, nothing equipped"))))))

(defn invulnerable-zombie [] (assoc (zombie 7 3 0) :invulnerable true))

(deftest hostile-attack-counts-no-hit-without-damage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (await (first-round-ms fight {:inventory sword :entities [(invulnerable-zombie)]} 20))]
          (is (nil? (:struck (core/job-memory eng "j1"))) "no damage seen: no hit recorded"))))))

(deftest hostile-attack-gives-up-on-a-mob-it-cannot-damage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (first-round-ms fight {:inventory sword :entities [(invulnerable-zombie)]} 20))]
          (is (= 16 (count (calls p "attack"))) "sixteen swings without damage, then no more")
          (is (empty? (filter #(= "fight_no_damage" (name (:kind %))) @seen)) "the ending carries the reason")
          (is (re-find #"no-damage" (str (:text (first (filter #(= :stopped (:kind %)) @seen)))))))))))

(deftest respond-flees-from-a-mob-that-takes-no-damage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (flight-round respond {:inventory sword :entities [(invulnerable-zombie)]}))]
          (is (= 16 (count (calls p "attack"))) "the fight stops after sixteen useless swings")
          (is (pos? (count (tu/walked-to eng))) "and the body retreats"))))))

(deftest respond-does-not-stop-no-response-while-the-fight-costs-health
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:health 20} :inventory sword :entities [(zombie 7 1 0)]})
              hits (atom 0)
              hurting {:round (fn ^:async hurt-yield [_c]
                                (swap! hits inc)
                                (fake/swap-self! p update :health dec)
                                (when (= 6 @hits) (swap! (fake/state p) assoc :entities []))
                                :continue)}
              eng (assoc-in eng [:jobs 'jobs.combat.attack] hurting)]
          (core/submit! eng respond {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (<= 6 @hits) "the fight went on past three hurt-yields")
          (is (not-any? #(= :no-response (:reason %)) @seen) "a fight that costs health is a response"))))))

(deftest respond-does-not-stop-no-response-while-the-fight-lands-hits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:health 20} :inventory sword :entities [(zombie 7 1 0)]})
              hits (atom 0)
              pausing {:round (fn ^:async pause [c]
                                (swap! hits inc)
                                (ctx/update-mem! c update-in [:hits 7] (fnil inc 0))
                                (when (= 6 @hits) (swap! (fake/state p) assoc :entities []))
                                :continue)}
              eng (assoc-in eng [:jobs 'jobs.combat.attack] pausing)]
          (core/submit! eng respond {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (<= 6 @hits) "the fight went on past three pauses that each landed a hit")
          (is (not-any? #(= :no-response (:reason %)) @seen) "a landed hit is a response"))))))

(deftest respond-looks-again-before-ending-a-fight-that-yielded-when-no-danger-is-seen-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 20} :inventory sword :entities [(zombie 7 1 0)]})
              calls (atom 0)
              blink {:round (fn ^:async blink [c]
                              (let [n (swap! calls inc)
                                    es (:entities @(fake/state p))]
                                (swap! (fake/state p) assoc :entities [])
                                (when (= 1 n) (js/setImmediate #(swap! (fake/state p) assoc :entities es)))
                                (if (= 1 n) :continue (ctx/end c :done nil))))}
              eng (assoc-in eng [:jobs 'jobs.combat.attack] blink)]
          (core/submit! eng respond {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (<= 2 @calls) "one empty check after a :continue does not end the round"))))))

(deftest a-fight-resumed-after-a-flee-measures-its-leash-from-where-the-body-now-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:health 20} :entities [(zombie 7 1 0)]})
              seen (atom :unset)
              runner {:defaults {:checks {:safe? '(always) :fit? '(always)}}
                      :round (fn ^:async r [c]
                               (ctx/update-mem! c assoc :last-run :flee :children {:fight {:start {:x 0 :y 64 :z 0}}})
                               (await (respond-to-hostile/run-child c :fight false))
                               (ctx/end c :done nil))}
              eng (-> eng
                      (assoc-in [:jobs 'runner] runner)
                      (assoc-in [:jobs 'jobs.combat.attack] {:round (fn ^:async look [c] (reset! seen (:start (ctx/mem c))) :continue)}))]
          (core/submit! eng '(runner) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (nil? @seen) "the start from before the flee is dropped"))))))

(deftest the-round-signature-changes-when-the-running-fight-lands-a-hit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:health 20} :entities [(zombie 7 1 0)]})
              out (atom nil)
              runner {:defaults {:checks {:safe? '(always) :fit? '(always)}}
                      :round (fn ^:async r [c]
                               (let [before (respond-to-hostile/signature c [])]
                                 (ctx/update-mem! c assoc-in [:children :fight :hits 7] 1)
                                 (reset! out [before (respond-to-hostile/signature c [])]))
                               (ctx/end c :done nil))}
              eng (assoc-in eng [:jobs 'runner] runner)]
          (core/submit! eng '(runner) {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (apply not= @out) "a hit the attack child counted shows in the signature"))))))

(deftest a-refired-reflex-remembers-the-hits-the-fight-landed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 20} :inventory [{:name "stone_sword" :count 1}] :entities [(zombie 7 1 0)]})
              fights (atom 0)
              hitter {:round (fn ^:async hit [c]
                               (swap! fights inc)
                               (ctx/update-mem! c merge {:hits {7 3} :health {7 2}})
                               (swap! (fake/state p) assoc :entities [])
                               :continue)}
              eng (assoc-in eng [:jobs 'jobs.combat.attack] hitter)]
          (core/submit! eng respond {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= 1 @fights))
          (fake/swap-self! p assoc :health 10)
          (fake/add-entity! p (zombie 7 1 0))
          (core/submit! eng respond {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= 2 @fights) "the second encounter fights the nearly dead zombie, no flight"))))))

(deftest a-refired-reflex-remembers-the-hits-of-a-fight-that-ended-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:health 20} :inventory [{:name "stone_sword" :count 1}] :entities [(zombie 7 1 0)]})
              fights (atom 0)
              decliner {:round (fn ^:async decline [c]
                                 (swap! fights inc)
                                 (swap! (fake/state p) assoc :entities [])
                                 (ctx/end c :nothing-possible :blocked {:struck {7 {:name "zombie" :hits 3 :health 2}}}))}
              eng (assoc-in eng [:jobs 'jobs.combat.attack] decliner)]
          (core/submit! eng respond {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= 1 @fights))
          (fake/swap-self! p assoc :health 10)
          (fake/add-entity! p (zombie 7 1 0))
          (core/submit! eng respond {})
          (dotimes [_ 2] (await (core/tick! eng)))
          (is (= 2 @fights) "the second encounter fights the nearly dead zombie, no flight"))))))

;; ----------------------------------------------------------------- retreat

(def retreat '(test.flight))

(defn last-move
  "The x and z of the latest walk's target."
  [eng]
  (select-keys (peek (tu/walked-to eng)) [:x :z]))

(defn first-move
  "The x and z of the flight's first walk's target."
  [eng]
  (select-keys (first (tu/walked-to eng)) [:x :z]))

(defn remember! [eng kind data] (mem/write! (:store eng) kind data mem/place-policy))

(deftest retreat-leans-towards-a-bed-or-home-unless-it-is-through-the-hostile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup-flight {:entities [(zombie 0 5)]})]
          (remember! eng :bed {:pos {:x -20 :y 64 :z -20}})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (let [[x _ z :as end] (:picked (first (filter #(= :retreat.search (:kind %)) @seen)))]
            (is (and (<= (js/Math.abs (+ x 20)) 1) (<= (js/Math.abs (+ z 20)) 1)) (str "the flight runs to the bed " (pr-str end))))
          (is (< (:z (first-move eng)) -1) "away from the hostile"))
        (let [{:keys [eng p]} (setup-flight {:entities [(zombie 0 5)]})]
          (remember! eng :home {:pos {:x 0 :y 64 :z 20}})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= {:x 0 :z -6} (first-move eng)) "a home beyond the hostile is ignored"))))))

(deftest retreat-flees-from-the-weighted-set-of-threats-not-the-nearest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup-flight {:entities [(zombie 1 0 4) (zombie 2 3 -3) (zombie 3 -3 -3)]})]
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (> (:z (first-move eng)) 1) "mobs on three sides: it leaves through the one open side, not away from the nearest"))))))

(deftest retreat-ignores-a-home-beyond-the-home-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup-flight {:entities [(zombie 0 5)]})]
          (remember! eng :home {:pos {:x -2100 :y 64 :z -2000}})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= {:x 0 :z -6} (first-move eng)) "a home 2000 blocks away does not bend the flight"))))))

(deftest retreat-avoids-hazard-entries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-flight {:entities [(zombie 5 0)]})]
          (mem/write! (:store eng) :hazard {:kind :lava :pos {:x -6 :y 64 :z 0}} {:cap 50 :ttl :forever})
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (let [m (first-move eng)]
            (is (<= 2.5 (js/Math.hypot (+ (:x m) 6) (:z m))) (str "the step keeps clear of the lava " (pr-str m)))
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
        (let [{:keys [eng p seen]} (setup-flight {:blocks stone-box :entities [(zombie 6 0)]})]
          (.override (.-world p) "steer" (fn [_ _ _] (js/Promise.resolve #js {:status "timeout" :pose #js {}})))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
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
        (let [{:keys [eng p]} (setup-flight {:entities [(zombie 2 0)]})]
          (stuck-walks! p)
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= 4 (count (calls p "attack"))) "unarmed and nothing to seal with: it hits back until the zombie (20, 5 a hit) dies")
          (is (= [] (:list (core/state eng))) "the zombie is gone: the flight is over"))))))

;; ----------------------------------------------------------------- trigger

(def when-hostile-near (:when (:hostile-near triggers/all)))

(defn holds? [world args]
  (when-hostile-near (tu/fake-on-floor world) {:data mem/empty-data :now 0} args))

(defn seeing-the-void
  "p whose body also sees the void beyond its floor (no ground there), not only the floor: a wall across the whole floor
  then leaves no unseen way round."
  [p]
  (tu/set-sensed! p #(.blockAt p %))
  p)

(defn holds-seeing-the-void? [world args]
  (when-hostile-near (seeing-the-void (tu/fake-on-floor world)) {:data mem/empty-data :now 0} args))

(deftest hostile-near-needs-a-hostile-mob-within-radius
  (is (holds? {:entities [(zombie 5 0)]} {:radius 8}))
  (is (not (holds? {:entities [(zombie 5 0)]} {:radius 4})) "beyond the radius")
  (is (not (holds? {:entities [{:id 1 :name "Steve" :kind "player" :pos {:x 2 :y 64 :z 0}}]} {:radius 8})) "a player")
  (is (not (holds? {:entities [{:id 2 :name "cow" :kind "passive" :pos {:x 2 :y 64 :z 0}}]} {:radius 8})) "a passive"))

(defn holds-after-hurt?
  "Whether the trigger holds at now 2000 with one :hurt entry (written at t, data) and no mob."
  [world t data]
  (boolean (when-hostile-near (tu/fake-on-floor world)
                              {:data (mem/add-entry mem/empty-data :hurt {:t t :data data} {:cap 50 :ttl 60000}) :now 2000} {})))

(deftest hostile-near-holds-for-a-shot-from-an-unseen-shooter-with-a-line-of-fire
  (let [arrow {:damageType "arrow" :dir {:x 1 :z 0}}
        wall (tu/box 3 64 -2 3 66 2 "stone")]
    (is (holds-after-hurt? {} 1500 arrow) "shot from the east in the open")
    (is (not (holds-after-hurt? {:blocks wall} 1500 arrow)) "a wall between the body and the shooter's spot stops the arrows")
    (is (holds-after-hurt? {:blocks (assoc wall "0,63,0" "air")} 1500 arrow)
        "the floor open under the body: a hit shows the way, not the height, so a shooter down the shaft has a line of fire")
    (is (not (holds-after-hurt? {} 1500 {:damageType "arrow"})) "no way known: no spot")
    (is (not (holds-after-hurt? {} 1500 {:attacker {:name "zombie"} :dir {:x 1 :z 0}})) "a melee hit")
    (is (not (holds-after-hurt? {} -4000 arrow)) "the shot is older than the shot window")))

(def thin-stone-box
  "A hollow stone box one block thick, x 5..9 and z -2..2 (feet and head inside): a zombie heard in it has its rough spot
  (4 blocks toward the sound) on open floor outside it."
  (apply dissoc (tu/box 5 63 -2 9 66 2 "stone") (keys (tu/box 6 64 -1 8 65 1 "stone"))))

(defn tunnel-east
  "Rock round a 1x2 tunnel along x (x -5..8, feet y 64) with the body in it; plugged at x 6 when plugged?."
  [plugged?]
  (cond-> (apply dissoc (tu/box -6 63 -2 9 66 2 "stone") (keys (tu/box -5 64 0 8 65 0 "air")))
    plugged? (merge (tu/box 6 64 0 6 65 0 "stone"))))

(deftest hostile-near-counts-a-groan-in-the-open-and-looks-in-closed-quarters
  (let [holds-seeing #(boolean (when-hostile-near (doto (:p (setup-seeing (assoc % :self {:pos [0.5 64 0.5]}) nil)) looked-round!)
                                                  {:data mem/empty-data :now 0} {}))]
    (is (true? (holds-seeing {:blocks thin-stone-box :entities [(zombie 7.5 0.5)]}))
        "in the open: a zombie heard in a box counts as seen (its rough spot has a way)")
    (is (false? (holds-seeing {:blocks (tunnel-east true) :entities [(zombie 7.5 0.5)]}))
        "in a tunnel, the zombie behind a plug: the look sees nothing")
    (is (true? (holds-seeing {:blocks (tunnel-east false) :entities [(zombie 7.5 0.5)]}))
        "in a tunnel, the zombie in it: seen, with a way")))

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
  "How many walks went to pos's x and z: go-near! walks (their :moved entries) and moveTo calls."
  [{:keys [p eng]} pos]
  (+ (count (filter #(and (= (:x pos) (:x %)) (= (:z pos) (:z %))) (tu/walked-to eng)))
     (count (filter #(let [a (.. % -args -pos)] (and (= (:x pos) (.-x a)) (= (:z pos) (.-z a)))) (calls p "moveTo")))))

(deftest respond-retreats-from-a-hostile-it-cannot-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup-flight {:inventory sword :entities [(zombie 6 0)] :unreachable ["6,64,0"]})]
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (is (= 3 (count (filter #(and (= :goal-not-standable (get-in % [:ending :cause :why])) (nil? (get-in % [:ending :because]))) @seen))) "three go-to walks towards the zombie gave up, never more (the flight's own leg ends :search-spent by the call's seconds)")
          (is (= 3 (walks-to {:p p :eng eng} {:x 6 :z 0})) "one blocked walk per go-to call: any y re-aim comes before the walk")
          (is (< (:x (last-move eng)) 0) "then it walks away instead")
          (is (zero? (count (calls p "attack")))))))))

;; ------------------------------------------------------- ranged mobs count further out

(defn skeleton [id x z] {:id id :name "skeleton" :kind "hostile" :pos {:x x :y 64 :z z}})

(def wood-sword [{:name "wooden_sword" :count 1}])

;; Wicken j3766: at 13 hp with a wooden sword the fight walked to a seen skeleton, beside a zombie heard further on
(deftest a-fight-counts-the-mobs-heard-by-the-way-to-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (tu/box -3 64 8 3 66 9 "stone")]
          (doseq [[entities decision why]
                  [[[(skeleton 1 0.5 4.5)] :fight "a lone skeleton is fought"]
                   [[(skeleton 1 0.5 4.5) (zombie 7 0.5 12.5)] :flee "a zombie heard behind the wall past it joins that fight: too dear at 13 hp"]]]
            (let [{:keys [eng seen]} (setup-seeing {:self {:health 13} :inventory wood-sword :blocks wall :entities entities} [15 15] 1000)]
              (core/submit! eng respond {})
              (await (core/tick! eng))
              (is (= [decision] (mapv :decision (decided seen))) why))))))))

(deftest blows-felt-lately-price-the-fight-at-their-rate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[hurts decision why]
                [[[] :fight "unhurt: a lone zombie costs less than walking away"]
                 [[{:amount 2.5 :attacker {:name "zombie"}} {:amount 2.5 :attacker {:name "zombie"}}] :flee "5 hp felt within a second: the fight's 3 s would cost 15 hp"]]]
          (let [{:keys [eng seen]} (setup-flight {:self {:health 20} :inventory wood-sword :entities [(zombie 1 0.5 2.5)]})]
            (doseq [h hurts] (mem/write! (:store eng) :hurt h))
            (core/submit! eng respond {})
            (await (core/tick! eng))
            (is (= [decision] (mapv :decision (decided seen))) why)))))))

;; Bramble #39: the fight walked to a skeleton with a creeper standing by it, beyond the trigger's melee radius; one blast did 17
(deftest a-creeper-standing-by-the-mob-the-fight-would-walk-to-makes-it-a-flight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [skel (skeleton 1 0.5 10)
              creeper {:id 20 :name "creeper" :kind "hostile" :pos {:x 2 :y 64 :z 10.5}}
              decisions (fn ^:async f [es] (mapv (juxt :mob :decision) (decided (:seen (await (flight-round respond {:inventory sword :entities es}))))))]
          (is (= [["skeleton" :fight]] (await (decisions [skel]))) "a lone skeleton is fought")
          (is (= [["creeper" :flee]] (await (decisions [skel creeper]))) "with a creeper beside it the body flees and says why")
          (let [{:keys [eng seen]} (setup-seeing {:inventory sword :entities [skel creeper]} [15 15] 1000)]
            (core/submit! eng respond {})
            (await (core/tick! eng))
            (is (= [["creeper" :flee]] (mapv (juxt :mob :decision) (decided seen))) "seen through perception (both in view, south) too")))))))

(deftest a-creeper-that-reaches-the-way-before-the-fight-ends-makes-it-a-flight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [skel (skeleton 1 0.5 10)
              creeper {:id 20 :name "creeper" :kind "hostile" :pos {:x 7 :y 64 :z 5}}
              decisions (fn ^:async f [es] (mapv (juxt :mob :decision) (decided (:seen (await (flight-round respond {:inventory sword :entities es}))))))]
          (is (= [["skeleton" :fight]] (await (decisions [skel]))) "a lone skeleton is fought")
          (is (= [["creeper" :flee]] (await (decisions [skel creeper])))
              "a creeper 8 off the skeleton and the body, beyond both the trigger's radius and its blast, comes to the fight in 2 s: flee"))))))

(deftest a-creeper-blows-where-the-fight-is-so-one-by-the-mob-fought-costs-more
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [fight-price (fn ^:async f [creeper-x]
                            (let [{:keys [seen]} (await (flight-round respond {:self {:health 20} :inventory sword
                                                                               :entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x creeper-x :y 64 :z 0}} (zombie 9 4 1)]}))]
                              (get-in (first (filter #(= :hostile.decided (:kind %)) @seen)) [:prices :fight])))]
          (is (> (await (fight-price 3)) (await (fight-price -3)))
              "both creepers are 3 blocks from the body; the one a block from the zombie blows beside the fight"))))))

(deftest a-creeper-with-company-is-never-swung-at-and-a-lone-one-backed-off-from-is-still-fought
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [creeper {:id 8 :name "creeper" :kind "hostile" :pos {:x 2 :y 64 :z 0}}
              {:keys [p]} (await (first-round-ms fight {:inventory sword :entities [creeper (zombie 7 3 0)]} 20))]
          (is (= [7 7 7 7] (take 4 (mapv #(.-id (.-args %)) (calls p "attack"))))
              "the nearer creeper is left to blow where the fight is; the zombie is the target"))
        (let [{:keys [eng]} (setup {:inventory sword :entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x 6.4 :y 64 :z 0}}]} 20)
              r (await (tu/child-outcome eng 'jobs.combat.attack {:targets :hostiles :radius 16 :ranged-radius 16} 50))]
          (is (= :hold-timeout (:reason r))
              "after a back-off the creeper 6 blocks off is waited for, not declined though a walk away would now be cheaper: the parent weighs, not each step"))))))

;; Card ad8c1e8b: live, the walls/pit/pocket quotes held the decision 2.4 s while a creeper closed in and lit its fuse
(deftest no-cover-is-quoted-with-a-creeper-near
  (with-redefs [shut-in/covers (fn [& _] (throw (js/Error. "quoted")))]
    (is (nil? (respond-to-hostile/cover-answers {} [#js {:name "creeper"} #js {:name "zombie"}] nil)))))

;; Card ad8c1e8b: a run-away call after the swing got going later than the fuse the swing began (10.7 hp lost live)
(deftest a-creeper-hit-is-stepped-back-from-inside-its-fuse-by-one-move-with-no-job-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory sword :entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x 2 :y 64 :z 0}}]} 20)
              at (atom {})
              stamp! (fn [name] (.override (.-world p) name (fn ^:async o [token args impl]
                                                              (swap! at update name #(or % ((:now eng))))
                                                              (await (impl token args)))))]
          (stamp! "attack")
          (stamp! "moveTo")
          (core/submit! eng fight {})
          (await (core/tick! eng))
          (is (< (- (get @at "moveTo") (get @at "attack")) (* 1000 answer/fuse-s)) "the step back begins within the fuse")
          (is (empty? (filter #(= :back (get-in % [:data :slot])) @seen)) "no child job for it"))))))

;; Card c9b2f421: a run's own change from fight to flight counted as a return (each doubles the walk's price)
(deftest a-runs-own-change-to-flight-is-no-return
  (let [entry (fn [t decision run] {:t t :data (cond-> {:mob "zombie" :decision decision} run (assoc :run run))})
        returns (fn [& entries] (hostile-fight/returns {:root "j1" :slots []
                                                        :view (fn [] {:now 1000000 :data {:entries {:hostile (vec entries) :job/j1 [{:t 990000 :data {:run-at 5}}]}}})}
                                                       ["zombie"]))]
    (is (= 0 (returns (entry 990000 :fight 5) (entry 995000 :flee 5))) "this run's fight turned flight")
    (is (= 1 (returns (entry 900000 :flee 3) (entry 905000 :fight 3) (entry 910000 :flee 3) (entry 995000 :flee 5))) "an earlier run that fled twice is one return")
    (is (= 2 (returns (entry 900000 :flee nil) (entry 905000 :flee nil))) "entries without a run count each")))

;; Card c9b2f421: the blows felt in the fight priced the walk once it began, so the run flipped back to the fight
(deftest blows-felt-before-a-flight-do-not-price-its-walk
  (let [hurt (fn [t] {:t t :data {:amount 2.5 :attacker {:name "zombie"}}})
        felt (fn [mem hurts] (respond-to-hostile/walk-felt {:root "j1" :slots []
                                                            :view (fn [] {:now 1000000 :data {:entries {:hurt (vec hurts) :job/j1 [{:t 990000 :data mem}]}}})}))]
    (is (nil? (felt {:decision :fight} [(hurt 999000)])) "no flight: the fight is priced at them")
    (is (zero? (felt {:decision :flee :flight-start 999500} [(hurt 998500) (hurt 999000)])) "felt in the fight before the flight")
    (is (pos? (felt {:decision :flee :flight-start 999000} [(hurt 999500)])) "felt on the walk")))

(deftest a-body-that-fled-ranged-mobs-twice-fights-them-to-the-end-however-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:health 14} :inventory sword :entities [(skeleton 1 10 0) (skeleton 2 11 1)]}
              fled-before (fn [n] (fn [{:keys [eng]}] (dotimes [_ n] (mem/write! (:store eng) :hostile {:mob "skeleton" :decision :flee} respond-to-hostile/hostile-policy))))
              round (fn ^:async f [n]
                      (let [s (setup-flight world)]
                        ((fled-before n) s)
                        (core/submit! (:eng s) respond {})
                        (await (core/tick! (:eng s)))
                        s))]
          (is (= [:flee] (map :decision (decided (:seen (await (round 0)))))) "no flight so far: the fight is too dear, retreat")
          (is (= [:fight] (map :decision (decided (:seen (await (round 2)))))) "the second return: the walk only comes again, fight"))))))

(deftest hostile-attack-walks-to-far-ranged-mobs-only-when-the-fight-is-the-cheaper-answer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (fn ^:async f [fled]
                      (let [{:keys [eng]} (setup {:inventory sword :entities [(skeleton 1 10 0) (skeleton 2 11 1)]} 20)]
                        (dotimes [_ fled] (mem/write! (:store eng) :hostile {:mob "skeleton" :decision :flee} respond-to-hostile/hostile-policy))
                        (core/submit! eng '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 6}) {})
                        (await (core/tick! eng))
                        (count (tu/walked-to eng))))]
          (is (zero? (await (walks 0))) "two shooting: walking away is cheaper, declined, no walk")
          (is (pos? (await (walks 2))) "fled twice lately, the walk only comes again: walked up to them"))))))

(deftest hostile-near-counts-ranged-mobs-out-to-the-ranged-radius
  (is (holds? {:entities [(skeleton 1 13 0)]} {:radius 8 :ranged-radius 16}) "a skeleton at 13")
  (is (not (holds? {:entities [(zombie 13 0)]} {:radius 8 :ranged-radius 16})) "a zombie at 13 is beyond 8")
  (is (not (holds? {:entities [(skeleton 1 13 0)]} {:radius 8 :ranged-radius 10})) "beyond the ranged radius")
  (is (holds? {:entities [(skeleton 1 13 0)]} {}) "a skeleton at 13 with the default ranged radius (16)")
  (is (not (holds? {:entities [(skeleton 1 17 0)]} {})) "a skeleton at 17 is beyond the default"))

(deftest hostile-near-counts-every-skeleton-variant-as-ranged
  (doseq [mob ["stray" "parched" "bogged"]]
    (is (holds? {:entities [(assoc (skeleton 1 13 0) :name mob)]} {:radius 8 :ranged-radius 16}) (str mob " at 13"))))

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

(deftest hostile-attack-and-hostile-near-agree-on-a-ranged-mob-behind-a-torch-wall
  (let [torches (arrow-wall "torch" [64 65 66])
        sk (assoc (skeleton 7 6 0) :visible false :seen true)]
    (is (holds? {:entities [sk] :blocks torches} {:radius 8 :ranged-radius 16}) "the trigger sees a line of fire through torches")
    (async done
      (tu/run-async done
        (fn ^:async t []
          (let [{:keys [eng]} (setup {:inventory sword :blocks torches :entities [sk]})]
            (core/submit! eng '(jobs.combat.attack {:targets :hostiles :radius 4 :ranged-radius 16}) {})
            (is (some? (core/tick! eng)) "the hostile attack keeps the skeleton as a target: same line of fire as the trigger")))))))

(deftest combat-hostiles-takes-a-ranged-radius
  (let [p (tu/fake-on-floor {:entities [(zombie 1 13 0) (skeleton 2 12 0) (zombie 3 5 0)]})
        ids (fn [hs] (mapv #(.-id %) hs))]
    (is (= [3] (ids (combat/hostiles p 8))))
    (is (= [3 2] (ids (combat/hostiles p 8 {:ranged-radius 16}))))))

(deftest respond-walks-up-to-a-skeleton-at-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (flight-round respond {:inventory sword :entities [(skeleton 7 8 0)]}))]
          (is (pos? (walks-to s {:x 8 :z 0})) "armed and healthy: it closes in on the skeleton"))))))

(deftest hostile-attack-does-not-close-in-on-a-far-skeleton-with-other-hostiles-about
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (flight-round '(jobs.combat.attack {:targets :hostiles :radius 4 :ranged-radius 16})
                                     {:inventory sword :entities [(skeleton 7 13 0) (skeleton 8 15 4)]}))]
          (is (zero? (walks-to s {:x 13 :z 0})) "a lone skeleton is walked up to; with another about it is not")
          (is (zero? (count (calls (:p s) "attack"))) "nothing in reach to swing at"))))))

;; ------------------------------------------------------- retreat keeps going and minds walls

(defn wall-cells
  "Stone at feet and head height on the given [x z] cells."
  [cells]
  (into {} (for [[x z] cells y [64 65]] [(str x "," y "," z) "stone"])))

(deftest retreat-turns-away-from-a-wall-behind-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-flight {:entities [(zombie 5 0)]
                                      :blocks (wall-cells (for [z (range -2 3)] [-3 z]))})]
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (let [m (first-move eng)]
            (is (>= (js/Math.abs (:z m)) 3) "not straight back into the wall")
            (is (<= (:x m) 0) "and not towards the zombie")))))))

(def box
  "A 3x3 pen around the origin, open only on the +x side."
  (wall-cells (concat (for [z (range -2 3)] [-2 z]) (for [x (range -2 2)] [x -2]) (for [x (range -2 2)] [x 2]))))

(deftest a-cornered-retreat-fights-when-armed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (flight-round retreat {:inventory sword :blocks box :entities [(zombie 2 0)]}))]
          (is (= 4 (count (calls p "attack"))) "nowhere to go: it hits back until the zombie dies")
          (is (= [] (:list (core/state eng)))))
        (let [{:keys [eng p]} (await (flight-round retreat {:blocks box :entities [(zombie 2 0)]}))]
          (is (= -1 (:x (first-move eng))) "unarmed: first a step back into the far corner of the pen")
          (is (= 4 (count (calls p "attack"))) "nowhere further and nothing to build with: the fist, not a failed round")
          (is (= [] (:list (core/state eng))) "no giving up while the zombie can be hit"))))))

(def dead-end
  "A corridor five wide, closed behind the body (z -2), open towards +z."
  (wall-cells (concat (for [x (range -3 4)] [x -2]) (for [z (range -2 8)] [-3 z]) (for [z (range -2 8)] [3 z]))))

(deftest a-retreat-with-only-side-steps-left-fights-instead-of-shuffling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (flight-round retreat {:inventory sword :blocks dead-end :entities [(zombie 0 3)]}))]
          (is (zero? (count (tu/walked-to eng))) "a two-block side step gains nothing on the zombie")
          (is (= 4 (count (calls p "attack")))))))))

;; ------------------------------------------------------- sight

(deftest respond-ignores-a-hostile-it-cannot-see
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory sword :blocks wall :entities [(zombie 5 0)]})]
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (is (empty? (:list (core/state eng))) "only a zombie behind the wall: nothing to respond to, done")
          (is (zero? (count (tu/walked-to eng)))))))))

(deftest hostile-attack-goes-for-a-visible-hostile-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round-ms '(jobs.combat.attack {:targets :hostiles :ranged-radius 16 :radius 8})
                                                 {:inventory sword :blocks wall :entities [(zombie 1 5 0) (zombie 2 0 6)]} 20))]
          (is (= 2 (first (mapv #(.-id (.-args %)) (calls p "attack"))))
              "the nearer zombie is behind the wall; the visible one is fought"))))))

;; ------------------------------------------------------- finishing a fight, eating on the run

(deftest respond-finishes-a-nearly-dead-hostile-below-min-health
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-flight {:inventory sword :entities [(assoc (zombie 100 3 0) :health 10)]})]
          (.override (.-world p) "attack"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (fake/swap-self! p assoc :health 6) r)))))
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack"))) "health 6 after the first hit, but one more hit kills the zombie at 5: it swings")
          (is (empty? (fake/entities p))))))))

(deftest respond-still-flees-below-min-health-from-a-healthy-hostile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-flight {:inventory sword :entities [(assoc (zombie 100 3 0) :health 20)]})]
          (.override (.-world p) "attack"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (fake/swap-self! p assoc :health 6) (mem/write! (:store eng) :hurt {:amount 14 :attacker {:id 100 :name "zombie"}}) r)))))
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "attack"))) "15 left is not nearly dead")
          (is (pos? (count (tu/walked-to eng))) "it flees"))))))

(deftest retreat-eats-once-when-the-gap-is-wide
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-flight {:self {:food 10} :inventory [{:name "bread" :count 3}]
                                             :entities [(skeleton 1 5 0)]})
              walks-before-eat (atom nil)]
          (.override (.-world p) "eat"
                     (fn [token a impl] (reset! walks-before-eat (count (tu/walked-to eng))) (impl token a)))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "eat"))) "once per flight")
          (is (pos? @walks-before-eat) "the skeleton was 5 away at the start: no time to eat before the gap opened"))))))

(deftest a-cornered-fight-is-kept-while-the-hostile-is-close
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-flight {:inventory sword :blocks box :entities [(assoc (zombie 2 0) :health 20)]})]
          (.override (.-world p) "attack"
                     (fn [token a impl]
                       (swap! (fake/state p) update :blocks #(apply dissoc % (map fake/parse-cell (keys box))))
                       (impl token a)))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= 4 (count (calls p "attack"))) "the pen opened up at the first swing, but the zombie is still at 2: kept fighting")
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
  (is (not (holds-seeing-the-void? {:blocks dividing-wall :entities [(visible-zombie 8 0)]} danger-args)) "behind a wall across the floor")
  (is (holds? {:blocks dividing-wall :entities [(visible-zombie 8 0)]} danger-args) "behind a wall whose ends the body has not seen past: a way round may be there")
  (is (not (holds-seeing-the-void? {:blocks trench :entities [(visible-zombie 8 0)]} danger-args)) "across a trench two deep: down is fine, up is not")
  (is (not (holds? {:blocks (cells-of "stone" (for [x [-1 0 1] z [-1 0 1] y [64 65] :when (not (and (= x 0) (= z 0)))] [x y z]))
                    :entities [(visible-zombie 5 0)]}
                   danger-args))
      "with the body sealed in"))

(deftest hostile-near-holds-for-a-mob-that-hit-the-body-lately-whatever-the-way-test-says
  (let [holds-hit-at? (fn [t] (boolean (when-hostile-near (seeing-the-void (tu/fake-on-floor {:blocks dividing-wall :entities [(visible-zombie 8 0)]}))
                                                         {:data (mem/add-entry mem/empty-data :hurt {:t t :data {:attacker {:id 7 :name "zombie"}}} {:cap 50 :ttl 60000})
                                                          :now 6000}
                                                         danger-args)))]
    (is (holds-hit-at? 2000) "its blow 4 s ago shows it reaches the body")
    (is (not (holds-hit-at? 500)) "a blow older than danger/::hit-s")))

(deftest hostile-near-fires-for-a-zombie-that-can-walk-to-the-body
  (is (holds? {:entities [(visible-zombie 5 0)]} danger-args) "open ground")
  (is (holds? {:blocks (cells-of "stone" (for [z (range -9 11) y [64 65 66]] [3 y z])) :entities [(visible-zombie 8 0)]} danger-args)
      "a wall with a way round at z -10")
  (is (holds? {:blocks (cells-of "stone" (for [x [3 4] z (range -10 11)] [x 62 z])) :entities [(visible-zombie 8 0)]} danger-args)
      "across a trench one deep: it climbs out"))

(defn crop-box
  "The hostile-line-of-fire fixture's box: hollow stone x 4-8, y floor-y to 66, z -2-2, its west wall open at z -1-1
  with plant on farmland in the gap."
  [floor-y plant]
  (merge (cells-of "stone" (for [x (range 4 9) y (range floor-y 67) z (range -2 3)
                                 :when (or (#{4 8} x) (#{floor-y 66} y) (#{-2 2} z))]
                             [x y z]))
         (cells-of "air" (for [y [64 65] z [-1 0 1]] [4 y z]))
         (cells-of "farmland" (for [z [-1 0 1]] [4 63 z]))
         (cells-of plant (for [z [-1 0 1]] [4 64 z]))))

(deftest hostile-near-takes-a-crop-gap-as-a-way-out-of-a-box
  (doseq [plant ["wheat" "carrots" "poppy"]]
    (is (holds? {:blocks (crop-box 63 plant) :entities [(visible-zombie 6.5 0.5)]} danger-args)
        (str plant ": a 2-high room, the zombie on its floor"))
    (is (not (holds? {:blocks (crop-box 64 plant) :entities [(visible-zombie 6.5 0.5)]} danger-args))
        (str plant ": a 1-high room, the zombie sunk in its floor cannot walk"))))

(deftest hostile-near-needs-a-line-of-fire-for-a-skeleton
  (is (not (holds? {:blocks dividing-wall :entities [(skeleton 1 8 0)]} danger-args)) "behind a wall with no opening")
  (is (holds? {:blocks (dissoc dividing-wall "3,65,0") :entities [(skeleton 1 8 0)]} danger-args)
      "through a 1x1 window it can shoot"))

;; ------------------------------------------------- the odds: a response the body can survive

(def iron-armour
  {:head {:name "iron_helmet" :count 1} :torso {:name "iron_chestplate" :count 1}
   :legs {:name "iron_leggings" :count 1} :feet {:name "iron_boots" :count 1}})

(deftest combat-fight-damage-weighs-weapon-mobs-and-armour
  (let [z {:name "zombie" :distance 3}]
    (is (= 3.75 (cost/fight-damage {:weapon "iron_sword" :equipment nil :mobs [z]})) "4 swings, 2.5 s at 0.6 hits/s of 2.5 (a zombie hit on easy)")
    (is (= 65 (cost/fight-damage {:weapon nil :equipment nil :mobs [z]})) "the fist: 20 hits, one landed every 1.3 s, the zombie hitting once a second (the measured rate is for swords)")
    (is (= 18.75 (cost/fight-damage {:weapon "iron_sword" :equipment nil :mobs [z z]})) "the second hits until it dies too; a group is hit once a second per mob, not at the lone-mob rate")
    (is (= 1.69 (/ (js/Math.round (* 100 (cost/fight-damage {:weapon "iron_sword" :equipment iron-armour :mobs [z]}))) 100))
        "iron armour: vanilla's 13.5 effective points against 2.5 hp hits")
    (is (= 1.875 (cost/fight-damage {:weapon "iron_sword" :equipment nil :mobs [(assoc z :hits 2)]})) "a struck mob has less left")
    (is (= (* 4 (threat/creeper-exchange)) (cost/fight-damage {:weapon "stone_sword" :equipment nil :mobs [{:name "creeper" :distance 3}]}))
        "a creeper: 4 swings at the measured exchange each (hit-and-back), not the blast")
    (is (= 6 (cost/fight-damage {:weapon "iron_sword" :equipment nil :mobs [{:name "skeleton" :distance 13}]}))
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
                                 ["sword vs a creeper within fuse range: hit-and-back" {:inventory sword :entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]} :fight]
                                 ["no weapon vs a creeper" {:entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]} :flee]
                                 ["sword vs a skeleton at range" {:inventory sword :entities [(assoc (skeleton 7 12 0) :visible true)]} :fight]
                                 ["no weapon vs a skeleton at range" {:entities [(assoc (skeleton 7 12 0) :visible true)]} :flee]
                                 ["sword vs two zombies, no armour" {:inventory sword :entities [(zombie 7 3 0) (zombie 8 3 1)]} :flee]
                                 ["sword vs two zombies in diamond" {:inventory sword :equipment diamond-armour
                                                                     :entities [(zombie 7 3 0) (zombie 8 3 1)]} :fight]]]
          (let [s (await (flight-round respond spec))
                got (:decision (first (hostile-entries (:eng s))))]
            (is (= want got) why)
            (is (= (= :flee want) (fled-by-walker? s)) (str why ": flees with the engine walker, no swing"))))))))

;; ce634b35 (Bramble #39): a blast of 17 beside a fight; card a45ce0db: priced as one Poisson count of blast-sized blows
(deftest a-creeper-by-a-zombie-at-normal-difficulty-is-walked-from-the-blast-leaving-too-little-for-the-zombies-blows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [respond-normal '(jobs.survival.respond-to-hostile {:settings {:jobs.lib.cost.threat/difficulty 2}})
              s (await (flight-round respond-normal {:inventory sword :entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x 3 :y 64 :z 0}} (zombie 9 0 4)]}))]
          (is (= :flee (:decision (first (hostile-entries (:eng s))))))
          (is (fled-by-walker? s)))))))

(deftest respond-flees-a-creeper-from-a-dead-end-it-cannot-back-off-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (flight-round respond {:inventory sword :blocks (wall-cells (concat (for [x (range -1 9)] [x 1]) (for [x (range -1 9)] [x -1]) [[-1 0]]))
                                              :entities [{:id 8 :name "creeper" :kind "hostile" :pos {:x 3 :y 64 :z 0}}]}))]
          (is (= :flee (:decision (first (hostile-entries (:eng s)))))))))))

(deftest respond-fights-with-the-best-weapon-equipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (flight-round respond {:inventory [{:name "stone_sword" :count 1} {:name "diamond_sword" :count 1}]
                                                        :entities [(zombie 3 0)]}))]
          (is (= ["diamond_sword"] (mapv #(.-item (.-args %)) (calls p "equip"))))
          (is (= 4 (count (calls p "attack"))) "the fake's hits of 5 kill the 20 hp zombie in four; no fifth swing at the corpse"))))))

(def stair-exit
  "A dead-end corridor one wide along +z, closed behind the body (z -1): the way out of a dug-in cell, the zombie in it."
  (wall-cells (concat (for [z (range -1 4)] [-1 z]) (for [z (range -1 4)] [1 z]) [[0 -1]])))

(deftest a-cornered-unarmed-body-off-centre-seals-itself-in-instead-of-punching
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup-flight {:bodyHitbox true :self {:pos [0.5 64 0.75]}
                                                   :inventory [{:name "cobblestone" :count 16}]
                                                   :blocks stair-exit :entities [(zombie 0.5 2.5)]})]
          (js/setTimeout #(set-entities! p []) 300)
          (core/submit! eng retreat {})
          (await (core/tick! eng))
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
  (is (= :retry (:persistence (:hostile-near triggers/all))))
  (is (zero? (:cooldown-s (:hostile-near triggers/all) 0))))

(deftest the-hostile-entry-is-never-backed-off
  (is (false? (:backoff (:hostile-near triggers/all)))))

(deftest a-far-walkable-zombie-does-not-hold-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup-flight {:entities [(zombie 3 0)]})]
          (core/register-reflex! eng {:trigger :hostile-near})
          (await (core/tick! eng))
          (is (= 1 (count (reflex-fired seen))) "a zombie at 3: the reflex fires")
          (is (> (fake/dist (fake/vec-pos (:pos (fake/self p))) [3 64 0]) 8) "fled out of the trigger's radius, and stopped once the zombie stood still")
          (is (empty? (reflex-jobs eng)) "the zombie is far outside the trigger's radius: the reflex ended")
          (is (= [:done] (mapv :outcome (reflex-ended seen)))))))))

(deftest a-retreat-is-done-the-first-round-no-danger-is-within-its-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (flight-round retreat {:entities [(zombie 5 0)]}))]
          (is (= {:x -6 :z 0} (first-move eng)) "a step of 6 directly away")
          (is (= [] (:list (core/state eng))) "one round: done once the zombie is beyond its follow range"))))))

(deftest a-blocked-retreat-with-the-danger-present-neither-ends-nor-backs-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup-flight {:entities [(zombie 5 0)]})]
          (stuck-walks! p)
          (core/register-reflex! eng {:trigger :hostile-near})
          (await (core/tick! eng))
          (is (= [[:stopped :completed_not_cleared]] (mapv (juxt :outcome :how) (reflex-ended seen)))
              "held the whole flight, then stopped still chased, the zombie still there")
          (await (core/tick! eng))
          (is (= 2 (count (reflex-fired seen))) "fired again at once: the zombie still stands")
          (is (empty? (filter #(= :backoff (:kind %)) @seen)) "never backed off"))))))

(deftest a-reflex-ended-by-its-time-budget-fires-again-while-the-danger-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup-flight {:entities [(zombie 5 0)]})]
          (stuck-walks! p)
          (core/register-reflex! eng {:trigger :hostile-near :job '(jobs.survival.respond-to-hostile {:spend {:job-time 2}})})
          (await (core/tick! eng))
          (is (= [[:stopped :job-time]] (mapv (juxt :outcome :reason) (reflex-ended seen))) "the time budget ended the call")
          (await (core/tick! eng))
          (is (= 2 (count (reflex-fired seen))) "fired again at once: the zombie still stands")
          (is (empty? (filter #(= :backoff (:kind %)) @seen)) "never backed off"))))))

(deftest an-explicit-cooldown-in-the-agents-entry-is-honoured
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[why entry fired]
                [["the default: fired again at once" {:trigger :hostile-near :args {:radius 10}} 2]
                 ["the agent's own cooldown" {:trigger :hostile-near :args {:radius 10} :persistence :cooldown :cooldown-s 30} 1]]]
          (let [{:keys [eng seen clock]} (setup {:inventory sword
                                                 :entities [(assoc (zombie 1 3 0) :health 5 :visible true)
                                                            (assoc (zombie 2 0 7) :visible true)]})]
            (core/register-reflex! eng (assoc entry :job '(jobs.survival.respond-to-hostile {:settings {:jobs.lib.danger/hostile-radius 4 :jobs.lib.price/way-radius 4}})))
            (await (core/tick! eng))
            (is (= [:done] (mapv :outcome (reflex-ended seen))) (str why ": the near zombie killed, the one at 7 is beyond the job's 4"))
            (swap! clock + 1000)
            (await (core/tick! eng))
            (is (= fired (count (reflex-fired seen))) why)))))))

(deftest a-hostile-reflex-cut-by-a-higher-one-fires-again-while-the-danger-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup-flight {:entities [(zombie 5 0)]})]
          (core/register-reflex! eng {:trigger :burning})
          (core/register-reflex! eng {:trigger :hostile-near})
          (let [release (.hold (.-world p) "steer")
                flight (core/tick! eng)]
            (await (js/Promise. (fn [r] (js/setTimeout r 50))))
            (is (= [:hostile-near] (mapv :reflex (reflex-fired seen))))
            (fake/swap-self! p assoc :inLava true)
            (let [burning (core/tick! eng)]
              (is (= :burning (:reflex (peek (reflex-fired seen)))) "the lava cuts the flight mid-walk")
              (release)
              (await flight)
              (fake/swap-self! p assoc :inLava false)
              (await burning)))
          (set-entities! p [(zombie (+ 3 (first (:pos (fake/self p)))) 0)])
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= :hostile-near (:reflex (peek (reflex-fired seen)))) "fired again, fresh, as soon as the fire is out"))))))

;; ------------------------------------------------------- heard mobs: rough spot only

(defn heard-zombie
  "A zombie 3 blocks east that is only heard (its exact place is 3 away; its rough spot, the near band, is 4)."
  []
  (zombie 7 3 0))

(deftest hostile-attack-never-swings-at-or-walks-to-a-heard-mob
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory sword :entities [(assoc (heard-zombie) :visible false)]})]
          (core/submit! eng fight {})
          (is (nil? (core/tick! eng)) "only a heard mob: the job declines, the retreat takes over")
          (is (zero? (count (calls p "attack"))) "no swing at a mob only heard")
          (is (zero? (count (tu/walked-to eng))) "no walk to its exact place"))))))

(deftest respond-weighs-a-heard-mob-by-its-rough-distance
  (let [p (tu/fake-on-floor {:floor flight-floor :entities [(assoc (heard-zombie) :visible false)]})
        e (first (combat/hostiles p 8))]
    (is (= 4 (:distance (hostile-fight/mob-of p nil e))) "the near band stands for 4 blocks, not the exact 3")))

(deftest recover-drops-logs-a-heard-danger-at-its-rough-spot
  (let [p (tu/fake-on-floor {:floor flight-floor :entities [(assoc (heard-zombie) :visible false)]})
        e (first (combat/hostiles p 8))
        f (recover-drops-danger/danger-fields p e)
        [bx] (let [pos (.-pos (.self p))] [(.-x pos)])]
    (is (= "zombie" (:mob f)))
    (is (= (+ bx 4) (:x (:mob-pos f))) "the rough spot 4 east, not the exact 3")))

(deftest hostile-attack-looks-toward-the-heard-mob-not-a-seen-one-past-the-leash
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory sword
                                      :entities [(zombie 7 -25 0) (assoc (zombie 8 3 0) :visible false)]})
              looks (atom [])
              args {:targets :hostiles :radius 30 :ranged-radius 30 :skip []}
              c {:primitives p :args args :view #(mem/view (:store eng)) :root nil :slots []
                 :now (constantly 0) :update-mem (fn [_ _] nil)
                 :act (fn [k a] (when (= :look k) (swap! looks conj a)) (js/Promise.resolve nil))}
              r (await (attack/hostile-step c))]
          (is (= [:nothing-possible :heard-only] ((juxt :kind :cause) r)) "the seen zombie is past the leash, the heard one is never fought")
          (is (= 1 (count @looks)))
          (is (pos? (.-x (.-pos (first @looks)))) "the look points east at the heard mob, not west at the seen one"))))))

(deftest hostile-attack-one-call-fights-until-the-mob-is-dead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (first-round-ms fight {:inventory sword :entities [(zombie 7 3 0)]} 20))]
          (is (= 4 (count (calls p "attack"))) "four hits in one call")
          (is (= [] (:list (core/state eng))) "done in the first call"))))))

;; ------------------------------------------------------- a mob that hurts the body is not given up on

(deftest attack-gives-up-on-a-blocked-mob-only-while-it-does-not-hurt-the-body-in-a-hostiles-fight
  (let [mem {:fails {7 2} :fail-health {7 12}}]
    (is (= 3 (attack/fail-count mem 7 12 true)) "blocked again, body unhurt since: the count goes on")
    (is (= 1 (attack/fail-count mem 7 9 true)) "the body lost health since: the count starts over")
    (is (= 3 (attack/fail-count mem 7 9 false)) "a plain attack counts on however hurt the body is")
    (is (= 1 (attack/fail-count {} 7 20 true)))))

(deftest respond-never-fights-an-enderman-however-good-the-sword
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (flight-round respond {:inventory sword :entities [{:id 100 :name "enderman" :kind "hostile" :pos {:x 5 :y 64 :z 0}}]}))]
          (is (= [:flee] (mapv :decision (decided seen)))))))))

(deftest no-fight-wins-against-an-enderman-however-much-health
  (let [{:keys [p]} (setup {:inventory sword :equipment {:torso {:name "iron_chestplate" :count 1}}})
        c {:primitives p :args {} :root "j1" :slots [] :view (fn [] {:now 0 :data {:entries {}}})}
        mob (fn [n] (clj->js {:id 100 :name n :type "hostile" :pos {:x 2 :y 64 :z 0} :health 20}))]
    (fake/swap-self! p assoc :health 200)
    (is (hostile-fight/wins? c [(mob "zombie")]) "control: the same kit beats a zombie")
    (is (not (hostile-fight/wins? c [(mob "enderman")])) "however much health: still a loss")))

(deftest a-calm-enderman-is-no-danger-an-angry-one-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[angry want] [[false []] [true [:flee]]]]
          (let [{:keys [seen]} (await (flight-round respond {:entities [{:id 100 :name "enderman" :kind "hostile" :angry angry :pos {:x 5 :y 64 :z 0}}]}))]
            (is (= want (mapv :decision (decided seen))) (str "angry " angry))))))))

(deftest a-skeleton-only-heard-far-off-is-no-danger-a-near-one-is
  (doseq [[x n] [[12 0] [6 1]]]
    (let [p (tu/fake-on-floor {:floor flight-floor :entities [{:id 5 :name "skeleton" :kind "hostile" :visible false :pos {:x x :y 64 :z 0}}]})]
      (is (= n (count (danger-q/dangers p 8 {:ranged-radius 16}))) (str "heard " x " blocks east")))))

;; ---- card 10a36e29: a flight from heard-only mobs that ends blocked mutes them, growing

(def heard-wall (tu/box 5 64 -6 5 65 -1 "stone"))

(defn mutes [eng] (mapv (comp :until :data) (mem/entries (mem/view (:store eng)) :heard-mute)))

(deftest a-flight-from-a-heard-only-mob-that-ends-blocked-mutes-heard-mobs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-seeing {:blocks heard-wall :entities [(zombie 6.5 -3.5)]} nil 1000)]
          (stuck-walks! p)
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (is (= 1 (count (mutes eng))) "one mute entry, not a mute per round")
          (is (true? (body/heard-muted? (mem/view (:store eng))))))))))

;; ---- a body that would fight leaves a mob it only hears alone

(defn heard-skeleton [id x z] {:id id :name "skeleton" :kind "hostile" :visible false :pos {:x x :y 64 :z z}})

(defn heard-only-events [seen] (filterv #(= :hostile.heard-only (:kind %)) @seen))
(defn left-heard-endings [seen] (filterv :mute-s @seen))

(deftest full-iron-and-a-sword-afford-two-skeletons-heard-near
  (let [near {:name "skeleton" :distance 4}
        cost-of #(cost/fight-damage {:weapon "iron_sword" :equipment iron-armour :mobs %})
        round2 #(/ (js/Math.round (* 100 %)) 100)]
    (is (= [1.45 7.26] (mapv (comp round2 cost-of) [[near] [near near]])) "of the 16 a body at 20 may spend")
    (is (= :fight (:answer (cost/decide {:health 20 :food 20 :level 0 :carried 0 :respawn {:x 40 :y 64 :z 0}}
                                        {:fight (cost/fight-answer {:weapon "iron_sword" :equipment iron-armour :mobs [near near]})
                                         :flee (cost/flee-answer {:food 20 :equipment iron-armour :mobs [(assoc near :follow 16) (assoc near :follow 16)]})}
                                        {:x 0 :y 64 :z 0}))))))

(deftest a-body-that-would-fight-does-not-flee-mobs-it-only-hears
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [skeletons [(heard-skeleton 5 6 0) (heard-skeleton 6 -3 3)]
              kit {:inventory sword :equipment iron-armour :entities skeletons}]
          (let [{:keys [eng seen]} (await (flight-round respond kit))]
            (is (= [:fight] (mapv :decision (decided seen))))
            (is (= [] (tu/walked-to eng)) "no flight from a sound")
            (is (= 1 (count (mutes eng))) "heard mobs are muted: the walk it cut goes on")
            (is (= [] (:list (core/state eng))) "done, not held")
            (is (= ["skeleton"] (mapv :mob (left-heard-endings seen))) "its ending says which mob it heard and left")
            (is (= [] (heard-only-events seen)) "and no event restates it"))
          (let [{:keys [eng seen] :as s} (setup-flight kit)]
            (mem/write! (:store eng) :hurt {:amount 3})
            (core/submit! eng respond {})
            (await (core/tick! eng))
            (is (walked? s) "just hurt: the unseen mob may be the one hitting, it flees")
            (is (= [] (heard-only-events seen)))))))))

(deftest a-held-fight-that-no-mob-comes-to-watches-the-mob-instead-of-fleeing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p raw]} (setup-seeing {:inventory sword :time 14000 :radius 6 :entities [(zombie 4 0)]} [0 4] 1000)]
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (let [view (mem/view (:store eng))]
            (is (= [7] (keys (body/watched view))) "the mob that never came is watched")
            (is (empty? (tu/walked-to eng)) "no flight")
            (is (= [] (:list (core/state eng))) "done, so the reflex does not fire again for it")
            (is (not (danger-q/hostile-near? p view {})) "and the trigger stays quiet while it stays there, inside danger/watch-gap")
            (swap! (fake/state raw) assoc-in [:entities 0 :pos] [2.5 64 0])
            (is (danger-q/hostile-near? p (mem/view (:store eng)) {}) "it fires again once the mob comes nearer")))))))

(deftest only-seen-melee-mobs-may-be-watched
  (let [p (tu/fake-on-floor {:floor flight-floor :entities [(zombie 5 0) (skeleton 9 -4 0)
                                                           {:id 10 :name "creeper" :kind "hostile" :pos {:x 4 :y 64 :z 0}}]})
        sensed (fn [ids] (mapv #(first (filter (fn [e] (= % (.-id e))) (danger-q/known-hostiles p 16 {}))) ids))]
    (are [ids ok?] (= ok? (respond-to-hostile/watchable? (sensed ids)))
      [7] true
      [7 9] false
      [10] false)))

(deftest a-flight-from-a-seen-mob-that-ends-blocked-mutes-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup-seeing {:entities [(zombie 0.5 4.5)]} nil 1000)]
          (stuck-walks! p)
          (core/submit! eng respond {})
          (await (core/tick! eng))
          (is (= [] (mutes eng))))))))

(deftest a-body-backs-off-from-a-creeper-along-open-ground
  (let [flat (fn [{:keys [y]}] (if (< y 64) "stone" "air"))
        cell (hostile-fight/back-cell flat {:x 0.5 :y 64 :z 0.5} {:x 2.5 :y 64 :z 0.5} 3)]
    (is (<= (:x cell) -2) "three blocks straight away from the creeper")
    (is (nil? (hostile-fight/back-cell (constantly "stone") {:x 0.5 :y 64 :z 0.5} {:x 2.5 :y 64 :z 0.5} 3)) "walled in: no cell")))

(deftest a-fight-walk-prices-the-drop-it-sees-and-an-unseen-way-at-the-unknown-prices
  (let [ledge (merge (tu/box 0 63 -2 2 63 2 "stone") (tu/box 3 59 -2 9 59 2 "stone"))
        zed (clj->js {:id 7 :name "zombie" :kind "hostile" :visible true :pos {:x 8.5 :y 60 :z 0.5}})
        hp (fn [p] (hostile-fight/walk-hp {:primitives p :args {} :root "j1" :slots [] :view (fn [] {:now 0 :data {:entries {}}})} [zed]))
        seen (tu/fake {:blocks ledge :self {:pos [0.5 64 0.5]}})
        dark (tu/fake {:blocks ledge :self {:pos [0.5 64 0.5]}})
        sensed (.-sensedAt dark)]
    (tu/set-sensed! dark (fn [q] (if (<= 3 (.-x q)) #js {:unknown true :pos q} (sensed q))))
    (is (<= 1 (hp seen)) "the 4-block drop to the zombie, seen, costs its fall")
    (is (< (hp dark) (hp seen)) "the ground beyond the ledge never seen: no drop known, its steps at the unknown prices")
    (is (pos? (cost/unseen-route-hp (constantly :ground) [{:x 0 :y 64 :z 0}])) "an unseen step is not free")
    (is (zero? (cost/unseen-route-hp (constantly :open) [{:x 0 :y 64 :z 0}])))))

(def pond
  "A pond east of the body: water at x 2..4, y 62 and 63 (the shore floor's level is 63), z -2..2, stone under it."
  (into {} (for [x (range 2 5) z (range -2 3) y [61 62 63]]
             [(str x "," y "," z) (if (= y 61) "stone" "water")])))

(deftest a-drowned-swimming-in-a-pond-beside-the-body-is-a-danger-at-any-depth
  (are [y] (holds? {:blocks pond :entities [{:id 9 :name "drowned" :kind "hostile" :pos {:x 3.5 :y y :z 0.5}}]} {:radius 8})
    63 62))

(deftest a-drowned-holding-a-trident-counts-as-ranged
  (let [drowned (fn [held] {:id 9 :name "drowned" :kind "hostile" :held held :pos {:x 3.5 :y 63 :z 0.5}})]
    (are [held fires?] (= fires? (boolean (holds? {:blocks pond :entities [(drowned held)]} {:radius 2 :ranged-radius 16})))
      "trident" true
      nil false)))

(def lake
  "A lake 7 deep under the flight floor's level: water at x -4..4, y 57..63, z -4..4, stone under it; its shore stands at y 64."
  (into {} (for [x (range -4 5) z (range -4 5) y (range 56 64)]
             [(str x "," y "," z) (if (= y 56) "stone" "water")])))

(defn drowned-at [x y z] {:id 9 :name "drowned" :kind "hostile" :visible true :pos {:x x :y y :z z}})

(deftest a-body-afloat-cannot-reach-a-drowned-deep-under-it-and-swims-out-rather-than-fight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:blocks lake :self {:pos [-0.5 63 0.5] :health 13} :inventory [{:name "stone_sword" :count 1}]}
              reaches? (fn [d] (let [p (tu/fake-on-floor (merge {:floor flight-floor :entities [d]} world))]
                                 (hostile-fight/reaches? {:primitives p} (first (danger-q/known-hostiles p 30 {:ranged-radius 30})))))]
          (are [d reached?] (= reached? (reaches? d))
            (drowned-at 2.5 57 0.5) false
            (drowned-at 2.5 63 0.5) true
            (drowned-at -0.5 60 0.5) true)
          (let [{:keys [seen]} (await (flight-round respond (assoc world :entities [(drowned-at 2.5 57 0.5)])))]
            (is (= [:flee] (mapv :decision (decided seen))) "13 hp, the drowned 3 blocks off and 6 down, the shore 4 blocks off")))))))

(deftest only-a-possible-mob-blow-counts-as-a-hit
  (are [m expected] (= expected (hurt/mob-hit? m))
    {:attacker {:name "zombie"} :damageType "minecraft:mob_attack"} true
    {:damageType "minecraft:arrow"} true
    {} true
    {:damageType "minecraft:magic"} false
    {:damageType "minecraft:starve"} false
    {:cause "fire"} false
    {:damageType "minecraft:fall"} false
    {:damages [{:damageType "minecraft:magic"} {:damageType "minecraft:fall"}]} false
    {:damages [{:damageType "minecraft:magic"} {:attacker {:name "spider"}}]} true))

(deftest a-fight-turned-flight-in-one-run-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:health 20} :inventory sword :entities [(zombie 3 0)]})
              eng (assoc-in eng [:jobs 'jobs.combat.attack]
                            {:round (fn ^:async f [_c] (fake/swap-self! p assoc :health 1) :continue)})]
          (core/submit! eng respond {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [:fight :flee] (mapv :decision (decided seen))) "each change of decision is one event")
          (is (re-find #"earlier fight at 20 hp, now 1 hp" (:text (last (filter #(= :hostile.decided (:kind %)) @seen))))))))))
