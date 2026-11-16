(ns engine.flee-home-test
  "A chased body near its own closed hut runs home and shuts the door when it gets there before the chasers (Juniper,
  2026-10-08: in its doorway with a zombie outside, the flight's straight lines ran out past the zombie and the body
  died far from the hut). A zombie standing at the door is not run into."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.rays :as rays]
            [engine.hut-shelter-test :as hs]
            [engine.hostile-test :as ht]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.pass :as pass]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.flight-job :as fj]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.clock :as clock]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.caught :as caught-prior]
            [jobs.lib.cost.danger :as cost-danger]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.survival.respond-flight :as retreat]
            [jobs.survival.respond.cover :as cover]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.survival.respond.options :as cornered]
            [jobs.survival.respond-choose :as choose]
            [jobs.lib.util :as u]
            [jobs.lib.rooms :as rooms]))

;; The hut of hut-shelter-test: walls x 3..7, z -2..2, roof y 67, room x 4..6 z -1..1, a door at (5,64,2) facing south.

(def home {:x 5 :y 64 :z 0})

(def no-ceiling "Settings that lift the search budget, for a test of what the prices alone cut." {::cost-danger/search-budget-s 1000})

(def open-door
  {"5,64,2" {:open true :half "lower" :facing "south"} "5,65,2" {:open true :half "upper" :facing "south"}})

(defn perceiving
  "p (seeing every block) with a perception over its world, its memory holding the ground round the hut up to two blocks over
  head height (what the walk planner reads as seen): room answers are kept as on a live body."
  [p]
  (let [per (perception/create (fake-raw/create p) {:now (constantly 1000000)})]
    (doseq [x (range -25 13) y (range 63 68) z (range -5 17)] (rays/touch! per [x y z]))
    (doto (js/Object.assign #js {} p) (aset "perception" per))))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (cond-> (tu/seeing-all (tu/fake-on-floor (dissoc world :home :perceive)))
            (:perceive world) perceiving)
        now (tu/act-clock clock p 1000)
        eng (core/create {:primitives p :jobs fj/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    (when-not (false? (:home world)) (mem/write! (:store eng) :home {:pos (:home world home)} {:cap 1 :ttl :forever}))
    {:eng eng :p p :seen seen}))

(def stall-waits
  "Waits after which a flight here counts as stuck (the fake's mobs never leave; flights here wait 5 at most): the mobs
  are cleared so the test fails instead of hanging."
  100)

(defn ^:async flee!
  "Run a flight in world to its end, at most 6 ticks; the mobs leave on wait leave-wait (default: only at stall-waits, a failure)."
  ([world] (await (flee! world stall-waits)))
  ([world leave-wait]
   (let [{:keys [eng p] :as s} (setup world)
         waits (atom 0)]
     (.override (.-world p) "wait" (fn ^:async f [token a impl]
                                     (let [n (swap! waits inc)]
                                       (when (#{leave-wait stall-waits} n) (swap! (fake/state p) assoc :entities []))
                                       (when (= n stall-waits) (is (< n stall-waits) "the flight ends before stall-waits holds")))
                                     (await (impl token a))))
     (core/submit! eng (tu/order '(test.flight)) {})
     (await (tu/run-until-empty eng 6))
     s)))

(defn zombie [x z chase] (cond-> {:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos {:x x :y 64 :z z} :visible true}
                           chase (assoc :chase {:speed chase})))

(def hut-floor
  "The fake lays no floor under columns a test built in: the hut's own floor, under the room and the door."
  (tu/box 3 63 -2 7 63 2 "cobblestone"))

(defn hut
  ([pos states entities] (hut pos states entities {}))
  ([pos states entities extra]
   (assoc (hs/hut-world pos (merge hut-floor extra)) :states states :entities entities :floor [-40 -40 40 40])))

(defn body [p] (fake/vec-pos (:pos (fake/self p))))

(defn in-room? [[x _ z]] (and (<= 4 x 7) (<= -1 z 2)))

(defn ran-home? [seen] (boolean (some #(= :retreat.home (:kind %)) @seen)))

(defn room-whys
  "Why each closed room of the first retreat.choice was not run to."
  [seen]
  (mapv :why (:rooms (first (filter #(= :retreat.choice (:kind %)) @seen)))))

(defn walk-targets [eng] (mapv #(select-keys % [:x :z]) (tu/walked-to eng)))

(deftest the-race-to-a-refuge-is-won-with-a-margin-at-a-flights-measured-pace
  (are [body-dist mob-dists food first?] (= first? (cost/reach-first? body-dist mob-dists food))
    2 [4] 20 true
    6.5 [13] 20 true
    8 [3] 20 false
    8 [] 20 true
    5 [3 30] 20 false
    10 [8] 20 false
    10 [13] 20 true
    10 [13] 6 false))

(deftest cornered-at-home-the-body-never-digs-its-own-hut
  (is (= [:plug :pillar :back-off :fight] (choose/escape-order {:home? true}))))

(deftest a-hut-is-a-closed-room-once-its-doors-are-shut-and-open-ground-is-not
  (let [p (tu/seeing-all (tu/fake-on-floor (hut {:x 5.5 :y 64 :z 0.5} open-door [])))]
    (is (true? (reach-room/closed-room? p home)) "an open door counts as one that can be shut")
    (is (false? (reach-room/closed-room? p {:x 15 :y 64 :z 0})) "the open field outside")
    (is (true? (reach-room/closed-room? p {:x 5 :y 64 :z 2})) "standing in the open door's own cell: judged by the room it belongs to")
    (is (= #{[5 64 0]} (set (filter #{[5 64 0]} (:cells (reach-room/room* p {:x 5 :y 64 :z 2}))))) "the room of a doorway holds the room cells")
    (is (false? (reach-room/closed-room? (tu/seeing-all (tu/fake-on-floor (hut {:x 5.5 :y 64 :z 0.5} {} []
                                                                          {"5,64,2" "air" "5,65,2" "air"})))
                                    home))
        "a doorway with no door")))

(deftest a-body-in-its-doorway-with-a-zombie-outside-goes-in-and-shuts-the-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee! (hut {:x 5.5 :y 64 :z 2.5} open-door [(zombie 5.5 4.6 0.4)])))]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (false? (hs/door-open? p)) "the door is shut on the zombie")
          (is (every? #(<= (:z %) 2) (walk-targets eng)) (str "never out past the door: " (walk-targets eng)))
          (is (= 1 (count (filter #(= :retreat.home (:kind %)) @seen))) "says once it runs home")
          (is (= [] (:list (core/state eng))) "the shut door ends the flight"))))))

(deftest a-body-outside-with-a-zombie-far-behind-runs-home-through-its-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee! (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [(zombie 5.5 13.5 0.4)])))
              steps (filter #(= :retreat.step-searches (:kind %)) @seen)
              budget-ms (* 1000 (cost/search-budget-s))]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (false? (hs/door-open? p)) "the door is shut behind it")
          (is (= [] (:list (core/state eng))))
          (is (some #(some (comp #{:survey-floods} first) (:searches %)) steps) (str "each step says what its searches spent: " (vec steps)))
          (is (every? #(< (js/Math.abs (- budget-ms (:left-ms %) (reduce + (map second (:searches %))))) 0.5) steps)
              "what they spent and what is left make up the step's meter"))))))

(deftest a-low-body-with-blocks-near-its-hut-runs-home-instead-of-pillaring-in-the-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> (hut {:x 5.5 :y 64 :z 4.5} hs/door-states [(zombie 5.5 12.5 0.4)])
                        (assoc :inventory [{:name "cobblestone" :count 20}])
                        (assoc-in [:self :health] 4))
              {:keys [p seen]} (await (flee! world))]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (empty? (filter #(= :retreat_sealed (:kind %)) @seen)) "no refuge was built outside"))))))

(deftest a-low-body-that-has-not-seen-into-its-shut-hut-opens-the-door-to-look-and-runs-in
  ;; the glance stopped at the shut door (a walk cannot stand in it) and saw nothing, or spent itself on the home cell,
  ;; which has no door beside it: the home stayed unseen and the body fled past its hut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> (hut {:x 5.5 :y 64 :z 5.5} hs/door-states [(zombie 5.5 12.5 nil)])
                        (assoc :inventory [{:name "cobblestone" :count 16}])
                        (assoc-in [:self :health] 4))
              {:keys [eng raw seen]} (ht/setup-seeing (dissoc world :floor) nil 1000)]
          (mem/write! (:store eng) :home {:pos {:x 4 :y 64 :z -1}} {:cap 1 :ttl :forever})
          (core/submit! eng (tu/order '(test.flight)) {})
          (await (tu/run-until-empty eng 6))
          (is (ran-home? seen))
          (is (in-room? (body raw)) (str "inside the hut: " (body raw)))
          (is (not (hs/door-open? raw)) "the door shut behind it")
          (is (empty? (filter #(= :retreat_sealed (:kind %)) @seen)) "no refuge built outside"))))))

(deftest the-hostile-reflex-prices-the-unseen-shut-home-after-the-look-a-flight-takes-first
  ;; respond priced the flight before the glance into the home: no refuge, so the pillar (809) beat the flight (1302)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> (hut {:x 5.5 :y 64 :z 5.5} hs/door-states [(zombie 5.5 12.5 nil)])
                        (assoc :inventory [{:name "cobblestone" :count 16}])
                        (assoc-in [:self :health] 4))
              {:keys [eng raw seen]} (ht/setup-seeing (dissoc world :floor) nil 1000)]
          (mem/write! (:store eng) :home {:pos {:x 4 :y 64 :z -1}} {:cap 1 :ttl :forever})
          (core/submit! eng (tu/order '(jobs.survival.respond-to-hostile {})) {})
          (await (tu/run-until-empty eng 3))
          (is (nil? (:cover (first (filter #(= :hostile.decided (:kind %)) @seen)))) "the home is the refuge, no cover built here")
          (is (in-room? (body raw)) (str "inside the hut: " (body raw)))
          (is (not (hs/door-open? raw)) "the door shut behind it")
          (is (empty? (filter #(= :retreat_sealed (:kind %)) @seen)) "no refuge built outside"))))))

(deftest a-look-into-the-unseen-home-is-an-answer-taken-only-where-its-price-pays
  ;; card a54e3b40: no gate; the look's worth is the chance the room is closed times what the run in saves over the best
  ;; answer without it, against its own seconds (the walk to the door and the looks, the zombie nearer by them)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [run (fn ^:async run [{:keys [closed-chance home? flood-pays?] :or {home? true flood-pays? true}}]
                    (let [world (-> (hut {:x 5.5 :y 64 :z 5.5} hs/door-states [(zombie 5.5 12.5 nil)])
                                    (assoc :inventory [{:name "cobblestone" :count 16}])
                                    (assoc-in [:self :health] 4))
                          {:keys [eng seen]} (ht/setup-seeing (dissoc world :floor) nil 1000)
                          chance cost/closed-chance
                          pays? cost/refuge-search-pays?
                          kept-fn reach-room/boundary-kept?]
                      (when home? (mem/write! (:store eng) :home {:pos {:x 4 :y 64 :z -1}} {:cap 1 :ttl :forever}))
                      (when closed-chance (set! cost/closed-chance closed-chance))
                      ;; a look's one flood (rooms/flood-s), not kept, that the race does not pay for; survey's searches cost other sums
                      (when-not flood-pays? (set! reach-room/boundary-kept? (constantly false)) (set! cost/refuge-search-pays? (fn [spent cost-s & more] (and (not= cost-s (rooms/flood-s)) (apply pays? spent cost-s more)))))
                      (try (core/submit! eng (tu/order '(jobs.survival.respond-to-hostile {})) {})
                           (await (tu/run-until-empty eng 1))
                           (finally (set! cost/closed-chance chance) (set! cost/refuge-search-pays? pays?) (set! reach-room/boundary-kept? kept-fn)))
                      [(first (filter #(= :hostile.look-in (:kind %)) @seen)) (first (filter #(= :hostile.decided (:kind %)) @seen))]))
              [look] (await (run {}))
              [never decided] (await (run {:closed-chance (constantly 0)}))
              [stranger] (await (run {:home? false}))
              [unpaid] (await (run {:flood-pays? false}))]
          (is (< (:cost look) (:value look)) (str "the look pays: " (select-keys look [:value :cost :closed :prices])))
          (is (< (get-in look [:prices :look-in]) (get-in look [:prices :pillar])) "and is the cheapest answer")
          (is (nil? never) "a room that cannot be closed is worth no look")
          (is (= :pillar (:cover decided)) (str "so the pillar it beats otherwise is taken: " (:prices decided)))
          (is (< (:cost stranger) (:value stranger)) (str "a stranger's building is looked into on the same price: " (select-keys stranger [:value :cost :closed :prices])))
          (is (nil? unpaid) "a look whose flood the race does not pay for is no answer"))))))

(deftest the-door-a-glance-opens-is-recorded-with-an-expiry
  ;; the glance clicked the door by hand and wrote no :opened entry: a cut flight left it open and unrecorded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> (hut {:x 5.5 :y 64 :z 5.5} hs/door-states [(zombie 5.5 12.5 nil)])
                        (assoc :inventory [{:name "cobblestone" :count 16}])
                        (assoc-in [:self :health] 4))
              {:keys [eng raw]} (ht/setup-seeing (dissoc world :floor) nil 1000)
              snap (atom nil)
              shut pass/shut-column!]
          (mem/write! (:store eng) :home {:pos {:x 4 :y 64 :z -1}} {:cap 1 :ttl :forever})
          (with-redefs [pass/shut-column! (fn ^:async s [c col]
                                            (reset! snap (mapv :data (mem/entries (ctx/view c) :opened)))
                                            (await (shut c col)))]
            (core/submit! eng (tu/order '(test.flight)) {})
            (await (tu/run-until-empty eng 6)))
          (is (= [{:x 5 :y 64 :z 2}] (map :cell @snap)) "the open door was recorded while open")
          (is (every? :until @snap) "with an expiry"))))))

(deftest a-chaser-that-beats-the-body-to-the-door-gets-no-glance-and-no-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> (hut {:x 5.5 :y 64 :z 5.5} hs/door-states [(zombie 7.5 3.5 nil)])
                        (assoc :inventory [{:name "cobblestone" :count 16}])
                        (assoc-in [:self :health] 4))
              {:keys [eng raw seen]} (ht/setup-seeing (dissoc world :floor) nil 1000)]
          (mem/write! (:store eng) :home {:pos {:x 4 :y 64 :z -1}} {:cap 1 :ttl :forever})
          (core/submit! eng (tu/order '(test.flight)) {})
          (await (tu/run-until-empty eng 6))
          (is (not (ran-home? seen)))
          (is (not (hs/door-open? raw)) "the door was never opened for a look"))))))

(deftest a-chaser-that-loses-to-the-walk-alone-but-not-to-the-looks-at-the-door-gets-no-glance
  (let [me {:x 5.5 :y 64 :z 5.5} d {:x 5 :y 64 :z 2}
        walk (u/dist me (rooms/mid d))
        mob-dist 5.5]
    (is (cost/reach-first? walk [mob-dist] 20 [\"zombie\"]) "the walk to the door alone is won")
    (is (not (cost/reach-first? (rooms/glance-steps me d 20 4) [mob-dist] 20 [\"zombie\"])) "the click, the walk through and the looks make it lost")))

(deftest a-creeper-is-never-led-to-the-hut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [creeper (assoc (zombie 5.5 13.5 0.4) :name "creeper")
              {:keys [p seen]} (await (flee! (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [creeper])))]
          (is (not (ran-home? seen)) "a blast at the door would take the door, chest and bed")
          (is (= [:creeper] (room-whys seen)) "and the flight says why the hut was no refuge")
          (is (not (in-room? (body p)))))))))

(deftest a-creeper-beside-the-hut-the-body-stands-in-is-not-shut-out-and-forgotten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [creeper (assoc (zombie 5.5 4.3 nil) :name "creeper")
              ;; the fake creeper never blasts or walks off: it leaves on the third hold, as one does that finds no way in
              {:keys [p seen]} (await (flee! (hut {:x 5.5 :y 64 :z 1.5} open-door [creeper]) 3))
              choice (first (filter #(= :retreat.choice (:kind %)) @seen))
              cornered (filter #(= :retreat.cornered (:kind %)) @seen)]
          (is (false? (hs/door-open? p)) "the door is shut: it takes the creeper's line of sight")
          (is (= ["creeper 7"] (:blast choice)) "the flight says the creeper is within blast reach")
          (is (= [[] nil] [(:rooms choice) (:dropped choice)]) "and looks for no room: walls do not stop the blast")
          (is (seq cornered) "the shut door does not end the flight: it goes on searching and finds the room's far side no gain over standing (a pocket 2.5 blocks deep)"))))))

(def no-door {"5,64,2" "air" "5,65,2" "air"})

(deftest a-body-with-a-block-runs-into-its-doorless-hut-and-fills-the-doorway
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> (hut {:x 5.5 :y 64 :z 6.5} {} [(zombie 5.5 13.5 0.4)] no-door)
                        (assoc :inventory [{:name "cobblestone" :count 20}]))
              {:keys [p seen]} (await (flee! world))]
          (is (ran-home? seen) "the doorless hut is a run-to room")
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (= ["cobblestone" "cobblestone"] (mapv #(hs/block-at p %) [[5 64 2] [5 65 2]])) "the doorway is filled behind it"))))))

(deftest a-body-with-no-block-still-runs-into-its-doorless-hut-but-cannot-fill-the-doorway
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (flee! (hut {:x 5.5 :y 64 :z 6.5} {} [(zombie 5.5 13.5 0.4)] no-door)))]
          (is (ran-home? seen) "the walls still cut the threat's angles")
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (= ["air" "air"] (mapv #(hs/block-at p %) [[5 64 2] [5 65 2]])) "no block to fill the doorway with"))))))

(deftest a-zombie-at-the-door-is-not-run-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (hut {:x 5.5 :y 64 :z 9.5} hs/door-states [(zombie 5.5 3.5 nil)]))]
          (core/submit! eng (tu/order '(test.flight)) {})
          (await (core/tick! eng))
          (is (= [:race-lost] (room-whys seen)) "the flight says why the hut was no refuge")
          (is (> (:z (first (walk-targets eng))) 9) (str "the first step is not toward the zombie: " (walk-targets eng)))
          (is (not (in-room? (body p)))))))))

(deftest a-zombie-between-the-body-and-its-door-is-never-run-past
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (flee! (hut {:x 5.5 :y 64 :z 12.5} hs/door-states [(zombie 5.5 7.5 0.4)])))]
          (is (< 12 (:z (first (walk-targets eng)))) (str "the first step is away from the zombie: " (walk-targets eng)))
          (is (some? (:home-why (first (filter #(= :retreat.choice (:kind %)) @seen))))
              "the zombie between: the home is dropped (a run home later, once round it, is a race won)")
          (is (= [] (:list (core/state eng))) "the body gets away"))))))

(deftest any-cell-of-the-room-is-home
  (let [p (tu/seeing-all (tu/fake-on-floor (hut {:x 4.5 :y 64 :z -0.5} open-door [])))
        room (reach-room/room p home)]
    (is (rooms/inside? p room) "a room cell other than the home cell")
    (swap! (fake/state p) assoc-in [:self :pos] [5.5 64 6.5])
    (is (not (rooms/inside? p room)) "outside the hut")))

(def long-hut
  "The hut stretched north: walls x 3..7, z -8..2, roof y 67, the room x 4..6 z -7..1, the door at (5,64,2)."
  (merge (into {} (remove (fn [[k _]] (let [[x _ z] (map js/Number (.split k ","))] (and (<= 4 x 6) (<= -7 z 1)))))
               (tu/box 3 64 -8 7 66 2 "cobblestone"))
         (tu/box 3 67 -8 7 67 2 "cobblestone")
         (tu/box 3 63 -8 7 63 2 "cobblestone")
         hs/door))

(deftest in-its-room-off-the-home-cell-the-body-shuts-the-far-door-and-does-not-walk-to-the-home-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:pos {:x 5.5 :y 64 :z -2.5}} :states open-door :blocks long-hut
                     :entities [(zombie 5.5 5.5 0.4)] :floor [-40 -40 40 40] :home {:x 5 :y 64 :z -6}}
              {:keys [eng p]} (await (flee! world))]
          (is (not-any? #{{:x 5 :z -6}} (walk-targets eng)) (str "no run to the home cell: " (walk-targets eng)))
          (is (<= -7 (:z (body p)) 1) (str "inside the hut: " (body p)))
          (is (false? (hs/door-open? p)) "the door, 5 blocks off, is shut on the zombie")
          (is (= [] (:list (core/state eng)))))))))

(defn ^:async first-step [world]
  (let [{:keys [eng] :as s} (setup world)]
    (core/submit! eng (tu/order '(test.flight)) {})
    (await (core/tick! eng))
    s))

(deftest a-door-on-the-far-side-of-the-hut-counts-the-way-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (first-step (hut {:x 5.5 :y 64 :z -4.5} hs/door-states
                                                     [(assoc (zombie 12.5 -2.5 nil) :visible true)])))]
          (is (some? (first (walk-targets eng))) "it walks somewhere")
          (is (not (in-room? (mapv (or (first (walk-targets eng)) home) [:x :y :z])))
              (str "the way round to the door is longer than the zombie's, it flees first: " (walk-targets eng))))))))

(deftest a-hungry-body-does-not-race-as-if-sprinting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (fn [food] (assoc-in (hut {:x 5.5 :y 64 :z 13.5} hs/door-states [(zombie 11.5 14.5 nil)]) [:self :food] food))]
          (is (ran-home? (:seen (await (first-step (world 20))))) "fed, it sprints home")
          (is (not (ran-home? (:seen (await (first-step (world 6)))))) "hungry, it walks and loses"))))))

(defn sleep [ms] (js/Promise. (fn [r] (js/setTimeout r ms))))

(defn ^:async first-move
  "Submit the retreat over world and let its round run until it runs home or seals itself in (takes cover), then clear
  the mobs so the round ends."
  [world]
  (let [{:keys [eng p seen] :as s} (setup world)]
    (core/submit! eng (tu/order '(test.flight)) {})
    (let [round (core/tick! eng)
          ended (atom false)]
      (.then round #(reset! ended true))
      (loop []
        (when-not (or @ended (ran-home? seen) (some #(= :retreat_sealed (:kind %)) @seen))
          (await (sleep 20))
          (recur)))
      (swap! (fake/state p) assoc :entities [])
      (await round))
    s))

(defn first-move-kind
  "The flight's first move: the run home (:retreat.home, said before its walk), another walk (:planned) or a seal where
  it stands (:retreat_sealed)."
  [seen]
  (some (comp #{:retreat.home :planned :retreat_sealed} :kind) @seen))

(deftest a-low-body-in-a-skeletons-line-of-fire-takes-cover-before-running-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (fn [health] (-> (hut {:x 5.5 :y 64 :z 6.5} hs/door-states
                                          [{:id 9 :uuid "u9" :name "skeleton" :kind "hostile" :pos {:x 5.5 :y 64 :z 14.5}
                                            :health 20 :visible true}])
                                     (assoc-in [:self :health] health)
                                     (assoc :inventory [{:name "cobblestone" :count 20}])))]
          (is (= :retreat.home (first-move-kind (:seen (await (first-move (world 20)))))) "healthy, it runs home at once")
          (is (#{:planned :retreat_sealed} (first-move-kind (:seen (await (first-move (world 8))))))
              "low and under fire, a step to cover comes before any run across open ground"))))))

;; Any closed building the body has seen is a refuge, not only its own home (owner Q132).

(defn shift
  "Blocks or states keyed \"x,y,z\" moved dx along x."
  [m dx]
  (into {} (map (fn [[k v]] (let [[x y z] (.split k ",")] [(str (+ (js/Number x) dx) "," y "," z) v]))) m))

(deftest a-body-runs-into-a-closed-building-it-has-seen-that-is-not-its-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee! (assoc (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [(zombie 5.5 13.5 0.4)])
                                                       :home false)))]
          (is (ran-home? seen) "it runs to the building")
          (is (in-room? (body p)) (str "inside the building: " (body p)))
          (is (false? (hs/door-open? p)) "the door is shut behind it")
          (is (= [] (:list (core/state eng)))))))))

(deftest a-near-building-beats-a-home-it-cannot-reach-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far-hut (shift (merge hs/hut hut-floor) -24)
              world (-> (hut {:x 5.5 :y 64 :z 6.5} (merge hs/door-states (shift hs/door-states -24)) [(zombie 5.5 13.5 0.4)]
                             far-hut)
                        (assoc :home {:x -19 :y 64 :z 0}))
              {:keys [p]} (await (flee! world))]
          (is (in-room? (body p)) (str "inside the near building, not off toward home: " (body p))))))))

(defn seeing-all-but
  "p seeing every block (tu/seeing-all) except those in cells hidden? [x y z] keeps unknown."
  [p hidden?]
  (tu/seeing-all p)
  (let [blocks (.-blocks p) block-at (.-blockAt p)
        hid? (fn [pos] (hidden? [(js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos))]))]
    (aset p "seenBlocks" (fn [q] (.filter (.call blocks p q) #(not (hid? (.-pos %))))))
    (aset p "seenBlockAt" (fn [pos] (if (hid? pos)
                                      #js {:unknown true :pos pos}
                                      (let [b (.call block-at p pos)] #js {:name (.-name b) :properties (.-properties b) :pos pos :age-ms 0}))))
    (aset p "sensedAt" (fn [pos] (if (hid? pos) #js {:unknown true} (.call block-at p pos))))
    p))

(deftest a-building-the-body-has-not-seen-is-no-refuge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [(zombie 5.5 13.5 0.4)]) :home false)
              {:keys [eng p seen]} (setup world)
              _ (seeing-all-but p (fn [[x _ z]] (and (<= 3 x 7) (<= -2 z 2))))]
          (core/submit! eng (tu/order '(test.flight)) {})
          (await (tu/run-until-empty eng 6))
          (is (not (ran-home? seen)) "no run to a building it never saw")
          (is (not (in-room? (body p)))))))))

(deftest a-room-whose-flood-reaches-ground-it-has-not-seen-is-no-refuge
  (let [unseen-ground (fn [[x y z]] (and (= 63 y) (or (not (and (<= 2 x 8) (<= -3 z 3)))
                                                       (and (<= 4 x 6) (<= -1 z 1) (not= [5 0] [x z])))))
        seen-hut (fn [states extra] (seeing-all-but (tu/fake-on-floor (hut {:x 5.5 :y 64 :z 0.5} states [] extra)) unseen-ground))]
    (is (false? (rooms/refuge-room? (seen-hut {} {"5,64,2" "air" "5,65,2" "air"}) home))
        "a doorway with no door, open air round the hut over ground not seen past a block: no wall there, no refuge")
    (is (true? (rooms/refuge-room? (seen-hut open-door {}) home)) "a door shuts it: a refuge whatever lies outside, its own floor not seen")))

(deftest a-refuge-answer-is-kept-only-while-the-steady-key-holds
  (let [floods (atom 0)
        ask (fn [& keys]
              (reset! floods 0)
              (let [p #js {:perception {:st #js {}}}
                    key (atom nil)]
                (with-redefs [reach-room/steady-key (fn [_] @key)
                              reach-room/room-kind-at (fn [_ _] nil)
                              reach-room/shut-kind-at (fn [_] nil)
                              rooms/flood-refuge? (fn [_ _ _] (swap! floods inc) true)]
                  (doseq [k keys] (reset! key k) (rooms/refuge-room? p home))))
              @floods)]
    (is (= 1 (ask "near world unchanged" "near world unchanged")) "the same key keeps the answer")
    (is (= 2 (ask "before" "after")) "a moved key asks again")
    (is (= 2 (ask nil nil)) "with no steady key nothing is kept: a world that changes under the fake is read fresh")))

(defn counting!
  "Replace the var at (getter) via (setter f) with a wrapper counting calls in counts under k; returns the restore fn.
  A multi-arity fn (compiled calls use its arity entries) counts only its widest arity: the shorter ones call it."
  [counts k getter setter]
  (let [f (getter)
        arity (fn [n] (aget f (str "cljs$core$IFn$_invoke$arity$" n)))
        widest (last (filter arity [2 3 4]))
        count! #(swap! counts update k (fnil inc 0))
        w (fn [& args] (when-not widest (count!)) (apply f args))]
    (doseq [n [2 3 4] :when (arity n)]
      (aset w (str "cljs$core$IFn$_invoke$arity$" n)
            (fn [& args] (when (= n widest) (count!)) (apply (arity n) args))))
    (setter w)
    #(setter f)))

(def lone-doors
  "Six oak doors standing alone in the open field west of the hut: both sides of each flood open."
  (into {} (for [i (range 6) :let [x (- -6 (* 3 i))] half ["lower" "upper"]]
             [(str x "," (if (= half "lower") 64 65) ",6") "oak_door"])))

(def lone-door-states
  (into {} (for [i (range 6) :let [x (- -6 (* 3 i))] half ["lower" "upper"]]
             [(str x "," (if (= half "lower") 64 65) ",6") {:open false :half half :facing "south"}])))

(deftest the-room-search-per-flight-step-stays-within-the-chasers-lead
  ;; each step's fresh floods are CPU the body stands still for: priced against the lead, nearest first, the rest later
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [counts (atom {})
              priced (atom [])
              survey rooms/survey
              restores [(counting! counts :closed-room #(identity rooms/flood-refuge?) #(set! rooms/flood-refuge? %))
                        (counting! counts :walk-steps #(identity reach-room/walk-steps) #(set! reach-room/walk-steps %))
                        (counting! counts :steps #(identity retreat/flight-step!) #(set! retreat/flight-step! %))]
              wrapped (fn [c home opts]
                        (let [r (survey c home opts)]
                          (swap! priced conj [(:spent-s r) (rooms/here-s (:primitives c) (:threats opts))])
                          r))
              _ (aset wrapped "cljs$core$IFn$_invoke$arity$3" wrapped)
              _ (set! rooms/survey wrapped)
              {:keys [p]} (try (await (flee! (assoc (hut {:x 5.5 :y 64 :z 6.5} (merge hs/door-states lone-door-states)
                                                         [(zombie 5.5 13.5 0.4)] lone-doors)
                                                    :home false :perceive true)))
                               (finally (set! rooms/survey survey) (doseq [r restores] (r))))
              {:keys [closed-room walk-steps steps]} @counts]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (pos? steps))
          (is (seq @priced))
          (is (every? (fn [[spent lead]] (< spent lead)) @priced) (str "each survey's CPU within the chasers' lead at the body: " @priced))
          (is (<= closed-room (* 2 7)) (str "each of the 7 doors' sides flooded once a flight: " @counts))
          (is (<= walk-steps (count @priced)) (str "one walk search a survey to the one closed room's door: " @counts)))))))

(defn reading!
  "Count p's cell reads (sensedAt) in reads."
  [p reads]
  (let [f (.-sensedAt p)]
    (aset p "sensedAt" (fn [pos] (swap! reads inc) (.call f p pos)))))

(deftest a-body-in-its-home-room-hit-on-the-run-is-cornered-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [homes (atom [])
              hit cornered/hit-on-the-run!
              order choose/escape-order]
          (set! cornered/hit-on-the-run! (fn [_] true))
          (set! choose/escape-order (fn [m] (swap! homes conj (:home? m)) []))
          (try (await (flee! (hut {:x 5.5 :y 64 :z 0.5} hs/door-states [(zombie 5.5 -0.5 0.4)])))
               (finally (set! cornered/hit-on-the-run! hit)
                        (set! choose/escape-order order)))
          (is (= true (first @homes)) (str @homes)))))))

(defn seen-zombie [x z] #js {:name "zombie" :visible true :pos #js {:x x :y 64 :z z}})

(defn margin [] (cost-danger/race-margin-s))

(deftest the-race-margin-is-a-flights-median-start
  ;; a race at body-speed leaves out the seconds from the decision until the body gets going
  (is (= 2 (settings/with-settings {::cost-danger/flight-starts {"pooled" {:n 1 :deciles [0 0 0 0 1 3 9 9 9 9]}}}
             cost-danger/race-margin-s))))

(deftest a-chaser-3-blocks-off-leaves-lead-for-a-few-floods-less-the-race-margin
  ;; no distance gate: its lead at the body, less the race margin a race to a refuge is won by, prices the step's floods
  ;; (the search budget lifted: the lead alone is priced here). The body stands among the lone doors: no room is won
  ;; that the farther sides could not beat, and each side judged floods open ground to the cap (flood-s)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (assoc (hut {:x -13.5 :y 64 :z 10.5} (merge hs/door-states lone-door-states) [] lone-doors)
                                          :home false))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              zombie [(seen-zombie -13.5 13.5)]
              judged (fn [margin] (rooms/new-step! c)
                       (settings/with-settings no-ceiling
                         #(with-redefs [cost-danger/race-margin-s (constantly margin)]
                            (- (* 2 7) (count (filter (comp #{:unjudged} :why) (:dropped (cover/survey c zombie false))))))))
              ;; a side is judged while the floods before it (charged to the meter at the ceiling) and its own price fit the lead
              paid (fn [margin] (count (take-while #(< (+ (* % reach-room/room-cells (cost/room-read-ceiling-s)) (rooms/judge-s)) (- (rooms/here-s (:primitives eng) zombie) margin)) (range))))
              [with-margin without] [(judged (margin)) (judged 0)]]
          (is (= (paid (margin)) with-margin) "as many sides as the lead less the margin pays for")
          (is (pos? with-margin) (str "a zombie 3 blocks off leaves lead for a flood: " with-margin))
          (is (< with-margin without) (str "the race margin's share of the lead buys none: " [with-margin without])))))))

(deftest a-steps-fresh-floods-are-as-many-as-the-chasers-lead-pays-for
  ;; the lead, not a count, bounds a step's floods: a far chaser leaves time for every door side, one at the body for none.
  ;; The body stands among the lone doors, nearer them than the hut: no room won early saves the farther sides.
  ;; The search budget (cost/search-budget-s) is lifted: the lead alone is priced here.
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (assoc (hut {:x -13.5 :y 64 :z 10.5} (merge hs/door-states lone-door-states) [] lone-doors)
                                          :home false))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              p (:primitives eng)
              unjudged-at (fn [threats] (rooms/new-step! c) (settings/with-settings no-ceiling #(mapv :at (filter (comp #{:unjudged} :why) (:dropped (cover/survey c threats false))))))
              far [(seen-zombie -13.5 40.5)]
              pressed [(seen-zombie -13.5 9)]
              sides (* 2 7)]
          (is (> (- (rooms/here-s p far) (margin)) (* sides (rooms/judge-s))) "the far chaser's lead pays for every side")
          (is (= [] (unjudged-at far)) "so every door side is judged in the one step")
          (is (< (- (rooms/here-s p pressed) (margin)) (rooms/judge-s)) "one at the body leaves no lead for a flood")
          (is (= sides (count (unjudged-at pressed))) "so none is"))))))

(deftest a-steps-searches-stop-at-the-search-budget-whatever-the-threat
  ;; Q141: a step's searches together are one search of cost/search-budget-s: the flood running when it runs out is cut
  ;; there (its side unjudged), none after it starts. Each side judged floods open ground to the cap (flood-s)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (assoc (hut {:x -13.5 :y 64 :z 10.5} (merge hs/door-states lone-door-states) [] lone-doors)
                                          :home false))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              flood (* (rooms/flood-s) (/ (cost/room-read-ceiling-s) (cost/room-read-s)))
              budget (cost/search-budget-s)
              judged (fn [threats fire]
                       (rooms/new-step! c)
                       (let [r (with-redefs [cover/fire-on (constantly fire)] (cover/survey c threats false))]
                         [(- (* 2 7) (count (filter (comp #{:unjudged} :why) (:dropped r))))
                          (<= (- budget (cost/room-read-ceiling-s)) (:spent-s r) budget)
                          (= (:spent-s r) (rooms/step-spent-s c))]))]
          (is (< flood budget (* 7 flood)) "the budget fits a flood, fewer than the sides")
          (are [threats fire] (= [(js/Math.floor (/ budget flood)) true true] (judged threats fire))
            [(seen-zombie -13.5 40.5)] nil
            [] nil
            [#js {:name "skeleton" :visible true :pos #js {:x -13.5 :y 64 :z 20.5}}] {:exposed? true})
          (rooms/new-step! c)
          (cover/survey c [] false)
          (is (= [] (filter (comp #{:not-closed :unseen} :why) (:dropped (cover/survey c [] false))))
              "a second survey in the step judges nothing more: the step's searches are one"))))))

(deftest the-step-meter-charges-the-upper-bound-cell-prices
  (let [m (rooms/search-meter 0)]
    (is (= [(cost/room-read-ceiling-s) (cost/walk-cell-ceiling-s)] [(:read-s m) (:cell-s m)]) "a wall-time ceiling is metered at the slowest scene")
    (is (< (cost/room-read-s) (cost/room-read-ceiling-s)) "above the mean a flood is priced at")
    (is (< (cost/walk-cell-s) (cost/walk-cell-ceiling-s)) "above the mean a walk search is priced at")))

(deftest door-sides-left-unjudged-by-a-step-are-judged-on-a-later-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} (merge hs/door-states lone-door-states) [] lone-doors)
                                          :home false))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              far [(seen-zombie 40.5 6.5)]
              ;; at the hut's door: no race to the hut is won
              pressed [(seen-zombie 5.5 3.5)]
              lead (- (rooms/here-s (:primitives eng) pressed) (margin))
              ;; two fresh judgements fit the pressed body's lead, less the race margin
              survey (fn [threats keep?] (with-redefs [rooms/judge-s (constantly (/ lead 2.5))] (rooms/survey c nil {:threats threats :keep? keep?})))
              unjudged #(count (filter (comp #{:unjudged} :why) (:dropped %)))
              lone #(count (filter (fn [{[x] :at why :why}] (and (neg? x) (= :unjudged why))) (:dropped %)))
              dry [(unjudged (survey pressed false)) (unjudged (survey pressed false))]
              steps [(survey pressed true) (survey pressed true) (survey pressed true)]
              counts (mapv unjudged steps)]
          (is (apply = dry) (str "a survey that keeps nothing (the flee price's) leaves the next step the same door sides to judge: " dry))
          (is (pos? (last counts)) (str "the pressed lead does not pay for every side at once: " counts))
          (is (apply > counts) (str "each later step judges more: " counts))
          (is (= (lone (last steps)) (lone (survey far true))) "with the hut's race won, a lone door farther than its walk saves nothing"))))))

(deftest a-crowd-of-doors-under-a-skeletons-fire-is-judged-alike-whatever-its-hit-chance
  ;; nothing races a shooter, but each second the body stands searching is a second in its fire: the search and the run
  ;; it spares are both seconds under that fire, so its hit chance, sure or none, weighs them alike
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} (merge hs/door-states lone-door-states) [] lone-doors)
                                          :home false))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              skeleton [#js {:name "skeleton" :visible true :pos #js {:x 5.5 :y 64 :z 14.5}}]
              judged (fn [chance]
                       (rooms/new-step! c)
                       (with-redefs [cost/threat-parts (fn [& _] {:chance chance :death 1})]
                         (- (* 2 7) (count (filter (comp #{:unjudged} :why) (:dropped (cover/survey c skeleton false)))))))
              counts (mapv judged [0 0.5 1])]
          (is (:exposed? (cover/fire-on c skeleton)) "the body stands in the skeleton's line of fire")
          (is (pos? (first counts)) (str "door sides judged: " counts))
          (is (apply = counts) (str "alike at every hit chance: " counts)))))))

(deftest the-homes-judgements-run-within-the-steps-search-and-its-doorway-room-is-judged-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (hut {:x 5.5 :y 64 :z 6.5} {} [] no-door))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              counts (atom {})
              restore (counting! counts :unroofed #(identity reach-room/unroofed-cell) #(set! reach-room/unroofed-cell %))
              threats [(seen-zombie 5.5 20.5)]
              budget (cost/search-budget-s)
              [spent kept a first-floods b] (try (let [spent (rooms/survey c home {:threats threats :spent-s budget})
                                                       kept (select-keys (ctx/mem c) [:refuge-rooms :refuge-gap-rooms])
                                                       a (rooms/survey c home {:threats threats}) n (:unroofed @counts)]
                                                   [spent kept a n (rooms/survey c home {:threats threats})])
                                                 (finally (restore)))
              gap? (fn [r] (some :gap (:rooms r)))]
          (is (= [{:at [5 64 0] :own? true :why :unjudged}] (:dropped spent)) "a step whose search is spent judges not even the home")
          (is (= {} kept) "and keeps nothing of it")
          (is (and (gap? a) (gap? b)) "the doorless home is a room at the next steps")
          (is (< (:spent-s b) (:spent-s a) budget) (str "the first pays the home's flood and its doorway room, the next neither: " [(:spent-s a) (:spent-s b)]))
          (is (pos? first-floods))
          (is (= first-floods (:unroofed @counts)) (str "the doorway room's floods are not run again: " first-floods " " @counts)))))))

(deftest a-room-whose-walk-search-the-step-cannot-pay-is-unpriced-not-wayless
  ;; the meter cuts a walk search as it runs: a room it found no way to then is priced again at a later step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (hut {:x 5.5 :y 64 :z 6.5} hs/door-states []))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              threats [(seen-zombie 5.5 30.5)]
              rooms (mapv #(dissoc % :way-in) (:rooms (rooms/survey c home {:threats threats})))
              budget (cost/search-budget-s)
              late (rooms/verdicts p rooms threats true (- budget (* 3 (cost/walk-cell-ceiling-s))) nil)
              fresh (rooms/verdicts p rooms threats true 0 nil)]
          (is (= [true] (mapv :home? rooms)) "the home")
          (is (= [:unpriced] (mapv :why (:verdicts late))) "three cells left of the step's search: the home's walk search is cut")
          (let [held (- budget (* 3 (cost/walk-cell-ceiling-s)))
                v (rooms/verdicts p rooms threats true 0 nil held)]
            (is (= [:unpriced] (mapv :why (:verdicts v))) "the flee search's hold leaves three cells: the walk search to the door is cut, the flee search keeps the rest")
            (is (<= (:spent-s v) (- budget held)) (str (:spent-s v))))
          (do
            (rooms/hold-for-flee! c {:x 5.5 :y 64 :z 30.5} nil 0)
            (is (= (* (cost/search-cell-ceiling-s) (cost/plan-nodes-per-block) (u/dist (u/self-pos c) {:x 5.5 :y 64 :z 30.5})) (rooms/flee-hold-s c)) "a plan's measured nodes a block of the straight way are held for the flee search")
            (rooms/hold-for-flee! c {:x 5.5 :y 64 :z 30.5} {:x 5.5 :y 64 :z 3000.5} 0)
            (is (= (* (cost/search-cell-ceiling-s) (cost/plan-nodes-per-block) (u/dist (u/self-pos c) {:x 5.5 :y 64 :z 30.5})) (rooms/flee-hold-s c)) "a far goal adds nothing: the home leg is the one held")
            (rooms/hold-for-flee! c {:x 5.5 :y 64 :z 30.5} {:x 5.5 :y 64 :z 8.5} 0)
            (is (= (* (cost/search-cell-ceiling-s) (cost/plan-nodes-per-block) (u/dist (u/self-pos c) {:x 5.5 :y 64 :z 30.5})) (rooms/flee-hold-s c)) "a goal nearer than home does not shrink the home's hold")
            (rooms/hold-for-flee! c nil {:x 5.5 :y 64 :z 8.5} 0)
            (is (= (* (cost/search-cell-ceiling-s) (cost/plan-nodes-per-block) (u/dist (u/self-pos c) {:x 5.5 :y 64 :z 8.5})) (rooms/flee-hold-s c)) "no home: the goal's plan is held")
            (rooms/hold-for-flee! c {:x 5.5 :y 64 :z 3000.5} nil 0)
            (is (= (cost/search-budget-s) (rooms/flee-hold-s c)) "never more than the step's search")
            (rooms/hold-for-flee! c {:x 5.5 :y 64 :z 7.5} nil 20)
            (is (= (* (cost/search-cell-ceiling-s) (cost/plan-nodes-per-block) 20) (rooms/flee-hold-s c)) "a body beside its home still holds the ring's plans: the ring's reach is the longer way")
            (rooms/new-step! c)
            (is (zero? (rooms/flee-hold-s c)) "a new step starts with none held"))
          (is (<= (:spent-s late) budget) (str (:spent-s late)))
          (is (= [nil] (mapv :why (:verdicts fresh))) "a step with its search to spend runs home")
          (is (< 0 (:spent-s fresh) budget) (str (:spent-s fresh))))))))

(deftest a-body-beside-its-home-holds-the-flee-search-its-ring
  ;; Fenwick seq 2622008: the home 1.5 blocks off held ~1 ms, the surveys spent the step and the ring planned nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (hut {:x 5.5 :y 64 :z 7.5} hs/door-states [(zombie 5.5 12.5 0.4)]))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              home-leg (* (cost/search-cell-ceiling-s) (cost/plan-nodes-per-block) (u/dist (u/self-pos c) (cover/home-pos c)))]
          (cover/hold-for-flee! c)
          (is (< home-leg (rooms/flee-hold-s c)) "the hold is longer than the home leg alone")
          (is (<= (rooms/flee-hold-s c) (cost/search-budget-s))))))))

(deftest surveys-that-would-spend-the-step-leave-the-flee-search-its-home-plan
  ;; Brindle S9 j1747: the survey's walk searches spent the whole step meter, the flee search got 0 ms and planned nothing.
  ;; The way-in walk search here eats all the survey's meter; the hut's open door lies 25 blocks off, the goal nearer or farther.
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (hut {:x 5.5 :y 64 :z 26.5} open-door []))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              threats [(seen-zombie 5.5 33.5)]
              step! (fn ^:async s [goal]
                      (rooms/new-step! c)
                      (ctx/update-mem! c assoc :goal goal)
                      (cover/hold-for-flee! c)
                      (with-redefs [rooms/way-in (fn [_ _] (reach/charge! (reach/metered-cap 1e9 :cell-s) :cell-s true) nil)]
                        (cover/survey c threats false))
                      (let [left (- (cost/search-budget-s) (rooms/step-spent-s c))
                            judged (await (respond.goal/judge! c threats [home] nil nil nil nil))]
                        [(< left (+ (rooms/flee-hold-s c) (cost/walk-cell-ceiling-s))) (:home-why judged)]))
              nearer (await (step! [5 64 21]))
              farther (await (step! [5 64 60]))]
          (is (= [[true nil] [true nil]] [nearer farther]) "the surveys leave only the hold, and the flee search plans the home on it"))))))

(defn ^:async holds-at-home-room
  "The flee hold (rooms/flee-hold-s) at each home-room! call of job spec's run over a zombie 6 blocks from the hut."
  [spec]
  (let [home-room rooms/home-room!
        holds (atom [])
        _ (set! rooms/home-room! (fn [c home r] (swap! holds conj (rooms/flee-hold-s c)) (home-room c home r)))
        {:keys [eng]} (setup (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [(zombie 5.5 12.5 0.4)]))]
    (try (core/submit! eng (tu/order spec) {})
         (await (tu/run-until-empty eng 2))
         (finally (set! rooms/home-room! home-room)))
    @holds))

(deftest a-flight-step-holds-the-flee-search-its-cpu-before-it-looks-for-the-home-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [holds (await (holds-at-home-room '(test.flight)))]
          (is (seq holds))
          (is (every? pos? holds) (str "the first search of the step, after the hold: " holds)))))))

(deftest respond-to-hostiles-flee-price-holds-the-flee-search-its-cpu-before-it-looks-for-the-home-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [holds (await (holds-at-home-room '(jobs.survival.respond-to-hostile)))]
          (is (seq holds))
          (is (every? pos? holds) (str "the first search of the step, after the hold: " holds)))))))

(deftest a-known-closed-home-whose-room-flood-is-cut-is-unjudged-not-dropped-silently
  ;; card 9ccd6f0c: kept closed in job memory, its room still floods for its cells; a cut leaves it to a later step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (hut {:x 5.5 :y 64 :z 6.5} hs/door-states []))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              threats [(seen-zombie 5.5 30.5)]
              first-step (rooms/survey c home {:threats threats})
              late (rooms/survey c home {:threats threats :spent-s (- (cost/search-budget-s) (* 3 (cost/room-read-s)))})]
          (is (= [true] (mapv :home? (:rooms first-step))) "the home is a closed room")
          (is (true? (get-in (ctx/mem c) [:refuge-rooms (rooms/cell-key home)])) "kept closed for the flight")
          (is (= [] (:rooms late)) "three cells left: its room's flood is cut")
          (is (some #(= {:at [5 64 0] :own? true :why :unjudged} %) (:dropped late)) (str "and the home says so: " (:dropped late))))))))

(deftest a-body-a-step-inside-its-open-door-stands-in-its-home-room-one-outside-does-not
  ;; Fenwick j363 (run4 F24): one step inside the open door, the cut search left home and goal unjudged and the body walked out
  (are [pos in?] (= in? (let [{:keys [eng]} (setup (hut pos open-door []))
                              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
                              _ (.setOwner (:primitives eng) "tx")]
                          (rooms/home-room! c home (cornered/close-radius))))
    {:x 5.5 :y 64 :z 1.5} true
    {:x 4.5 :y 64 :z -0.5} true
    {:x 5.5 :y 64 :z 5.5} false
    {:x 15.5 :y 64 :z 0.5} false))

(deftest a-goal-a-cut-search-left-unjudged-is-no-leg-out-of-the-home-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (hut {:x 5.5 :y 64 :z 1.5} open-door []))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              _ (ctx/update-mem! c assoc :goal [5 64 12])
              threats [(seen-zombie 5.5 -6.5)]
              judge! (fn ^:async j [] (rooms/spent! c (cost/search-budget-s) :test) (await (respond.goal/judge! c threats [home] home nil nil nil)))
              unasked (await (judge!))
              _ (rooms/new-step! c)
              in? (rooms/home-room! c home (cornered/close-radius))
              inside (await (judge!))]
          (is (true? in?))
          (is (= [nil :unjudged] (mapv :why [unasked inside])) (str "a cut plan is no reason against the goal, but in the home room it is: " (mapv #(select-keys % [:goal :why]) [unasked inside]))))))))

(deftest the-flee-prices-home-judgement-serves-the-flights-first-step-from-the-same-cell
  ;; Fenwick j363/j381 (run4 F24): the price's survey kept nothing, so the step after it floods and walk-searches home again on the meter they share
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} hs/door-states []) :perceive true))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              threats [(seen-zombie 5.5 30.5)]
              price (rooms/survey c home {:threats threats :keep? false})
              step (rooms/survey c home {:threats threats})]
          (is (= [true true] (mapv (comp :home? first :rooms) [price step])) "the home is a closed room both times")
          (is (true? (get-in (ctx/mem c) [:refuge-rooms (rooms/cell-key home)])) "the price keeps the home's judgement")
          (is (pos? (:ways-s price)) "the price walk-searches the way in")
          (is (= [0 0] [(:ways-s step) (:spent-s step)]) (str "the step from the same cell runs neither again: " (select-keys step [:spent-s :ways-s]))))))))

(deftest a-way-in-found-while-another-doors-search-was-cut-is-not-kept-as-the-best
  ;; card 9ccd6f0c: the cut door may hold the shorter way, so verdicts prices the room again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (hut {:x 5.5 :y 64 :z 6.5} hs/door-states []))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              threats [(seen-zombie 5.5 30.5)]
              [room walk] [reach-room/room reach-room/walk-steps]
              searches (atom 0)
              r (try (set! reach-room/room (fn [p pos] (some-> (room p pos) (update :doors #(into (vec %) %)))))
                     (set! reach-room/walk-steps (let [f (fn [] (if (= 1 (swap! searches inc)) 7 (do (reach/charge! 1 :cell-s true) nil)))]
                                                                 (fn ([_ _ _] (f)) ([_ _ _ _] (f)))))
                     (first (:rooms (rooms/survey c home {:threats threats})))
                     (finally (set! reach-room/room room) (set! reach-room/walk-steps walk)))]
          (is (= 2 @searches) "one door's search found a way, the other's was cut")
          (is (:home? r))
          (is (not (contains? r :way-in)) "no way-in kept for verdicts to take as the best"))))))

(deftest an-open-home-is-flooded-once-across-flights
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [open-home {:x -10 :y 64 :z 0}
              {:keys [eng p]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} hs/door-states []) :home false))
              reads (atom 0)
              _ (reading! p reads)
              flight (fn [root token]
                       (let [c (core/make-ctx eng {:root root :slots [] :chain [root] :token token :args {} :round 1})
                             _ (.setOwner (:primitives eng) token)
                             before @reads]
                         (rooms/survey c open-home)
                         (- @reads before)))
              first-flight (flight "j8" "t8")
              second-flight (flight "j9" "t9")]
          (is (pos? first-flight))
          (is (< second-flight (/ first-flight 10)) (str "cells read by the first flight " first-flight ", the next " second-flight)))))))

(deftest a-room-with-an-unseen-cell-is-judged-again-once-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hidden (atom true)
              {:keys [eng p]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} hs/door-states []) :home false))
              _ (seeing-all-but p (fn [[x y z]] (and @hidden (= [5 64 0] [x y z]))))
              c (core/make-ctx eng {:root "j8" :slots [] :chain ["j8"] :token "t8" :args {} :round 1})
              _ (.setOwner (:primitives eng) "t8")
              home-why #(some (fn [d] (when (= [5 64 0] (:at d)) (:why d))) (:dropped (rooms/survey c home)))]
          (is (= :unseen (home-why)) "a cell not seen leaves the home neither closed nor open")
          (reset! hidden false)
          (swap! (fake/state p) update :blocks #(into {} %))
          (is (nil? (home-why)) "seen, the home is a refuge")
          (is (= 1 (count (:rooms (rooms/survey c home))))))))))

(deftest a-body-at-a-buildings-door-looks-at-each-unseen-side-before-it-drops-the-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hidden (atom #{[5 64 0] [6 64 0]})
              looks (atom 0)
              {:keys [eng p seen]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [(zombie 5.5 13.5 0.4)]) :home false))
              _ (seeing-all-but p (fn [cell] (contains? @hidden cell)))
              _ (.override (.-world p) "look" (fn ^:async o [token args impl]
                                                (swap! looks inc)
                                                (reset! hidden (disj @hidden (let [q (.-pos args)] [(js/Math.floor (.-x q)) 64 (js/Math.floor (.-z q))])))
                                                (swap! (fake/state p) update :blocks #(into {} %))
                                                (await (impl token args))))]
          (core/submit! eng (tu/order '(test.flight)) {})
          (await (tu/run-until-empty eng 6))
          (is (empty? @hidden) "every unseen side was looked at")
          (is (<= 2 @looks) "one look cannot show two sides")
          (is (ran-home? seen) "the room seen, it runs in")
          (is (in-room? (body p)) (str "inside the building: " (body p))))))))

(deftest a-creeper-chase-does-not-take-the-body-to-a-doorway-to-look-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hidden (atom #{[5 64 0]})
              looks (atom 0)
              creeper (assoc (zombie 5.5 20.5 0.4) :name "creeper")
              {:keys [eng p]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [creeper]) :home false))
              _ (seeing-all-but p (fn [cell] (contains? @hidden cell)))
              _ (.override (.-world p) "look" (fn ^:async o [token args impl]
                                                (swap! looks inc)
                                                (await (impl token args))))]
          (core/submit! eng (tu/order '(test.flight)) {})
          (await (tu/run-until-empty eng 6))
          (is (zero? @looks) "a creeper behind is not led to a door for a look"))))))

(defn shifted
  "The cell-keyed map m moved dx blocks along x."
  [m dx]
  (into {} (map (fn [[k v]] (let [[x y z] (.split k ",")] [(str (+ dx (js/parseInt x)) "," y "," z) v]))) m))

(deftest a-room-nearer-than-the-home-is-the-first-the-flight-races-for
  ;; the home is ranked by its distance like any room: home first would put a far home before a room by the body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [near-hut (shifted (merge hs/walls hs/roof hs/door hut-floor) -20)
              {:keys [eng]} (setup (hut {:x -14.5 :y 64 :z 6.5} (merge hs/door-states (shifted hs/door-states -20)) [] near-hut))
              c (core/make-ctx eng {:root "j8" :slots [] :chain ["j8"] :token "t8" :args {} :round 1})
              _ (.setOwner (:primitives eng) "t8")
              rooms (:rooms (settings/with-settings no-ceiling #(rooms/survey c (cover/home-pos c))))]
          (is (= [false true] (mapv :home? rooms)) (str "the room 7 blocks off, then the home 20 off: " (mapv :start rooms))))))))

(deftest a-fourth-room-the-run-wins-is-found-past-three-nearer-ones
  ;; rooms are cut by their price, not by a count: three nearer huts whose doors are walled off leave the fourth (the
  ;; search budget lifted: here the price alone is the cut)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dxs [0 -8 -16 -24]
              huts (apply merge (for [dx dxs] (shifted (merge hs/walls hs/roof hs/door hut-floor) dx)))
              walled (apply merge (for [dx (butlast dxs)] (shifted {"5,64,3" "cobblestone" "5,65,3" "cobblestone"} dx)))
              {:keys [eng p]} (setup (assoc (hut {:x 8.5 :y 64 :z 6.5} (apply merge (for [dx dxs] (shifted hs/door-states dx))) []
                                                 (merge huts walled))
                                            :home false))
              c (core/make-ctx eng {:root "j8" :slots [] :chain ["j8"] :token "t8" :args {} :round 1})
              _ (.setOwner (:primitives eng) "t8")
              threats [(seen-zombie 40.5 6.5)]
              [survey verdicts] (settings/with-settings no-ceiling
                                  #(let [s (rooms/survey c nil {:threats threats})] [s (:verdicts (rooms/verdicts p (:rooms s) threats true (:spent-s s) nil))]))
              hut-of (fn [r] (some #(when (contains? (:cells r) [(+ 5 %) 64 0]) %) dxs))]
          (is (= dxs (mapv (comp hut-of :room) verdicts)) (str "all four rooms, nearest first: " (mapv :start (:rooms survey))))
          (is (= [:no-way :no-way :no-way nil] (mapv :why verdicts)) (str (mapv :why verdicts)))
          (is (= -24 (hut-of (:room (rooms/pick verdicts)))) "the run is to the fourth"))))))

(deftest a-home-past-64-blocks-is-still-the-flights-home
  ;; its run is priced by the flight's search like a near one's, not cut by a range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} hs/door-states []) :home {:x 105 :y 64 :z 0}))
              c (core/make-ctx eng {:root "j8" :slots [] :chain ["j8"] :token "t8" :args {} :round 1})]
          (is (= {:x 105 :y 64 :z 0} (cover/home-pos c))))))))

(deftest a-look-through-the-homes-door-is-not-gated-by-the-flood-survey-did-not-gate
  ;; survey judges the home whatever its search costs; a home door side it left :unjudged is looked into on the same rule
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hidden (atom #{[5 64 0] [5 64 1]})
              {:keys [eng p]} (setup (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [(zombie 5.5 13.5 0.4)]))
              _ (seeing-all-but p (fn [cell] (contains? @hidden cell)))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              threats [(seen-zombie 5.5 13.5)]
              s (rooms/survey c home {:threats threats})
              cut (update s :dropped (fn [ds] (mapv #(if (:own? %) % (assoc % :why :unjudged)) ds)))
              pays cost/refuge-search-pays?
              looks (do (set! cost/refuge-search-pays? (constantly false))
                        (try (cover/building-looks c cut threats) (finally (set! cost/refuge-search-pays? pays))))]
          (is (some :own? (:dropped s)) "the hidden home is dropped unseen")
          (is (seq looks) "its door sides are looked into though no flood pays"))))))

(deftest a-strangers-look-whose-boundary-flood-is-kept-is-not-gated-by-the-flood-price
  ;; the look's floods cost nothing once kept, as survey's kept sides: a price that refuses a fresh flood does not refuse it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hidden (atom #{[5 64 0] [5 64 1]})
              {:keys [eng p]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [(zombie 5.5 13.5 0.4)]) :home false))
              _ (seeing-all-but p (fn [cell] (contains? @hidden cell)))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              threats [(seen-zombie 5.5 13.5)]
              s (rooms/survey c nil {:threats threats})
              pays cost/refuge-search-pays?
              kept-fn reach-room/boundary-kept?
              looks (fn [kept?]
                      (set! cost/refuge-search-pays? (constantly false))
                      (set! reach-room/boundary-kept? (constantly kept?))
                      (try (cover/building-looks c s threats)
                           (finally (set! cost/refuge-search-pays? pays) (set! reach-room/boundary-kept? kept-fn))))]
          (is (some (comp #{:unseen} :why) (:dropped s)) "a stranger's building is dropped unseen")
          (is (empty? (looks false)) "a fresh flood that does not pay is not looked into")
          (is (seq (looks true)) "the same look once its floods are kept is free"))))))

(deftest a-boundarys-floods-are-kept-once-it-is-read
  (let [{:keys [p]} (ht/setup-seeing (dissoc (hut {:x 5.5 :y 64 :z 5.5} hs/door-states []) :floor) nil 1000)
        door {:x 5 :y 64 :z 2}]
    (is (false? (reach-room/boundary-kept? p door)))
    (reach-room/boundary p door)
    (is (true? (reach-room/boundary-kept? p door)))))

(deftest a-decision-past-its-deadline-still-gives-the-flee-search-its-hold
  ;; Sorrel skeleton death (card 24286035): the covers' quotes, which no meter stops, ran past the decision's deadline,
  ;; every flee search then planned 0 nodes and flee was priced on a guess
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (hut {:x 5.5 :y 64 :z 26.5} open-door []))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              threats [(seen-zombie 5.5 33.5)]
              _ (rooms/new-step! c)
              _ (cover/hold-for-flee! c)
              _ (ctx/update-mem! c assoc :decision-deadline (- (clock/now) 1))
              judged (await (respond.goal/judge! c threats [home] nil nil nil nil))]
          (is (pos? (rooms/flee-hold-s c)))
          (is (pos? (:nodes (meta (:ends judged)))) "the flee search plans on its held share")
          (is (nil? (:home-why judged)) "the home is judged, not left :unjudged"))))))

(deftest a-flee-search-that-planned-nothing-prices-its-way-at-the-mean-a-search-judges
  ;; Sorrel skeleton death (card 7915bb83): every flee search planned 0 nodes, yet the flee was priced as if a way into
  ;; the unseen were there (1 - way-on-chance, 0.004), below what a search judges on average
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:home false :entities [(zombie 6.5 0.5 nil)]})
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              _ (rooms/new-step! c)
              _ (ctx/update-mem! c assoc :search-s (cost/search-budget-s))
              r (await (cover/caught-chance! c nil))]
          (is (= (caught-prior/judged-way-chance) (:chance r)) "the step's search spent: the flee is priced at the measured mean, not as a way found"))))))
