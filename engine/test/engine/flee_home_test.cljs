(ns engine.flee-home-test
  "A chased body near its own closed hut runs home and shuts the door when it gets there before the chasers (Juniper,
  2026-10-08: in its doorway with a zombie outside, the flight's straight lines ran out past the zombie and the body
  died far from the hut). A zombie standing at the door is not run into."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.hut-shelter-test :as hs]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.flight-job :as fj]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.cost :as cost]
            [jobs.lib.reach.room :as reach-room]
            [jobs.survival.respond-flight :as retreat]
            [jobs.survival.retreat-cornered :as cornered]
            [jobs.lib.rooms :as rooms]))

;; The hut of hut-shelter-test: walls x 3..7, z -2..2, roof y 67, room x 4..6 z -1..1, a door at (5,64,2) facing south.

(def home {:x 5 :y 64 :z 0})

(def open-door
  {"5,64,2" {:open true :half "lower" :facing "south"} "5,65,2" {:open true :half "upper" :facing "south"}})

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (dissoc world :home)))
        now (tu/act-clock clock p 1000)
        eng (core/create {:primitives p :jobs fj/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    (when-not (false? (:home world)) (mem/write! (:store eng) :home {:pos (:home world home)} {:cap 1 :ttl :forever}))
    {:eng eng :p p :seen seen}))

(defn ^:async flee! [world]
  (let [{:keys [eng] :as s} (setup world)]
    (core/submit! eng '(test.flight) {})
    (await (tu/run-until-empty eng 6))
    s))

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

(deftest the-race-to-a-refuge-is-won-with-a-margin-at-the-bodys-own-speed
  (are [body-dist mob-dists food first?] (= first? (cost/reach-first? body-dist mob-dists food))
    2 [3] 20 true
    6.5 [13] 20 true
    8 [3] 20 false
    8 [] 20 true
    5 [3 30] 20 false
    10 [8] 20 true
    10 [8] 6 false))

(deftest a-flank-turn-is-taken-only-when-the-body-still-gains-on-a-chaser
  (are [food degrees gains?] (= gains? (cost/outruns-at? food degrees))
    20 60 false
    20 45 true
    6 60 false
    20 0 true))

(deftest cornered-at-home-the-body-never-digs-its-own-hut
  (is (= [:plug :pillar :back-off :fight] (cornered/escape-order {:home? true}))))

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
        (let [{:keys [eng p]} (await (flee! (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [(zombie 5.5 13.5 0.4)])))]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (false? (hs/door-open? p)) "the door is shut behind it")
          (is (= [] (:list (core/state eng)))))))))

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
              {:keys [p seen]} (await (flee! (hut {:x 5.5 :y 64 :z 1.5} open-door [creeper])))
              choice (first (filter #(= :retreat.choice (:kind %)) @seen))
              steps (mapv :target (filter #(= :retreat.target (:kind %)) @seen))]
          (is (false? (hs/door-open? p)) "the door is shut: it takes the creeper's line of sight")
          (is (= ["creeper 7"] (:blast choice)) "the flight says the creeper is within blast reach")
          (is (= [[] nil] [(:rooms choice) (:dropped choice)]) "and looks for no room: walls do not stop the blast")
          (is (= [-1] (distinct (map #(nth % 2) steps)))
              (str "the shut door does not end the flight: it steps to the far side of the room, " steps)))))))

(deftest a-zombie-at-the-door-is-not-run-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (hut {:x 5.5 :y 64 :z 9.5} hs/door-states [(zombie 5.5 3.5 nil)]))]
          (core/submit! eng '(test.flight) {})
          (await (core/tick! eng))
          (is (= [:race-lost] (room-whys seen)) "the flight says why the hut was no refuge")
          (is (> (:z (first (walk-targets eng))) 9) (str "the first step is not toward the zombie: " (walk-targets eng)))
          (is (not (in-room? (body p)))))))))

(deftest a-zombie-between-the-body-and-its-door-is-flanked-never-run-past
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (flee! (hut {:x 5.5 :y 64 :z 12.5} hs/door-states [(zombie 5.5 7.5 0.4)])))]
          (is (<= 2 (js/Math.abs (- (:x (first (walk-targets eng))) 5)) 5) (str "the first step turns toward the hut's side: " (walk-targets eng)))
          (is (< 12 (:z (first (walk-targets eng)))) (str "and still away from the zombie: " (walk-targets eng)))
          (is (not (ran-home? seen)) "the zombie stays nearer the door than the body's way round")
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
    (core/submit! eng '(test.flight) {})
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
        (let [world (fn [food] (assoc-in (hut {:x 5.5 :y 64 :z 13.5} hs/door-states [(zombie 11.5 8.5 nil)]) [:self :food] food))]
          (is (ran-home? (:seen (await (first-step (world 20))))) "fed, it sprints home")
          (is (not (ran-home? (:seen (await (first-step (world 6)))))) "hungry, it walks and loses"))))))

(defn sleep [ms] (js/Promise. (fn [r] (js/setTimeout r ms))))

(defn ^:async first-move
  "Submit the retreat over world and let its round run until it runs home or seals itself in (takes cover), then clear
  the mobs so the round ends."
  [world]
  (let [{:keys [eng p seen] :as s} (setup world)]
    (core/submit! eng '(test.flight) {})
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
          (core/submit! eng '(test.flight) {})
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

(deftest the-room-search-per-flight-step-stays-bounded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [counts (atom {})
              restores [(counting! counts :closed-room #(identity rooms/refuge-room?) #(set! rooms/refuge-room? %))
                        (counting! counts :room #(identity reach-room/room) #(set! reach-room/room %))
                        (counting! counts :walk-steps #(identity reach-room/walk-steps) #(set! reach-room/walk-steps %))
                        (counting! counts :steps #(identity retreat/flight-step!) #(set! retreat/flight-step! %))]
              {:keys [p]} (try (await (flee! (assoc (hut {:x 5.5 :y 64 :z 6.5} (merge hs/door-states lone-door-states)
                                                         [(zombie 5.5 13.5 0.4)] lone-doors)
                                                    :home false)))
                               (finally (doseq [r restores] (r))))
              {:keys [closed-room room walk-steps steps]} @counts]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (pos? steps))
          (is (<= closed-room (* 2 (rooms/max-doors))) (str "each door side flooded once a flight: " @counts))
          (is (<= room (+ closed-room (* steps (rooms/max-rooms)))) (str "rooms rebuilt per step: " @counts))
          (is (<= walk-steps (* steps (rooms/max-rooms))) (str "one walk search per room door per step: " @counts)))))))

(defn reading!
  "Count p's cell reads (sensedAt) in reads."
  [p reads]
  (let [f (.-sensedAt p)]
    (aset p "sensedAt" (fn [pos] (swap! reads inc) (.call f p pos)))))

(deftest a-close-melee-chaser-gets-a-first-step-that-floods-no-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [reads (atom 0)
              per-step (atom [])
              step retreat/flight-step!
              world (assoc (hut {:x 5.5 :y 64 :z 6.5} (merge hs/door-states lone-door-states) [(zombie 5.5 9.5 0.4)] lone-doors)
                           :home false)
              {:keys [eng p]} (setup world)
              _ (reading! p reads)
              _ (set! retreat/flight-step! (fn [c] (let [before @reads]
                                                     (.then (step c) (fn [r] (swap! per-step conj (- @reads before)) r)))))]
          (core/submit! eng '(test.flight) {})
          (try (await (tu/run-until-empty eng 2))
               (finally (set! retreat/flight-step! step)))
          (let [flood (let [before @reads] (reach-room/closed-room? p {:x -6 :y 64 :z 5}) (- @reads before))]
            (is (seq @per-step))
            (is (< (first @per-step) flood) (str "cells read by the first step, fewer than one open room flood (" flood "): " @per-step))))))))

(deftest a-body-in-its-home-room-hit-on-the-run-is-cornered-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [homes (atom [])
              hit cornered/hit-on-the-run!
              order cornered/escape-order]
          (set! cornered/hit-on-the-run! (fn [_] true))
          (set! cornered/escape-order (fn [m] (swap! homes conj (:home? m)) []))
          (try (await (flee! (hut {:x 5.5 :y 64 :z 0.5} hs/door-states [(zombie 5.5 -0.5 0.4)])))
               (finally (set! cornered/hit-on-the-run! hit)
                        (set! cornered/escape-order order)))
          (is (= true (first @homes)) (str @homes)))))))

(deftest door-sides-left-unjudged-by-a-step-are-judged-on-a-later-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (assoc (hut {:x 5.5 :y 64 :z 6.5} (merge hs/door-states lone-door-states) [] lone-doors)
                                          :home false))
              c (core/make-ctx eng {:root "j9" :slots [] :chain ["j9"] :token "tx" :args {} :round 1})
              _ (.setOwner (:primitives eng) "tx")
              unjudged #(count (filter (comp #{:unjudged} :why) (:dropped (rooms/survey c nil %))))
              counts [(unjudged 0) (unjudged 2) (unjudged 2)]]
          (is (pos? (first counts)) (str "none judged at first: " counts))
          (is (apply > counts) (str "each later step judges more: " counts)))))))

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
                                                (await (impl token args))))]
          (core/submit! eng '(test.flight) {})
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
          (core/submit! eng '(test.flight) {})
          (await (tu/run-until-empty eng 6))
          (is (zero? @looks) "a creeper behind is not led to a door for a look"))))))
