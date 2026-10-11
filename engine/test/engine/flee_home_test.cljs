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
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.cost :as cost]
            [jobs.lib.reach :as reach]
            [jobs.survival.retreat :as retreat]
            [jobs.survival.retreat-home :as retreat-home]))

;; The hut of hut-shelter-test: walls x 3..7, z -2..2, roof y 67, room x 4..6 z -1..1, a door at (5,64,2) facing south.

(def home {:x 5 :y 64 :z 0})

(def open-door
  {"5,64,2" {:open true :half "lower" :facing "south"} "5,65,2" {:open true :half "upper" :facing "south"}})

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (dissoc world :home)))
        now (tu/act-clock clock p 1000)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    (when-not (false? (:home world)) (mem/write! (:store eng) :home {:pos (:home world home)} {:cap 1 :ttl :forever}))
    {:eng eng :p p :seen seen}))

(defn ^:async flee! [world]
  (let [{:keys [eng] :as s} (setup world)]
    (core/submit! eng '(jobs.survival.retreat) {})
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
  (let [order (retreat/escape-order {:home? true})]
    (is (not-any? #{:pit :pocket} order) (str order))
    (is (= :seal (first order)))))

(deftest a-hut-is-a-closed-room-once-its-doors-are-shut-and-open-ground-is-not
  (let [p (tu/seeing-all (tu/fake-on-floor (hut {:x 5.5 :y 64 :z 0.5} open-door [])))]
    (is (true? (reach/closed-room? p home)) "an open door counts as one that can be shut")
    (is (false? (reach/closed-room? p {:x 15 :y 64 :z 0})) "the open field outside")
    (is (false? (reach/closed-room? (tu/seeing-all (tu/fake-on-floor (hut {:x 5.5 :y 64 :z 0.5} {} []
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

(deftest a-zombie-at-the-door-is-not-run-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (hut {:x 5.5 :y 64 :z 9.5} hs/door-states [(zombie 5.5 3.5 nil)]))]
          (core/submit! eng '(jobs.survival.retreat) {})
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
        room (reach/room p home)]
    (is (retreat-home/inside? p room) "a room cell other than the home cell")
    (swap! (fake/state p) assoc-in [:self :pos] [5.5 64 6.5])
    (is (not (retreat-home/inside? p room)) "outside the hut")))

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
    (core/submit! eng '(jobs.survival.retreat) {})
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
    (core/submit! eng '(jobs.survival.retreat) {})
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
          (core/submit! eng '(jobs.survival.retreat) {})
          (await (tu/run-until-empty eng 6))
          (is (not (ran-home? seen)) "no run to a building it never saw")
          (is (not (in-room? (body p)))))))))

(defn counting!
  "Replace the var at (getter) via (setter f) with a wrapper counting calls in counts under k; returns the restore fn."
  [counts k getter setter]
  (let [f (getter)]
    (setter (fn [& args] (swap! counts update k (fnil inc 0)) (apply f args)))
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
              restores [(counting! counts :closed-room #(identity reach/closed-room?) #(set! reach/closed-room? %))
                        (counting! counts :room #(identity reach/room) #(set! reach/room %))
                        (counting! counts :walk-steps #(identity reach/walk-steps) #(set! reach/walk-steps %))
                        (counting! counts :steps #(identity retreat/flight-step!) #(set! retreat/flight-step! %))]
              {:keys [p]} (try (await (flee! (assoc (hut {:x 5.5 :y 64 :z 6.5} (merge hs/door-states lone-door-states)
                                                         [(zombie 5.5 13.5 0.4)] lone-doors)
                                                    :home false)))
                               (finally (doseq [r restores] (r))))
              {:keys [closed-room room walk-steps steps]} @counts]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (pos? steps))
          (is (<= closed-room (* 2 (retreat-home/max-doors))) (str "each door side flooded once a flight: " @counts))
          (is (<= room (+ closed-room (* steps (retreat-home/max-rooms)))) (str "rooms rebuilt per step: " @counts))
          (is (<= walk-steps (* steps (retreat-home/max-rooms))) (str "one walk search per room door per step: " @counts)))))))
