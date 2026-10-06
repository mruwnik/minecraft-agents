(ns engine.hut-shelter-test
  "A body inside its own roofed hut with a shut door (game-agent bugs #33/#42): the unstick job leaves through the door
  instead of pillaring in the room and digging the roof, the night trigger holds only when the roof is really
  gone, and dig-in mends a hole in the roof of a closed room instead of walling the body in at feet and head height."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.jobs.shelter :as sh]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.shelter-test :as st]
            [engine.test-util :as tu :refer [box]]
            [engine.unstick-test :as ut]
            [engine.triggers :as triggers]
            [engine.world :as ew]
            [jobs.survival.dig-in :as dig-in]))

;; A cobblestone hut: walls x 3..7, z -2..2, y 64..66, roof at y 67, the room x 4..6, z -1..1. A shut oak door in the
;; south wall at (5,64,2)/(5,65,2); a chest and a torch on either side of the cell inside the door, as in the bug's hut.

(def walls
  (into {} (remove (fn [[k _]] (let [[x _ z] (map js/Number (.split k ","))] (and (<= 4 x 6) (<= -1 z 1)))))
        (box 3 64 -2 7 66 2 "cobblestone")))

(def roof (box 3 67 -2 7 67 2 "cobblestone"))

(def door {"5,64,2" "oak_door" "5,65,2" "oak_door"})

(def door-states {"5,64,2" {:open false :half "lower" :facing "south"} "5,65,2" {:open false :half "upper" :facing "south"}})

(def furniture {"4,64,1" "chest" "6,64,1" "torch"})

(def hut (merge walls roof door furniture))

(defn hut-world
  "The hut on the walk floor with the body at pos; extra blocks merged over it (\"air\" removes one)."
  [pos extra]
  {:floor tu/walk-floor :self {:pos pos} :states door-states
   :blocks (into {} (remove (comp #{"air"} val)) (merge hut extra))})

(defn block-at [p [x y z]] (.-name (.blockAt p (tu/pos x y z))))

(defn door-open? [p] (:open (js->clj (.-properties (.blockAt p (tu/pos 5 64 2))) :keywordize-keys true)))

(defn room-blocks
  "The blocks now in the room's open cells at feet and head height (y 64, 65), other than the furniture."
  [p]
  (into {} (for [x (range 4 7) y [64 65] z (range -1 2)
                 :let [cell [x y z] name (block-at p cell)]
                 :when (and (not= "air" name) (not (contains? furniture (str x "," y "," z))))]
             [cell name])))

;; ------------------------------------------------------------------ unstick: out through the door

(def outside {:x 5 :y 64 :z 8})

(deftest unstick-in-a-hut-leaves-through-the-door-without-pillaring-or-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (ut/setup (merge (hut-world {:x 5 :y 64 :z 1} {})
                                               {:inventory [{:name "cobblestone" :count 32}
                                                            {:name "stone_pickaxe" :count 1}]}))]
          (ut/block-moveTo! p) ; the job's own moveTo cannot route through a door
          (ut/seed-moved! eng (repeat 4 {:from {:x 5 :y 64 :z 1} :to {:x 5 :y 64 :z 1} :status "blocked" :target outside}))
          (core/submit! eng '(jobs.maintenance.unstick) {})
          (await (st/tick-n eng 30))
          (is (= [] (ut/calls p "jumpPlace")) "no pillar in the room")
          (is (= [] (ut/calls p "dig")) "no roof or wall dug")
          (is (= [] (ut/calls p "place")))
          (is (= {} (room-blocks p)) "nothing left at feet or head height in the room")
          (is (= "cobblestone" (block-at p [5 67 1])) "the roof is whole")
          (is (<= 3 (.-z (.-pos (.self p)))) "the body is outside, past the door")
          (is (false? (door-open? p)) "the door is shut behind it")
          (is (= [] (:list (core/state eng))) "the spell is over"))))))

(deftest unstick-in-a-closed-pit-still-pillars-when-no-walk-gets-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [here {:x 5 :y 61 :z 0}
              {:keys [eng p]} (ut/setup {:self {:pos here} :blocks ut/pit :inventory [{:name "dirt" :count 4}]})]
          (await (ut/run-unstick! eng here ut/goal))
          (is (= 3 (count (ut/calls p "jumpPlace"))) "a real pit: the walk finds no way, so go-to pillars")
          (is (= [] (:list (core/state eng)))))))))

;; ------------------------------------------------------------------ the night trigger in the hut

(deftest night-in-the-hut-holds-only-when-the-roof-above-is-gone
  (are [pos extra held] (= held (st/fires? (merge (hut-world pos extra) {:time st/night}) {}))
    {:x 5 :y 64 :z 0} {} false
    {:x 5 :y 65 :z 0} {"5,64,0" "cobblestone"} false
    {:x 5 :y 64 :z 1} {} false
    {:x 5 :y 64 :z 0} {"5,67,0" "air"} true
    {:x 5 :y 65 :z 0} {"5,64,0" "cobblestone" "5,67,0" "air"} true))

;; ------------------------------------------------------------------ dig-in in a room with a hole in the roof

(defn ^:async dig-in! [world]
  (let [{:keys [eng p seen] :as s} (st/setup (merge world {:time st/night :inventory st/dirt-stack}))]
    (core/submit! eng '(jobs.survival.dig-in) {})
    (await (st/run-until-empty eng 8))
    (assoc s :eng eng :p p :seen seen)))

(deftest dig-in-in-a-closed-room-mends-the-roof-hole-instead-of-walling-the-body-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[pos extra before] [[{:x 5 :y 64 :z 0} {"5,67,0" "air"} {}]
                                    [{:x 5 :y 65 :z 0} {"5,64,0" "cobblestone" "5,67,0" "air"} {[5 64 0] "cobblestone"}]]]
          (let [{:keys [eng p]} (await (dig-in! (hut-world pos extra)))]
            (is (= [{:x 5 :y 67 :z 0}] (mapv st/arg-pos (st/calls p "place"))) (str "one block, in the hole " pos))
            (is (= "dirt" (block-at p [5 67 0])))
            (is (= [] (st/calls p "dig")))
            (is (= before (room-blocks p)) "no block placed at feet or head height in the room")
            (is (= [{:pos pos :roof {:x 5 :y 67 :z 0} :state :built :room true}]
                   (st/entries eng :shelter)))))))))

(deftest dig-in-in-a-room-that-is-open-or-has-no-door-walls-the-body-in-as-before
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[why extra states] [["a doorway with no door" {"5,64,2" "air" "5,65,2" "air"} door-states]
                                    ["an open door" {} (update-vals door-states #(assoc % :open true))]
                                    ["no door at all: a pit, left by a stair" {"5,64,2" "cobblestone" "5,65,2" "cobblestone"} {}]]]
          (let [{:keys [p]} (await (dig-in! (assoc (hut-world {:x 5 :y 64 :z 0} (assoc extra "5,67,0" "air")) :states states)))]
            (is (< 1 (count (st/calls p "place"))) why)))))))

(deftest shelter-in-a-mended-room-holds-the-night-and-leaves-by-day-without-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (hut-world {:x 5 :y 64 :z 0} {"5,67,0" "air"})
                                               {:time st/night :inventory st/dirt-stack}))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (await (st/tick-n eng 6))
          (is (= [{:x 5 :y 67 :z 0}] (mapv st/arg-pos (st/calls p "place"))))
          (is (some? (:pending-reflex (core/state eng))) "the shelter holds the body at night")
          (.setTime (.-world p) st/noon)
          (await (st/tick-n eng 6))
          (is (nil? (:pending-reflex (core/state eng))) "the shelter ended at day")
          (is (= [] (st/calls p "dig")) "nothing dug to leave: the room's door is the way out")
          (is (= {} (room-blocks p))))))))

(deftest a-room-entry-is-not-shut-in
  (let [p (tu/fake (hut-world {:x 5 :y 64 :z 0} {}))]
    (is (true? (dig-in/shut-in? p {:pos {:x 5 :y 64 :z 0} :roof {:x 5 :y 67 :z 0}})) "a plain roofed entry")
    (is (false? (dig-in/shut-in? p {:pos {:x 5 :y 64 :z 0} :roof {:x 5 :y 67 :z 0} :room true})))))

;; ------------------------------------------------------------------ a roofed body sleeps in the bed beside it

(def bed-block {"6,64,0" "red_bed"})

(defn bed-trigger-holds?
  "The night condition for the hut with the body at {:x 5 :y 64 :z 0} and a known bed at 6,64,0, after seed!."
  [world seed!]
  (let [{:keys [eng]} (st/setup {})]
    (st/know-bed! eng {:x 6 :y 64 :z 0})
    (seed! eng)
    (boolean ((:when (get triggers/all :night)) (tu/fake world) (mem/view (:store eng)) {}))))

(deftest night-holds-for-a-roofed-body-with-a-bed-it-has-not-slept-in-tonight
  (let [night-world (merge (hut-world {:x 5 :y 64 :z 0} bed-block) {:time st/night})
        none (fn [_])
        slept (fn [eng] (mem/write! (:store eng) :slept {:pos {:x 6 :y 64 :z 0}} {:cap 10 :ttl (* 7 st/day-ms)}))
        unreachable (fn [eng] (mem/write! (:store eng) :bed-unreachable {:pos {:x 6 :y 64 :z 0}} {:cap 5 :ttl 600000}))]
    (is (true? (bed-trigger-holds? night-world none)) "roofed, night, bed known, not slept")
    (is (false? (bed-trigger-holds? night-world slept)) "slept already tonight")
    (is (false? (bed-trigger-holds? night-world unreachable)) "the bed was given up on")
    (is (false? (bed-trigger-holds? (assoc night-world :time st/noon) none)) "by day")
    (is (false? (bed-trigger-holds? (assoc night-world :self {:pos {:x 5 :y 64 :z 0} :isSleeping true}) none)) "asleep")))

(deftest shelter-in-a-roofed-hut-sleeps-in-the-bed-and-holds-till-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (hut-world {:x 5 :y 64 :z 0} bed-block) {:time st/night}))]
          (st/know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 6))
          (is (= 1 (count (st/calls p "sleep"))) "slept in the bed")
          (is (= [] (st/calls p "dig")))
          (is (= [] (st/calls p "place")) "no dig-in")
          (is (= 1 (count (st/entries eng :slept)))))))))

(deftest a-sleeping-shelter-holds-the-night-and-ends-at-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (hut-world {:x 5 :y 64 :z 0} bed-block) {:time st/night}))
              id (core/submit! eng '(jobs.survival.night) {})]
          (st/know-bed! eng {:x 6 :y 64 :z 0})
          (set! (.-sleep p) (fn [& _] (swap! (fake/state p) assoc-in [:self :isSleeping] true)
                              (js/Promise.resolve #js {:status "sleeping"})))
          (await (st/tick-n eng 7))
          (is (some #{id} (:list (core/state eng))) "still listed while asleep at night")
          (is (nil? (core/waiting eng id)) "not parked waiting while asleep")
          (swap! (fake/state p) #(-> % (assoc :time st/noon) (assoc-in [:self :isSleeping] false)))
          (await (st/tick-n eng 4))
          (is (not-any? #{id} (:list (core/state eng))) "ended once it is day"))))))

(deftest night-holds-for-a-roofed-body-carrying-a-bed-and-knowing-none
  (let [world (merge (hut-world {:x 5 :y 64 :z 0} {}) {:time st/night :inventory [{:name "red_bed" :count 1}]})
        holds (fn [world seed!]
                (let [{:keys [eng]} (st/setup {})]
                  (seed! eng)
                  (boolean ((:when (get triggers/all :night)) (tu/fake world) (mem/view (:store eng)) {}))))]
    (is (true? (holds world (fn [_]))) "a carried bed, none known")
    (is (false? (holds (dissoc world :inventory) (fn [_]))) "no bed carried")
    (is (false? (holds world (fn [eng] (mem/write! (:store eng) :bed-place-failed {} {:cap 1 :ttl 600000})))) "set-up failed lately")
    (is (false? (holds world (fn [eng] (st/know-bed! eng {:x 6 :y 64 :z 0})
                                (mem/write! (:store eng) :slept {:pos {:x 6 :y 64 :z 0}} {:cap 10 :ttl (* 7 st/day-ms)}))))
        "a known bed already slept in")))

(deftest shelter-in-a-hut-puts-a-carried-bed-down-records-it-and-sleeps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (hut-world {:x 5 :y 64 :z 0} {})
                                               {:time st/night :skipNight false :inventory [{:name "red_bed" :count 1}]}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 8))
          
          (is (= 1 (count (st/calls p "place"))) "one bed placed")
          (is (= "red_bed" (.-item (.-args (first (st/calls p "place"))))))
          (is (= [] (st/calls p "dig")))
          (is (= 1 (count (st/calls p "sleep"))) "slept in it")
          (is (= 1 (count (st/entries eng :bed))) "recorded as the bed")
          (is (= (st/arg-pos (first (st/calls p "place"))) (:pos (first (st/entries eng :bed)))))
          (is (= 1 (count (st/entries eng :slept)))))))))

(deftest shelter-in-a-closet-with-no-room-for-the-bed-does-nothing-and-is-not-refired
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [boxed (into {} (for [x [4 5 6] z [-1 0 1] :when (not= [x z] [5 0])] [(str x ",64," z) "cobblestone"]))
              world (merge (hut-world {:x 5 :y 64 :z 0} boxed) {:time st/night :inventory [{:name "red_bed" :count 1}]})
              {:keys [eng p]} (st/setup world)]
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 4))
          (is (= [] (st/calls p "place")) "one free cell is no room")
          (is (= 1 (count (st/entries eng :bed-place-failed))))
          (is (false? (boolean ((:when (get triggers/all :night)) p (mem/view (:store eng)) {}))) "not refired"))))))

(deftest shelter-in-a-hut-with-no-room-for-the-bed-gives-up-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [boxed (into {} (for [x [4 5 6] z [-1 0 1] :when (not (#{[5 0] [4 0] [5 -1] [4 -1]} [x z]))] [(str x ",64," z) "cobblestone"]))
              {:keys [eng p]} (st/setup (merge (hut-world {:x 5 :y 64 :z 0} boxed)
                                               {:time st/night :inventory [{:name "red_bed" :count 1}]}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 4))
          (is (= [] (st/calls p "place")))
          (is (= [] (st/calls p "sleep")))
          (is (= 1 (count (st/entries eng :bed-place-failed)))))))))

;; ------------------------------------------------------------------ bed review findings (card d9c87329)

(defn holds-with
  "The night condition in world after seed! ran on a fresh engine's store. Options: :zones and :claims (the
  zone list and claims the trigger reads), :later (ms the clock moves on after seed!)."
  [world seed! & {:keys [zones claims later] :or {zones [] claims [] later 0}}]
  (let [{:keys [eng clock]} (st/setup {})
        kn (ew/of-data {} {} zones)
        p (tu/fake world)]
    (swap! (:state kn) assoc :area-claims {:value claims})
    (seed! eng)
    (swap! clock + later)
    (boolean ((:when (get triggers/all :night)) p (mem/view (:store eng)) {} kn))))

(def night-bed-world (merge (hut-world {:x 5 :y 64 :z 0} bed-block) {:time st/night}))

(deftest a-recent-failed-sleep-stops-night-from-holding-the-roofed-body
  (let [carrying (assoc (merge (hut-world {:x 5 :y 64 :z 0} {}) {:time st/night}) :inventory [{:name "red_bed" :count 1}])
        failed (fn [eng] (mem/write! (:store eng) :sleep-failed {:pos {:x 6 :y 64 :z 0}} sh/sleep-failed-policy))
        known (fn [eng] (st/know-bed! eng {:x 6 :y 64 :z 0}))]
    (is (true? (holds-with night-bed-world known)))
    (is (false? (holds-with night-bed-world (fn [eng] (known eng) (failed eng)))) "known bed")
    (is (true? (holds-with night-bed-world (fn [_]))) "a bed seen in the room, none remembered")
    (is (false? (holds-with night-bed-world failed)) "a bed seen in the room")
    (is (true? (holds-with night-bed-world failed :later (inc (:ttl sh/sleep-failed-policy)))) "seen bed, entry expired")
    (is (true? (holds-with night-bed-world (fn [eng] (known eng) (failed eng)) :later (inc (:ttl sh/sleep-failed-policy))))
        "known bed, entry expired")
    (is (true? (holds-with carrying (fn [_]))))
    (is (false? (holds-with carrying failed)) "a carried bed")
    (is (true? (holds-with carrying failed :later (inc (:ttl sh/sleep-failed-policy)))) "a carried bed, entry expired")))

;; ------------------------------------------------------------------ beds of another owner (zones and claims)

(def bed-cell {:x 6 :y 64 :z 0})

(def self-name (.-username (.self (tu/fake {}))))

(defn bed-zone [owner & [allow]]
  (cond-> {:name "house" :min [3 64 -2] :max [7 66 2] :owner owner} allow (assoc :allow allow)))

(defn bed-claim [owner]
  {:id "c1" :owner owner :status :active :until 99999999999 :min [3 64 -2] :max [7 66 2]})

(def occupied-world
  "night-bed-world with the bed's occupied state set, as a player sees a bed another sleeps in."
  (assoc night-bed-world :states {"6,64,0" {:occupied true}}))

(def lying-world
  "night-bed-world with another player lying in the bed (no state set)."
  (assoc night-bed-world :entities [{:id 9 :name "Miles" :kind "player" :pos [6 64 0] :sleeping true}]))

(deftest night-uses-a-bed-in-any-zone-or-claim-but-not-an-occupied-one
  (let [known (fn [eng] (st/know-bed! eng bed-cell))]
    (doseq [[label seed!] [["seen" (fn [_])] ["remembered" known]]]
      (is (true? (holds-with night-bed-world seed! :zones [])) (str label ": unclaimed"))
      (is (true? (holds-with night-bed-world seed! :zones [(bed-zone self-name)])) (str label ": own zone"))
      (is (true? (holds-with night-bed-world seed! :zones nil)) (str label ": no zone list read"))
      (is (true? (holds-with night-bed-world seed! :zones [(bed-zone "Miles")])) (str label ": another owner's zone"))
      (is (true? (holds-with night-bed-world seed! :claims [(bed-claim "Miles")])) (str label ": another owner's claim"))
      (is (false? (holds-with occupied-world seed! :zones [])) (str label ": occupied"))
      (is (false? (holds-with occupied-world seed! :zones [(bed-zone "Miles")])) (str label ": occupied, another's zone"))
      (is (true? (holds-with occupied-world seed! :zones [(bed-zone self-name)])) (str label ": occupied, own zone"))
      (is (true? (holds-with lying-world seed! :zones [])) (str label ": a player asleep in it in sight: log out for them")))))

(defn zoned-setup
  "st/setup with a world holding zones and claims."
  [world zones claims]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} world))
        w (ew/of-data {} {} zones)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock) :world w
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (swap! (:state w) assoc :area-claims {:value claims})
    {:eng eng :p p :seen seen :clock clock}))

(deftest shelter-sleeps-in-any-free-bed-and-records-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zones claims] [[[] []] [[(bed-zone self-name)] []] [[] [(bed-claim self-name)]]
                                [[(bed-zone "Miles")] []] [[] [(bed-claim "Miles")]]]]
          (let [{:keys [eng p]} (zoned-setup night-bed-world zones claims)]
            (core/submit! eng '(jobs.survival.night) {})
            (await (st/tick-n eng 6))
            (is (= 1 (count (st/calls p "sleep"))) (pr-str [zones claims]))
            (is (= [{:pos bed-cell}] (st/entries eng :bed)) "recorded as :bed")))))))

(deftest shelter-leaves-an-occupied-bed-alone-unless-it-is-in-its-own-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [w [occupied-world lying-world]
                known? [false true]
                zones [[] [(bed-zone "Miles")]]]
          (let [{:keys [eng p]} (zoned-setup w zones [])]
            (when known? (st/know-bed! eng bed-cell))
            (core/submit! eng '(jobs.survival.night) {})
            (await (st/tick-n eng 6))
            (is (= [] (st/calls p "sleep")) (pr-str [known? zones]))
            (is (= (if known? [{:pos bed-cell}] []) (st/entries eng :bed)) "nothing new recorded as :bed")))))))

(deftest shelter-tries-an-occupied-bed-in-its-own-zone-and-a-refusal-is-a-failed-sleep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [w [occupied-world lying-world]]
          (let [{:keys [eng p]} (zoned-setup w [(bed-zone self-name)] [])]
            (set! (.-sleep p) (fn [& _] (js/Promise.resolve #js {:status "occupied"})))
            (core/submit! eng '(jobs.survival.night) {})
            (await (st/tick-n eng 12))
            (is (= [{:pos bed-cell}] (st/entries eng :sleep-failed)) "the normal failed-sleep memory")
            (is (= [] (st/entries eng :bed-unreachable)) "not a permanent skip")))))))

(deftest a-sleep-that-ends-without-sleeping-writes-a-short-sleep-failed-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (hut-world {:x 5 :y 64 :z 0} bed-block) {:time st/night}))]
          (st/know-bed! eng {:x 6 :y 64 :z 0})
          (set! (.-sleep p) (fn [& _] (js/Promise.resolve #js {:status "occupied"})))
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (= [{:pos {:x 6 :y 64 :z 0}}] (st/entries eng :sleep-failed)))
          (is (false? (boolean ((:when (get triggers/all :night)) p (mem/view (:store eng)) {})))
              "night no longer holds on the taken bed"))))))

;; a one-wide tunnel (x 0..20, z 0, y 64..65) through stone: roofed, but no room

(def tunnel
  (into {} (remove (fn [[k _]] (let [[x y z] (map js/Number (.split k ","))] (and (<= 0 x 20) (<= 64 y 65) (= z 0)))))
        (box 0 64 -1 20 66 1 "stone")))

(deftest a-miner-in-a-tunnel-walks-to-a-remembered-bed-or-puts-a-carried-one-down
  (let [world (fn [extra] (merge {:floor tu/walk-floor :self {:pos {:x 5 :y 64 :z 0}} :time st/night
                                  :blocks (merge tunnel {"10,64,0" "red_bed"})} extra))
        know (fn [eng] (st/know-bed! eng {:x 10 :y 64 :z 0}))]
    (is (true? (holds-with (world {}) know)) "a bed 5 blocks along the tunnel")
    (is (true? (holds-with (world {:inventory [{:name "red_bed" :count 1}]}) (fn [_]))) "a carried bed")))

(deftest a-bed-remembered-away-from-the-hut-is-still-the-bed-to-use
  (let [world (merge (hut-world {:x 5 :y 64 :z 0} {"30,64,0" "red_bed"}) {:time st/night})]
    (is (true? (holds-with world (fn [eng] (st/know-bed! eng {:x 30 :y 64 :z 0})))) "remembered 25 blocks away")))

(deftest a-hut-bed-memory-does-not-know-is-found-and-slept-in-before-any-second-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge (hut-world {:x 5 :y 64 :z 0} bed-block)
                           {:time st/night :inventory [{:name "red_bed" :count 1}]})
              {:keys [eng p]} (st/setup world)]
          (is (true? (boolean ((:when (get triggers/all :night)) p (mem/view (:store eng)) {}))) "holds for the seen bed")
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 8))
          (is (= [] (st/calls p "place")) "no second bed")
          (is (= 1 (count (st/calls p "sleep"))))
          (is (= [{:pos {:x 6 :y 64 :z 0}}] (st/entries eng :bed)) "the seen bed is recorded"))))))

(deftest a-bed-behind-a-wall-is-not-seen
  (let [p (tu/fake (merge (hut-world {:x 5 :y 64 :z 0} {}) {:blocks (assoc (:blocks (hut-world {:x 5 :y 64 :z 0} {})) "9,64,0" "red_bed")}))]
    (is (nil? (sh/seen-bed p)))))

(deftest a-placed-bed-does-not-overwrite-a-known-bed-beyond-the-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far {:x 80 :y 64 :z 0}
              {:keys [eng p seen]} (st/setup (merge (hut-world {:x 5 :y 64 :z 0} {"80,64,0" "red_bed"})
                                                    {:time st/night :inventory [{:name "red_bed" :count 1}]}))]
          (st/know-bed! eng far)
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 8))
          (is (= 1 (count (st/calls p "place"))) "put the carried one down")
          (is (= 1 (count (st/calls p "sleep"))))
          (is (= far (mem/place (mem/view (:store eng)) :bed)) "the known bed is kept")
          (is (seq (st/emitted seen :place.kept)) "and one place.kept event says so"))))))

(deftest a-placed-bed-is-set-with-a-click-facing-along-the-row
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (hut-world {:x 5 :y 64 :z 0} {})
                                               {:time st/night :skipNight false :inventory [{:name "red_bed" :count 1}]}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 8))
          (let [call (first (st/calls p "place"))
                foot (st/arg-pos call)
                click (js->clj (.-click (.-args call)) :keywordize-keys true)
                stand (js->clj (.-pos (.self p)) :keywordize-keys true)
                dx (- (:x foot) (:x stand)) dz (- (:z foot) (:z stand))]
            (is (= (update foot :y dec) (:against click)) "against the floor under the foot cell")
            (is (< (js/Math.abs (- (:yaw click) (js/Math.atan2 (- dx) (- dz)))) 1e-9) "yaw along the row")
            (is (= "red_bed" (block-at p [(+ (:x foot) dx) (:y foot) (+ (:z foot) dz)])) "the head cell is the one after the foot")))))))

(deftest a-gap-in-a-wall-is-a-doorway-and-a-cell-in-the-room-is-not
  (let [p (tu/fake (hut-world {:x 5 :y 64 :z 0} {"5,64,2" "air" "5,65,2" "air"}))]
    (is (true? (sh/doorway? p {:x 5 :y 64 :z 2})))
    (is (false? (sh/doorway? p {:x 5 :y 64 :z 1})))
    (is (false? (sh/doorway? p {:x 4 :y 64 :z 0})))))
