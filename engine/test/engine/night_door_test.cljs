(ns engine.night-door-test
  "The night shuts an open door of the closed room the body stands in instead of digging in behind it."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.hut-shelter-test :refer [hut-world door-states door-open? roof door]]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [jobs.lib.world-files :as ew]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.beds :as sh-beds]
            [engine.shelter-support :as st]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]))

(def open-door-states (update-vals door-states #(assoc % :open true)))

(defn open-door-hut [pos extra] (assoc (hut-world pos extra) :states open-door-states :time st/night))

(defn fires-seen? [world]
  (let [eng (:eng (st/setup {}))
        p (tu/fake world)]
    (st/looked-round! eng p)
    (boolean ((:when (get triggers/all :night)) p (mem/view (:store eng)) {}))))

(deftest night-holds-for-a-closed-room-with-an-open-door-until-the-door-is-shut
  (are [world held] (= held (fires-seen? world))
    (open-door-hut {:x 5 :y 64 :z 0} {}) true
    (assoc (open-door-hut {:x 5 :y 64 :z 0} {}) :states door-states) false
    (open-door-hut {:x 5 :y 64 :z 0} {"5,67,0" "air"}) true))

(deftest night-in-a-room-with-an-open-door-shuts-the-door-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (open-door-hut {:x 5 :y 64 :z 0} {}) {:inventory st/dirt-stack}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 8))
          (is (false? (door-open? p)) "the door is shut")
          (is (= [] (st/calls p "place")) "nothing placed")
          (is (= [] (st/calls p "dig")) "nothing dug")
          (is (= [] (st/entries eng :shelter)) "no dig-in shelter"))))))

(defn seeing-setup
  "st/setup over a body that has seen only what its sight passes showed (perception over the fake), as live."
  [world]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} world))
        _ (st/dawn-after! p st/default-dawn)
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})
        eng (core/create {:primitives (perception/wrap p per) :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p}))

(deftest night-looks-up-at-the-roof-before-judging-the-room-open-and-then-shuts-its-open-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (seeing-setup (merge (open-door-hut {:x 5 :y 64 :z 0} {}) {:inventory st/dirt-stack}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (false? (door-open? p)) "the door is shut")
          (is (= [] (st/calls p "place")) "nothing placed"))))))

(deftest night-shuts-an-open-door-of-a-room-with-a-hole-in-its-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (open-door-hut {:x 5 :y 64 :z 0} {"3,64,0" "air" "3,65,0" "air"}) {:inventory st/dirt-stack}))]
          (mem/write! (:store eng) :bed-open {:pos {:x 4 :y 64 :z 0}} sh-beds/bed-open-policy)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 8))
          (is (false? (door-open? p)) "the door is shut though the room is open"))))))

(deftest night-in-a-room-with-an-open-door-and-a-hole-in-the-roof-digs-in-as-before
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (open-door-hut {:x 5 :y 64 :z 0} {"5,67,0" "air"}) {:inventory st/dirt-stack}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 8))
          (is (seq (st/calls p "place")) "dug in: the room cannot be closed"))))))

(def long-hut-extra
  "The hut's room stretched north to z -7: the door at z 2 is out of a hand's reach from its far end."
  (merge (box 3 64 -8 7 66 -2 "cobblestone") (box 3 67 -8 7 67 -3 "cobblestone")
         (into {} (for [x [3 7] y [64 65 66] z (range -7 2)] [(str x "," y "," z) "cobblestone"]))
         (into {} (for [x [4 5 6] y [64 65 66] z (range -7 -1)] [(str x "," y "," z) "air"]))
         (into {} (for [x [4 5 6] z (range -7 2)] [(str x ",67," z) "cobblestone"]))
         (into {} (for [x [4 5 6] y [64 65 66]] [(str x "," y ",-8") "cobblestone"]))))

(deftest night-in-a-long-room-walks-to-the-far-door-and-shuts-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (open-door-hut {:x 5 :y 64 :z -7} long-hut-extra) {:inventory st/dirt-stack}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (false? (door-open? p)) "the door is shut")
          (is (= [] (st/calls p "place")) "nothing placed"))))))

(deftest a-door-that-stays-open-is-given-up-once-and-the-night-digs-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (st/setup (merge (open-door-hut {:x 5 :y 64 :z 0} {}) {:inventory st/dirt-stack}))]
          (.override (.-world p) "useOn" (fn [token a impl] (impl token (clj->js {:pos {:x 3 :y 64 :z 0}}))))
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (true? (door-open? p)) "the click did nothing")
          (is (= [{:door [5 64 2]}] (st/entries eng :night-door-failed)))
          (is (= 1 (count (filterv #(= :shelter.door-failed (:kind %)) @seen))))
          (is (seq (st/calls p "place")) "the body shuts itself in on its spot (dig-in) instead of calling the hut roofed")
          (is (not-any? #(= :shelter.exposed (:kind %)) @seen))
          (is (seq (st/entries eng :shelter)) "a shelter is written"))))))

(deftest an-open-iron-door-no-hand-shuts-is-named-and-the-night-digs-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup (merge (open-door-hut {:x 5 :y 64 :z 0} {"5,64,2" "iron_door" "5,65,2" "iron_door"}) {:inventory st/dirt-stack}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (= 1 (count (filterv #(= :shelter.door-failed (:kind %)) @seen))) "the open iron door is named")
          (is (seq (st/calls p "place")) "dug in: the roofed hut is not shut"))))))

(defn zoned-setup
  "st/setup over the same world with a zone list in the engine."
  [world zones]
  (let [clock (atom 1000000)
        p (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} world))
        _ (st/dawn-after! p st/default-dawn)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (ew/of-data {} {} zones)})]
    {:eng eng :p p}))

(deftest a-door-in-anothers-zone-is-shut-all-the-same
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (zoned-setup (merge (open-door-hut {:x 5 :y 64 :z 0} {}) {:inventory st/dirt-stack})
                                           [{:name "yard" :min [5 60 2] :max [5 70 2] :owner "Miles"}])]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (false? (door-open? p)) "shutting a door changes no block: no zone rule applies")
          (is (= [] (st/entries eng :night-door-failed))))))))

(deftest a-failed-door-is-given-up-wherever-the-body-stands-in-the-room
  (let [p (tu/fake (open-door-hut {:x 5 :y 64 :z -1} {}))
        view (fn [door] {:data (mem/add-entry mem/empty-data :night-door-failed {:t 0 :data {:door door}} {:cap 1 :ttl 600000}) :now 0})]
    (is (= [5 64 2] (some-> (sh/open-room-door p {:data mem/empty-data :now 0}) :cell sh/door-key)))
    (is (nil? (sh/open-room-door p (view [5 64 2]))) "that door is not tried again")
    (is (some? (sh/open-room-door p (view [9 64 9]))) "another door is")))

(deftest night-steps-in-from-the-door-cell-before-shutting-the-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (merge (open-door-hut {:x 5 :y 64 :z 2} {}) {:inventory st/dirt-stack}))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (< (:z (st/pos-of p)) 2) "the body stands inside the room, not in the door cell")
          (is (false? (door-open? p)) "the door is shut"))))))
