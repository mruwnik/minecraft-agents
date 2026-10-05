(ns engine.hut-shelter-test
  "A body inside its own roofed hut with a shut door (game-agent bugs #33/#42): the unstick job leaves through the door
  instead of pillaring in the room and digging the roof, the night-unsafe trigger holds only when the roof is really
  gone, and dig-in mends a hole in the roof of a closed room instead of walling the body in at feet and head height."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.shelter-test :as st]
            [engine.test-util :as tu :refer [box]]
            [engine.unstick-test :as ut]
            [engine.triggers :as triggers]
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
          (await (st/tick-n eng 3))
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
        (let [{:keys [eng p]} (ut/setup {:self {:pos ut/at5} :blocks ut/deep-pit :inventory [{:name "dirt" :count 4}]})]
          (await (ut/run-attempts! eng p 1 :free))
          (is (= [{:item "dirt" :count 3}] (ut/call-args p "jumpPlace")) "a real pit: the walk finds no way, so it pillars"))))))

;; ------------------------------------------------------------------ night-unsafe in the hut

(deftest night-unsafe-in-the-hut-holds-only-when-the-roof-above-is-gone
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
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe}]}"))
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
  "The night-unsafe condition for the hut with the body at {:x 5 :y 64 :z 0} and a known bed at 6,64,0, after seed!."
  [world seed!]
  (let [{:keys [eng]} (st/setup {})]
    (st/know-bed! eng {:x 6 :y 64 :z 0})
    (seed! eng)
    (boolean ((:when (get triggers/all :night-unsafe)) (tu/fake world) (mem/view (:store eng)) {}))))

(deftest night-unsafe-holds-for-a-roofed-body-with-a-bed-it-has-not-slept-in-tonight
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
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (st/tick-n eng 6))
          (is (= 1 (count (st/calls p "sleep"))) "slept in the bed")
          (is (= [] (st/calls p "dig")))
          (is (= [] (st/calls p "place")) "no dig-in")
          (is (= 1 (count (st/entries eng :slept)))))))))
