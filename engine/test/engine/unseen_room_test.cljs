(ns engine.unseen-room-test
  "A cell the body has not seen reads as the worse case for the question: a wall for 'would this fill shut me in?', open
  ground for 'am I sheltered?'. Each caller moved off the true world answers differently when its walls are hidden."
  (:require [cljs.test :refer [deftest is]]
            [engine.test-util :as tu]
            [jobs.build.from-plan.cells :as fp-cells]
            [jobs.farm.harvest :as harvest]
            [jobs.gather.mine.targets :as targets]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.shelter.inside :as inside]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.movement.go-to.restore :as restore]
            [jobs.survival.restore-broken :as restore-broken]))

(def body {:x 0.5 :y 64 :z 0.5})

(def sealed
  "Stone on the eight cells around (0 64 0), two high, and a roof."
  (into {"0,66,0" "stone"} (for [x [-1 0 1] z [-1 0 1] :when (not= 0 x z) y [64 65]] [(str x "," y "," z) "stone"])))

(def two-doorways (apply dissoc sealed ["1,64,0" "1,65,0" "-1,64,0" "-1,65,0"]))

(defn hiding
  "p whose body has not seen cells (a set of [x y z])."
  [p cells]
  (let [sensed (.-sensedAt p)]
    (tu/set-sensed! p (fn [q] (if (contains? cells [(.-x q) (.-y q) (.-z q)]) #js {:unknown true :pos q} (sensed q))))
    p))

(def west-outside (set (for [x (range -8 -1) z (range -3 4) y [64 65]] [x y z])))

(defn doorways-p []
  (hiding (tu/fake {:self {:pos body} :floor [-30 -30 30 30] :blocks two-doorways}) west-outside))

(defn walled-p
  "A body in a closed cell whose walls and roof it has not looked at."
  []
  (hiding (tu/fake {:self {:pos body} :floor [-30 -30 30 30] :blocks sealed}) (set (map #(mapv js/parseInt (.split % ",")) (keys sealed)))))

(deftest a-fill-of-one-doorway-shuts-in-a-body-whose-other-way-out-is-unseen
  (let [c {:primitives (doorways-p)}]
    (is (true? (fp-cells/seals? c [1 64 0])))
    (is (= :needed (restore/skip-why (assoc c :view (fn [] {}) :root "r" :slots []) {:cell [1 64 0]})))))

(deftest a-restore-that-fills-the-only-seen-way-out-skips-while-the-other-is-unseen
  (let [c {:primitives (doorways-p)}]
    (is (true? (restore-broken/seals-body? c {:cell [1 64 0] :action :dig} {:enclosed (delay false)})))))

(deftest a-roof-and-walls-the-body-has-not-looked-at-give-no-shelter
  (let [p (walled-p)]
    (is (false? (inside/shut-in? p {:start {:y 70}})))
    (is (true? (harvest/shut-in-start? {:primitives (tu/fake {:self {:pos body} :floor [-30 -30 30 30] :blocks sealed})} {})) "seen walls")
    (is (false? (harvest/shut-in-start? {:primitives p} {})))))

(deftest a-dry-pocket-walled-by-stone-the-body-has-not-seen-is-not-a-pocket
  (let [water (assoc sealed "0,64,1" "water" "0,65,1" "water")
        seen (tu/fake {:self {:pos body} :floor [-30 -30 30 30] :blocks water})
        hidden (hiding (tu/fake {:self {:pos body} :floor [-30 -30 30 30] :blocks water})
                       (set (for [x [-1 0 1] z [-1 0 1] y [64 65 66] :when (not= 0 x z)] [x y z])))]
    (is (true? (choose/air-pocket? {:primitives seen} {:reason :air})))
    (is (false? (choose/air-pocket? {:primitives hidden} {:reason :air})))))

(def one-doorway
  "The sealed cell with a doorway east, its outside and the cell above the doorway not looked at."
  (apply dissoc sealed ["1,64,0" "1,65,0"]))

(defn doorway-p []
  (hiding (tu/fake {:self {:pos body} :floor [-30 -30 30 30] :blocks one-doorway :inventory [{:name "stone" :count 4}]})
          (set (for [x (range 2 8) z (range -3 4) y [64 65]] [x y z]))))

(deftest an-escalation-hole-is-kept-while-the-only-way-out-is-unseen
  (let [hole {:cell [1 66 0] :action :dig :was "stone" :now "air" :escalation true :any-of ["stone"]}]
    (is (= :shut-in (tidy/why-not (doorway-p) hole)))))

(deftest a-restore-pass-reads-hidden-walls-as-open-ground-for-the-enclosure
  (is (false? @(:enclosed (restore-broken/probe {:primitives (walled-p)})))))

(deftest digging-the-floor-of-a-way-whose-headroom-is-unseen-is-not-the-last-exit
  (let [p (hiding (tu/fake {:self {:pos body} :floor [-30 -30 30 30] :blocks one-doorway}) #{[1 65 0]})]
    (is (false? (targets/last-exit? {:primitives p} [1 63 0])))))
