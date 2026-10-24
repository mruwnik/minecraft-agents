(ns engine.jobs-danger-test
  "jobs.lib.cost/route-danger: the danger of walking a route past the mobs the body senses, over their reach (jobs.lib.threat-field), after its armour."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.cost :as d]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.reach :as reach]
            [jobs.lib.threat-field :as field]
            [engine.game :as game]
            [engine.test-util :as tu]))

(def body {:x 0.5 :y 64 :z 0.5})

(def route
  "A straight walk east along z 0 from x 0 to x 30 on the floor."
  (mapv (fn [x] {:x x :y 64 :z 0}) (range 0 31)))

(defn mob [id name x z] {:id id :name name :kind "hostile" :pos {:x (+ x 0.5) :y 64 :z (+ z 0.5)}})

(def skeleton-wall
  "A closed stone box around the cell (15 64 8): a skeleton inside has no line of fire and no way out."
  (into {} (for [x (range 13 18) z (range 6 11) y [64 65 66 67]
                 :when (or (#{13 17} x) (#{6 10} z) (= y 67))]
             [(str x "," y "," z) "stone"])))

(defn world
  "A fake body on the floor, selected so costs read the rules it announces (easy), as the engine does at start."
  [& {:keys [blocks]}]
  (doto (tu/fake {:self {:pos body} :floor [-10 -20 40 20] :blocks (or blocks {})}) game/select!))

(def naked {})
(def iron-armour {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"}
                  :legs {:name "iron_leggings"} :feet {:name "iron_boots"}})

(defn reach-of [p] (fn [m] (field/reach-fn (field/field p {:mob (:name m) :pos (:pos m)}))))

(defn death-s "Seconds a death on the route costs a body whose bed is where the route starts." [] (d/death-cost {:respawn body} (nth route 15)))

(defn route-danger [p route mobs equipment & opts] (apply d/route-danger game/default-version (reach-of p) route mobs equipment :death-s (death-s) opts))

(defn danger [p mobs & opts] (:danger (apply route-danger p route mobs naked opts)))

(deftest no-mobs-no-danger
  (is (= 0 (danger (world) []))))

(deftest a-zombie-beside-the-route-is-a-danger
  (is (pos? (danger (world) [(mob 1 "zombie" 15 2)]))))

(deftest a-creeper-is-worse-than-a-zombie
  (is (> (danger (world) [(mob 1 "creeper" 15 2)]) (danger (world) [(mob 1 "zombie" 15 2)]))))

(deftest closer-along-its-way-is-worse-and-a-way-past-its-follow-range-is-nothing
  (let [p (world)]
    (is (> (danger p [(mob 1 "zombie" 15 1)]) (danger p [(mob 1 "zombie" 15 8)]) (danger p [(mob 1 "zombie" 15 19)]) 0)
        "19 blocks off is 19 steps of its way: still within its follow range (35)")
    (is (= 0 (danger p [(mob 1 "vindicator" 15 19)])) "a vindicator follows 12")))

(deftest a-drowned-holding-a-trident-is-worse-than-one-with-bare-hands
  (let [p (world :blocks (into {} (for [x (range 13 18) z (range -2 4) y [63 64]] [(str x "," y "," z) "water"])))
        drowned (mob 1 "drowned" 15 1)]
    (is (> (danger p [(assoc drowned :held "trident")]) (danger p [drowned]) 0))))

(deftest a-walled-off-skeleton-is-no-danger-and-an-open-one-is
  (is (= 0 (danger (world :blocks skeleton-wall) [(mob 1 "skeleton" 15 8)])))
  (is (pos? (danger (world) [(mob 1 "skeleton" 15 8)]))))

(deftest a-walled-off-zombie-is-no-danger
  (is (= 0 (danger (world :blocks skeleton-wall) [(mob 1 "zombie" 15 8)]))))

(deftest passive-mobs-are-no-danger
  (is (= 0 (danger (world) [{:id 1 :name "cow" :kind "passive" :pos {:x 15.5 :y 64 :z 1.5}}]))))

(deftest dangers-add-up-and-the-main-contributor-is-first
  (let [r (route-danger (world) route [(mob 1 "zombie" 15 2) (mob 2 "creeper" 20 2)] naked)]
    (is (= "creeper" (:name (first (:mobs r)))))
    (is (= (:danger r) (reduce + (map :danger (:mobs r)))))))

(deftest a-route-past-zombies-is-priced-at-a-hit-a-second-not-the-lone-fight-rate
  (let [r (route-danger (world) route [(mob 1 "zombie" 15 2) (mob 2 "zombie" 16 2)] naked)]
    (is (= [7.5 7.5] (mapv :threat (:mobs r))) "3 s of exposure, one hit of 2.5 (easy) a second each")))

(defn threat-of [p name & opts] (:threat (first (:mobs (apply route-danger p route [(mob 1 name 15 2)] naked opts)))))

(deftest overrides-set-a-mob-s-threat
  (let [p (world)]
    (is (= 0 (danger p [(mob 1 "zombie" 15 2)] :overrides {"zombie" 0})))
    (is (> (danger p [(mob 1 "zombie" 15 2)] :overrides {"zombie" 100}) (danger p [(mob 1 "zombie" 15 2)])))
    (is (= (* 2 (threat-of p "zombie")) (threat-of p "zombie" :overrides {"zombie" {:times 2}})))))

(deftest armour-lowers-the-danger
  (let [p (world)]
    (are [m] (< (:danger (route-danger p route [m] iron-armour))
                (:danger (route-danger p route [m] naked)))
      (mob 1 "zombie" 15 2)
      (mob 1 "creeper" 15 2)
      (mob 1 "skeleton" 15 8))))

(defn with-enchant [ench]
  (update iron-armour :torso assoc :enchants [{:name ench :lvl 4}]))

(deftest blast-protection-lowers-the-creeper-and-projectile-protection-the-skeleton
  (let [p (world)
        dz (fn [m eq] (:danger (route-danger p route [m] eq)))
        creeper (mob 1 "creeper" 15 2)
        skeleton (mob 2 "skeleton" 15 8)]
    (is (< (dz creeper (with-enchant "blast_protection")) (dz creeper iron-armour)))
    (is (= (dz skeleton (with-enchant "blast_protection")) (dz skeleton iron-armour)) "blast does nothing for arrows")
    (is (< (dz skeleton (with-enchant "projectile_protection")) (dz skeleton iron-armour)))
    (is (= (dz creeper (with-enchant "projectile_protection")) (dz creeper iron-armour)))
    (is (< (dz skeleton (with-enchant "protection")) (dz skeleton iron-armour)) "protection helps against all")))

(deftest js-entities-are-read-too
  (let [p (world)]
    (is (= (danger p [(mob 1 "zombie" 15 2)])
           (danger p [#js {:id 1 :name "zombie" :kind "hostile" :pos #js {:x 15.5 :y 64 :z 2.5}}])))))

(deftest an-empty-route-is-no-danger
  (is (= 0 (:danger (route-danger (world) [] [(mob 1 "zombie" 1 1)] naked)))))

;; ---------------------------------------------------------------- a body with no answer

(defn armed-danger
  ([m weapon health] (armed-danger m weapon health naked))
  ([m weapon health equipment] (:danger (route-danger (world) route [m] equipment :weapon weapon :health health))))

(deftest an-unarmed-body-facing-a-skeleton-is-priced-far-above-an-armed-one
  (let [skeleton (mob 2 "skeleton" 15 0)
        armed (armed-danger skeleton "iron_sword" 20 iron-armour)
        bare (armed-danger skeleton nil 20)]
    (is (pos? armed))
    (is (> bare (* 2 armed)))))

(deftest a-danger-is-at-most-the-hurt-and-one-death
  (let [spider (mob 1 "spider" 15 0)
        hp-death (/ (death-s) d/per-danger)]
    (is (<= (armed-danger spider nil 20) (+ 20 hp-death)) "a fist fight with a spider costs the health and a death, not a multiple of it")
    (is (<= (armed-danger spider nil 6) (+ 6 hp-death)))))

(deftest an-unarmed-body-beside-its-bed-prices-a-spider-below-the-pile-it-guards
  (let [pile {:x 15 :y 64 :z 0}
        worth (:value (d/item-value [{:name "oak_log" :count 15} {:name "feather" :count 1} {:name "oak_planks" :count 2}]))
        r (route-danger (world) route [(mob 1 "spider" 15 0)] naked :death-s (d/death-cost {:respawn {:x 15 :y 64 :z 3}} pile))]
    (is (< 0 (* d/per-danger (:danger r)) worth) "the trial's pile, its bed 3 blocks off: the spider was priced 5820 s")))

(deftest a-body-that-would-win-the-fight-is-priced-alike-whatever-it-wins-with
  (let [zombie (mob 1 "zombie" 15 2)
        armed (armed-danger zombie "iron_sword" 20 iron-armour)]
    (is (< (js/Math.abs (- armed (armed-danger zombie "wooden_sword" 20 iron-armour))) 1e-6))
    (is (> (armed-danger zombie nil 20 iron-armour) armed)
        "bare hands lose to a zombie even in iron armour")))

(deftest the-weaker-the-body-the-dearer-the-danger
  (let [zombie (mob 1 "zombie" 15 2)]
    (is (> (armed-danger zombie "iron_sword" 6 iron-armour) (armed-danger zombie "iron_sword" 20 iron-armour))
        "a fist fight with a zombie kills at 6 and at 20 alike, so the health only caps the hurt; a sword fight is lost only by the weaker")))

(deftest an-unreachable-or-passive-mob-stays-free-however-weak-the-body
  (is (= 0 (:danger (route-danger (world :blocks skeleton-wall) route [(mob 1 "skeleton" 15 8)] naked :weapon nil :health 4))))
  (is (= 0 (:danger (route-danger (world) route [{:id 1 :name "cow" :kind "passive" :pos {:x 15.5 :y 64 :z 1.5}}] naked :weapon nil :health 4)))))

(deftest an-enderman-is-not-priced-as-a-fight-the-body-cannot-win
  (let [p (world)
        ender (mob 1 "enderman" 15 2)]
    (is (< 0 (danger p [ender] :health 20) 20) "provoked-only share, no death")
    (is (= 0 (danger p [(assoc ender :angry false)])) "a calm enderman adds nothing")))
