(ns engine.planner-damage-test
  "The planner's damage budget (options.damageBudget hp, damageWeight s/hp, fallFactor, landing): certain damage (a fall
  over 3 blocks, a plant's touch) adds up along a path and a move that would take it over the budget is refused; the
  price of an hp is damageWeight. Without the options the planner plans as before."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.path.planner.base :as base]
            [engine.planner-fixture :as pf :refer [cells moves near run MOVE]]))

(defn cost [r k] (get-in r [:path :cost k]))

;; A platform x 0..4, z 0..4 whose top is d blocks over the floor, and a walkway of the same height along z 5..30 to a staircase
;; at x 5..12, z 28..30 down to the floor: the cliff drop is the short way to the goal at (8 64 2), the walkway the long one.

(defn stairs [d]
  (pf/world {:fill (into [[0 64 0 4 (+ 63 d) 30 "stone"]]
                         (for [k (range (dec d))] [(+ 5 k) 64 28 (+ 5 k) (+ 62 d (- k)) 30 "stone"]))}))

(defn from [d] {:x 2 :y (+ 64 d) :z 2})

(def goal (near 8 64 2))

(defn plan-cliff [d options] (run (stairs d) goal (merge {:maxDrop 16} options) (from d)))

(defn drops [r] (count (filter #{(:drop MOVE)} (moves r))))

(deftest no-budget-plans-as-before
  (let [plain (plan-cliff 8 {})
        infinite (plan-cliff 8 {:damageBudget js/Infinity})]
    (is (= (cells plain) (cells infinite)))
    (is (= (cost plain :seconds) (cost infinite :seconds)))
    (is (= (cost plain :risk) (cost infinite :risk)))
    (is (= (:expanded plain) (:expanded infinite)))))

(deftest default-drop-of-3-takes-no-damage
  (let [r (run (pf/world {:fill [[0 64 0 4 66 4 "stone"]]}) goal {:damageBudget 0} (from 3))]
    (is (= "found" (:status r)))
    (is (= 0 (cost r :damage)))))

(deftest a-cliff-within-budget-is-dropped
  (let [r (plan-cliff 8 {:damageBudget 7 :damageWeight 1})]
    (is (= "found" (:status r)))
    (is (= 5 (cost r :damage)))
    (is (= 1 (drops r)))
    (is (re-find #"1 drop of 8 \(5 hp\)" (get-in r [:path :summary])))
    (is (not (:damageRefused r)))))

(deftest a-cliff-over-budget-takes-the-long-way
  (let [r (plan-cliff 8 {:damageBudget 4 :damageWeight 1})]
    (is (= "found" (:status r)))
    (is (= 0 (cost r :damage)))
    (is (<= (cost r :maxDrop) 3) "only free drops")
    (is (> (cost r :seconds) 10))
    (is (not (:damageRefused r)))))

;; two 6-drops in a row (3 hp each), the only way: a one-way trip down a pair of ledges
(def ledges (pf/world {:fill [[0 64 0 4 75 4 "stone"] [5 64 0 8 69 4 "stone"]]}))

(deftest two-drops-share-one-budget
  (let [start {:x 2 :y 76 :z 2}
        goal (near 12 64 2)
        tight (run ledges goal {:maxDrop 16 :damageBudget 5} start)
        enough (run ledges goal {:maxDrop 16 :damageBudget 6} start)]
    (is (not= "found" (:status tight)))
    (is (true? (:damageRefused tight)))
    (is (= "found" (:status enough)))
    (is (= 6 (cost enough :damage)))))

(def hay-id (.stateAt (pf/world {:blocks [[0 70 0 "hay_block"]]}) 0 70 0))
(def slime-id (.stateAt (pf/world {:blocks [[0 70 0 "slime_block"]]}) 0 70 0))

(defn landing [& kv] (js/Map. (clj->js (mapv vec (partition 2 kv)))))

(deftest landing-factors-set-the-fall-damage
  (let [on (fn [block budget]
             (run (pf/world {:fill [[0 64 0 4 74 4 "stone"] [5 64 0 7 64 4 block]]}) (near 6 65 2)
                  {:maxDrop 16 :damageBudget budget :landing (landing hay-id 0.2 slime-id -1)}
                  {:x 4 :y 75 :z 2}))]
    (testing "10 blocks onto hay: 2 hp, where the floor beside it would cost 8"
      (is (= 2 (cost (on "hay_block" 5) :damage))))
    (testing "onto slime: the body bounces and settles, no damage even with no budget"
      (is (= "found" (:status (on "slime_block" 0))))
      (is (= 0 (cost (on "slime_block" 0) :damage))))
    (testing "the bounce takes time: more seconds than a landing that does not bounce (factor 0)"
      (let [still (run (pf/world {:fill [[0 64 0 4 74 4 "stone"] [5 64 0 7 64 4 "slime_block"]]}) (near 6 65 2)
                       {:maxDrop 16 :damageBudget 0 :landing (landing slime-id 0)}
                       {:x 4 :y 75 :z 2})]
        (is (> (cost (on "slime_block" 0) :seconds) (+ 1 (cost still :seconds))))))
    (testing "onto stone: the full fall"
      (is (= 7 (cost (on "stone" 10) :damage))))))

;; the cliff of landing-factors-set-the-fall-damage over a stone floor (x 5..9) with a slime pad on it: a 10-block drop
;; from (4 75 2) lands at (5 65 2), the pad's x 4 side is the cliff wall
(defn pad-drop [pad budget & [options]]
  (run (pf/world {:fill [[0 64 0 4 74 4 "stone"] [5 64 0 9 64 4 "stone"] pad]}) (near 6 65 2)
       (merge {:maxDrop 16 :damageBudget budget :landing (landing slime-id -1)} options)
       {:x 4 :y 75 :z 2}))

(def wide-pad [5 64 1 7 64 3 "slime_block"])

(deftest a-bounce-needs-a-pad-it-stays-on
  (testing "slime round the landing (or the wall): the bounce, no damage"
    (is (= 0 (cost (pad-drop wide-pad 0) :damage))))
  (testing "a 1x1 pad: the bounce may carry the body onto the stone beside it, priced as the full fall"
    (is (true? (:damageRefused (pad-drop [5 64 2 5 64 2 "slime_block"] 0))))
    (is (= 7 (cost (pad-drop [5 64 2 5 64 2 "slime_block"] 10) :damage))))
  (testing "stone at the pad's level beside the landing: no bounce price"
    (is (true? (:damageRefused (pad-drop [5 64 2 7 64 3 "slime_block"] 0))))))

;; a 1x1 slime pad at (5 64 2) at the foot of the cliff, walled round by stone h blocks over the pad's level: the 10-block
;; drop bounces about 6.3 blocks, so a lower wall lets the bounce carry the body onto it or over it
(defn walled-pad [h budget]
  (run (pf/world {:fill [[0 64 0 4 74 4 "stone"] [6 64 1 6 (+ 64 h) 3 "stone"] [5 64 1 5 (+ 64 h) 1 "stone"]
                         [5 64 3 5 (+ 64 h) 3 "stone"] [5 64 2 5 64 2 "slime_block"]]})
       (near 5 65 2)
       {:maxDrop 16 :damageBudget budget :landing (landing slime-id -1)}
       {:x 4 :y 75 :z 2}))

(deftest the-bounce-peak-and-settle-time-follow-vanilla-physics
  (testing "a 10-block fall: first peak 6.33, settled (on the pad, |vy| at most 0.4) 90 ticks after the first contact"
    (is (= 6.33 (/ (js/Math.round (* 100 (:peak (base/bounce-flight 10)))) 100)))
    (is (= 90 (:settle (base/bounce-flight 10)))))
  (testing "a 6-block fall peaks at 4.05: the wall must reach 5 (the peak plus a margin)"
    (is (= 5 (base/bounce-wall 6))))
  (testing "a 15-block fall takes 116 ticks to settle"
    (is (= 116 (:settle (base/bounce-flight 15))))))

(defn capped-pad
  "walled-pad under a 6-block cliff with stone over the ring at the cliff's top level (y 70), so the ring's top is no
  ledge to step down from into the shaft"
  [h budget]
  (run (pf/world {:fill [[0 64 0 4 70 4 "stone"] [6 64 1 6 (+ 64 h) 3 "stone"] [5 64 1 5 (+ 64 h) 1 "stone"]
                         [5 64 3 5 (+ 64 h) 3 "stone"] [5 64 2 5 64 2 "slime_block"]
                         [6 70 1 6 70 3 "stone"] [5 70 1 5 70 1 "stone"] [5 70 3 5 70 3 "stone"]]})
       (near 5 65 2)
       {:maxDrop 16 :damageBudget budget :landing (landing slime-id -1)}
       {:x 4 :y 71 :z 2}))

(deftest a-6-block-bounce-needs-a-5-high-wall
  (testing "4 high: under the 4.05 peak plus the margin, priced as the full fall"
    (is (true? (:damageRefused (capped-pad 4 0)))))
  (testing "5 high: the bounce, no damage"
    (is (= 0 (cost (capped-pad 5 0) :damage)))))

(deftest a-wall-holds-the-bounce-only-up-to-its-peak
  (testing "a shaft walled up to the bounce's peak: the bounce, no damage"
    (is (= 0 (cost (walled-pad 7 0) :damage))))
  (testing "a 1-high wall ring: refused at no budget"
    (is (true? (:damageRefused (walled-pad 1 0)))))
  (testing "a 2-high wall ring under a 10-block drop: priced as the full fall"
    (is (true? (:damageRefused (walled-pad 2 0))))
    (is (pos? (cost (walled-pad 2 10) :damage)))))

(deftest a-bounce-needs-a-pad-the-body-sees
  (testing "options.landingSeen says every pad cell is seen: the bounce"
    (is (= 0 (cost (pad-drop wide-pad 0 {:landingSeen (fn [_ _ _] true)}) :damage))))
  (testing "a pad cell not seen now: the full fall"
    (is (true? (:damageRefused (pad-drop wide-pad 0 {:landingSeen (fn [x _ z] (not (and (== x 6) (== z 3))))})))))
  (testing "no pad cell seen"
    (is (= 7 (cost (pad-drop wide-pad 10 {:landingSeen (fn [_ _ _] false)}) :damage)))))

(deftest water-drops-stay-free
  (let [r (run (pf/world {:fill [[0 64 0 4 83 4 "stone"] [5 63 0 7 64 4 "water"]]}) (near 6 63 2)
               {:maxDrop 3 :damageBudget 0}
               {:x 4 :y 84 :z 2})]
    (is (= "found" (:status r)))
    (is (= 0 (cost r :damage)))))

(deftest fall-factor-scales-the-damage
  (let [taken (plan-cliff 8 {:damageBudget 3 :damageWeight 1 :fallFactor 0.52})
        refused (plan-cliff 8 {:damageBudget 2 :damageWeight 1 :fallFactor 0.52})]
    (is (< (js/Math.abs (- 2.6 (cost taken :damage))) 1e-6))
    (is (= 1 (drops taken)))
    (is (<= (cost refused :maxDrop) 3))))

(deftest the-price-of-a-hp-decides-between-the-cliff-and-the-way-round
  (let [round (cost (plan-cliff 8 {:damageBudget 0}) :seconds)
        cheap (/ round 5 2)
        dear (/ (* round 2) 5)
        r #(plan-cliff 8 {:damageBudget 7 :damageWeight %})]
    (is (= 5 (cost (r cheap) :damage)))
    (is (= 0 (cost (r dear) :damage)))))

(deftest a-returnable-search-never-takes-the-cliff
  (let [r (plan-cliff 8 {:damageBudget 7 :damageWeight 1 :returnable true})]
    (is (zero? (cost r :damage)) "a returnable search never takes the cliff")))

;; a wall x 10..11 with a gap of berry bushes (2 across, -1 hp each) and a detour round its end
(def corridor
  (pf/world {:fill [[10 64 -2 11 66 30 "stone"] [10 64 10 11 66 11 "air"]]
             :blocks [[10 64 10 "sweet_berry_bush"] [11 64 10 "sweet_berry_bush"] [10 64 11 "sweet_berry_bush"] [11 64 11 "sweet_berry_bush"]]}))

(def corridor-from {:x 2 :y 64 :z 10})
(def corridor-goal (near 18 64 10))

(defn through [options] (run corridor corridor-goal options corridor-from))
(defn crosses-berries? [r] (boolean (some #{[10 64 10] [10 64 11]} (cells r))))

(deftest berries-are-walked-when-the-detour-costs-more-than-the-hp
  (let [round (through {:damageBudget 0})]
    (is (= "found" (:status round)))
    (is (not (crosses-berries? round)))
    (let [secs (cost round :seconds)
          taken (through {:damageBudget 5 :damageWeight 1})
          refused (through {:damageBudget 1 :damageWeight 1})
          dear (through {:damageBudget 5 :damageWeight (* 2 secs)})]
      (is (crosses-berries? taken))
      (is (= 2 (cost taken :damage)))
      (is (not (crosses-berries? refused)) "a budget of 1 hp cannot take both bushes")
      (is (not (crosses-berries? dear))))))

;; Two openings in a wall and a second wall behind them: the near opening is 2 bushes (2 hp), the far one 1 bush; the way on
;; is one more bush (z 15 of the back wall). Within budget 2 only the dearer far opening leads on: a node reached by the
;; cheaper, hurt branch and again by the dearer, unhurt one keeps both records.
(def two-doors
  (pf/world {:fill [[10 64 -40 11 66 70 "stone"] [10 64 10 11 66 10 "air"] [10 64 20 10 66 20 "air"] [11 64 20 11 66 20 "air"]
                    [14 64 -40 14 66 70 "stone"] [14 64 15 14 66 15 "air"]]
             :blocks [[10 64 10 "sweet_berry_bush"] [11 64 10 "sweet_berry_bush"] [10 64 20 "sweet_berry_bush"] [14 64 15 "sweet_berry_bush"]]}))

(defn two-doors-run [budget]
  (run two-doors (near 18 64 15) {:damageBudget budget :damageWeight 0} {:x 2 :y 64 :z 10}))

(deftest a-node-keeps-a-record-per-hp-spent
  (let [roomy (two-doors-run 3)
        tight (two-doors-run 2)]
    (is (= "found" (:status roomy)))
    (is (some #{[10 64 10]} (cells roomy)) "the near opening is the cheaper way")
    (is (= 3 (cost roomy :damage)))
    (is (= "found" (:status tight)))
    (is (= 2 (cost tight :damage)) (pr-str (cells tight)))
    (is (some #{[10 64 20]} (cells tight)))))
