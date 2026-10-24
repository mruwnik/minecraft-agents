(ns engine.threat-field-test
  "jobs.lib.threat-field: a known mob's field is the cells its way and line of fire reach, not a sphere round it; jobs.lib.cost/death-chance prices a route by it."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.rays :as rays]
            [engine.test-util :as tu]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost :as cost]
            [jobs.lib.threat-field :as field]))

(defn reach-of [spec mob pos] (field/reach-fn (field/field (tu/seeing-all (tu/fake-on-floor spec)) {:mob mob :pos pos})))

(def pen
  "A stone ring 3 high (y 64..66) round x -3..3, z -3..3: the floor beyond the floor rectangle is unseen, a way for a mob."
  (into {} (for [x (range -3 4) z (range -3 4) :when (or (#{-3 3} x) (#{-3 3} z)) y [64 65 66]] [(str x "," y "," z) "stone"])))

(deftest a-walkers-field-is-its-way-out-to-its-follow-range-not-a-sphere
  (let [reach (reach-of {} "zombie" {:x -20.5 :y 64 :z 0.5})]
    (is (= 1 (reach -20 64 0)) "its own cell")
    (is (< 0 (reach 10 64 0) (reach -10 64 0) 1) "falling off along its way: 30 steps off still counts, past a 16 or 24 block sphere")
    (is (nil? (reach 16 64 0)) "past its follow range (35)"))
  (let [reach (reach-of {:blocks pen} "zombie" {:x 0.5 :y 64 :z 0.5})]
    (is (= 1 (reach 1 64 0)))
    (is (nil? (reach 5 64 0)) "5 blocks off outside a pen it cannot leave: outside")))

(deftest a-field-given-an-extent-holds-the-way-to-every-cell-within-it
  (let [at (fn [threat] (field/reach-fn (field/field (tu/seeing-all (tu/fake-on-floor {})) (merge {:mob "zombie" :pos {:x 0.5 :y 64 :z 0.5}} threat))))]
    (is (nil? ((at {}) 30 64 14)) "44 steps along the grid: past its follow range (35)")
    (is (some? ((at {:extent 35}) 30 64 14)) "within 35 blocks as the crow flies: its way there is held")))

(deftest a-ranged-mobs-field-adds-the-cells-it-can-shoot
  (let [pillar {"6,64,0" "stone" "6,65,0" "stone" "6,66,0" "stone"}
        skeleton (reach-of {:blocks pillar} "skeleton" {:x 0.5 :y 64 :z 0.5})
        zombie (reach-of {:blocks pillar} "zombie" {:x 0.5 :y 64 :z 0.5})]
    (is (nil? (zombie 6 67 0)) "a melee mob does not climb the pillar")
    (is (= 1 (skeleton 6 67 0)) "a skeleton shoots the body on it"))
  (is (nil? ((reach-of {:blocks pen} "skeleton" {:x 0.5 :y 64 :z 0.5}) 5 64 0)) "no line of fire through the pen's wall"))

(deftest a-ranged-mobs-fire-cells-end-at-its-kinds-range
  (let [pillar {"12,64,0" "stone" "12,65,0" "stone" "12,66,0" "stone"}
        at (fn [mob] (reach-of {:blocks pillar} mob {:x 0.5 :y 64 :z 0.5}))]
    (is (= 1 ((at "skeleton") 12 67 0)) "a skeleton shoots 12 blocks (range 15)")
    (is (nil? ((at "pillager") 12 67 0)) "a pillager's crossbow reaches 8")))

(deftest a-walk-within-a-zombies-way-is-a-likelier-death-than-one-past-a-pen
  (let [body {:health 20}
        route {:cells (for [x (range 5 15)] {:x x :y 64 :z 0})}
        chance (fn [spec] (cost/death-chance body (assoc route :threats [{:mob "zombie" :weight 1 :reach (reach-of spec "zombie" {:x 0.5 :y 64 :z 0.5})}])))]
    (is (> (chance {}) (chance {:blocks pen})))
    (is (= (cost/death-chance body {}) (chance {:blocks pen})) "penned in: only the route's class")))

(deftest a-kept-field-holds-until-the-body-sees-a-cell-of-it-change
  (let [raw (tu/fake-on-floor {:floor [-40 -20 40 20] :self {:pos {:x 12.5 :y 64 :z 0.5}}})
        per (perception/create (fake-raw/create raw) {:now (constantly 1000000)})
        c {:primitives (perception/wrap raw per) :view (fn [] {:now 0 :data {}})}
        look! #(doseq [yaw [0 90 180 270]] (swap! (fake/state raw) assoc :yaw yaw) (rays/pass! per))
        zombie {:mob "zombie" :pos {:x 0.5 :y 64 :z 0.5}}
        _ (look!)
        before (field/field-of c zombie)]
    (swap! (fake/state raw) fake/put-block [-60 64 0] "stone")
    (rays/touch! per [-60 64 0])
    (is (identical? before (field/field-of (check-cache/fresh c) zombie))
        "a cell it did not read changed: the same field, for a fresh check too")
    (swap! (fake/state raw) #(-> % (fake/put-block [6 64 0] "stone") (fake/put-block [6 65 0] "stone")))
    (look!)
    (is (some? ((field/reach-fn before) 6 64 0)))
    (is (nil? ((field/reach-fn (field/field-of c zombie)) 6 64 0)) "a pillar seen on its way: no longer in its field")))

(defn seen-body
  "A primitives-and-perception pair over a floor with the body at x 12.5; (look!) turns it all round."
  [spec]
  (let [raw (tu/fake-on-floor (merge {:floor [-40 -20 40 20] :self {:pos {:x 12.5 :y 64 :z 0.5}}} spec))
        per (perception/create (fake-raw/create raw) {:now (constantly 1000000)})]
    {:c {:primitives (perception/wrap raw per) :view (fn [] {:now 0 :data {}})}
     :per per
     :look! #(doseq [yaw [0 90 180 270]] (swap! (fake/state raw) assoc :yaw yaw) (rays/pass! per))}))

(deftest a-water-mobs-field-follows-the-water-the-body-sees
  (let [pool (into {} (for [x (range 8 11) z (range -1 2)] [(str x ",63," z) "water"]))
        {:keys [c look!]} (seen-body {:blocks pool})
        drowned {:mob "drowned" :pos {:x 9.5 :y 63 :z 0.5}}
        before (field/field-of c drowned)]
    (is (zero? (.-length (:cells before))) "no water seen yet: no field")
    (look!)
    (is (= 9 (.-length (:cells (field/field-of c drowned)))) "the pool seen: its cells")))

(deftest a-field-is-not-kept-when-the-section-reads-were-cleared-during-its-read
  (let [{:keys [c per look!]} (seen-body {})
        zombie {:mob "zombie" :pos {:x 0.5 :y 64 :z 0.5}}
        other {:mob "zombie" :pos {:x 20.5 :y 64 :z 10.5}}]
    (look!)
    (field/field-of c zombie)
    (let [secs (.-secs ^js (.get field/memories (:primitives c)))]
      (dotimes [i field/max-sections] (.set secs (- -1 i) #js {})))
    (let [again (field/field-of c other)]
      (is (not (identical? again (field/field-of c other))) "its entries are no longer in the body's section reads"))))

(deftest the-kept-reads-start-over-in-another-dimension
  (let [{:keys [c per look!]} (seen-body {})]
    (look!)
    (field/field-of c {:mob "zombie" :pos {:x 0.5 :y 64 :z 0.5}})
    (let [mem ^js (.get field/memories (:primitives c))]
      (is (= 1 (.-size (.-fields mem))))
      (set! (.-dim ^js (:st per)) "the_nether")
      (field/field-of c {:mob "zombie" :pos {:x 20.5 :y 64 :z 10.5}})
      (is (= 1 (.-size (.-fields mem))) "the other dimension's fields are dropped"))))

(deftest the-time-a-route-spends-in-a-zombies-field-prices-with-how-close-it-comes
  (let [body {:health 20}
        threats [{:mob "zombie" :weight 1 :reach (reach-of {} "zombie" {:x -20.5 :y 64 :z 0.5})}]
        route (fn [cells] {:cells cells :threats threats})
        crossing (route (for [x (range 10 13)] {:x x :y 64 :z 0}))
        along (route (for [z (range -8 9)] {:x 10 :y 64 :z z}))
        close (route (for [x (range -22 -18)] {:x x :y 64 :z 0}))
        stay (fn [s] (route [{:x 10 :y 64 :z 0 :s s}]))
        base (cost/death-chance body {})
        hurt #(cost/route-hurt body %)
        dies #(- (cost/death-chance body %) base)]
    (is (< 0 (hurt crossing) (hurt along) (hurt close)) "a long walk along the far edge over a short crossing of it, a close crossing over both")
    (is (< 0 (dies crossing) (dies along) (dies close)))
    (is (< 0 (hurt (stay 1)) (hurt (stay 600))) "600 s at a far-edge cell over 1 s there")
    (is (< 0 (dies (stay 1)) (dies (stay 600))))))
