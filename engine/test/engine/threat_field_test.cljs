(ns engine.threat-field-test
  "jobs.lib.threat-field: a known mob's field is the cells its way and line of fire reach, not a sphere round it; jobs.lib.cost/death-chance prices a route by it."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.rays :as rays]
            [engine.test-util :as tu]
            [engine.path.planner.base :as base]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost :as cost]
            [jobs.lib.memory-age :as memory-age]
            [jobs.lib.reach :as reach]
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
  (let [at (fn [threat] (let [{:keys [steps]} (field/field (tu/seeing-all (tu/fake-on-floor {})) (merge {:mob "zombie" :pos {:x 0.5 :y 64 :z 0.5}} threat))]
                          (fn [x y z] (.get steps (base/cell-key x y z)))))]
    (is (nil? ((at {}) 30 64 14)) "44 steps along the grid: past its follow range (35)")
    (is (= 44 ((at {:extent 35}) 30 64 14)) "within 35 blocks as the crow flies: its way there is held")))

(deftest an-extent-field-holds-the-steps-the-walk-gives-and-no-cells
  (let [p (tu/seeing-all (tu/fake-on-floor {:blocks (merge pen {"9,64,2" "stone" "9,65,2" "stone" "10,64,2" "stone" "10,65,2" "stone"})}))
        pos {:x 4.5 :y 64 :z 0.5}
        extent 20
        span (field/walk-range "zombie" extent)
        walked (field/walk-field (reach/mob-lookup p) (reach/cell-of pos) span 10 4000)
        expected (into {} (map (fn [^js c] [(base/cell-key (aget c 0) (aget c 1) (aget c 2)) (aget c 4)])) walked)
        f (field/field p {:mob "zombie" :pos pos :extent extent})]
    (is (= expected (into {} (map vec) (es6-iterator-seq (.entries ^js (:steps f))))) "the steps of the walk to the range")
    (is (zero? (.-length (:cells f))) "no cells, no weights")))

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
    (is (zero? (chance {:blocks pen})) "penned in: the zombie never meets the walk")))

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

(deftest a-walkers-field-and-its-steps-over-an-extent-are-one-walk
  (let [{:keys [c look!]} (seen-body {:blocks pen})
        zombie {:mob "zombie" :pos {:x 20.5 :y 64 :z 0.5}}
        steps (fn [f] (into {} (map vec) (es6-iterator-seq (.entries ^js (:steps f)))))]
    (look!)
    (let [f (field/field-of c zombie)
          e (field/field-of c (assoc zombie :extent 35))]
      (is (= 1 (.-size (.-fields ^js (.get field/memories (:primitives c))))) "the walk is kept once, for both")
      (is (= (steps (field/field (:primitives c) zombie)) (steps f)) "the field as its own walk gives it")
      (is (= (steps (field/field (:primitives c) (assoc zombie :extent 35))) (steps e)) "the steps over the extent as their own walk gives them")
      (is (< (.-size (:steps f)) (.-size (:steps e))) "the extent walks farther"))))

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

(deftest a-roaming-mob-keeps-at-most-max-fields-the-least-recently-used-dropped
  (let [{:keys [c look!]} (seen-body {})
        zombie (fn [x] {:mob "zombie" :pos {:x (+ x 0.5) :y 64 :z 0.5}})]
    (look!)
    (let [home (field/field-of c (zombie -30))]
      (doseq [x (range -27 30 3)]
        (field/field-of c (zombie -30))
        (field/field-of c (zombie x)))
      (is (= field/max-fields (.-size (.-fields ^js (.get field/memories (:primitives c))))) "20 places walked, max-fields kept")
      (is (identical? home (field/field-of c (zombie -30))) "the place asked all along is still kept"))))

(deftest a-shared-value-of-an-older-world-is-dropped-at-the-next-miss
  (let [epoch (atom 0)
        p #js {:worldEpoch (fn [] @epoch)}
        c {:primitives p :view (fn [] {:now 0 :data {}})}]
    (field/shared c [:a] (constantly 1))
    (reset! epoch 1)
    (field/shared c [:b] (constantly 2))
    (is (= ["[:b]"] (vec (es6-iterator-seq (.keys ^js (.get field/bodies p))))) "a value no check can hit is not held")))

;; a mob roaming in a still world asks under a new key per place: the body must not keep a toll vector of each
(deftest shared-keeps-at-most-max-fields-values-as-keys-and-epochs-come
  (let [epoch (atom 0)
        p #js {:worldEpoch (fn [] @epoch)}
        c {:primitives p :view (fn [] {:now 0 :data {}})}
        sizes (for [i (range 200)]
                (do (when (zero? (mod i 50)) (swap! epoch inc))
                    (field/shared c [:place i] (constantly i))
                    (.-size ^js (.get field/bodies p))))]
    (is (= field/max-fields (apply max sizes)))
    (is (= 199 (field/shared c [:place 199] (constantly :read-again))) "the latest is still kept")))

(deftest a-shared-value-is-read-again-when-the-memory-changes
  (let [mem (atom 0)
        p #js {:worldEpoch (fn [] 0) :perception {:st #js {}}}
        c {:primitives p :view (fn [] {:now 0 :data {}})}]
    (with-redefs [memory-age/epoch (fn [_] @mem)]
      (field/shared c [:a] (constantly 1))
      (is (= 1 (field/shared c [:a] (constantly 2))) "the same world and memory: kept")
      (swap! mem inc)
      (is (= 3 (field/shared c [:a] (constantly 3))) "the body saw more: its fields read memory, so read again"))))

(deftest a-body-without-a-world-epoch-keeps-no-shared-value
  (let [p #js {}
        c {:primitives p :view (fn [] {:now 0 :data {}})}
        n (atom 0)]
    (field/shared c [:a] #(swap! n inc))
    (field/shared c [:a] #(swap! n inc))
    (is (= 2 @n))
    (is (nil? (.get field/bodies p)) "nothing is stored for a body without an epoch")))

(deftest a-shared-field-is-kept-until-the-fakes-world-changes
  (let [p (tu/fake-on-floor {:floor [-40 -20 40 20] :self {:pos {:x 12.5 :y 64 :z 0.5}}})
        c {:primitives p :view (fn [] {:now 0 :data {}})}
        zombie {:mob "zombie" :pos {:x 0.5 :y 64 :z 0.5}}
        before (field/field-of c zombie)]
    (is (identical? before (field/field-of c zombie)) "the world did not change: the same field, however long after")
    (swap! (fake/state p) #(-> % (fake/put-block [6 64 0] "stone") (fake/put-block [6 65 0] "stone")))
    (is (some? ((field/reach-fn before) 6 64 0)))
    (is (nil? ((field/reach-fn (field/field-of c zombie)) 6 64 0)) "a pillar put on its way: the field is read again")))

(deftest the-time-a-route-spends-in-a-zombies-field-prices-with-how-close-it-comes
  (let [body {:health 20}
        threats [{:mob "zombie" :weight 1 :reach (reach-of {} "zombie" {:x -20.5 :y 64 :z 0.5})}]
        route (fn [cells] {:cells cells :threats threats})
        crossing (route (for [x (range 10 13)] {:x x :y 64 :z 0}))
        along (route (for [z (range -8 9)] {:x 10 :y 64 :z z}))
        close (route (for [x (range -22 -18)] {:x x :y 64 :z 0}))
        stay (fn [s] (route [{:x 10 :y 64 :z 0 :s s}]))
        hurt #(:hurt (cost/route-parts body %))
        dies #(cost/death-chance body %)]
    (is (< 0 (hurt crossing) (hurt along) (hurt close)) "a long walk along the far edge over a short crossing of it, a close crossing over both")
    (is (< 0 (dies crossing) (dies along) (dies close)))
    (is (< 0 (hurt (stay 1)) (hurt (stay 600))) "600 s at a far-edge cell over 1 s there")
    (is (< 0 (dies (stay 1)) (dies (stay 600))))))

(def hut
  "The route-risk fixture's hut: stone x 14..18, z 14..18, y 64..66, hollow inside, glass panes and a shut door in the east wall (x 18)."
  (merge (into {} (for [x (range 14 19) z (range 14 19) y [64 65 66]
                         :when (or (= 66 y) (#{14 18} x) (#{14 18} z))]
                    [(str x "," y "," z) "stone"]))
         {"18,64,15" "glass" "18,65,15" "glass" "18,64,17" "glass" "18,65,17" "glass"
          "18,64,16" "oak_door" "18,65,16" "oak_door"}))

(deftest a-sealed-room-the-body-has-seen-is-outside-a-zombies-field
  (let [{:keys [c look!]} (seen-body {:blocks hut :self {:pos {:x 16.5 :y 64 :z 16.5}}})
        _ (look!)
        reach (field/reach-fn (field/field-of c {:mob "zombie" :pos {:x 30.5 :y 64 :z 15.5}}))]
    (is (nil? (reach 16 64 16)) "walls, glass, door and roof seen from inside: the zombie outside has no way in")))
