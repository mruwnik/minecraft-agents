(ns engine.fake-test
  "engine.fake: the fake primitives world (a port of js/fake.test.mjs)."
  (:require [cljs.test :refer [deftest is are async]]
            [clojure.string :as str]
            [engine.fake :as fake]
            [engine.test-util :as tu]))

(defn at [x y z] {:x x :y y :z z})

(def got #(js->clj % :keywordize-keys true))

(defn tick [] (js/Promise. (fn [resolve] (js/setTimeout resolve 1))))

(defn owned
  ([] (owned {}))
  ([spec]
   (let [p (fake/create spec)]
     (.setOwner p "t1")
     p)))

(defn act
  "Call primitive method on p as token t1 with the cljs map m as its args."
  [p method m]
  (js-invoke p method "t1" (clj->js m)))

(defn async-test [f]
  (async done (tu/run-async done f)))

(defn ^:async for-each
  "Await (f case) for each case, in order."
  [f cases]
  (loop [[c & more] (seq cases)]
    (when c
      (await (f c))
      (recur more))))

(defn rejection
  "The error the promise returned by thunk rejects with, or nil when it resolves."
  [thunk]
  (try
    (-> (thunk) (.then (fn [_] nil)) (.catch identity))
    (catch :default e (js/Promise.resolve e))))

(defn thrown [thunk]
  (try (thunk) nil (catch :default e e)))

(defn ents [p m] (got (.entities p (clj->js m))))
(defn blocks-of [p m] (got (.blocks p (clj->js m))))
(defn block-at [p x y z] (got (.blockAt p (tu/pos x y z))))
(defn self-of [p] (got (.self p)))
(defn inv-pairs [p] (mapv (juxt :name :count) (:inventory (self-of p))))
(defn item-ents [p] (ents p {:kind "item"}))
(defn world [p] (.-world p))
(defn calls [p] (.-calls (world p)))
(defn offline? [p] (:offline @(fake/state p)))

(defn body-events [p]
  (let [seen (atom [])]
    (.onBodyEvent p (fn [e] (swap! seen conj (got e))))
    seen))

(defn xyz [k] (mapv js/parseInt (str/split k #",")))

(deftest self-reports-position-vitals-time-and-inventory
  (let [p (fake/create {:self {:pos (at 1 64 2) :health 15 :food 9 :username "F"}
                        :time 13000
                        :inventory [{:name "bread" :count 2}]})
        s (self-of p)]
    (is (= (at 1 64 2) (:pos s)))
    (is (= 15 (:health s)))
    (is (= 9 (:food s)))
    (is (= false (:isDay s)))
    (is (= "F" (:username s)))
    (is (= [["bread" 2]] (mapv (juxt :name :count) (:inventory s))))))

(deftest is-day-at-time
  (are [time day?] (= day? (:isDay (self-of (fake/create {:time time}))))
    1000 true
    12000 true
    13000 false
    23000 false
    23500 true))

(deftest entities-are-filtered-by-radius-and-kind-and-sorted-by-distance
  (let [p (fake/create {:entities [{:id 1 :name "zombie" :kind "hostile" :pos (at 10 64 0)}
                                   {:id 2 :name "cow" :kind "passive" :pos (at 2 64 0)}
                                   {:id 3 :name "skeleton" :kind "hostile" :pos (at 4 64 0)}
                                   {:id 4 :name "creeper" :kind "hostile" :pos (at 40 64 0)}]})]
    (is (= [3 1] (mapv :id (ents p {:radius 16 :kind "hostile"}))))
    (is (= [2 3 1] (mapv :id (ents p {:radius 16}))))
    (is (= [2] (mapv :id (ents p {:names ["cow"]}))))
    (is (= 1 (count (ents p {:radius 16 :max 1}))))))

(deftest blocks-match-by-names-or-predicate-sorted-by-distance
  (let [p (fake/create {:blocks {"5,64,0" "oak_log" "2,64,0" "birch_log" "1,63,0" "dirt" "50,64,0" "oak_log"}})]
    (is (= ["birch_log" "oak_log"] (mapv :name (blocks-of p {:names ["oak_log" "birch_log"]}))))
    (is (= [2 5 50] (mapv (comp :x :pos) (blocks-of p {:match #(str/ends-with? % "_log") :radius 100}))))
    (is (= "dirt" (:name (block-at p 1 63 0))))
    (is (= "air" (:name (block-at p 9 9 9))))))

(deftest block-at-returns-null-for-a-cell-in-an-unloaded-chunk
  (let [p (fake/create {:blocks {"1,64,0" "dirt"} :unloaded ["1,64,0" "2,64,0"]})]
    (is (nil? (.blockAt p (tu/pos 1 64 0))))
    (is (nil? (.blockAt p (tu/pos 2 64 0))))
    (is (= "air" (:name (block-at p 3 64 0))))))

(deftest acting-with-a-stale-token-rejects-with-cut
  (async-test
   (fn ^:async t []
     (let [p (owned)
           e (await (rejection #(.moveTo p "other" (clj->js {:pos (at 1 64 0)}))))]
       (is (= "cut" (some-> e .-code)))))))

(deftest changing-the-owner-cuts-a-held-call
  (async-test
   (fn ^:async t []
     (let [p (owned)
           _ (.hold (world p) "moveTo")
           walk (.moveTo p "t1" (clj->js {:pos (at 3 64 0)}))
           _ (.setOwner p "t2")
           e (await (rejection (fn [] walk)))]
       (is (= "cut" (some-> e .-code)))
       (is (= (at 0 64 0) (:pos (self-of p))))))))

(deftest a-released-hold-runs-the-default-implementation-or-returns-the-given-result
  (async-test
   (fn ^:async t []
     (let [p (owned)
           release (.hold (world p) "moveTo")
           walk (.moveTo p "t1" (clj->js {:pos (at 3 64 0)}))
           _ (release)
           walked (await walk)
           release2 (.hold (world p) "dig")
           dig (.dig p "t1" (clj->js {:pos (at 1 64 0)}))
           _ (release2 #js {:status "cannot"})
           dug (await dig)]
       (is (= "arrived" (.-status walked)))
       (is (= "cannot" (.-status dug)))))))

(deftest move-to-arrives-goes-partial-past-max-distance-and-is-blocked-when-unreachable
  (async-test
   (fn ^:async t []
     (let [p (owned {:unreachable ["9,64,9"]})
           near (await (act p "moveTo" {:pos (at 3 64 4)}))
           at-near (:pos (self-of p))
           far (got (await (act p "moveTo" {:pos (at 103 64 4) :maxDistance 10})))
           blocked (await (act p "moveTo" {:pos (at 9 64 9)}))]
       (is (= "arrived" (.-status near)))
       (is (= (at 3 64 4) at-near))
       (is (= "partial" (:status far)))
       (is (= (at 13 64 4) (:pos far)))
       (is (= "blocked" (.-status blocked)))))))

(deftest dig-removes-the-block-and-drops-an-item-entity
  (async-test
   (fn ^:async t []
     (let [p (owned {:blocks {"1,64,0" "stone" "20,64,0" "stone"} :drops {"stone" "cobblestone"}})
           r (got (await (act p "dig" {:pos (at 1 64 0)})))
           after (:name (block-at p 1 64 0))
           dropped (get-in (first (item-ents p)) [:item :name])
           again (await (act p "dig" {:pos (at 1 64 0)}))
           far (await (act p "dig" {:pos (at 20 64 0)}))]
       (is (= "dug" (:status r)))
       (is (= "stone" (:block r)))
       (is (= "cobblestone" (:name (first (:drops r)))))
       (is (= "air" after))
       (is (= "cobblestone" dropped))
       (is (= "missing" (.-status again)))
       (is (= "unreachable" (.-status far)))))))

(deftest place-of-a-campfire-lights-it-other-blocks-get-no-lit-state
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "campfire" :count 1} {:name "stone" :count 1}]})
           r1 (await (act p "place" {:pos (at 1 64 0) :item "campfire"}))
           lit (get-in (block-at p 1 64 0) [:properties :lit])
           r2 (await (act p "place" {:pos (at 2 64 0) :item "stone"}))
           stone-lit (get-in (block-at p 2 64 0) [:properties :lit])]
       (is (= "placed" (.-status r1)))
       (is (= true lit))
       (is (= "placed" (.-status r2)))
       (is (nil? stone-lit))))))

(deftest collect-moves-a-dropped-item-into-the-inventory
  (async-test
   (fn ^:async t []
     (let [p (owned {:blocks {"1,64,0" "oak_log"}})
           {:keys [drops]} (got (await (act p "dig" {:pos (at 1 64 0)})))
           id (:id (first drops))
           r (got (await (act p "collect" {:id id})))
           have (:count (first (filter #(= "oak_log" (:name %)) (:inventory (self-of p)))))
           again (await (act p "collect" {:id id}))]
       (is (= "collected" (:status r)))
       (is (= [{:name "oak_log" :count 1}] (:gained r)))
       (is (= 1 have))
       (is (= "gone" (.-status again)))))))

(deftest place-uses-an-inventory-item-on-an-empty-cell
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "dirt" :count 1}] :blocks {"1,64,0" "stone"}})
           r1 (await (act p "place" {:pos (at 1 64 0) :item "dirt"}))
           r2 (await (act p "place" {:pos (at 2 64 0) :item "dirt"}))
           placed (:name (block-at p 2 64 0))
           r3 (await (act p "place" {:pos (at 3 64 0) :item "dirt"}))]
       (is (= "occupied" (.-status r1)))
       (is (= "placed" (.-status r2)))
       (is (= "dirt" placed))
       (is (= "no-item" (.-status r3)))))))

(deftest place-puts-a-block-into-water-and-lava
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "dirt" :count 2}]
                     :blocks {"1,64,0" "water" "2,64,0" "lava"}
                     :states {"1,64,0" {:level 0}}})
           r1 (await (act p "place" {:pos (at 1 64 0) :item "dirt"}))
           b (block-at p 1 64 0)
           r2 (await (act p "place" {:pos (at 2 64 0) :item "dirt"}))]
       (is (= "placed" (.-status r1)))
       (is (= "dirt" (:name b)))
       (is (nil? (get-in b [:properties :level])))
       (is (= "placed" (.-status r2)))))))

(deftest containers-can-be-inspected-and-transferred-to-and-from
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "oak_log" :count 5}]
                     :containers {"1,64,1" [{:name "cobblestone" :count 10}]}})
           dep (await (act p "transfer" {:pos (at 1 64 1) :direction "deposit" :item "oak_log" :count 3}))
           wd (await (act p "transfer" {:pos (at 1 64 1) :direction "withdraw" :item "cobblestone" :count 20}))
           seen (got (await (act p "inspectContainer" {:pos (at 1 64 1)})))
           missing (await (act p "inspectContainer" {:pos (at 30 64 1)}))]
       (is (= ["ok" 3] [(.-status dep) (.-moved dep)]))
       (is (= ["ok" 10] [(.-status wd) (.-moved wd)]))
       (is (= [["oak_log" 3]] (mapv (juxt :name :count) (:items seen))))
       (is (= "missing" (.-status missing)))))))

(deftest equip-and-eat-use-the-inventory
  (async-test
   (fn ^:async t []
     (let [p (owned {:self {:food 10} :inventory [{:name "bread" :count 1} {:name "iron_axe" :count 1}]})
           eq (await (act p "equip" {:item "iron_axe"}))
           held (:held (self-of p))
           none (await (act p "equip" {:item "diamond"}))
           ate (await (act p "eat" {}))
           no-food (await (act p "eat" {}))]
       (is (= "equipped" (.-status eq)))
       (is (= "iron_axe" held))
       (is (= "no-item" (.-status none)))
       (is (= ["ate" "bread" 15] [(.-status ate) (.-item ate) (.-food ate)]))
       (is (= "no-food" (.-status no-food)))))))

(deftest attack-hits-then-kills
  (async-test
   (fn ^:async t []
     (let [p (owned {:entities [{:id 5 :name "zombie" :kind "hostile" :pos (at 2 64 0) :health 6}]})
           a (await (act p "attack" {:id 5}))
           b (await (act p "attack" {:id 5}))
           c (await (act p "attack" {:id 5}))]
       (is (= "hit" (.-status a)))
       (is (= "killed" (.-status b)))
       (is (= "gone" (.-status c)))))))

(deftest attack-reports-hurt-and-an-invulnerable-entity-takes-no-damage
  (async-test
   (fn ^:async t []
     (let [p (owned {:entities [{:id 5 :name "zombie" :kind "hostile" :pos (at 2 64 0) :health 6}
                                {:id 6 :name "zombie" :kind "hostile" :pos (at 2 64 1) :invulnerable true}]})
           a (got (await (act p "attack" {:id 6})))
           b (got (await (act p "attack" {:id 5})))
           c (got (await (act p "attack" {:id 5})))]
       (is (= {:status "hit" :health 20 :hurt false} a))
       (is (= {:status "hit" :health 1 :hurt true} b))
       (is (= {:status "killed" :health 0 :hurt true} c))))))

(deftest sleep-works-at-night-on-a-bed-and-makes-it-morning
  (async-test
   (fn ^:async t []
     (let [p (owned {:time 1000 :blocks {"1,64,0" "red_bed"}})
           day (await (act p "sleep" {:pos (at 1 64 0)}))
           _ (.setTime (world p) 14000)
           nobed (await (act p "sleep" {:pos (at 2 64 0)}))
           slept (await (act p "sleep" {:pos (at 1 64 0)}))]
       (is (= "not-night" (.-status day)))
       (is (= "missing" (.-status nobed)))
       (is (= "sleeping" (.-status slept)))
       (is (= true (:isDay (self-of p))))))))

(deftest calls-are-logged-and-overrides-replace-an-implementation
  (async-test
   (fn ^:async t []
     (let [p (owned)
           _ (.override (world p) "look"
                        (fn ^:async o [token args impl]
                          (let [r (await (impl token args))]
                            #js {:status "odd" :viaDefault (.-status r)})))
           r (got (await (act p "look" {:yaw 0 :pitch 0})))]
       (is (= {:status "odd" :viaDefault "ok"} r))
       (is (= [["look" "t1"]] (mapv (juxt #(.-name %) #(.-token %)) (calls p))))))))

(deftest body-events-reach-listeners-until-unsubscribed
  (let [p (fake/create {})
        seen (atom [])
        off (.onBodyEvent p (fn [e] (swap! seen conj (.-kind e))))]
    (.emit (world p) #js {:kind "hurt" :health 5})
    (off)
    (.emit (world p) #js {:kind "chat"})
    (is (= ["hurt"] @seen))))

(deftest die-emits-died-with-the-position-inventory-and-experience-then-drops-the-inventory-there
  (let [p (fake/create {:self {:pos (at 3 64 1) :experience {:level 4 :points 60 :progress 0.5}}
                        :inventory [{:name "bread" :count 2}]})
        seen (body-events p)]
    (.die (world p))
    (is (= [{:kind "died"
             :pos (at 3 64 1)
             :inventory [{:name "bread" :count 2 :slot 0}]
             :experience {:level 4 :points 60}}]
           @seen))
    (is (= [] (:inventory (self-of p))))
    (is (= {:level 0 :points 0 :progress 0} (:experience (self-of p))))
    (is (= [["bread" 2 (at 3 64 1)]]
           (mapv (juxt #(get-in % [:item :name]) #(get-in % [:item :count]) :pos) (item-ents p))))))

(deftest self-carries-the-survival-fields-with-healthy-defaults
  (let [s (self-of (fake/create {}))]
    (is (= [20 false false false false 5 "overworld"]
           [(:oxygen s) (:onFire s) (:inWater s) (:inLava s) (:isSleeping s) (:foodSaturation s) (:dimension s)]))
    (is (= {:level 0 :points 0 :progress 0} (:experience s)))))

(deftest self-effects-default-to-none-and-come-from-the-spec
  (let [effects [{:name "fire_resistance" :amplifier 0 :duration 600}]]
    (is (= [] (:effects (self-of (fake/create {})))))
    (is (= effects (:effects (self-of (fake/create {:self {:effects effects}})))))))

(deftest self-survival-fields-come-from-the-spec
  (let [s (self-of (fake/create {:self {:oxygen 3 :onFire true :inLava true :dimension "the_nether"
                                        :experience {:level 2 :points 20 :progress 0.5}}}))]
    (is (= [3 true true "the_nether" 2]
           [(:oxygen s) (:onFire s) (:inLava s) (:dimension s) (:level (:experience s))]))))

(deftest player-entities-default-to-awake-with-a-username-mobs-get-neither
  (let [p (fake/create {:entities [{:id 1 :name "Ann" :kind "player" :pos (at 1 64 0)}
                                   {:id 2 :name "creeper" :kind "hostile" :creeper true :pos (at 2 64 0)}
                                   {:id 3 :name "Sue" :kind "player" :sleeping true :pos (at 3 64 0)}]})
        [ann creeper sue] (ents p {})]
    (is (= ["Ann" false true true false]
           [(:username ann) (:sleeping ann) (:sleeping sue) (:creeper creeper) (contains? creeper :sleeping)]))))

(deftest a-creeper-entity-carries-creeper-true-without-being-told-other-hostiles-do-not
  (let [p (fake/create {:entities [{:id 1 :name "creeper" :kind "hostile" :pos (at 1 64 0)}
                                   {:id 2 :name "zombie" :kind "hostile" :pos (at 2 64 0)}]})
        [creeper zombie] (ents p {})]
    (is (= [true false] [(:creeper creeper) (contains? zombie :creeper)]))))

(deftest offline-flips-the-flag-emits-offline-and-online-and-resolves-ok-after-the-wait
  (async-test
   (fn ^:async t []
     (let [p (owned {:offlineScale 0.001})
           seen (body-events p)
           pending (act p "offline" {:ms 20000})
           _ (await (tick))
           during (offline? p)
           r (got (await pending))
           after (offline? p)]
       (is (= true during))
       (is (= {:status "ok" :ms 20000} r))
       (is (= false after))
       (is (= ["offline" "online"] (mapv :kind @seen)))))))

(deftest fake-offline-with-ms-reports
  (async-test
   (fn ^:async t []
     (await (for-each
             (fn ^:async f [[args expected]]
               (let [p (owned {:offlineScale 0})
                     r (await (act p "offline" args))]
                 (is (= expected (.-ms r)) (str "args " args))))
             [[{} 300000] [{:ms 99999999} 600000]])))))

(deftest fake-offline-with-a-stale-token-rejects-with-cut
  (async-test
   (fn ^:async t []
     (let [p (fake/create {})
           e (await (rejection #(.offline p "nope" #js {:ms 1})))]
       (is (= "cut" (some-> e .-code)))))))

(deftest a-cut-during-the-fake-wait-still-comes-back-online-and-resolves-cut
  (async-test
   (fn ^:async t []
     (let [p (owned {:offlineScale 0.001})
           pending (act p "offline" {:ms 20000})
           _ (await (tick))
           _ (.setOwner p "t2")
           r (got (await pending))]
       (is (= {:status "cut"} r))
       (is (= false (offline? p)))))))

(deftest blocks-and-block-at-report-a-crop-age-that-dig-clears
  (async-test
   (fn ^:async t []
     (let [p (owned {:blocks {"1,64,0" "wheat" "2,64,0" "dirt"} :ages {"1,64,0" 7}})
           ages (mapv :age (blocks-of p {:names ["wheat"]}))
           age (:age (block-at p 1 64 0))
           dirt-has (contains? (block-at p 2 64 0) :age)
           _ (await (act p "dig" {:pos (at 1 64 0)}))
           after-has (contains? (block-at p 1 64 0) :age)]
       (is (= [7] ages))
       (is (= 7 age))
       (is (= false dirt-has))
       (is (= false after-has))))))

(deftest a-killed-animal-drops-its-listed-items
  (async-test
   (fn ^:async t []
     (let [p (owned {:entities [{:id 5 :name "cow" :kind "passive" :pos (at 1 64 0) :health 5
                                 :drops [{:name "beef" :count 2}]}]})
           r (await (act p "attack" {:id 5}))]
       (is (= "killed" (.-status r)))
       (is (= [["beef" 2]] (mapv (juxt #(get-in % [:item :name]) #(get-in % [:item :count])) (item-ents p))))))))

(deftest move-to-can-be-told-to-fail-with-no-path-blocked-reason-no-path-the-body-stays
  (async-test
   (fn ^:async t []
     (let [p (owned {:noPath ["9,64,9"]})
           r (await (act p "moveTo" {:pos (at 9 64 9)}))]
       (is (= ["blocked" "noPath" (at 0 64 0)] [(.-status r) (.-reason r) (got (.-pos r))]))))))

(def sea {:self {:pos (at 0 60 0) :oxygen 3 :inWater true}
          :blocks {"0,60,0" "water" "0,61,0" "water" "0,62,0" "water" "0,63,0" "water"}})

(deftest swim-surfaces-the-body-rises-to-the-top-water-cell-and-oxygen-refills
  (async-test
   (fn ^:async t []
     (let [p (owned sea)
           r (got (await (act p "swim" {:ms 3000})))
           s (self-of p)]
       (is (= {:status "surfaced" :oxygen {:before 3 :after 20}} r))
       (is (= (at 0 63 0) (:pos s)))
       (is (= 20 (:oxygen s)))))))

(deftest swim-on-dry-land-reports-surfaced-and-changes-nothing
  (async-test
   (fn ^:async t []
     (let [p (owned {:self {:oxygen 20}})
           r (got (await (.swim p "t1")))]
       (is (= {:status "surfaced" :oxygen {:before 20 :after 20}} r))
       (is (= (at 0 64 0) (:pos (self-of p))))))))

(deftest swim-can-be-told-to-fail-timeout-nothing-moves
  (async-test
   (fn ^:async t []
     (let [p (owned (assoc sea :swimFails true))
           r (got (await (.swim p "t1")))]
       (is (= {:status "timeout" :oxygen {:before 3 :after 3}} r))
       (is (= (at 0 60 0) (:pos (self-of p))))))))

(deftest swim-with-a-stale-token-rejects-with-cut
  (async-test
   (fn ^:async t []
     (let [p (owned sea)
           e (await (rejection #(.swim p "other")))]
       (is (= "cut" (some-> e .-code)))))))

(deftest place-with-a-water-bucket-pours-water-and-swaps-in-an-empty-bucket-bucket-scoops-it-back
  (async-test
   (fn ^:async t []
     (let [p (owned {:blocks {"1,63,0" "dirt"} :inventory [{:name "water_bucket" :count 1}]})
           r1 (await (act p "place" {:pos (at 1 64 0) :item "water_bucket"}))
           water (:name (block-at p 1 64 0))
           inv1 (mapv :name (:inventory (self-of p)))
           r2 (await (act p "place" {:pos (at 1 64 0) :item "bucket"}))
           air (:name (block-at p 1 64 0))
           inv2 (mapv :name (:inventory (self-of p)))
           r3 (await (act p "place" {:pos (at 1 64 0) :item "bucket"}))]
       (is (= "placed" (.-status r1)))
       (is (= "water" water))
       (is (= ["bucket"] inv1))
       (is (= "placed" (.-status r2)))
       (is (= "air" air))
       (is (= ["water_bucket"] inv2))
       (is (= "missing" (.-status r3)))))))

(def fake-wall (into {} (for [x [2 3] y [64 65 66]] [(str x "," y ",0") "stone"])))
(def fake-zombie {:id 9 :name "zombie" :kind "hostile" :pos (at 5 64 0)})

(deftest entities-marks-a-hostile-behind-a-wall-not-visible-and-visible-once-the-wall-is-gone
  (let [walled (fake/create {:blocks fake-wall :entities [fake-zombie]})
        before (:visible (first (ents walled {:kind "hostile"})))
        _ (run! #(fake/remove-block! walled (xyz %)) (keys fake-wall))
        after (:visible (first (ents walled {:kind "hostile"})))]
    (is (= false before))
    (is (= true after))))

(deftest only-hostiles-carry-visible-and-a-spec-can-force-it
  (let [p (fake/create {:blocks fake-wall
                        :entities [{:id 1 :name "cow" :kind "passive" :pos (at 5 64 0)}
                                   (assoc fake-zombie :visible true)]})]
    (is (= false (contains? (first (ents p {:kind "passive"})) :visible)))
    (is (= true (:visible (first (ents p {:kind "hostile"})))))))

(def fake-cow {:id 1 :name "cow" :kind "passive" :pos (at 5 64 0)})

(defn hittable-of [blocks]
  (:hittable (first (ents (fake/create {:blocks blocks :entities [fake-cow]}) {}))))

(deftest entities-hittable
  (are [label blocks expected] (= expected (hittable-of blocks))
    "open air" {} true
    "a glass wall" {"2,64,0" "glass" "2,65,0" "glass"} false
    "a stone wall" fake-wall false
    "water between" {"2,64,0" "water" "2,65,0" "water"} true
    "tall grass between" {"2,64,0" "tall_grass"} true
    "a fence post the ray passes over" {"2,64,0" "oak_fence"} true
    "a glass pane the ray goes through" {"2,64,0" "glass_pane" "2,65,0" "glass_pane"} false
    "an unknown-to-the-map block name" {"2,64,0" "weird_thing" "2,65,0" "weird_thing"} false))

(deftest hittable-is-on-every-non-item-entity-within-6-blocks-absent-beyond-and-on-items-and-a-spec-can-force-it
  (let [p (fake/create {:blocks fake-wall
                        :entities [(assoc fake-cow :hittable true)
                                   {:id 2 :name "zombie" :kind "hostile" :pos (at 5 64 1) :hittable false}
                                   {:id 3 :name "cow" :kind "passive" :pos (at 8 64 0)}
                                   {:id 4 :name "dropped" :kind "item" :pos (at 3 64 0)}]})
        by (into {} (map (juxt :id identity)) (ents p {}))]
    (is (= true (:hittable (by 1))))
    (is (= false (:hittable (by 2))))
    (is (= false (contains? (by 3) :hittable)))
    (is (= false (contains? (by 4) :hittable)))))

(deftest wait-returns-ok-at-once-and-a-held-wait-is-cut-by-an-owner-change
  (async-test
   (fn ^:async t []
     (let [p (owned)
           r (got (await (act p "wait" {:ms 2000})))
           _ (.hold (world p) "wait")
           call (act p "wait" {:ms 2000})
           _ (.setOwner p "t2")
           e (await (rejection (fn [] call)))]
       (is (= {:status "ok"} r))
       (is (= "cut" (some-> e .-code)))))))

(deftest while-offline-the-fake-reports-it-and-sensing-says-offline-instead-of-stale-values
  (async-test
   (fn ^:async t []
     (let [p (owned {:offlineScale 0.001
                     :entities [{:id 1 :name "zombie" :kind "hostile" :pos (at 2 64 0)}]
                     :blocks {"1,64,0" "stone"}})
           before (.isOffline p)
           pending (act p "offline" {:ms 20000})
           _ (await (tick))
           during {:offline (.isOffline p)
                   :self (self-of p)
                   :entities (ents p {})
                   :blocks (blocks-of p {})
                   :block-at (.blockAt p (tu/pos 1 64 0))}
           _ (await pending)
           after {:offline (.isOffline p)
                  :username (:username (self-of p))
                  :entities (count (ents p {}))}]
       (is (= false before))
       (is (= {:offline true :self {:status "offline"} :entities [] :blocks [] :block-at nil} during))
       (is (= {:offline false :username "Fake" :entities 1} after))))))

(deftest a-cut-ends-the-fake-wait-early-and-the-body-is-back-before-the-offline-call-resolves
  (async-test
   (fn ^:async t []
     (let [p (owned {:offlineScale 1})
           seen (body-events p)
           pending (act p "offline" {:ms 600000})
           _ (await (tick))
           _ (.setOwner p "t2")
           offline-now (.isOffline p)
           r (got (await pending))]
       (is (= true offline-now))
       (is (= {:status "cut"} r))
       (is (= false (.isOffline p)))
       (is (= ["offline" "online"] (mapv :kind @seen)))))))

(deftest fake-place-treats-a-free-cell-as-free-and-replaces-it-for-a-block-and-for-a-water-bucket
  (async-test
   (fn ^:async t []
     (await (for-each
             (fn ^:async f [name]
               (let [p (owned {:inventory [{:name "dirt" :count 1} {:name "water_bucket" :count 1}]
                               :blocks {"1,64,0" name "2,64,0" name}})
                     r1 (await (act p "place" {:pos (at 1 64 0) :item "dirt"}))
                     b1 (:name (block-at p 1 64 0))
                     r2 (await (act p "place" {:pos (at 2 64 0) :item "water_bucket"}))
                     b2 (:name (block-at p 2 64 0))]
                 (is (= "placed" (.-status r1)) name)
                 (is (= "dirt" b1) name)
                 (is (= "placed" (.-status r2)) name)
                 (is (= "water" b2) name)))
             ["fire" "soul_fire" "short_grass" "tall_grass" "grass" "snow"])))))

(def pool
  "Water at y 62 and 63 for x -1..2, z -1..1 over a stone floor at y 61; the body at the surface of column 0."
  {:self {:pos (at 0 63 0) :oxygen 20 :inWater true :onGround false}
   :blocks (into {} (for [x (range -1 3) z (range -1 2) [y n] [[61 "stone"] [62 "water"] [63 "water"]]]
                      [(str x "," y "," z) n]))})

(defn ^:async swim-toward [world target]
  (let [p (owned world)
        r (await (act p "swim" {:ms 3000 :toward target}))]
    [(.-status r) (self-of p)]))

(defn ledge
  "Stone in column x 3, z 0, from y 61 up to top."
  [top]
  (into {} (for [y (range 61 (inc top))] [(str "3," y ",0") "stone"])))

(deftest swim-toward-lands-on-a-rim-at-most-one-block-above-the-water
  (async-test
   (fn ^:async t []
     (doseq [[label top] [["flush with the water" 62] ["one above the water" 63] ["rim one block over the surface" 64]]]
       (let [[status s] (await (swim-toward (update pool :blocks merge (ledge top)) (at 3 (inc top) 0)))]
         (is (= "landed" status) label)
         (is (= (at 3 (inc top) 0) (:pos s)) label)
         (is (= [false true] [(:inWater s) (:onGround s)]) label))))))

(deftest swim-toward-a-rim-too-high-or-land-behind-a-wall-times-out-on-a-crest
  (async-test
   (fn ^:async t []
     (doseq [[label blocks target]
             [["rim two blocks over the surface" (ledge 65) (at 3 66 0)]
              ["land behind a wall two above the water"
               (merge (ledge 63) {"2,62,0" "stone" "2,63,0" "stone" "2,64,0" "stone" "2,65,0" "stone"})
               (at 3 64 0)]
              ["a pillar two over the surface in the way" (merge (ledge 63) {"1,63,0" "stone" "1,64,0" "stone" "1,65,0" "stone"}) (at 3 64 0)]
              ["the target is not land" {} (at 2 63 0)]
              ["too far" {"9,63,0" "stone"} (at 9 64 0)]]]
       (let [[status s] (await (swim-toward (update pool :blocks merge blocks) target))]
         (is (= "timeout" status) label)
         (is (= (at 0 63 0) (:pos s)) (str label ": still at the surface"))
         (is (= [false false] [(:inWater s) (:onGround s)]) (str label ": on a jump crest")))))))

(deftest swim-surfacing-is-back-in-the-water
  (async-test
   (fn ^:async t []
     (let [p (owned (update pool :self assoc :inWater false :onGround false))
           r (await (act p "swim" {:ms 3000}))]
       (is (= "surfaced" (.-status r)))
       (is (true? (:inWater (self-of p))))))))

(deftest fake-jump-place-raises-the-body-one-block-per-placement-and-consumes-the-items
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "dirt" :count 3}] :blocks {"0,63,0" "stone"}})
           r (got (await (act p "jumpPlace" {:item "dirt" :count 2})))
           pos (:pos (self-of p))
           b1 (:name (block-at p 0 64 0))
           b2 (:name (block-at p 0 65 0))
           left (:count (first (filter #(= "dirt" (:name %)) (:inventory (self-of p)))))]
       (is (= {:status "done" :placed 2} r))
       (is (= (at 0 66 0) pos))
       (is (= "dirt" b1))
       (is (= "dirt" b2))
       (is (= 1 left))))))

(deftest fake-jump-place-stops-early-with-a-reason-no-item-no-headroom-nothing-solid-below
  (async-test
   (fn ^:async t []
     (let [none (owned {:blocks {"0,63,0" "stone"}})
           low (owned {:inventory [{:name "dirt" :count 3}] :blocks {"0,63,0" "stone" "0,66,0" "stone"}})
           air (owned {:inventory [{:name "dirt" :count 3}]})
           short (owned {:inventory [{:name "dirt" :count 1}] :blocks {"0,63,0" "stone"}})
           r-none (got (await (act none "jumpPlace" {:item "dirt"})))
           r-low (got (await (act low "jumpPlace" {:item "dirt" :count 3})))
           r-air (got (await (act air "jumpPlace" {:item "dirt"})))
           r-short (got (await (act short "jumpPlace" {:item "dirt" :count 3})))]
       (is (= {:status "failed" :placed 0 :reason "no-item"} r-none))
       (is (= {:status "failed" :placed 0 :reason "no-headroom"} r-low))
       (is (= {:status "failed" :placed 0 :reason "no-support"} r-air))
       (is (= {:status "partial" :placed 1 :reason "no-item"} r-short))))))

(deftest fake-jump-place-stops-under-a-ceiling-that-appears-as-the-body-rises
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "dirt" :count 5}] :blocks {"0,63,0" "stone" "0,67,0" "stone"}})
           r (got (await (act p "jumpPlace" {:item "dirt" :count 5})))]
       (is (= {:status "partial" :placed 1 :reason "no-headroom"} r))
       (is (= (at 0 65 0) (:pos (self-of p))))))))

(deftest fake-toss-takes-the-items-from-every-stack-and-drops-one-item-entity-three-blocks-along-x
  (async-test
   (fn ^:async t []
     (let [p (owned {:self {:pos (at 1 64 2)}
                     :inventory [{:name "dirt" :count 40} {:name "bread" :count 2} {:name "dirt" :count 30}]})
           r (got (await (act p "toss" {:item "dirt"})))
           names (mapv :name (:inventory (self-of p)))
           drop (first (item-ents p))]
       (is (= {:status "tossed" :count 70} r))
       (is (= ["bread"] names))
       (is (= ["item" {:name "dirt" :count 70} (at 4 64 2)] [(:name drop) (:item drop) (:pos drop)]))
       (is (= [["toss" {:item "dirt"}]] (mapv (juxt #(.-name %) #(got (.-args %))) (calls p))))))))

(deftest fake-toss-honours-a-smaller-count-clamps-a-larger-one-and-reports-no-item-for-nothing-carried
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "dirt" :count 5}]})
           a (got (await (act p "toss" {:item "dirt" :count 2})))
           b (got (await (act p "toss" {:item "dirt" :count 9})))
           c (got (await (act p "toss" {:item "dirt"})))]
       (is (= {:status "tossed" :count 2} a))
       (is (= {:status "tossed" :count 3} b))
       (is (= {:status "no-item" :count 0} c))
       (is (= 2 (count (item-ents p))))))))

(deftest fake-collect-emits-one-picked-up-event-per-gained-item
  (async-test
   (fn ^:async t []
     (let [p (owned {:entities [{:id 5 :name "item" :kind "item" :pos (at 1 64 0) :item {:name "oak_log" :count 3}}]})
           seen (body-events p)
           _ (await (act p "collect" {:id 5}))]
       (is (= [{:kind "picked-up" :item "oak_log" :count 3}] @seen))))))

(deftest fake-toss-with-a-slot-throws-that-whole-stack-only-no-item-for-an-empty-slot-or-another-item
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "dirt" :count 40} {:name "bread" :count 2} {:name "dirt" :count 6}]})
           a (got (await (act p "toss" {:item "dirt" :slot 2})))
           inv (inv-pairs p)
           dropped (:item (first (item-ents p)))
           b (got (await (act p "toss" {:item "dirt" :slot 1})))
           c (got (await (act p "toss" {:item "dirt" :slot 9})))]
       (is (= {:status "tossed" :count 6} a))
       (is (= [["dirt" 40] ["bread" 2]] inv))
       (is (= {:name "dirt" :count 6} dropped))
       (is (= {:status "no-item" :count 0} b))
       (is (= {:status "no-item" :count 0} c))))))

(deftest the-fake-is-never-settling-unless-told
  (let [p (fake/create {})]
    (is (= [false false] [(.isSettling p) (:settling (self-of p))]))))

(deftest world-settle-sets-the-settling-flag-that-is-settling-and-self-report
  (let [p (fake/create {})]
    (.settle (world p) true)
    (is (= [true true] [(.isSettling p) (:settling (self-of p))]))
    (.settle (world p) false)
    (is (= [false false] [(.isSettling p) (:settling (self-of p))]))))

(deftest with-settles-the-body-is-settling-after-the-offline-return
  (async-test
   (fn ^:async t []
     (await (for-each
             (fn ^:async f [[settles expected]]
               (let [p (owned {:settles settles :offlineScale 0.001})
                     pending (act p "offline" {:ms 20000})
                     _ (await (tick))
                     during (.isSettling p)
                     _ (await pending)
                     after (.isSettling p)]
                 (is (= false during) "offline is its own state")
                 (is (= expected after) (str "settles " settles))))
             [[true true] [false false]])))))

(deftest with-settles-world-respawn-emits-respawned-and-settling
  (are [settles expected]
       (let [p (fake/create {:settles settles :self {:pos (at 3 64 4) :dimension "overworld"}})
             seen (body-events p)]
         (.respawn (world p))
         (and (= [{:kind "respawned" :pos (at 3 64 4) :dimension "overworld"}] @seen)
              (= expected (.isSettling p))))
    true true
    false false))

(deftest drive-records-controls-and-look-a-stale-token-throws-cut
  (let [p (owned)
        _ (.drive p "t1" (clj->js {:controls {:forward true :jump true}}))
        _ (.drive p "t1" (clj->js {:controls {:jump false} :look {:yaw 90 :pitch 10}}))
        controls (:controls @(fake/state p))
        look [(:yaw @(fake/state p)) (:pitch @(fake/state p))]
        _ (.drive p "t1" (clj->js {:look {:dyaw 300}}))
        yaw (:yaw @(fake/state p))
        e (thrown #(.drive p "old" (clj->js {:controls {:back true}})))]
    (is (= {:forward true :jump false} controls))
    (is (= [90 10] look))
    (is (= 30 yaw))
    (is (= "cut" (some-> e .-code)))
    (is (nil? (:back (:controls @(fake/state p)))))))

(deftest fake-drive-clears-the-controls
  (are [label clear]
       (let [p (owned)]
         (.drive p "t1" (clj->js {:controls {:forward true}}))
         (clear p)
         (= {} (:controls @(fake/state p))))
    "stopDriving" (fn [p] (.stopDriving p))
    "an owner change" (fn [p] (.setOwner p "t2"))
    "a null owner" (fn [p] (.setOwner p nil))))

(deftest fakes-do-not-share-nested-default-self-objects
  (let [first-p (fake/create {})
        _ (fake/swap-self! first-p #(-> %
                                        (assoc-in [:pos 0] 99)
                                        (assoc-in [:experience :level] 7)
                                        (update :effects (fnil conj []) "speed")))
        second-p (fake/create {})
        s (fake/self second-p)]
    (is (= 0 (get-in s [:pos 0])))
    (is (= 0 (get-in s [:experience :level])))
    (is (= [] (vec (:effects s))))))

(deftest place-of-a-seed-on-farmland-plants-the-crop-at-age-0-and-consumes-the-seed
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "wheat_seeds" :count 2}] :blocks {"1,63,0" "farmland"}})
           r (got (await (act p "place" {:pos (at 1 64 0) :item "wheat_seeds"})))
           b (block-at p 1 64 0)
           age (:age (first (blocks-of p {:names ["wheat"]})))
           left (:count (first (:inventory (self-of p))))]
       (is (= {:status "placed" :block "wheat_seeds"} r))
       (is (= {:name "wheat" :pos (at 1 64 0) :age 0 :properties {:age 0}} b))
       (is (= 0 age))
       (is (= 1 left))))))

(deftest place-of-a-seed-over-anything-but-farmland-fails-and-consumes-nothing
  (async-test
   (fn ^:async t []
     (let [p (owned {:inventory [{:name "wheat_seeds" :count 2}] :blocks {"1,63,0" "stone"}})
           r (await (act p "place" {:pos (at 1 64 0) :item "wheat_seeds"}))
           b (:name (block-at p 1 64 0))
           left (:count (first (:inventory (self-of p))))]
       (is (= "failed" (.-status r)))
       (is (re-find #"still air" (.-reason r)))
       (is (= "air" b))
       (is (= 2 left))))))

(deftest place-of-an-item-plants-its-crop
  (async-test
   (fn ^:async t []
     (await (for-each
             (fn ^:async f [[item crop]]
               (let [p (owned {:inventory [{:name item :count 1}] :blocks {"1,63,0" "farmland"}})
                     r (got (await (act p "place" {:pos (at 1 64 0) :item item})))
                     b (block-at p 1 64 0)]
                 (is (= {:status "placed" :block item} r) item)
                 (is (= crop (:name b)) item)
                 (is (= 0 (:age b)) item)
                 (is (= 0 (count (:inventory (self-of p)))) item)))
             [["wheat_seeds" "wheat"] ["carrot" "carrots"] ["potato" "potatoes"] ["beetroot_seeds" "beetroots"]])))))

(deftest a-drops-array-spawns-one-item-entity-per-name
  (async-test
   (fn ^:async t []
     (let [p (owned {:blocks {"1,64,0" "wheat"} :ages {"1,64,0" 7} :drops {"wheat" ["wheat" "wheat_seeds"]}})
           r (got (await (act p "dig" {:pos (at 1 64 0)})))]
       (is (= [["wheat" 1] ["wheat_seeds" 1]] (mapv (juxt :name :count) (:drops r))))
       (is (= ["wheat" "wheat_seeds"] (mapv #(get-in % [:item :name]) (item-ents p))))))))

(deftest a-successful-sleep-puts-the-body-in-bed-an-acting-call-then-leaves-it-first
  (async-test
   (fn ^:async t []
     (let [p (owned {:time 14000 :skipNight false :blocks {"1,64,0" "red_bed"}})
           before (:isSleeping (self-of p))
           r (await (act p "sleep" {:pos (at 1 64 0)}))
           during (:isSleeping (self-of p))
           _ (await (act p "look" {:yaw 0 :pitch 0}))
           after (:isSleeping (self-of p))]
       (is (= false before))
       (is (= "sleeping" (.-status r)))
       (is (= true during))
       (is (= false after))))))

(deftest by-default-a-sleep-skips-the-night-morning-and-the-body-awake
  (async-test
   (fn ^:async t []
     (let [p (owned {:time 14000 :blocks {"1,64,0" "red_bed"}})
           r (await (act p "sleep" {:pos (at 1 64 0)}))
           s (self-of p)]
       (is (= "sleeping" (.-status r)))
       (is (= false (:isSleeping s)))
       (is (= 0 (:timeOfDay s)))))))

(def crafting {:self {:held nil} :inventory [{:name "oak_log" :count 2}]})
(def table {"1,64,0" "crafting_table"})
(def wheat3 [{:name "wheat" :count 3}])

(def craft-cases
  [["unknown item" crafting {:item "nothing"} {:status "cannot" :reason "no-recipe"}]
   ["planks" crafting {:item "oak_planks"} {:status "crafted" :item "oak_planks" :made 4 :used {:oak_log 1}}]
   ["rounds up to whole batches" crafting {:item "oak_planks" :count 5}
    {:status "crafted" :item "oak_planks" :made 8 :used {:oak_log 2}}]
   ["runs out midway" crafting {:item "oak_planks" :count 12}
    {:status "partial" :item "oak_planks" :made 8 :used {:oak_log 2} :reason "no-item"
     :recipes [{:oak_log 1}] :have {:oak_planks 8}}]
   ["missing ingredients" crafting {:item "stick"}
    {:status "no-item" :recipes [{:oak_planks 2}] :have {:oak_log 2}}]
   ["no table near" {:inventory wheat3} {:item "bread"} {:status "unreachable" :reason "no-table"}]
   ["table given but not a table" {:inventory wheat3 :blocks table} {:item "bread" :table (at 2 64 0)}
    {:status "unreachable" :reason "not-a-table"}]
   ["table given but far" {:inventory wheat3 :blocks {"9,64,0" "crafting_table"}} {:item "bread" :table (at 9 64 0)}
    {:status "out-of-reach" :reason "too-far" :table (at 9 64 0)}]
   ["table not handed over is never found" {:inventory wheat3 :blocks table} {:item "bread"}
    {:status "unreachable" :reason "no-table"}]
   ["table near" {:inventory wheat3 :blocks table} {:item "bread" :table (at 1 64 0)}
    {:status "crafted" :item "bread" :made 1 :used {:wheat 3}}]
   ["spec recipes merge" {:inventory [{:name "dirt" :count 1}] :recipes {"gravel" {:count 2 :needs {"dirt" 1}}}}
    {:item "gravel"} {:status "crafted" :item "gravel" :made 2 :used {:dirt 1}}]
   ["full" {:inventory (into [{:name "oak_log" :count 1}] (map (fn [i] {:name (str "junk" i) :count 1})) (range 35))}
    {:item "oak_planks"} {:status "full" :made 0 :used {}}]])

(deftest fake-craft-cases
  (async-test
   (fn ^:async t []
     (await (for-each
             (fn ^:async f [[label spec args want]]
               (let [r (got (await (act (owned spec) "craft" args)))]
                 (is (= want r) label)))
             craft-cases)))))

(deftest fake-craft-consumes-and-adds-to-the-inventory
  (async-test
   (fn ^:async t []
     (let [p (owned crafting)
           _ (await (act p "craft" {:item "oak_planks"}))]
       (is (= [["oak_log" 1] ["oak_planks" 4]] (inv-pairs p)))))))

(deftest fake-craft-is-recorded-and-can-be-overridden
  (async-test
   (fn ^:async t []
     (let [p (owned)
           _ (.override (world p) "craft" (fn ^:async o [] #js {:status "failed" :reason "x"}))
           r (got (await (act p "craft" {:item "stick"})))
           recorded (.-name (last (calls p)))
           e (await (rejection #(.craft p "old" #js {:item "stick"})))]
       (is (= {:status "failed" :reason "x"} r))
       (is (= "craft" recorded))
       (is (= "cut" (some-> e .-code)))))))

(deftest fake-chat-records-the-line-and-resolves-sent
  (async-test
   (fn ^:async t []
     (let [p (owned {:entities [{:id 1 :name "Steve" :kind "player" :pos (at 1 64 0)}]})
           a (got (await (act p "chat" {:message "hi"})))
           b (got (await (act p "chat" {:message "psst" :to "Steve"})))]
       (is (= {:status "sent" :parts 1} a))
       (is (= {:status "sent" :parts 1 :to "Steve"} b))
       (is (= [["hi" nil] ["psst" "Steve"]] (mapv (juxt :message :to) (:chat @(fake/state p)))))))))

(deftest fake-chat-to-a-player-who-is-not-there-is-gone-and-records-nothing
  (async-test
   (fn ^:async t []
     (let [p (owned)
           r (got (await (act p "chat" {:message "psst" :to "Nobody"})))]
       (is (= {:status "gone" :to "Nobody"} r))
       (is (empty? (:chat @(fake/state p))))))))

(deftest fake-chat-applies-the-sink-assertion-and-keeps-prototype-names-gone
  (async-test
   (fn ^:async t []
     (let [p (owned {:entities [{:id 1 :name "Steve" :kind "player" :pos (at 1 64 0)}]})
           errors (await (for-each
                          (fn ^:async f [m]
                            (let [e (await (rejection #(.chat p "t1" m)))]
                              (is (some? e) "rejected")
                              (is (re-find #"refusing" (str (some-> e .-message))))))
                          [#js {:message "/op me"} #js {:message "a\nb"} #js {}]))
           r (got (await (act p "chat" {:message "hi" :to "constructor"})))]
       (is (= {:status "gone" :to "constructor"} r))
       (is (empty? (:chat @(fake/state p))))))))

(deftest block-at-marks-full-cubes-only
  (let [p (fake/create {:blocks {"1,64,0" "stone" "2,64,0" "farmland" "3,64,0" "wheat" "4,64,0" "oak_slab"}})]
    (is (= true (:fullCube (block-at p 1 64 0))))
    (are [x] (= false (contains? (block-at p x 64 0) :fullCube))
      2 3 4 9)))

(deftest self-equipment-is-empty-slots-by-default-and-shows-the-spec-equipment-the-held-item-as-main-hand
  (let [bare (fake/create {})
        dressed (fake/create {:self {:held "iron_sword"}
                              :equipment {:head {:name "iron_helmet" :durability 150}
                                          :offHand {:name "shield" :count 1}}})]
    (is (= {:head nil :torso nil :legs nil :feet nil :offHand nil :mainHand nil}
           (:equipment (self-of bare))))
    (is (= {:head {:name "iron_helmet" :count 1 :durability 150}
            :torso nil
            :legs nil
            :feet nil
            :offHand {:name "shield" :count 1}
            :mainHand {:name "iron_sword" :count 1}}
           (:equipment (self-of dressed))))
    (is (= [] (:inventory (self-of dressed))) "worn items are not carried items")))

;; ---- place with a click: the state comes from face, cursor and look (engine.fake.placing) ----

(defn builder [blocks inventory]
  (owned {:blocks blocks :inventory (mapv (fn [name] {:name name :count 2}) inventory)}))

(deftest place-with-a-click-sets-the-state-the-game-would-and-reports-it
  (async-test
   (fn ^:async t []
     (let [p (builder {"1,64,0" "stone"} ["oak_stairs"])
           click {:against (at 1 64 0) :cursor {:x 0 :y 0.75 :z 0.5} :yaw (* 1.5 js/Math.PI) :pitch 0}
           r (got (await (act p "place" {:pos (at 0 64 0) :item "oak_stairs" :click click})))
           b (block-at p 0 64 0)]
       (is (= "placed" (:status r)))
       (is (= "oak_stairs" (:block r)))
       (is (= ["oak_stairs" "east" "top"]
              [(get-in r [:placed :name]) (get-in r [:placed :properties :facing]) (get-in r [:placed :properties :half])]))
       (is (= ["east" "top"] [(get-in b [:properties :facing]) (get-in b [:properties :half])]))))))

(deftest place-with-a-click-on-air-has-no-support-and-keeps-the-item
  (async-test
   (fn ^:async t []
     (let [p (builder {} ["oak_log"])
           r (await (act p "place" {:pos (at 0 64 0) :item "oak_log"
                                    :click {:against (at 0 63 0) :cursor {:x 0.5 :y 1 :z 0.5}}}))
           left (:count (first (:inventory (self-of p))))]
       (is (= "no-support" (.-status r)))
       (is (= 2 left))))))

(deftest a-door-makes-its-upper-half-and-a-bed-its-head-a-bed-with-no-room-is-refused-the-item-kept
  (async-test
   (fn ^:async t []
     (let [p (builder {"0,63,0" "stone" "2,63,0" "stone" "2,64,1" "stone"} ["oak_door" "white_bed"])
           down {:x 0.5 :y 1 :z 0.5}
           _ (await (act p "place" {:pos (at 0 64 0) :item "oak_door"
                                    :click {:against (at 0 63 0) :cursor down :yaw 0 :pitch 0}}))
           door (block-at p 0 65 0)
           bed (await (act p "place" {:pos (at 2 64 0) :item "white_bed"
                                      :click {:against (at 2 63 0) :cursor down :yaw js/Math.PI :pitch 0}}))
           bed-cell (:name (block-at p 2 64 0))
           beds-left (:count (first (filter #(= "white_bed" (:name %)) (:inventory (self-of p)))))
           _ (await (act p "place" {:pos (at 2 64 0) :item "white_bed"
                                    :click {:against (at 2 63 0) :cursor down :yaw (* 1.5 js/Math.PI) :pitch 0}}))
           head (block-at p 3 64 0)]
       (is (= ["oak_door" "upper"] [(:name door) (get-in door [:properties :half])]))
       (is (= "failed" (.-status bed)))
       (is (= "air" bed-cell))
       (is (= 2 beds-left))
       (is (= ["white_bed" "head"] [(:name head) (get-in head [:properties :part])]))))))

(deftest a-plain-place-clicks-the-block-below-first-looking-at-it-from-the-eye-and-reports-the-state
  (async-test
   (fn ^:async t []
     (let [p (owned {:self {:pos (at 0 64 3)}
                     :blocks {"0,63,0" "stone"}
                     :inventory [{:name "oak_log" :count 1} {:name "oak_stairs" :count 1}]})
           log (got (.-placed (await (act p "place" {:pos (at 0 64 0) :item "oak_log"}))))
           stairs (got (await (act p "place" {:pos (at 0 65 0) :item "oak_stairs"})))]
       (is (= {:name "oak_log" :properties {:axis "y"}} log))
       (is (= ["north" "bottom"]
              [(get-in stairs [:placed :properties :facing]) (get-in stairs [:placed :properties :half])]))))))

;; pathWorld reuses its snapshot while no block changed; every write must show in the next call
(defn path-state [p x y z] (.stateAt ^js (.-snapshot (.pathWorld p)) x y z))

(deftest path-world-sees-a-dug-and-a-placed-block-in-the-next-call
  (async-test
   (fn ^:async t []
     (let [p (owned {:blocks {"1,64,0" "stone" "5,64,0" "stone"} :inventory [{:name "stone" :count 1}]})
           stone (path-state p 1 64 0)
           same (identical? (.-snapshot (.pathWorld p)) (.-snapshot (.pathWorld p)))
           _ (await (act p "dig" {:pos (at 1 64 0)}))
           dug (path-state p 1 64 0)
           _ (await (act p "place" {:pos (at 2 64 0) :item "stone"}))
           placed (path-state p 2 64 0)]
       (is (pos? stone))
       (is same)
       (is (zero? dug))
       (is (= stone placed))))))
