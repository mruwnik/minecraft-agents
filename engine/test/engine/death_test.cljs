(ns engine.death-test
  "Recovering drops after a death: the value and cost functions, the died
  trigger and the recover-drops job against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.library-test :refer [setup run-until-empty calls inv]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.triggers.died :as died]
            [engine.value :as value]))

;; ---------------------------------------------------------------- the value

(def junk [{:name "wheat_seeds" :count 64} {:name "dirt" :count 64} {:name "cobblestone" :count 64}
           {:name "oak_sapling" :count 3} {:name "stick" :count 12} {:name "dandelion" :count 2}])

(def iron-kit [{:name "iron_pickaxe" :count 1} {:name "iron_chestplate" :count 1}
               {:name "bow" :count 1} {:name "cooked_beef" :count 20}])

(def netherite-kit [{:name "netherite_chestplate" :count 1}
                    {:name "diamond_sword" :count 1 :enchants [{:name "sharpness" :lvl 5}]}
                    {:name "stone_pickaxe" :count 1 :nbt {:Damage 0}}])

(deftest inventory-value-tiers
  (are [inventory level expected] (= expected (value/inventory-value inventory level))
    [] 0 0
    junk 0 0
    iron-kit 0 16
    netherite-kit 0 75
    netherite-kit 10 80
    junk 4 2
    [{:name "stone_pickaxe" :count 1 :enchants []}] 0 1))

(deftest inventory-value-counts-metals-per-item
  (are [n expected] (= expected (value/inventory-value [{:name "iron_ingot" :count n}] 0))
    1 2
    3 6
    8 16
    32 64))

(deftest inventory-value-adds-stacks-of-one-name
  (is (= (value/inventory-value [{:name "raw_iron" :count 24}] 0)
         (value/inventory-value [{:name "raw_iron" :count 4} {:name "raw_iron" :count 20}] 0)
         48)))

(def death-pile-of-the-field-report
  [{:name "raw_iron" :count 4 :slot 17} {:name "raw_iron" :count 20 :slot 18}
   {:name "cobblestone" :count 64} {:name "cobblestone" :count 36}
   {:name "stone_pickaxe" :count 1} {:name "oak_planks" :count 29}])

(deftest a-pile-of-raw-iron-outweighs-two-hostiles-and-a-walk
  (is (> (value/inventory-value death-pile-of-the-field-report 0)
         (value/retrieval-cost {:x 50 :y 40 :z 3} {:x 20 :y 66 :z 2}
                               [{:x 52 :y 40 :z 3} {:x 55 :y 40 :z 3}] "mob" 1500))))

(deftest inventory-value-treats-unknown-items-as-junk
  (is (= 0 (value/inventory-value [{:name "mystery_item" :count 1}] 0))))

;; ----------------------------------------------------------------- the cost

(def here {:x 0 :y 64 :z 0})

(defn cost [& {:keys [death now hostiles cause elapsed]
               :or {death here now here hostiles [] cause "fall" elapsed 0}}]
  (value/retrieval-cost death now hostiles cause elapsed))

(deftest retrieval-cost-grows-with-distance
  (is (= 0.0 (cost)))
  (is (= 10.0 (cost :now {:x 100 :y 64 :z 0})))
  (is (< (cost :now {:x 10 :y 64 :z 0}) (cost :now {:x 50 :y 64 :z 0}))))

(deftest retrieval-cost-counts-hostiles-near-the-death-point-only
  (let [near {:x 10 :y 64 :z 0}
        far {:x 40 :y 64 :z 0}]
    (is (= 10.0 (cost :hostiles [near])))
    (is (= 20.0 (cost :hostiles [near near])))
    (is (= 0.0 (cost :hostiles [far])))))

(deftest retrieval-cost-grows-as-the-window-closes
  (is (< (cost :elapsed 0) (cost :elapsed 60000) (cost :elapsed 240000) (cost :elapsed 299000))))

(deftest retrieval-cost-is-infinite-at-the-end-of-the-window
  (is (= js/Infinity (cost :elapsed 300000)))
  (is (= js/Infinity (cost :elapsed 400000))))

(deftest retrieval-cost-is-infinite-when-the-drops-are-unreachable-by-cause
  (are [cause] (= js/Infinity (cost :cause cause))
    "lava" "fire" "void" "Lava" :lava "in_fire" "out_of_world"))

(deftest retrieval-cost-is-infinite-for-an-unknown-cause-without-a-position
  (is (= js/Infinity (value/retrieval-cost nil here [] nil 0)))
  (is (= 0.0 (cost :cause nil)) "an unknown cause with a position is fine"))

;; --------------------------------------------------------------- the trigger

(defn memory-with
  "A memory view at now-ms with [kind t data] entries."
  [now-ms & entries]
  {:data (reduce (fn [d [kind t data]] (mem/add-entry d kind {:t t :data data} nil)) mem/empty-data entries)
   :now now-ms})

(defn died-holds [memory] ((:when died/died) nil memory {}))

(def died-at [:died 1000 {:pos {:x 20 :y 64 :z 0} :inventory []}])
(def respawned-at [:respawned 1100 {:pos {:x 0 :y 64 :z 0}}])

(deftest died-trigger-holds-after-a-death
  (is (true? (died-holds (memory-with 2000 died-at respawned-at)))))

(deftest died-trigger-waits-for-the-respawn
  (is (false? (died-holds (memory-with 2000 died-at))) "the body is still dead")
  (is (false? (died-holds (memory-with 2000 [:respawned 500 {}] died-at))) "a respawn older than the death does not count")
  (is (true? (died-holds (memory-with 2000 died-at [:respawned 1100 {}])))))

(deftest died-trigger-does-not-hold-without-a-death
  (is (false? (died-holds (memory-with 2000)))))

(deftest died-trigger-does-not-hold-after-recovered
  (is (false? (died-holds (memory-with 2000 died-at respawned-at [:recovered 1500 {:decision :skip}]))))
  (is (true? (died-holds (memory-with 2000 [:recovered 500 {:decision :skip}] died-at respawned-at)))
      "a recovery older than the death does not count"))

(deftest died-trigger-does-not-hold-after-five-minutes
  (is (true? (died-holds (memory-with (+ 1000 299000) died-at respawned-at))))
  (is (false? (died-holds (memory-with (+ 1000 300000) died-at respawned-at)))))

(deftest died-trigger-is-registered-and-runs-the-job
  (is (= died/died (:died triggers/all)))
  (is (= '(jobs.survival.recover-drops) (:job died/died))))

;; ---------------------------------------------------- body events to memory

(deftest died-and-respawned-body-events-land-in-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})
              view #(mem/view (:store eng))]
          (.emit (.-world p) #js {:kind "died" :pos #js {:x 5 :y 64 :z 7} :inventory #js [#js {:name "diamond" :count 2 :slot 0}]})
          (.emit (.-world p) #js {:kind "respawned" :pos #js {:x 0 :y 64 :z 0} :dimension "overworld"})
          (is (= {:pos {:x 5 :y 64 :z 7} :inventory [{:name "diamond" :count 2 :slot 0}]}
                 (:data (mem/latest (view) :died))))
          (is (= {:x 0 :y 64 :z 0} (:pos (:data (mem/latest (view) :respawned)))))
          (is (= 1000000 (:t (mem/latest (view) :died)))))))))

;; ------------------------------------------------------------------ the job

(def death-pos {:x 20 :y 64 :z 0})
(def diamonds [{:name "diamond_pickaxe" :count 1 :slot 0} {:name "diamond_sword" :count 1 :slot 1}])

(defn drop-item [id x name]
  {:id id :name "item" :kind "item" :pos {:x x :y 64 :z 0} :item {:name name :count 1}})

(def drops [(drop-item 1 20 "diamond_pickaxe") (drop-item 2 21 "diamond_sword")])

(defn die-unrespawned!
  "The body has died and no :respawned entry exists yet."
  [eng data] (mem/write! (:store eng) :died data))

(defn die!
  "A death that happened 5 s ago and a respawn 3 s ago: the body is alive again and settled."
  [eng data]
  (let [store (:store eng)
        now ((:now @store))
        entry (fn [t d] {:t t :wt ((:world-time @store)) :data d})]
    (swap! store update :data
           #(-> % (mem/add-entry :died (entry (- now 5000) data) nil)
                (mem/add-entry :respawned (entry (- now 3000) {:pos {:x 0 :y 64 :z 0}}) nil)))))

(defn recovered [eng] (:data (mem/latest (mem/view (:store eng)) :recovered)))

(def job '(jobs.survival.recover-drops))

(deftest recover-drops-skips-a-junk-inventory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory junk})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= [] (:list (core/state eng))))
          (is (= [] (tu/walk-calls p)))
          (is (= [] (calls p "collect")))
          (is (= :skip (:decision (recovered eng))))
          (is (= 0 (:value (recovered eng)))))))))

(deftest recover-drops-skips-when-the-cause-was-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds :cause "lava"})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= [] (tu/walk-calls p)))
          (is (= :skip (:decision (recovered eng))))
          (is (= :infinite (:cost (recovered eng)))))))))

(deftest recover-drops-goes-and-collects-valuable-drops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (is (< (await (run-until-empty eng 10)) 10))
          (is (= [] (:list (core/state eng))))
          (is (= [death-pos] (tu/walked-to eng)))
          (is (= {"diamond_pickaxe" 1 "diamond_sword" 1} (inv p)))
          (is (= {:decision :collected :items 2} (select-keys (recovered eng) [:decision :items])))
          (is (pos? (:value (recovered eng))))
          (is (false? (died-holds (mem/view (:store eng)))) "the trigger is cleared"))))))

(deftest recover-drops-abandons-when-the-death-point-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops :unreachable ["20,64,0"]})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (is (< (await (run-until-empty eng 10)) 10))
          (is (= [] (:list (core/state eng))))
          (is (= [] (calls p "collect")))
          (is (= {:decision :abandoned :reason :unreachable} (select-keys (recovered eng) [:decision :reason]))))))))

(deftest recover-drops-abandons-when-the-window-closes-mid-trip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds})
          (swap! clock + 300000)
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= [] (:list (core/state eng))))
          (is (= [] (calls p "collect")))
          (is (= :abandoned (:decision (recovered eng)))))))))

(deftest recover-drops-abandons-when-nothing-is-left-at-the-death-point
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:floor tu/walk-floor})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= {:decision :abandoned :items 0} (select-keys (recovered eng) [:decision :items]))))))))

(deftest recover-drops-yields-to-a-hostile-and-resumes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}
              {:keys [eng p]} (setup {:floor tu/walk-floor :entities (conj drops zombie)})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= [] (tu/walk-calls p)) "no acting while a hostile is within the danger radius")
          (is (= 1 (count (:list (core/state eng)))) "the job stays listed")
          (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "hostile" (:kind e))) %))
          (await (run-until-empty eng 10))
          (is (= :collected (:decision (recovered eng)))))))))

;; ------------------------------------------- keyed to the death, settling, reporting

(def second-death-pos {:x -30 :y 64 :z 0})
(def second-drops [(drop-item 3 -30 "diamond_pickaxe") (drop-item 4 -31 "diamond_sword")])

(defn decided-events [seen] (filter #(= :recover-drops.decided (:kind %)) @seen))

(defn job-memory [eng] (core/job-memory eng "j1"))

(deftest recover-drops-starts-afresh-for-a-second-death
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:floor tu/walk-floor :entities (into drops second-drops)})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= [death-pos] (tu/walked-to eng)))
          (swap! clock + 10)
          (die! eng {:pos second-death-pos :inventory diamonds})
          (await (run-until-empty eng 15))
          (is (= second-death-pos (last (tu/walked-to eng))))
          (is (= :collected (:decision (recovered eng))))
          (is (= 2 (:items (recovered eng))) "the second death got its own :collected, not a stale :abandoned"))))))

(deftest recover-drops-waits-for-the-respawn-to-settle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds})
          (swap! clock + 100)
          (mem/write! (:store eng) :respawned {:pos {:x 0 :y 64 :z 0}})
          (swap! clock + 500)
          (core/submit! eng job {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (nil? (:decided (job-memory eng))) "500 ms after the respawn: not yet")
          (is (= [] (tu/walk-calls p)))
          (swap! clock + 2000)
          (await (core/tick! eng))
          (is (some? (:decided (job-memory eng))) "2500 ms after the respawn: decided")
          (await (run-until-empty eng 10))
          (is (= :collected (:decision (recovered eng)))))))))

;; The field race: the died reflex fired before :respawned was recorded, go-to "arrived" as the dead
;; body, and collect ran from the spawn point.
(deftest recover-drops-waits-for-the-respawn-before-planning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:floor tu/walk-floor :entities drops})]
          (die-unrespawned! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (swap! clock + 500)
          (await (core/tick! eng))
          (is (nil? (:decided (job-memory eng))) "dead: no decision")
          (is (= [] (tu/walk-calls p)) "dead: no walking")
          (is (= [] (calls p "collect")))
          (is (= 1 (count (:list (core/state eng)))) "the job stays listed")
          (swap! clock + 500)
          (mem/write! (:store eng) :respawned {:pos {:x 0 :y 64 :z 0}})
          (swap! clock + 3000)
          (await (run-until-empty eng 15))
          (is (= [death-pos] (tu/walked-to eng)))
          (is (= {:decision :collected :items 2} (select-keys (recovered eng) [:decision :items]))))))))

(deftest recover-drops-reports-a-skip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {})]
          (die! eng {:pos death-pos :inventory junk})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= [:skip] (map :decision (decided-events seen))))
          (is (seq (:text (first (decided-events seen))))))))))

(deftest recover-drops-reports-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 10))
          (is (= [:collected] (map :decision (decided-events seen))))
          (is (seq (:text (first (decided-events seen))))))))))

(deftest recover-drops-reports-abandoned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= [:abandoned] (map :decision (decided-events seen))))
          (is (seq (:text (first (decided-events seen))))))))))

;; ------------------------------------------- a job does not survive a death

(defn die-event [p] (.emit (.-world p) #js {:kind "died" :pos #js {:x 5 :y 64 :z 7} :inventory #js []}))

(defn cancelled-by [seen]
  (->> @seen (filter #(= [:job :cancelled] [(:source %) (:kind %)])) (mapv (juxt :job :by))))

(deftest a-death-drops-every-queued-and-held-job
  (let [{:keys [eng p seen]} (setup {})
        a (core/submit! eng job {})
        b (core/submit! eng job {:hold? true})
        c (core/submit! eng job {:front? true})]
    (die-event p)
    (is (= [] (:list (core/state eng))))
    (is (= {} (:instances (core/state eng))))
    (is (= #{[a :death] [b :death] [c :death]} (set (cancelled-by seen))))))

(deftest a-job-queued-after-the-death-survives
  (let [{:keys [eng p]} (setup {})]
    (core/submit! eng job {})
    (die-event p)
    (let [after (core/submit! eng job {})]
      (is (= [after] (:list (core/state eng)))))))

(deftest other-body-events-drop-nothing
  (doseq [event [#js {:kind "hurt" :health 5}
                 #js {:kind "respawned" :pos #js {:x 0 :y 64 :z 0} :dimension "overworld"}]]
    (let [{:keys [eng p]} (setup {})
          id (core/submit! eng job {})]
      (.emit (.-world p) event)
      (is (= [id] (:list (core/state eng))) (.-kind event)))))
