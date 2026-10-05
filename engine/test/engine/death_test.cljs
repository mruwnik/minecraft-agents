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
            [engine.triggers.died :as died]))

;; The value and cost functions are engine.jobs.value (jobs_value_test); route danger engine.jobs.danger.

(def junk
  "A pile too small to walk 20 blocks for: a few dirt and seeds."
  [{:name "wheat_seeds" :count 5} {:name "dirt" :count 3}])

(def death-pile-of-the-field-report
  [{:name "raw_iron" :count 4 :slot 17} {:name "raw_iron" :count 20 :slot 18}
   {:name "cobblestone" :count 64} {:name "cobblestone" :count 36}
   {:name "stone_pickaxe" :count 1} {:name "oak_planks" :count 29}])

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
          (is (< (:value (recovered eng)) (:cost (recovered eng)))))))))

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

(deftest recover-drops-counts-items-picked-up-on-the-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})
              iron [{:name "raw_iron" :count 20 :slot 0}]]
          (die! eng {:pos death-pos :inventory iron})
          (core/submit! eng job {})
          (loop [n 0]
            (when (and (< n 30) (empty? (tu/walk-calls p)))
              (await (core/tick! eng))
              (recur (inc n))))
          (swap! (fake/state p) #(-> % (assoc :entities []) (assoc :inventory [{:name "raw_iron" :count 20}])))
          (is (< (await (run-until-empty eng 10)) 10))
          (is (= {:decision :collected :items 20} (select-keys (recovered eng) [:decision :items]))))))))

(deftest recover-drops-counts-only-the-pile-items-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})
              iron [{:name "raw_iron" :count 20 :slot 0}]]
          (die! eng {:pos death-pos :inventory iron})
          (core/submit! eng job {})
          (loop [n 0]
            (when (and (< n 30) (empty? (tu/walk-calls p)))
              (await (core/tick! eng))
              (recur (inc n))))
          (swap! (fake/state p) #(-> % (assoc :entities []) (assoc :inventory [{:name "dirt" :count 9}])))
          (is (< (await (run-until-empty eng 10)) 10))
          (is (= {:decision :abandoned :items 0 :reason :nothing-found}
                 (select-keys (recovered eng) [:decision :items :reason]))))))))

(defn iron-drop [id x]
  {:id id :name "item" :kind "item" :pos {:x x :y 64 :z 0} :item {:name "raw_iron" :count 10}})

(def iron-pile [{:name "raw_iron" :count 20 :slot 0}])

(deftest recover-drops-collects-a-pile-scattered-eight-blocks-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities [(iron-drop 1 27) (iron-drop 2 28)]})]
          (die! eng {:pos death-pos :inventory iron-pile})
          (core/submit! eng job {})
          (is (< (await (run-until-empty eng 20)) 20))
          (is (= {"raw_iron" 20} (inv p)))
          (is (= {:decision :collected :items 20} (select-keys (recovered eng) [:decision :items]))))))))

(deftest recover-drops-leaves-a-foreign-pile-eight-blocks-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [foreign {:id 3 :name "item" :kind "item" :pos {:x 28 :y 64 :z 0} :item {:name "diamond" :count 5}}
              {:keys [eng p]} (setup {:floor tu/walk-floor :entities [(iron-drop 1 27) foreign]})]
          (die! eng {:pos death-pos :inventory [{:name "raw_iron" :count 10 :slot 0}]})
          (core/submit! eng job {})
          (is (< (await (run-until-empty eng 20)) 20))
          (is (= {"raw_iron" 10} (inv p)) "the foreign diamonds stay on the ground")
          (is (= [3] (map :id (filter #(= 3 (:id %)) (fake/entities p))))))))))

(deftest recover-drops-ignores-items-behind-a-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [z (range -3 4) y [64 65 66]] [(str "24," y "," z) "stone"]))
              {:keys [eng p]} (setup {:floor tu/walk-floor :blocks wall :entities [(iron-drop 1 27) (iron-drop 2 28)]})]
          (die! eng {:pos death-pos :inventory iron-pile})
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (is (= [] (calls p "collect")))
          (is (= {:decision :abandoned :items 0 :reason :nothing-found}
                 (select-keys (recovered eng) [:decision :items :reason]))))))))

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

;; ------------------------------------------- cut by a higher reflex, fired again

(defn ^:async tick-n! [eng clock n]
  (dotimes [_ n]
    (swap! clock + 500)
    (await (core/tick! eng))))

(defn fired-reflexes [seen]
  (keep #(when (= [:reflex :fired] [(:source %) (:kind %)]) (:reflex %)) @seen))

;; The owner's case: a death trip cut by a higher reflex resumes once that reflex ends, because the died trigger
;; still holds; the fresh recover-drops keeps the decision and the baseline taken before the cut, so what was
;; picked up on the walk still counts.
(deftest recover-drops-fires-again-after-a-higher-reflex-cuts-it-and-keeps-its-trip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:floor tu/walk-floor :entities drops})
              iron [{:name "raw_iron" :count 20 :slot 0}]]
          (core/register-reflex! eng {:trigger :burning})
          (core/register-reflex! eng {:trigger :died})
          (die! eng {:pos death-pos :inventory iron})
          (loop [n 0]
            (when (and (< n 30) (empty? (tu/walk-calls p)))
              (await (core/tick! eng))
              (recur (inc n))))
          (swap! (fake/state p) #(-> % (assoc :entities []) (assoc :inventory [{:name "raw_iron" :count 20}])))
          (fake/swap-self! p assoc :onFire true)
          (await (core/tick! eng))
          (is (= [:died :burning] (vec (fired-reflexes seen))) "burning cuts the trip")
          (fake/swap-self! p assoc :onFire false)
          (await (tick-n! eng clock 20))
          (is (= [:died :burning :died] (vec (take 3 (fired-reflexes seen)))) "the died trigger fires the trip again")
          (is (= {:decision :collected :items 20} (select-keys (recovered eng) [:decision :items]))))))))

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

;; ------------------------------------------- value against cost, overrides, the decision text

(def zombie-by-the-way
  "A zombie 3 blocks off the walk to the death point, 15 from the body: no yield, but a danger on the way."
  {:id 7 :name "zombie" :kind "hostile" :pos {:x 15.5 :y 64 :z 3.5}})

(defn fetching-events [seen] (filter #(= :recover-drops.fetching (:kind %)) @seen))

(defn pile-drops [items]
  (map-indexed (fn [i {:keys [name count]}] {:id (+ 100 i) :name "item" :kind "item" :pos {:x 20 :y 64 :z 0}
                                             :item {:name name :count count}})
               items))

(deftest recover-drops-fetches-the-field-report-pile-past-a-zombie
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:floor tu/walk-floor :entities (conj (vec (pile-drops death-pile-of-the-field-report))
                                                                                zombie-by-the-way)})]
          (die! eng {:pos death-pos :inventory death-pile-of-the-field-report})
          (core/submit! eng job {})
          (loop [n 0]
            (when (and (< n 30) (empty? (fetching-events seen)))
              (await (core/tick! eng))
              (recur (inc n))))
          (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "hostile" (:kind e))) %))
          (await (run-until-empty eng 20))
          (is (= :collected (:decision (recovered eng))))
          (is (= [death-pos] (tu/walked-to eng)))
          (let [text (:text (first (fetching-events seen)))]
            (is (re-find #"raw_iron" text) "the main item is named")
            (is (re-find #"zombie" text) "the danger on the way is named")
            (is (re-find #"walk" text))))))))

(deftest recover-drops-value-overrides-can-make-a-pile-worthless
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory iron-pile})
          (core/submit! eng (list 'jobs.survival.recover-drops {:value-overrides {"raw_iron" 0}}) {})
          (await (run-until-empty eng 5))
          (is (= [] (tu/walk-calls p)))
          (is (= :skip (:decision (recovered eng))))
          (is (= 0 (:value (recovered eng)))))))))

(deftest recover-drops-danger-overrides-can-make-a-zombie-too-dangerous
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:floor tu/walk-floor :entities (conj drops zombie-by-the-way)})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng (list 'jobs.survival.recover-drops {:danger-overrides {"zombie" 1000}}) {})
          (await (run-until-empty eng 5))
          (is (= [] (tu/walk-calls p)))
          (is (= :skip (:decision (recovered eng))))
          (is (re-find #"zombie" (:text (first (decided-events seen))))))))))

(deftest recover-drops-ignores-overrides-that-are-not-maps-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng (list 'jobs.survival.recover-drops {:value-overrides "lots"}) {})
          (await (run-until-empty eng 10))
          (is (= :collected (:decision (recovered eng))))
          (is (seq (filter #(= :recover-drops.bad-overrides (:kind %)) @seen))))))))

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
