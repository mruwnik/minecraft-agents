(ns engine.death-test
  "Recovering drops after a death: the value and cost functions, the died
  trigger and the recover-drops job against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [jobs.lib.world-files :as ew]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.expr :as expr]
            [engine.fake :as fake]
            [engine.library-test :refer [setup calls inv]]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.lib.body :as body]
            [jobs.lib.cost.death :as death]
            [jobs.survival.recover-drops :as recover-drops]
            [jobs.survival.recover-drops.config :as cfg]
            [jobs.survival.recover-drops.estimate :as est]
            [triggers.survival.died :as died]))

;; The value and cost functions are jobs.lib.cost (jobs_value_test, cost_test); route danger jobs_danger_test.

(def sword "The body carries a weapon, so a zombie on the way is a fight it can afford." [{:name "iron_sword" :count 1}])

(def junk
  "A pile too small to walk 20 blocks for: a few dirt and seeds."
  [{:name "wheat_seeds" :count 5} {:name "dirt" :count 3}])

(def pile-values
  "recover-drops :value-overrides that price the field-report pile at a few hundred seconds, so the trip's danger and cost decide it."
  {"raw_iron" 3 "cobblestone" 0.05 "oak_planks" 0.05 "stone_pickaxe" 1 "diamond_pickaxe" 20 "diamond_sword" 10})

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

(defn died-holds [memory] (died/died nil memory {}))

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

(deftest burning-trigger-waits-while-the-body-is-dead
  (let [burning (:when (:burning triggers/all))
        world (tu/fake {:self {:inLava true :onFire true}})]
    (is (true? (burning world (memory-with 2000) {})))
    (is (false? (burning world (memory-with 2000 died-at) {})) "dead: the corpse lies in the lava until the respawn")
    (is (true? (burning world (memory-with 2000 died-at respawned-at) {})))))

(deftest died-trigger-does-not-hold-after-recovered
  (is (false? (died-holds (memory-with 2000 died-at respawned-at [:recovered 1500 {:decision :skip}]))))
  (is (true? (died-holds (memory-with 2000 [:recovered 500 {:decision :skip}] died-at respawned-at)))
      "a recovery older than the death does not count"))

(deftest died-trigger-counts-world-ticks-when-the-entries-have-an-age
  (let [aged (fn [now age]
               (assoc (memory-with now respawned-at) :age age
                      :data (-> mem/empty-data
                                (mem/add-entry :died {:t 1000 :age 100 :data {:pos {:x 20 :y 64 :z 0}}} nil)
                                (mem/add-entry :respawned {:t 1100 :data {:pos {:x 0 :y 64 :z 0}}} nil))))]
    (is (true? (died-holds (aged 400000 6099))) "a frozen or slow world: the wall clock is long past, the ticks are not")
    (is (false? (died-holds (aged 1200 6100))))))

(deftest died-trigger-does-not-hold-after-five-minutes
  (is (true? (died-holds (memory-with (+ 1000 299000) died-at respawned-at))))
  (is (false? (died-holds (memory-with (+ 1000 300000) died-at respawned-at)))))

;; ---------------------------------------------------- reflexes held while a pile is pending

(def dead-view (memory-with 2000 died-at))
(def alive-view (memory-with 2000 died-at respawned-at))

(deftest a-dead-body-holds-every-covered-reflex-back
  (is (every? #(body/recovery-pending? dead-view %) [:night :dusk-home :needs-bed :hungry :loose-items])))

(deftest a-respawned-body-is-sheltered-by-night-reflexes-and-kept-off-errands
  (are [id held] (= held (body/recovery-pending? alive-view id))
    :night false
    :dusk-home false
    :needs-bed false
    :hungry true
    :loose-items true))

(deftest the-covered-reflexes-are-a-setting
  (is (false? (settings/with-settings {::body/recovery-holds #{}} #(body/recovery-pending? alive-view :hungry))))
  (is (true? (settings/with-settings {::body/recovery-holds #{:night}} #(body/recovery-pending? alive-view :night)))))

(deftest the-hold-after-a-respawn-ends-after-a-set-time
  (let [later (memory-with (+ 1000 200000) died-at respawned-at)]
    (is (false? (body/recovery-pending? later :hungry)))
    (is (true? (settings/with-settings {::body/recovery-hold-s 250} #(body/recovery-pending? later :hungry))))))

(deftest died-trigger-is-registered-and-runs-the-job
  (is (= died/died (:when (:died triggers/all))))
  (is (= '(jobs.survival.recover-drops) (:job (:died triggers/all)))))

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
  ([eng data] (die! eng data 5000))
  ([eng data ago-ms]
   (let [store (:store eng)
         now ((:now @store))
         entry (fn [t d] {:t t :wt ((:world-time @store)) :data d})]
     (swap! store update :data
            #(-> % (mem/add-entry :died (entry (- now ago-ms) data) nil)
                 (mem/add-entry :respawned (entry (- now (- ago-ms 2000)) {:pos {:x 0 :y 64 :z 0}}) nil))))))

(defn recovered-trip [eng] (:data (mem/latest (mem/view (:store eng)) :recover-trip)))

(defn recovered [eng] (:data (mem/latest (mem/view (:store eng)) :recovered)))

(defn danger-remembered [eng] (:data (mem/latest (mem/view (:store eng)) :recover-danger)))

(defn ending-event [seen] (first (filter #(#{:completed :stopped} (:kind %)) @seen)))

(def job '(jobs.survival.recover-drops))

(defn emitted [seen kind] (filterv #(= kind (:kind %)) @seen))

(deftest recover-drops-warns-when-the-body-respawned-away-from-the-bed-it-slept-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities drops})]
          (mem/write! (:store eng) :bed {:pos {:x 40 :y 64 :z 0}} mem/place-policy)
          (mem/write! (:store eng) :slept {:pos {:x 40 :y 64 :z 0}} {:cap 10 :ttl 1000000})
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 10))
          (is (= 1 (count (emitted seen :respawn_away_from_bed))))
          (is (= :warn (:level (first (emitted seen :respawn_away_from_bed))))))))))

(defn ^:async away-warns
  "How many respawn_away_from_bed warns a recover-drops gives with only a :spawn-bed at spawn-bed."
  [spawn-bed]
  (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities drops})]
    (mem/write! (:store eng) :spawn-bed {:pos spawn-bed} {:cap 1 :ttl :forever})
    (die! eng {:pos death-pos :inventory diamonds})
    (core/submit! eng job {})
    (await (run-until-empty eng 10))
    (count (emitted seen :respawn_away_from_bed))))

(deftest recover-drops-warns-when-the-body-respawned-away-from-a-spawn-bed-it-never-slept-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= 1 (await (away-warns {:x 40 :y 64 :z 0}))))))))

(deftest recover-drops-does-not-warn-near-a-spawn-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= 0 (await (away-warns {:x 3 :y 64 :z 0}))))))))

(deftest recover-drops-does-not-warn-near-the-bed-or-without-sleeping-in-it
  (are [bed slept]
       (async done
         (tu/run-async done
           (fn ^:async t []
             (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities drops})]
               (mem/write! (:store eng) :bed {:pos bed} mem/place-policy)
               (when slept (mem/write! (:store eng) :slept {:pos slept} {:cap 10 :ttl 1000000}))
               (die! eng {:pos death-pos :inventory diamonds})
               (core/submit! eng job {})
               (await (run-until-empty eng 10))
               (is (= [] (emitted seen :respawn_away_from_bed)))))))
    {:x 3 :y 64 :z 0} {:x 3 :y 64 :z 0}
    {:x 40 :y 64 :z 0} nil
    {:x 40 :y 64 :z 0} {:x 90 :y 64 :z 0}))

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

(deftest recover-drops-collects-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "one round walks, collects and ends")
          (is (= {"diamond_pickaxe" 1 "diamond_sword" 1} (inv p)))
          (is (= :collected (:decision (recovered eng)))))))))

(deftest recover-drops-retries-a-blocked-walk-and-then-collects
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:floor tu/walk-floor :entities drops :unreachable ["20,64,0"]})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 2))
          (swap! clock + 60000)
          (is (nil? (recovered eng)) "a blocked walk is no verdict")
          (is (= [] (:list (core/state eng))) "the run ended; the death stays open for the trigger to fire again")
          (swap! (fake/state p) assoc :unreachable #{})
          (core/submit! eng job {})
          (is (< (await (run-until-empty eng 20)) 20))
          (is (= {"diamond_pickaxe" 1 "diamond_sword" 1} (inv p)))
          (is (= :collected (:decision (recovered eng)))))))))

(deftest recover-drops-reports-unreachable-when-the-window-closes-on-a-blocked-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup {:floor tu/walk-floor :entities drops :unreachable ["20,64,0"]})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 6))
          (swap! clock + 300000)
          (core/submit! eng job {})
          (await (run-until-empty eng 6))
          (is (= {:decision :abandoned :reason :unreachable} (select-keys (recovered eng) [:decision :reason]))))))))

(deftest recover-drops-collects-items-beyond-the-radius-from-the-body-but-near-the-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (drop-item 2 25.5 "diamond_sword")
              {:keys [eng p]} (setup {:floor tu/walk-floor :entities [(drop-item 1 20 "diamond_pickaxe") far]})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng (list 'jobs.survival.recover-drops {:settings {:jobs.survival.recover-drops.config/collect-radius 6}}) {})
          (is (< (await (run-until-empty eng 20)) 20))
          (is (= {"diamond_pickaxe" 1 "diamond_sword" 1} (inv p)))
          (is (= {:decision :collected :items 2} (select-keys (recovered eng) [:decision :items]))))))))

(deftest recover-drops-ends-partial-and-names-what-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities [(drop-item 1 20 "diamond_pickaxe")]})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (is (< (await (run-until-empty eng 20)) 20))
          (is (= {:decision :partial :items 1 :left {"diamond_sword" 1} :reason :not-visible}
                 (select-keys (recovered eng) [:decision :items :left :reason]))))))))

(deftest recover-drops-searches-around-the-pile-for-drops-not-in-sight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities [(drop-item 1 20 "diamond_pickaxe") (assoc (drop-item 2 22 "diamond_sword") :visible false)]})
              collected (atom false)
              show! (fn [v] (swap! (fake/state p) update :entities (fn [es] (mapv #(cond-> % (= 2 (:id %)) (assoc :visible v)) es))))]
          (.override (.-world p) "collect"
                     (fn [token a impl] (reset! collected true) (impl token a)))
          (.override (.-world p) "steer"
                     (fn [token a impl] (when @collected (show! true)) (impl token a)))
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng (list 'jobs.survival.recover-drops {:settings {:jobs.survival.recover-drops.config/collect-radius 8}}) {})
          (is (< (await (run-until-empty eng 40)) 40))
          (is (= {"diamond_pickaxe" 1 "diamond_sword" 1} (inv p)) "a short walk round the pile brought it into sight")
          (is (= :collected (:decision (recovered eng)))))))))

(deftest recover-drops-turns-to-look-for-a-drop-beside-the-body-outside-its-view
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities [(drop-item 1 20 "diamond_pickaxe") (assoc (drop-item 2 22 "diamond_sword") :visible false)]})]
          (.override (.-world p) "look"
                     (fn [token a impl]
                       (swap! (fake/state p) update :entities (fn [es] (mapv #(cond-> % (= 2 (:id %)) (assoc :visible true)) es)))
                       (impl token a)))
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng (list 'jobs.survival.recover-drops {:settings {:jobs.survival.recover-drops.config/collect-radius 8}}) {})
          (is (< (await (run-until-empty eng 40)) 40))
          (is (= {"diamond_pickaxe" 1 "diamond_sword" 1} (inv p)) "a turn of the head, not a walk round the pile, brought it into sight")
          (is (= :collected (:decision (recovered eng)))))))))

(defn die-twice!
  "A death 310 s ago (its pile gone) and one 5 s ago with a pile of diamonds; the newer declined for a danger that is remembered."
  [eng]
  (die! eng {:pos {:x 200 :y 64 :z 0} :inventory diamonds} 310000)
  (die! eng {:pos death-pos :inventory diamonds})
  (mem/write! (:store eng) :recover-danger {:death-t (:t (mem/latest (mem/view (:store eng)) :died)) :until (+ ((:now @(:store eng))) 60000) :text "creeper"}
              {:cap 5 :ttl 86400000}))

(deftest recover-drops-does-not-spend-a-run-on-an-expired-pile-while-a-newer-one-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:floor tu/walk-floor :entities drops})]
          (die-twice! eng)
          (core/submit! eng job {})
          (await (run-until-empty eng 10))
          (is (nil? (:reason (recovered eng))) (pr-str (recovered eng))))))))

(deftest recover-drops-retry-serves-the-newest-pile-and-drops-the-expired-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})]
          (die-twice! eng)
          (core/submit! eng '(jobs.survival.recover-drops {:retry true}) {})
          (await (run-until-empty eng 40))
          (is (= {"diamond_pickaxe" 1 "diamond_sword" 1} (inv p)))
          (is (= [:superseded :collected]
                 (mapv #(or (:reason (:data %)) (:decision (:data %))) (mem/entries (mem/view (:store eng)) :recovered)))))))))

(deftest recover-drops-looks-round-once-for-a-drop-that-went-out-of-sight-down-a-shaft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities [(drop-item 1 20 "diamond_pickaxe") (drop-item 2 24 "diamond_sword")]})
              hidden (atom false)
              show! (fn [v] (swap! (fake/state p) update :entities (fn [es] (mapv #(cond-> % (= 2 (:id %)) (assoc :visible v)) es))))]
          (.override (.-world p) "collect"
                     (fn [token a impl] (when-not @hidden (reset! hidden true) (show! false)) (impl token a)))
          (.override (.-world p) "steer"
                     (fn [token a impl] (when @hidden (show! true)) (impl token a)))
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng (list 'jobs.survival.recover-drops {:settings {:jobs.survival.recover-drops.config/collect-radius 8}}) {})
          (is (< (await (run-until-empty eng 40)) 40))
          (is (= {"diamond_pickaxe" 1 "diamond_sword" 1} (inv p)) "the walk to where it was last seen found it again")
          (is (= :collected (:decision (recovered eng)))))))))

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

(defn ^:async cut-at-the-walk!
  "Run the job until it is held at its first walk act, cut it, then call between."
  [eng p between]
  (.hold (.-world p) "moveTo")
  (.hold (.-world p) "steer")
  (let [running (core/tick! eng)]
    (loop [i 0]
      (when (and (empty? (tu/walk-calls p)) (< i 400))
        (await (js/Promise. (fn [r] (js/setTimeout r 5))))
        (recur (inc i))))
    (core/cut! eng (core/holder eng) :test nil)
    (await running)
    (between)))

(deftest recover-drops-counts-items-picked-up-on-the-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})
              iron [{:name "raw_iron" :count 20 :slot 0}]
              picked-up (fn ^:async picked [t a impl]
                          (let [r (await (impl t a))]
                            (swap! (fake/state p) #(-> % (assoc :entities []) (assoc :inventory [{:name "raw_iron" :count 20}])))
                            r))]
          (die! eng {:pos death-pos :inventory iron})
          (.override (.-world p) "moveTo" picked-up)
          (.override (.-world p) "steer" picked-up)
          (core/submit! eng job {})
          (await (core/tick! eng))
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

(defn appear-after-first-leg!
  "Cut the first walk step short (2 s, a few blocks, the clock moved on so the leg's seconds are used) and add entity e to the fake world as it is taken."
  [p clock e]
  (let [n (atom 0)]
    (.override (.-world p) "steer"
               (fn [token args impl]
                 (when (= 1 (swap! n inc))
                   (set! (.-timeoutS args) 2)
                   (swap! clock + 2000)
                   (swap! (fake/state p) update :entities conj e))
                 (impl token args)))))

(deftest recover-drops-rechecks-the-dangers-between-legs-of-the-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos [4 64 2]}
              {:keys [eng p seen clock]} (setup {:floor tu/walk-floor :inventory sword :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds})
          (appear-after-first-leg! p clock zombie)
          (core/submit! eng (list (first job) {:settings {:jobs.survival.recover-drops.config/leg-s 1} :value-overrides pile-values}) {})
          (await (run-until-empty eng 3))
          (is (re-find #"zombie" (:text (danger-remembered eng))) "a mob that turned up on the way makes the trip dear: it backs off")
          (is (nil? (recovered eng)) "the death stays open")
          (is (< (.-x (.-pos (.self p))) 19) "the body did not walk on to the pile"))))))

;; ------------------------------------------- every :declined says why

(defn declined-events [seen] (filterv #(and (= :stopped (:kind %)) (#{:danger :unreachable} (:reason %))) @seen))

(deftest recover-drops-declines-with-unreachable-when-the-walk-does-not-arrive
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities drops :unreachable ["20,64,0"]})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 6))
          (let [e (first (declined-events seen))]
            (is (= :unreachable (:reason e)))
            (is (seq (:text e)))))))))

(deftest recover-drops-does-not-count-a-walk-the-own-shelter-refused-as-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds})
          (with-redefs [ctx/call (fn [& _] (js/Promise.resolve
                                            (contract/ending :nothing-possible {:reason :unreachable :why :start-enclosed}
                                                             {:arrived false :why :start-enclosed :inside-own-shelter {:x 0 :y 64 :z 0}})))]
            (dotimes [_ 5]
              (core/submit! eng job {})
              (await (run-until-empty eng 6))))
          (is (nil? (recovered eng)) "no verdict: the trip waits for the shelter to open")
          (is (seq (declined-events seen)))
          (is (every? #(re-find #"shelter" (:text %)) (declined-events seen))))))))

;; ------------------------------------------- cut by a higher reflex, fired again

(defn ^:async tick-n! [eng clock n]
  (dotimes [_ n]
    (swap! clock + 500)
    (await (core/tick! eng))))

(defn fired-reflexes [seen]
  (keep #(when (= [:reflex :fired] [(:source %) (:kind %)]) (:reflex %)) @seen))

;; A death trip cut mid-walk resumes with the decision and the baseline taken before the cut, so what was
;; picked up on the walk still counts.
(deftest recover-drops-resumes-a-cut-trip-and-keeps-its-baseline
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:floor tu/walk-floor :entities drops})
              iron [{:name "raw_iron" :count 20 :slot 0}]]
          (die! eng {:pos death-pos :inventory iron})
          (core/submit! eng job {})
          (await (cut-at-the-walk! eng p #(swap! (fake/state p) (fn [w] (-> w (assoc :entities []) (assoc :inventory [{:name "raw_iron" :count 20}]))))))
          (is (= 1 (count (:list (core/state eng)))) "the cut job stays listed")
          (is (nil? (recovered eng)))
          (await (run-until-empty eng 5))
          (is (= {:decision :collected :items 20} (select-keys (recovered eng) [:decision :items]))))))))

;; ------------------------------------------- keyed to the death, settling, reporting

(def second-death-pos {:x -30 :y 64 :z 0})
(def second-drops [(drop-item 3 -30 "diamond_pickaxe") (drop-item 4 -31 "diamond_sword")])

(defn job-memory [eng] (core/job-memory eng "j1"))

(deftest recover-drops-starts-afresh-for-a-second-death
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:floor tu/walk-floor :entities (into drops second-drops)})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (cut-at-the-walk! eng p (fn [] (swap! clock + 10) (die! eng {:pos second-death-pos :inventory diamonds}))))
          (await (run-until-empty eng 15))
          (core/submit! eng job {})
          (await (run-until-empty eng 15))
          (is (= second-death-pos (last (tu/walked-to eng))))
          (is (= :collected (:decision (recovered eng))))
          (is (= 2 (:items (recovered eng))) "the second death got its own :collected, not a stale :abandoned"))))))

(defn later!
  "After ms real time: advance the fake clock by clock-ms, then f."
  [clock clock-ms ms f]
  (js/setTimeout (fn [] (swap! clock + clock-ms) (f)) ms))

(deftest recover-drops-waits-for-the-respawn-to-settle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:floor tu/walk-floor :entities drops})
              walked-at (atom nil)]
          (die! eng {:pos death-pos :inventory diamonds})
          (swap! clock + 100)
          (mem/write! (:store eng) :respawned {:pos {:x 0 :y 64 :z 0}})
          (swap! clock + 500)
          (core/submit! eng job {})
          (later! clock 2000 120 #(reset! walked-at (count (tu/walk-calls p))))
          (await (core/tick! eng))
          (is (= 0 @walked-at) "500 ms after the respawn: held, no walk")
          (is (some? (:decided (recovered-trip eng))) "2500 ms after the respawn: decided")
          (is (= [] (:list (core/state eng))))
          (is (= :collected (:decision (recovered eng)))))))))

;; The field race: the died reflex fired before :respawned was recorded, go-to "arrived" as the dead
;; body, and collect ran from the spawn point.
(deftest recover-drops-waits-for-the-respawn-before-planning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:floor tu/walk-floor :entities drops})
              walked-at (atom nil)]
          (die-unrespawned! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (later! clock 500 80 #(reset! walked-at (count (tu/walk-calls p))))
          (js/setTimeout (fn [] (mem/write! (:store eng) :respawned {:pos {:x 0 :y 64 :z 0}}) (swap! clock + 3000)) 160)
          (await (core/tick! eng))
          (is (= 0 @walked-at) "dead: no walking")
          (is (= [death-pos] (tu/walked-to eng)))
          (is (= {:decision :collected :items 2} (select-keys (recovered eng) [:decision :items]))))))))

(deftest recover-drops-holds-while-waiting-for-the-respawn-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock seen]} (setup {:floor tu/walk-floor :entities drops})]
          (die-unrespawned! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (js/setTimeout (fn [] (swap! clock + 500) (mem/write! (:store eng) :respawned {:pos {:x 0 :y 64 :z 0}}) (swap! clock + 3000)) 120)
          (await (core/tick! eng))
          (is (some #(and (= :holding (:kind %)) (= :respawning (:reason %))) @seen)))))))

(def noop-stub
  "A jobs.time.noop stand-in that records each pause asked of it and ends at once."
  (fn [pauses]
    {:round (fn ^:async stub-round [c] (swap! pauses conj (:args c)) (ctx/end c :done nil {}))}))

(deftest recover-drops-waits-for-the-respawn-in-noop-pause-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pauses (atom [])
              clock (atom 1000000)
              [_ sink] (tu/legacy-capture-sink)
              p (tu/seeing-all (tu/fake-on-floor {:floor tu/walk-floor :entities drops}))
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'jobs.time.noop (noop-stub pauses))
                                :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                :world (ew/of-data {} {} [])
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (die-unrespawned! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (js/setTimeout (fn [] (swap! clock + 500) (mem/write! (:store eng) :respawned {:pos {:x 0 :y 64 :z 0}}) (swap! clock + 3000)) 120)
          (await (core/tick! eng))
          (is (seq @pauses))
          (is (= #{:respawning} (set (map :why @pauses)))))))))

(deftest recover-drops-gives-up-waiting-when-the-window-closes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup {:floor tu/walk-floor :entities drops})]
          (die-unrespawned! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (later! clock 300000 80 identity)
          (await (core/tick! eng))
          (is (= {:decision :abandoned :reason :window-closed} (select-keys (recovered eng) [:decision :reason]))))))))

(deftest recover-drops-reports-a-skip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {})]
          (die! eng {:pos death-pos :inventory junk})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= :skip (:decision (ending-event seen))))
          (is (seq (:text (ending-event seen)))))))))

(deftest recover-drops-reports-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities drops})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 10))
          (is (= [:completed :collected] ((juxt :kind (comp :decision)) (ending-event seen))))
          (is (seq (:text (ending-event seen)))))))))

(deftest recover-drops-reports-abandoned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {})]
          (die! eng {:pos death-pos :inventory diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= :abandoned (:decision (ending-event seen))))
          (is (seq (:text (ending-event seen)))))))))

;; ------------------------------------------- value against cost, overrides, the decision text

(def zombie-by-the-way
  "A zombie 3 blocks off the walk to the death point, 15 from the body: no yield, but a danger on the way."
  {:id 7 :name "zombie" :kind "hostile" :pos {:x 15.5 :y 64 :z 3.5}})

(defn pile-drops [items]
  (map-indexed (fn [i {:keys [name count]}] {:id (+ 100 i) :name "item" :kind "item" :pos {:x 20 :y 64 :z 0}
                                             :item {:name name :count count}})
               items))

(defn ^:async picked-up-before-the-trip
  "The :recovered of a trip for a 42-item pile whose 36 cobblestone the body picked up at pickup-pos before the job."
  [pickup-pos]
  (let [pile [{:name "raw_iron" :count 6 :slot 0} {:name "cobblestone" :count 36 :slot 1}]
        {:keys [eng]} (setup {:floor tu/walk-floor :entities (vec (pile-drops (take 1 pile))) :inventory [{:name "cobblestone" :count 36}]})]
    (die! eng {:pos death-pos :inventory pile})
    (mem/write! (:store eng) :picked-up {:item "cobblestone" :count 36 :pos pickup-pos} {:cap 64 :ttl :forever})
    (core/submit! eng job {})
    (await (run-until-empty eng 30))
    (select-keys (recovered eng) [:decision :items :left])))

(deftest recover-drops-counts-only-pickups-at-the-pile-made-before-the-trip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:decision :collected :items 42} (await (picked-up-before-the-trip (assoc death-pos :x 21.5)))))
        (is (= {:decision :partial :items 6 :left {"cobblestone" 36}} (await (picked-up-before-the-trip {:x -30.5 :y 64 :z 0.5}))))))))

(deftest recover-drops-ends-partial-unreachable-when-the-pile-is-in-sight-and-none-comes-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [iron [{:name "raw_iron" :count 20 :slot 0}]
              {:keys [eng p]} (setup {:floor tu/walk-floor :entities (vec (pile-drops iron))})]
          (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :visible true) es)))
          (.override (.-world p) "collect" (fn [_ _ _] (clj->js {:status "unreachable"})))
          (die! eng {:pos death-pos :inventory iron})
          (core/submit! eng job {})
          (is (< (await (run-until-empty eng 30)) 30))
          (is (= {:decision :partial :items 0 :reason :unreachable}
                 (select-keys (recovered eng) [:decision :items :reason]))))))))

(deftest recover-drops-fetches-the-field-report-pile-past-a-zombie
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:floor tu/walk-floor :inventory sword :entities (conj (vec (pile-drops death-pile-of-the-field-report))
                                                                                zombie-by-the-way)})]
          (die! eng {:pos death-pos :inventory death-pile-of-the-field-report})
          (core/submit! eng job {})
          (loop [n 0]
            (when (and (< n 30) (nil? (recovered-trip eng)))
              (await (core/tick! eng))
              (recur (inc n))))
          (let [text (est/decision-text :fetch (:decided (recovered-trip eng)) (cfg/margin))]
            (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "hostile" (:kind e))) %))
            (core/submit! eng job {})
            (await (run-until-empty eng 20))
            (is (= :collected (:decision (recovered eng))))
            (is (= [death-pos] (tu/walked-to eng)) "one walk to the pile: its steer does not run out")
            (is (= "arrived" (:status (:data (mem/latest (mem/view (:store eng)) :moved)))))
            (is (re-find #"raw_iron" text) "the main item is named")
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

(deftest recover-drops-refuses-overrides-that-are-not-maps-at-submit
  (is (re-find #"jobs.survival.recover-drops :value-overrides must be a map of a string to a number or a map, got \"lots\""
               (expr/problem registry/jobs '(jobs.survival.recover-drops {:value-overrides "lots"})))))

;; ------------------------------------------- a job does not survive a death

(defn die-event [p] (.emit (.-world p) #js {:kind "died" :pos #js {:x 5 :y 64 :z 7} :inventory #js []}))

(defn cancelled-by [seen]
  (->> @seen (filter #(= [:job :cancelled] [(:source %) (:kind %)])) (mapv (juxt :job :by))))

(deftest a-death-drops-every-queued-job
  (let [{:keys [eng p seen]} (setup {})
        a (core/submit! eng job {})
        b (core/submit! eng job {})
        c (core/submit! eng job {:next? true})]
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

(deftest a-job-submitted-to-survive-a-death-runs-after-the-respawn-reflexes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:floor tu/walk-floor})
              kept (core/submit! eng '(jobs.movement.look-around) {:survive-death? true})]
          (core/submit! eng job {})
          (core/register-reflex! eng {:trigger :died})
          (die-event p)
          (is (= [kept] (:list (core/state eng))))
          (swap! clock + 1000)
          (.respawn (.-world p))
          (swap! clock + 2500) ; past recover-drops' settle wait; this clock only moves between ticks
          (await (run-until-empty eng 10))
          (let [order (keep-indexed (fn [i e] (cond (= [:reflex :fired :died] [(:source e) (:kind e) (:reflex e)]) [:fired i]
                                                    (and (= :round_started (:kind e)) (= kept (:job e))) [:survivor i]))
                                    @seen)]
            (is (= :fired (first (first order))) "recover-drops has its turn first")
            (is (some #(= :survivor (first %)) order) "then the surviving job runs")))))))

(deftest other-body-events-drop-nothing
  (doseq [event [#js {:kind "hurt" :health 5}
                 #js {:kind "respawned" :pos #js {:x 0 :y 64 :z 0} :dimension "overworld"}]]
    (let [{:keys [eng p]} (setup {})
          id (core/submit! eng job {})]
      (.emit (.-world p) event)
      (is (= [id] (:list (core/state eng))) (.-kind event)))))

;; ------------------------------------------------------------ death places

(defn deaths [eng] (mem/entries (mem/view (:store eng)) :deaths))

(deftest core-records-a-death-without-any-job
  (let [{:keys [eng]} (setup {:floor tu/walk-floor})]
    (core/record-body-event! eng #js {:kind "died" :pos #js {:x 1 :y 64 :z 2} :cause "lava" :dimension "the_nether"})
    (is (= [{:pos {:x 1 :y 64 :z 2} :dimension "the_nether" :cause "lava"}]
           (map #(dissoc (:data %) :death-t) (deaths eng))))
    (is (= (:t (mem/latest (mem/view (:store eng)) :died)) (:death-t (:data (first (deaths eng))))))
    (is (= :forever (:ttl (mem/policy (mem/view (:store eng)) :deaths))))))

(deftest core-records-each-death-once-and-caps-the-list
  (let [{:keys [eng]} (setup {:floor tu/walk-floor})]
    (core/record-body-event! eng #js {:kind "died" :pos #js {:x 0 :y 64 :z 0}})
    (core/record-death! eng (mem/latest (mem/view (:store eng)) :died))
    (is (= 1 (count (deaths eng))) "one death, one entry")
    (doseq [i (range 12)]
      (mem/write! (:store eng) :deaths {:pos {:x i :y 64 :z 0} :death-t i} core/deaths-policy))
    (is (= 10 (count (deaths eng))))))

(deftest the-notice-rate-is-the-gamma-posterior-of-the-counts-of-the-mobs-class
  (let [counts {:jobs.lib.cost.death/notice-rate {:other {:k 9 :n-s 38} :ranged {:k 2 :n-s 5}}}
        with #(settings/with-settings counts %)]
    (is (= {:shape 9.5 :rate 38} (with #(death/notice-prior "zombie"))))
    (is (= {:shape 9.5 :rate 38} (with #(death/notice-prior "creeper"))))
    (is (= {:shape 2.5 :rate 5} (with #(death/notice-prior "skeleton"))) "a skeleton is priced by the ranged counts")
    (is (= {:shape 9.5 :rate 38} (settings/with-settings {:jobs.lib.cost.death/notice-rate {:other [{:k 9 :n-s 38}] :ranged {:k 2 :n-s 5}}} #(death/notice-prior "zombie")))
        "one case counted apart is that case's posterior")
    (let [alike (settings/with-settings {:jobs.lib.cost.death/notice-rate {:other [{:k 5 :n-s 20} {:k 5 :n-s 20}] :ranged {:k 2 :n-s 5}}} #(death/notice-prior "zombie"))
          apart (settings/with-settings {:jobs.lib.cost.death/notice-rate {:other [{:k 9 :n-s 20} {:k 1 :n-s 20}] :ranged {:k 2 :n-s 5}}} #(death/notice-prior "zombie"))]
      (is (= {:shape 10.5 :rate 40} alike) "cases that agree pool to the posterior of their sums")
      (is (< (js/Math.abs (- (/ (:shape apart) (:rate apart)) (/ 10.5 40))) 1e-9) "cases that disagree keep the mean of their sums")
      (is (< (:shape apart) 5) "and widen it by their spread"))
    (is (< (js/Math.abs (- (- 1 (js/Math.pow (+ 1 (/ 2 38)) -9.5)) (with #(:met (death/met-stretch "zombie" (constantly 1) [{:x 0 :y 0 :z 0 :s 2}]))))) 1e-9)
        "2 s in the field at weight 1: 1 - E[e^(-2r)] over the posterior")))
