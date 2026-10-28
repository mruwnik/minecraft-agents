(ns engine.scenarios-test
  "The shipped scenarios run end to end against the fake primitives."
  (:require [cljs.test :refer [deftest is async]]
            [clojure.set :as set]
            [clojure.string :as str]
            ["fs" :as fs]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.trigger-api :as trigger-api]
            [engine.triggers :as triggers]))

(def all-triggers
  "The shipped triggers plus the condition language, as a body has them (ad hoc :when entries)."
  (trigger-api/with-conditions triggers/all trigger-api/compile-condition))

(defn tree [x z height]
  (merge
   (into {} (for [y (range 64 (+ 64 height))] [(str x "," y "," z) "oak_log"]))
   {(str x "," (+ 64 height) "," z) "oak_leaves"
    (str (inc x) "," (+ 63 height) "," z) "oak_leaves"}))

(defn boot
  "An engine over the fake world loaded with the scenario file. Its clock moves ms-per-call (default 0) with every
  primitive call: a round that ends by time (a whole flight) needs it."
  ([file world] (boot file world 0))
  ([file world ms-per-call]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (cond-> (tu/seeing-all (tu/fake-on-floor world)) (contains? world :time) tu/with-sky-only)
        now (tu/act-clock clock p ms-per-call)
        eng (core/create {:primitives p :jobs registry/jobs :triggers all-triggers :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})
        s (scenario/read-file file)]
    (is (= [] (scenario/problems registry/jobs all-triggers s)))
    (trigger-api/load-scenario! eng s)
    {:eng eng :p p :seen seen :clock clock})))

(defn ^:async run-ticks [eng clock n step-ms]
  (loop [i 0]
    (when (< i n)
      (await (core/tick! eng))
      (swap! clock + step-ms)
      (recur (inc i)))))

(defn names-started [seen]
  (set (keep #(when (= :round_started (:kind %)) (:name %)) @seen)))

(deftest woodcutter-runs-on-the-fake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (boot "scenarios/woodcutter.edn"
                                               {:blocks (merge (tree 4 0 4) {"4,63,0" "dirt" "8,64,8" "chest"})
                                                :inventory [{:name "oak_sapling" :count 1}]
                                                :containers {"8,64,8" []}})]
          ;; The chest becomes known only after the harvest: a chest known from the start would be deposited into first.
          (await (run-ticks eng clock 20 1000))
          (is (= ["j2"] (:list (core/state eng))) "harvest finished; deposit declines without a chest")
          (mem/write! (:store eng) :chest {:pos {:x 8 :y 64 :z 8}} mem/place-policy)
          (await (run-ticks eng clock 20 1000))
          (is (= [] (:list (core/state eng))) "deposit finishes")
          (is (= #{"jobs.forestry.harvest-wood" "jobs.storage.deposit"} (names-started seen)))
          (is (not-any? #(or (= :required (:attention %)) (#{:failed :error} (:kind %))) @seen)))))))

(defn fired [seen]
  (keep #(when (= [:reflex :fired] [(:source %) (:kind %)]) (:reflex %)) @seen))

(deftest survival-idles-and-fires-its-reflexes-on-the-fake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (boot "scenarios/survival.edn"
                                               {:time 1000
                                                :floor [-45 -10 40 10]
                                                :self {:experience {:level 3 :points 40 :progress 0}}
                                                :inventory [{:name "bread" :count 4} {:name "stone_sword" :count 1}]}
                                               1000)
              food #(:food (fake/self p))]
          (is (= [:suffocating :burning :wedged :died :hostile-near :poisoned :night :low-health :dawn :dusk-home :hungry :stuck-underground :part-cut-tree :replant-owed :needs-bed :needs-weapon :door-left
                  :inventory-nearly-full :scaffold-left :tidy-pending
                  :pen-gate :mounted :dark-work]
                 (mapv :id (:register (core/state eng)))))
          (await (run-ticks eng clock 3 1000))
          (is (= [] (fired seen)) "a healthy body in daylight fires nothing")
          (is (= #{"(repeat jobs.time.noop)"} (names-started seen)) "it idles in a pause")

          (fake/swap-self! p assoc :health 3 :food 10)
          (await (run-ticks eng clock 3 1000))
          (is (= [:low-health] (fired seen)) "low health is the rest's, food under 18 included")
          (is (< 10 (food)) "the rest ate where the body stands")
          (is (= [] (tu/walk-calls p)) "no walk home")
          (fake/swap-self! p assoc :health 20)
          (await (run-ticks eng clock 3 1000))
          (is (some #(= [:reflex :ended :low-health] [(:source %) (:kind %) (:reflex %)]) @seen)
              "healed, the rest ends")

          (fake/add-entity! p {:id 50 :name "zombie" :kind "hostile" :pos [4 64 0] :health 20})
          (await (run-ticks eng clock 3 1000))
          (is (= [:low-health :hostile-near] (fired seen)) "a hostile fires respond-to-hostile (a whole flight)")
          (swap! (fake/state p) assoc :entities [])
          (await (run-ticks eng clock 10 1000))
          (is (some #(= [:reflex :ended :hostile-near] [(:source %) (:kind %) (:reflex %)]) @seen)
              "with the hostile gone the reflex ends")
          (let [looks (count (filter #(and (= :round_started (:kind %)) (= "j1" (:job %))) @seen))]
            (await (run-ticks eng clock 2 1000))
            (is (< looks (count (filter #(and (= :round_started (:kind %)) (= "j1" (:job %))) @seen)))
                "the body is back to looking around"))

          (.die (.-world p))
          (is (= {:level 3 :points 40} (:experience (:data (mem/latest (mem/view (:store eng)) :died))))
              "the died entry keeps the experience")
          (await (run-ticks eng clock 3 1000))
          (is (= [:low-health :hostile-near] (fired seen)) "a dead body fires nothing")
          (.respawn (.-world p))
          (swap! clock + 2500) ; the job holds still until the respawn has settled; this clock only moves between ticks
          (await (run-ticks eng clock 3 1000))
          (is (= [:low-health :hostile-near :died] (fired seen)) "the respawn lets a death fire recover-drops, which returns the sword")
          (await (run-ticks eng clock 10 1000))
          (is (= {:decision :collected :items 3}
                 (select-keys (:data (mem/latest (mem/view (:store eng)) :recovered)) [:decision :items]))
              "the drops lie at its feet, so the trip is worth it; items counts the stacks (two bread left after eating, the sword)")
          (is (= ["bread" "stone_sword"] (mapv #(.-name %) (.-inventory (.self p)))) "the bread and the sword are back")
          (is (not-any? #(and (#{:error :failed} (:kind %)) (not= :body (:source %))) @seen)
              "no errors besides the death itself"))))))

(deftest a-saved-register-with-hungry-before-died-still-recovers-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (boot "scenarios/survival.edn"
                                               {:time 1000 :floor [-45 -10 40 10]
                                                :self {:food 5}
                                                :inventory [{:name "stone_sword" :count 1}]}
                                               1000)
              reg (:register (core/state eng))
              hungry (some #(when (= :hungry (:id %)) %) reg)]
          (swap! (:state eng) assoc :register (vec (cons hungry (remove #(= :hungry (:id %)) reg))))
          (.die (.-world p))
          (await (run-ticks eng clock 3 1000))
          (is (= [] (fired seen)) "a dead body starts no hunt")
          (.respawn (.-world p))
          (fake/swap-self! p assoc :food 5)
          (swap! clock + 2500)
          (await (run-ticks eng clock 3 1000))
          (is (= :died (first (fired seen))) "the pile first, with :hungry listed ahead of :died"))))))

(def survival-cooldowns
  {:suffocating 0 :burning 0 :hungry 90 :needs-bed 600 :needs-weapon 600 :night 10 :door-left 5 :died 30 :inventory-nearly-full 120 :tidy-pending 10})

(deftest survival-triggers-wait-a-cooldown-after-their-job-ends
  (let [{:keys [eng]} (boot "scenarios/survival.edn" {})
        by-id (into {} (map (juxt :id identity)) (:register (core/state eng)))]
    (doseq [[id cooldown] survival-cooldowns]
      (is (= [:cooldown cooldown] ((juxt :persistence :cooldown-s) (by-id id))) (str id " in the register"))
      (is (= [:cooldown cooldown] ((juxt :persistence :cooldown-s) (triggers/all id))) (str id " by default")))))

;; A default trigger left out of survival.edn on purpose goes here, with the reason: a register that omits a new default
;; never offers it to existing bodies.
(def survival-excluded
  #{:loose-items ; would fire on the death pile the survival tests pin to recover-drops
    :ore-seen}) ; a survival scenario runs no mining job

(deftest survival-registers-every-default-trigger
  (let [{:keys [eng]} (boot "scenarios/survival.edn" {})]
    (is (= #{} (set/difference (set (keys triggers/all)) survival-excluded (set (map :id (:register (core/state eng)))))))))

(deftest survival-hostile-near-never-cools-down
  (let [{:keys [eng]} (boot "scenarios/survival.edn" {})
        entry (some #(when (= :hostile-near (:id %)) %) (:register (core/state eng)))]
    (is (= [:retry 0] ((juxt :persistence :cooldown-s) entry)) "a danger reflex: no cooldown, by the trigger's default")))

(deftest survival-digs-in-at-night-in-the-open-on-its-own
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (boot "scenarios/survival.edn"
                                               {:time 14000
                                                :inventory [{:name "dirt" :count 32} {:name "stone_sword" :count 1}]})]
          ;; the night is one round until morning: day comes at its first hold
          (.override (.-world p) "wait" (fn ^:async g [token a impl]
                                          (when (= 5000 (.-ms a)) (.setTime (.-world p) 1000))
                                          (await (impl token a))))
          (await (run-ticks eng clock 6 1000))
          (is (some #{:night} (fired seen)) "night in the open fires the night reflex with no job submitted")
          (is (contains? (names-started seen) "jobs.survival.night") "the reflex ran the night job")
          (is (seq (filter #(= "place" (.-name %)) (.-calls (.-world p)))) "no bed: shelter dug in and placed blocks"))))))

(def dirt-ground
  "Dirt (no tool needed) from y 60 to 63 over x and z -8..8."
  (into {} (for [x (range -8 9) z (range -8 9) y (range 60 64)] [(str x "," y "," z) "dirt"])))

(deftest survival-respawned-at-night-far-from-home-goes-for-the-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
                (let [{:keys [eng p seen clock]} (boot "scenarios/survival.edn" {:time 21000 :floor [-30 -10 260 10] :blocks dirt-ground :inventory [{:name "stone_sword" :count 1}]} 1000)
              store (:store eng)
              pile (for [i (range 3)] {:name "iron_ingot" :count 30 :slot i})]
          (mem/write! store :home {:pos {:x 200 :y 64 :z 0}} mem/place-policy)
          (mem/write! store :spawn-bed {:pos {:x 205 :y 64 :z 0}} {:cap 1 :ttl :forever})
          (mem/write! store :spawn-reset {} {:cap 5 :ttl 1200000})
          (mem/write! store :died {:pos {:x 200 :y 64 :z 0} :inventory pile} nil)
          (swap! clock + 1000)
          (mem/write! store :respawned {:pos {:x 0 :y 64 :z 0}} nil)
          (swap! clock + 5000)
          (await (run-ticks eng clock 8 1000))
          (is (not-any? #{:dusk-home} (fired seen)) "no walk home at night")
          (is (some #{:died} (fired seen)) "the pile is fetched in the dark, not waited for"))))))

(def scenario-files
  (->> (fs/readdirSync "scenarios") (filter #(str/ends-with? % ".edn")) sort))

(deftest died-is-registered-before-the-other-survival-reflexes
  (doseq [f scenario-files
          :let [ids (mapv :trigger (:register (scenario/read-file (str "scenarios/" f))))
                at (into {} (map-indexed (fn [i id] [id i])) ids)]
          :when (contains? at :died)
          later [:hostile-near :night :dusk-home :hungry]
          :when (contains? at later)]
    (is (< (at :died) (at later)) (str f ": :died before " later))))
