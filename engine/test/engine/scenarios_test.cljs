(ns engine.scenarios-test
  "The shipped scenarios run end to end against the fake primitives."
  (:require [cljs.test :refer [deftest is async]]
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
        p (tu/fake-on-floor world)
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
                                               {:blocks (merge (tree 4 0 4) {"8,64,8" "chest"})
                                                :inventory [{:name "oak_sapling" :count 1}]
                                                :containers {"8,64,8" []}})]
          ;; The chest becomes known only after the harvest: deposit puts away
          ;; every non-tool stack, saplings included, so with a chest known
          ;; from the start it would take the sapling the replant needs.
          (await (run-ticks eng clock 20 1000))
          (is (= ["j2"] (:list (core/state eng))) "harvest finished; deposit declines without a chest")
          (mem/write! (:store eng) :chest {:pos {:x 8 :y 64 :z 8}} mem/place-policy)
          (await (run-ticks eng clock 20 1000))
          (is (= [] (:list (core/state eng))) "deposit finishes")
          (is (= #{"jobs.forestry.harvest-wood" "jobs.storage.deposit"} (names-started seen)))
          (is (= "oak_sapling" (.-name (.blockAt p #js {:x 4 :y 64 :z 0}))) "replanted")
          (is (not-any? #(or (= :required (:attention %)) (#{:failed :error} (:kind %))) @seen)))))))

(deftest pace-cuts-runs-on-the-fake-and-the-reflex-cuts-pace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (boot "scenarios/pace-cuts.edn" {:self {:pos {:x -78 :y 69 :z -40}}})
              world (.-world p)]
          (await (core/tick! eng))
          (is (= ["jobs.movement.look-around"] (vec (names-started seen))) "the look timer fires at once")
          (let [release (.hold world "moveTo")
                pacing (core/tick! eng)]
            (swap! clock + 21000)
            (let [looking (core/tick! eng)]
              (release)
              (await pacing)
              (await looking)))
          (is (some #(= [:job :cut] [(:source %) (:kind %)]) @seen) "the reflex cut the pace round")
          (await (run-ticks eng clock 30 1000))
          (is (= [] (:list (core/state eng))) "pace resumes and finishes its rounds")
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
                                                :inventory [{:name "bread" :count 4}]}
                                               1000)
              food #(:food (fake/self p))]
          (is (= [:suffocating :burning :wedged :hostile-near :night :hungry :stuck :door-left
                  :died :inventory-nearly-full :scaffold-left :tidy-pending]
                 (mapv :id (:register (core/state eng)))))
          (await (run-ticks eng clock 3 1000))
          (is (= [] (fired seen)) "a healthy body in daylight fires nothing")
          (is (= #{"(repeat jobs.movement.look-around)"} (names-started seen)) "it idles looking around")

          (fake/swap-self! p assoc :health 5 :food 10)
          (await (run-ticks eng clock 3 1000))
          (is (= [:hungry] (fired seen)) "low health below 18 food fires get-food")
          (is (< 10 (food)) "get-food ate where the body stands")
          (is (= [] (tu/walk-calls p)) "no walk home")
          (fake/swap-self! p assoc :health 20)
          (await (run-ticks eng clock 3 1000))
          (is (some #(= [:reflex :ended :hungry] [(:source %) (:kind %) (:reflex %)]) @seen)
              "fed, get-food ends")

          (fake/add-entity! p {:id 50 :name "zombie" :kind "hostile" :pos [4 64 0] :health 20})
          (await (run-ticks eng clock 3 1000))
          (is (= [:hungry :hostile-near] (fired seen)) "a hostile fires respond-to-hostile (a whole flight)")
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
          (is (= [:hungry :hostile-near] (fired seen)) "a dead body fires nothing")
          (.respawn (.-world p))
          (swap! clock + 2500) ; the job holds still until the respawn has settled; this clock only moves between ticks
          (await (run-ticks eng clock 3 1000))
          (is (= [:hungry :hostile-near :died] (fired seen)) "the respawn lets a death fire recover-drops")
          (await (run-ticks eng clock 10 1000))
          (is (= {:decision :collected :items 2}
                 (select-keys (:data (mem/latest (mem/view (:store eng)) :recovered)) [:decision :items]))
              "the drops lie at its feet, so the trip is worth it; items counts the stack (two bread left after eating)")
          (is (= ["bread"] (mapv #(.-name %) (.-inventory (.self p)))) "the bread is back")
          (is (not-any? #(and (#{:error :failed} (:kind %)) (not= :body (:source %))) @seen)
              "no errors besides the death itself"))))))

(def survival-cooldowns
  {:suffocating 0 :burning 0 :hungry 90 :night 10 :stuck 60 :door-left 5 :died 30 :inventory-nearly-full 120 :tidy-pending 10})

(deftest survival-triggers-wait-a-cooldown-after-their-job-ends
  (let [{:keys [eng]} (boot "scenarios/survival.edn" {})
        by-id (into {} (map (juxt :id identity)) (:register (core/state eng)))]
    (doseq [[id cooldown] survival-cooldowns]
      (is (= [:cooldown cooldown] ((juxt :persistence :cooldown-s) (by-id id))) (str id " in the register"))
      (is (= [:cooldown cooldown] ((juxt :persistence :cooldown-s) (triggers/all id))) (str id " by default")))))

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
                                                :inventory [{:name "dirt" :count 16}]})]
          ;; the night is one round until morning: day comes at its first hold
          (.override (.-world p) "wait" (fn ^:async g [token a impl]
                                          (when (= 5000 (.-ms a)) (.setTime (.-world p) 1000))
                                          (await (impl token a))))
          (await (run-ticks eng clock 6 1000))
          (is (some #{:night} (fired seen)) "night in the open fires the night reflex with no job submitted")
          (is (contains? (names-started seen) "jobs.survival.night") "the reflex ran the night job")
          (is (seq (filter #(= "place" (.-name %)) (.-calls (.-world p)))) "no bed: shelter dug in and placed blocks"))))))
