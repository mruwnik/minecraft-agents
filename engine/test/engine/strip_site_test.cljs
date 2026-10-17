(ns engine.strip-site-test
  "jobs.strip.site over the fake world: every child is a recording stub, the corridors are the T-shaped site of
  engine.strip-test (stem z-27-0:0..20 into x-27-20:-20..0 and x-27-20:0..20)."
  (:require [cljs.reader :as reader]
            [engine.contract :as contract]
            [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            ["path" :as path]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.path.fixture :as fx]
            [engine.registry :as registry]
            [engine.strip-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.stock :as stock]
            [jobs.lib.world-files :as world]))

(def job 'jobs.strip.site)
(def blueprint
  (reader/read-string (fs/readFileSync (path/join (fx/engine-root) ".." "blueprints" "field-base.edn") "utf8")))
(def plan {:id "base" :parts [{:id "base" :blueprint "field-base" :at [0 64 100]}]})
(def args {:site "t" :at [0 -27 10] :base "base" :shaft false})
(def bed [1 65 102])
(def chest-1 [1 65 101])
(def chest-3 [4 65 101])
(def old-home {:x 5 :y 64 :z 5})
(def old-bed {:x 6 :y 64 :z 6})

(def site-blocks (into {} (map (fn [{:keys [name pos]}] [(str (:x pos) "," (:y pos) "," (:z pos)) name])) (st/t-world (constantly false))))

(defn xyz [[x y z]] {:x x :y y :z z})

(defn ending-of
  "The ending of a stub's result r: stopped with its :reason when its :status is :stopped, else done; the rest its data."
  [c r]
  (if (= :stopped (:status r))
    (ctx/end c :nothing-possible (:reason r) (dissoc r :status :reason))
    (ctx/end c :done nil (dissoc r :status))))

(def holds (constantly (contract/ok :holds "holds")))

(defn stub-of [real round-fn]
  (assoc (dissoc real :target :quote-of :done? :can-run? :safe? :fit?) :can-start? holds
         :round (fn ^:async stub-round [c] (ending-of c (round-fn c)))))

(defn ending-job
  "The real job of sym with its round replaced by one that ends with kind and cause."
  [sym kind cause]
  (assoc (dissoc (get registry/jobs sym) :can-run? :safe? :fit?) :can-start? holds :round (fn ^:async ending-round [c] (ctx/end c kind cause nil))))

(defn done [result] (fn [_] result))

(def furnace-1 [5 65 102])
(def furnace-2 [5 65 103])

(defn smelt-stub
  "jobs.items.smelt over a furnaces atom {furnace {:phase :cooking|:ready :item :count}}: the first call loads and yields, the
  condition fails :cooking until the furnace is :ready, then the next call gives the ingots; :fail makes it stop with that reason."
  [p calls furnaces fail]
  (let [key-of (comp pr-str :furnace :args)]
    (assoc (dissoc (get registry/jobs 'jobs.items.smelt) :target :done? :quote-of)
           :can-run? #(if (= :cooking (:phase (get @furnaces (key-of %))))
                          (contract/later :cooking "cooking")
                          (contract/ok :ready "ready"))
           :round (fn ^:async smelt-round [c]
                    (swap! calls conj ['jobs.items.smelt (:args c)])
                    (let [{:keys [furnace item count]} (:args c)
                          state (get @furnaces (key-of c))]
                      (cond
                        fail (ctx/end c :nothing-possible fail)
                        (= :cooking (:phase state)) (ctx/end c :condition-failed :cooking)
                        (nil? state)
 (do (swap! (fake/state p) update :inventory (fn [inv] (mapv #(if (= item (:name %)) (update % :count - count) %) inv)))
                                         (swap! furnaces assoc (key-of c) {:phase :cooking :item item :count count})
                                         :continue)
                        :else (do (fake/add-item! p "iron_ingot" (:count state))
                                  (swap! furnaces dissoc (key-of c))
                                  (ctx/end c :done nil {:smelted (:count state) :wanted (:count state)}))))))))

(defn ready! [furnaces] (swap! furnaces update-vals #(assoc % :phase :ready)))

(defn setup
  "An engine over the site with every child of the job stubbed. results: job symbol -> (fn [ctx]) giving the child's result,
  the default being success."
  [& {:keys [results inventory dir pos plans fail-smelt entities triggers ending-jobs food]}]
  (let [results (atom results)
        furnaces (atom {})
        p (tu/seeing-all (tu/fake {:self (cond-> {:pos (or pos {:x 0 :y -27 :z 10})} food (assoc :food food)) :blocks (cond-> site-blocks pos (assoc "0,63,100" "stone" "12,63,100" "stone" "12,59,100" "stone"))
                                   :inventory (or inventory [{:name "torch" :count 32} {:name "iron_pickaxe" :count 1}])
                                   :entities (or entities [])}))
        calls (atom [])
        defaults {'jobs.build.from-plan {:placed 5}
                  'jobs.survival.set-spawn {:status :completed}
                  'jobs.memory.set-place {:ok true}
                  'jobs.memory.forget-place {:ok true}
                  'jobs.items.get-tool {:status :done}
                  'jobs.access.stair {:status :done}
                  'jobs.strip.corridor {:status :done :taken {:wood 3} :new-segments []}
                  'jobs.strip.seal {:status :done :reason :sealed :placed 9}
                  'jobs.storage.haul {:gave-up false :moved {"rail" 7}}
                  'jobs.storage.deposit {:gave-up false}
                  'jobs.storage.withdraw {:gave-up false}
                  'jobs.items.obtain {:status :done}
                  'jobs.survival.get-food {:status :done}}
        stubs (into {} (for [[sym res] defaults]
                         [sym (stub-of (get registry/jobs sym)
                                       (fn [c]
                                         (swap! calls conj [sym (:args c)])
                                         (let [r (let [r (get @results sym res)] (if (fn? r) (r (:args c)) r))]
                                           (when (and (= 'jobs.items.get-tool sym) (not= :stopped (:status r))) (fake/add-item! p "shears" 1))
                                           (when (= 'jobs.access.stair sym) (swap! (fake/state p) assoc-in [:self :pos 1] (:y (:args c))))
                                           r)))]))
        go-to (stub-of (get registry/jobs 'jobs.movement.go-to)
                       (fn [c]
                         (let [pos (:pos (:args c))
                               [x y z] (if (map? pos) [(:x pos) (:y pos) (:z pos)] pos)]
                           (swap! calls conj ['jobs.movement.go-to {:pos [x y z]}])
                           (swap! (fake/state p) assoc-in [:self :pos] [x y z])
                           {:arrived true})))
        clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        eng (core/create {:primitives p :jobs (merge (assoc (merge registry/jobs stubs) 'jobs.movement.go-to go-to 'jobs.items.smelt (smelt-stub p calls furnaces fail-smelt)) ending-jobs)
                          :triggers (or triggers triggers/all) :dir (or dir (tu/tmp-dir)) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})
                          :world (world/of-data (merge {"base" plan} plans) {"field-base" blueprint} [])})]
    (mem/write! (:store eng) :home {:pos old-home} mem/place-policy)
    (mem/write! (:store eng) :spawn-bed {:pos old-bed} {:cap 1 :ttl :forever})
    {:eng eng :p p :calls calls :results results :seen seen :furnaces furnaces :clock clock}))

(defn start! [{:keys [eng]} a] (tu/child-outcome eng job a 600))
(defn names [{:keys [calls]}] (mapv (comp name first) @calls))
(defn called [{:keys [calls]} sym] (mapv second (filter #(= sym (first %)) @calls)))

(deftest strips-every-segment-seals-leaf-first-and-hauls-the-base-chests
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup)
              r (await (start! env args))
              seg-ids (mapv (comp :id :segment) (called env 'jobs.strip.corridor))]
          (is (= :done (:status r)))
          (is (= {:sealed 2 :stripped 1} (:segments r)))
          (is (= ["z-27-0:0..20" "x-27-20:-20..0" "x-27-20:0..20"] seg-ids) "the stem next to the entry first, then its branches")
          (is (= [{:at (xyz [-1 -27 20]) :axis :x :side :to} {:at (xyz [1 -27 20]) :axis :x :side :from}]
                 (mapv #(select-keys % [:at :axis :side]) (called env 'jobs.strip.seal))))
          (is (= ["from-plan" "set-spawn" "set-place" "get-tool" "corridor" "corridor" "seal" "corridor" "seal" "haul" "set-place" "set-spawn"]
                 (mapv #(last (re-find #"\.([a-z-]+)$" %)) (names env))) "the order of the children, home and spawn restored last")
          (is (= {:from [chest-1 chest-3] :to nil} (select-keys (first (called env 'jobs.storage.haul)) [:from :to]))))))))

(deftest home-and-spawn-sit-at-the-base-while-stripping-and-come-back-after-the-haul
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup)
              _ (await (start! env args))
              places (filterv #(= 'jobs.memory.set-place (first %)) @(:calls env))
              spawns (called env 'jobs.survival.set-spawn)]
          (is (= [{:name "home" :pos (xyz bed)} {:name "home" :pos old-home}] (mapv (comp #(select-keys % [:name :pos]) second) places)))
          (is (= [(xyz bed) old-bed] (mapv :bed spawns))))))))

(deftest a-restart-after-the-base-phase-does-not-build-it-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :results {'jobs.items.get-tool {:status :stopped :reason :no-source}})
              first-run (await (start! env args))
              _ (reset! (:calls env) [])
              _ (swap! (fake/state (:p env)) update :inventory conj {:name "shears" :count 1})
              second-run (await (start! env args))]
          (is (= :stopped (:status first-run)))
          (is (= :done (:status second-run)))
          (is (empty? (called env 'jobs.build.from-plan)) "the saved phase says the base stands")
          (is (empty? (called env 'jobs.items.get-tool)) "shears are carried"))))))

(deftest a-child-failure-stops-with-its-reason-and-puts-home-and-spawn-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :results {'jobs.strip.seal {:status :stopped :reason :no-blocks}})
              r (await (start! env args))]
          (is (= {:status :stopped :reason :child-failed :child "jobs.strip.seal" :because :no-blocks :phase :strip}
                 (select-keys r [:status :reason :child :because :phase])))
          (is (= ["set-place" "set-spawn"] (mapv #(last (re-find #"\.([a-z-]+)$" %)) (take-last 2 (names env)))) "restored on the stop"))))))

(deftest a-relisted-site-resumes-at-the-segment-it-stopped-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :results {'jobs.strip.seal {:status :stopped :reason :no-blocks}})
              _ (await (start! env args))
              _ (reset! (:calls env) [])
              _ (swap! (:results env) assoc 'jobs.strip.seal {:status :done :reason :sealed :placed 9})
              r (await (start! env args))]
          (is (= :done (:status r)))
          (is (= {:sealed 2 :stripped 1} (:segments r)))
          (is (= ["x-27-20:0..20"] (mapv (comp :id :segment) (called env 'jobs.strip.corridor))) "only the segment not yet stripped")
          (is (empty? (called env 'jobs.build.from-plan))))))))

(deftest a-segment-that-stops-three-times-for-one-reason-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [refuse (fn [a] (if (= "x-27-20:0..20" (:id (:segment a))) {:status :stopped :reason :refused} {:status :done :taken {}}))
              env (setup :results {'jobs.strip.corridor refuse})
              r (await (start! env args))]
          (is (= :done (:status r)))
          (is (= {:stripped 1 :sealed 1 :skipped 1} (:segments r)))
          (is (= 3 (count (filter #(= "x-27-20:0..20" (:id (:segment %))) (called env 'jobs.strip.corridor))))))))))

(deftest a-site-whose-every-segment-was-skipped-ends-all-skipped-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :results {'jobs.strip.corridor {:status :stopped :reason :refused}
                                   'jobs.strip.seal {:status :stopped :reason :refused}})
              r (await (start! env args))]
          (is (= [:stopped :all-skipped] ((juxt :status :reason) r)))
          (is (= {:refused 3} (:skipped r))))))))

(deftest a-full-bag-sends-the-body-to-the-base-chest-and-back-to-the-same-segment
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              full-once (fn [_] (if (= 1 (swap! n inc)) {:status :stopped :reason :bag-full :taken {:wood 4}} {:status :done :taken {:wood 2}}))
              env (setup :results {'jobs.strip.corridor full-once})
              r (await (start! env args))
              ids (mapv (comp :id :segment) (called env 'jobs.strip.corridor))]
          (is (= :done (:status r)))
          (is (= {:wood 10} (:taken r)))
          (is (= ["z-27-0:0..20" "z-27-0:0..20" "x-27-20:-20..0" "x-27-20:0..20"] ids))
          (is (= [{:chest (xyz chest-1) :keep {"torch" 64}}] (mapv #(select-keys % [:chest :keep]) (called env 'jobs.storage.deposit)))))))))

(deftest no-torches-and-none-to-be-had-stops-no-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :inventory [{:name "iron_pickaxe" :count 1}])
              r (await (start! env args))]
          (is (= {:status :stopped :reason :no-torches :phase :strip} (select-keys r [:status :reason :phase])))
          (is (= ["torch"] (mapv :item (called env 'jobs.items.obtain))))
          (is (empty? (called env 'jobs.strip.corridor))))))))

(defn hungry-env
  "A site run whose first corridor call stops :food-short, the body hungry with nothing to eat and get-food known to have failed."
  [& {:as opts}]
  (let [n (atom 0)
        first-food-short (fn [_] (if (= 1 (swap! n inc)) {:status :stopped :reason :food-short} {:status :done :taken {:wood 2}}))
        env (apply setup (apply concat (merge {:food 2 :results {'jobs.strip.corridor first-food-short}} opts)))]
    (mem/write! (:store (:eng env)) :hungry {:food 2} {:cap 10 :ttl 3600000})
    env))

(deftest a-food-trip-gets-food-with-get-food-and-goes-back-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [penv (atom nil)
              env (hungry-env :results {'jobs.strip.corridor (let [n (atom 0)] (fn [_] (if (= 1 (swap! n inc)) {:status :stopped :reason :food-short} {:status :done :taken {:wood 2}})))
                                        'jobs.survival.get-food (fn [_] (fake/add-item! (:p @penv) "bread" 1) {:status :done})})
              _ (reset! penv env)
              r (await (start! env args))]
          (is (= :done (:status r)))
          (is (= 1 (count (called env 'jobs.survival.get-food)))))))))

(deftest a-food-trip-that-gets-no-food-stops-food-short-after-one-try
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (hungry-env)
              r (await (start! env args))]
          (is (= {:status :stopped :reason :food-short} (select-keys r [:status :reason])))
          (is (= 1 (count (called env 'jobs.survival.get-food)))))))))

(deftest a-food-trip-with-fetch-off-stops-food-short-without-get-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (hungry-env)
              r (await (start! env (assoc args :fetch false)))]
          (is (= {:status :stopped :reason :food-short} (select-keys r [:status :reason])))
          (is (empty? (called env 'jobs.survival.get-food))))))))

(deftest the-seal-child-gets-the-fetch-arg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup)
              _ (await (start! env (assoc args :fetch #{:tool})))]
          (is (= [#{:tool}] (distinct (map :fetch (called env 'jobs.strip.seal))))))))))

(deftest a-body-inside-the-radius-but-far-from-at-walks-to-at-before-reading-the-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :pos {:x 0 :y -27 :z 50})
              r (await (start! env args))]
          (is (= :done (:status r)))
          (is (= [[0 -27 10]] (mapv :pos (called env 'jobs.movement.go-to)))))))))

(deftest a-shaft-plan-is-taken-down-with-the-stair-through-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shaft {:id "shaft" :kind :mineshaft :parts [{:id "stair" :stair {:from [12 64 100] :heading :east :to-y 60} :want :clear}]}
              env (setup :pos {:x 0 :y 64 :z 100} :plans {"shaft" shaft})
              r (await (start! env (assoc args :shaft "shaft")))]
          (is (= :done (:status r)))
          (is (= [{:dir :down :heading :east :y 60 :through "shaft"}] (mapv #(select-keys % [:dir :heading :y :through]) (called env 'jobs.access.stair)))))))))

(deftest haul-false-leaves-the-loot-in-the-base-chests
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup)
              r (await (start! env (assoc args :haul false)))]
          (is (= :done (:status r)))
          (is (empty? (called env 'jobs.storage.haul)))
          (is (= ["set-place" "set-spawn"] (mapv #(last (re-find #"\.([a-z-]+)$" %)) (take-last 2 (names env))))))))))

(deftest a-spawner-stop-seals-the-segment-at-once-and-skips-what-hangs-beyond-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spawner (fn [a] (if (= "x-27-20:-20..0" (:id (:segment a))) {:status :stopped :reason :spawner :cell [-5 -26 22]} {:status :done :taken {}}))
              env (setup :results {'jobs.strip.corridor spawner})
              r (await (start! env (assoc args :spawner :seal)))]
          (is (= :done (:status r)))
          (is (= {:sealed 2 :stripped 1} (:segments r)))
          (is (= [:seal] (distinct (map :spawner (called env 'jobs.strip.corridor)))) "the policy goes to the corridor")
          (is (= 1 (count (filter #(= "x-27-20:-20..0" (:id (:segment %))) (called env 'jobs.strip.corridor)))) "not retried")
          (is (= 2 (count (called env 'jobs.strip.seal)))))))))

(def ore-bag [{:name "torch" :count 32} {:name "iron_pickaxe" :count 1} {:name "raw_iron" :count 16} {:name "coal" :count 4}])

(defn full-then [env after]
  (let [n (atom 0)]
    (fn [_] (case (swap! n inc)
              1 {:status :stopped :reason :bag-full :taken {}}
              (do (after env) {:status :done :taken {}})))))

(deftest a-return-loads-both-furnaces-and-goes-back-down-and-the-next-visit-deposits-the-ingots-before-the-haul
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :inventory ore-bag)
              _ (swap! (:results env) assoc 'jobs.strip.corridor (full-then env #(ready! (:furnaces %))))
              r (await (start! env args))
              smelts (called env 'jobs.items.smelt)
              short-names (mapv #(last (re-find #"\.([a-z-]+)$" %)) (names env))]
          (is (= :done (:status r)))
          (is (= [{:furnace (xyz furnace-1) :item "raw_iron" :count 8} {:furnace (xyz furnace-2) :item "raw_iron" :count 8}]
                 (mapv #(select-keys % [:furnace :item :count]) (take 2 smelts))) "both furnaces loaded, half each")
          (is (= ["corridor" "smelt" "smelt" "deposit" "corridor"] (subvec short-names 4 9)) "back down without waiting")
          (is (= ["smelt" "smelt" "deposit" "haul"] (take 4 (take-last 6 short-names))) "ingots collected and banked before the haul")
          (is (= (take 2 smelts) (drop 2 smelts)) "the same args resume the same children")
          (is (= {"raw_iron" 16} (:smelted r))))))))

(deftest haul-false-still-collects-the-output-still-cooking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :inventory ore-bag)
              _ (swap! (:results env) assoc 'jobs.strip.corridor (full-then env #(ready! (:furnaces %))))
              r (await (start! env (assoc args :haul false)))]
          (is (= :done (:status r)))
          (is (empty? (called env 'jobs.storage.haul)))
          (is (= ["smelt" "smelt" "deposit"] (take 3 (take-last 5 (mapv #(last (re-find #"\.([a-z-]+)$" %)) (names env))))) "ingots collected and banked before the cleanup")
          (is (= {"raw_iron" 16} (:smelted r))))))))

(deftest raw-ore-the-base-chests-hold-is-withdrawn-before-it-is-loaded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup)
              _ (mem/write! (:store (:eng env)) stock/stock-kind {:pos chest-1 :items {"raw_iron" 20 "stone" 9}} stock/stock-policy)
              _ (swap! (:results env) assoc
                       'jobs.strip.corridor (full-then env #(ready! (:furnaces %)))
                       'jobs.storage.withdraw (fn [_] (fake/add-item! (:p env) "raw_iron" 20) {:gave-up false}))
              r (await (start! env args))
              short-names (mapv #(last (re-find #"\.([a-z-]+)$" %)) (names env))]
          (is (= :done (:status r)))
          (is (= [{:chest (xyz chest-1) :items {"raw_iron" 20}}] (mapv #(select-keys % [:chest :items]) (called env 'jobs.storage.withdraw))))
          (is (= ["withdraw" "smelt" "smelt"] (subvec short-names 5 8)) "taken out, then loaded in both furnaces")
          (is (= [10 10] (mapv :count (take 2 (called env 'jobs.items.smelt))))))))))

(deftest smelt-false-leaves-the-ore-raw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :inventory ore-bag)
              _ (swap! (:results env) assoc 'jobs.strip.corridor (full-then env (constantly nil)))
              r (await (start! env (assoc args :smelt false)))]
          (is (= :done (:status r)))
          (is (empty? (called env 'jobs.items.smelt)))
          (is (= 1 (count (called env 'jobs.storage.deposit)))))))))

(deftest a-smelt-that-gives-up-does-not-stop-the-strip-and-is-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :inventory ore-bag :fail-smelt "no-fuel")
              _ (swap! (:results env) assoc 'jobs.strip.corridor (full-then env (constantly nil)))
              r (await (start! env args))]
          (is (= :done (:status r)))
          (is (= ["no-fuel" "no-fuel"] (mapv :reason (:smelt-failed r))))
          (is (= 2 (count (called env 'jobs.items.smelt))) "once per furnace, no retry loop"))))))

(defn ends [r] [(:kind (:ending r)) (:cause (:ending r))])

(deftest the-ending-kind-follows-the-stop-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [no-torches (await (start! (setup :inventory [{:name "iron_pickaxe" :count 1}]) args))
              seal-fails (await (start! (setup :results {'jobs.strip.seal {:status :stopped :reason :no-blocks}}) args))
              done (await (start! (setup) args))]
          (is (= [:condition-failed :no-torches] (ends no-torches)))
          (is (= :strip (:phase no-torches)) "the data keeps the phase it stopped in")
          (is (= [:nothing-possible :child-failed] (ends seal-fails)) "a child that stopped on its own reason is no failed condition")
          (is (= {:child "jobs.strip.seal" :because :no-blocks} (select-keys seal-fails [:child :because])))
          (is (= :done (:status done)))
          (is (= {:sealed 2 :stripped 1} (:segments done))))))))

(deftest a-childs-ending-kind-is-passed-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[kind expected] [[:condition-failed [:condition-failed :child-failed]]
                                 [:impossible [:impossible :child-failed]]
                                 [:limit-reached [:done nil]]]]
          (let [r (await (start! (setup :ending-jobs {'jobs.strip.seal (ending-job 'jobs.strip.seal kind :whatever)}) args))]
            (is (= expected [(or (:kind (:ending r)) (:status r)) (:cause (:ending r))]) (str kind))
            (is (= {:child "jobs.strip.seal" :because :whatever} (select-keys r [:child :because])))))))))

(deftest a-seal-limit-below-least-does-not-end-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (start! (setup :ending-jobs {'jobs.strip.seal (ending-job 'jobs.strip.seal :limit-reached :whatever)})
                               (assoc args :least 99)))]
          (is (= [:stopped :limit-reached :child-failed] [(:status r) (:kind (:ending r)) (:cause (:ending r))]) "a :least the stripped segments do not reach ends the site stopped"))))))

(deftest a-failed-walk-down-keeps-the-go-tos-own-kind-and-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shaft {:id "shaft" :kind :mineshaft :parts [{:id "stair" :stair {:from [12 64 100] :heading :east :to-y 60} :want :clear}]}
              run (fn [walk-end] (start! (setup :pos {:x 0 :y 64 :z 100} :plans {"shaft" shaft} :ending-jobs {'jobs.movement.go-to (ending-job 'jobs.movement.go-to (first walk-end) (second walk-end))}) (assoc args :shaft "shaft")))
              stuck (await (run [:nothing-possible :unreachable]))
              blocked (await (run [:condition-failed :no-path]))]
          (is (= [:nothing-possible :unreachable] (ends stuck)))
          (is (= [:condition-failed :child-failed] (ends blocked)))
          (is (= {:child "jobs.movement.go-to" :because :no-path} (select-keys blocked [:child :because]))))))))

(deftest a-call-without-site-or-base-or-at-for-a-new-site-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [no-site (await (start! (setup) (dissoc args :site)))
              no-base (await (start! (setup) (dissoc args :base)))
              no-at (await (start! (setup) (dissoc args :at)))]
          (is (= [[:impossible :bad-args] [:impossible :bad-args] [:impossible :bad-args]]
                 (mapv (juxt (comp :kind :ending) :reason) [no-site no-base no-at]))))))))

(deftest a-danger-near-holds-the-site-back-from-the-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :triggers {} :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 0 :y -27 :z 13} :health 20 :visible true}])
              r (await (start! env args))]
          (is (= :not-done r) "the job yields every round")
          (is (= :hostile-near (:reason (first (filter #(= :waiting (:kind %)) @(:seen env))))) "waiting with its reason")
          (is (empty? (names env)) "no phase ran"))))))

(deftest a-danger-arriving-after-the-start-holds-the-resume
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env-at (atom nil)
              zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 0 :y -27 :z 13} :health 20 :visible true}
              env (setup :triggers {}
                         :results {'jobs.strip.corridor (fn [_]
                                                          (fake/add-entity! (:p @env-at) zombie)
                                                          (core/cancel! (:eng @env-at) (first (:list (core/state (:eng @env-at)))))
                                                          {:status :done :taken {} :new-segments []})})
              _ (reset! env-at env)]
          (core/submit! (:eng env) (list job args) {})
          (await (tu/run-until-empty (:eng env) 600))
          (is (= 1 (count (called env 'jobs.strip.corridor))) "cut after the first corridor")
          (is (= :not-done (await (start! env (dissoc args :at)))) "the resume yields every round")
          (is (= 1 (count (called env 'jobs.strip.corridor))) "no corridor ran with the hostile near")
          (is (= :hostile-near (:reason (last (filter #(= :waiting (:kind %)) @(:seen env)))))))))))

(deftest a-cut-site-puts-home-and-spawn-back-in-its-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [eng-at (atom nil)
              env (setup :results {'jobs.strip.corridor (fn [_] (core/cancel! @eng-at (first (:list (core/state @eng-at)))) {:status :done :taken {} :new-segments []})})
              _ (reset! eng-at (:eng env))]
          (core/submit! (:eng env) (list job args) {})
          (await (tu/run-until-empty (:eng env) 600))
          (is (= ["set-place" "set-spawn"] (mapv #(last (re-find #"\.([a-z-]+)$" %)) (take-last 2 (names env)))) "restored on the cut")
          (is (= [(xyz bed) old-bed] (mapv :bed (called env 'jobs.survival.set-spawn)))))))))
