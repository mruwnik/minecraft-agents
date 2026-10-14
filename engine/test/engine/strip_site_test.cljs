(ns engine.strip-site-test
  "jobs.strip.site over the fake world: every child is a recording stub, the corridors are the T-shaped site of
  engine.strip-test (stem z-27-0:0..20 into x-27-20:-20..0 and x-27-20:0..20)."
  (:require [cljs.reader :as reader]
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

(defn stub-of [real round-fn]
  (assoc (dissoc real :conditions :target :quote-of) :check (constantly true)
         :round (fn ^:async stub-round [c] (round-fn c) :done)))

(defn done [result] (fn [_] result))

(def furnace-1 [5 65 102])
(def furnace-2 [5 65 103])

(defn smelt-stub
  "jobs.items.smelt over a furnaces atom {furnace {:phase :cooking|:ready :item :count}}: the first call loads and yields, the
  check declines :cooking until the furnace is :ready, then the next call gives the ingots; :fail makes it stop with that reason."
  [p calls furnaces fail]
  (let [key-of (comp pr-str :furnace :args)]
    (assoc (dissoc (get registry/jobs 'jobs.items.smelt) :conditions :target :done? :quote-of)
           :check (fn [c] (or (not= :cooking (:phase (get @furnaces (key-of c)))) (do (ctx/wait c {:reason :cooking}) false)))
           :round (fn ^:async smelt-round [c]
                    (swap! calls conj ['jobs.items.smelt (:args c)])
                    (let [{:keys [furnace item count]} (:args c)
                          state (get @furnaces (key-of c))]
                      (cond
                        fail (do (ctx/result! c {:status :stopped :reason fail}) :done)
                        (nil? state) (do (swap! (fake/state p) update :inventory (fn [inv] (mapv #(if (= item (:name %)) (update % :count - count) %) inv)))
                                         (swap! furnaces assoc (key-of c) {:phase :cooking :item item :count count})
                                         :continue)
                        :else (do (fake/add-item! p "iron_ingot" (:count state))
                                  (swap! furnaces dissoc (key-of c))
                                  (ctx/result! c {:smelted (:count state) :wanted (:count state)})
                                  :done)))))))

(defn ready! [furnaces] (swap! furnaces update-vals #(assoc % :phase :ready)))

(defn setup
  "An engine over the site with every child of the job stubbed. results: job symbol -> (fn [ctx]) giving the child's result,
  the default being success."
  [& {:keys [results inventory dir pos plans fail-smelt]}]
  (let [results (atom results)
        furnaces (atom {})
        p (tu/seeing-all (tu/fake {:self {:pos (or pos {:x 0 :y -27 :z 10})} :blocks (cond-> site-blocks pos (assoc "0,63,100" "stone" "12,63,100" "stone" "12,59,100" "stone"))
                                   :inventory (or inventory [{:name "torch" :count 32} {:name "iron_pickaxe" :count 1}])}))
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
                  'jobs.items.obtain {:status :done}}
        stubs (into {} (for [[sym res] defaults]
                         [sym (stub-of (get registry/jobs sym)
                                       (fn [c]
                                         (swap! calls conj [sym (:args c)])
                                         (let [r (let [r (get @results sym res)] (if (fn? r) (r (:args c)) r))]
                                           (when (and (= 'jobs.items.get-tool sym) (not= :stopped (:status r))) (fake/add-item! p "shears" 1))
                                           (when (= 'jobs.access.stair sym) (swap! (fake/state p) assoc-in [:self :pos 1] (:y (:args c))))
                                           (ctx/result! c r))))]))
        go-to (stub-of (get registry/jobs 'jobs.movement.go-to)
                       (fn [c]
                         (let [pos (:pos (:args c))
                               [x y z] (if (map? pos) [(:x pos) (:y pos) (:z pos)] pos)]
                           (swap! (fake/state p) assoc-in [:self :pos] [x y z])
                           (ctx/result! c {:arrived true}))))
        clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        eng (core/create {:primitives p :jobs (assoc (merge registry/jobs stubs) 'jobs.movement.go-to go-to 'jobs.items.smelt (smelt-stub p calls furnaces fail-smelt))
                          :triggers triggers/all :dir (or dir (tu/tmp-dir)) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})
                          :world (world/of-data (merge {"base" plan} plans) {"field-base" blueprint} [])})]
    (mem/write! (:store eng) :home {:pos old-home} mem/place-policy)
    (mem/write! (:store eng) :spawn-bed {:pos old-bed} {:cap 1 :ttl :forever})
    {:eng eng :p p :calls calls :results results :seen seen :furnaces furnaces}))

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
