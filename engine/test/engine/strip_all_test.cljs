(ns engine.strip-all-test
  "jobs.strip.all and its siblings (base, smelt, haul) over engine.strip-site-test's fake site: the children of the leaf jobs are recording stubs."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.strip-site-test :as st :refer [args bed chest-1 chest-3 furnace-1 furnace-2 old-bed old-home xyz setup start-job! names called ending-job]]
            [engine.test-util :as tu]
            [jobs.lib.stock :as stock]))

(def all 'jobs.strip.all)
(defn start! [env a] (start-job! env all a))
(defn short-names [env] (mapv #(last (re-find #"\.([a-z-]+)$" %)) (names env)))
(def ore-bag [{:name "torch" :count 32} {:name "iron_pickaxe" :count 1} {:name "raw_iron" :count 16} {:name "coal" :count 4}])

(deftest strips-every-segment-then-banks-and-hauls-the-base-chests-and-restores-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup)
              r (await (start! env args))]
          (is (= :done (:status r)))
          (is (= {:sealed 2 :stripped 1} (:segments r)))
          (is (= ["from-plan" "set-spawn" "set-place" "get-tool" "corridor" "corridor" "seal" "corridor" "seal" "deposit" "haul" "set-place" "set-spawn"]
                 (short-names env)) "base, shears, strip, bank, haul, then home and spawn back")
          (is (= {:from [chest-1 chest-3]} (select-keys (first (called env 'jobs.storage.haul)) [:from])))
          (is (= [{:name "home" :pos (xyz bed)} {:name "home" :pos old-home}]
                 (mapv (comp #(select-keys % [:name :pos]) second) (filter #(= 'jobs.memory.set-place (first %)) @(:calls env)))))
          (is (= [(xyz bed) old-bed] (mapv :bed (called env 'jobs.survival.set-spawn)))))))))

(deftest a-site-that-ends-short-is-still-smelted-and-hauled-then-reports-its-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :results {'jobs.strip.corridor {:status :stopped :reason :refused}
                                   'jobs.strip.seal {:status :stopped :reason :refused}})
              r (await (start! env args))]
          (is (= [:stopped :all-skipped] ((juxt :status :reason) r)))
          (is (= 1 (count (called env 'jobs.storage.haul))) "the base chests are hauled")
          (is (= ["set-place" "set-spawn"] (take-last 2 (short-names env)))))))))

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

(deftest a-failed-site-child-stops-with-its-reason-and-puts-home-and-spawn-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :results {'jobs.strip.seal {:status :stopped :reason :no-blocks}})
              r (await (start! env args))]
          (is (= {:status :stopped :reason :child-failed :child "jobs.strip.seal" :because :no-blocks :phase :site}
                 (select-keys r [:status :reason :child :because :phase])))
          (is (= ["set-place" "set-spawn"] (take-last 2 (short-names env))) "restored on the stop")
          (is (empty? (called env 'jobs.storage.haul)) "no haul after a failed strip"))))))

(deftest a-site-stop-passes-its-own-reason-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :inventory [{:name "iron_pickaxe" :count 1}])
              r (await (start! env args))]
          (is (= {:status :stopped :reason :no-torches :phase :site} (select-keys r [:status :reason :phase]))))))))

(deftest an-unknown-base-plan-stops-bad-plan-before-anything-is-built
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup)
              r (await (start! env (assoc args :base "no-such-base")))]
          (is (= [:condition-failed :bad-plan :base] [(:kind (:ending r)) (:reason r) (:phase r)]))
          (is (empty? (called env 'jobs.build.from-plan))))))))

(deftest a-night-set-spawn-is-waited-out-not-a-child-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :ending-jobs {'jobs.survival.set-spawn (ending-job 'jobs.survival.set-spawn :condition-failed {:reason :night})})]
          (is (= :not-done (await (start! env args))) "the base phase yields while it is night")
          (is (empty? (called env 'jobs.strip.corridor))))))))

(deftest a-haul-that-waits-for-light-is-waited-out-not-a-child-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :ending-jobs {'jobs.storage.haul (ending-job 'jobs.storage.haul :condition-failed {:reason :exposed})})]
          (is (= :not-done (await (start! env args))) "the haul phase yields while the chest is in the dark"))))))

(deftest a-cut-run-puts-home-and-spawn-back-in-its-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [eng-at (atom nil)
              env (setup :results {'jobs.strip.corridor (fn [_] (core/cancel! @eng-at (first (:list (core/state @eng-at)))) {:status :done :taken {} :new-segments []})})
              _ (reset! eng-at (:eng env))]
          (core/submit! (:eng env) (list all args) {})
          (await (tu/run-until-empty (:eng env) 600))
          (is (= ["set-place" "set-spawn"] (take-last 2 (short-names env))) "restored on the cut")
          (is (= [(xyz bed) old-bed] (mapv :bed (called env 'jobs.survival.set-spawn)))))))))

(deftest carried-ore-is-loaded-in-both-furnaces-collected-and-banked-before-the-haul
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :inventory ore-bag)
              r (await (start! env args))
              smelts (called env 'jobs.items.smelt)]
          (is (= :done (:status r)))
          (is (= [{:furnace (xyz furnace-1) :item "raw_iron" :count 8} {:furnace (xyz furnace-2) :item "raw_iron" :count 8}]
                 (mapv #(select-keys % [:furnace :item :count]) (take 2 smelts))) "both furnaces loaded, half each")
          (is (= ["smelt" "smelt" "smelt" "smelt" "deposit" "haul"] (subvec (short-names env) 9 15)) "loaded, collected, then banked, then hauled")
          (is (= (take 2 smelts) (drop 2 smelts)) "the same args resume the same children")
          (is (= {"raw_iron" 16} (:smelted r))))))))

(deftest raw-ore-the-base-chests-hold-is-withdrawn-before-it-is-loaded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup)
              _ (mem/write! (:store (:eng env)) stock/stock-kind {:pos chest-1 :items {"raw_iron" 20 "stone" 9}} stock/stock-policy)
              _ (swap! (:results env) assoc 'jobs.storage.withdraw (fn [_] (fake/add-item! (:p env) "raw_iron" 20) {:gave-up false}))
              r (await (start! env args))]
          (is (= :done (:status r)))
          (is (= [{:chest (xyz chest-1) :items {"raw_iron" 20}}] (mapv #(select-keys % [:chest :items]) (called env 'jobs.storage.withdraw))))
          (is (= ["withdraw" "smelt" "smelt"] (subvec (short-names env) 9 12)) "taken out, then loaded in both furnaces")
          (is (= [10 10] (mapv :count (take 2 (called env 'jobs.items.smelt))))))))))

(deftest a-smelt-that-gives-up-does-not-stop-the-run-and-is-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup :inventory ore-bag :fail-smelt "no-fuel")
              r (await (start! env args))]
          (is (= :done (:status r)))
          (is (= ["no-fuel" "no-fuel"] (mapv :reason (:smelt-failed r))))
          (is (= 2 (count (called env 'jobs.items.smelt))) "once per furnace, no retry loop")
          (is (= 1 (count (called env 'jobs.storage.haul))) "the haul still runs"))))))

(deftest the-strips-worth-is-the-worth-of-its-base-build
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (setup)]
          (await (start! env (assoc args :worth 77)))
          (is (= 77 (:worth (first (called env 'jobs.build.from-plan))))))))))

(deftest the-siblings-run-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [base (setup)
              _ (await (start-job! base 'jobs.strip.base {:base "base"}))
              haul (setup)
              hr (await (start-job! haul 'jobs.strip.haul {:base "base"}))
              smelt (setup :inventory ore-bag)
              sr (await (start-job! smelt 'jobs.strip.smelt {:base "base"}))]
          (is (= ["from-plan" "set-spawn" "set-place"] (short-names base)))
          (is (= ["deposit" "haul"] (short-names haul)))
          (is (= :done (:status hr)))
          (is (= {"raw_iron" 16} (:smelted sr))))))))
