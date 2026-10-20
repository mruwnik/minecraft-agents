(ns engine.farm-contract-test
  "jobs.farm.till, fertilize, compost, find-spot, plant and harvest as contract jobs: they export the contract and end with a kind and a
  cause (0 of N is never done; a partial run carries its counts)."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.compost-test :as ct]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fertilize-test :as ft]
            [engine.find-spot-test :as fst]
            [engine.harvest-test :as ht]
            [engine.plant-test :as pt]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.till-test :as tt]
            [jobs.lib.look :as look]
            [jobs.lib.world-files :as ew]))

(def jobs '[jobs.farm.till jobs.farm.fertilize jobs.farm.compost jobs.farm.find-spot jobs.farm.plant jobs.farm.harvest])

(deftest every-farm-job-exports-the-contract-and-quotes-validly
  (let [p (tu/fake {:inventory tt/hoe})
        args {'jobs.farm.till tt/box 'jobs.farm.fertilize {:max 16 :radius 8} 'jobs.farm.compost {:times 1} 'jobs.farm.find-spot {:w 3 :h 3 :range 6}
              'jobs.farm.plant {:box pt/field-box} 'jobs.farm.harvest {:radius 6}}]
    (doseq [sym jobs
            :let [job (get registry/jobs sym)]]
      (is (vector? (:target job)) (str sym " has a target"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args (get args sym)}))) (str sym)))))

(deftest farm-jobs-default-a-time-spend
  (doseq [sym '[jobs.farm.till jobs.farm.plant jobs.farm.fertilize jobs.farm.compost jobs.farm.find-spot jobs.farm.tend]]
    (is (some? (get-in (:defaults (get registry/jobs sym)) [:spend :job-time])) (str sym))))

(defn ^:async ending-of
  "Run job with args as a ctx/call child of a recording parent, at most n ticks: the child's {:kind :cause :data} (nil
  when it does not end)."
  [{:keys [eng]} job args n]
  (let [out (atom nil)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data])) (ctx/end c :done nil)))))}
        eng (assoc-in eng [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng n))
    @out))

(defn kc "Kind and cause keyword (nil cause stays nil) of an ending." [e] (select-keys e [:kind :cause]))
(defn kcm "Kind and cause map without its text and parts." [e] (update (select-keys e [:kind :cause]) :cause dissoc :text :parts))

;; ------------------------------------------------------------------ till

(def till 'jobs.farm.till)

(deftest till-tilling-some-is-done-with-its-counts-and-tilling-none-is-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (tt/setup {:inventory tt/hoe :blocks (assoc tt/four "1,64,1" "cobblestone")}) till tt/box 30))]
          (is (= :done (:kind e)))
          (is (= {:tilled 3 :skipped {{:x 1 :y 63 :z 1} :covered}} (dissoc (:data e) :text)) "a partial run carries its counts"))
        (doseq [[world args kind reason] [[{:inventory tt/hoe :blocks {"1,63,1" "stone"}} {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}}
                                           :condition-failed :not-tillable]
                                          [{:inventory tt/hoe :blocks {"5,63,0" "dirt"} :unreachable ["5,63,0"]}
                                           {:from {:x 5 :y 63 :z 0} :to {:x 5 :y 63 :z 0}} :nothing-possible :unreachable]]]
          (let [e (await (ending-of (tt/setup world) till args 30))]
            (is (= {:kind kind :cause reason} (kc e)))
            (is (= 0 (get-in e [:data :tilled])))))))))

(deftest till-refused-before-it-starts-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :condition-failed :cause {:reason :no-zones}}
               (kcm (await (ending-of (tt/setup {:inventory tt/hoe :blocks tt/four} (ew/of-data {} {} nil)) till tt/box 5)))))
        (is (= {:kind :condition-failed :cause {:reason :no-tool :kind "hoe"}}
               (kcm (await (ending-of (tt/setup {:blocks tt/four}) till (assoc tt/box :fetch false) 5)))))
        (is (= :bad-args (get-in (await (ending-of (tt/setup {:inventory tt/hoe}) till {:from {:x 0 :y 63 :z 0}} 5)) [:cause :reason])))))))

(deftest till-with-no-hoe-and-nothing-to-fetch-ends-with-the-failed-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (tt/setup {:blocks tt/four}) till tt/box 80))]
          (is (not= :done (:kind e)))
          (is (= {:reason :tool :kind "hoe"} (select-keys (:cause e) [:reason :kind])))
          (is (= 0 (get-in e [:data :tilled]))))))))

(deftest till-uses-only-the-hoe-the-call-lists
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (tt/setup {:inventory [{:name "stone_hoe" :count 1} {:name "diamond_hoe" :count 1}] :blocks tt/four})
              e (await (ending-of env till (assoc tt/box :use {:tools ["diamond_hoe"]}) 30))]
          (is (= :done (:kind e)))
          (is (= #{"diamond_hoe"} (set (map #(.-item (.-args %)) (tt/calls (:p env) "useOn"))))))))))

(deftest till-over-cells-that-are-all-farmland-is-done-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (tt/setup {:inventory tt/hoe :blocks (zipmap (keys tt/four) (repeat "farmland"))})
              e (await (ending-of env till tt/box 20))]
          (is (= {:kind :done :cause :already} (kc e)))
          (is (empty? (tt/calls (:p env) "useOn"))))))))

(deftest till-partial-run-is-done-only-when-it-meets-least
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:inventory tt/hoe :blocks (assoc tt/four "1,64,1" "cobblestone")}]
          (doseq [[least kind cause] [[3 :done nil] [4 :nothing-possible :partial] [:all :nothing-possible :partial]]]
            (let [e (await (ending-of (tt/setup world) till (assoc tt/box :least least) 30))]
              (is (= {:kind kind :cause cause} (kc e)) (str ":least " least))
              (is (= 3 (get-in e [:data :tilled])))))
          (is (some? (contract/call-problem till nil (assoc tt/box :least 0))) ":least 0 is no call, so 0 tilled is never enough"))))))

;; ------------------------------------------------------------------ fertilize

(def fertilize 'jobs.farm.fertilize)

(deftest fertilize-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (ft/setup ft/two-wheat) fertilize {} 30))]
          (is (= :done (:kind e)))
          (is (= {:used 4} (dissoc (:data e) :text))))
        (is (= {:kind :done :cause nil :data {:used 0}}
               (update (await (ending-of (ft/setup {:inventory ft/meal :blocks {"2,64,0" "wheat"} :ages {"2,64,0" 7}}) fertilize {} 10)) :data dissoc :text))
            "nothing unripe: nothing to do")
        (let [env (ft/setup {:inventory ft/meal :blocks {"2,64,0" "wheat"} :ages {"2,64,0" 3}})]
          (.override (.-world (:p env)) "useOn" (fn ^:async f [_ _ _] #js {:status "unchanged" :consumed 0}))
          (is (= :condition-failed (:kind (await (ending-of env fertilize {} 10)))) "an unripe crop, none fertilized"))
        (is (= {:kind :condition-failed :cause {:reason :need :item "bone_meal"}}
               (kcm (await (ending-of (ft/setup {:blocks {"2,64,0" "wheat"} :ages {"2,64,0" 3}}) fertilize {:fetch false} 5)))))
        (is (= {:kind :condition-failed :cause {:reason :no-zones}}
               (kcm (await (ending-of (ft/setup ft/two-wheat (ew/of-data {} {} nil)) fertilize {} 5)))))))))

(deftest fertilize-partial-run-is-done-only-when-it-meets-least
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc ft/two-wheat :inventory [{:name "bone_meal" :count 1}])]
          (doseq [[least kind cause] [[1 :done nil] [2 :condition-failed {:reason :need :item "bone_meal" :count 1}] [:all :condition-failed {:reason :need :item "bone_meal" :count 1}]]]
            (let [e (await (ending-of (ft/setup world) fertilize {:least least :fetch false} 30))]
              (is (= {:kind kind :cause cause} (kc e)) (str ":least " least))
              (is (= 1 (get-in e [:data :used]))))))))))

;; ------------------------------------------------------------------ compost

(def compost 'jobs.farm.compost)

(deftest compost-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (ct/setup {:inventory (ct/inv "wheat_seeds" 10) :blocks ct/comp-block}) compost
                                  {:at ct/at :items ["wheat_seeds"]} 40))]
          (is (= :done (:kind e)))
          (is (= 1 (get-in e [:data :bone-meal]))))
        (is (= {:kind :condition-failed :cause {:reason :no-composter}}
               (kcm (await (ending-of (ct/setup {:inventory (ct/inv "oak_leaves" 4)}) compost {} 10)))))
        (is (= {:kind :condition-failed :cause {:reason :nothing-to-feed}}
               (kcm (await (ending-of (ct/setup {:inventory (ct/inv "stick" 4) :blocks ct/comp-block}) compost {:at ct/at} 10)))))))))

(deftest compost-stopped-early-is-done-only-when-it-meets-least
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[least kind cause] [[1 :done nil] [2 :condition-failed :nothing-to-feed]]]
          (let [e (await (ending-of (ct/setup {:inventory (ct/inv "wheat_seeds" 10) :blocks ct/comp-block}) compost
                                    {:at ct/at :items ["wheat_seeds"] :times 2 :least least} 60))]
            (is (= {:kind kind :cause cause} (kc e)) (str ":least " least))
            (is (= 1 (get-in e [:data :bone-meal])))))))))

;; ------------------------------------------------------------------ find-spot

(def find-spot 'jobs.farm.find-spot)

(deftest find-spot-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :nothing-possible :cause :none} (kc (await (ending-of (fst/setup {:blocks {}}) find-spot fst/small 4)))))
        (let [e (await (ending-of (fst/setup {:blocks (fst/patch 4 0 3 3 63 "dirt")}) find-spot (assoc fst/small :range 8) 4))]
          (is (= :done (:kind e)))
          (is (= {:x 4 :y 63 :z 0} (get-in e [:data :spot]))))
        (let [e (await (ending-of (fst/setup {:blocks (fst/patch 4 0 3 3 63 "dirt") :unreachable ["4,64,0"]}) find-spot
                                  (assoc fst/small :range 8 :walk true) 4))]
          (is (= {:kind :nothing-possible :cause :unreachable} (kc e)))
          (is (= {:x 4 :y 63 :z 0} (get-in e [:data :spot])) "the spot is still handed over"))))))

;; ------------------------------------------------------------------ plant

(def plant 'jobs.farm.plant)

(deftest plant-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (pt/start (pt/field-world (pt/inv "wheat_seeds" 9))) plant {:box pt/field-box} 100))]
          (is (= {:kind :done :cause nil} (kc e)))
          (is (= {:planted 9 :skipped []} (dissoc (:data e) :text))))
        (is (= {:kind :condition-failed :cause {:reason :nothing-to-do}}
               (kcm (await (ending-of (pt/start (pt/field-world (pt/inv "wheat_seeds" 9))) plant {:box (pt/box 8 8 9 9)} 20)))))
        (is (= {:kind :condition-failed :cause {:reason :need :any-of ["beetroot_seeds" "carrot" "potato" "wheat_seeds"] :count 9}}
               (kcm (await (ending-of (pt/start (pt/field-world [])) plant {:box pt/field-box :fetch false} 20)))))
        (is (= {:kind :condition-failed :cause {:reason :no-zones}}
               (kcm (await (ending-of (pt/start (pt/field-world (pt/inv "wheat_seeds" 9)) (ew/of-data {} {} nil)) plant {:box pt/field-box} 20)))))))))

(deftest plant-without-a-box-or-plan-is-never-bad-args
  (let [c {:primitives (tu/fake {}) :args {}}]
    (is (= [:never :bad-args] (tu/kind-reason ((:can-start? (get registry/jobs plant)) c))))))

(deftest a-field-with-no-bare-cell-is-a-look-until-the-body-has-looked-round
  (let [c {:primitives (tu/fake {:blocks {}}) :args {:box pt/field-box :ignore-zones? true}}
        status (fn [surveyed?] (with-redefs [ctx/mem (constantly {}) look/surveyed? (fn ([_] surveyed?) ([_ _] surveyed?))]
                                 ((:can-start? (get registry/jobs plant)) c)))]
    (is (= [:look :nothing-to-do] (tu/kind-reason (status false))))
    (is (= [:later :nothing-to-do] (tu/kind-reason (status true))))))

(deftest plant-done-is-a-status-with-the-counts
  (let [c {:primitives (tu/fake {}) :args {:box pt/field-box}}
        [kind _ data] (with-redefs [ctx/mem (constantly {:planted 2})] ((:done? (get registry/jobs plant)) c))]
    (is (= [:later :cells 2] [kind (:units data) (:done data)]))))

(deftest plant-with-no-bare-cell-after-the-look-is-nothing-to-do-and-names-dirt-that-needs-tilling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake (pt/field-world (pt/inv "wheat_seeds" 9) {:blocks {}}))
              _ (pt/look-then-see p)
              env (pt/start-on p)
              e (await (ending-of env plant {:box pt/field-box} 20))]
          (is (seq (pt/calls p "look")))
          (is (= {:kind :condition-failed :cause {:reason :nothing-to-do}} (kcm e))))
        (let [e (await (ending-of (pt/start (pt/field-world (pt/inv "wheat_seeds" 9)
                                                           {:blocks (into {} (for [x (range 2 5) z (range 2 5)] [(str x ",63," z) "dirt"]))}))
                                  plant {:box pt/field-box} 20))]
          (is (= {:kind :condition-failed :cause {:reason :nothing-to-do :untilled 9}} (kcm e))))))))

(deftest plant-a-carried-seed-the-food-reserve-keeps-is-a-failed-condition-and-nothing-is-fetched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as env} (pt/start (pt/field-world (pt/inv "carrot" 6)))
              e (await (ending-of env plant {:box pt/field-box :seed "carrot"} 20))]
          (is (= {:kind :condition-failed :cause {:reason :food-reserve :seed "carrot" :held 6 :kept 6 :reserve-points 60}} (kcm e)))
          (is (empty? (filter #(= :need (:slot %)) (pt/events-of seen :child_started))))
          (is (= {"carrot" 6} (pt/inv-of p))))))))

(deftest plant-every-cell-skipped-is-nothing-possible-and-fewer-seeds-than-least-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (pt/start (pt/field-world (pt/inv "wheat_seeds" 9)))]
          (.override (.-world (:p env)) "place" (fn ^:async f [_ _ _] #js {:status "failed" :reason "refused"}))
          (let [e (await (ending-of env plant {:box pt/field-box} 100))]
            (is (= {:kind :nothing-possible :cause :gave-up} (kc e)))
            (is (= 0 (get-in e [:data :planted])))
            (is (= 9 (count (get-in e [:data :skipped]))))))
        (doseq [[least kind cause] [[5 :done :done] [6 :condition-failed :need] [:all :condition-failed :need]]]
          (let [e (await (ending-of (pt/start (pt/field-world (pt/inv "carrot" 5 "bread" 12))) plant {:box pt/field-box :least least :fetch false} 100))]
            (is (= [kind cause] [(:kind e) (contract/cause-key e)]) (str ":least " least))
            (is (= 5 (get-in e [:data :planted])))))
        (doseq [[least kind cause] [[9 :done nil] [:all :done nil]]]
          (let [e (await (ending-of (pt/start (pt/field-world (pt/inv "wheat_seeds" 9))) plant {:box pt/field-box :least least} 100))]
            (is (= {:kind kind :cause cause} (kc e)) (str "nothing left, :least " least))
            (is (= 9 (get-in e [:data :planted])))))))))

;; ------------------------------------------------------------------ harvest

(def harvest 'jobs.farm.harvest)

(deftest harvest-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (ht/start {:world ht/wheat-world}) harvest {} 200))]
          (is (= {:kind :done :cause nil} (kc e)))
          (is (= {:cut 9 :replanted 9 :bare [] :lost [] :gave-up false} (dissoc (:data e) :text :spent :spent-at))))
        (let [cells (for [x (range 8 13)] (ht/cell-key x 64 0))
              e (await (ending-of (ht/start {:world {:blocks (zipmap cells (repeat "wheat")) :ages (zipmap cells (repeat 7)) :unreachable cells}})
                                  harvest {:radius 14} 400))]
          (is (= {:kind :nothing-possible :cause :gave-up} (kc e)))
          (is (= 0 (get-in e [:data :cut]))))
        (let [p (tu/fake-on-floor {:blocks {}})
              _ (ht/look-then-see p)
              e (await (ending-of (ht/start {:p p}) harvest {} 50))]
          (is (= {:kind :condition-failed :cause :no-crop-seen} (kc e))))
        (is (= {:kind :condition-failed :cause {:reason :no-zones}}
               (kcm (await (ending-of (ht/start {:world ht/zoned-world :shared (ew/of-data {} {} nil)}) harvest {:radius 6} 20)))))
        (is (= {:kind :condition-failed :cause {:reason :refused}}
               (kcm (await (ending-of (ht/start {:world ht/zoned-world :shared (ew/of-data {} {} [(ht/zone-over-crop "Miles")])}) harvest {:radius 6} 20)))))
        (is (= {:kind :done :cause :already}
               (kc (await (ending-of (ht/start {:world {:blocks (ht/field "wheat" 3 [2 3] [2]) :ages (ht/ages 3 [2 3] [2])}}) harvest {} 20)))))))))

(deftest harvest-partial-run-is-done-only-when-it-meets-least
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[least kind cause] [[9 :done nil] [:all :done nil]]]
          (let [e (await (ending-of (ht/start {:world ht/wheat-world}) harvest {:least least} 200))]
            (is (= {:kind kind :cause cause} (kc e)) (str ":least " least))
            (is (= 9 (get-in e [:data :cut])))))))))

(deftest harvest-quote-counts-the-seeds-it-replants
  (let [p (tu/seeing-all (tu/fake-on-floor ht/wheat-world))
        quote-for (fn [args] ((:quote-of (get registry/jobs harvest)) {:primitives p :args (merge (ht/plain-args {}) args)}))]
    (is (= {"wheat_seeds" [0 9 9]} (:items (:uses (quote-for {})))))
    (is (nil? (:items (:uses (quote-for {:replant false})))))
    (are [least d cause] (= cause (contract/least-cause {:least least} (quote-for {}) {:left d}))
      9 9 nil
      10 9 {:reason :least :least 10 :most 9}
      :all 9 nil
      :all 10 {:reason :least :least :all :most 9})))
