(ns engine.farm-contract-test
  "jobs.farm.till, fertilize, compost and find-spot as contract jobs: they export the contract and end with a kind and a
  cause (0 of N is never done; a partial run carries its counts)."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.compost-test :as ct]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fertilize-test :as ft]
            [engine.find-spot-test :as fst]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.till-test :as tt]
            [jobs.lib.world-files :as ew]))

(def jobs '[jobs.farm.till jobs.farm.fertilize jobs.farm.compost jobs.farm.find-spot])

(deftest every-farm-job-exports-the-contract-and-quotes-validly
  (let [p (tu/fake {:inventory tt/hoe})
        args {'jobs.farm.till tt/box 'jobs.farm.fertilize {:max 16 :radius 8} 'jobs.farm.compost {:times 1} 'jobs.farm.find-spot {:w 3 :h 3 :range 6}}]
    (doseq [sym jobs
            :let [job (get registry/jobs sym)]]
      (is (vector? (:conditions job)) (str sym " has conditions"))
      (is (nil? (:check job)) (str sym " has no old check"))
      (is (vector? (:target job)) (str sym " has a target"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args (get args sym)}))) (str sym)))))

(defn ^:async ending-of
  "Run job with args as a ctx/call child of a recording parent, at most n ticks: the child's {:kind :cause :data} (nil
  when it does not end)."
  [{:keys [eng]} job args n]
  (let [out (atom nil)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data])) :done))))}
        eng (assoc-in eng [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng n))
    @out))

(defn kc [e] (select-keys e [:kind :cause]))

;; ------------------------------------------------------------------ till

(def till 'jobs.farm.till)

(deftest till-tilling-some-is-done-with-its-counts-and-tilling-none-is-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (tt/setup {:inventory tt/hoe :blocks (assoc tt/four "1,64,1" "cobblestone")}) till tt/box 30))]
          (is (= :done (:kind e)))
          (is (= {:tilled 3 :skipped {{:x 1 :y 63 :z 1} :covered}} (:data e)) "a partial run carries its counts"))
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
               (kc (await (ending-of (tt/setup {:inventory tt/hoe :blocks tt/four} (ew/of-data {} {} nil)) till tt/box 5)))))
        (is (= {:kind :condition-failed :cause {:reason :no-tool :kind "hoe"}}
               (kc (await (ending-of (tt/setup {:blocks tt/four}) till (assoc tt/box :fetch false) 5)))))
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
          (is (= {:used 4} (:data e))))
        (is (= {:kind :done :cause nil :data {:used 0}}
               (await (ending-of (ft/setup {:inventory ft/meal :blocks {"2,64,0" "wheat"} :ages {"2,64,0" 7}}) fertilize {} 10)))
            "nothing unripe: nothing to do")
        (let [env (ft/setup {:inventory ft/meal :blocks {"2,64,0" "wheat"} :ages {"2,64,0" 3}})]
          (.override (.-world (:p env)) "useOn" (fn ^:async f [_ _ _] #js {:status "unchanged" :consumed 0}))
          (is (= :nothing-possible (:kind (await (ending-of env fertilize {} 10)))) "an unripe crop, none fertilized"))
        (is (= {:kind :condition-failed :cause {:reason :need :item "bone_meal"}}
               (kc (await (ending-of (ft/setup {:blocks {"2,64,0" "wheat"} :ages {"2,64,0" 3}}) fertilize {:fetch false} 5)))))
        (is (= {:kind :condition-failed :cause {:reason :no-zones}}
               (kc (await (ending-of (ft/setup ft/two-wheat (ew/of-data {} {} nil)) fertilize {} 5)))))))))

(deftest fertilize-partial-run-is-done-only-when-it-meets-least
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc ft/two-wheat :inventory [{:name "bone_meal" :count 1}])]
          (doseq [[least kind cause] [[1 :done nil] [2 :nothing-possible :need] [:all :nothing-possible :need]]]
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
        (is (= {:kind :condition-failed :cause :no-composter}
               (kc (await (ending-of (ct/setup {:inventory (ct/inv "oak_leaves" 4)}) compost {} 10)))))
        (is (= {:kind :condition-failed :cause :nothing-to-feed}
               (kc (await (ending-of (ct/setup {:inventory (ct/inv "stick" 4) :blocks ct/comp-block}) compost {:at ct/at} 10)))))))))

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
