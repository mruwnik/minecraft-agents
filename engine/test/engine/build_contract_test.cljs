(ns engine.build-contract-test
  "jobs.blocks.clear-cells and jobs.build.path as contract jobs: they export the contract and end with a kind and a cause
  (0 of N is never done; a partial run carries its counts)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.build-from-plan-test :as bfp]
            [engine.harvest-test :as h]
            [engine.clear-cells-test :as cbt]
            [engine.contract :as contract]
            [engine.farm-contract-test :as fct :refer [ending-of kc]]
            [engine.path-test :as pt]
            [engine.core :as core]
            [engine.event-api :as event-api]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.world-files :as ew]))

(defn bad-args [e] [(:kind e) (get-in e [:cause :reason])])

(def from-plan 'jobs.build.from-plan)

(def jobs '[jobs.blocks.clear-cells jobs.build.path jobs.build.from-plan])

(deftest every-build-job-exports-the-contract-and-quotes-validly
  (let [p (tu/fake {:inventory pt/shovel})
        args {'jobs.blocks.clear-cells cbt/box 'jobs.build.path pt/box 'jobs.build.from-plan {:plan "pen"}}]
    (doseq [sym jobs
            :let [job (get registry/jobs sym)]]
      (is (some? (:can-run? job)) (str sym " has can-run?"))
      (is (some? (:can-start? job)) (str sym " has can-start?"))
      (is (vector? (:target job)) (str sym " has a target"))
      (when (not= sym from-plan) ; its quote reads the judged plan: the ending tests run it
        (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :view (constantly {:now 0 :data {:entries {}}}) :args (get args sym)}))) (str sym))))))

;; ------------------------------------------------------------------ clear-cells

(deftest clear-cells-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (cbt/setup {:blocks cbt/eight}) cbt/job cbt/box 40))]
          (is (= :done (:kind e)))
          (is (= {:dug 8 :skipped {} :kept 0 :fluids {}} (dissoc (:data e) :text))))
        (is (= {:kind :done :cause :already} (kc (await (ending-of (cbt/setup {}) cbt/job cbt/one 10)))) "an empty box: nothing to do")
        (let [e (await (ending-of (cbt/setup {:blocks {"1,64,1" "bedrock"}}) cbt/job cbt/one 10))]
          (is (= {:kind :condition-failed :cause :cannot} (kc e)) "nothing dug, the cell can never be dug")
          (is (= 0 (get-in e [:data :dug]))))
        (let [e (await (ending-of (cbt/setup {:blocks {"9,55,1" "dirt"}}) cbt/job cbt/one-cell 20))]
          (is (= {:kind :nothing-possible :cause :unreachable} (kc e))))))))

(deftest clear-cells-start-facts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [:impossible :bad-args] (bad-args (await (ending-of (cbt/setup {}) cbt/job {} 5)))))
        (is (= [:impossible :bad-args]
               (bad-args (await (ending-of (cbt/setup {}) cbt/job {:area {:min {:x 0 :y 0 :z 0} :max {:x 7 :y 7 :z 7}}} 5)))) "over 400 cells")
        (is (= [:condition-failed :no-zones]
               (bad-args (await (ending-of (cbt/setup {:blocks cbt/eight} (ew/of-data {} {} nil)) cbt/job cbt/box 5)))))
        (is (= :refused (get-in (await (ending-of (cbt/setup {:blocks cbt/eight} (ew/of-data {"pad" cbt/pad-plan} {} [cbt/farm-zone]))
                                                  cbt/job cbt/box 5))
                                [:cause :reason])))))))

;; ------------------------------------------------------------------ path

(def path 'jobs.build.path)

(deftest path-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (pt/setup {:inventory pt/shovel :blocks pt/four}) path pt/box 10))]
          (is (= :done (:kind e)))
          (is (= {:pathed 4 :skipped {}} (dissoc (:data e) :text))))
        (let [e (await (ending-of (pt/setup {:inventory pt/shovel :blocks (assoc pt/four "1,64,1" "cobblestone")}) path pt/box 10))]
          (is (= {:kind :done :cause nil} (kc e)) "a partial run carries its counts"))
        (let [e (await (ending-of (pt/setup {:inventory pt/shovel :blocks {"1,63,1" "stone"}}) path
                                  {:from {:x 1 :y 63 :z 1} :to {:x 1 :y 63 :z 1}} 10))]
          (is (= {:kind :condition-failed :cause :not-pathable} (kc e)) "nothing pathed")
          (is (= 0 (get-in e [:data :pathed]))))
        (let [e (await (ending-of (pt/setup {:inventory pt/shovel :blocks {"5,63,0" "dirt"} :unreachable ["5,63,0"]}) path
                                  {:from {:x 5 :y 63 :z 0} :to {:x 5 :y 63 :z 0}} 30))]
          (is (= :nothing-possible (:kind e))))))))

(deftest path-start-facts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [:impossible :bad-args]
               (bad-args (await (ending-of (pt/setup {:inventory pt/shovel}) path {:from {:x 0 :y 63 :z 0}} 5)))))
        (is (= [:condition-failed :no-tool]
               (bad-args (await (ending-of (pt/setup {:blocks pt/four}) path (assoc pt/box :ways #{:carried}) 5)))))
        (is (= [:condition-failed :no-zones]
               (bad-args (await (ending-of (pt/setup {:inventory pt/shovel :blocks pt/four} (ew/of-data {} {} nil)) path pt/box 5)))))
        (is (= :refused (get-in (await (ending-of (pt/setup {:inventory pt/shovel :blocks pt/four}
                                                            (ew/of-data {} {} [(pt/zone [1 60 1] [2 70 2])]))
                                                  path pt/box 5))
                                [:cause :reason])))))))

(deftest path-with-no-shovel-and-nothing-to-fetch-ends-with-the-failed-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (pt/setup {:blocks pt/four}) path pt/box 80))]
          (is (= :condition-failed (:kind e)))
          (is (= {:reason :no-tool :kind "shovel"} (:cause e)))
          (is (= 0 (get-in e [:data :pathed]))))))))

(deftest path-uses-only-the-shovel-the-call-lists
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (pt/setup {:inventory [{:name "stone_shovel" :count 1} {:name "diamond_shovel" :count 1}] :blocks pt/four})
              e (await (ending-of env path (assoc pt/box :use {:tools ["diamond_shovel"]}) 30))]
          (is (= :done (:kind e)))
          (is (= #{"diamond_shovel"} (set (map #(.-item (.-args %)) (pt/calls (:p env) "useOn"))))))))))

;; ------------------------------------------------------------------ from-plan

(defn plan-ending
  "The ending of from-plan with args over the fake world spec and plans (zones default none), at most 300 ticks."
  ([spec plans args] (plan-ending spec plans args []))
  ([spec plans args zones] (ending-of (bfp/start spec plans zones) from-plan args 300)))

(deftest from-plan-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (plan-ending {:inventory bfp/kit} {"pen" (bfp/pen-plan)} {:plan "pen"}))]
          (is (= :done (:kind e)))
          (is (= 9 (get-in e [:data :placed]))))
        (let [e (await (plan-ending {:inventory bfp/kit} {"pen" (bfp/pen-plan)} {:plan "pen" :least 1}))]
          (is (= :done (:kind e))))
        (let [e (await (plan-ending {:inventory [{:name "oak_fence" :count 4} {:name "oak_fence_gate" :count 1} {:name "torch" :count 1}]}
                                    {"pen" (bfp/pen-plan)} {:plan "pen" :ways #{:carried}}))]
          (is (= :condition-failed (:kind e)) "all wanted, some placed, material short: not done")
          (is (= {"oak_fence" 3} (get-in e [:cause :needs])))
          (is (= 3 (count (get-in e [:data :missing]))))
          (is (= 6 (get-in e [:data :placed]))))
        (let [e (await (plan-ending {:inventory [{:name "oak_fence" :count 4} {:name "oak_fence_gate" :count 1} {:name "torch" :count 1}]}
                                    {"pen" (bfp/pen-plan)} {:plan "pen" :ways #{:carried} :least 2}))]
          (is (= :done (:kind e)) "a partial run meets its :least"))))))

(deftest from-plan-with-nothing-placed-is-never-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "post" :cells [[40 64 0]] :want "oak_fence"}]}
              e (await (plan-ending {:inventory bfp/kit :unreachable (map #(apply h/cell-key %) (plan/stand-cells [40 64 0] 64 nil #{}))}
                                    {"far" plan} {:plan "far"}))]
          (is (= {:kind :nothing-possible :cause :incomplete} (kc e)))
          (is (= 0 (get-in e [:data :placed]))))))))

(deftest from-plan-quotes-the-cells-it-owes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (bfp/start {:inventory bfp/kit} {"pen" (bfp/pen-plan)})
              q (:quote (await (event-api/quote-view (:eng s) (str from-plan) (pr-str {:plan "pen"}))))]
          (is (nil? (contract/quote-problem q)) (pr-str q))
          (is (= [0 9 9] (:done q))))))))

(deftest from-plan-quote-of-a-built-gate-allows-it-to-read-unseen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (bfp/start {:inventory bfp/kit} {"pen" (bfp/pen-plan)})
              _ (await (bfp/child-outcome (:eng s) from-plan {:plan "pen"} 200))
              q (:quote (await (event-api/quote-view (:eng s) (str from-plan) (pr-str {:plan "pen"}))))]
          (is (= [0 0 1] (:done q))))))))

(deftest from-plan-start-facts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [:condition-failed :plan-trouble] (bad-args (await (plan-ending {:inventory bfp/kit} {} {:plan "nope"})))))
        (is (= [:impossible :plan-trouble] (bad-args (await (plan-ending {:inventory bfp/kit} {"pen" (bfp/pen-plan)} {:plan "pen" :part "nope"})))))
        (is (= [:condition-failed :no-zones]
               (bad-args (await (plan-ending {:inventory bfp/kit} {"pen" (bfp/pen-plan)} {:plan "pen"} nil)))))
        (let [e (await (plan-ending {:inventory []} {"pen" (bfp/pen-plan)} {:plan "pen" :ways #{:carried}}))]
          (is (= :condition-failed (:kind e)))
          (is (= {:reason :no-items :needs {"oak_fence" 7 "oak_fence_gate" 1 "torch" 1}}
                 (select-keys (:cause e) [:reason :needs]))))))))

(deftest from-plan-quotes-the-items-it-places
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (bfp/start {:inventory bfp/kit} {"pen" (bfp/pen-plan)})
              q (:quote (await (event-api/quote-view (:eng s) (str from-plan) (pr-str {:plan "pen"}))))
              items (get-in q [:uses :items])]
          (is (= {"oak_fence" 7 "oak_fence_gate" 1 "torch" 1} (update-vals items second))))))))

(deftest clear-cells-quotes-the-wear-of-the-tool-it-digs-with
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (cbt/setup {:blocks cbt/eight :inventory [{:name "iron_shovel" :count 1} {:name "stone_pickaxe" :count 1}]})
              q (:quote (await (event-api/quote-view eng "jobs.blocks.clear-cells" (pr-str cbt/box))))]
          (is (= {"iron_shovel" [4 4 4] "stone_pickaxe" [4 4 4]} (get-in q [:uses :wear]))))))))
