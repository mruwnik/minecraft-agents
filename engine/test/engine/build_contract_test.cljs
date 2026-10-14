(ns engine.build-contract-test
  "jobs.build.clear-box and jobs.build.path as contract jobs: they export the contract and end with a kind and a cause
  (0 of N is never done; a partial run carries its counts)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.clear-box-test :as cbt]
            [engine.contract :as contract]
            [engine.farm-contract-test :as fct :refer [ending-of kc]]
            [engine.path-test :as pt]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as ew]))

(defn bad-args [e] [(:kind e) (get-in e [:cause :reason])])

(def jobs '[jobs.build.clear-box jobs.build.path])

(deftest every-build-job-exports-the-contract-and-quotes-validly
  (let [p (tu/fake {:inventory pt/shovel})
        args {'jobs.build.clear-box cbt/box 'jobs.build.path pt/box}]
    (doseq [sym jobs
            :let [job (get registry/jobs sym)]]
      (is (vector? (:conditions job)) (str sym " has conditions"))
      (is (nil? (:check job)) (str sym " has no old check"))
      (is (vector? (:target job)) (str sym " has a target"))
      (is (nil? (contract/quote-problem ((:quote-of job) {:primitives p :args (get args sym)}))) (str sym)))))

;; ------------------------------------------------------------------ clear-box

(deftest clear-box-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (cbt/setup {:blocks cbt/eight}) cbt/job cbt/box 40))]
          (is (= :done (:kind e)))
          (is (= {:dug 8 :skipped {} :kept 0 :fluids {}} (:data e))))
        (is (= {:kind :done :cause nil} (kc (await (ending-of (cbt/setup {}) cbt/job cbt/one 10)))) "an empty box: nothing to do")
        (let [e (await (ending-of (cbt/setup {:blocks {"1,64,1" "bedrock"}}) cbt/job cbt/one 10))]
          (is (= {:kind :condition-failed :cause :cannot} (kc e)) "nothing dug, the cell can never be dug")
          (is (= 0 (get-in e [:data :dug]))))
        (let [e (await (ending-of (cbt/setup {:blocks {"9,55,1" "dirt"}}) cbt/job {:from {:x 9 :y 55 :z 1} :to {:x 9 :y 55 :z 1}} 20))]
          (is (= {:kind :nothing-possible :cause :unreachable} (kc e))))))))

(deftest clear-box-start-facts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [:impossible :bad-args] (bad-args (await (ending-of (cbt/setup {}) cbt/job {:from {:x 0 :y 0 :z 0}} 5)))))
        (is (= [:impossible :bad-args]
               (bad-args (await (ending-of (cbt/setup {}) cbt/job {:from {:x 0 :y 0 :z 0} :to {:x 7 :y 7 :z 7}} 5)))) "over 400 cells")
        (is (= {:kind :condition-failed :cause {:reason :no-zones}}
               (kc (await (ending-of (cbt/setup {:blocks cbt/eight} (ew/of-data {} {} nil)) cbt/job cbt/box 5)))))
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
          (is (= {:pathed 4 :skipped {}} (:data e))))
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
        (is (= {:kind :condition-failed :cause {:reason :no-tool :kind "shovel"}}
               (kc (await (ending-of (pt/setup {:blocks pt/four}) path (assoc pt/box :fetch false) 5)))))
        (is (= {:kind :condition-failed :cause {:reason :no-zones}}
               (kc (await (ending-of (pt/setup {:inventory pt/shovel :blocks pt/four} (ew/of-data {} {} nil)) path pt/box 5)))))
        (is (= :refused (get-in (await (ending-of (pt/setup {:inventory pt/shovel :blocks pt/four}
                                                            (ew/of-data {} {} [(pt/zone [1 60 1] [2 70 2])]))
                                                  path pt/box 5))
                                [:cause :reason])))))))

(deftest path-with-no-shovel-and-nothing-to-fetch-ends-with-the-failed-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (ending-of (pt/setup {:blocks pt/four}) path pt/box 80))]
          (is (not= :done (:kind e)))
          (is (= 0 (get-in e [:data :pathed]))))))))

(deftest path-uses-only-the-shovel-the-call-lists
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (pt/setup {:inventory [{:name "stone_shovel" :count 1} {:name "diamond_shovel" :count 1}] :blocks pt/four})
              e (await (ending-of env path (assoc pt/box :use {:tools ["diamond_shovel"]}) 30))]
          (is (= :done (:kind e)))
          (is (= #{"diamond_shovel"} (set (map #(.-item (.-args %)) (pt/calls (:p env) "useOn"))))))))))
