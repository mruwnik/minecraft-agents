(ns engine.fell-contract-test
  "jobs.forestry.fell-tree and jobs.forestry.fell-from-pillar as contract jobs: they export the contract and end with a
  kind and a cause (0 of N is never done); the pillar is a child of the felling."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.event-api :as event-api]
            [engine.core :as core]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.fell-tree-test :as ft]
            [engine.forest-maintain-test :as fm]
            [engine.library-test :as lt]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.access.approach :as approach]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.forestry.fell-tree :as fell-tree]
            [engine.memory :as mem]))

(def jobs '[jobs.forestry.fell-tree jobs.forestry.fell-from-pillar])

(defn ^:async quote-of [eng sym args]
  (:quote (await (event-api/quote-view eng (str sym) (pr-str args)))))

(deftest both-jobs-export-the-contract-and-quote-validly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fm/start ft/tall {})
              args {'jobs.forestry.fell-tree {:radius 10}
                    'jobs.forestry.fell-from-pillar {:base {:x 6 :y 64 :z 0} :species "oak" :radius 10}}]
          (doseq [sym jobs
                  :let [job (get registry/jobs sym)]]
            (is (nil? (:check job)) (str sym " is a contract job"))
            (is (nil? (:check job)) (str sym " has no old check"))
            (is (vector? (:target job)) (str sym " has a target"))
            (is (nil? (contract/quote-problem (await (quote-of (:eng s) sym (get args sym))))) (str sym))))))))

(deftest fell-trees-walk-is-go-tos-quote
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (fm/start ft/tall {})
              walk (await (quote-of eng 'jobs.movement.go-to {:pos [6 64 0] :range 2}))
              fell (await (quote-of eng 'jobs.forestry.fell-tree {:radius 10}))]
          (is (js/isFinite (first (get-in walk [:uses :job-time]))))
          (is (= (+ 2 (first (get-in walk [:uses :job-time]))) (first (get-in fell [:uses :job-time])))))))))

(defn ^:async fell-ending
  "The ending of job with args on a fresh world of spec, in at most n ticks (nil when it does not end)."
  ([spec job args n] (fell-ending spec job args n nil))
  ([spec job args n after-start]
   (let [s (fm/start spec {})]
     (ft/eye-reach! (:p s) (:lose-logs? spec))
     (when after-start (after-start s))
     (assoc (select-keys s [:p :seen :eng]) :end (await (ending-of s job args n))))))

(defn kc [e] {:kind (:kind e) :cause (contract/cause-key e)})

(deftest a-felled-tree-is-done-and-a-missing-one-is-condition-failed-no-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end]} (await (fell-ending ft/tall 'jobs.forestry.fell-tree {:radius 10} 200))]
          (is (= :done (:kind end)))
          (is (= {:base {:x 6 :y 64 :z 0} :logs 8 :axe :none-found} (dissoc (:data end) :text))))
        (let [{:keys [end]} (await (fell-ending {:blocks {}} 'jobs.forestry.fell-tree {:radius 10} 40))]
          (is (= {:kind :condition-failed :cause :no-tree} (kc end)))
          (is (= 10 (get-in end [:cause :radius]))))))))

(deftest no-tree-is-a-look-until-the-body-has-looked-round
  (let [c {:primitives (tu/fake {:blocks {}}) :view (constantly {:now 0}) :args {:radius 10}}
        status (fn [surveyed?] (with-redefs [ctx/mem (constantly {}) look/surveyed? (fn ([_] surveyed?) ([_ _] surveyed?))] (fell-tree/no-tree c)))]
    (is (= [:look :no-tree] ((juxt first (comp :reason last)) (status false))))
    (is (= [:later :no-tree] ((juxt first (comp :reason last)) (status true))))))

(deftest an-at-column-with-no-log-ends-impossible-no-tree-naming-the-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [at {:x 6 :y 64 :z 0}
              {:keys [end]} (await (fell-ending {:blocks {}} 'jobs.forestry.fell-tree {:at at} 40))]
          (is (= {:kind :impossible :cause :no-tree} (kc end)))
          (is (= at (get-in end [:cause :at]))))))))

(deftest an-at-column-with-no-log-is-never-not-later
  (let [at {:x 6 :y 64 :z 0}
        c {:primitives (tu/fake {:blocks {}}) :view (constantly {:now 0}) :args {:at at}}
        status (with-redefs [ctx/mem (constantly {}) look/surveyed? (fn ([_] true) ([_ _] true))] (fell-tree/no-tree c))]
    (is (= [:never :no-tree at] [(first status) (:reason (last status)) (:at (last status))]))))

(deftest a-column-not-yet-seen-is-walked-to-not-looked-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (fm/start {:blocks {}} {})]
          (core/submit! eng '(jobs.forestry.fell-tree {:at {:x 30 :y 64 :z 0}}) {})
          (await (core/tick! eng))
          (is (empty? (filterv #(= :waiting (:kind %)) @seen)) "not held back by a look-first no-tree"))))))

(deftest every-tree-out-of-reach-is-nothing-possible-tree-unreachable-with-the-columns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end]} (await (fell-ending {:blocks (lt/tree 8 0 "oak" 3) :unreachable ["8,64,0"]}
                                                'jobs.forestry.fell-tree {:radius 20} 100))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end)))
          (is (= 1 (get-in end [:data :unreachable])))
          (is (= [[8 0]] (get-in end [:data :columns]))))))))

(deftest a-pillar-that-cannot-be-had-ends-with-the-fetch-ending-and-fetch-false-with-the-need
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end p]} (await (fell-ending (assoc ft/tall :inventory [] :lose-logs? true) 'jobs.forestry.fell-tree {:radius 10 :fetch false} 100))]
          (is (= {:kind :condition-failed :cause :need} (kc end)))
          (is (= ["dirt" "cobblestone"] (get-in end [:cause :any-of])))
          (is (pos? (get-in end [:cause :count])))
          (is (= [] (filter #(= "jumpPlace" (.-name %)) (.-calls (.-world p))))))))))

(deftest a-refused-pillar-leaves-the-tree-and-the-call-ends-tree-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "grove" :min [-6 64 -9] :max [16 80 9] :allow #{:dig}}]
              {:keys [end]} (await (fm/with-zones zones #(fell-ending ft/tall 'jobs.forestry.fell-tree {:radius 10} 120)))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end))))))))

(deftest fell-from-pillar-ends-done-with-its-logs-and-a-refused-one-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [args {:base {:x 6 :y 64 :z 0} :species "oak" :radius 10}
              {:keys [end eng]} (await (fell-ending ft/tall 'jobs.forestry.fell-from-pillar args 200))]
          (is (= :done (:kind end)))
          (is (pos? (get-in end [:data :logs])))
          (is (= [] (ledger/open-entries (mem/view (:store eng)))) "the pillar is taken back"))
        (let [zones [{:name "grove" :min [-6 64 -9] :max [16 80 9] :allow #{:dig}}]
              {:keys [end]} (await (fm/with-zones zones #(fell-ending ft/tall 'jobs.forestry.fell-from-pillar {:base {:x 6 :y 64 :z 0} :species "oak" :radius 10} 120)))]
          (is (= {:kind :condition-failed :cause :refused} (kc end))))
        (let [{:keys [end]} (await (fell-ending ft/tall 'jobs.forestry.fell-from-pillar {} 20))]
          (is (= {:kind :impossible :cause :bad-args} (kc end))))))))

(deftest a-pillar-with-no-log-in-reach-is-taken-back-and-the-tree-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end eng]} (with-redefs [approach/plan (fn [_] {:stand [[5 64 0]]})]
                                  (await (fell-ending ft/tall 'jobs.forestry.fell-tree {:radius 10} 200)))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end)))
          (is (= [] (ledger/open-entries (mem/view (:store eng))))))))))
