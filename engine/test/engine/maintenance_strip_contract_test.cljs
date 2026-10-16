(ns engine.maintenance-strip-contract-test
  "survival.restore-broken, maintenance.shut-doors, strip.loot-cart and strip.seal as contract jobs: they export the
  contract, quote validly, and end with a kind and a cause."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.blocks-dig-test :as bd]
            [engine.contract :as contract]
            [engine.registry :as registry]
            [engine.strip-seal-test :as ss]
            [engine.test-util :as tu]
            [jobs.strip.seal :as seal]))

(defn job [sym] (get registry/jobs sym))

(deftest each-job-exports-the-contract-and-quotes-validly
  (doseq [[sym args] [['jobs.survival.restore-broken {}]
                      ['jobs.maintenance.shut-doors {}]
                      ['jobs.strip.loot-cart {:id 5}]
                      ['jobs.strip.seal {:at [0 64 0] :axis :x :side :from :width 3 :height 3}]]]
    (is (nil? (:check (job sym))) (str sym " is a contract job"))
    (is (nil? (:check (job sym))) (str sym " has no old check"))
    (is (some? (:done? (job sym))) (str sym " has done?"))
    (is (vector? (:target (job sym))) (str sym " has a target"))
    (is (nil? (contract/quote-problem ((:quote-of (job sym)) {:primitives (tu/fake {}) :args args :quote-call {:spend {:job-time 60}} :view (fn [] {:data {} :now 0})}))) (str sym))))

(defn ^:async seal-ending [env args]
  (:ending (await (ss/run! env args))))

(deftest seal-stops-end-with-their-kind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :nothing-possible :cause :no-blocks}
               (select-keys (await (seal-ending (ss/world :inventory (into [{:name "cobblestone" :count 4}] ss/tools)) (assoc ss/args :fetch false))) [:kind :cause])))
        (is (= {:kind :condition-failed :cause :blocked}
               (select-keys (await (seal-ending (ss/world :extra {"0,64,1" "chest"}) ss/args)) [:kind :cause])))
        (is (= {:kind :condition-failed :cause :refused}
               (select-keys (await (seal-ending (ss/world :zones [{:name "farm" :min [0 60 0] :max [0 70 0] :owner "Miles"}]) ss/args)) [:kind :cause])))
        (is (= {:kind :nothing-possible :cause :wrong-side}
               (select-keys (await (seal-ending (ss/world :at 4 :extra (merge (tu/box 2 64 -1 2 66 1 "stone") (tu/box -11 64 -2 -11 66 2 "stone") (tu/box 11 64 -2 11 66 2 "stone"))) ss/args)) [:kind :cause])))))))
