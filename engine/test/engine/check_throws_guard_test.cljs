(ns engine.check-throws-guard-test
  "contract/verdict runs every condition, so a job's can-run? may run while its can-start? precondition is false (bad
  args, nothing loaded). Each registered job's can-start? and can-run? must answer, never throw, on an empty body
  with no args or nil args (spec-refused values never reach a check)."
  (:require [cljs.test :refer [deftest is]]
            [engine.contract :as contract]
            [engine.expr :as expr]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]))

(def bad-args [{} {:item "nope" :target "nope" :count 0} {:pos nil :item nil :target nil :chest nil :furnace nil :box nil}])

(defn throws
  "The error text when export of job throws for args on an empty body, else nil."
  [sym export args]
  (let [job (get registry/jobs sym)
        defaults (second (expr/leaf registry/jobs sym {}))
        c {:primitives (tu/fake-on-floor {}) :args (merge defaults args) :root "j" :slots [] :id "j" :sym sym :emit (fn [& _])
           :engine {:warned (atom #{}) :world (world/of-data {} {} []) :state (atom {}) :jobs registry/jobs}
           :view (fn [] {:now 1000 :data {:entries {} :policies {}}})}]
    (try (contract/verdict c (select-keys job [export])) nil
         (catch :default e (str e)))))

(deftest checks-answer-on-empty-and-bad-args
  (let [failing (into {} (for [[sym job] registry/jobs
                               export [:can-start? :can-run?]
                               :when (get job export)
                               args bad-args
                               :let [err (throws sym export args)]
                               :when err]
                           [[sym export] err]))]
    (is (= {} failing) "a check throws: guard it so it answers :never or :later")))
