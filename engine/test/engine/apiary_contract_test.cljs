(ns engine.apiary-contract-test
  "apiary.guard and apiary.harvest as contract jobs: they export the contract, quote validly, wait on their start facts
  and end with a kind and a cause."
  (:require [cljs.test :refer [deftest is async]]
            [engine.apiary-guard-test :as g]
            [engine.apiary-harvest-test :as h]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]))

(defn job [sym] (get registry/jobs sym))

(deftest each-job-exports-the-contract-and-quotes-validly
  (doseq [sym ['jobs.apiary.guard 'jobs.apiary.harvest 'jobs.apiary.maintain]]
    (is (some? (:can-start? (job sym))) (str sym " has can-start?"))
    (is (vector? (:target (job sym))) (str sym " has a target"))
    (is (some? (:done? (job sym))) (str sym " has done?"))
    (is (nil? (contract/quote-problem ((:quote-of (job sym)) {:primitives (tu/fake {}) :args {:max 4} :quote-call {:spend {:job-time 60}}}))) (str sym))))

(defn ^:async ends
  "Submit spec over world w (its :zones kept) and tick until the job ends or waits: {:event :completed|:stopped :kind :reason}, or {:event :waiting :reason}."
  [spec w]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/seeing-all (tu/fake-on-floor w))
        now (tu/act-clock clock p 20)
        eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir) :now now
                          :world (world/of-data {} {} (:zones w []))
                          :events (events/make {:sinks [sink] :now now})})]
    (core/submit! eng spec {})
    (loop [i 0]
      (when (and (< i 80) (not-any? #(#{:completed :stopped} (:kind %)) @seen))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [e (first (filter #(#{:completed :stopped} (:kind %)) @seen))
          wt (last (filter #(= :waiting (:kind %)) @seen))]
      (core/shutdown! eng)
      (if e
        (assoc (select-keys (:data e) [:kind :reason]) :event (:kind e))
        {:event (when wt :waiting) :reason (:reason (:data wt))}))))

(def guard '(jobs.apiary.guard {}))
(def no-fetch-guard '(jobs.apiary.guard {:ways #{:carried}}))
(def carpet (g/inv "white_carpet" 2))
(def safe-fire (assoc-in (g/fire-world {}) [:blocks "2,65,0"] "white_carpet"))

(deftest guard-waits-on-its-start-facts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :waiting :reason :no-carpet}
               (await (ends no-fetch-guard (g/fire-world {:inventory (g/inv "moss_carpet" 3)})))))
        (is (= {:event :waiting :reason :no-campfire}
               (await (ends no-fetch-guard (-> (g/fire-world {:inventory (g/inv "stick" 1)}) (g/open-side "3,64,0")
                                      (assoc-in [:blocks "2,65,0"] "white_carpet"))))))
        (is (= {:event :completed} (select-keys (await (ends guard safe-fire)) [:event])) "every fire already safe: the goal holds")
        (is (= {:event :waiting :reason :nothing-to-guard} (await (ends guard {}))))
        (is (= {:event :waiting :reason :nothing-to-guard}
               (await (ends '(jobs.apiary.guard {:box {:min {:x 10 :y 60 :z -5} :max {:x 14 :y 70 :z 5}}})
                            (g/fire-world {:inventory carpet})))))
        (is (= {:event :waiting :reason :nothing-to-guard}
               (await (ends '(jobs.apiary.guard {:box {:min [10 60 -5] :max [14 70 5]}})
                            (g/fire-world {:inventory carpet})))) "vector corners are a box too")
        (is (= {:event :waiting :reason :refused}
               (await (ends guard (assoc (g/fire-world {:inventory carpet}) :zones [(assoc g/zone-over-fire :owner "Miles")])))))))))

;; place refused on the cell over the fire, dig allowed: every fire is left out of the survey
(deftest guard-waits-refused-when-only-the-place-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :waiting :reason :refused}
               (await (ends guard (assoc (g/fire-world {:inventory carpet})
                                         :zones [{:name "bees" :owner "Miles" :min [2 65 0] :max [2 65 0]}])))))))))

(deftest guard-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ends guard (g/fire-world {:inventory carpet}))) [:event])))
        (is (= {:event :stopped :kind :nothing-possible :reason :unreachable}
               (await (ends '(jobs.apiary.guard {:radius 30})
                            (assoc (g/fire-world {:inventory carpet}) :self {:pos {:x 20 :y 64 :z 0}} :unreachable ["2,64,0"])))))))))

(def harvest '(jobs.apiary.harvest {}))
(def no-fetch-harvest '(jobs.apiary.harvest {:ways #{:carried}}))
(def shears (h/inv "shears" 1))

(deftest harvest-waits-on-its-start-facts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :waiting :reason :no-tool} (await (ends no-fetch-harvest (h/world {:inventory (h/inv "stick" 1)})))))
        (is (= {:event :waiting :reason :no-hive} (await (ends harvest {:inventory shears}))))
        (is (= {:event :waiting :reason :not-ripe} (await (ends harvest (h/world {:inventory shears :level 2})))))
        (is (= {:event :waiting :reason :not-smoked} (await (ends harvest (h/world {:inventory shears :smoke? false})))))
        (is (= {:event :waiting :reason :no-hive}
               (await (ends '(jobs.apiary.harvest {:box {:min {:x 10 :y 60 :z -5} :max {:x 14 :y 70 :z 5}}}) (h/world {:inventory shears})))))
        (is (= {:event :waiting :reason :open-fire}
               (await (ends harvest (-> (h/world {:inventory shears})
                                        (update :blocks #(-> % (dissoc h/under) (assoc "2,62,0" "campfire")))
                                        (update :states #(-> % (dissoc h/under) (assoc "2,62,0" {:lit true}))))))))
        (is (= {:event :waiting :reason :no-tool} (await (ends '(jobs.apiary.harvest {:with :bottle :ways #{:carried}}) (h/world {:inventory shears})))))
        (is (= {:event :waiting :reason :no-zones} (await (ends harvest (assoc (h/world {:inventory shears}) :zones nil)))))
        (is (= {:event :waiting :reason :refused}
               (await (ends harvest (assoc (h/world {:inventory shears}) :zones [{:name "bees" :min [2 60 0] :max [2 70 0] :owner "Miles"}])))))))))

(deftest harvest-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed} (select-keys (await (ends harvest (h/world {:inventory shears}))) [:event])))
        (is (= {:event :stopped :kind :nothing-possible :reason :unreachable}
               (await (ends '(jobs.apiary.harvest {:radius 30})
                            (assoc (h/world {:inventory shears}) :self {:pos {:x 20 :y 64 :z 0}} :unreachable ["2,64,0"])))))))))

(deftest a-missing-item-is-fetched-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :reason :need} (select-keys (await (ends harvest (h/world {:inventory (h/inv "stick" 1)}))) [:event :reason])))
        (is (= {:event :stopped :reason :need} (select-keys (await (ends guard (g/fire-world {:inventory (g/inv "moss_carpet" 3)}))) [:event :reason])))
        (is (= {:event :stopped :reason :need}
               (select-keys (await (ends guard (-> (g/fire-world {:inventory (g/inv "stick" 1)}) (g/open-side "3,64,0")
                                                   (assoc-in [:blocks "2,65,0"] "white_carpet")))) [:event :reason])))))))

(deftest harvest-below-least-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (h/world {:inventory (h/inv "shears" 1)})]
          (is (= {:event :completed} (select-keys (await (ends '(jobs.apiary.harvest {}) w)) [:event])))
          (is (= {:event :stopped :kind :nothing-possible :reason :not-ripe}
                 (await (ends '(jobs.apiary.harvest {:least 3}) w)))
              "one hive taken of three asked: the hive left is not ripe"))))))
