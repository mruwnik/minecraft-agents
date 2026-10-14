(ns engine.retreat-contract-test
  "jobs.survival.retreat as a contract job: its exports, how a flight ends, and its time spend."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.flee-test :as ft]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(defn setup
  "flee-test/setup with the canonical event sink and a contract parent 'caller that calls retreat once with ctx/call
  and keeps the ending in out."
  [world out]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (merge {:floor ft/big-floor} world)))
        now (tu/act-clock clock p ft/ms-per-call)
        caller {:conditions []
                :round (fn ^:async caller-round [c]
                         (let [r (await (ctx/call c :flee 'jobs.survival.retreat {}))]
                           (if (= :continue r) :continue (do (reset! out r) (ctx/end c :done nil)))))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'caller caller) :triggers triggers/all :dir (tu/tmp-dir)
                          :now now :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :seen seen}))

(defn job-ev [seen kind] (filterv #(= [:job kind] [(:source %) (:kind %)]) @seen))

(def chaser [(ft/zombie 7 5 {:chase {:speed 0.7}})])

(deftest retreat-exports-the-contract-and-quotes-validly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [def (get registry/jobs 'jobs.survival.retreat)
              c {:primitives (tu/fake {}) :view (constantly {:data {} :now 0}) :root 1 :slots [] :args {}}]
          (is (some? (:conditions def)))
          (is (nil? (:check def)))
          (is (= {:spend {:time :jobs.survival.retreat/time-s}} (:defaults def)))
          (is (= {:done? false :units :hostiles :done 0 :left 0} ((:done? def) c)))
          (is (nil? (contract/quote-problem (await ((:quote-of def) c))))))))))

(deftest old-weapons-become-the-calls-tools
  (let [legacy (:legacy-args (get registry/jobs 'jobs.survival.retreat))]
    (is (= {:radius 8 :use {:tools ["sword" "axe"]}} (legacy {:radius 8 :weapons ["sword" :axe]})))
    (is (= {:use {:tools ["trident"]}} (legacy {:weapons ["sword"] :use {:tools ["trident"]}})) "a given :use wins")))

(deftest a-whole-flight-ends-done-with-who-it-fled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [out (atom nil)
              {:keys [eng]} (setup {:entities chaser} out)]
          (core/submit! eng '(caller) {})
          (await (tu/run-until-empty eng 5))
          (is (= :done (:kind @out)))
          (is (= {:fled [7] :ended :far} (:data @out))))))))

(deftest the-time-spend-ends-a-flight-when-limits-are-enforced
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {:engine.contract/enforce-limits true :jobs.survival.retreat/time-s 3}
                 (fn ^:async enforced []
                   (let [{:keys [eng seen]} (setup {:entities chaser} (atom nil))]
                     (core/submit! eng '(jobs.survival.retreat {}) {})
                     (await (tu/run-until-empty eng 5))
                     (let [{:keys [kind cause]} (:data (first (job-ev seen :stopped)))]
                       (is (= [:limit-reached {:line :time :spend 3}] [kind (select-keys cause [:line :spend])])))))))))))

(deftest in-shadow-mode-the-time-spend-only-tells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {:jobs.survival.retreat/time-s 3}
                 (fn ^:async shadow []
                   (let [{:keys [eng seen]} (setup {:entities chaser} (atom nil))]
                     (core/submit! eng '(jobs.survival.retreat {}) {})
                     (await (tu/run-until-empty eng 5))
                     (is (= {:line :time :spend 3} (select-keys (:cause (:data (first (job-ev seen :limit_would_end)))) [:line :spend])))
                     (is (= 1 (count (job-ev seen :completed))) "the flight runs to its end")))))))))
