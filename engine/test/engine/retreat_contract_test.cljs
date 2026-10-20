(ns engine.retreat-contract-test
  "jobs.survival.retreat as a contract job: its exports, how a flight ends, and its time spend."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.flee-test :as ft]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.retreat :as retreat]
            [jobs.survival.retreat-choose :as choose]))

(defn setup
  "flee-test/setup with the canonical event sink and a contract parent 'caller that calls retreat once with ctx/call
  and keeps the ending in out."
  [world out]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (merge {:floor ft/big-floor} world)))
        now (tu/act-clock clock p ft/ms-per-call)
        caller {:round (fn ^:async caller-round [c]
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
          (is (nil? (:check def)))
          (is (= {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time :jobs.survival.retreat/job-time-s}} (:defaults def)))
          (is (= [:ok :clear] (tu/kind-reason ((:done? def) c))))
          (is (= {:units :hostiles :done 0 :left 0} (select-keys (nth ((:done? def) c) 2) [:units :done :left])))
          (is (nil? (contract/quote-problem (await ((:quote-of def) c))))))))))

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

(deftest the-time-spend-ends-a-flight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {:jobs.survival.retreat/job-time-s 3}
                 (fn ^:async spend []
                   (let [{:keys [eng seen]} (setup {:entities chaser} (atom nil))]
                     (core/submit! eng '(jobs.survival.retreat {}) {})
                     (await (tu/run-until-empty eng 5))
                     (let [{:keys [kind cause]} (:data (first (job-ev seen :stopped)))]
                       (is (= [:limit-reached {:line :job-time :spend 3}] [kind (select-keys cause [:line :spend])])))))))))))

(deftest a-flight-with-under-a-steer-left-ends-limit-reached-cleanly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [time-s [1.3 2.2 2.6 3.4 4.9]]
          (await (settings/with-settings {:jobs.survival.retreat/job-time-s time-s}
                   (fn ^:async near-budget []
                     (let [{:keys [eng seen]} (setup {:entities chaser} (atom nil))]
                       (core/submit! eng '(jobs.survival.retreat {}) {})
                       (await (tu/run-until-empty eng 10))
                       (is (empty? (:list (core/state eng))) (str time-s " s: the flight left the list"))
                       (is (= [[:limit-reached :job-time]]
                              (mapv #(let [{:keys [kind cause]} (:data %)] [kind (:line cause)]) (job-ev seen :stopped)))
                           (str time-s " s"))
                       (is (empty? (filter #(= :error (:level %)) @seen)) (str time-s " s: no error")))))))))))

(deftest a-cornered-hold-is-a-pause-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              c {:engine {:jobs registry/jobs}
                 :call (fn [slot def args] (swap! calls conj [slot (:sym def) (:why args)]) (js/Promise.resolve (contract/ending :done nil nil)))}]
          (is (= :again (await (retreat/wait-far! c))))
          (is (= [[:pause 'jobs.time.noop "cornered"]] @calls)))))))

(deftest the-choice-follows-the-documented-order
  (are [facts option] (= option (:option (choose/choose (merge {:threat :mob :outruns? true} facts))))
    {:threat nil :hit? true} :end
    {:threat nil :shot-open? true} :dodge
    {:early :r :hit? true} :early
    {:no-gain? true :hit? true} :stuck
    {:angry-enderman? true :hit? true :door :d} :cornered
    {:hit? true :door :d} :cornered
    {:flyer? true :door :d} :cornered
    {:door :d :inside? true :home-door :h} :shut-door
    {:inside? true :home-door :h :way :w} :shut-home-door
    {:inside? true :way :w} :stuck
    {:way :w :outruns? false} :run-home
    {:outruns? false} :cornered-inside
    {} :walk))
