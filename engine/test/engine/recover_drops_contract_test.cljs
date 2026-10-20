(ns engine.recover-drops-contract-test
  "jobs.survival.recover-drops as a contract job: what each way of ending a run is."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.death-test :as dt :refer [die! job recovered]]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn setup
  "library-test/setup with the canonical event sink, so a root's ending data (its :kind and :reason) is readable."
  [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (ew/of-data {} {} (:zones world []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn end-of [seen]
  (let [e (first (filter #(#{:completed :stopped} (:kind %)) @seen))]
    (when e (if (= :completed (:kind e)) {:kind :done} (select-keys (:data e) [:kind :reason])))))

(deftest recover-drops-exports-the-contract-and-quotes-validly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [def (get registry/jobs 'jobs.survival.recover-drops)]
          (is (some? (:can-run? def)))
          (is (nil? (:check def)))
          (is (some? (:done? def)))
          (is (nil? (contract/quote-problem
                     (await ((:quote-of def) {:primitives (tu/fake {}) :view (constantly {:data {} :now 0})
                                              :args {} :quote-call {:spend {:job-time 60}}}))))))))))

(deftest a-collected-pile-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities dt/drops})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 10))
          (is (= {:kind :done} (end-of seen)))
          (is (= :collected (:decision (recovered eng)))))))))

(defn dear-zombie-run
  "Die beside a zombie priced at 1000, submit recover-drops with the extra args, run it out: [eng seen id]."
  [extra]
  (let [zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}
        {:keys [eng seen clock]} (setup {:floor tu/walk-floor :inventory dt/sword :entities (conj dt/drops zombie)})]
    (die! eng {:pos dt/death-pos :inventory dt/diamonds} 3000)
    {:eng eng :seen seen :clock clock
     :id (core/submit! eng (list 'jobs.survival.recover-drops (merge {:danger-overrides {"zombie" 1000}} extra)) {})}))

(deftest a-danger-that-makes-the-pile-dear-waits-the-listed-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock id]} (dear-zombie-run {})
              timer (js/setInterval #(swap! clock + 500) 5)]
          (await (run-until-empty eng 12))
          (js/clearInterval timer)
          (is (= :danger (:reason (core/waiting eng id))))
          (is (nil? (recovered eng)) "the death stays open"))))))

(deftest a-danger-that-makes-the-pile-dear-ends-a-retry-condition-failed-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (dear-zombie-run {:retry true})
              timer (js/setInterval #(swap! clock + 500) 5)]
          (await (run-until-empty eng 12))
          (js/clearInterval timer)
          (is (= {:kind :condition-failed :reason :danger} (end-of seen)))
          (is (nil? (recovered eng)) "the death stays open"))))))

(deftest a-walk-that-does-not-arrive-ends-nothing-possible-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities dt/drops :unreachable ["20,64,0"]})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 6))
          (is (= {:kind :nothing-possible :reason :unreachable} (end-of seen))))))))

(deftest a-pile-not-worth-the-trip-ends-condition-failed-not-worth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities dt/drops})]
          (die! eng {:pos dt/death-pos :inventory dt/junk})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= {:kind :condition-failed :reason :not-worth} (end-of seen)))
          (is (= :skip (:decision (recovered eng)))))))))

(deftest drops-not-all-back-end-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities [(dt/drop-item 1 20 "diamond_pickaxe")]})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (is (= {:kind :nothing-possible :reason :not-visible} (end-of seen))))))))

(deftest a-closed-window-ends-condition-failed-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:floor tu/walk-floor :entities dt/drops})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (swap! clock + 300000)
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= {:kind :condition-failed :reason :gone} (end-of seen))))))))

(deftest nothing-left-at-the-death-point-ends-condition-failed-nothing-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (core/submit! eng job {})
          (await (run-until-empty eng 5))
          (is (= {:kind :condition-failed :reason :nothing-found} (end-of seen))))))))
