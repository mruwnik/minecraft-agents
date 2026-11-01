(ns engine.storage-look-test
  "jobs.storage.look against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.lib.stock :as stock]))

(defn setup [world]
  (let [clock (atom 1000000)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)]
    {:p p :eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                             :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})}))

(deftest look-books-the-contents-and-takes-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}] "12,64,0" [{:name "emerald" :count 3}]}})]
          (core/submit! eng '(jobs.storage.look {:chests [[10 64 0] [12 64 0]]}) {})
          (await (run-until-empty eng 12))
          (is (= {[10 64 0] {"bread" 5} [12 64 0] {"emerald" 3}}
                 (stock/stock-of (mem/view (:store eng)))) "the chests list in observe chests")
          (is (= [{:name "bread" :count 5}] (get-in @(fake/state p) [:containers [10 64 0]])))
          (is (empty? (filter #(= "transfer" (.-name %)) (.-calls (.-world p))))))))))

(deftest default-look-opens-every-booked-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}] "12,64,0" [{:name "emerald" :count 3}]}})]
          (doseq [cell [[10 64 0] [12 64 0]]]
            (mem/write! (:store eng) stock/stock-kind {:pos cell :items {"stale" 1}} stock/stock-policy))
          (core/submit! eng '(jobs.storage.look) {})
          (await (run-until-empty eng 12))
          (is (= {[10 64 0] {"bread" 5} [12 64 0] {"emerald" 3}} (stock/stock-of (mem/view (:store eng))))
              "booked chests are outside the storage group, and the default look still refreshed them"))))))
