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
          (core/submit! eng (tu/order '(jobs.storage.look {:chests [[10 64 0] [12 64 0]]})) {})
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
          (core/submit! eng (tu/order '(jobs.storage.look {})) {})
          (await (run-until-empty eng 12))
          (is (= {[10 64 0] {"bread" 5} [12 64 0] {"emerald" 3}} (stock/stock-of (mem/view (:store eng))))
              "booked chests are outside the storage group, and the default look still refreshed them"))))))

(deftest a-gone-chest-is-skipped-and-unbooked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:containers {"12,64,0" [{:name "emerald" :count 3}]}})]
          (doseq [cell [[10 64 0] [12 64 0]]]
            (mem/write! (:store eng) stock/stock-kind {:pos cell :items {"stale" 1}} stock/stock-policy))
          (core/submit! eng (tu/order '(jobs.storage.look {})) {})
          (await (run-until-empty eng 12))
          (is (= {[12 64 0] {"emerald" 3}} (stock/stock-of (mem/view (:store eng))))
              "the chest that is gone is forgotten, the other one was still opened"))))))

(deftest transfers-on-one-chest-leave-the-other-chests-booked
  (let [store (mem/open (tu/tmp-dir) {})
        c {:view #(mem/view store)
           :remember (fn [kind data policy] (mem/write! store kind data policy))
           :forget (fn [kind pred] (mem/forget-where! store kind pred))}]
    (stock/note-stock! c [1 64 1] [{:name "bread" :count 5}])
    (stock/note-stock! c [2 64 2] [])
    (dotimes [_ (inc (:cap stock/stock-policy))] (stock/note-moved! c [2 64 2] "wheat" 1))
    (is (= {[1 64 1] {"bread" 5} [2 64 2] {"wheat" 65}} (stock/stock-of (mem/view store))))))

(deftest a-double-chest-is-booked-once-whichever-half-is-opened
  (let [store (mem/open (tu/tmp-dir) {})
        halves {[1 64 1] {:type "right" :facing "north"} [2 64 1] {:type "left" :facing "north"}}
        p #js {:seenBlockAt (fn [pos] (let [cell [(.-x pos) (.-y pos) (.-z pos)]]
                                        (clj->js {:name "chest" :pos {:x (first cell) :y (second cell) :z (last cell)} :properties (get halves cell)})))}
        c {:primitives p :view #(mem/view store)
           :remember (fn [kind data policy] (mem/write! store kind data policy))
           :forget (fn [kind pred] (mem/forget-where! store kind pred))}]
    (stock/note-stock! c [1 64 1] [{:name "wheat" :count 539}])
    (stock/note-stock! c [2 64 1] [{:name "wheat" :count 546}])
    (is (= {[2 64 1] {"wheat" 546}} (stock/stock-of (mem/view store))) "one entry, the latest count")
    (is (= {"wheat" 546} (stock/stock-at (mem/view store) [1 64 1])) "either half reads it")
    (stock/note-moved! c [1 64 1] "wheat" -46)
    (is (= {"wheat" 500} (stock/stock-at (mem/view store) [2 64 1])))))
