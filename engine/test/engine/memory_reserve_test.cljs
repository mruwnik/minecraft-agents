(ns engine.memory-reserve-test
  "jobs.memory.reserve and unreserve: the agent's writes to the body's reserve register."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [run-until-empty]]
            [jobs.lib.reserve :as reserve]
            [engine.triggers :as triggers]))

(defn setup []
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {:floor tu/walk-floor :inventory [{:name "diamond" :count 5} {:name "bread" :count 2}]})
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen}))

(defn ^:async run-job [eng spec]
  (core/submit! eng spec {})
  (await (run-until-empty eng 8)))

(defn live [eng] (reserve/entries (mem/view (:store eng))))

(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn run-world [done f] (tu/run-async done (fn ^:async t [] (await (f (setup))))))

(deftest reserve-writes-an-agent-entry-with-strength-one-by-default
  (async done (run-world done (fn ^:async t [{:keys [eng seen]}]
    (await (run-job eng '(jobs.memory.reserve {:item "diamond" :count 3 :why "for the beacon"})))
    (is (= [{:id "agent-diamond" :item "diamond" :count 3 :strength 1 :by "agent" :why "for the beacon" :until nil}]
           (live eng)))
    (is (= [{:id "agent-diamond" :item "diamond"}]
           (mapv #(select-keys % [:id :item]) (events-of seen :reserve.set))))))))

(deftest reserve-by-tag-takes-a-strength-and-a-ttl
  (async done (run-world done (fn ^:async t [{:keys [eng]}]
    (await (run-job eng '(jobs.memory.reserve {:id "snack" :tag :food :strength 0.5 :ttl-s 60})))
    (is (= [{:id "snack" :tag :food :count :all :strength 0.5 :by "agent" :why nil :until 1060000}] (live eng)))))))

(deftest writing-an-id-again-replaces-its-entry
  (async done (run-world done (fn ^:async t [{:keys [eng]}]
    (await (run-job eng '(jobs.memory.reserve {:id "k" :item "diamond" :count 1})))
    (await (run-job eng '(jobs.memory.reserve {:id "k" :item "bread" :count 2 :strength 0.3})))
    (is (= [["k" "bread" 0.3]] (mapv (juxt :id :item :strength) (live eng))))))))

(deftest unreserve-drops-an-id-or-every-entry-of-an-item
  (async done (run-world done (fn ^:async t [{:keys [eng seen]}]
    (await (run-job eng '(jobs.memory.reserve {:id "a" :item "diamond"})))
    (await (run-job eng '(jobs.memory.reserve {:id "b" :item "bread"})))
    (await (run-job eng '(jobs.memory.reserve {:id "c" :item "bread" :strength 0.2})))
    (await (run-job eng '(jobs.memory.unreserve {:id "a"})))
    (is (= ["b" "c"] (mapv :id (live eng))))
    (await (run-job eng '(jobs.memory.unreserve {:item "bread"})))
    (is (empty? (live eng)))
    (is (= [1 2] (mapv :removed (events-of seen :reserve.cleared))))))))

(deftest unreserve-of-nothing-is-already-done
  (async done (run-world done (fn ^:async t [{:keys [eng seen]}]
    (await (run-job eng '(jobs.memory.unreserve {:id "none"})))
    (is (empty? (live eng)))
    (is (empty? (events-of seen :reserve.refused)))))))

(deftest a-bad-call-is-refused-and-writes-nothing
  (async done (run-world done (fn ^:async t [{:keys [eng seen]}]
    (doseq [spec ['(jobs.memory.reserve {})
                  '(jobs.memory.reserve {:item "diamond" :tag :food})
                  '(jobs.memory.unreserve {})
                  '(jobs.memory.unreserve {:id "a" :item "diamond"})]]
      (await (run-job eng spec)))
    (is (empty? (live eng)))))))
