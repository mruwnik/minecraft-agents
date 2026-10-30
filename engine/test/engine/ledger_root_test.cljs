(ns engine.ledger-root-test
  "jobs.lib.ledger: an entry follows the root of the job tree that placed it."
  (:require [cljs.test :refer [deftest is]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.expiry :as expiry]
            [jobs.lib.ledger :as ledger]))

(def ttl-ms (* 1000 3600))

(defn rig []
  (let [clock (atom 1000000)
        s (mem/open (tu/tmp-dir) {:now #(deref clock)})
        ctx (fn [id root] {:id id :root root :view #(mem/view s)
                           :remember (fn [k d p] (mem/write! s k d p))})]
    {:clock clock :store s :ctx ctx}))

(defn place! [c cell]
  (ledger/remember! c (ledger/intend (ledger/open-entries ((:view c))) {:cell cell :item "dirt" :before "air" :job (:id c) :purpose :pillar})))

(defn entries [{:keys [store]}] (ledger/open-entries (mem/view store)))

(defn orphan? [{:keys [clock]} e] (not (expiry/current? @clock e)))

(deftest a-child-placers-entry-records-the-root
  (let [r (rig) c ((:ctx r) "j1/bed" "j1")]
    (place! c [0 64 0])
    (is (= "j1" (:root (first (entries r)))))))

(deftest the-root-releasing-ends-the-time-of-its-childrens-entries
  (let [r (rig)]
    (place! ((:ctx r) "j1/bed" "j1") [0 64 0])
    (place! ((:ctx r) "j9/fill" "j9") [5 64 0])
    (ledger/release-root! ((:ctx r) "j1/other" "j1"))
    (is (every? #(not (orphan? r %)) (entries r)) "a non-root job releases nothing")
    (ledger/release-root! ((:ctx r) "j1" "j1"))
    (is (= [true false] (mapv #(orphan? r %) (sort-by :cell (entries r)))) "only the root j1's entries are out of time")))

(deftest a-running-root-keeps-its-entries-past-the-ttl
  (let [r (rig) child ((:ctx r) "j1/stair" "j1") root ((:ctx r) "j1" "j1")]
    (place! child [0 64 0])
    (reset! (:clock r) (+ 1000000 (* 0.75 ttl-ms)))
    (ledger/keep-alive! root)
    (reset! (:clock r) (+ 1000000 (* 1.5 ttl-ms)))
    (is (false? (orphan? r (first (entries r)))) "renewed while the root ran")
    (reset! (:clock r) (+ 1000000 (* 3 ttl-ms)))
    (is (true? (orphan? r (first (entries r)))) "the ttl is the backstop once nobody renews")))

(deftest reading-the-ledger-renews-the-roots-entries
  (let [r (rig) child ((:ctx r) "j1/stair" "j1")]
    (place! child [0 64 0])
    (reset! (:clock r) (+ 1000000 (* 0.75 ttl-ms)))
    (ledger/entries child)
    (reset! (:clock r) (+ 1000000 (* 1.5 ttl-ms)))
    (is (false? (orphan? r (first (entries r)))) "a read by a job of the tree renewed the entry")))
