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
        ctx (fn [id root] {:id id :root root :token 1 :view #(mem/view s)
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

(deftest a-read-from-another-root-leaves-the-expiry-alone
  (let [r (rig)]
    (place! ((:ctx r) "j1/stair" "j1") [0 64 0])
    (reset! (:clock r) (+ 1000000 (* 0.75 ttl-ms)))
    (ledger/entries ((:ctx r) "j9/scan" "j9"))
    (reset! (:clock r) (+ 1000000 (* 1.5 ttl-ms)))
    (is (true? (orphan? r (first (entries r)))) "a job of another root did not renew the entry")))

(deftest a-check-reads-the-ledger-without-writing-it
  (let [r (rig) child ((:ctx r) "j1/stair" "j1")
        check (assoc child :token nil :remember #(throw (js/Error. "cut: the ownership token changed")))]
    (place! child [0 64 0])
    (reset! (:clock r) (+ 1000000 (* 0.75 ttl-ms)))
    (is (= 1 (count (ledger/entries check))))))

(defn tunnel-ctx [{:keys [store]}]
  {:primitives #js {:self (fn [] #js {:dimension "overworld"})}
   :view #(mem/view store)
   :remember (fn [k d p] (mem/write! store k d p))
   :forget (fn [k pred] (mem/forget-where! store k pred))})

(defn tunnels [{:keys [store]}]
  (mapv #(select-keys (:data %) [:origin :delta :steps]) (mem/entries (mem/view store) :tunnel-way)))

(deftest one-line-of-one-step-calls-is-remembered-as-one-tunnel
  (let [r (rig) c (tunnel-ctx r)]
    (doseq [z (range -5 -45 -1)]
      (ledger/record-tunnel! c [-15686 40 z] [0 -1] 1))
    (is (= [{:origin [-15686 40 -5] :delta [0 -1] :steps 40}] (tunnels r)))
    (is (every? (ledger/own-cells c) [[-15686 40 -20] [-15686 41 -44] [-15686 40 -45]]))))

(deftest a-later-stretch-with-the-same-origin-extends-the-tunnel-and-other-lines-stay-apart
  (let [r (rig) c (tunnel-ctx r)]
    (ledger/record-tunnel! c [0 64 0] [1 0] 3)
    (ledger/record-tunnel! c [0 64 1] [1 0] 3)
    (ledger/record-tunnel! c [0 64 0] [1 0] 8)
    (ledger/record-tunnel! c [20 64 0] [1 0] 2)
    (is (= [{:origin [0 64 1] :delta [1 0] :steps 3} {:origin [0 64 0] :delta [1 0] :steps 8} {:origin [20 64 0] :delta [1 0] :steps 2}] (tunnels r)))))
