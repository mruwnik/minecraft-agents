(ns engine.check-cache-test
  "jobs.lib.check-cache: a value read once per window and per job instance."
  (:require [cljs.test :refer [deftest is]]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as cc]))

(def engine {:warned (atom #{})})

(defn ctx-at [id t] {:id id :slots [] :engine engine :view (fn [] {:now t})})

(deftest a-value-is-read-once-per-window
  (let [n (atom 0)
        read #(swap! n inc)
        w (cc/ttl-ms)]
    (is (= [1 1] [(cc/cached (ctx-at "a" 0) :k read) (cc/cached (ctx-at "a" (dec w)) :k read)]))
    (is (= 2 (cc/cached (ctx-at "a" (+ w 1)) :k read)) "after the window it is read again")
    (is (= 2 @n))))

(deftest keys-and-jobs-do-not-share-values
  (let [n (atom 0)
        read #(swap! n inc)]
    (is (= [1 2 3] [(cc/cached (ctx-at "b" 0) :k read) (cc/cached (ctx-at "b" 0) :other read) (cc/cached (ctx-at "c" 0) :k read)]))))

(deftest engines-do-not-share-values
  (let [n (atom 0)
        read #(swap! n inc)
        other (assoc (ctx-at "d" 0) :engine {:warned (atom #{})})]
    (is (= [1 2] [(cc/cached (ctx-at "d" 0) :k read) (cc/cached other :k read)]))))

(deftest keyed-values-are-reused-whatever-their-age-and-only-the-latest-inputs-are-kept
  (let [n (atom 0)
        read #(swap! n inc)
        keep (settings/get cc/settings ::cc/keep)
        at #(cc/keyed (ctx-at "k" 0) :t % read)]
    (is (= 1 (at 1)))
    (is (= 1 (cc/keyed (ctx-at "k" 10000000) :t 1 read)) "an old value is reused")
    (doseq [i (range 2 (+ 2 keep))] (at i))
    (is (= (inc keep) @n))
    (at 1)
    (is (= (+ 2 keep) @n) "inputs older than the latest kept are read again")
    (at (+ 1 keep))
    (is (= (+ 2 keep) @n) "the latest are kept")))

(deftest keyed-values-are-read-again-when-a-setting-changes
  (let [n (atom 0)
        read #(swap! n inc)
        c (ctx-at "s" 0)]
    (cc/keyed c :t 1 read)
    (settings/scoped {::cc/ttl-ms 5} #(cc/keyed c :t 1 read))
    (is (= 2 @n))))

(deftest entries-of-an-ended-job-are-freed-and-a-running-sibling-keeps-its-own
  (let [state (atom {:instances {"x" {} "y" {}}})
        eng {:warned (atom #{}) :state state}
        c (fn [id] (assoc (ctx-at id 0) :engine eng))
        ids #(set (map first (keys @(cc/entries-of (c "x")))))]
    (cc/keyed (c "x") :t 1 (constantly 1))
    (cc/cached (c "y") :k (constantly 1))
    (is (= #{"x" "y"} (ids)))
    (swap! state update :instances dissoc "x")
    (cc/keyed (c "z") :t 1 (constantly 1))
    (is (= #{"y" "z"} (ids)) "x ended: gone; y still runs: kept; z is the caller's own")))

(deftest child-call-entries-live-with-their-root-instance
  (let [state (atom {:instances {"j1" {}}})
        eng {:warned (atom #{}) :state state}
        c (fn [id] (assoc (ctx-at id 0) :engine eng))
        ids #(set (map first (keys @(cc/entries-of (c "j1")))))]
    (cc/keyed (c "j1") :planned 1 (constantly 1))
    (cc/keyed (c "j1/kid") :planned 1 (constantly 1))
    (cc/keyed (c "j1/kid/lever") :planned 1 (constantly 1))
    (is (= #{"j1" "j1/kid" "j1/kid/lever"} (ids)) "a child's first store keeps its live ancestors")
    (cc/keyed (c "j10") :planned 1 (constantly 1))
    (swap! state update :instances dissoc "j1")
    (cc/keyed (c "j2/kid") :planned 1 (constantly 1))
    (is (= #{"j2/kid"} (ids)) "j1 ended: it and its children are gone; j10 is no instance either")))
