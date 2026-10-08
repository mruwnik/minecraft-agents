(ns engine.check-cache-test
  "jobs.lib.check-cache: a value read once per window and per job instance."
  (:require [cljs.test :refer [deftest is]]
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
