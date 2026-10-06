(ns engine.remember-test
  "jobs.memory.remember: one memory entry of a job's own kind, written from a spec, read back by the (since :kind)
  condition fact."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.condition :as c]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def remember 'jobs.memory.remember)

(defn setup []
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run-job
  "Submit spec and tick until the list is empty (at most 8 ticks)."
  [eng spec]
  (core/submit! eng spec {})
  (loop [i 0]
    (when (and (< i 8) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn view [eng] (mem/view (:store eng)))

(defn entries [eng kind] (mapv :data (mem/entries (view eng) kind)))

(def hour-ms (* 60 60 1000))

(deftest remember-writes-one-entry-and-reports-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (await (run-job eng (list remember {:kind :bred-cows :data {:fed 2}})))
          (is (= [{:fed 2}] (entries eng :bred-cows)))
          (is (= [{:memory-kind :bred-cows :entry {:fed 2}}]
                 (mapv #(select-keys % [:memory-kind :entry]) (events-of seen :memory.remembered))))
          (is (empty? (events-of seen :memory.refused))))))))

(deftest remember-without-data-writes-an-empty-map
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (await (run-job eng (list remember {:kind :bred-cows})))
          (is (= [{}] (entries eng :bred-cows))))))))

(deftest remember-takes-its-policy-from-ttl-s-and-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args policy] [[{} mem/default-policy]
                               [{:ttl-s 259200} {:cap 50 :ttl (* 3 24 hour-ms)}]
                               [{:cap 1} {:cap 1 :ttl hour-ms}]
                               [{:ttl-s 90 :cap 3} {:cap 3 :ttl 90000}]]]
          (let [{:keys [eng]} (setup)]
            (await (run-job eng (list remember (assoc args :kind :bred-cows))))
            (is (= policy (mem/policy (view eng) :bred-cows)) (pr-str args))))))))

(deftest remember-keeps-only-the-newest-cap-entries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)]
          (doseq [n [1 2 3]]
            (await (run-job eng (list remember {:kind :bred-cows :cap 2 :data {:n n}}))))
          (is (= [{:n 2} {:n 3}] (entries eng :bred-cows))))))))

(deftest remember-refuses-bad-arguments-as-data-and-writes-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args reason] [[{} :bad-kind]
                               [{:kind "bred-cows"} :bad-kind]
                               [{:kind :bred/cow} :bad-kind]
                               [{:kind :job/j1} :bad-kind]
                               [{:kind :hurt} :reserved-kind]
                               [{:kind :slept} :reserved-kind]
                               [{:kind :scaffold} :reserved-kind]
                               [{:kind :bed} :reserved-kind]
                               [{:kind :home} :reserved-kind]
                               [{:kind :food-source} :reserved-kind]
                               [{:kind :opened} :reserved-kind]
                               [{:kind :watched} :reserved-kind]
                               [{:kind :watch-turned} :reserved-kind]
                               [{:kind :sleep-failed} :reserved-kind]
                               [{:kind :bed-place-failed} :reserved-kind]
                               [{:kind :shelter-trapped} :reserved-kind]
                               [{:kind :bred-cows :data [1 2]} :bad-data]
                               [{:kind :bred-cows :data "x"} :bad-data]
                               [{:kind :bred-cows :ttl-s 0} :bad-ttl]
                               [{:kind :bred-cows :ttl-s -5} :bad-ttl]
                               [{:kind :bred-cows :ttl-s "60"} :bad-ttl]
                               [{:kind :bred-cows :ttl-s js/NaN} :bad-ttl]
                               [{:kind :bred-cows :cap 0} :bad-cap]
                               [{:kind :bred-cows :cap 1.5} :bad-cap]
                               [{:kind :bred-cows :cap "2"} :bad-cap]]]
          (let [{:keys [eng seen]} (setup)]
            (await (run-job eng (list remember args)))
            (is (= [reason] (mapv :reason (events-of seen :memory.refused))) (pr-str args))
            (is (every? string? (mapv :text (events-of seen :memory.refused))) (pr-str args))
            (is (empty? (events-of seen :memory.remembered)) (pr-str args))
            (is (empty? (entries eng (:kind args))) (pr-str args))
            (is (empty? (:list (core/state eng))) "the job still ended")))))))

(deftest remember-will-not-take-the-name-of-a-recorded-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (mem/write! (:store eng) :old-farm {:pos {:x 1 :y 64 :z 1}} mem/place-policy)
          (await (run-job eng (list remember {:kind :old-farm})))
          (is (= [:reserved-kind] (mapv :reason (events-of seen :memory.refused))))
          (is (= [{:pos {:x 1 :y 64 :z 1}}] (entries eng :old-farm))))))))

;; ------------------------------------------------------------------ the composition

(def breed-again
  "Breed again 20 minutes after the last time, or when it was never done."
  '(or (not (known? (since :bred-cows))) (> (since :bred-cows) 1200)))

(defn fires?
  "Whether the trigger condition holds now, in the engine's own clock and memory."
  [{:keys [eng p]}]
  (let [r (c/compile breed-again)]
    (:value (c/evaluate (:node r) {:world p :memory (view eng)} {}))))

(defn advance! [{:keys [clock]} seconds] (swap! clock + (* 1000 seconds)))

(deftest a-sequence-that-ends-in-remember-drives-a-since-trigger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup)
              spec (list 'seq (list 'jobs.debug.notify {:text "bred"}) (list remember {:kind :bred-cows}))]
          (is (true? (fires? s)) "never written")
          (await (run-job (:eng s) spec))
          (is (= 1 (count (entries (:eng s) :notify))) "the first job ran")
          (is (false? (fires? s)) "right after the sequence ran")
          (advance! s 1200)
          (is (false? (fires? s)) "1200 s is not over 1200")
          (advance! s 1)
          (is (true? (fires? s)) "after 1200 s")
          (advance! s 3600)
          (is (empty? (entries (:eng s) :bred-cows)) "the entry fell out of memory")
          (is (true? (fires? s)) "an expired entry is the same as never"))))))
