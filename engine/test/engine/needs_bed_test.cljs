(ns engine.needs-bed-test
  "The :needs-bed trigger: a body overdue for sleep with no bed gets one by day."
  (:require [cljs.test :refer [deftest is]]
            [engine.expr :as expr]
            [engine.registry :as registry]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.shelter :as sh]
            [triggers.survival.needs-bed :as nb]))

(defn holds?
  "The trigger over a fake world (night? and inv given), the memory entries [[kind data age-days] ..] written age-days ago."
  [{:keys [night inv entries] :or {entries []}}]
  (let [clock (atom 0)
        store (mem/open (tu/tmp-dir) {:now #(deref clock)})]
    (doseq [[kind data age] entries]
      (reset! clock (- (* 10 sh/ms-per-day) (* age sh/ms-per-day)))
      (mem/write! store kind data {:cap 8 :ttl (* 20 sh/ms-per-day)}))
    (reset! clock (* 10 sh/ms-per-day))
    (nb/needs-bed (tu/fake-on-floor (cond-> {:floor [-10 -10 10 10] :inventory (or inv [])} night (assoc :time 18000)))
                  (mem/view store) {})))

(def slept-long-ago [[:slept {:pos {:x 0 :y 64 :z 0}} 4]])

(deftest an-overdue-body-with-no-bed-by-day-needs-one
  (is (true? (holds? {:entries slept-long-ago}))))

(deftest a-body-that-never-slept-counts-from-its-respawn-or-its-first-memory
  (is (true? (holds? {:entries [[:looked {} 4]]})))
  (is (true? (holds? {:entries [[:looked {} 4] [:respawned {} 3.5]]})))
  (is (false? (holds? {:entries [[:looked {} 4] [:respawned {} 1]]})))
  (is (false? (holds? {:entries [[:looked {} 4] [:deaths {:death-t (- (* 10 sh/ms-per-day) sh/ms-per-day)} 1]]}))))

(deftest a-rested-body-or-one-with-no-memory-does-not
  (is (false? (holds? {:entries [[:slept {:pos {:x 0 :y 64 :z 0}} 1] [:looked {} 4]]})))
  (is (false? (holds? {}))))

(deftest a-carried-or-remembered-bed-or-the-night-needs-no-errand
  (is (false? (holds? {:entries slept-long-ago :inv [{:name "red_bed" :count 1}]})))
  (is (false? (holds? {:entries (conj slept-long-ago [:bed {:pos {:x 5 :y 64 :z 0}} 2])})))
  (is (false? (holds? {:entries slept-long-ago :night true}))))

(deftest the-trigger-args-are-valid-obtain-args
  (is (nil? (expr/problem registry/jobs (list 'jobs.items.obtain nb/defaults)))))
