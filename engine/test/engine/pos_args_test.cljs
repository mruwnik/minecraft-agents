(ns engine.pos-args-test
  "A position in a job arg is [x y z] or {:x :y :z} of finite numbers; anything else is refused, never read as 0,0,0."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.test-util :as tu]
            [engine.unstick-test :as ut]
            [jobs.lib.near :as near]))

(deftest cell-of-takes-both-forms-and-refuses-the-rest
  (are [in out] (= out (near/cell-of in))
    [5 64 3]            {:x 5 :y 64 :z 3}
    {:x 5.7 :y 64 :z -3.2} {:x 5 :y 64 :z -4}
    [5 64]              nil
    [5 "a" 3]           nil
    {:x 1 :y 2}         nil
    nil                 nil
    [js/NaN 64 3]       nil))

(deftest pace-walks-toward-vector-points
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (ut/setup {:self {:pos ut/at5} :floor tu/walk-floor})]
          (core/submit! eng '(jobs.movement.pace {:a [5 64 3] :b [5 64 0] :laps 1}) {})
          (await (core/tick! eng))
          (is (= {:x 5 :y 64 :z 3} (:target (first (ut/moved eng)))) "the leg went to a, not the origin"))))))

(deftest pace-refuses-a-bad-point-at-submit
  (doseq [bad [[5 64] "x" {:x 1 :y 2}]]
    (let [{:keys [eng]} (ut/setup {:self {:pos ut/at5} :floor tu/walk-floor})]
      (is (thrown-with-msg? js/Error #":a must be \[x y z\]"
                            (core/submit! eng (list 'jobs.movement.pace {:a bad :b [5 64 0]}) {}))
          (pr-str bad)))))
