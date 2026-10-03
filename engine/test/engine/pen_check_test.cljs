(ns engine.pen-check-test
  "jobs.animals.pen-check against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]))

(def job 'jobs.animals.pen-check)

(def ground
  (into {} (for [x (range -10 16) z (range -10 16)] [(str x ",63," z) "stone"])))

(def fence-ring
  (into {} (for [x (range -1 6) z (range -1 6) :when (or (#{-1 5} x) (#{-1 5} z))] [(str x ",64," z) "oak_fence"])))

(defn world [more & [states]]
  {:self {:pos {:x 2 :y 64 :z 2}}
   :blocks (merge ground fence-ring more)
   :states (or states {})})

(defn ^:async run-job
  "Submit the job with args in a world and run two ticks; the pen-check.done event, or nil."
  [args w]
  (let [s (h/setup w)]
    (core/submit! (:eng s) (list job args) {})
    (dotimes [_ 2]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    {:event (first (filter #(= :pen-check.done (:kind %)) @(:seen s)))
     :listed (:list (core/state (:eng s)))}))

(deftest a-closed-pen-from-a-cell-is-reported-closed-with-its-size
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event listed]} (await (run-job {:at [2 64 2]} (world {})))]
          (is (= {:closed? true :cells 25 :leaks [] :gates []}
                 (select-keys event [:closed? :reason :cells :leaks :gates])))
          (is (empty? listed)))))))

(deftest an-open-gate-is-a-leak-at-the-gate-and-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:at [2 64 2] :max-cells 100}
                                              (world {"2,64,-1" "oak_fence_gate"} {"2,64,-1" {:open true}})))]
          (is (false? (:closed? event)))
          (is (= :leak (:reason event)))
          (is (= [{:pos {:x 2 :y 64 :z -1} :why :open-gate}] (:leaks event)))
          (is (= [{:pos {:x 2 :y 64 :z -1} :open? true}] (:gates event))))))))

(deftest a-box-gives-the-exact-crossings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:box {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 64 :z 4}}}
                                              (world {"5,64,1" "air"})))]
          (is (false? (:closed? event)))
          (is (= [{:pos {:x 5 :y 64 :z 1} :why :gap}] (:leaks event)))
          (is (= 25 (:cells event))))))))

(deftest a-start-in-the-air-is-no-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:at [2 70 2]} (world {})))]
          (is (= :no-start (:reason event))))))))

(deftest without-a-start-or-a-box-the-job-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {} (world {})))]
          (is (nil? event)))))))
