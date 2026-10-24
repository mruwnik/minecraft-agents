(ns engine.standing-test
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.cost :as cost]
            [jobs.lib.standing :as standing]))

(defn ctx-with [state waiting]
  {:id "j1/tend" :engine {:state (atom state) :waiting (atom waiting) :now (constantly 10000)}
   :primitives #js {:self (fn [] #js {:pos #js {:x 0 :y 0 :z 0}})}})

(defn told
  "A wait reason whose end is known, ms after it was told at t."
  [reason ms t] (with-meta {:reason reason :recheck-after-ms ms :wait-ms ms} {:t t}))

(defn polled
  "A wait reason rechecked every ms, its end unknown."
  [reason ms t] (with-meta {:reason reason :recheck-after-ms ms} {:t t}))

(defn pass [worth work-s] (constantly {:worth worth :work-s work-s}))

(def listed
  {:list ["j1" "j2" "j3" "j4"]
   :instances {"j1" {:spec {:op :repeat}}
               "j2" {:spec {:job 'jobs.animals.maintain}}
               "j3" {:spec {:job 'jobs.items.obtain}}
               "j4" {:spec {:job 'jobs.gather.mine}}}})

(deftest a-listed-job-that-waits-or-failed-does-not-hold-a-standing-job
  (is (= ["j3" "j4"] (standing/runnable-others (ctx-with listed {}))))
  (is (= ["j4"] (standing/runnable-others (ctx-with listed {"j3" {:reason :no-pick}}))))
  (is (= ["j3"] (standing/runnable-others (ctx-with (assoc listed :failed {"j4" {:error "x"}}) {})))))

(deftest with-nothing-else-listed-the-body-walks-to-the-site-whatever-the-distance
  (is (nil? (standing/away-cause (ctx-with (assoc listed :list ["j1" "j2"]) {}) {:x 5000 :y 0 :z 0} (pass 0 0))))
  (is (= :away (:reason (standing/away-cause (ctx-with listed {"j3" {:reason :w}}) {:x 1 :y 1 :z 1} (pass 1000 0))))))

(def trip-waits {"j3" (told :exposed 60000 10000) "j4" (told :exposed 60000 10000)})

(deftest a-short-wait-next-to-a-far-site-keeps-the-body-put
  (is (= :away (:reason (standing/away-cause (ctx-with listed (assoc trip-waits "j3" (told :exposed 20000 10000))) {:x 100 :y 0 :z 0} (pass 10 0))))))

(deftest a-long-wait-is-read-from-its-declared-length-not-its-recheck
  (let [long-wait (with-meta {:reason :exposed :recheck-after-ms 10000 :wait-ms 300000} {:t 10000})]
    (is (nil? (standing/away-cause (ctx-with listed {"j3" long-wait "j4" long-wait}) {:x 100 :y 0 :z 0} (pass 10 0))))))

(deftest a-risky-walk-polled-every-10-s-is-not-a-10-s-wait
  (is (nil? (standing/away-cause (ctx-with listed {"j3" (polled :risky 10000 10000) "j4" (polled :risky 10000 10000)}) {:x 100 :y 0 :z 0} (pass 10 0)))))

(deftest a-wait-of-unknown-length-is-priced-at-the-prior-not-dropped
  (is (= :away (:reason (standing/away-cause (ctx-with listed {"j3" (told :exposed 300000 10000) "j4" {:reason :hostile-near}}) {:x 200 :y 0 :z 0} (pass 10 0))))))

(deftest the-pass-work-time-counts-in-the-delay
  (are [work-s expected] (= expected (:reason (standing/away-cause (ctx-with listed trip-waits) {:x 30 :y 0 :z 0} (pass 10 work-s))))
    0 nil
    100 :away))

(deftest the-delay-is-weighed-against-what-the-pass-is-worth
  (are [worth expected] (= expected (:reason (standing/away-cause (ctx-with listed (assoc trip-waits "j3" (told :exposed 20000 10000))) {:x 100 :y 0 :z 0} (pass worth 0))))
    50 nil
    30 :away))

(deftest the-wait-left-shrinks-with-the-time-since-it-was-told
  (is (= 20 (standing/wait-left-s (ctx-with listed {"j3" (told :exposed 30000 0)}) "j3")))
  (is (= cost/unknown-wait-s (standing/wait-left-s (ctx-with listed {"j3" {:reason :no-pick}}) "j3")))
  (is (nil? (standing/wait-left-s (ctx-with listed {}) "j3"))))
