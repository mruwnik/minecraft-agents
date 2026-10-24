(ns engine.standing-test
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.standing :as standing]))

(defn ctx-with [state waiting]
  {:id "j1/tend" :engine {:state (atom state) :waiting (atom waiting)}})

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

(deftest away-needs-a-runnable-other-job-whatever-the-distance
  (is (nil? (standing/away-cause (ctx-with listed {"j3" {:reason :w} "j4" {:reason :w}}) {:x 5000 :y 0 :z 0})))
  (is (= :away (:reason (standing/away-cause (ctx-with listed {"j3" {:reason :w}}) {:x 1 :y 1 :z 1})))))
