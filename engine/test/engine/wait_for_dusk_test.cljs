(ns engine.wait-for-dusk-test
  "jobs.time.wait-for-dusk against the fake world: it declines until evening, done at once when started late."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]
            [jobs.time.wait-for-dusk :as dusk]))

(def job 'jobs.time.wait-for-dusk)

(defn ^:async listed-after-tick [time]
  (let [s (h/setup {:time time})]
    (core/submit! (:eng s) (list job) {})
    (swap! (:clock s) + 700)
    (await (core/tick! (:eng s)))
    (:list (core/state (:eng s)))))

(deftest check-is-true-from-the-start-of-dusk-through-the-night
  (are [time ready] (= ready (boolean (dusk/check {:primitives (tu/fake {:time time})})))
    1000 false
    11999 false
    12000 true
    13000 true
    23999 true
    0 false))

(deftest declines-by-day-and-is-done-in-the-evening
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= ["j1"] (await (listed-after-tick 6000))) "day: still waiting")
        (is (= [] (await (listed-after-tick 12000))) "dusk: done")
        (is (= [] (await (listed-after-tick 18000))) "already night at the start: done at once")))))
