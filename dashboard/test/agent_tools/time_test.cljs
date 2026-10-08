(ns agent-tools.time-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.time :as time]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(defn world-with-pose [pose]
  (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "clock-test-"))
        view (.join path dir "agents" "Bob" "view")]
    (.mkdirSync fs view #js {:recursive true})
    (.writeFileSync fs (.join path view "pose.json") (js/JSON.stringify (clj->js pose)))
    dir))

(def pose {:world "w" :status "online" :dimension "overworld" :t 1000000 :timeOfDay 13398})

(defn clock-of [pose now]
  (let [dir (world-with-pose pose)
        result (time/clock {:world "w" :world-dir dir} now)]
    (.rmSync fs dir #js {:recursive true :force true})
    result))

(deftest a-fresh-online-reading-gives-the-time-and-the-night-line
  (let [night (clock-of pose 1001000)
        day (clock-of (assoc pose :timeOfDay 12541) 1001000)
        dusk (clock-of (assoc pose :timeOfDay 12542) 1001000)]
    (is (= [true 13398 false] ((juxt :ok :time-of-day :day?) night)))
    (is (true? (:day? day)))
    (is (false? (:day? dusk)))))

(deftest a-stale-or-offline-reading-is-no-clock
  (is (= {:ok false :reason :time-unknown} (select-keys (clock-of pose (+ 1000000 time/stale-ms 1)) [:ok :reason])))
  (is (false? (:ok (clock-of (assoc pose :status "offline") 1001000)))))

(deftest a-dawn-result-without-a-checkpoint-or-body-gets-no-missed
  (async done
    (-> (js/Promise.all #js [(time/with-missed! {:world-dir "/nonexistent"} {:ok true :wake :day})
                             (time/with-missed! {:world-dir "/nonexistent"} {:ok true :wake :day :by "Bob"})])
        (.then (fn [rs] (is (= [{:ok true :wake :day} {:ok true :wake :day :by "Bob"}] (vec rs)))))
        (.finally done))))
