(ns agent-tools.arg-errors-test
  (:require [cljs.test :refer [deftest is are async]]
            [agent-tools.jobs :as jobs]
            [agent-tools.observe :as observe]
            [agent-tools.triggers :as triggers]
            [agent-tools.world :as world]
            [agent-tools.world-data :as data]))

(defn check-bad-args! [tool main! done]
  (let [lines (atom [])
        stderr (atom [])
        original js/console.error]
    (set! js/console.error #(swap! stderr conj %&))
    (-> (main! ["Bob"] {:output #(swap! lines conj %)})
        (.then (fn [code]
                 (set! js/console.error original)
                 (is (= 2 code) tool)
                 (is (empty? @stderr) tool)
                 (let [value (data/read-edn (apply str @lines))]
                   (is (= false (:ok value)) tool)
                   (is (= :bad-args (:reason value)) tool)
                   (is (string? (:message value)) tool)
                   (is (string? (:usage value)) tool))))
        (.catch (fn [e] (is false (str tool ": " e))))
        (.then (fn [_] (set! js/console.error original) (done))))))

(deftest jobs-bad-arguments-print-edn (async done (check-bad-args! "jobs" jobs/main! done)))
(deftest observe-bad-arguments-print-edn (async done (check-bad-args! "observe" observe/main! done)))
(deftest triggers-bad-arguments-print-edn (async done (check-bad-args! "triggers" triggers/main! done)))
(deftest world-bad-arguments-print-edn (async done (check-bad-args! "world" world/main! done)))
