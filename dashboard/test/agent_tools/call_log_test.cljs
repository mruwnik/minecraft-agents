(ns agent-tools.call-log-test
  (:require [cljs.test :refer [deftest is are]]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [agent-tools.call-log :as call-log]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(defn tmp-ctx []
  (let [worlds (.mkdtempSync fs (.join path (.tmpdir os) "call-log-"))]
    (.mkdirSync fs (.join path worlds "w" "agents" "Probe") #js {:recursive true})
    {:worlds worlds :world "w" :body "Probe"}))

(defn lines [ctx]
  (mapv reader/read-string (str/split-lines (.readFileSync fs (call-log/log-file ctx) "utf8"))))

(deftest a-call-writes-start-and-end-lines
  (let [ctx (tmp-ctx)
        call (call-log/begin! ctx "observe" ["--wait" "--observer" "Probe"])
        _ (call-log/finish! call 0 "{:ok true :wake :chat :from \"x\"}")
        [start end] (lines ctx)]
    (is (= {:t :start :tool "observe" :argv ["--wait" "--observer" "Probe"] :observer "Probe"}
           (select-keys start [:t :tool :argv :observer])))
    (is (= {:t :end :exit 0 :ok true :wake :chat} (select-keys end [:t :exit :ok :wake])))
    (is (= (:id start) (:id end)))
    (is (<= (:ms start) (:ms end)))))

(deftest a-failing-call-records-the-exit-and-no-wake
  (let [ctx (tmp-ctx)
        _ (call-log/finish! (call-log/begin! ctx "jobs" ["list"]) 2 "{:ok false}")
        end (second (lines ctx))]
    (is (= {:exit 2 :ok false} (select-keys end [:exit :ok])))
    (is (not (contains? end :wake)))))

(deftest an-unwritable-log-never-throws
  (let [blocker (.join path (:worlds (tmp-ctx)) "file")
        _ (.writeFileSync fs blocker "")
        ctx {:worlds blocker :world "w" :body "Probe"}]
    (is (= 0 (do (call-log/finish! (call-log/begin! ctx "say" ["hi"]) 0 "") 0)))))

(deftest the-observer-comes-from-either-option-form
  (are [argv want] (= want (call-log/observer-of argv))
    ["--observer" "A"] "A"
    ["--observer=B" "x"] "B"
    ["--" "--observer" "C"] nil
    ["x"] nil))

(deftest a-missing-body-folder-is-not-created
  (let [ctx (assoc (tmp-ctx) :body "Ghost")]
    (call-log/finish! (call-log/begin! ctx "say" ["hi"]) 0 "")
    (is (not (.existsSync fs (.join path (:worlds ctx) "w" "agents" "Ghost"))))))
