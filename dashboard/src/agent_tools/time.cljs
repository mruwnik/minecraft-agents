(ns agent-tools.time
  "Read the world's newest connected Overworld observer; never start a body."
  (:require [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:timers/promises" :refer [setTimeout]]))

(def usage "time.mjs --world WORLD clock|dawn [--worlds DIR --state LEGACY_PARENT --timeout 1200 --poll-ms 1000]\nclock: :time-of-day in ticks (0-24000; 13000-23000 is night), :day? true by day, :age-ms how old the reading is. dawn: waits until day.")
(def stale-ms 90000)

(defn options [argv]
  (let [{:keys [positionals values]} (map-tool/parse-options argv
        {:world {:type "string"} :state {:type "string"} :worlds {:type "string"}
         :timeout {:type "string"} :poll-ms {:type "string"}})
        [command & extra] positionals
        command (keyword (or command "clock"))
        timeout (js/Number (or (:timeout values) 1200))
        poll-ms (js/Number (or (:poll-ms values) 1000))]
    (when (or (seq extra) (not (#{:clock :dawn} command))) (throw (data/fail :invalid-command usage)))
    (when (and (= command :clock) (or (:timeout values) (:poll-ms values)))
      (throw (data/fail :invalid-option "timeout and poll-ms apply only to dawn")))
    (when-not (and (js/Number.isFinite timeout) (<= 0 timeout 3600)
                   (js/Number.isInteger poll-ms) (<= 10 poll-ms 10000))
      (throw (data/fail :invalid-option "timeout must be 0..3600 seconds; poll-ms must be 10..10000")))
    {:ctx (data/context (select-keys values [:state :worlds :world])) :command command
     :timeout-ms (* 1000 timeout) :poll-ms poll-ms}))

(defn report [ctx body now]
  (try
    (let [file (.join path (:world-dir ctx) "agents" body "view" "pose.json")]
      (when (<= (.-size (.statSync fs file)) 1048576)
        (let [pose (js->clj (js/JSON.parse (.readFileSync fs file "utf8")) :keywordize-keys true)
              tick (:timeOfDay pose) seen (:t pose)]
          ;; Offline pose timestamps are disconnect times, not observations.
          (when (and (= (:world ctx) (:world pose)) (= "online" (:status pose))
                     (#{"overworld" "minecraft:overworld"} (:dimension pose))
                     (js/Number.isFinite tick) (<= 0 tick) (< tick 24000)
                     (js/Number.isFinite seen) (<= 0 (- now seen) stale-ms))
            {:ok true :world (:world ctx) :time-of-day tick :day? (or (< tick 12542) (> tick 23460))
             :seen-at seen :age-ms (- now seen) :by body}))))
    (catch :default _ nil)))

(defn clock [ctx now]
  (let [dir (.join path (:world-dir ctx) "agents")
        bodies (try (->> (array-seq (.readdirSync fs dir #js {:withFileTypes true}))
                         (filter #(and (.isDirectory %) (re-matches #"[A-Za-z0-9_-]{1,64}" (.-name %))))
                         (map #(.-name %)))
                    (catch :default e (if (= "ENOENT" (.-code e)) [] (throw e))))]
    (or (first (sort-by (juxt (comp - :seen-at) :by) (keep #(report ctx % now) bodies)))
        {:ok false :world (:world ctx) :reason :time-unknown})))

(defn execute! [{:keys [ctx command timeout-ms poll-ms]}]
  (let [started (js/Date.now) deadline (+ started timeout-ms)]
    (letfn [(poll []
              (let [now (js/Date.now) result (clock ctx now)]
                (cond
                  (= command :clock) (js/Promise.resolve result)
                  (not (:ok result)) (js/Promise.resolve (assoc result :waited-ms (- now started)))
                  (:day? result) (js/Promise.resolve (assoc result :wake :day :waited-ms (- now started)))
                  (>= now deadline) (js/Promise.resolve (assoc result :ok false :reason :timeout :waited-ms (- now started)))
                  :else (.then (setTimeout (min poll-ms (- deadline now))) (fn [_] (poll))))))]
      (poll))))

(defn main! [argv]
  (-> (js/Promise.resolve nil)
      (.then (fn [_] (execute! (options (vec argv)))))
      (.then (fn [result] (.write (.-stdout js/process) (str (data/write-edn result) "\n")) (if (:ok result) 0 1)))
      (.catch (fn [error] (.write (.-stdout js/process) (str (data/write-edn (map-tool/error-result error)) "\n")) 2))))
