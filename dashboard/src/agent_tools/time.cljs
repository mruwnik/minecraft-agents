(ns agent-tools.time
  "Read the world's newest connected Overworld observer; never start a body."
  (:require [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            [agent-tools.http :as http]
            [agent-tools.snapshot :as snap]
            [agent-tools.observe.missed :refer [missed!]]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:timers/promises" :refer [setTimeout]]))

(def usage "time.mjs --world WORLD clock|dawn [--worlds DIR --state LEGACY_PARENT --body BODY --timeout 1200 --poll-ms 1000]\nclock: the asking body's reading (a trial body never reads a Probe* world-test body); :time-of-day in ticks (0-24000; 13000-23000 is night), :day? true by day, :age-ms how old the reading is (both commands wake an unwatched online body whose pose is stale), :note when it is over 5 s old or at night (why a bed may not skip). dawn: waits until the morning (time of day 0-5999; in the evening it waits for the next dawn), then adds :missed (job ends, flights, fights, reflex ends, damage, deaths since that body's observer last looked).")
(def stale-ms 90000)

(defn options [argv]
  (let [{:keys [positionals values]} (map-tool/parse-options argv
        {:world {:type "string"} :body {:type "string"} :state {:type "string"} :worlds {:type "string"}
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
    {:ctx (data/context (select-keys values [:state :worlds :world])) :command command :body (:body values)
     :timeout-ms (* 1000 timeout) :poll-ms poll-ms}))

(def night-note "Sleeping skips the night only when every online player sleeps; one awake player means no skip, the clock still runs.")

(defn noted
  "Say plainly how old a reading is when it is not fresh, and why a bed may not skip the night."
  [{:keys [age-ms day? by] :as reading}]
  (let [notes (cond-> []
                (> age-ms 5000) (conj (str "Reading is " (js/Math.round (/ age-ms 1000)) " s old (last seen by " by "); the real time of day is later."))
                (not day?) (conj night-note))]
    (cond-> reading (seq notes) (assoc :note (apply str (interpose " " notes))))))

(defn read-pose
  "The body's pose.json as a map, or {:why kw} when it cannot be used."
  [ctx body]
  (try
    (let [file (.join path (:world-dir ctx) "agents" body "view" "pose.json")]
      (if (> (.-size (.statSync fs file)) 1048576)
        {:why :pose-too-big}
        (js->clj (js/JSON.parse (.readFileSync fs file "utf8")) :keywordize-keys true)))
    (catch :default e {:why (if (= "ENOENT" (.-code e)) :no-pose :pose-unreadable)})))

(defn why-unusable
  "Why a pose gives no clock reading (nil when it does). Offline pose timestamps are disconnect times, not observations."
  [ctx pose now]
  (let [tick (:timeOfDay pose) seen (:t pose)]
    (cond
      (:why pose) (:why pose)
      (not= (:world ctx) (:world pose)) :other-world
      (not= "online" (:status pose)) :offline
      (not (#{"overworld" "minecraft:overworld"} (:dimension pose))) :other-dimension
      (not (and (js/Number.isFinite tick) (<= 0 tick) (< tick 24000))) :no-time-of-day
      (not (and (js/Number.isFinite seen) (<= 0 (- now seen) stale-ms))) :stale)))

(defn report [ctx body now]
  (let [pose (read-pose ctx body)]
    (when-not (why-unusable ctx pose now)
      {:ok true :world (:world ctx) :time-of-day (:timeOfDay pose) :day? (or (< (:timeOfDay pose) 12542) (> (:timeOfDay pose) 23460))
       :seen-at (:t pose) :age-ms (- now (:t pose)) :by body})))

(defn body-names [ctx]
  (try (->> (array-seq (.readdirSync fs (.join path (:world-dir ctx) "agents") #js {:withFileTypes true}))
            (filter #(and (.isDirectory %) (re-matches #"[A-Za-z0-9_-]{1,64}" (.-name %))))
            (map #(.-name %)))
       (catch :default e (if (= "ENOENT" (.-code e)) [] (throw e)))))

(defn probe? [body] (str/starts-with? (str body) "Probe"))

(defn clock
  "The asking body's reading when it has a fresh one, else the newest body's. A world-test probe body is read only
  for a probe asker or no asker: its cases set the server time, so a trial body would see the clock jump.
  Without a reading, :bodies says why each body gave none."
  [ctx now asker]
  (let [bodies (cond->> (body-names ctx) (and asker (not (probe? asker))) (remove probe?))]
    (or (some-> (first (sort-by (juxt (comp not #(= asker (:by %))) (comp - :seen-at) :by) (keep #(report ctx % now) bodies))) noted)
        {:ok false :world (:world ctx) :reason :time-unknown
         :bodies (into {} (map (fn [b] [b (why-unusable ctx (read-pose ctx b) now)])) bodies)})))

(defn with-missed!
  "A dawn result with what happened to the asking body (else the one that gave the clock) since its observer last looked (:missed)."
  [ctx asker {:keys [by] :as result}]
  (if-not (or asker by)
    (js/Promise.resolve result)
    (let [get! (fn [socket-path path options] (http/request (assoc options :socket-path socket-path :path path :label "time" :timeout-ms 3000)))]
      (-> (missed! get! (.join path (:world-dir ctx) "agents" (or asker by) "engine" "events.sock")
                   (.join path (:world-dir ctx) "observers" (or asker by) "agent.edn"))
          (.then #(cond-> result % (assoc :missed %)))))))

(defn dawn-reached?
  "Whether the day just began: ticks 0..5999. Evening ticks and the last ticks before the sun wraps still wait."
  [tick] (< tick 6000))

(defn online-watchable
  "The online bodies whose pose a time answer relies on: the asker, else every body. A body writes pose.json only while its watching marker is fresh."
  [ctx body]
  (filter #(= "online" (:status (read-pose ctx %))) (if body [body] (body-names ctx))))

(defn ^:async wake-stale!
  "Every command reads through this: online bodies whose pose is stale are marked watched and waited for, so a running body that nobody watches is not read as stale."
  [ctx body]
  (let [t (js/Date.now)
        stale (filter #(= :stale (why-unusable ctx (read-pose ctx %) t)) (online-watchable ctx body))]
    (doseq [b stale] (snap/touch-watch! ctx b t))
    (doseq [b stale]
      (await (snap/wait-for-dump! ctx b t #(js/Promise. (fn [ok] (js/setTimeout ok %))) #(js/Date.now))))))

(defn execute! [{:keys [ctx command timeout-ms poll-ms body]}]
  (let [started (js/Date.now) deadline (+ started timeout-ms)]
    (letfn [(poll []
              (let [now (js/Date.now)
                    _ (when (= command :dawn) (doseq [b (online-watchable ctx body)] (snap/touch-watch! ctx b now)))
                    result (clock ctx now body)]
                (cond
                  (= command :clock) (js/Promise.resolve result)
                  (not (:ok result)) (js/Promise.resolve (assoc result :waited-ms (- now started)))
                  (dawn-reached? (:time-of-day result)) (js/Promise.resolve (assoc result :wake :day :waited-ms (- now started)))
                  (>= now deadline) (js/Promise.resolve (assoc result :ok false :reason :timeout :waited-ms (- now started)))
                  :else (.then (setTimeout (min poll-ms (- deadline now))) (fn [_] (poll))))))]
      (.then (wake-stale! ctx body) (fn [_] (poll))))))

(defn main! [argv]
  (-> (js/Promise.resolve nil)
      (.then (fn [_] (let [opts (options (vec argv))]
                       (-> (execute! opts)
                           (.then #(if (= :dawn (:command opts)) (with-missed! (:ctx opts) (:body opts) %) %))))))
      (.then (fn [result] (.write (.-stdout js/process) (str (data/write-edn result) "\n")) (if (:ok result) 0 1)))
      (.catch (fn [error] (.write (.-stdout js/process) (str (data/write-edn (map-tool/error-result error)) "\n")) 2))))
