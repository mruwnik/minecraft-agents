(ns agent-tools.observe.request
  "observe.mjs usage, option parsing and the request each command makes."
  (:require [engine.bodies :as bodies]
            [agent-tools.world-data :as data]
            [agent-tools.http :as http]
            [agent-tools.map :as map-tool]
            ["node:path" :as path]
            [clojure.string :as str]
            [agent-tools.observe.status :refer [truthy-text?]]))

(def usage "usage: observe.mjs <agent> --world <world> [status [--raw|--verbose] [--wait --timeout 60s --chatter addressed --observer agent --watch j12 --watch-action move-home] | inventory [--raw] [--slots] | equipment [--raw] | job <id> | result <id> | catalog <job|trigger> <name> | catalog <jobs|triggers> [prefix]] [--limit <n>] [--offset <n>] [--worlds <dir>] [--state <legacy-parent>]\nstatus: :pos [x y z], :health and :food 0-20, :current the running job, :mode scheduled or manual. job <id>: its state and outcome; an unknown id answers :unknown-job.")

;; Requests

(def body-name #"[A-Za-z0-9_-]{1,40}")
(def world-name #"[A-Za-z0-9_-]{1,64}")

(def option-spec
  {:state {:type "string"} :worlds {:type "string"} :world {:type "string"}
   :limit {:type "string"} :offset {:type "string"}
   :raw {:type "boolean" :default false} :slots {:type "boolean" :default false}
   :verbose {:type "boolean" :default false} :wait {:type "boolean" :default false}
   :timeout {:type "string"} :chatter {:type "string"} :observer {:type "string"}
   :watch {:type "string" :multiple true} :watch-action {:type "string" :multiple true}
   :from {:type "string"} :poll-ms {:type "string"}
   :danger {:type "boolean" :default false} :disconnect {:type "boolean" :default false}})

(defn fail [message] (throw (ex-info message {::error message})))

(defn check [bad? message] (when bad? (fail message)))

(defn integer-in? [n low high] (and (js/Number.isInteger n) (<= low n high)))

(defn limit-param
  "The validated --limit as a query value (nil when absent)."
  [limit high]
  (when (some? limit)
    (let [n (js/Number limit)]
      (check (not (integer-in? n 1 high)) (str "--limit must be an integer from 1 to " high))
      (str n))))

(defn listed [values] (vec (mapcat #(str/split % #",") values)))

(def duration-units {"ms" 1 "s" 1000 "m" 60000})

(defn wait-options [{:keys [timeout observer chatter watch watch-action from poll-ms danger disconnect]}]
  (let [[_ amount unit] (re-matches #"^(\d+(?:\.\d+)?)(ms|s|m)?$" (or timeout "60s"))
        timeout-ms (if amount (* (js/Number amount) (duration-units (or unit "s"))) js/NaN)
        observer (or observer "agent")
        chatter (or chatter "addressed")
        watch (listed (some-> watch array-seq))
        watch-actions (listed (some-> watch-action array-seq))
        poll (js/Number (or poll-ms 250))]
    (check (not (and (js/Number.isFinite timeout-ms) (<= 10 timeout-ms 3600000))) "--timeout must be between 10ms and 60m")
    (check (not (re-matches body-name observer)) "--observer must be 1-40 letters, digits, underscores or hyphens")
    (check (not (#{"none" "addressed" "all"} chatter)) "--chatter must be none, addressed, or all")
    (check (or (some #(not (re-matches #"j[0-9]+" %)) watch) (> (count watch) 32)) "--watch needs up to 32 comma-separated job IDs")
    (check (or (some #(not (re-matches #"[A-Za-z0-9_.:-]{1,80}" %)) watch-actions) (> (count watch-actions) 32))
           "--watch-action needs up to 32 comma-separated action request IDs")
    (check (and (truthy-text? from) (not (re-matches body-name from))) "--from must be a player name")
    (check (not (integer-in? poll 50 5000)) "--poll-ms must be 50-5000")
    {:timeout-ms timeout-ms :observer observer :chatter chatter :watch watch :watch-actions watch-actions
     :poll-ms poll :from from :danger danger :disconnect disconnect}))

(defn endpoint-for
  "[endpoint params] of the operation, validating its arguments."
  [op kind-or-id rest values]
  (case op
    "status"
    (do (check (or (some? kind-or-id) (seq rest)) "status takes no positional arguments")
        (check (some? (:offset values)) "--offset is only valid for catalog lists")
        (check (and (:raw values) (some? (:limit values))) "--limit cannot be combined with --raw")
        [(if (:raw values) "/snapshot" "/status")
         (if-let [limit (limit-param (:limit values) 32)] [["limit" limit]] [])])

    ("inventory" "equipment")
    (do (check (or (some? kind-or-id) (seq rest)) (str op " takes no positional arguments"))
        (check (or (some? (:limit values)) (some? (:offset values))) (str "--limit and --offset are not valid for " op))
        (check (and (:raw values) (:slots values)) "--slots is redundant with --raw")
        (check (and (= "equipment" op) (:slots values)) "--slots is only valid for inventory")
        ["/inventory" []])

    ("job" "result")
    (do (check (or (not (truthy-text? kind-or-id)) (seq rest)) (str op " needs one job ID, such as j12"))
        (check (and (= "result" op) (not (re-matches #"j[0-9]+" kind-or-id))) "result needs a job ID such as j12")
        (check (and (= "result" op) (some? (:limit values))) "result has a fixed bounded history; --limit is not accepted")
        (check (or (:raw values) (some? (:offset values))) "--raw and --offset are only valid for status and catalog lists respectively")
        ["/job" (into [["id" kind-or-id]] (when-let [limit (limit-param (:limit values) 32)] [["limit" limit]]))])

    "catalog"
    (cond
      (#{"job" "trigger"} kind-or-id)
      (let [[name] rest]
        (check (or (:raw values) (some? (:limit values)) (some? (:offset values))) "catalog detail does not accept --raw, --limit, or --offset")
        (check (or (not (truthy-text? name)) (not= 1 (count rest))) "catalog needs job <jobs.namespace.name> or trigger <trigger-name>")
        (check (and (= "job" kind-or-id) (not (re-matches #"jobs(?:\.[a-z][a-z0-9-]*)+" name))) "job name must be an exact jobs namespace")
        (check (and (= "trigger" kind-or-id) (not (re-matches #"[a-z][a-z0-9-]*" name))) "trigger name must be a lowercase identifier")
        ["/catalog" [["kind" kind-or-id] ["name" name]]])

      (#{"jobs" "triggers"} kind-or-id)
      (let [prefix (or (first rest) "")
            limit (if (nil? (:limit values)) 20 (js/Number (:limit values)))
            offset (if (nil? (:offset values)) 0 (js/Number (:offset values)))]
        (check (:raw values) "--raw is only valid for status")
        (check (> (count rest) 1) "catalog list accepts at most one prefix")
        (check (and (= "jobs" kind-or-id) (truthy-text? prefix) (not (re-matches #"jobs(?:\.[a-z][a-z0-9-]*)*(?:\.)?" prefix)))
               "job prefix must start with jobs.")
        (check (and (= "triggers" kind-or-id) (truthy-text? prefix) (not (re-matches #"[a-z][a-z0-9-]*" prefix)))
               "trigger prefix must be a lowercase identifier prefix")
        (check (not (integer-in? limit 1 64)) "--limit must be an integer from 1 to 64")
        (check (not (integer-in? offset 0 10000)) "--offset must be an integer from 0 to 10000")
        ["/catalog" [["kind" kind-or-id] ["prefix" prefix] ["limit" (str limit)] ["offset" (str offset)]]])

      :else (fail "catalog needs job|trigger <name> or jobs|triggers [prefix]"))

    (fail (str "unknown operation " op))))

(defn query-string [params]
  (let [search (js/URLSearchParams.)]
    (doseq [[k v] params] (.set search k v))
    (.toString search)))

(defn request-for
  "The request an argv describes: {:agent :world :state :socket-path :path} plus :wait-options, :inventory-mode with
  :slots and :raw, and :verbose when asked for; {:error message} when it is not valid."
  [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options argv option-spec)
          [agent requested-op kind-or-id & rest] positionals
          op (or requested-op "status")
          world (:world values)]
      (check (and (:slots values) (not= "inventory" op)) "--slots is only valid for inventory")
      (check (not (and agent (re-matches body-name agent))) "agent must be a body name")
      (check (nil? world) (bodies/missing-world-error "--world"))
      (check (not (re-matches world-name world)) "the world must be a name of letters, digits, _ and -")
      (let [state (bodies/storage-root values map-tool/default-state-dir)
            [endpoint params] (endpoint-for op kind-or-id rest values)
            wait-options (when (:wait values)
                           (check (or (not= "status" op) (:raw values) (:verbose values) (truthy-text? (:limit values)))
                                  "--wait is only valid with compact status")
                           (wait-options values))
            query (query-string params)]
        (when-not (:wait values)
          (check (or (some #(some? (get values %)) [:timeout :chatter :observer :watch :watch-action :from :poll-ms])
                     (:danger values) (:disconnect values))
                 "wait options require --wait"))
        (check (and (:verbose values) (or (not= "status" op) (:raw values)))
               "--verbose is only valid for status without --raw")
        (cond-> {:agent agent :world world :state state
                 :socket-path (.join path (bodies/body-dir state world agent) "engine" "events.sock")
                 :path (if (seq query) (str endpoint "?" query) endpoint)}
          wait-options (assoc :wait-options wait-options)
          (= "result" op) (assoc :result-id kind-or-id)
          (#{"inventory" "equipment"} op) (assoc :inventory-mode (keyword op) :slots (:slots values) :raw (:raw values))
          (:verbose values) (assoc :verbose true))))
    (catch :default error
      {:error (or (some-> (ex-data error) ::error) (.-message error))})))

(defn legacy-notice [request]
  (let [endpoint (first (str/split (:path request) #"\?"))
        fallback (if (= "/inventory" endpoint)
                   ""
                   (str " :fallback {:op :status :raw true :world " (js/JSON.stringify (:world request))
                        " :state " (data/write-edn (:state request)) "}"))]
    (str "{:ok false :reason :observe-unavailable :body " (js/JSON.stringify (:agent request))
         " :endpoint " (js/JSON.stringify endpoint) " :action :restart-with-current-build" fallback "}")))

(def unsupported-route? http/unsupported-route?)
