(ns agent-tools.say
  "Fast control-adjacent public chat and whisper command validation."
  (:require [engine.bodies :as bodies]
            [agent-tools.http :as http]
            [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            [clojure.string :as str]
            ["node:path" :as path]))

(def usage "usage: say.mjs <body> --world <world> <message> [--to <player>] [--worlds <dir> --state <legacy-parent>]")

(defn request-for [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options (vec argv)
          {:state {:type "string"} :worlds {:type "string"}
           :world {:type "string"} :to {:type "string"}})
          [body message & extra] positionals
          world (:world values)
          to (:to values)
          cleaned (some-> message str (str/replace #"[\x00-\x1f\x7f]" " ") (str/replace "§" "") str/trim)
          limit (if to (- 256 (count (str "/tell " to " "))) 256)]
      (when-not (and (string? body) (re-matches #"[A-Za-z0-9_-]{1,40}" body))
        (throw (js/Error. "body must be a valid name")))
      (when-not (and (string? world) (re-matches #"[A-Za-z0-9_-]{1,64}" world))
        (throw (js/Error. "missing or invalid --world <world>")))
      (when (seq extra) (throw (js/Error. "message must be one shell-quoted argument")))
      (when-not (and cleaned (not (empty? cleaned))) (throw (js/Error. "message must not be empty")))
      (when (str/starts-with? cleaned "/") (throw (js/Error. "message cannot start with /")))
      (when (and to (not (re-matches #"[A-Za-z0-9_]{3,16}" to)))
        (throw (js/Error. "--to must be a Minecraft player name (3-16 letters, digits, or _)")))
      (when (> (count cleaned) limit)
        (throw (js/Error. (str "message is longer than " limit " characters"))))
      (let [state (bodies/storage-root values map-tool/default-state-dir)]
        {:body body :world world :message cleaned :to to :state state
         :socketPath (.join path (bodies/worlds-dir state) world "agents" body "engine" "events.sock")}))
    (catch :default error {:error (.-message error)})))

(def chat-timeout-ms 10000)
(def max-response-bytes 65536)

(defn failure-for [error]
  (case (aget error "code")
    ("EPERM" "EACCES")
    {:ok false :reason :socket-access-denied
     :message "Permission denied connecting to the body event socket; the message was not sent."}
    ("ENOENT" "ECONNREFUSED")
    {:ok false :reason :no-running-body
     :message "The body is not running (no event socket answers); the message was not sent."}
    {:ok false :reason :transport-error :confirmation :unknown
     :message "Chat confirmation is unknown; inspect server chat before sending again."}))

(defn print-edn! [value] (.write (.-stdout js/process) (str (data/write-edn value) "\n")))

(defn send! [{:keys [socketPath message to]}]
  (http/request {:socket-path socketPath :method "POST" :path "/chat" :label "say"
                 :headers {"content-type" "application/edn"}
                 :body (data/write-edn (cond-> {:message message} to (assoc :to to)))
                 :timeout-ms chat-timeout-ms :max-bytes max-response-bytes}))

(defn deliver! [{:keys [status content-type text]}]
  (when-not (http/edn-response? content-type) (throw (js/Error. "unexpected response format")))
  (.write (.-stdout js/process) (if (str/ends-with? text "\n") text (str text "\n")))
  (if (= 200 status) 0 1))

(defn main!
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv]
   (let [request (request-for argv)]
     (if (:error request)
       (do (print-edn! {:ok false :reason :bad-args :message (:error request)}) (js/Promise.resolve 2))
       (-> (send! request)
           (.then deliver!)
           (.catch (fn [error] (print-edn! (failure-for error)) 2)))))))
