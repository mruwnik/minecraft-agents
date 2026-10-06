(ns agent-tools.drive
  "Manual control of a body over its control socket: drive.mjs <agent> <op> ..."
  (:require [agent-tools.http :as http]
            [agent-tools.map :as map-tool]
            [engine.bodies :as bodies]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:path" :as path]))

(def controls ["forward" "back" "left" "right" "jump" "sneak" "sprint"])

(def usage
  (str "usage: drive.mjs <agent> <op> [args] --world <world> [--worlds <dir>] [--state <legacy-parent>]\n"
       "  take --who NAME --why \"<text>\" --idle-s <n> | hold <control>[,<control>...] <ms> | look <yaw> <pitch>\n"
       "  turn <dyaw> [dpitch] | jump | stop | ping | state | release [--force]\n"
       "  controls: " (str/join " " controls)))

(defn socket-path-for [{:keys [state world agent]}]
  (.join path (bodies/body-dir state world agent) "engine" "control.sock"))

(def options
  {:who {:type "string" :default "claude"}
   :state {:type "string"} :worlds {:type "string"}
   :world {:type "string"}
   :why {:type "string" :default ""}
   :force {:type "boolean" :default false}
   :idle-s {:type "string"}})

;; parseArgs would read "-10" as a short option, so hide negative numbers from it
(defn negative-number? [token] (boolean (re-matches #"-\d+(\.\d+)?" token)))
(defn hide [token] (if (negative-number? token) (str (char 0) token) token))
(defn unhide [token] (if (str/starts-with? token (char 0)) (subs token 1) token))

(defn num [text]
  (when (and (some? text) (not= "" (str/trim text)) (js/Number.isFinite (js/Number text)))
    (js/Number text)))

(defn post [body] {:method "POST" :path "/drive" :body body})

(def builders
  {"take" (fn [{:keys [who why idle-s]}]
            (if (nil? idle-s)
              (post {:op "take" :who who :why why})
              (if-let [idle (num idle-s)]
                (post {:op "take" :who who :why why :idleS idle})
                {:error "--idle-s must be a number"})))
   "stop" (fn [{:keys [who]}] (post {:op "stop" :who who}))
   "ping" (fn [{:keys [who]}] (post {:op "ping" :who who}))
   "release" (fn [{:keys [who force]}] (post (cond-> {:op "release" :who who} force (assoc :force true))))
   "state" (fn [_] {:method "GET" :path "/drive" :body nil})
   "jump" (fn [{:keys [who]}] (post {:op "set" :who who :controls {:jump true} :ms 300}))
   "hold" (fn [{:keys [who args]}]
            (let [[names ms-text] args]
              (if (or (nil? names) (nil? ms-text))
                {:error "hold needs <control>[,<control>...] <ms>"}
                (let [listed (vec (array-seq (.split names ",")))
                      bad (first (remove (set controls) listed))
                      ms (num ms-text)]
                  (cond
                    (some? bad) {:error (str "unknown control " bad)}
                    (not (and ms (js/Number.isInteger ms))) {:error "ms must be an integer"}
                    :else (post {:op "set" :who who :controls (into {} (map (fn [c] [(keyword c) true])) listed) :ms ms}))))))
   "look" (fn [{:keys [who args]}]
            (let [[yaw pitch] (map num args)]
              (if (or (nil? yaw) (nil? pitch))
                {:error "look needs <yaw> <pitch> (numbers)"}
                (post {:op "set" :who who :look {:yaw yaw :pitch pitch}}))))
   "turn" (fn [{:keys [who args]}]
            (let [dyaw (num (first args))
                  dpitch (if (nil? (second args)) 0 (num (second args)))]
              (if (or (nil? dyaw) (nil? dpitch))
                {:error "turn needs <dyaw> [dpitch] (numbers)"}
                (post {:op "set" :who who :look {:dyaw dyaw :dpitch dpitch}}))))})

(defn parse [argv]
  (try (map-tool/parse-options (mapv hide argv) options)
       (catch :default e {:error (.-message e)})))

(defn request-for [argv]
  (let [parsed (parse argv)]
    (if (:error parsed)
      parsed
      (let [{:keys [positionals values]} parsed
            [agent op & args] (map unhide positionals)
            world (:world values)]
        (cond
          (or (nil? agent) (nil? op)) {:error "need <agent> and <op>"}
          (nil? world) {:error (bodies/missing-world-error "--world")}
          (not (and (re-matches bodies/name-re world) (re-matches bodies/name-re agent)))
          {:error "the agent and --world must be names of letters, digits, _ and -"}
          (not (contains? builders op)) {:error (str "unknown op " op)}
          :else
          (let [req ((builders op) (assoc values :args (vec args)))]
            (if (:error req)
              req
              (try (merge {:agent agent :world world :state (bodies/storage-root values map-tool/default-state-dir)} req)
                   (catch :default e {:error (.-message e)})))))))))

(defn exit-code-for [{:keys [status json]}]
  (if (and (>= status 200) (< status 300) (true? (:ok json))) 0 1))

(defn parse-json [text]
  (try (js->clj (js/JSON.parse text) :keywordize-keys true) (catch :default _ nil)))

(def timeout-ms 3000)
(def max-response-bytes 65536)

(defn request-options [socket-path {:keys [method path body]}]
  {:socket-path socket-path :method method :path path :label "drive"
   :headers {"content-type" "application/json"}
   :timeout-ms timeout-ms :max-bytes max-response-bytes
   :body (when (some? body) (js/JSON.stringify (clj->js body)))})

(defn send! [socket-path req] (http/request (request-options socket-path req)))

(defn no-body-text [agent socket]
  (str "no running body " agent " ("
       (if (.existsSync fs socket)
         (str "connection failed at " socket)
         (str "no control socket at " socket))
       ")"))

(defn failure-text [error agent socket]
  (case (aget error "code")
    "ETIMEDOUT" (str "drive request to " agent " timed out after 3 s")
    "ERESPONSETOOLARGE" (str "drive response from " agent " exceeded 64 KB")
    "ECONNRESET" (str "connection to " agent " was reset")
    "ENOENT" (no-body-text agent socket)
    "ECONNREFUSED" (no-body-text agent socket)
    (str "drive request to " agent " failed (" (or (aget error "code") (.-message error)) ")")))

(defn main!
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv]
   (let [req (request-for argv)]
     (if (:error req)
       (do (js/console.error (str (:error req) "\n" usage)) (js/Promise.resolve 2))
       (let [socket (socket-path-for req)]
         (.then (send! socket req)
                (fn [{:keys [status text]}]
                  (js/console.log text)
                  (exit-code-for {:status status :json (parse-json text)}))
                (fn [error]
                  (js/console.error (failure-text error (:agent req) socket))
                  2)))))))
