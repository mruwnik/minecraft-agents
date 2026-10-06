(ns agent-tools.world
  "Typed requests to a body's world port: world.mjs <agent> <command> ..."
  (:require [agent-tools.drive :as drive]
            [agent-tools.http :as http]
            [agent-tools.map :as map-tool]
            [agent-tools.observe :as observe]
            [agent-tools.world-data :as data]
            [engine.bodies :as bodies]
            [clojure.string :as str]
            ["node:crypto" :as crypto]
            ["node:fs" :as fs]
            ["node:path" :as path]))

(def usage
  (str "usage: world.mjs <agent> <command> [args] --world <world> [--worlds <dir>] [--state <legacy-parent>]\n"
       "  submit move-to <x> <y> <z> [--range <n>] [--timeout-s <1..10>] [--max-distance <1..64>]\n"
       "    (walks like go-to, doors included; one bounded step: for a longer walk submit jobs.movement.go-to)\n"
       "  submit dig <x> <y> <z> | submit place <x> <y> <z> <item>\n"
       "  submit use-on <x> <y> <z> [--item <item>] [--face up|down|north|south|east|west]\n"
       "  submit interact <entity-id> [--item <item>] [--request-id <id>]\n"
       "  submit wear [<item>]   (puts a carried armour piece on; no item: the best carried piece for each empty or weaker slot)\n"
       "  status <request-id> | cancel <request-id> | inventory\n"
       "Acquire the body first: drive.mjs take --who NAME --why \"<text>\" --idle-s N (a not-driver refusal names the holder and its idle time left).\n"
       "Actions run one at a time in the order submitted: one submitted while another runs is queued behind it\n"
       "(:status :queued, :position <place in the queue>, :behind <the running action's request-id>, at most 8 waiting); cancel drops a queued one, a release drops them all.\n"
       "Submit returns at once with the request-id; add --wait [--timeout 60s] to block until that action ends (or until\n"
       "anything that ends observe --wait: addressed chat, attention, the timeout) and print {:operation .. :wait <the wake>},\n"
       "the wake carrying the action's result and a bounded summary of what else happened meanwhile."))

(def request-timeout-ms 3000)
(def max-response-bytes 65536)

(def options
  {:who {:type "string" :default "claude"} :state {:type "string"} :worlds {:type "string"} :world {:type "string"}
   :range {:type "string"} :timeout-s {:type "string"} :max-distance {:type "string"}
   :item {:type "string"} :face {:type "string"} :request-id {:type "string"}
   :wait {:type "boolean"} :timeout {:type "string"}})

(def action-options [:range :timeout-s :max-distance :item :face])

(defn fail [message] (throw (js/Error. message)))

(defn num [text name]
  (when-not (and (some? text) (not (str/blank? text)) (js/Number.isFinite (js/Number text)))
    (fail (str name " must be a finite number")))
  (js/Number text))

(defn pos-args [action args expected]
  (when-not (= expected (count args))
    (fail (str action " needs " (if (= expected 4) "x y z item" "x y z"))))
  {:pos {:x (num (nth args 0) "x") :y (num (nth args 1) "y") :z (num (nth args 2) "z")}})

(defn submit-args [action args {:keys [range timeout-s max-distance item face]}]
  (case action
    "move-to" (cond-> (pos-args action args 3)
                (some? range) (assoc :range (num range "--range"))
                (some? timeout-s) (assoc :timeoutS (num timeout-s "--timeout-s"))
                (some? max-distance) (assoc :maxDistance (num max-distance "--max-distance")))
    "dig" (pos-args action args 3)
    "place" (do (when (or (not= 4 (count args)) (some? item)) (fail "place needs x y z item"))
                (assoc (pos-args action args 4) :item (nth args 3)))
    "use-on" (cond-> (pos-args action args 3) (seq item) (assoc :item item) (seq face) (assoc :face face))
    "wear" (do (when (> (count args) 1) (fail "wear needs at most one item"))
               (if (seq args) {:item (first args)} {}))
    "interact" (do (when-not (= 1 (count args)) (fail "interact needs one entity-id"))
                   (cond-> {:id (num (first args) "entity-id")} (seq item) (assoc :item item)))
    (fail (str "unknown action " (or action "undefined")))))

(defn submit-body [action args who values]
  (let [allowed (case action "move-to" [:range :timeout-s :max-distance] "use-on" [:item :face] "interact" [:item] [])
        unsupported (first (filter (fn [k] (and (some? (k values)) (not (some #{k} allowed)))) action-options))
        _ (when unsupported (fail (str "--" (name unsupported) " is not valid for " action)))
        arguments (submit-args action args values)
        request-id (or (:request-id values) (.randomUUID crypto))]
    (when-not (re-matches #"[A-Za-z0-9._-]{1,80}" request-id)
      (fail "--request-id must be 1..80 letters, digits, dot, _ or -"))
    {:op :submit :who who :request-id request-id :action (keyword action) :args arguments}))

(defn command-body [command action args who values]
  (case command
    "inventory" {:op :inventory :who who}
    ("status" "cancel") (do (when (or (nil? action) (= "" action) (seq args)) (fail (str command " needs exactly one request-id")))
                            {:op (keyword command) :who who :request-id action})
    "submit" (submit-body action args who values)
    (fail (str "unknown command " command))))

(defn request-for-unsafe [argv]
  (let [{:keys [positionals values]} (map-tool/parse-options (mapv drive/hide argv) options)
        [agent command action & args] (map drive/unhide positionals)
        world (:world values)
        who (:who values)]
    (cond
      (not (and (string? agent) (re-matches #"[A-Za-z0-9_-]{1,40}" agent)))
      {:error "agent must be a body name (letters, digits, _ or -, max 40)"}
      (or (nil? command) (= "" command)) {:error "need <agent> and <command>"}
      (nil? world) {:error (bodies/missing-world-error "--world")}
      (not (re-matches bodies/name-re world)) {:error "the world must be a name of letters, digits, _ and -"}
      (not (<= 1 (count who) 80)) {:error "--who must be 1..80 characters"}
      (and (some? (:request-id values)) (not= command "submit")) {:error "--request-id is only valid for submit"}
      (and (not= command "submit") (some #(some? (% values)) action-options)) {:error "action options are only valid with submit"}
      (and (:wait values) (not= command "submit")) {:error "--wait requires submit"}
      (and (:timeout values) (not (:wait values))) {:error "--timeout requires --wait"}
      :else (do (when (:wait values) (observe/wait-options {:timeout (:timeout values)}))
                (cond-> {:agent agent :world world :state (bodies/storage-root values map-tool/default-state-dir) :who who
                         :body (command-body command action (vec args) who values)}
                  (:wait values) (assoc :wait {:timeout (:timeout values)}))))))

(defn request-for [argv]
  (try (request-for-unsafe argv) (catch :default error {:error (.-message error)})))

(defn body-edn [body] (data/write-edn body))

(defn send! [socket-path body request-fn]
  (http/request (cond-> {:socket-path socket-path :method "POST" :path "/world" :label "world"
                         :headers {"content-type" "application/edn"} :body (str (body-edn body) "\n")
                         :timeout-ms request-timeout-ms :max-bytes max-response-bytes}
                  request-fn (assoc :request-fn request-fn))))

(defn ^:async wait-after!
  "After an accepted submit, wait for its action as observe --wait --watch-action does: the answer with the wake under
  :wait."
  [parsed answer request-fn]
  (let [get! (fn [socket endpoint options]
               (observe/get! socket endpoint (cond-> options request-fn (assoc :request-fn request-fn))))
        base {:agent (:agent parsed) :world (:world parsed) :state (:state parsed)
              :socket-path (.join path (bodies/body-dir (:state parsed) (:world parsed) (:agent parsed)) "engine" "events.sock")}
        wake (await (observe/wait-for! base {:watch-actions [(get-in parsed [:body :request-id])]
                                             :timeout (get-in parsed [:wait :timeout])} get!))]
    (assoc answer :wait wake)))

;; The engine's not-driver refusal carries no detail: ask GET /drive who holds the body and add it.
(defn ^:async name-holder
  "A not-driver answer with :detail {:holder :idle-left-s} from the drive state; any other answer unchanged."
  [answer socket who request-fn]
  (if-not (= "not-driver" (data/name (:reason answer)))
    answer
    (let [state (try (let [response (await (http/request (cond-> {:socket-path socket :method "GET" :path "/drive" :label "drive"
                                                                 :timeout-ms request-timeout-ms :max-bytes max-response-bytes}
                                                          request-fn (assoc :request-fn request-fn))))]
                       (when (http/edn-response? (:content-type response)) (data/read-edn (:text response))))
                     (catch :default _ nil))
          holder (get-in state [:manual :who])]
      (if (and holder (not= holder who))
        (assoc answer :detail {:holder holder :idle-left-s (get-in state [:manual :idleLeftS])})
        answer))))

(defn no-body-text [socket-path]
  (if (.existsSync fs socket-path)
    "no running body (connection refused)"
    (str "no running body (no socket at " socket-path ")")))

(defn failure-text [error socket-path]
  (case (aget error "code")
    "ETIMEDOUT" "request timed out after 3s"
    "ERESPONSETOOLARGE" "engine response exceeded 64 KB"
    "ECONNRESET" "connection was reset"
    "ENOENT" (no-body-text socket-path)
    "ECONNREFUSED" (no-body-text socket-path)
    (str "request failed (" (or (aget error "code") (.-message error)) ")")))

(defn print-text! [text] (.write (.-stdout js/process) text))

(defn main!
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv] (main! argv {}))
  ([argv {:keys [request-fn output] :or {output print-text!}}]
   (let [parsed (request-for argv)]
     (if (:error parsed)
       (do (js/console.error (str (:error parsed) "\n" usage)) (js/Promise.resolve 2))
       (let [socket (drive/socket-path-for parsed)]
         (-> (send! socket (:body parsed) request-fn)
             (.then (fn [response] {:response response})
                    (fn [error] {:error error}))
             (.then
              (fn [{:keys [response error]}]
                (if error
                  (let [request-id (get-in parsed [:body :request-id])]
                    (js/console.error (str (failure-text error socket) "; command was not confirmed"
                                           (when request-id (str "; retry/query with --request-id " request-id))))
                    2)
                  (let [{:keys [status content-type text]} response
                        ok? (and (>= status 200) (< status 300))
                        answer (when (http/edn-response? content-type) (try (data/read-edn text) (catch :default _ nil)))]
                    (cond
                      (nil? answer)
                      (do (output (str "{:ok false :reason :world-unavailable :http-status " status
                                       " :hint \"restart body with the current engine build\"}\n"))
                          1)
                      (and (:wait parsed) ok? (:ok answer))
                      (.then (wait-after! parsed answer request-fn)
                             (fn [result] (output (str (data/write-edn result) "\n")) 0))
                      ok? (do (output text) 0)
                      :else (.then (name-holder answer socket (:who parsed) request-fn)
                                   (fn [named] (output (str (data/write-edn named) "\n")) 1)))))))
             (.catch (fn [error]
                       (js/console.error (str "the command was accepted, but reporting its result failed: "
                                              (or (aget error "code") (.-message error))))
                       2))))))))
