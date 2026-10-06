(ns dashboard.server.posts
  "The state-changing routes: chat and whisper send, restart, blueprint preview."
  (:require [dashboard.blueprint-library :as blueprint-lib]
            [dashboard.chat-send :as chat-send]
            [dashboard.edn :as edn]
            [dashboard.guard :as guard]
            [dashboard.rcon :as rcon]
            [dashboard.restart :as restart]
            [dashboard.server.files :refer [max-preview-bytes]]
            [dashboard.server.responses :refer [guarded-post! send-edn! send-json!]]
            [dashboard.server.engine-state :refer [bodies]]
            [dashboard.server.chat-log :refer [chat-sender]]))

;; ---------------------------------------------------------------- chat send (POST /api/chat/send)
;; dashboard.chat-send validates the body and builds the fixed tellraw command; the runner is RCON (dashboard.rcon),
;; or, with DASHBOARD_CHAT_DRY=1, one that only logs the command. The sender is DASHBOARD_CHAT_AS (default "dashboard").
(def chat-dry? (= "1" (.-DASHBOARD_CHAT_DRY js/process.env)))
(def chat-stamps (atom []))

(defn run-chat-command! [command]
  (if chat-dry?
    (do (println (str "chat send (dry run): " command)) (js/Promise.resolve "dry"))
    (rcon/send-command! command)))

(defn send-rcon-failure!
  "A fixed 502 body; the detail (paths, socket text) stays in the server log."
  [res e]
  (js/console.error (str "RCON failed: " (ex-message e)))
  (send-json! res 502 {:error "RCON failed"}))

(defn send-chat! [req res]
  (guarded-post!
   req res guard/max-body-bytes
   (fn [text]
     (let [{:keys [status json command stamps]} (chat-send/plan text {:sender chat-sender :stamps @chat-stamps :now (js/Date.now)})]
       (reset! chat-stamps stamps)
       (if status
         (send-json! res status json)
         (-> (run-chat-command! command)
             (.then (fn [_] (send-json! res 200 {:ok true :command command})))
             (.catch (fn [e] (send-rcon-failure! res e)))))))))

(defn send-whisper! [req res {target :name world :world}]
  (guarded-post!
   req res guard/max-body-bytes
   (fn [text]
     (let [engine-bodies (filter #(and (:engine %) (= world (:world %))) (bodies (js/Date.now)))
           {:keys [status json command stamps]} (chat-send/plan-whisper
                                                 target text
                                                 {:sender chat-sender :stamps @chat-stamps :now (js/Date.now)
                                                  :known (set (map :name engine-bodies))
                                                  :online (set (map :name (filter :up engine-bodies)))})]
       (reset! chat-stamps stamps)
       (if status
         (send-json! res status json)
         (-> (run-chat-command! command)
             (.then (fn [_] (send-json! res 200 {:ok true :command command})))
             (.catch (fn [e] (send-rcon-failure! res e)))))))))


;; ---------------------------------------------------------------- restart (POST /api/restart)
;; Asks the launcher (start.mjs) to rebuild and replace this server; 202 at once, the build happens there.
(defn request-restart! [req res]
  (if-not (restart/loopback-address? (some-> req .-socket .-remoteAddress))
    (send-json! res 403 {:error "restart is for loopback clients only"})
    (guarded-post!
     req res guard/max-body-bytes
     (fn [_]
       (if (restart/ask-launcher!)
         (send-json! res 202 {:ok true :build-id restart/build-id :message "restart requested; the launcher builds first and keeps this server if the build fails"})
         (send-json! res 409 {:error "no launcher: this server was not started by npm start"}))))))

(defn preview! [req res]
  (guarded-post!
   req res max-preview-bytes
   (fn [text]
     (if (nil? text)
       (send-edn! res 413 {:error "preview body exceeds 2 MiB"})
       (try
         (let [input (edn/one-form text)]
           (if-not (and (map? input) (string? (:source input))
                        (or (nil? (:stock input)) (string? (:stock input))))
             (send-edn! res 400 {:error "preview requires {:source <EDN text> :stock <optional EDN text>}"})
             (let [detail (blueprint-lib/preview (:source input) (:stock input))]
               (send-edn! res 200 detail))))
         (catch :default e (send-edn! res 400 {:error (ex-message e)})))))
   {:content-types ["application/edn"] :send-error send-edn!}))
