(ns dashboard.server.settings
  "One body's settings: GET /api/settings/<world>/<name> reads the engine's /settings, POST changes the body's
  settings.edn (set, unset) or reloads it. The engine validates; a refusal comes back as {:error reason}."
  (:require [dashboard.edn :as edn]
            [dashboard.guard :as guard]
            [dashboard.server.responses :refer [guarded-post! send-edn!]]
            [dashboard.server.engine-state :refer [canonical-engine? event-socket-request!]]))

(defn engine-request
  "[path body] of the engine call for a dashboard request map {:op :set|:unset|:reload :key :value}, else nil."
  [{:keys [op key value] :as request}]
  (when (map? request)
    (case op
      :reload ["/settings/reload" {}]
      :unset (when (keyword? key) ["/settings/unset" {:key key}])
      :set (when (and (keyword? key) (contains? request :value)) ["/settings/set" {:key key :value value}])
      nil)))

(defn failure! [res e]
  (when-not (.-headersSent res)
    (let [engine-body (.-engineBody e)]
      (send-edn! res (if engine-body 409 503)
                 {:error (str (or (some-> engine-body :reason str) (.-message e))
                              (when-let [problems (seq (:problems engine-body))] (str " (" (first problems) ")")))}))))

(defn send-settings! [res body]
  (if-not (canonical-engine? body)
    (send-edn! res 404 {:error "no canonical engine event service"})
    (-> (event-socket-request! body "GET" "/settings" nil)
        (.then #(send-edn! res 200 %))
        (.catch #(failure! res %)))))

(defn change-settings! [req res body]
  (guarded-post!
   req res guard/max-body-bytes
   (fn [text]
     (let [request (when text (try (edn/one-form text) (catch :default _ nil)))
           [path engine-body] (engine-request request)]
       (cond
         (not (canonical-engine? body)) (send-edn! res 404 {:error "no canonical engine event service"})
         (not text) (send-edn! res 413 {:error "request body too large"})
         (not path) (send-edn! res 400 {:error "expected {:op :set :key :ns/key :value v}, {:op :unset :key k} or {:op :reload}"})
         :else (-> (event-socket-request! body "POST" path engine-body)
                   (.then #(send-edn! res 200 %))
                   (.catch #(failure! res %))))))
   {:content-types ["application/edn"] :send-error send-edn!}))
