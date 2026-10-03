(ns engine.event-api
  "Private engine event and durable-attention API over a local Unix socket.
  The wire format is EDN so keyword event fields survive unchanged."
  (:require [cljs.reader :as reader]
            [engine.core :as core]
            [engine.events :as events]
            ["fs" :as fs]
            ["http" :as http]
            ["net" :as net]
            ["path" :as path]))

(def content-type "application/edn; charset=utf-8")
(def max-body-bytes 16384)
(def max-limit 1000)
(def reasons #{:handled :condition-recovered})

(defn respond! [res status value]
  (.writeHead res status #js {"Content-Type" content-type "Cache-Control" "no-store"})
  (.end res (pr-str value)))

(defn bad! [res status reason]
  (respond! res status {:ok false :reason reason}))

(defn read-body [req]
  (js/Promise.
   (fn [resolve reject]
     (let [chunks (atom [])
           size (atom 0)]
       (.on req "data" (fn [chunk]
                         (swap! size + (.-length chunk))
                         (if (> @size max-body-bytes)
                           (reject (js/Error. "body too large"))
                           (swap! chunks conj chunk))))
       (.on req "end" (fn []
                        (try
                          (resolve (.toString (js/Buffer.concat (clj->js @chunks)) "utf8"))
                          (catch :default e (reject e)))))
       (.on req "error" reject)))))

(defn parse-edn [text]
  (try {:value (reader/read-string text)}
       (catch :default _ {:error :bad-edn})))

(defn number-param [params key fallback maximum]
  (let [text (.get params key)]
    (if (nil? text)
      fallback
      (let [n (js/Number text)]
        (when (and (js/Number.isSafeInteger n) (<= 0 n maximum)) n)))))

(defn snapshot [eng]
  (let [p (:primitives eng)]
    {:body (.-username (.self p))
     :generation-id (:generation-id (core/state eng))
     :state (core/state eng)
     :outstanding (core/outstanding eng)
     :position (core/self-pos p)
     :offline (core/offline? eng)
     :settling (core/settling? eng)
     :cursor (events/cursor (:events eng))}))

(defn prepare-socket! [socket-path]
  (js/Promise.
   (fn [resolve reject]
     (if-not (fs/existsSync socket-path)
       (resolve true)
       (let [probe (net/createConnection #js {:path socket-path})]
         (.once probe "connect" (fn []
                                  (.destroy probe)
                                  (reject (js/Error. (str "event socket already has a listener: " socket-path)))))
         (.once probe "error" (fn [e]
                                (if (#{"ECONNREFUSED" "ENOENT"} (.-code e))
                                  (try (fs/rmSync socket-path) (resolve true) (catch :default err (reject err)))
                                  (reject e)))))))))

(defn create [socket-path eng]
  (let [listening? (atom false)
        server (http/createServer
                (fn [req res]
                  (try
                    (let [url (js/URL. (.-url req) "http://engine")
                          pathname (.-pathname url)
                          method (.-method req)]
                      (cond
                        (and (= method "GET") (= pathname "/snapshot"))
                        (respond! res 200 (snapshot eng))

                        (and (= method "GET") (= pathname "/events"))
                        (let [params (.-searchParams url)
                              stream-id (.get params "stream-id")
                              after (number-param params "after" nil js/Number.MAX_SAFE_INTEGER)
                              limit (number-param params "limit" 200 max-limit)]
                          (if (and stream-id (some? after) (some? limit) (pos? limit))
                            (respond! res 200 (events/read-after (:events eng)
                                                                  {:stream-id stream-id :after after :limit limit}))
                            (bad! res 400 :bad-query)))

                        (and (= method "POST") (= pathname "/attention/resolve"))
                        (if-not (and (aget (.-headers req) "content-type")
                                     (.startsWith (aget (.-headers req) "content-type") "application/edn"))
                          (bad! res 415 :content-type-must-be-application-edn)
                          (-> (read-body req)
                              (.then
                               (fn [text]
                                 (let [{:keys [value error]} (parse-edn text)]
                                   (cond
                                     error (bad! res 400 error)
                                     (not (and (map? value)
                                               (string? (:request-id value))
                                               (contains? reasons (:reason value))))
                                     (bad! res 400 :bad-request)
                                     :else
                                     (let [result (core/resolve-attention! eng (:request-id value) (:reason value))]
                                       (respond! res 200 {:ok true :request-id (:request-id value)
                                                          :resolved (= result :resolved)
                                                          :already-resolved (= result :already-resolved)}))))))
                              (.catch (fn [e]
                                        (bad! res (if (= "body too large" (.-message e)) 413 400)
                                              (if (= "body too large" (.-message e)) :too-large :bad-edn))))))

                        :else (bad! res 404 :not-found)))
                    (catch :default _ (bad! res 500 :internal-error)))))]
    {:listen (fn []
               (js/Promise.
                (fn [resolve reject]
                  (fs/mkdirSync (path/dirname socket-path) #js {:recursive true})
                  (-> (prepare-socket! socket-path)
                      (.then (fn []
                               (.once server "error" reject)
                               (.listen server socket-path
                                        (fn []
                                          (try
                                            (fs/chmodSync socket-path 384)
                                            (reset! listening? true)
                                            (resolve true)
                                            (catch :default e (reject e)))))))))))
     :close (fn []
              (js/Promise.
               (fn [resolve reject]
                 (if-not @listening?
                   (resolve true)
                   (.close server
                           (fn []
                             (reset! listening? false)
                             (fs/rmSync socket-path #js {:force true})
                             (resolve true)))))))}))
