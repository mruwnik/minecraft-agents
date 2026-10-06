(ns dashboard.server.events
  "One body's action log, the paged canonical event feed and attention resolution."
  (:require [dashboard.engine-events :as ee]
            ["fs" :as fs]
            [dashboard.guard :as guard]
            [cljs.reader :as reader]
            [dashboard.server.files :refer [file-exists? port read-range]]
            [dashboard.server.responses :refer [read-body send-edn!]]
            [dashboard.server.engine-state :refer [body-key canonical-engine? engine-folder? event-page! event-page-size event-socket-request! events-file state-cursor]]))

;; ---------------------------------------------------------------- one body's action log
;; the last bytes of events.jsonl, filtered to what the popup lists; cached per folder until the file's size changes
(def log-tail-bytes (* 1024 1024))
(def log-cache (atom {}))
(def default-log-limit 300)
(def max-log-limit 2000)

(defn log-limit [text]
  (let [n (js/parseInt text 10)]
    (if (and (not (js/isNaN n)) (pos? n)) (min n max-log-limit) default-log-limit)))

(defn read-log [body]
  (let [file (events-file body)
        size (.-size (.statSync fs file))
        k (body-key body)
        cached (get @log-cache k)]
    (if (= size (:size cached))
      (:events cached)
      (let [start (max 0 (- size log-tail-bytes))
            bytes (read-range file start size)
            whole (if (pos? start) (ee/drop-torn-head bytes) bytes)
            events (mapv ee/log-entry (filter ee/log-worthy? (ee/parse-event-lines (ee/decode-bytes whole))))]
        (swap! log-cache assoc k {:size size :events events})
        events))))

(defn query-int [query key fallback]
  (let [n (js/parseInt (.get query key) 10)]
    (if (js/Number.isFinite n) n fallback)))

(defn cursor-for-page [stream-id after page]
  {:stream-id stream-id :seq (or (:seq (peek (vec (:events page)))) after 0)})

(defn canonical-feed! [body query]
  (-> (event-socket-request! body "GET" "/snapshot" nil)
      (.then (fn [initial]
               (let [{:keys [stream-id seq]} (state-cursor initial)
                     limit (min event-page-size (log-limit (.get query "limit")))
                     requested-stream (.get query "stream-id")
                     requested-after (query-int query "after" -1)
                     after (if (and requested-stream (not (neg? requested-after)))
                             requested-after
                             (max 0 (- seq limit)))]
                 (-> (event-page! body (or requested-stream stream-id) after limit)
                     (.then (fn [page]
                              (if-not (:gap? page)
                                {:snapshot initial :page page :after after :gap? false}
                                (-> (event-socket-request! body "GET" "/snapshot" nil)
                                    (.then (fn [fresh]
                                             (let [{fresh-stream :stream-id fresh-seq :seq} (state-cursor fresh)
                                                   oldest (or (:oldest-seq page) 1)
                                                   newest (or (:latest-seq page) fresh-seq)
                                                   tail-after (max 0 (max (dec oldest) (- newest limit)))]
                                               (-> (event-page! body fresh-stream tail-after limit)
                                                   (.then (fn [tail]
                                                            {:snapshot fresh :page tail :after tail-after :gap? true}))))))))))))))))

(defn send-events! [res body query]
  (cond
    (not (engine-folder? body))
    (send-edn! res 404 {:error (str "no engine body called " (:name body) " in world " (:world body))})

    (canonical-engine? body)
    (-> (canonical-feed! body query)
        (.then (fn [{:keys [snapshot page after gap?]}]
                 (let [cursor (cursor-for-page (:stream-id (state-cursor snapshot)) after page)]
                   (send-edn! res 200 {:body (:name body)
                                       :generation-id (:generation-id snapshot)
                                       :stream-id (:stream-id cursor)
                                       :cursor cursor
                                       :events (vec (:events page))
                                       :outstanding (or (:outstanding snapshot) {})
                                       :gap? gap?
                                       :more? (< (or (:seq cursor) after) (or (:latest-seq page) (:seq cursor) after))}))))
        (.catch (fn [e] (when-not (.-headersSent res)
                          (send-edn! res 503 {:error (ee/socket-failure-text e)})))))

    ;; Compatibility for old running bodies only. A body with events.edn/events.sock never also reads JSONL.
    (not (file-exists? (events-file body)))
    (send-edn! res 404 {:error "no event stream"})

    :else
    (send-edn! res 200 {:body (:name body) :generation-id "legacy" :stream-id "legacy"
                        :cursor {:stream-id "legacy" :seq (or (:seq (peek (read-log body))) 0)}
                        :events (vec (take-last (log-limit (.get query "limit")) (read-log body)))
                        :outstanding {} :gap? false :more? false})))

(defn resolve-attention! [req res body]
  (let [headers (.-headers req)
        refused (or (guard/method-refusal (.-method req))
                    (guard/refusal {:host (.-host headers) :origin (.-origin headers)
                                    :content-type (aget headers "content-type") :port port
                                    :content-types ["application/edn"]}))]
    (cond
      refused (send-edn! res (:status refused) {:error (:error refused)})
      (not (canonical-engine? body)) (send-edn! res 404 {:error "no canonical engine event service"})
      :else
      (read-body req guard/max-body-bytes
                 (fn [text]
                   (if-not text
                     (send-edn! res 413 {:error "request body too large"})
                     (let [request (try (reader/read-string text) (catch :default _ nil))]
                       (if-not (and (map? request) (string? (:request-id request)) (= :handled (:reason request)))
                         (send-edn! res 400 {:error "expected {:request-id string :reason :handled}"})
                         (-> (event-socket-request! body "POST" "/attention/resolve" (select-keys request [:request-id :reason]))
                             (.then #(send-edn! res 200 %))
                             (.catch (fn [e] (when-not (.-headersSent res)
                                               (send-edn! res 503 {:error (ee/socket-failure-text e)})))))))))))))
