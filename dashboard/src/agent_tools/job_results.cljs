(ns agent-tools.job-results
  "Bounded job observations from the existing event API. This is tool-side
   history, not an engine wait or a promise of retained child result! values."
  (:require [cljs.reader :as reader]
            [agent-tools.storage-compat :as compat]))

(def history-limit 1000)
(def result-limit 8000) ; Events a result read scans back (a job's outcome sits among scheduler noise).
(def event-limit 8)
(def terminal-kinds
  "Every event kind that ends a job (the engine's end statuses); one definition for results and observe wakes."
  #{:completed :failed :cancelled :stopped})
(def internal-kinds #{:queued :round_started :completed :failed :cancelled :cut
                      :yielded :memory_written :backoff :check_failed :declined :child_started :child_ended})

(defn belongs? [id e]
  (and (= :job (:source e))
       (or (= id (get-in e [:context :job-id]))
           (= id (first (get-in e [:context :chain]))))))

(defn bounded [value]
  (let [budget (volatile! 128)
        clipped (volatile! false)]
    (letfn [(trim [v depth]
              (if (or (> depth 6) (not (pos? @budget)))
                (do (vreset! clipped true) :truncated)
                (do (vswap! budget dec)
                    (cond
                      (string? v) (if (> (count v) 240)
                                    (do (vreset! clipped true) (str (subs v 0 240) "…")) v)
                      (map? v) (do (when (> (count v) 32) (vreset! clipped true))
                                   (into {} (map (fn [[k x]] [(if (> (count (str k)) 80) (do (vreset! clipped true) (subs (str k) 0 80)) k) (trim x (inc depth))])) (take 32 v)))
                      (coll? v) (do (when (> (count v) 32) (vreset! clipped true))
                                    (mapv #(trim % (inc depth)) (take 32 v)))
                      :else v))))]
      {:value (trim value 0) :truncated? @clipped})))

(defn never-issued?
  "True when id cannot be a job of this generation: ids are j<n> from a counter, next-id being the next to issue."
  [id next-id]
  (let [n (some-> (re-matches #"j(\d+)(?:/.*)?" id) second js/parseInt)]
    (and (number? next-id) (or (nil? n) (>= n next-id)))))

(defn project
  "Project retained observations belonging to one root job in one generation.
   Status comes from a top-level terminal event; events include child domain
   observations, never unrelated chat or routine scheduler/action bookkeeping.
   next-id (optional) is the engine's id counter: an id it never issued is :unknown-job even when history is partial."
  ([id generation events partial?] (project id generation events partial? nil))
  ([id generation events partial? next-id]
  (let [matching (->> events (filter #(and (= generation (:generation-id %)) (belongs? id %))) (sort-by :seq) vec)
        terminal (last (filter #(and (= id (get-in % [:context :job-id]))
                                     (terminal-kinds (:kind %))) matching))
        queued? (some #(and (= id (get-in % [:context :job-id])) (= :queued (:kind %))) matching)
        observations (filterv #(not (internal-kinds (:kind %))) matching)
        selected (take-last event-limit observations)
        clipped (bounded (mapv (fn [e] (cond-> {:event (:kind e)}
                                       (:message e) (assoc :message (:message e))
                                       (seq (:data e)) (assoc :data (:data e)))) selected))]
    (if (empty? matching)
      {:ok false :id id :reason (if (and partial? (not (never-issued? id next-id))) :job-history-unavailable :unknown-job) :history-window result-limit}
      (cond-> (merge {:ok true :id id :status (or (:kind terminal) :unfinished)
                      :finished? (some? terminal)
                      :history (if (or partial? (not queued?)) :partial :complete)}
                     ;; No terminal event: the job is still waiting or running; its events so far are not its outcome.
                     (when-not terminal
                       {:state (if (some #(and (= id (get-in % [:context :job-id])) (= :round_started (:kind %))) matching)
                                 :running :queued)}))
        (seq selected) (assoc :events (:value clipped))
        (or (> (count observations) event-limit) (:truncated? clipped)) (assoc :events-truncated? true)
        (= :failed (:kind terminal)) (assoc :error (:value (bounded (get-in terminal [:data :error])))))))))

(defn ^:async history!
  "Read at most 1000 retained events in bounded pages. get! is (get! socket-path path {:signal :timeout-ms
   :max-bytes}), a promise of {:status :content-type :text}. snap, when given, is the snapshot whose :cursor ends
   the read (read from /snapshot when nil). A shared deadline limits the whole read; reducing a read-only page after
   its byte cap is safe."
  [get! socket-path {:keys [signal deadline snap limit] :or {limit history-limit}}]
  (let [deadline (or deadline (+ (js/Date.now) 3000))
        read (fn [endpoint]
               (when (>= (js/Date.now) deadline)
                 (throw (doto (js/Error. "job history deadline exceeded") (aset "code" "ETIMEDOUT"))))
               (.then (get! socket-path endpoint
                            {:signal signal :timeout-ms (max 1 (min 3000 (- deadline (js/Date.now)))) :max-bytes 262144})
                      (fn [response]
                        (when-not (and (= 200 (:status response))
                                       (re-find #"(?i)^application/edn(?:;|$)" (or (:content-type response) "")))
                          (throw (js/Error. "job result history unavailable")))
                        (reader/read-string (:text response)))))
        snap (or snap (await (read "/snapshot")))
        cursor (:cursor snap)
        target (:seq cursor)
        query (fn [after size] (str "/events?stream-id=" (js/encodeURIComponent (:stream-id cursor))
                                    "&after=" after "&limit=" size))]
    (loop [after (max 0 (- target limit)) size 128 events [] partial? false]
      (let [attempt (try {:page (await (read (query after size)))}
                         (catch :default e
                           (if (and (= "ERESPONSETOOLARGE" (.-code e)) (> size 1)) {:smaller? true} (throw e))))]
        (if (:smaller? attempt)
          (recur after (max 1 (quot size 2)) events partial?)
          (let [page (:page attempt)]
            (if (and (:gap? page) (number? (:oldest-seq page)) (< after (dec (:oldest-seq page))))
              (recur (dec (:oldest-seq page)) size events true)
              (let [batch (filterv #(<= (:seq %) target) (:events page))
                    combined (into events (take (- limit (count events)) batch))
                    next-seq (or (:seq (last batch)) after)]
                (if (or (:gap? page) (empty? batch) (>= next-seq target) (>= (count combined) limit))
                  {:snapshot snap :events combined :gap? (boolean (:gap? page)) :partial? (or partial? (:gap? page))
                   :truncated? (> target limit)}
                  (recur next-seq size combined partial?))))))))))

(defn ^:async read!
  "The projected outcome of job id from the retained history (see history! for get!)."
  [get! socket-path id {:keys [signal deadline]}]
  (let [history (await (history! get! socket-path {:signal signal :deadline deadline :limit result-limit}))]
    (project id (get-in history [:snapshot :generation-id]) (:events history) (or (:partial? history) (:truncated? history))
             (get-in history [:snapshot :state :next-id]))))

(defn js-get
  "A cljs get! over a JavaScript one: (get-fn socket path #js {:signal :timeoutMs :maxBytes}), a promise of
   #js {:status :contentType :text}."
  [get-fn]
  (fn [socket-path endpoint {:keys [signal timeout-ms max-bytes]}]
    (.then (get-fn socket-path endpoint #js {:signal signal :timeoutMs timeout-ms :maxBytes max-bytes})
           (fn [r] {:status (aget r "status") :content-type (aget r "contentType") :text (aget r "text")}))))

(defn js-project [id generation events partial?]
  (compat/to-js (project id generation (compat/from-js events) partial?)))
(defn js-read [socket-path id get-fn signal deadline]
  (.then (read! (js-get get-fn) socket-path id {:signal signal :deadline deadline}) compat/to-js))
(defn js-history [socket-path get-fn signal deadline snap]
  (.then (history! (js-get get-fn) socket-path {:signal signal :deadline deadline :snap (when snap (compat/from-js snap))})
         compat/to-js))
