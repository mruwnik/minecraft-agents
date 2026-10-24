(ns agent-tools.job-results
  "Bounded job observations from the existing event API. This is tool-side
   history, not an engine wait or a promise of retained child result! values."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [agent-tools.storage-compat :as compat]))

(def history-limit 1000)
(def result-limit 8000) ; Events a result read scans back (a job's outcome sits among scheduler noise).
(def event-limit 8)
(def max-string-chars 4000) ; A stop's cause names every child down the chain; it must stay whole.
(def scan-deadline-ms 15000) ; A read of the result-limit window is some 16 pages; a loaded body answers each slowly.
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
                      (string? v) (if (> (count v) max-string-chars)
                                    (do (vreset! clipped true) (str (subs v 0 max-string-chars) "…")) v)
                      (map? v) (do (when (> (count v) 32) (vreset! clipped true))
                                   (into {} (map (fn [[k x]] [(if (> (count (str k)) 80) (do (vreset! clipped true) (subs (str k) 0 80)) k) (trim x (inc depth))])) (take 32 v)))
                      (coll? v) (do (when (> (count v) 32) (vreset! clipped true))
                                    (mapv #(trim % (inc depth)) (take 32 v)))
                      :else v))))]
      {:value (trim value 0) :truncated? @clipped})))

(defn next-text
  "The :next calls of a job's ending (jobs.lib.next-call: {:job :args}, the job itself again last with :again) as one
  line of specs to submit, or nil without any."
  [calls]
  (let [spec #(str "(" (:job %) " " (pr-str (or (:args %) {})) ")")
        {again true fixes false} (group-by (comp boolean :again) (filter map? calls))]
    (when (seq calls)
      (str/join ", then " (remove nil? [(when (seq fixes) (str/join " or " (map spec fixes)))
                                        (when (seq again) (str "resubmit " (spec (first again))))])))))

(defn waited?
  "True when stopped event e is a wait ending: the engine told the same wait in a :waiting event of the job just before
  (end-slot! merges the wait map into the stop's data). A result a job returned with status :stopped never was one."
  [e waits]
  (let [data (dissoc (:data e) :status)]
    (boolean (some #(= data (merge {:reason (:reason %)} (dissoc % :reason))) waits))))

(defn yielded-text
  "For a job that an exclusive run ended without finishing (:stopped :yielded, :declined, or a wait it was told, waits
  being the :data of the job's :waiting events; a real ending such as :ways or a returned :stopped result gets nil; the
  job is dropped, so a resubmit starts over): one sentence on why and what the round got done; else nil."
  ([e exclusive?] (yielded-text e exclusive? nil))
  ([e exclusive? waits]
   (let [reason (get-in e [:data :reason])]
     (when (and (= :stopped (:kind e)) (not= :cut reason) (or exclusive? (#{:yielded :declined} reason)))
       (let [items (get-in e [:report :yields :items])
             done (when (seq items) (str " Got done: " (str/join ", " (map (fn [[item n]] (str (name item) " " n)) (sort-by (comp str key) items))) "."))]
         (case reason
           :declined (str "Declined to run (" (:text e) "); nothing ran.")
           :yielded (str "Yielded after its one exclusive round and is gone." done " Submit the remaining steps as separate calls, or without --exclusive.")
           (when (waited? e waits)
             (str "Waited after its one exclusive round (" (:text e) ") and is gone." done " Resubmit once that is met, or submit without --exclusive."))))))))

(defn never-issued?
  "True when id cannot be a job of this generation: ids are j<n> from a counter, next-id being the next to issue."
  [id next-id]
  (let [n (some-> (re-matches #"j(\d+)(?:/.*)?" id) second js/parseInt)]
    (and (number? next-id) (or (nil? n) (>= n next-id)))))

(defn project
  "Project retained observations belonging to one root job in one generation.
   Status comes from a top-level terminal event; events include child domain
   observations (a child's tagged with its :job, so its stop is not read as the job's), never unrelated chat or routine scheduler/action bookkeeping.
   next-id (optional) is the engine's id counter: an id it never issued is :unknown-job even when history is partial."
  ([id generation events partial?] (project id generation events partial? nil))
  ([id generation events partial? next-id]
  (let [matching (->> events (filter #(and (= generation (:generation-id %)) (belongs? id %))) (sort-by :seq) vec)
        terminal (last (filter #(and (= id (get-in % [:context :job-id]))
                                     (terminal-kinds (:kind %))) matching))
        queued? (some #(and (= id (get-in % [:context :job-id])) (= :queued (:kind %))) matching)
        exclusive? (some #(and (= id (get-in % [:context :job-id])) (= :queued (:kind %)) (:exclusive %)) matching)
        waits (keep #(when (and (= id (get-in % [:context :job-id])) (= :waiting (:kind %))) (:data %)) matching)
        observations (filterv #(not (internal-kinds (:kind %))) matching)
        selected (take-last event-limit observations)
        clipped (bounded (mapv (fn [e] (cond-> {:event (:kind e)}
                                       (not= id (get-in e [:context :job-id])) (assoc :job (get-in e [:context :job-id]))
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
        (seq (:data terminal)) (assoc :result (:value (bounded (:data terminal))))
        (yielded-text terminal exclusive? waits) (assoc :note (yielded-text terminal exclusive? waits))
        (seq selected) (assoc :events (:value clipped))
        (or (> (count observations) event-limit) (:truncated? clipped)) (assoc :events-truncated? true)
        (:kind (:data terminal)) (assoc :ending (select-keys (:data terminal) [:kind :cause :reason]))
        (= :failed (:kind terminal)) (assoc :error (:value (bounded (get-in terminal [:data :error])))))))))

(defn with-recorded
  "events plus, for each id in ids the engine recorded as ended (recorded, the /results answer) with no terminal
  event among them, a synthetic terminal event of the recorded status (seq 0): a job that ended before the window."
  [events generation recorded ids]
  (let [has-end? (into #{} (keep #(when (terminal-kinds (:kind %)) (get-in % [:context :job-id]))) events)]
    (into (vec events)
          (keep (fn [id]
                  (when-let [{:keys [status result text result-clipped]} (and (not (has-end? id)) (get recorded id))]
                    (cond-> {:seq 0 :generation-id generation :source :job :kind status
                             :context {:job-id id :chain [id]} :data (or result {})}
                      result-clipped (assoc-in [:data :result-clipped] true)
                      text (assoc :message text)))))
          ids)))

(defn ^:async history!
  "Read at most 1000 retained events in bounded pages. get! is (get! socket-path path {:signal :timeout-ms
   :max-bytes}), a promise of {:status :content-type :text}. snap, when given, is the snapshot whose :cursor ends
   the read (read from /snapshot when nil). record-ids, when given, are looked up on /results and returned as
   :recorded. A shared deadline limits the whole read; reducing a read-only page after
   its byte cap is safe."
  [get! socket-path {:keys [signal deadline snap limit record-ids] :or {limit history-limit}}]
  (let [deadline (or deadline (+ (js/Date.now) (if (> limit history-limit) scan-deadline-ms 3000)))
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
        recorded (when (seq record-ids)
                   (:results (await (read (str "/results?ids=" (str/join "," record-ids))))))
        cursor (:cursor snap)
        target (:seq cursor)
        query (fn [after size] (str "/events?stream-id=" (js/encodeURIComponent (:stream-id cursor))
                                    "&after=" after "&limit=" size))]
    (loop [after (max 0 (- target limit)) size 512 events [] partial? false]
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
                  {:snapshot snap :recorded recorded :events combined :gap? (boolean (:gap? page)) :partial? (or partial? (:gap? page))
                   :truncated? (> target limit)}
                  (recur next-seq size combined partial?))))))))))

(defn ^:async read!
  "The projected outcome of job id from the retained history (see history! for get!): the newest history-limit
  events first, the result-limit window only when those do not hold the whole job."
  [get! socket-path id {:keys [signal deadline]}]
  (let [project-from (fn [history partial?]
                       (project id (get-in history [:snapshot :generation-id])
                                (with-recorded (:events history) (get-in history [:snapshot :generation-id]) (:recorded history) [id])
                                partial?
                                (get-in history [:snapshot :state :next-id])))
        recent-history (await (history! get! socket-path {:signal signal :deadline (or deadline (+ (js/Date.now) 3000))
                                                          :limit history-limit :record-ids [id]}))
        recent (project-from recent-history false)
        next-id (get-in recent-history [:snapshot :state :next-id])]
    (if (or (and (:ok recent) (= :complete (:history recent)))
            (never-issued? id next-id))
      recent
      (let [history (await (history! get! socket-path {:signal signal :deadline deadline :limit result-limit :record-ids [id]
                                                       :snap (:snapshot recent-history)}))]
        (project-from history (or (:partial? history) (:truncated? history)))))))

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
