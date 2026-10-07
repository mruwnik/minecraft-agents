(ns agent-tools.observe
  "observe.mjs <agent> --world <world>: compact status, inventory, jobs and catalogs read from a body's events socket, and the --wait mode that blocks until something worth an agent's attention happens. The wait keeps a per-observer cursor and attention state in worlds/<world>/observers/<agent>/<observer>.edn (the checkpoint) and a lock directory beside it, so concurrent observers of one name are refused."
  (:require [engine.bodies :as bodies]
            [agent-tools.world-data :as data]
            [agent-tools.http :as http]
            [agent-tools.inventory :as inventory]
            [agent-tools.job-results :as job-results]
            ["node:path" :as path]
            [clojure.string :as str]
            ["node:timers/promises" :as timers]
            [agent-tools.observe.status :refer [attention-changes classify code-of coded collect compact-status death-jobs clear-reported id-str instance-names recovered? signature stale-reconnect? summary-result track-deaths with-death-jobs]]
            [agent-tools.observe.lock :refer [acquire! checkpoint! observer-count saved-checkpoint]]
            [agent-tools.observe.request :refer [legacy-notice request-for usage wait-options]]
            [shadow.cljs.modern :refer [js-await]]))

(def request-timeout-ms 3000)
(def max-response-bytes 262144)
;; The wait loop

(defn delay! [ms signal]
  (.setTimeout timers ms nil (if signal #js {:signal signal} #js {})))

(defn events-query [stream-id after limit]
  (str "/events?stream-id=" (js/encodeURIComponent stream-id) "&after=" after "&limit=" limit))

(defn watched-event
  "The key under which the latest lifecycle event of a watched job is kept, or nil."
  [e opts]
  (let [{:keys [source kind context]} e]
    (cond
      (and (= :job source) (some #{(:job-id context)} (:watch opts)) (or (#{:queued :round_started} kind) (job-results/terminal-kinds kind)))
      (str "job:" (:job-id context)))))

(defn recent-results
  "Classified results of the watched jobs among past events, oldest first."
  [events cursor generation opts body]
  (->> events
       (filter #(and (<= (:seq %) (:seq cursor)) (= generation (:generation-id %))))
       (reduce (fn [latest e] (if-let [k (watched-event e opts)] (assoc latest k e) latest)) {})
       vals (sort-by :seq)
       (keep #(classify % opts body))
       vec))

(defn with-history
  "A job-finished wake with the job's projected outcome (agent-tools.job-results/project): its retained domain
  events, how complete the history is (:history, :unavailable when it could not be read) and :events-truncated?."
  [wake outcome]
  (cond-> wake
    (:events outcome) (assoc :events (:events outcome))
    :always (assoc :history (or (:history outcome) :unavailable))
    (:events-truncated? outcome) (assoc :events-truncated? true)))

(defn with-projections
  "The recent results with each job-finished wake carrying its outcome projected from the history already read."
  [results history generation]
  (mapv #(if (= :job-finished (:wake %))
           (with-history % (job-results/project (:job %) generation (:events history) (:partial? history)))
           %)
        results))

(defn unavailable-jobs
  "Watched jobs found neither among the retained events of this generation nor in the snapshot's live scheduler
  state: their outcome has left the history window. Nil when the snapshot carries no scheduler state."
  [events cursor generation opts snap]
  (when-let [instances (get-in snap [:state :instances])]
    (let [seen (->> events
                    (filter #(and (<= (:seq %) (:seq cursor)) (= generation (:generation-id %))))
                    (keep #(watched-event % opts))
                    set)]
      (filterv #(not (or (seen (str "job:" %)) (contains? instances %))) (:watch opts)))))

(defn attention-wake [changes]
  (cond-> (array-map :wake :attention :requests (:changed changes))
    (:more changes) (assoc :more? true)))

(declare step)

(defn- read!
  "GET `endpoint` from the body's events socket as EDN, within the wait's deadline."
  [{:keys [request get! signal deadline]} endpoint]
  (js-await [response (get! (:socket-path request) endpoint
                            {:signal signal :timeout-ms (max 1 (min request-timeout-ms (- @deadline (js/Date.now))))})]
    (when-not (and (= 200 (:status response)) (http/edn-response? (:content-type response)))
      (throw (coded (if (= 404 (:status response)) "EOBSERVEUNAVAILABLE" "EBADRESPONSE") (:text response))))
    (data/read-edn (:text response))))

(defn- aborted? [{:keys [signal]}] (and signal (aget signal "aborted")))

(defn- job-history [{:keys [request get! signal deadline] :as w} result]
  (if (and (= :job-finished (:wake result)) (not (contains? result :history)))
    (-> (job-results/read! get! (:socket-path request) (:job result) {:signal signal :deadline @deadline})
        (.then #(with-history result %))
        (.catch (fn [error] (if (aborted? w) (throw error) (with-history result {})))))
    (js/Promise.resolve result)))

(defn- save! [{:keys [ephemeral? file deaths st]}]
  (when-not ephemeral? (checkpoint! file (assoc @st :cancelled (:cancelled @deaths)))))

(defn- finish!
  "Deliver `result` (with the running summary and, for a finished job, its history), then move the checkpoint."
  [{:keys [summary deliver st] :as w} result]
  (when (aborted? w) (throw (coded "ABORT_ERR" "cancelled")))
  (js-await [result (job-history w result)]
    (let [output (summary-result @summary result)]
      (js-await [_ (js/Promise.resolve (deliver output))]
        (save! w)
        output))))

(defn- timeout! [{:keys [summary] :as w}]
  (finish! w (if (seq (:counts @summary)) (array-map :wake :timeout) (array-map :wake :timeout :changed false))))

(defn- reset-with-status! [w reason]
  (js-await [status (read! w "/status")]
    (finish! w (array-map :wake :reset :reason reason :status (compact-status status)))))

(defn- recovered-later?
  "The batch is one page: look at the following pages for the body coming back."
  [w event page]
  (letfn [(from [after pages]
            (js-await [p (read! w (events-query (:stream-id page) after 256))]
              (let [evs (:events p)]
                (cond (some recovered? evs) true
                      (or (empty? evs) (zero? pages) (:gap? p)) false
                      :else (from (:seq (last evs)) (dec pages))))))]
    (from (:seq event) 8)))

(defn- continue-event [{:keys [st deaths summary opts body] :as w} event more later page]
  (when (and (= :attention (:source event)) (= :resolved (:kind event)))
    (let [id (id-str (:request-id event))
          drop-id (fn [m] (into (empty m) (remove (fn [[k _]] (= id (id-str k)))) m))]
      (swap! st update :seen dissoc id)
      (swap! st update-in [:snap :outstanding] #(drop-id (or % {})))))
  (if (and (= :system (:source event)) (#{:started :restored} (:kind event)))
    (js-await [snap (read! w "/snapshot")]
      ;; The restart wake says nothing of the events skipped before it (old jobs); the
      ;; watched jobs are looked up in the history on the next call.
      (reset! summary {:counts {} :items [] :more false})
      (swap! deaths dissoc :cancelled)
      (swap! st assoc :snap snap :cursor (:cursor snap) :seen {} :pending [] :lookup true)
      (finish! w (array-map :wake :reset :reason :engine-restarted)))
    (let [failed? (= :reconnect-failed (:kind event))
          _ (swap! deaths track-deaths event)
          dead-jobs (death-jobs @deaths event)
          _ (swap! deaths clear-reported event)
          immediate (when-not (and failed? (stale-reconnect? event later))
                      (some-> (classify event opts body) (with-death-jobs dead-jobs)))
          skip! (fn [] (swap! summary collect event dead-jobs) (more))]
      (cond
        (nil? immediate) (skip!)
        (and failed? (empty? later) (< (:seq event) (:latest-seq page)))
        (js-await [back? (recovered-later? w event page)]
          (if back? (skip!) (finish! w immediate)))
        :else (finish! w immediate)))))

(defn- process-events [{:keys [st deadline opts] :as w} page events]
  (if (empty? events)
    (cond
      (>= (js/Date.now) @deadline) (timeout! w)
      (< (:seq (:cursor @st)) (:latest-seq page)) (step w)
      :else (js-await [_ (delay! (min (:poll-ms opts) (max 1 (- @deadline (js/Date.now)))) (:signal w))]
              (step w)))
    (let [event (first events)
          more #(process-events w page (rest events))]
      (swap! st assoc :cursor {:stream-id (:stream-id page) :seq (:seq event)})
      (if (= :required (:attention event))
        (js-await [snap (read! w "/snapshot")]
          (swap! st assoc :snap snap)
          (let [update (attention-changes (:outstanding snap) (:seen @st))]
            (swap! st assoc :seen (:seen update))
            (if (seq (:changed update))
              (finish! w (attention-wake update))
              (continue-event w event more (rest events) page))))
        (continue-event w event more (rest events) page)))))

(defn- poll [{:keys [st deaths] :as w}]
  (let [{:keys [cursor]} @st]
    (js-await [page (read! w (events-query (:stream-id cursor) (:seq cursor) 256))]
      (if (:gap? page)
        (do (swap! st assoc :cursor (:cursor page) :seen {})
            (swap! deaths dissoc :cancelled)
            (reset-with-status! w :event-gap))
        (process-events w page (:events page))))))

(defn- after-lookup [{:keys [st deadline opts] :as w}]
  (swap! st assoc :lookup false)
  (swap! st update :pending (fn [pending] (filterv #(some #{(:job %)} (:watch opts)) pending)))
  (if-let [pending (seq (:pending @st))]
    (do (swap! st assoc :pending (vec (rest pending)))
        (finish! w (first pending)))
    (if (>= (js/Date.now) @deadline)
      (timeout! w)
      (poll w))))

(defn- lookup-history [{:keys [request get! signal deadline st]}]
  (job-results/history! get! (:socket-path request)
                        {:signal signal :deadline @deadline
                         :snap (assoc (:snap @st) :cursor (:cursor @st))}))

(defn- step [{:keys [st deaths opts generation body watching?] :as w}]
  (let [changes (attention-changes (:outstanding (:snap @st)) (:seen @st))]
    (swap! st assoc :seen (:seen changes))
    (cond
      (seq (:changed changes)) (finish! w (attention-wake changes))

      (and (:lookup @st) watching?)
      (js-await [history (lookup-history w)]
        (let [unavailable (unavailable-jobs (:events history) (:cursor @st) generation opts (:snap @st))]
          (cond
            (:gap? history)
            (do (swap! st assoc :lookup false)
                (swap! deaths dissoc :cancelled)
                (finish! w (array-map :wake :reset :reason :history-unavailable)))

            (seq unavailable)
            (do (swap! st assoc :lookup false)
                (finish! w (array-map :wake :reset :reason :history-unavailable :jobs unavailable
                                      :history-window job-results/history-limit)))

            :else
            (do (swap! st assoc :pending (-> (recent-results (:events history) (:cursor @st) generation opts body)
                                             (with-projections history generation)))
                (after-lookup w)))))

      :else (after-lookup w))))

(defn- start-wait
  "Read the first snapshot, set up the wait's state from the saved checkpoint and run the loop."
  [{:keys [request ephemeral? file dir opts st deaths timeout-finish deadline] :as w}]
  (let [saved (when-not ephemeral? (saved-checkpoint file))]
    (when (and (not ephemeral?) (> (observer-count dir) 64)) (throw (coded "EOBSERVERLIMIT" "observer limit")))
    (vreset! deadline (+ (js/Date.now) (:timeout-ms opts)))
    (js-await [snap (read! w "/snapshot")]
      (let [generation (:generation-id snap)
            seen-now (if ephemeral? (into {} (map (fn [[id r]] [(id-str id) (signature r)])) (:outstanding snap)) {})
            w (assoc w :generation generation :body (or (:body snap) (:agent request)) :watching? (boolean (seq (:watch opts))))]
        (reset! st {:cursor (or (:cursor saved) (:cursor snap)) :generation generation
                    :seen (or (:seen saved) seen-now) :lookup (or (nil? saved) (true? (:lookup saved)))
                    :pending (vec (:pending saved)) :snap snap})
        (reset! deaths {:names (instance-names snap) :cancelled (vec (:cancelled saved))})
        (when-not saved
          (save! (assoc w :st (atom (select-keys @st [:cursor :generation :seen :lookup])))))
        (vreset! timeout-finish #(timeout! w))
        (if (and saved (not= (:generation saved) generation))
          (do (swap! st assoc :cursor (:cursor snap) :seen {} :pending [] :lookup true)
              (swap! deaths dissoc :cancelled)
              (reset-with-status! w :engine-restarted))
          (step w))))))

(defn wait-observe
  "Wait for the next thing worth reporting: a promise of the result map. get! is (get! socket-path path options) and
  answers a promise of {:status :content-type :text}; deliver is called with the result before the checkpoint
  moves, so a failed delivery leaves the checkpoint where it was. Rejects with coded errors (EOBSERVERBUSY,
  EOBSERVERLIMIT, EATTENTIONLIMIT, EOBSERVEUNAVAILABLE, ABORT_ERR, ...). With (:ephemeral request) the wait takes no lock and keeps no checkpoint file, and
  attention outstanding at its start counts as seen (a submit wait is about its own job)."
  [request get! signal deliver]
  (try
    (let [opts (:wait-options request)
          dir (.join path (bodies/worlds-dir (:state request)) (:world request) "observers" (:agent request))
          ephemeral? (:ephemeral request)
          release (if ephemeral? (fn []) (acquire! dir (:observer opts)))
          timeout-finish (volatile! nil)
          w {:request request :get! get! :signal signal :deliver deliver :opts opts :dir dir :ephemeral? ephemeral?
             :file (.join path dir (str (:observer opts) ".edn"))
             :deaths (atom {}) :st (atom nil) :summary (atom {:counts {} :items [] :more false})
             :deadline (volatile! nil) :timeout-finish timeout-finish}]
      (-> (js/Promise.resolve nil)
          (.then #(start-wait w))
          (.catch (fn [error]
                    (if (and (= "ETIMEDOUT" (code-of error)) @(:deadline w) (>= (js/Date.now) @(:deadline w)) @timeout-finish)
                      (@timeout-finish)
                      (throw error))))
          (.finally release)))
    (catch :default error (js/Promise.reject error))))

;; Transport and the command

(defn get!
  "GET over the body's events socket: a promise of {:status :content-type :text}. Options :timeout-ms :max-bytes
  :signal :request-fn."
  [socket-path request-path {:keys [timeout-ms max-bytes signal request-fn] :or {timeout-ms request-timeout-ms max-bytes max-response-bytes}}]
  (http/request (cond-> {:socket-path socket-path :path request-path :label "observe" :timeout-ms timeout-ms
                         :max-bytes max-bytes :signal signal}
                  request-fn (assoc :request-fn request-fn))))

(defn print-text!
  "Write to stdout: a promise that resolves once the text is flushed."
  [text]
  (js/Promise. (fn [resolve reject] (.write (.-stdout js/process) text #(if % (reject %) (resolve nil))))))

(def failure-reasons
  (merge http/transport-reasons
         {"ABORT_ERR" :cancelled "EOBSERVERBUSY" :observer-busy "EOBSERVERLIMIT" :observer-limit
          "EATTENTIONLIMIT" :attention-limit "EOBSERVEUNAVAILABLE" :observe-unavailable}))

(defn failure-text [request error]
  (str "{:ok false :reason " (data/write-edn (get failure-reasons (code-of error) :transport-error))
       " :body " (js/JSON.stringify (:agent request)) "}\n"))

(defn line [text] (if (str/ends-with? text "\n") text (str text "\n")))

(defn wait-for!
  "Wait as observe --wait does, watching the jobs watch (an ID list): a promise of
  the wake map, or of {:ok false :reason r} when the wait could not run. base is {:agent :world :state :socket-path};
  timeout as on the command line (default 60s). The wait is ephemeral: no observer lock or checkpoint (so it never contends with an observe --wait), and attention outstanding at its start does not wake it. SIGINT and SIGTERM cancel it."
  [base {:keys [watch timeout observer]} get!]
  (let [controller (js/AbortController.)
        cancel #(.abort controller)
        opts (wait-options {:timeout timeout :observer observer :watch (clj->js watch)})]
    (.once js/process "SIGINT" cancel)
    (.once js/process "SIGTERM" cancel)
    (-> (wait-observe (assoc base :wait-options opts :ephemeral true) get! (.-signal controller) identity)
        (.catch (fn [error] {:ok false :reason (get failure-reasons (code-of error) :transport-error)}))
        (.finally (fn []
                    (.removeListener js/process "SIGINT" cancel)
                    (.removeListener js/process "SIGTERM" cancel))))))

(defn wait!
  "Run the wait with SIGINT and SIGTERM cancelling it; the exit code."
  [request {:keys [get! output]}]
  (let [controller (js/AbortController.)
        cancel #(.abort controller)]
    (.once js/process "SIGINT" cancel)
    (.once js/process "SIGTERM" cancel)
    (-> (wait-observe request get! (.-signal controller) (fn [result] (output (str (data/write-edn result) "\n"))))
        (.then (fn [_] 0))
        (.finally (fn []
                    (.removeListener js/process "SIGINT" cancel)
                    (.removeListener js/process "SIGTERM" cancel))))))

(defn print-outcome!
  "Print a job's projected outcome (agent-tools.job-results/project); a promise of the exit code."
  [output outcome]
  (-> (output (str (data/write-edn outcome) "\n")) (.then (constantly (if (false? (:ok outcome)) 1 0)))))

(defn job-outcome!
  "Read job id's outcome from the retained event history and print it; a promise of the exit code."
  [request {:keys [output get!]} id]
  (-> (job-results/read! get! (:socket-path request) id {})
      (.then #(print-outcome! output %))))

(defn job-not-found? [status text]
  (and (= 404 status) (= :job-not-found (:reason (data/read-edn text)))))

(defn respond!
  "Print the engine's answer the way the request asked for it; a promise of the exit code. A job the scheduler
  no longer holds is answered from the retained event history."
  [request {:keys [output] :as opts} {:keys [status content-type text] :as response}]
  (let [path (:path request)
        starts? (fn [& prefixes] (some #(str/starts-with? path %) prefixes))
        print! #(output (str (data/write-edn %) "\n"))]
    (cond
      (not (http/edn-response? content-type))
      (-> (output "{:ok false :reason :bad-response :detail :unexpected-content-type}\n") (.then (constantly 1)))

      (and (starts? "/status" "/job" "/catalog" "/inventory") (http/unsupported-route? response))
      (-> (output (str (legacy-notice request) "\n")) (.then (constantly 2)))

      (and (starts? "/status") (not (:verbose request)) (= 200 status))
      (-> (print! (compact-status (data/read-edn text))) (.then (constantly 0)))

      (and (starts? "/inventory") (:inventory-mode request) (not (:raw request)) (= 200 status))
      (let [result (data/read-edn text)]
        (-> (print! (inventory/compact result (:inventory-mode request) (:slots request)))
            (.then (constantly (if (false? (:ok result)) 1 0)))))

      (and (starts? "/job?") (job-not-found? status text))
      (job-outcome! request opts (.get (js/URLSearchParams. (second (str/split path #"\?" 2))) "id"))

      (and (starts? "/inventory") (= :equipment (:inventory-mode request)) (:raw request) (= 200 status))
      (let [result (data/read-edn text)]
        (-> (print! {:equipment (or (:equipment result) :none)})
            (.then (constantly (if (false? (:ok result)) 1 0)))))

      :else
      (-> (output (line text)) (.then (constantly (if (<= 200 status 299) 0 1)))))))

(defn main!
  "Run the command; a promise of the exit code. Options (for tests): :output (text -> promise or nil), :request-fn."
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv] (main! argv {}))
  ([argv {:keys [output request-fn] :or {output print-text!}}]
   (let [request (request-for argv)
         output #(js/Promise.resolve (output %))
         get-fn (fn [socket-path request-path options] (get! socket-path request-path (cond-> options request-fn (assoc :request-fn request-fn))))
         opts {:output output :get! get-fn}]
     (if (:error request)
       (do (js/console.error (str (:error request) "\n" usage)) (js/Promise.resolve 2))
       (-> (js/Promise.resolve nil)
           (.then (fn []
                    (cond
                      (:wait-options request) (wait! request opts)
                      (:result-id request) (job-outcome! request opts (:result-id request))
                      :else
                      (-> (get-fn (:socket-path request) (:path request) {})
                          (.then #(respond! request opts %))))))
           (.catch (fn [error]
                     (-> (output (failure-text request error)) (.then (constantly 2))))))))))
