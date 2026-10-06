(ns agent-tools.observe
  "observe.mjs <agent> --world <world>: compact status, inventory, jobs and catalogs read from a body's events
  socket, and the --wait mode that blocks until something worth an agent's attention happens. The wait keeps a
  per-observer cursor and attention state in worlds/<world>/observers/<agent>/<observer>.edn (the checkpoint) and
  a lock directory beside it, so concurrent observers of one name are refused."
  (:require [engine.bodies :as bodies]
            [agent-tools.http :as http]
            [agent-tools.inventory :as inventory]
            [agent-tools.job-results :as job-results]
            [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            [shadow.cljs.modern :refer (js-await)]
            [clojure.string :as str]
            ["node:crypto" :as crypto]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:timers/promises" :as timers]))

(def request-timeout-ms 3000)
(def max-response-bytes 262144)
(def private-dir-mode 448)  ; 0700
(def private-file-mode 384) ; 0600

(def usage "usage: observe.mjs <agent> --world <world> [status [--raw|--verbose] [--wait --timeout 60s --chatter addressed --observer agent --watch j12 --watch-action move-home] | inventory [--raw] [--slots] | equipment [--raw] | job <id> | result <id> | catalog <job|trigger> <name> | catalog <jobs|triggers> [prefix]] [--limit <n>] [--offset <n>] [--worlds <dir>] [--state <legacy-parent>]\nstatus: :pos [x y z], :health and :food 0-20, :current the running job, :mode scheduled or manual. job <id>: its state and outcome; an unknown id answers :unknown-job.")

;; Small helpers

(defn clean-pairs
  "An array map of the given keys and values (flat), in order, without the nil values."
  [& kvs]
  (apply array-map (mapcat identity (remove (comp nil? second) (partition 2 kvs)))))

(defn clean [m] (apply clean-pairs (mapcat identity m)))

(defn clip
  ([s] (clip s 240))
  ([s limit] (if (string? s) (subs s 0 (min limit (count s))) s)))

(defn round1 [n] (/ (js/Math.round (* n 10)) 10))

(defn position [pos] (when pos (mapv #(round1 (% pos)) [:x :y :z])))

(defn id-str [k] (str (data/name k)))

(defn truthy-text? [text] (and (some? text) (not= "" text)))

(defn coded [code message]
  (doto (js/Error. message) (aset "code" code)))

(defn code-of [error] (aget error "code"))

;; Compact status

(defn job-view
  "A job row; :waiting is why its check declines (the engine's reason map), when it waits."
  [item]
  (clean-pairs :id (:id item) :name (clip (:name item) 120) :status (:status item) :reflex (:reflex item)
               :waiting (:waiting item)))

(defn attention-item [a]
  (clean-pairs :id (:request-id a) :job (:job-id a) :reason (:reason a) :message (clip (:message a))))

(defn failure-view [x]
  (cond-> (array-map)
    (contains? x :id) (assoc :id (:id x))
    (contains? x :error) (assoc :error (clip (:error x)))))

(defn nonzero [n] (when (and (number? n) (not= 0 n) (not (js/Number.isNaN n))) n))

(defn died-view
  "A recent death: where it happened (the drops lie there), the cause when known, seconds since and seconds
  until the drops despawn; once the pile is picked up (:recovered \"collected\") the pile and timer are left out."
  [{:keys [pos cause ago-ms despawns-in-ms recovered]}]
  (let [at (position pos)
        picked-up? (= "collected" (name (or recovered "")))]
    (clean-pairs :at at :cause (clip cause 80) :ago-s (js/Math.round (/ ago-ms 1000))
                 :recovered (when recovered (name recovered))
                 :pile-at (when-not picked-up? at)
                 :despawns-in-s (when-not picked-up? (js/Math.round (/ despawns-in-ms 1000))))))

(defn offline-view
  "The away record with :back-in-s (seconds until a planned return) beside the epoch :back-at."
  [away now]
  (when away
    (cond-> away
      (number? (:back-at away)) (assoc :back-in-s (max 0 (js/Math.round (/ (- (:back-at away) now) 1000)))))))

(defn compact-status
  "Status without metadata or empty collections: positions rounded, queued jobs apart from the current one."
  ([s] (compact-status s (js/Date.now)))
  ([s now]
  (if (false? (:ok s))
    s
    (let [{:keys [current jobs failed outstanding manual]} s
          items (:items jobs)
          queued (remove #(= (:id %) (:id current)) items)
          queued-total (- (or (:total jobs) 0) (if (some #(= (:id %) (:id current)) items) 1 0))]
      (clean-pairs
       :mode (:mode s)
       :offline (offline-view (:offline s) now)
       :last-known (when (:last-known s) true)
       :idle (when-not current true)
       :pos (position (:position s))
       :died (when (:died s) (died-view (:died s)))
       :health (:health s)
       :food (:food s)
       :current (when current (job-view current))
       :manual (when manual (clean-pairs :who (clip (:who manual) 80) :why (clip (:why manual) 160)))
       :jobs (when (nonzero queued-total)
               (cond-> (array-map :total queued-total :items (mapv job-view queued))
                 (:more? jobs) (assoc :more? true)))
       :failed (when (nonzero (:total failed))
                 (array-map :total (:total failed) :items (mapv failure-view (:items failed))))
       :attention (when (nonzero (:total outstanding))
                    (array-map :total (:total outstanding) :items (mapv attention-item (:items outstanding)))))))))

;; Attention

(defn sha256 [text] (.digest (.update (.createHash crypto "sha256") text) "hex"))

(defn signature
  "Digest of what an attention request says, ignoring when and where it was raised. The EDN text hashed is kept
  byte-identical across versions: observer checkpoints hold these digests."
  [r]
  (let [e (or (:event r) {})]
    (sha256 (data/write-edn {:job (:job-id r) :reason (:reason r) :kind (:kind e) :message (clip (:message e))
                             :data (dissoc (or (:data e) {}) :pos :time-ms)}))))

(def max-attention 4096)
(def max-changed 4)

(defn attention-changes
  "Compare the outstanding requests (id -> request) with the digests already seen (id string -> digest): the new
  seen map, up to four requests that are new or changed, and whether more remain."
  [outstanding seen]
  (when (> (count outstanding) max-attention) (throw (coded "EATTENTIONLIMIT" "too many attention requests")))
  (let [signed (mapv (fn [[id r]] [(id-str id) r (signature r)]) outstanding)
        [next changed] (reduce (fn [[next changed] [id r sig]]
                                 (let [fresh? (and (not= (get seen id) sig) (< (count changed) max-changed))
                                       known (if fresh? sig (get seen id))]
                                   [(cond-> next (some? known) (assoc id known))
                                    (cond-> changed fresh? (conj (attention-item (assoc r :request-id id :message (get-in r [:event :message])))))]))
                               [{} []] signed)]
    {:seen next :changed changed
     :more (boolean (some (fn [[id _ sig]] (not= (get next id) sig)) signed))}))

;; Wake classification and quiet summaries

(defn lower [text] (when (string? text) (str/lower-case text)))

(defn addressed? [e body]
  (let [d (:data e)]
    (or (= :whisper (:kind e)) (true? (:whisper d)) (= (lower (:to d)) (lower body))
        (.test (js/RegExp. (str "(^|[^A-Za-z0-9_])" body "([^A-Za-z0-9_]|$)") "i") (or (:message e) "")))))

(defn action-result [r d]
  (clean-pairs :status (or (:status r) (:status d))
               :reason (clip (first (filter some? [(:reason r) (:reason d) (:error d)])))
               :block (clip (:block r) 80) :consumed (:consumed r) :hurt (:hurt r) :health (:health r)
               :pos (position (:pos r))
               :distance (when (number? (:distance r)) (round1 (:distance r)))))

(defn recovered?
  "Whether an event says the body is back."
  [e]
  (and (= :body (:source e)) (boolean (#{:online :spawned :respawned} (:kind e)))))

(defn stale-reconnect?
  "Whether a reconnect-failed event is not worth a wake: a later try of the same outage (only the first wakes; the
  rest are counted in the summary) or one the body has since recovered from (a later online/spawned in the backlog)."
  [e later]
  (boolean (or (> (or (get-in e [:data :attempt]) 1) 1)
               (some recovered? later))))

(defn classify
  "The wake an event causes under the options, or nil. Options: :from :chatter :watch :watch-actions :danger :disconnect."
  [e opts body]
  (let [{:keys [source kind]} e
        d (or (:data e) {})]
    (cond
      (and (= :body source) (#{:chat :whisper} kind))
      (when-not (or (and (:from opts) (not= (lower (:from d)) (lower (:from opts))))
                    (= "none" (:chatter opts))
                    (and (= "addressed" (:chatter opts)) (not (addressed? e body))))
        (clean-pairs :wake :chat :from (clip (:from d) 40) :message (clip (:message e))
                     :whisper (when (= :whisper kind) true)))

      (and (= :body source) (= :reconnect-failed kind))
      (clean-pairs :wake :reconnect-failed :reason (clip (:reason d)))

      (and (= :body source) (:danger opts) (#{:hurt :died} kind))
      (clean-pairs :wake :danger :event (:kind e) :health (:health d))

      (and (= :body source) (:disconnect opts) (= :disconnected kind))
      (clean-pairs :wake :disconnected :reason (clip (:reason d)))

      (and (= :action source) (= :done kind) (some #{(get-in e [:context :action-id])} (:watch-actions opts)))
      (array-map :wake :action-finished :action (get-in e [:context :action-id]) :result (action-result (or (:result d) {}) d))

      (and (= :job source) (job-results/terminal-kinds kind) (some #{(get-in e [:context :job-id])} (:watch opts)))
      (clean-pairs :wake :job-finished :job (get-in e [:context :job-id]) :result (:kind e)
                   :message (clip (or (:message e) (:error d)))))))

(defn collect
  "Fold one routine event into the bounded quiet summary {:counts :items :more}."
  [summary e]
  (let [{:keys [source kind]} e
        d (or (:data e) {})
        category (cond
                   (and (= :job source) (job-results/terminal-kinds kind)) kind
                   (and (= :reflex source) (= :fired kind)) :reflexes
                   (= :make-room.tossed kind) :tossed
                   (#{:picked-up :hurt :died :disconnected :online :reconnect-failed} kind) kind
                   (= :notice (:attention e)) :notices)]
    (if-not category
      summary
      (let [summary (update-in summary [:counts category] #(min 1000000 (inc (or % 0))))]
        (if (< (count (:items summary)) 4)
          (update summary :items conj
                  (clean-pairs :event (:kind e) :job (when-not (= :reflexes category) (get-in e [:context :job-id]))
                               :reflex (get-in e [:context :reflex-id]) :item (clip (:item d) 80) :count (:count d)
                               :message (clip (first (filter some? [(:message e) (:error d) (:reason d)])))))
          (assoc summary :more true))))))

(defn summary-result [summary result]
  (if (seq (:counts summary))
    (assoc (clean result) :summary (cond-> (array-map :counts (:counts summary) :items (:items summary))
                                     (:more summary) (assoc :more? true)))
    (clean result)))

;; Observer lock and checkpoint

;; a lock dir with no pid file this old was left by a process that died between mkdir and writing its pid
(def pidless-lock-stale-ms 5000)

(defn process-alive?
  "kill -0: ESRCH means gone, EPERM means it exists under another user."
  ([pid] (process-alive? pid #(.kill js/process % 0)))
  ([pid kill!]
   (try (kill! pid) true
        (catch :default e
          (case (code-of e) "ESRCH" false "EPERM" true (throw e))))))

(defn lock-owner-gone?
  "True when the lock dir at `dir` belongs to nobody: its pid is dead, or it never got a pid and is old."
  [dir]
  (let [pid (try (js/Number (.readFileSync fs (.join path dir "pid") "utf8")) (catch :default _ nil))
        pid? (and pid (js/Number.isInteger pid) (not= 0 pid))]
    (if pid?
      (not (process-alive? pid))
      (>= (- (js/Date.now) (.-mtimeMs (.statSync fs dir))) pidless-lock-stale-ms))))

(defn take-reclaim-mutex!
  "Reclaimers take turns through a mutex dir beside the lock. One left by a crashed reclaimer (older than
  pidless-lock-stale-ms) is renamed away atomically, then taken again. Throws EEXIST while another reclaimer works."
  [mutex]
  (let [take! #(.mkdirSync fs mutex #js {:mode private-dir-mode})]
    (try (take!)
         (catch :default error
           (when-not (= "EEXIST" (code-of error)) (throw error))
           (let [old? (try (>= (- (js/Date.now) (.-mtimeMs (.statSync fs mutex))) pidless-lock-stale-ms)
                           (catch :default _ false))]
             (when-not old? (throw error))
             (try (.renameSync fs mutex (str mutex ".dead-" (.-pid js/process)))
                  (catch :default e (when-not (= "ENOENT" (code-of e)) (throw e))))
             (.rmSync fs (str mutex ".dead-" (.-pid js/process)) #js {:recursive true :force true})
             (take!))))))

(defn reclaim-stale-lock!
  "Replace a dead owner's lock with a fresh one. Reclaimers are serialised by a mutex dir, and a lock held by a live
  owner is never moved: only a dead owner's dir (which nobody else may touch meanwhile) is removed. Throws EEXIST
  when the lock is live or another reclaimer or observer got there first. `step!` is a test hook called at each stage."
  ([lock] (reclaim-stale-lock! lock (fn [_])))
  ([lock step!]
   (let [mutex (str lock ".reclaim")]
     (take-reclaim-mutex! mutex)
     (try
       (step! :mutex-held)
       (when (.existsSync fs lock)
         (when-not (lock-owner-gone? lock) (throw (coded "EEXIST" "lock is held")))
         (.rmSync fs lock #js {:recursive true :force true}))
       (step! :lock-removed)
       (.mkdirSync fs lock #js {:mode private-dir-mode})
       (finally (.rmSync fs mutex #js {:recursive true :force true}))))))

(defn acquire!
  "Take the observer's lock directory; the function that gives it back. Throws EOBSERVERBUSY while a live process holds it."
  [dir observer]
  (.mkdirSync fs dir #js {:recursive true :mode private-dir-mode})
  (let [lock (.join path dir (str observer ".lock"))
        busy #(coded "EOBSERVERBUSY" "observer busy")]
    (try (.mkdirSync fs lock #js {:mode private-dir-mode})
         (catch :default error
           (when-not (= "EEXIST" (code-of error)) (throw error))
           (when-not (lock-owner-gone? lock) (throw (busy)))
           (try (reclaim-stale-lock! lock)
                (catch :default e
                  (throw (if (= "EEXIST" (code-of e)) (busy) e))))))
    (try (.writeFileSync fs (.join path lock "pid") (str (.-pid js/process)) #js {:mode private-file-mode})
         (catch :default error
           (throw (if (= "ENOENT" (code-of error)) (busy) error))))
    #(.rmSync fs lock #js {:recursive true :force true})))

(defn checkpoint! [file {:keys [cursor generation seen pending lookup]}]
  (let [temp (str file "." (.-pid js/process) ".tmp")
        state (cond-> (array-map :cursor cursor :generation generation
                                 :seen (into (array-map) (map (fn [[id sig]] [(keyword id) sig])) seen))
                (some? pending) (assoc :pending pending)
                lookup (assoc :lookup true))]
    (.writeFileSync fs temp (str (data/write-edn state) "\n") #js {:mode private-file-mode})
    (.renameSync fs temp file)))

(defn saved-checkpoint [file]
  (when (.existsSync fs file)
    (let [saved (data/read-edn (.readFileSync fs file "utf8"))]
      (update saved :seen #(into {} (map (fn [[id sig]] [(id-str id) sig])) %)))))

(defn observer-count [dir]
  (->> (array-seq (.readdirSync fs dir))
       (filter #(or (str/ends-with? % ".edn") (str/ends-with? % ".lock")))
       (map #(str/replace % #"\.(edn|lock)$" ""))
       set count))

;; The wait loop

(defn delay! [ms signal]
  (.setTimeout timers ms nil (if signal #js {:signal signal} #js {})))

(defn events-query [stream-id after limit]
  (str "/events?stream-id=" (js/encodeURIComponent stream-id) "&after=" after "&limit=" limit))

(defn watched-event
  "The key under which the latest lifecycle event of a watched action or job is kept, or nil."
  [e opts]
  (let [{:keys [source kind context]} e]
    (cond
      (and (= :action source) (some #{(:action-id context)} (:watch-actions opts)) (#{:started :done} kind))
      (str "action:" (:action-id context))
      (and (= :job source) (some #{(:job-id context)} (:watch opts)) (or (#{:queued :round_started} kind) (job-results/terminal-kinds kind)))
      (str "job:" (:job-id context)))))

(defn recent-results
  "Classified results of the watched actions and jobs among past events, oldest first."
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

(defn wait-observe
  "Wait for the next thing worth reporting: a promise of the result map. get! is (get! socket-path path options) and
  answers a promise of {:status :content-type :text}; deliver is called with the result before the checkpoint
  moves, so a failed delivery leaves the checkpoint where it was. Rejects with coded errors (EOBSERVERBUSY,
  EOBSERVERLIMIT, EATTENTIONLIMIT, EOBSERVEUNAVAILABLE, ABORT_ERR, ...)."
  [request get! signal deliver]
  (try
    (let [opts (:wait-options request)
          dir (.join path (bodies/worlds-dir (:state request)) (:world request) "observers" (:agent request))
          release (acquire! dir (:observer opts))
          file (.join path dir (str (:observer opts) ".edn"))
          aborted? #(and signal (aget signal "aborted"))
          deadline (volatile! nil)
          timeout-finish (volatile! nil)
          st (atom nil)
          summary (atom {:counts {} :items [] :more false})
          read! (fn [endpoint]
                  (js-await [response (get! (:socket-path request) endpoint
                                            {:signal signal :timeout-ms (max 1 (min request-timeout-ms (- @deadline (js/Date.now))))})]
                    (when-not (and (= 200 (:status response)) (http/edn-response? (:content-type response)))
                      (throw (coded (if (= 404 (:status response)) "EOBSERVEUNAVAILABLE" "EBADRESPONSE") (:text response))))
                    (data/read-edn (:text response))))
          job-history (fn [result]
                        (if (and (= :job-finished (:wake result)) (not (contains? result :history)))
                          (-> (job-results/read! get! (:socket-path request) (:job result) {:signal signal :deadline @deadline})
                              (.then #(with-history result %))
                              (.catch (fn [error] (if (aborted?) (throw error) (with-history result {})))))
                          (js/Promise.resolve result)))
          finish! (fn [result]
                    (when (aborted?) (throw (coded "ABORT_ERR" "cancelled")))
                    (js-await [result (job-history result)]
                      (let [output (summary-result @summary result)]
                        (js-await [_ (js/Promise.resolve (deliver output))]
                          (checkpoint! file @st)
                          output))))
          timeout! (fn [] (finish! (if (seq (:counts @summary)) (array-map :wake :timeout) (array-map :wake :timeout :changed false))))
          reset-with-status! (fn [reason]
                               (js-await [status (read! "/status")]
                                 (finish! (array-map :wake :reset :reason reason :status (compact-status status)))))]
      (-> (js/Promise.resolve nil)
          (.then
           (fn []
             (let [saved (saved-checkpoint file)]
               (when (> (observer-count dir) 64) (throw (coded "EOBSERVERLIMIT" "observer limit")))
               (vreset! deadline (+ (js/Date.now) (:timeout-ms opts)))
               (js-await [snap (read! "/snapshot")]
                 (let [generation (:generation-id snap)
                       lookup (or (nil? saved) (true? (:lookup saved)))]
                   (reset! st {:cursor (or (:cursor saved) (:cursor snap)) :generation generation
                               :seen (or (:seen saved) {}) :lookup lookup :pending (vec (:pending saved)) :snap snap})
                   (when-not saved
                     (checkpoint! file (select-keys @st [:cursor :generation :seen :lookup])))
                   (vreset! timeout-finish timeout!)
                   (if (and saved (not= (:generation saved) generation))
                     (do (swap! st assoc :cursor (:cursor snap) :seen {} :pending [] :lookup true)
                         (reset-with-status! :engine-restarted))
                     (let [body (or (:body snap) (:agent request))
                           watching? (or (seq (:watch-actions opts)) (seq (:watch opts)))]
                       (letfn [(poll []
                                 (let [{:keys [cursor]} @st]
                                   (js-await [page (read! (events-query (:stream-id cursor) (:seq cursor) 256))]
                                     (if (:gap? page)
                                       (do (swap! st assoc :cursor (:cursor page) :seen {})
                                           (reset-with-status! :event-gap))
                                       (process-events page (:events page))))))
                               (process-events [page events]
                                 (if (empty? events)
                                   (cond
                                     (>= (js/Date.now) @deadline) (timeout!)
                                     (< (:seq (:cursor @st)) (:latest-seq page)) (step)
                                     :else (js-await [_ (delay! (min (:poll-ms opts) (max 1 (- @deadline (js/Date.now)))) signal)]
                                             (step)))
                                   (let [event (first events)
                                         more #(process-events page (rest events))]
                                     (swap! st assoc :cursor {:stream-id (:stream-id page) :seq (:seq event)})
                                     (if (= :required (:attention event))
                                       (js-await [snap (read! "/snapshot")]
                                         (swap! st assoc :snap snap)
                                         (let [update (attention-changes (:outstanding snap) (:seen @st))]
                                           (swap! st assoc :seen (:seen update))
                                           (if (seq (:changed update))
                                             (finish! (attention-wake update))
                                             (continue-event event more (rest events) page))))
                                       (continue-event event more (rest events) page)))))
                               (continue-event [event more later page]
                                 (when (and (= :attention (:source event)) (= :resolved (:kind event)))
                                   (let [id (id-str (:request-id event))
                                         drop-id (fn [m] (into (empty m) (remove (fn [[k _]] (= id (id-str k)))) m))]
                                     (swap! st update :seen dissoc id)
                                     (swap! st update-in [:snap :outstanding] #(drop-id (or % {})))))
                                 (if (and (= :system (:source event)) (#{:started :restored} (:kind event)))
                                   (js-await [snap (read! "/snapshot")]
                                     ;; The restart wake says nothing of the events skipped before it (old jobs); the
                                     ;; watched jobs are looked up in the history on the next call.
                                     (reset! summary {:counts {} :items [] :more false})
                                     (swap! st assoc :snap snap :cursor (:cursor snap) :seen {} :pending [] :lookup true)
                                     (finish! (array-map :wake :reset :reason :engine-restarted)))
                                   (let [failed? (= :reconnect-failed (:kind event))
                                         immediate (when-not (and failed? (stale-reconnect? event later))
                                                     (classify event opts body))
                                         skip! (fn [] (swap! summary collect event) (more))]
                                     (cond
                                       (nil? immediate) (skip!)
                                       (and failed? (empty? later) (< (:seq event) (:latest-seq page)))
                                       (js-await [back? (recovered-later? event page)]
                                         (if back? (skip!) (finish! immediate)))
                                       :else (finish! immediate)))))
                               (recovered-later? [event page]
                                 ;; the batch is one page: look at the following pages for the body coming back
                                 (letfn [(from [after pages]
                                           (js-await [p (read! (events-query (:stream-id page) after 256))]
                                             (let [evs (:events p)]
                                               (cond (some recovered? evs) true
                                                     (or (empty? evs) (zero? pages) (:gap? p)) false
                                                     :else (from (:seq (last evs)) (dec pages))))))]
                                   (from (:seq event) 8)))
                               (after-lookup []
                                 (swap! st assoc :lookup false)
                                 (swap! st update :pending
                                        (fn [pending] (filterv #(if (= :action-finished (:wake %))
                                                                  (some #{(:action %)} (:watch-actions opts))
                                                                  (some #{(:job %)} (:watch opts)))
                                                               pending)))
                                 (if-let [pending (seq (:pending @st))]
                                   (do (swap! st assoc :pending (vec (rest pending)))
                                       (finish! (first pending)))
                                   (if (>= (js/Date.now) @deadline)
                                     (timeout!)
                                     (poll))))
                               (lookup-history []
                                 (job-results/history! get! (:socket-path request)
                                                       {:signal signal :deadline @deadline
                                                        :snap (assoc (:snap @st) :cursor (:cursor @st))}))
                               (step []
                                 (let [changes (attention-changes (:outstanding (:snap @st)) (:seen @st))]
                                   (swap! st assoc :seen (:seen changes))
                                   (cond
                                     (seq (:changed changes)) (finish! (attention-wake changes))

                                     (and (:lookup @st) watching?)
                                     (js-await [history (lookup-history)]
                                       (let [unavailable (unavailable-jobs (:events history) (:cursor @st) generation opts (:snap @st))]
                                         (cond
                                           (:gap? history)
                                           (do (swap! st assoc :lookup false)
                                               (finish! (array-map :wake :reset :reason :history-unavailable)))

                                           (seq unavailable)
                                           (do (swap! st assoc :lookup false)
                                               (finish! (array-map :wake :reset :reason :history-unavailable :jobs unavailable
                                                                   :history-window job-results/history-limit)))

                                           :else
                                           (do (swap! st assoc :pending (-> (recent-results (:events history) (:cursor @st) generation opts body)
                                                                            (with-projections history generation)))
                                               (after-lookup)))))

                                     :else (after-lookup))))]
                         (step)))))))))
          (.catch (fn [error]
                    (if (and (= "ETIMEDOUT" (code-of error)) @deadline (>= (js/Date.now) @deadline) @timeout-finish)
                      (@timeout-finish)
                      (throw error))))
          (.finally release)))
    (catch :default error (js/Promise.reject error))))

;; Requests

(def body-name #"[A-Za-z0-9_-]{1,40}")
(def world-name #"[A-Za-z0-9_-]{1,64}")

(def option-spec
  {:state {:type "string"} :worlds {:type "string"} :world {:type "string"}
   :limit {:type "string"} :offset {:type "string"}
   :raw {:type "boolean" :default false} :slots {:type "boolean" :default false}
   :verbose {:type "boolean" :default false} :wait {:type "boolean" :default false}
   :timeout {:type "string"} :chatter {:type "string"} :observer {:type "string"}
   :watch {:type "string" :multiple true} :watch-action {:type "string" :multiple true}
   :from {:type "string"} :poll-ms {:type "string"}
   :danger {:type "boolean" :default false} :disconnect {:type "boolean" :default false}})

(defn fail [message] (throw (ex-info message {::error message})))

(defn check [bad? message] (when bad? (fail message)))

(defn integer-in? [n low high] (and (js/Number.isInteger n) (<= low n high)))

(defn limit-param
  "The validated --limit as a query value (nil when absent)."
  [limit high]
  (when (some? limit)
    (let [n (js/Number limit)]
      (check (not (integer-in? n 1 high)) (str "--limit must be an integer from 1 to " high))
      (str n))))

(defn listed [values] (vec (mapcat #(str/split % #",") values)))

(def duration-units {"ms" 1 "s" 1000 "m" 60000})

(defn wait-options [{:keys [timeout observer chatter watch watch-action from poll-ms danger disconnect]}]
  (let [[_ amount unit] (re-matches #"^(\d+(?:\.\d+)?)(ms|s|m)?$" (or timeout "60s"))
        timeout-ms (if amount (* (js/Number amount) (duration-units (or unit "s"))) js/NaN)
        observer (or observer "agent")
        chatter (or chatter "addressed")
        watch (listed (some-> watch array-seq))
        watch-actions (listed (some-> watch-action array-seq))
        poll (js/Number (or poll-ms 250))]
    (check (not (and (js/Number.isFinite timeout-ms) (<= 10 timeout-ms 3600000))) "--timeout must be between 10ms and 60m")
    (check (not (re-matches body-name observer)) "--observer must be 1-40 letters, digits, underscores or hyphens")
    (check (not (#{"none" "addressed" "all"} chatter)) "--chatter must be none, addressed, or all")
    (check (or (some #(not (re-matches #"j[0-9]+" %)) watch) (> (count watch) 32)) "--watch needs up to 32 comma-separated job IDs")
    (check (or (some #(not (re-matches #"[A-Za-z0-9_.:-]{1,80}" %)) watch-actions) (> (count watch-actions) 32))
           "--watch-action needs up to 32 comma-separated action request IDs")
    (check (and (truthy-text? from) (not (re-matches body-name from))) "--from must be a player name")
    (check (not (integer-in? poll 50 5000)) "--poll-ms must be 50-5000")
    {:timeout-ms timeout-ms :observer observer :chatter chatter :watch watch :watch-actions watch-actions
     :poll-ms poll :from from :danger danger :disconnect disconnect}))

(defn endpoint-for
  "[endpoint params] of the operation, validating its arguments."
  [op kind-or-id rest values]
  (case op
    "status"
    (do (check (or (some? kind-or-id) (seq rest)) "status takes no positional arguments")
        (check (some? (:offset values)) "--offset is only valid for catalog lists")
        (check (and (:raw values) (some? (:limit values))) "--limit cannot be combined with --raw")
        [(if (:raw values) "/snapshot" "/status")
         (if-let [limit (limit-param (:limit values) 32)] [["limit" limit]] [])])

    ("inventory" "equipment")
    (do (check (or (some? kind-or-id) (seq rest)) (str op " takes no positional arguments"))
        (check (or (some? (:limit values)) (some? (:offset values))) (str "--limit and --offset are not valid for " op))
        (check (and (:raw values) (:slots values)) "--slots is redundant with --raw")
        (check (and (= "equipment" op) (:slots values)) "--slots is only valid for inventory")
        ["/inventory" []])

    ("job" "result")
    (do (check (or (not (truthy-text? kind-or-id)) (seq rest)) (str op " needs one job ID, such as j12"))
        (check (and (= "result" op) (not (re-matches #"j[0-9]+" kind-or-id))) "result needs a job ID such as j12")
        (check (and (= "result" op) (some? (:limit values))) "result has a fixed bounded history; --limit is not accepted")
        (check (or (:raw values) (some? (:offset values))) "--raw and --offset are only valid for status and catalog lists respectively")
        ["/job" (into [["id" kind-or-id]] (when-let [limit (limit-param (:limit values) 32)] [["limit" limit]]))])

    "catalog"
    (cond
      (#{"job" "trigger"} kind-or-id)
      (let [[name] rest]
        (check (or (:raw values) (some? (:limit values)) (some? (:offset values))) "catalog detail does not accept --raw, --limit, or --offset")
        (check (or (not (truthy-text? name)) (not= 1 (count rest))) "catalog needs job <jobs.namespace.name> or trigger <trigger-name>")
        (check (and (= "job" kind-or-id) (not (re-matches #"jobs(?:\.[a-z][a-z0-9-]*)+" name))) "job name must be an exact jobs namespace")
        (check (and (= "trigger" kind-or-id) (not (re-matches #"[a-z][a-z0-9-]*" name))) "trigger name must be a lowercase identifier")
        ["/catalog" [["kind" kind-or-id] ["name" name]]])

      (#{"jobs" "triggers"} kind-or-id)
      (let [prefix (or (first rest) "")
            limit (if (nil? (:limit values)) 20 (js/Number (:limit values)))
            offset (if (nil? (:offset values)) 0 (js/Number (:offset values)))]
        (check (:raw values) "--raw is only valid for status")
        (check (> (count rest) 1) "catalog list accepts at most one prefix")
        (check (and (= "jobs" kind-or-id) (truthy-text? prefix) (not (re-matches #"jobs(?:\.[a-z][a-z0-9-]*)*(?:\.)?" prefix)))
               "job prefix must start with jobs.")
        (check (and (= "triggers" kind-or-id) (truthy-text? prefix) (not (re-matches #"[a-z][a-z0-9-]*" prefix)))
               "trigger prefix must be a lowercase identifier prefix")
        (check (not (integer-in? limit 1 64)) "--limit must be an integer from 1 to 64")
        (check (not (integer-in? offset 0 10000)) "--offset must be an integer from 0 to 10000")
        ["/catalog" [["kind" kind-or-id] ["prefix" prefix] ["limit" (str limit)] ["offset" (str offset)]]])

      :else (fail "catalog needs job|trigger <name> or jobs|triggers [prefix]"))

    (fail (str "unknown operation " op))))

(defn query-string [params]
  (let [search (js/URLSearchParams.)]
    (doseq [[k v] params] (.set search k v))
    (.toString search)))

(defn request-for
  "The request an argv describes: {:agent :world :state :socket-path :path} plus :wait-options, :inventory-mode with
  :slots and :raw, and :verbose when asked for; {:error message} when it is not valid."
  [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options argv option-spec)
          [agent requested-op kind-or-id & rest] positionals
          op (or requested-op "status")
          world (:world values)]
      (check (and (:slots values) (not= "inventory" op)) "--slots is only valid for inventory")
      (check (not (and agent (re-matches body-name agent))) "agent must be a body name")
      (check (nil? world) (bodies/missing-world-error "--world"))
      (check (not (re-matches world-name world)) "the world must be a name of letters, digits, _ and -")
      (let [state (bodies/storage-root values map-tool/default-state-dir)
            [endpoint params] (endpoint-for op kind-or-id rest values)
            wait-options (when (:wait values)
                           (check (or (not= "status" op) (:raw values) (:verbose values) (truthy-text? (:limit values)))
                                  "--wait is only valid with compact status")
                           (wait-options values))
            query (query-string params)]
        (when-not (:wait values)
          (check (or (some #(some? (get values %)) [:timeout :chatter :observer :watch :watch-action :from :poll-ms])
                     (:danger values) (:disconnect values))
                 "wait options require --wait"))
        (check (and (:verbose values) (or (not= "status" op) (:raw values)))
               "--verbose is only valid for status without --raw")
        (cond-> {:agent agent :world world :state state
                 :socket-path (.join path (bodies/body-dir state world agent) "engine" "events.sock")
                 :path (if (seq query) (str endpoint "?" query) endpoint)}
          wait-options (assoc :wait-options wait-options)
          (= "result" op) (assoc :result-id kind-or-id)
          (#{"inventory" "equipment"} op) (assoc :inventory-mode (keyword op) :slots (:slots values) :raw (:raw values))
          (:verbose values) (assoc :verbose true))))
    (catch :default error
      {:error (or (some-> (ex-data error) ::error) (.-message error))})))

(defn legacy-notice [request]
  (let [endpoint (first (str/split (:path request) #"\?"))
        fallback (if (= "/inventory" endpoint)
                   ""
                   (str " :fallback {:op :status :raw true :world " (js/JSON.stringify (:world request))
                        " :state " (data/write-edn (:state request)) "}"))]
    (str "{:ok false :reason :observe-unavailable :body " (js/JSON.stringify (:agent request))
         " :endpoint " (js/JSON.stringify endpoint) " :action :restart-with-current-build" fallback "}")))

(def unsupported-route? http/unsupported-route?)

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
  {"ABORT_ERR" :cancelled "EOBSERVERBUSY" :observer-busy "EOBSERVERLIMIT" :observer-limit
   "EATTENTIONLIMIT" :attention-limit "EOBSERVEUNAVAILABLE" :observe-unavailable "ETIMEDOUT" :timeout
   "ERESPONSETOOLARGE" :response-too-large "ECONNREFUSED" :no-running-body "ENOENT" :no-running-body
   "EACCES" :socket-access-denied})

(defn failure-text [request error]
  (str "{:ok false :reason " (data/write-edn (get failure-reasons (code-of error) :transport-error))
       " :body " (js/JSON.stringify (:agent request)) "}\n"))

(defn line [text] (if (str/ends-with? text "\n") text (str text "\n")))

(defn wait-for!
  "Wait as observe --wait does, watching the jobs watch and the world actions watch-actions (ID lists): a promise of
  the wake map, or of {:ok false :reason r} when the wait could not run. base is {:agent :world :state :socket-path};
  timeout and observer as on the command line (default 60s and agent). SIGINT and SIGTERM cancel it."
  [base {:keys [watch watch-actions timeout observer]} get!]
  (let [controller (js/AbortController.)
        cancel #(.abort controller)
        opts (wait-options {:timeout timeout :observer observer :watch (clj->js watch) :watch-action (clj->js watch-actions)})]
    (.once js/process "SIGINT" cancel)
    (.once js/process "SIGTERM" cancel)
    (-> (wait-observe (assoc base :wait-options opts) get! (.-signal controller) identity)
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
