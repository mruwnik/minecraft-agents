(ns agent-tools.jobs
  "Job command validation and native EDN requests; HTTP remains a Node boundary."
  (:require [engine.bodies :as bodies]
            [agent-tools.http :as http]
            [agent-tools.job-results :as job-results]
            [agent-tools.map :as map-tool]
            [agent-tools.observe :as observe]
            [agent-tools.observe.request :as observe-request]
            [agent-tools.vitals :as vitals]
            [agent-tools.world-data :as data]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:crypto" :as crypto])
  (:require-macros [dashboard.jobs-registry :refer [position-bound-jobs]]))

(defn usage-for [prefix]
  "Build the usage string with the given invocation prefix (e.g., 'jobs.mjs' or './bin/jobs')."
  (str "usage: " prefix " <body> --world <world> list [--limit 8 --offset 0] | show <jID> | submit '(jobs.storage.deposit {:items [\"bread\"]})' [--hold] [--survive-death] [--next | --now] [--alongside] [--wait [--timeout 60s]] | cancel <jID> | cancel-all | retry <jID> | resolve <request-id> --reason handled|condition-recovered [--worlds DIR --state LEGACY_PARENT]\n"
       "While manual control holds the body (drive.mjs take) only the driver's one job runs (world.mjs submit): others stay :queued and list/submit say who holds it; drive.mjs <body> release frees it.\n"
       "A job that depends on where the body stands (access.stair, access.bridge, access.pillar, gather.mine) gets :submitted-at (the body's position at submit) added to its args; if the body is then over 6 blocks from it when the job starts, the job waits :moved-since-submit (something moved the body meanwhile) instead of running there. Give :here true in the args (any job takes it) to run wherever the body is.\n"
       "submit appends the job to the end of the list (jobs take turns). --hold makes the job hold the body: no other job gets a round until it ends or fails (reflexes still come first).\n"
       "A spec is a job name then its args map: submit '(jobs.movement.go-to {:pos {:x 1 :y 64 :z 2}})' or '(jobs.items.get-tool {:item \"stone_pickaxe\"})'. Find a job's args with observe.mjs <body> catalog job <name> (catalog jobs [prefix] lists names).\n"
       "  --survive-death  the job stays listed through the body's death and runs again once the respawn reflexes (recover-drops, night) are done; other jobs are cancelled; a (repeat ...) spec does this by default\n"
       "  --next  list it directly after the current job: it gets the next round, nothing is cut\n"
       "  --now    cut the current job and run this one at once (a normal job); the cut job keeps its memory and\n"
       "           continues right after this one yields or ends. Reflexes still come first: --now queues behind a running reflex (night, hostile) and the answer's :hint names it. --now and --next cannot be combined (refused).\n"
       "  --alongside  submit even when a job of the same job name, or another digging job (mine, stair, tunnel, strip.*), is running or queued on this body: without it such a submit is held back and the answer names the clashing job; two of one job can undo each other's work. A (repeat ...) submit is never held\n"
       "  --wait   block until the job ends, or until anything that ends observe --wait (addressed chat, attention, an\n"
       "           engine restart, the --timeout), and print {:job .. :wait <the wake>}: the job's last events and a\n"
       "           bounded summary of what else happened meanwhile (reflexes fired, pickups, hurt, other jobs ended,\n"
       "           warnings). If the wait ends before the job, :follow names the command that waits on, :note says the job is not lost, :now has its status and :waiting reason, and the exit code is 1 (0 only when the job ended). Uses the observer\n"
       "           checkpoint observe --wait uses, so the same events are not reported twice.\n"
       "Without --wait mutations return immediately; observe.mjs <body> --world <world> --wait --watch jID tracks completion."))

(def usage (usage-for "jobs.mjs"))

(def spec-example "a spec is one list: a job name then an args map, e.g. (jobs.movement.go-to {:pos {:x 1 :y 64 :z 2}})")

(defn read-spec [text]
  (try (map-tool/read-edn text)
       (catch :default error
         (throw (js/Error. (str (.-message error) "; " spec-example))))))

(defn spec-for [text]
  (when (or (not (string? text)) (> (.byteLength js/Buffer text) 12000))
    (throw (js/Error. "spec must be EDN text, at most 12000 bytes")))
  (let [form (read-spec text)]
    (when-not (and (seq? form) (symbol? (first form)))
      (throw (js/Error. (str "spec must be one native EDN job expression list; " spec-example))))
    form))

(defn standing?
  "A (repeat ...) spec is housekeeping that does not depend on inventory or position, so a death keeps it."
  [text]
  (= 'repeat (first (spec-for text))))

(defn clash-name
  "The job name a submit would clash on: a plain job call's name; nil for a (repeat ...) or other combinator."
  [text]
  (let [head (str (first (spec-for text)))]
    (when (str/starts-with? head "jobs.") head)))

(def digging-jobs #{"jobs.gather.mine" "jobs.access.stair" "jobs.access.tunnel"})

(defn digging?
  "Mine, stair, tunnel and every jobs.strip.* job cut the same rock, so any two undercut each other."
  [job-name]
  (boolean (or (contains? digging-jobs job-name) (str/starts-with? (str job-name) "jobs.strip."))))

(defn clashes?
  [submitted live]
  (or (= submitted live) (and (digging? submitted) (digging? live))))

(defn request-for [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options argv
          {:state {:type "string"} :worlds {:type "string"} :world {:type "string"}
           :request-id {:type "string"} :limit {:type "string"} :offset {:type "string"}
           :reason {:type "string"} :hold {:type "boolean"} :survive-death {:type "boolean"} :next {:type "boolean"} :now {:type "boolean"}
           :wait {:type "boolean"} :timeout {:type "string"} :alongside {:type "boolean"}})
          [body op arg & extra] positionals
          op (keyword (or op "list"))
          interrupt? (and (= op :submit) (true? (:now values)))
          no-argument? (#{:list :cancel-all} op)
          mutating? (boolean (#{:submit :cancel :cancel-all :retry :resolve} op))
          spec-op? (= :submit op)]
      (when-not (and (string? body) (re-matches #"[A-Za-z0-9_-]{1,40}" body))
        (throw (js/Error. "body must be a valid name")))
      (when (nil? (:world values))
        (throw (js/Error. "missing --world <world>: the world the body plays in (a folder under worlds/)")))
      (when-not (re-matches #"[A-Za-z0-9_-]{1,64}" (:world values))
        (throw (js/Error. "the world must be a name of letters, digits, _ and -")))
      (when-not (#{:list :show :submit :cancel :cancel-all :retry :resolve} op)
        (throw (js/Error. "unknown operation")))
      (when (or (seq extra) (if no-argument? (some? arg) (nil? arg)))
        (throw (js/Error. (str (name op) (if no-argument? " takes no argument" " needs exactly one argument")
                         (when spec-op? (str "; " spec-example))))))
      (when (and (not mutating?) (:request-id values))
        (throw (js/Error. "--request-id requires a mutation")))
      (when (and (= op :resolve) (:request-id values))
        (throw (js/Error. "resolve uses the attention request ID positional argument")))
      (when (and (not= op :list) (or (:limit values) (:offset values)))
        (throw (js/Error. "--limit and --offset require list")))
      (when (and (contains? values :hold) (not spec-op?))
        (throw (js/Error. "--hold requires submit")))
      (when (and (contains? values :survive-death) (not spec-op?))
        (throw (js/Error. "--survive-death requires submit")))
      (when (and (contains? values :next) (not= op :submit))
        (throw (js/Error. "--next requires submit")))
      (when (and (contains? values :now) (not= op :submit))
        (throw (js/Error. "--now requires submit")))
      (when (and (:now values) (:next values))
        (throw (js/Error. "--now and --next are exclusive: --now cuts the current job, --next waits for its round to end")))
      (when (and (contains? values :alongside) (not= op :submit))
        (throw (js/Error. "--alongside requires submit")))
      (when (and (contains? values :wait) (not= op :submit))
        (throw (js/Error. "--wait requires submit")))
      (when (and (:timeout values) (not (:wait values)))
        (throw (js/Error. "--timeout requires --wait")))
      (when (:wait values)
        (observe-request/wait-options {:timeout (:timeout values)}))
      (when (and (= op :resolve) (not (#{"handled" "condition-recovered"} (:reason values))))
        (throw (js/Error. "resolve requires --reason handled or condition-recovered")))
      (when (and (not= op :resolve) (:reason values))
        (throw (js/Error. "--reason requires resolve")))
      (let [state (bodies/storage-root values map-tool/default-state-dir)
            base (cond-> {:body body :world (:world values) :state state
                          :socketPath (.join path (bodies/worlds-dir state) (:world values) "agents" body "engine" "events.sock")
                          :mutating mutating?}
                   (:wait values) (assoc :wait {:timeout (:timeout values)})
                   (and spec-op? (not (:alongside values))) (assoc :clash-name (clash-name arg)))]
        (cond
          (= op :list)
          (let [limit (js/Number (or (:limit values) 8)) offset (js/Number (or (:offset values) 0))]
            (when-not (and (js/Number.isInteger limit) (<= 1 limit 32)
                           (js/Number.isInteger offset) (<= 0 offset 10000))
              (throw (js/Error. "list limit must be 1..32 and offset 0..10000")))
            (assoc base :path (str "/jobs?limit=" limit "&offset=" offset)))
          :else
          (do
            (when (and (#{:show :cancel :retry} op) (not (re-matches #"j[0-9]+" arg)))
              (throw (js/Error. "job ID must be j<number>")))
            (when (and (= op :resolve) (not (re-matches #"[A-Za-z0-9_.:-]{1,80}" arg)))
              (throw (js/Error. "attention request ID must be a short identifier")))
            (if (= op :show)
              (assoc base :path (str "/job?id=" arg))
              (if (= op :resolve)
                (assoc base :path "/attention/resolve" :resolve true
                       :request {:request-id arg :reason (keyword (:reason values))})
              (let [id (or (:request-id values) (.randomUUID crypto))]
                (when-not (re-matches #"[A-Za-z0-9_.:-]{1,80}" id)
                  (throw (js/Error. "--request-id must be a short identifier")))
                (assoc base :path "/jobs"
                       :request (cond-> {:op (if interrupt? :interrupt op) :request-id id}
                                  spec-op? (assoc :spec (spec-for arg))
                                  (#{:cancel :retry} op) (assoc :id arg)
                                  (contains? values :hold) (assoc :hold? (:hold values))
                                  (or (:survive-death values) (and spec-op? (standing? arg))) (assoc :survive-death? true)
                                  (contains? values :next) (assoc :next? (:next values)))))))))))
    (catch :default error {:error (.-message error)})))

;; Transport

(def request-timeout-ms 3000)
(def max-get-bytes 262144)
(def max-post-bytes 65536)
(def max-generation-records 128)
(def private-dir-mode 448)  ; 0700
(def private-file-mode 384) ; 0600

(defn get! [socket-path path {:keys [request-fn]}]
  (http/request (cond-> {:socket-path socket-path :path path :label "jobs" :timeout-ms request-timeout-ms :max-bytes max-get-bytes}
                  request-fn (assoc :request-fn request-fn))))

(defn post!
  "POST an EDN body (default path /jobs): a promise of {:status :content-type :text}; options :path :timeout-ms :request-fn."
  [socket-path body {:keys [path timeout-ms request-fn] :or {path "/jobs" timeout-ms request-timeout-ms}}]
  (http/request (cond-> {:socket-path socket-path :method "POST" :path path :label "jobs" :timeout-ms timeout-ms
                         :max-bytes max-post-bytes :headers {"content-type" "application/edn"} :body (data/write-edn body)}
                  request-fn (assoc :request-fn request-fn))))

(defn read-generation [file] (:generation-id (data/read-edn (.readFileSync fs file "utf8"))))

(defn prune-generations! [meta-dir]
  (let [files (->> (array-seq (.readdirSync fs meta-dir))
                   (filter #(str/ends-with? % ".edn"))
                   (map (fn [f] (let [file (.join path meta-dir f)] {:file file :at (.-mtimeMs (.statSync fs file))})))
                   (sort-by :at >))]
    (doseq [{:keys [file]} (drop max-generation-records files)]
      (.rmSync fs file #js {:force true}))))

(defn record-generation!
  "The generation a request ID is bound to, so a retry with the same ID reuses it. The first writer wins."
  [meta-file meta-dir generation]
  (let [written (try (.writeFileSync fs meta-file (data/write-edn {:generation-id generation})
                                     #js {:mode private-file-mode :flag "wx"})
                     generation
                     (catch :default error
                       (when-not (= "EEXIST" (.-code error)) (throw error))
                       (read-generation meta-file)))]
    (prune-generations! meta-dir)
    written))

(defn generation-for!
  "Bind the request to a generation: the one recorded for its ID, else the snapshot's."
  [r snapshot]
  (let [meta-dir (.join path (.dirname path (.dirname path (:socketPath r))) ".commands" "jobs")
        meta-file (.join path meta-dir (str (get-in r [:request :request-id]) ".edn"))
        _ (.mkdirSync fs meta-dir #js {:recursive true :mode private-dir-mode})
        cached (when (.existsSync fs meta-file) (read-generation meta-file))
        generation (or cached (:generation-id (data/read-edn (:text snapshot))))]
    (when-not (string? generation) (throw (js/Error. "generation unavailable")))
    (if cached generation (record-generation! meta-file meta-dir generation))))

(defn stamp-from
  "spec with :submitted-at position added to its args map (none given: a new one) when the job is position-bound and the call gave neither :submitted-at nor :here; else spec. The runner makes such a call wait if the body has moved (engine.core.ending/moved-or)."
  [spec position takes-from?]
  (let [args (second spec)]
    (if (and takes-from? position (nil? (nnext spec)) (or (nil? args) (map? args))
             (not (contains? args :submitted-at)) (not (contains? args :here)))
      (list (first spec) (assoc args :submitted-at (select-keys position [:x :y :z])))
      spec)))

(def from-jobs
  "Jobs whose meaning depends on where the body stands (:position-bound? in their meta): they get :submitted-at."
  (position-bound-jobs))

(defn takes-from?
  "Whether the spec's job is one of from-jobs."
  [spec]
  (contains? from-jobs (str (first spec))))

(defn stamped
  "The request body with :submitted-at stamped into a submitted spec (see stamp-from); position is the snapshot's."
  [request position]
  (if-not (and (#{:submit :interrupt} (:op request)) (:spec request) position)
    request
    (update request :spec stamp-from position (takes-from? (:spec request)))))

(defn exchange! [r opts]
  (cond
    (:resolve r) (post! (:socketPath r) (:request r) (assoc opts :path (:path r)))
    (:mutating r)
    (.then (get! (:socketPath r) "/snapshot" opts)
           (fn [snapshot]
             (when-not (and (= 200 (:status snapshot)) (http/edn-response? (:content-type snapshot)))
               (throw (js/Error. "snapshot unavailable")))
             (let [generation (generation-for! r snapshot)]
               (post! (:socketPath r) (-> (stamped (:request r) (:position (data/read-edn (:text snapshot)))) (assoc :generation-id generation)) (assoc opts :path (:path r))))))
    :else (get! (:socketPath r) (:path r) opts)))

(defn job-detail [value]
  (cond-> (into {} (filter (comp some? val)) (select-keys value [:id :name :status :round :spec :waiting]))
    (:failure value) (assoc :failure (:failure value))
    (pos? (or (get-in value [:attention :total]) 0)) (assoc :attention (:attention value))))

(defn print-text! [text] (.write (.-stdout js/process) text))

(defn line [text] (if (str/ends-with? text "\n") text (str text "\n")))

(defn history-outcome!
  "show of a job the scheduler no longer holds (finished or dropped): its outcome from the retained event history, as
  observe job prints it. A promise of the exit code."
  [r output {:keys [request-fn]}]
  (let [get! (fn [socket endpoint options]
               (observe/get! socket endpoint (cond-> options request-fn (assoc :request-fn request-fn))))
        id (second (re-find #"id=(j[0-9]+)" (:path r)))]
    (.then (job-results/read! get! (:socketPath r) id {})
           (fn [outcome]
             (.then (js/Promise.resolve (output (str (data/write-edn outcome) "\n")))
                    (fn [_] (if (false? (:ok outcome)) 1 0)))))))

(def finished-hint "no such job in the list: it may have finished already (finished jobs leave it); jobs show <jID> gives its outcome")

(def hold-hint "--hold holds the body, it does not pause the job: it runs now; cancel <jID> stops it while it is listed")

(defn manual-hint [{:keys [who why]}]
  (str "waiting: manual control held by " who (when (seq why) (str " (" why ")"))
       "; only its job runs meanwhile, queued jobs run after drive.mjs <body> release"))

(defn reflex-hint
  "Why a queued job waits behind the running reflex of /status's :current: its id, name and, for the night handler, until when."
  [{:keys [id name reflex]}]
  (str "queued behind reflex " (pr-str reflex) " (" id " " name "): reflexes hold the body first, the job runs once it ends"
       (when (= :night reflex) " (night: about dawn)")))

(defn away-hint [{:keys [by why back-at]}]
  (str "queued: the body is logged out (" (some-> by name) (when why (str " " (name why))) (when back-at (str ", back about " back-at)) "); the job runs after it is back"))

(defn queued? [value]
  (or (= :queued (get-in value [:job :status]))
      (some #(= :queued (:status %)) (:items value))))

(defn with-hint
  "The mutation answer plus a :hint where its meaning is easily misread: a cancel/retry of a job that is gone, a held submit,
  a queued job while manual (the :manual map of /status) holds the body."
  ([r value] (with-hint r value nil))
  ([r value {:keys [manual current offline]}]
  (cond
    (and manual (queued? value)) (assoc value :hint (manual-hint manual))
    (and (:reflex current) (queued? value)) (assoc value :hint (reflex-hint current))
    (and offline (queued? value)) (assoc value :hint (away-hint offline))
    (and (:mutating r) (= :job-not-found (:reason value))) (assoc value :hint finished-hint)
    (and (:mutating r) (true? (get-in value [:job :hold?]))) (assoc value :hint hold-hint)
    :else value)))

(defn manual-for
  "A promise of /status's {:manual :current :offline} when the answer has queued jobs (a list, or a submit), else nil; nil when status is unreadable."
  [r response opts]
  (let [value (when (http/edn-response? (:content-type response)) (try (data/read-edn (:text response)) (catch :default _ nil)))]
    (if-not (and (= 200 (:status response)) (queued? value)
                 (or (str/starts-with? (:path r) "/jobs?") (#{:submit :interrupt} (get-in r [:request :op]))))
      (js/Promise.resolve nil)
      (-> (get! (:socketPath r) "/status" opts)
          (.then (fn [status] (when (http/edn-response? (:content-type status)) (select-keys (data/read-edn (:text status)) [:manual :current :offline]))))
          (.catch (fn [_] nil))))))

(defn deliver! [r output {:keys [status content-type text] :as response} & [opts]]
  (when-not (http/edn-response? content-type) (throw (js/Error. "unexpected response format")))
  (let [value (data/read-edn text)
        print! #(output (str (data/write-edn %) "\n"))]
    (cond
      (and (not (:mutating r)) (str/starts-with? (:path r) "/job?") (= 404 status) (= :job-not-found (:reason value)))
      (history-outcome! r output (or opts {}))

      (and (= 404 status) (= :not-found (:reason value)))
      (do (print! {:ok false :reason :jobs-unavailable :action :restart-with-current-build}) 2)

      (and (not (:mutating r)) (str/starts-with? (:path r) "/job?") (= 200 status))
      (do (print! (job-detail value)) 0)

      (and (:mutating r) (= :request-uncertain (:reason value)))
      (do (print! (assoc value :request-id (get-in r [:request :request-id]))) 1)

      (not= value (with-hint r value (:status opts)))
      (do (print! (with-hint r value (:status opts))) (if (= 200 status) 0 1))

      :else (do (output (line text)) (if (= 200 status) 0 1)))))

(defn failure-for [r error]
  (let [request-id (get-in r [:request :request-id])]
    (cond-> {:ok false :reason (http/transport-reason error)}
      (:resolve r) (assoc :request-id request-id :confirmation :unknown
                          :message "Resolve confirmation is unknown; inspect outstanding attention before another request.")
      (and (:mutating r) (not (:resolve r)))
      (merge (if (= :no-running-body (http/transport-reason error))
               {:confirmation :not-sent
                :message "The body is not running (restarting?), so nothing was queued. Wait a little, then send the command again."}
               {:request-id request-id :confirmation :unknown
                :message (str "The body may or may not have queued this. Repeat the same command with --request-id " request-id " (the body returns the first result if it already ran it); do not submit a new ID.")})))))

(defn follow-command [id] (str "./bin/observe --wait --watch " id))

(defn unfinished-note
  "Said when a wait ends before the job does: the job is not lost, what it is doing now (now: job-now's map, nil when
  unknown), and where its result will be."
  [id wake now]
  (let [holders (->> (:jobs wake) (filter :holding) (keep :name))
        tail (str ". Result later: ./bin/jobs show " id ", or keep waiting: " (follow-command id))]
    (str id
         (case (:status now)
           :running " is running, not lost"
           (:queued :resuming) (str " is " (name (:status now)) " (waiting its turn or cut by a reflex), not lost"
                                    (when (seq holders) (str " (holding: " (str/join ", " holders) ")"))
                                    "; it runs when the body is free")
           (str " is still queued, running or cut by a reflex, not lost"
                (when (seq holders) (str " (holding: " (str/join ", " holders) ")"))
                "; it runs when the body is free"))
         tail)))

(defn wait-result
  "The submit answer with the wake that ended the wait under :wait, and :follow and :note when the job had not ended by then."
  ([answer id wake] (wait-result answer id wake nil))
  ([answer id wake now]
   (cond-> (assoc answer :wait wake)
     (not (and (= :job-finished (:wake wake)) (= id (:job wake))))
     (assoc :follow (follow-command id) :note (unfinished-note id wake now)))))

(defn job-now
  "A promise of the job as show prints it (status, waiting reason), so a wait that ended first says what holds the job; nil when unreadable."
  [r id get! opts]
  (-> (get! (:socketPath r) (str "/job?id=" id) opts)
      (.then (fn [{:keys [status content-type text]}]
               (when (and (= 200 status) (http/edn-response? content-type))
                 (job-detail (data/read-edn text)))))
      (.catch (fn [_] nil))))

(defn submit-and-wait!
  "Submit, then wait for the job as observe --wait --watch does; print one map. A refused submit prints as without
  --wait and does not wait. A promise of the exit code."
  [r output {:keys [request-fn] :as opts}]
  (.then (exchange! r opts)
         (fn [{:keys [status content-type text] :as response}]
           (let [answer (when (http/edn-response? content-type) (data/read-edn text))
                 id (get-in answer [:job :id])]
             (if-not (and (= 200 status) (:ok answer) (string? id))
               (deliver! r output response)
               (let [get! (fn [socket endpoint options]
                            (observe/get! socket endpoint (cond-> options request-fn (assoc :request-fn request-fn))))]
                 (.then (manual-for r response opts)
                        (fn [manual]
                          (.then (vitals/around! (:socketPath r) get!
                                                 #(observe/wait-for! {:agent (:body r) :world (:world r) :state (:state r) :socket-path (:socketPath r)}
                                                                     {:watch [id] :timeout (get-in r [:wait :timeout])} get!))
                                 (fn [[wake report]]
                                   (let [waited (wait-result answer id wake)]
                                     (.then (if (:follow waited) (job-now r id get! opts) (js/Promise.resolve nil))
                                            (fn [now]
                                              (let [waited (wait-result answer id wake now)]
                                              (output (str (data/write-edn (-> (with-hint r waited (when (:follow waited) manual))
                                                                               (cond-> now (assoc :now now))
                                                                               (assoc :vitals report))) "\n"))
                                              (if (:follow waited) 1 0)))))))))))))))

(defn clash-hint [body submitted {:keys [id name label status]}]
  (str id " " name (when label (str " (" label ")")) " is " (cljs.core/name status) " on this body; submitting " submitted " can undo its work (two digs undercut each other). "
       "Wait for it (observe.mjs " body " --wait --watch " id "), stop it (jobs.mjs " body " cancel " id "), or run both anyway: repeat this submit with --alongside."))

(defn clashes-for
  "A promise of the live jobs of the body that share the submit's job name or are both digging jobs (nil when none, or the list is unreadable)."
  [r opts]
  (if-not (:clash-name r)
    (js/Promise.resolve nil)
    (-> (get! (:socketPath r) "/jobs?limit=32&offset=0" opts)
        (.then (fn [{:keys [status content-type text]}]
                 (when (and (= 200 status) (http/edn-response? content-type))
                   (->> (:items (data/read-edn text))
                        (filter #(and (clashes? (:clash-name r) (:name %)) (#{:running :queued :resuming} (:status %)) (not (:reflex %))))
                        (map #(select-keys % [:id :name :label :status]))
                        seq
                        vec))))
        (.catch (fn [_] nil)))))

(defn run-submit! [r output opts]
  (if (:wait r)
    (submit-and-wait! r output opts)
    (.then (exchange! r opts) (fn [response] (.then (manual-for r response opts) (fn [manual] (deliver! r output response (assoc opts :status manual))))))))

(defn run-request!
  "Send the request map r (request-for's shape) and print the answer; a promise of the exit code."
  [r output {:keys [request-fn] :as opts}]
  (-> (clashes-for r opts)
      (.then (fn [clashes]
               (if (seq clashes)
                 (do (output (str (data/write-edn {:ok false :reason :job-clash :clashes clashes :hint (clash-hint (:body r) (:clash-name r) (first clashes))}) "\n"))
                     1)
                 (run-submit! r output opts))))
      (.catch (fn [error]
                (output (str (data/write-edn (failure-for r error)) "\n"))
                2))))

(defn main!
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv] (main! argv {}))
  ([argv {:keys [output] :or {output print-text!} :as opts}]
   (let [r (request-for argv)]
     (if (:error r)
       (http/print-bad-args! output (:error r) usage)
       (run-request! r output opts)))))
