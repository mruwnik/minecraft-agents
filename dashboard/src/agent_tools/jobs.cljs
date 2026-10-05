(ns agent-tools.jobs
  "Job command validation and native EDN requests; HTTP remains a Node boundary."
  (:require [engine.bodies :as bodies]
            [agent-tools.http :as http]
            [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:crypto" :as crypto]))

(def usage "usage: jobs.mjs <body> --world <world> list [--limit 8 --offset 0] | show <jID> | submit <EDN-spec> [--hold --front] | interrupt <EDN-spec> | cancel <jID> | cancel-all | retry <jID> | resolve <request-id> --reason handled|condition-recovered [--worlds DIR --state LEGACY_PARENT]\nMutations return immediately; observe.mjs <body> --world <world> --wait --watch jID tracks completion.")

(defn spec-for [text]
  (when (or (not (string? text)) (> (.byteLength js/Buffer text) 12000))
    (throw (js/Error. "spec must be EDN text, at most 12000 bytes")))
  (let [form (map-tool/read-edn text)]
    (when-not (and (seq? form) (symbol? (first form)))
      (throw (js/Error. "spec must be one native EDN job expression list")))
    form))

(defn request-for [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options argv
          {:state {:type "string"} :worlds {:type "string"} :world {:type "string"}
           :request-id {:type "string"} :limit {:type "string"} :offset {:type "string"}
           :reason {:type "string"} :hold {:type "boolean"} :front {:type "boolean"}})
          [body op arg & extra] positionals
          op (keyword (or op "list"))
          no-argument? (#{:list :cancel-all} op)
          mutating? (boolean (#{:submit :interrupt :cancel :cancel-all :retry :resolve} op))
          spec-op? (#{:submit :interrupt} op)]
      (when-not (and (string? body) (re-matches #"[A-Za-z0-9_-]{1,40}" body))
        (throw (js/Error. "body must be a valid name")))
      (when (nil? (:world values))
        (throw (js/Error. "missing --world <world>: the world the body plays in (a folder under worlds/)")))
      (when-not (re-matches #"[A-Za-z0-9_-]{1,64}" (:world values))
        (throw (js/Error. "the world must be a name of letters, digits, _ and -")))
      (when-not (#{:list :show :submit :interrupt :cancel :cancel-all :retry :resolve} op)
        (throw (js/Error. "unknown operation")))
      (when (or (seq extra) (if no-argument? (some? arg) (nil? arg)))
        (throw (js/Error. (str (name op) (if no-argument? " takes no argument" " needs exactly one argument")))))
      (when (and (not mutating?) (:request-id values))
        (throw (js/Error. "--request-id requires a mutation")))
      (when (and (= op :resolve) (:request-id values))
        (throw (js/Error. "resolve uses the attention request ID positional argument")))
      (when (and (not= op :list) (or (:limit values) (:offset values)))
        (throw (js/Error. "--limit and --offset require list")))
      (when (and (contains? values :hold) (not spec-op?))
        (throw (js/Error. "--hold requires submit or interrupt")))
      (when (and (contains? values :front) (not= op :submit))
        (throw (js/Error. "--front requires submit")))
      (when (and (= op :resolve) (not (#{"handled" "condition-recovered"} (:reason values))))
        (throw (js/Error. "resolve requires --reason handled or condition-recovered")))
      (when (and (not= op :resolve) (:reason values))
        (throw (js/Error. "--reason requires resolve")))
      (let [state (bodies/storage-root values map-tool/default-state-dir)
            base {:body body :world (:world values) :state state
                  :socketPath (.join path (bodies/worlds-dir state) (:world values) "agents" body "engine" "events.sock")
                  :mutating mutating?}]
        (cond
          (= op :list)
          (let [limit (js/Number (or (:limit values) 8)) offset (js/Number (or (:offset values) 0))]
            (when-not (and (js/Number.isInteger limit) (<= 1 limit 32)
                           (js/Number.isInteger offset) (<= 0 offset 10000))
              (throw (js/Error. "list limit must be1..32 and offset0..10000")))
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
                       :request (cond-> {:op op :request-id id}
                                  spec-op? (assoc :spec (spec-for arg))
                                  (#{:cancel :retry} op) (assoc :id arg)
                                  (contains? values :hold) (assoc :hold? (:hold values))
                                  (contains? values :front) (assoc :front? (:front values)))))))))))
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

(defn exchange! [r opts]
  (cond
    (:resolve r) (post! (:socketPath r) (:request r) (assoc opts :path (:path r)))
    (:mutating r)
    (.then (get! (:socketPath r) "/snapshot" opts)
           (fn [snapshot]
             (when-not (and (= 200 (:status snapshot)) (http/edn-response? (:content-type snapshot)))
               (throw (js/Error. "snapshot unavailable")))
             (post! (:socketPath r) (assoc (:request r) :generation-id (generation-for! r snapshot)) (assoc opts :path (:path r)))))
    :else (get! (:socketPath r) (:path r) opts)))

(defn job-detail [value]
  (cond-> (into {} (filter (comp some? val)) (select-keys value [:id :name :status :round :spec :waiting]))
    (:failure value) (assoc :failure (:failure value))
    (pos? (or (get-in value [:attention :total]) 0)) (assoc :attention (:attention value))))

(defn print-text! [text] (.write (.-stdout js/process) text))

(defn line [text] (if (str/ends-with? text "\n") text (str text "\n")))

(defn deliver! [r output {:keys [status content-type text]}]
  (when-not (http/edn-response? content-type) (throw (js/Error. "unexpected response format")))
  (let [value (data/read-edn text)
        print! #(output (str (data/write-edn %) "\n"))]
    (cond
      (and (= 404 status) (= :not-found (:reason value)))
      (do (print! {:ok false :reason :jobs-unavailable :action :restart-with-current-build}) 2)

      (and (not (:mutating r)) (str/starts-with? (:path r) "/job?") (= 200 status))
      (do (print! (job-detail value)) 0)

      (and (:mutating r) (= :request-uncertain (:reason value)))
      (do (print! (assoc value :request-id (get-in r [:request :request-id]))) 1)

      :else (do (output (line text)) (if (= 200 status) 0 1)))))

(defn failure-for [r error]
  (let [request-id (get-in r [:request :request-id])]
    (cond-> {:ok false :reason (if (#{"ENOENT" "ECONNREFUSED"} (aget error "code")) :no-running-body :transport-error)}
      (:resolve r) (assoc :request-id request-id :confirmation :unknown
                          :message "Resolve confirmation is unknown; inspect outstanding attention before another request.")
      (and (:mutating r) (not (:resolve r))) (assoc :request-id request-id :confirmation :unknown
                                                    :message "Query/retry with the same request ID; do not submit a new ID."))))

(defn main!
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv] (main! argv {}))
  ([argv {:keys [output] :or {output print-text!} :as opts}]
   (let [r (request-for argv)]
     (if (:error r)
       (do (js/console.error (str (:error r) "\n" usage)) (js/Promise.resolve 2))
       (-> (js/Promise.resolve nil)
           (.then #(exchange! r opts))
           (.then #(deliver! r output %))
           (.catch (fn [error]
                     (output (str (data/write-edn (failure-for r error)) "\n"))
                     2)))))))
