(ns agent-tools.triggers
  "Trigger registry commands: arg parsing, one-form EDN input, the bounded projection of the register's answers,
  and the generation-aware request over the body's events socket."
  (:require [engine.bodies :as bodies]
            [agent-tools.http :as http]
            [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            [clojure.string :as str]
            ["node:path" :as path]))

(def usage "usage: triggers.mjs <body> --world <world> <command> [id] [options]
  list [--limit 8 --offset 0] | show <id>
  upgrade   (adds the new default triggers a restart offered, at their scenario priority)
  add|put <id> --trigger <predefined> [--args EDN] [--job EDN]
  add|put <id> --when EDN --job EDN
    [--persistence stop|retry|cooldown] [--cooldown-s N] [--for 10m] [--backoff EDN]
  remove <id> | mute <id> [--for 10m] | unmute <id>
  move <id> --before <anchor>|--after <anchor> [--for 10m]
  reset <id> --property position|mute
  [--by agent] [--worlds DIR] [--state LEGACY_PARENT]
Add and put both create or replace a custom entry; built-in entries cannot be replaced or removed.")

(def request-timeout-ms 3000)
(def max-response-bytes 262144)

(defn fail [message] (throw (js/Error. message)))

(defn identifier [s]
  (let [text (if (string? s) (str/replace s #"^:" "") "")]
    (when-not (re-matches #"[a-z][a-z0-9-]{0,39}" text) (fail "trigger ID must be a short lowercase name"))
    (keyword text)))

(defn one-form [text]
  (when (or (not (string? text)) (> (.byteLength js/Buffer text) 12000))
    (fail "EDN input must be at most 12000 bytes"))
  (let [parsed (map-tool/read-edn (str "(" text ")"))]
    (when-not (and (seq? parsed) (= 1 (count parsed))) (fail "expected one EDN form"))
    (first parsed)))

(def duration-units {"ms" 0.001 "s" 1 "m" 60 "h" 3600 "d" 86400})

(defn duration [text]
  (let [[_ n unit] (re-matches #"(\d+(?:\.\d+)?)(ms|s|m|h|d)?" (or text ""))
        seconds (if n (* (js/Number n) (duration-units (or unit "s"))) js/NaN)]
    (when-not (and (js/Number.isFinite seconds) (pos? seconds) (<= seconds 31536000))
      (fail "--for must be a positive duration up to 365 days, e.g. 30s or 10m"))
    seconds))

(def option-spec
  (into {:state {:type "string"} :worlds {:type "string"} :world {:type "string"} :by {:type "string" :default "agent"}}
        (map (fn [k] [k {:type "string"}]))
        [:limit :offset :trigger :when :job :args :persistence :cooldown-s :for :backoff :before :after :property]))

(def put-options [:trigger :when :job :args :persistence :cooldown-s :for :backoff])
(def allowed-options
  {:list [:limit :offset] :show [] :add put-options :put put-options :remove [] :mute [:for] :unmute []
   :move [:before :after :for] :reset [:property] :upgrade []})
(def op-for {:add :put :put :put :unmute :clear :reset :clear})

(defn put-fields
  "The fields of an add or put request: one of --trigger or --when, the job, args, persistence, cooldown, backoff."
  [v]
  (when (= (some? (:trigger v)) (some? (:when v))) (fail "give exactly one of --trigger or --when"))
  (let [ad-hoc? (some? (:when v))
        condition (when ad-hoc? (one-form (:when v)))
        _ (when (and ad-hoc? (nil? (:job v))) (fail "--when needs --job"))
        job (when (some? (:job v)) (one-form (:job v)))
        _ (when (and (some? job) (not (and (seq? job) (symbol? (first job)))))
            (fail "--job must be a native EDN job list"))
        args (when (some? (:args v))
               (when ad-hoc? (fail "ad hoc conditions use job args, not --args"))
               (let [form (one-form (:args v))]
                 (when-not (map? form) (fail "--args must be an EDN map"))
                 form))
        persistence (when (some? (:persistence v))
                      (when-not (#{"stop" "retry" "cooldown"} (:persistence v)) (fail "invalid persistence"))
                      (keyword (:persistence v)))
        cooldown (when (some? (:cooldown-s v))
                   (let [n (js/Number (:cooldown-s v))]
                     (when-not (and (not (str/blank? (:cooldown-s v))) (js/Number.isFinite n) (>= n 0)) (fail "cooldown must be nonnegative"))
                     n))
        backoff (when (some? (:backoff v)) (one-form (:backoff v)))]
    (cond-> {}
      (some? (:trigger v)) (assoc :trigger (identifier (:trigger v)))
      ad-hoc? (assoc :when condition)
      (some? job) (assoc :job job)
      (some? args) (assoc :args args)
      (some? persistence) (assoc :persistence persistence)
      (some? cooldown) (assoc :cooldown-s cooldown)
      (some? backoff) (assoc :backoff backoff))))

(defn move-fields [v id]
  (when (= (some? (:before v)) (some? (:after v))) (fail "move needs exactly one of --before or --after"))
  (let [anchor (identifier (or (:before v) (:after v)))]
    (when (= anchor id) (fail "cannot move a trigger relative to itself"))
    {(if (some? (:before v)) :above :below) anchor}))

(defn mutation [command v id]
  (let [request (cond-> {:op (op-for command command) :id id :by (:by v)}
                  (some? (:for v)) (assoc :ttl-s (duration (:for v))))]
    (case command
      (:add :put) (merge request (put-fields v))
      :move (merge request (move-fields v id))
      :unmute (assoc request :property :mute)
      :reset (do (when-not (#{"position" "mute"} (:property v)) (fail "reset needs --property position or mute"))
                 (assoc request :property (keyword (:property v))))
      request)))

(defn list-request [base v]
  (let [limit (js/Number (or (:limit v) 8)) offset (js/Number (or (:offset v) 0))]
    (when-not (and (js/Number.isInteger limit) (<= 1 limit 32) (js/Number.isInteger offset) (<= 0 offset 10000))
      (fail "limit 1..32 and offset 0..10000 required"))
    (assoc base :path "/triggers" :limit limit :offset offset :mutating false)))

(defn request-for-unsafe [argv]
  (let [{:keys [positionals values]} (map-tool/parse-options (vec argv) option-spec)
        v values
        [body command-name id-text & extra] positionals
        command (keyword (or command-name "list"))]
    (when-not (and body (re-matches bodies/name-re body)) (fail "body must be a valid name"))
    (when (nil? (:world v)) (fail (bodies/missing-world-error "--world")))
    (when-not (re-matches bodies/name-re (:world v)) (fail "world must be a valid name"))
    (when-not (contains? allowed-options command) (fail "unknown command"))
    (when (or (seq extra) (if (#{:list :upgrade} command) (some? id-text) (nil? id-text)))
      (fail (str (name command) (if (#{:list :upgrade} command) " takes no ID" " needs exactly one ID"))))
    (doseq [k (keys v)]
      (when-not (or (#{:state :worlds :world :by} k) (some #{k} (allowed-options command)))
        (fail (str "--" (name k) " is not valid for " (name command)))))
    (when (or (str/blank? (:by v)) (> (count (:by v)) 40)) (fail "--by must be 1..40 characters"))
    (let [state (bodies/storage-root v map-tool/default-state-dir)
          socket-path (.join path (bodies/body-dir state (:world v) body) "engine" "events.sock")
          base {:body body :world (:world v) :state state :socketPath socket-path :command command}]
      (case command
        :list (list-request base v)
        :upgrade (assoc base :path "/triggers" :id :upgrade :mutating true :request {:op :upgrade :by (:by v)})
        (let [id (identifier id-text)]
          (if (= command :show)
            (assoc base :path (str "/triggers?id=" (js/encodeURIComponent (name id))) :id id :mutating false)
            (let [request (mutation command v id)]
              (when (> (.byteLength js/Buffer (data/write-edn request)) 15000)
                (fail "combined request exceeds 15000 bytes"))
              (assoc base :path "/triggers" :id id :mutating true :request request))))))))

(defn request-for [argv]
  (try (request-for-unsafe argv) (catch :default error {:error (.-message error)})))

;; Bounded projection. Budgets and depth limits keep any answer small however the register grows.

(defn ordered-map
  "A map from key/value pairs that prints in the given order."
  [pairs]
  (with-meta (into {} pairs) {:json-keys (mapv first pairs)}))

(defn entries-of [m]
  (if-let [ks (:json-keys (meta m))]
    (map (fn [k] [k (get m k)]) (filter #(contains? m %) ks))
    (seq m)))

(defn bounded
  ([value] (bounded value (volatile! 128) 0))
  ([value budget depth]
   (vswap! budget dec)
   (let [deeper #(bounded % budget (inc depth))]
     (cond
       (or (neg? @budget) (> depth 6)) :truncated
       (string? value) (subs value 0 (min 240 (count value)))
       (vector? value) (mapv deeper (take 16 value))
       (or (keyword? value) (symbol? value)) value
       (seq? value) (apply list (mapv deeper (take 16 value)))
       (map? value) (ordered-map (mapv (fn [[k v]] [k (deeper v)]) (take 16 (entries-of value))))
       :else value))))

(defn list-item [ranks e]
  (ordered-map
   (cond-> (cond-> [] (some? (:id e)) (conj [:id (:id e)]) (some? (:trigger e)) (conj [:trigger (:trigger e)]))
     (some? (:when e)) (conj [:when (bounded (:when e))])
     (contains? ranks (:id e)) (conj [:priority (get ranks (:id e))])
     (and (seq? (:job e)) (symbol? (first (:job e)))) (conj [:job (str (first (:job e)))])
     (:builtin? e) (conj [:builtin? true])
     (:muted e) (conj [:muted true])
     (:stopped? e) (conj [:stopped? true])
     (:cooling-until e) (conj [:cooling true])
     (:backing-off e) (conj [:backing-off true]))))

(defn compact-list [{:keys [offset limit]} value]
  (let [all (vec (:items value))
        selected (vec (take limit (drop offset all)))
        ranks (into {} (map-indexed (fn [index id] [id (inc index)])) (:order value))
        next-offset (+ offset (count selected))]
    (ordered-map
     (cond-> [[:total (or (:total value) (count all))]
              [:items (mapv #(list-item ranks %) selected)]]
       (< next-offset (count all)) (conj [:next-offset next-offset])))))

(defn compact-show [{:keys [id]} value]
  (if-let [entry (first (filter #(= id (:id %)) (:items value)))]
    (let [result (bounded entry)]
      (if (some? (get-in value [:explain :terms]))
        (assoc result :explain (bounded (get-in value [:explain :terms])))
        result))
    {:ok false :reason :trigger-not-found :id id}))

(defn compact-mutation [value]
  (ordered-map
   (cond-> [[:ok true] [:id (:id value)] [:op (:op value)]]
     (some? (:created? value)) (conj [:created? (:created? value)])
     (:muted (:trigger value)) (conj [:muted true])
     (:moved (:trigger value)) (conj [:moved (bounded (:moved (:trigger value)))]))))

(defn compact [r value]
  (cond
    (false? (:ok value)) (bounded value)
    (= :list (:command r)) (compact-list r value)
    (= :show (:command r)) (compact-show r value)
    (= :upgrade (:command r)) (ordered-map [[:ok true] [:op :upgrade] [:added (:added value)]])
    :else (compact-mutation value)))

;; Transport

(defn socket-options [{:keys [timeout-ms request-fn]}]
  (cond-> {:timeout-ms (or timeout-ms request-timeout-ms) :max-bytes max-response-bytes :label "triggers"}
    request-fn (assoc :request-fn request-fn)))

(defn get! [socket-path path opts]
  (http/request (assoc (socket-options opts) :socket-path socket-path :path path)))

(defn post!
  "POST an EDN body to /triggers: a promise of {:status :content-type :text}; options :timeout-ms and :request-fn."
  [socket-path body opts]
  (http/request (assoc (socket-options opts) :socket-path socket-path :method "POST" :path "/triggers"
                       :headers {"content-type" "application/edn"} :body (data/write-edn body))))

(def unavailable {:ok false :reason :triggers-unavailable :action :restart-with-current-build})

(defn read-value [text]
  (let [value (data/read-edn text)]
    (when-not (map? value) (fail "bad response"))
    value))

(defn exchange!
  "A promise of the engine's response, or ::unavailable when the build has no trigger route. sent is set once the
  mutation request has been handed to the socket."
  [r opts sent]
  (if-not (:mutating r)
    (get! (:socketPath r) (:path r) opts)
    (.then (get! (:socketPath r) "/triggers" opts)
           (fn [current]
             (when-not (http/unsupported-route? current)
               (when-not (and (= 200 (:status current)) (http/edn-response? (:content-type current)))
                 (fail "trigger API unavailable"))
               (let [generation (:generation-id (read-value (:text current)))]
                 (when-not (string? generation) (fail "no generation"))
                 (vreset! sent true)
                 (post! (:socketPath r) (assoc (:request r) :generation-id generation) opts)))))))

(defn failure-for [error sent?]
  (let [code (aget error "code")]
    (cond-> {:ok false
             :reason (keyword (cond (= "ERESPONSETOOLARGE" code) "response-too-large"
                                    (#{"ENOENT" "ECONNREFUSED"} code) "no-running-body"
                                    :else "transport-error"))}
      sent? (assoc :confirmation :unknown
                   :message "Inspect trigger show/list before retrying; mutations are not automatically retried."))))

(defn print-text! [text] (.write (.-stdout js/process) text))

(defn deliver!
  "Print the projection of a response and return the exit code."
  [r output response]
  (let [print! #(output (str (data/write-edn %) "\n"))]
    (if (nil? response)
      (do (print! unavailable) 2)
      (do (when-not (http/edn-response? (:content-type response)) (fail "bad content type"))
          (let [value (read-value (:text response))]
            (if (and (= 404 (:status response)) (= :not-found (:reason value)))
              (do (print! unavailable) 2)
              (let [projected (compact r value)]
                (print! projected)
                (if (and (= 200 (:status response)) (not (false? (:ok projected)))) 0 1))))))))

(defn main!
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv] (main! argv {}))
  ([argv {:keys [output] :or {output print-text!} :as opts}]
   (let [r (request-for argv)
         sent (volatile! false)]
     (if (:error r)
       (do (js/console.error (str (:error r) "\n" usage)) (js/Promise.resolve 2))
       (-> (exchange! r opts sent)
           (.then (fn [response] (deliver! r output response)))
           (.catch (fn [error]
                     (output (str (data/write-edn (failure-for error @sent)) "\n"))
                     2)))))))
