(ns engine.event-api
  "The engine's private HTTP API on a local Unix socket (mode 0600). The wire format is EDN.
  GET   /snapshot, /status, /inventory, /events, /job, /jobs, /triggers, /catalog
  POST  /jobs and /triggers (changes), /attention/resolve, /chat
  Bodies are limited to 16 KB. Lists are paged and bounded."
  (:require [cljs.reader :as reader]
            [engine.core :as core]
            [engine.chat :as chat]
            [engine.expr :as expr]
            [engine.events :as events]
            [engine.job-api :as job-api]
            [engine.memory :as mem]
            [engine.trigger-api :as trigger-api]
            ["fs" :as fs]
            ["http" :as http]
            ["net" :as net]
            ["path" :as path]
            [engine.game :as game]))

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

(defn engine-failed!
  "Log the failure e (message and stack) as an :error event, then answer 500 :engine-error
  for a failure of the engine itself (not of the request), unless the answer has begun.
  A failing log goes to stderr: the answer is still sent."
  [eng res e]
  (try
    (events/emit! (:events eng) {:source :system :kind :engine-error :level :error
                                 :text (str "event api: " (or (some-> e .-message) e))
                                 :stack (some-> e .-stack str)})
    (catch :default e2
      (.write js/process.stderr (str "event api failure: " e " (and the log failed: " e2 ")\n"))))
  (when-not (.-headersSent res) (bad! res 500 :engine-error)))

(defn parse-edn [text]
  (try {:value (reader/read-string text)}
       (catch :default _ {:error :bad-edn})))

(defn number-param [params key fallback maximum]
  (let [text (.get params key)]
    (if (nil? text)
      fallback
      (let [n (js/Number text)]
        (when (and (js/Number.isSafeInteger n) (<= 0 n maximum)) n)))))

(defn snapshot
  "The whole persisted engine state with the body's position, offline/settling flags and the event cursor."
  [eng]
  (let [p (:primitives eng)]
    {:body (.-username (.self p))
     :generation-id (:generation-id (core/state eng))
     :state (core/state eng)
     :outstanding (core/outstanding eng)
     :position (core/self-pos p)
     :offline (core/offline? eng)
     :away (core/away eng)
     :settling (core/settling? eng)
     :cursor (events/cursor (:events eng))}))

(def status-job-limit 4)
(def attention-limit 4)
(def catalog-page-limit 20)
(def max-catalog-page-limit 64)
(def catalog-doc-limit 1200)
(def inventory-stack-limit 46)
(def armour-slots #{:head :torso :legs :feet})
(def equipment-slots [:head :torso :legs :feet :offHand :mainHand])
(def enchant-limit 8)

(defn short-text [x n]
  (when (string? x) (subs x 0 (min n (count x)))))

(defn inventory-stack [stack]
  (when (and (map? stack) (string? (:name stack))
             (integer? (:count stack)) (pos? (:count stack)))
    (cond-> {:name (short-text (:name stack) 80) :count (:count stack)}
      (and (integer? (:slot stack)) (<= 0 (:slot stack) 45)) (assoc :slot (:slot stack)))))

(defn enchant-entry [e]
  (when (and (map? e) (string? (:name e)) (integer? (:level e)))
    {:name (short-text (:name e) 40) :level (:level e)}))

(defn equipment-item [item]
  (when (and (map? item) (string? (:name item)))
    (cond-> {:name (short-text (:name item) 80)}
      (seq (:enchants item)) (assoc :enchants (->> (:enchants item) (keep enchant-entry) (take enchant-limit) vec))
      (and (integer? (:count item)) (pos? (:count item))) (assoc :count (:count item))
      (and (number? (:durability item)) (not (neg? (:durability item)))) (assoc :durability (:durability item)))))

(defn last-known
  "What the body last read before it went offline (position, health, food, inventory, equipment), or nil."
  [p]
  (when (fn? (.-lastKnown p))
    (some-> (.lastKnown p) (js->clj :keywordize-keys true))))

(defn inventory-view
  "Read-only player inventory and worn slots, bounded to the vanilla player-window capacity."
  [eng]
  (let [offline? (core/offline? eng)
        known (when offline? (last-known (:primitives eng)))]
    (if (and offline? (nil? known))
      {:ok false :reason :offline :offline (core/away eng)}
      (let [self (or known (js->clj (.self (:primitives eng)) :keywordize-keys true))
          all-stacks (or (:inventory self) [])
          stacks (->> all-stacks (take inventory-stack-limit) (keep inventory-stack) vec)
          equipment (into {} (keep (fn [slot]
                                     (if-let [item (equipment-item (get-in self [:equipment slot]))]
                                       [slot item]
                                       (when (armour-slots slot) [slot :empty])))) equipment-slots)]
      (cond-> {:ok true :inventory stacks}
        (seq equipment) (assoc :equipment equipment)
        (> (count all-stacks) inventory-stack-limit) (assoc :more? true)
        offline? (assoc :last-known true :offline (core/away eng)))))))

(defn bounded-value
  "A small EDN-safe copy of user-supplied job args or specs: depth at most 4, strings cut to 160
  characters, at most budget nodes in all. Cut parts read :truncated."
  [value depth budget]
  (cond
    (zero? @budget) :truncated
    (> depth 4) :truncated
    (or (nil? value) (string? value) (keyword? value) (symbol? value)
        (number? value) (boolean? value))
    (do (vswap! budget dec)
        (if (string? value) (short-text value 160) value))
    (map? value)
    (do (vswap! budget dec)
        (loop [entries (seq value) out {}]
          (if (or (empty? entries) (zero? @budget))
            (cond-> out (seq entries) (assoc :truncated true))
            (let [[k v] (first entries)]
              (recur (next entries)
                     (assoc out
                            (bounded-value k (inc depth) budget)
                            (bounded-value v (inc depth) budget)))))))
    (or (sequential? value) (set? value))
    (do (vswap! budget dec)
        (loop [items (seq value) out []]
          (if (or (empty? items) (zero? @budget))
            (cond-> out (seq items) (conj :truncated))
            (recur (next items) (conj out (bounded-value (first items) (inc depth) budget))))))
    :else (do (vswap! budget dec) (short-text (pr-str value) 160))))

(defn instance-status
  "A job's status; running-id is the instance whose round is in flight now (the state's :current can lag it)."
  ([s id] (instance-status s id nil))
  ([s id running-id]
  (cond
    (contains? (:failed s) id) :failed
    (or (= id (:current s)) (= id running-id)) :running
    (= id (:resume s)) :resuming
    (some #{id} (:list s)) :queued
    :else :unknown)))

(defn job-summary
  "A queue row; waiting is why the job waits (its check's reason), or nil."
  [s id waiting running-id]
  (when-let [inst (get-in s [:instances id])]
    (cond-> {:id id
             :name (short-text (expr/label (:spec inst)) 160)
             :round (:round inst)
             :status (instance-status s id running-id)
             :hold? (boolean (:hold? inst))
             :reflex (:reflex inst)}
      waiting (assoc :waiting (bounded-value waiting 0 (volatile! 32))))))

(defn attention-summary [[request-id request]]
  (let [event (:event request)]
    {:request-id request-id
     :job-id (:job-id request)
     :reason (:reason request)
     :kind (:kind event)
     :message (short-text (:message event) 240)
     :updated-at (:updated-at request)}))

(defn death-summary
  "What status reports of a :died memory entry at now: {:pos :cause? :ago-ms :despawns-in-ms :recovered?}
  while the drops can still be there (under the five minute despawn window), else nil. :recovered is the decision
  of a :recovered entry newer than the death (:collected, :partial, :skip, :abandoned), when there is one."
  ([entry now] (death-summary entry now nil))
  ([entry now recovered]
   (when entry
     (let [ago (- now (:t entry))
           {:keys [pos cause]} (:data entry)
           decision (when (and recovered (> (:t recovered) (:t entry))) (:decision (:data recovered)))]
       (when (< ago game/despawn-ms)
         (cond-> {:pos pos :ago-ms ago :despawns-in-ms (- game/despawn-ms ago)}
           cause (assoc :cause cause)
           decision (assoc :recovered decision)))))))

(defn status
  "The compact status view: mode, position, health, food, the current job, up to limit queued jobs,
  failed jobs, outstanding attention requests and the event cursor."
  [eng requested-limit]
  (let [s (core/state eng)
        p (:primitives eng)
        self (.self p)
        known (when (core/offline? eng) (last-known p))
        current (or (core/holder eng)
                    (when-let [id (:resume s)] {:id id}))
        current-id (:id current)
        manual @(:manual eng)
        limit (or requested-limit status-job-limit)
        queue-count (count (:list s))
        queue (mapv #(job-summary s % (core/waiting eng %) (:id (core/running eng))) (take limit (:list s)))
        attention (->> (:attention s)
                       (sort-by (fn [[id req]] [(- (or (:updated-at req) 0)) id]))
                       (take attention-limit)
                       (mapv attention-summary))]
    {:body (.-username self)
     :generation-id (:generation-id s)
     :cursor (events/cursor (:events eng))
     :mode (cond (core/manual? eng) :manual
                 (core/offline? eng) :offline
                 (core/settling? eng) :settling
                 :else :scheduled)
     :offline (core/away eng)
     :manual (when manual
               (cond-> (select-keys manual [:who :why :since])
                 (:who manual) (update :who #(short-text (str %) 80))
                 (string? (:why manual)) (update :why #(short-text % 160))))
     :position (or (core/self-pos p) (some-> known :pos (select-keys [:x :y :z])))
     :last-known (when known true)
     :died (let [view (mem/view (:store eng))]
             (death-summary (mem/latest view :died) (:now view) (mem/latest view :recovered)))
     :health (let [h (if known (:health known) (.-health self))] (when (number? h) h))
     :food (let [f (if known (:food known) (.-food self))] (when (number? f) f))
     :current (when current
                (when-let [summary (job-summary s current-id nil (:id (core/running eng)))]
                  (assoc summary
                         :status (cond (core/running eng) :running
                                       (:reflex current) :reflex
                                       (= current-id (:resume s)) :resuming
                                       :else (instance-status s current-id))
                         :reflex (:reflex current))))
     :jobs {:total queue-count
            :items (->> queue (remove nil?) vec)
            :more? (> queue-count limit)}
     :failed (let [failed (->> (:failed s)
                               (sort-by key)
                               (mapv (fn [[id failure]]
                                       {:id id :error (short-text (:error failure) 240)})))]
               {:total (count failed) :items (->> failed (take attention-limit) vec)
                :more? (> (count failed) attention-limit)})
     :outstanding {:total (count (:attention s)) :items attention
                   :more? (> (count (:attention s)) attention-limit)}}))

(defn job-detail
  "One listed job with its bounded spec and args, status, wait reason, failure and attention requests; nil when unknown."
  [eng id limit]
  (let [s (core/state eng)]
    (when-let [inst (get-in s [:instances id])]
      (let [[_ args] (core/job-of eng inst)
            failure (get-in s [:failed id])
            requests (->> (:attention s)
                          (keep (fn [[request-id request]]
                                  (when (= id (:job-id request)) (attention-summary [request-id request]))))
                          (sort-by (juxt :updated-at :request-id))
                          vec)]
        (let [budget (volatile! 64)]
        {:ok true :generation-id (:generation-id s) :id id :name (short-text (expr/label (:spec inst)) 160)
         :spec (bounded-value (:spec inst) 0 budget)
         :args (bounded-value args 0 budget) :bounded? true :round (:round inst) :hold? (boolean (:hold? inst))
         :reflex (:reflex inst) :status (instance-status s id (:id (core/running eng)))
         :waiting (some-> (core/waiting eng id) (bounded-value 0 (volatile! 32)))
         :current? (= id (:id (core/holder eng)))
         :failure (when failure {:error (short-text (:error failure) 1000) :at (:t failure)})
         :attention {:total (count requests) :items (->> requests (take limit) vec)
                     :more? (> (count requests) limit)}})))))

(defn catalog-entry [eng kind name]
  (case kind
    "job" (let [sym (symbol name)
                entry (get (:jobs eng) sym)]
            (when entry
              {:ok true :kind :job :name sym
               :doc (short-text (:doc entry) catalog-doc-limit)
               :args (bounded-value (:args entry) 0 (volatile! 64))}))
    "trigger" (let [id (keyword name)
                    entry (get (:triggers eng) id)]
                (when entry
                  {:ok true :kind :trigger :name id
                   :job (:job entry) :args (bounded-value (:args entry) 0 (volatile! 64))
                   :persistence (:persistence entry) :cooldown-s (:cooldown-s entry)}))
    nil))

(defn catalog-list [eng kind prefix offset limit]
  (let [name-of (fn [x] (if (keyword? x) (name x) (str x)))
        names (case kind
                "jobs" (sort-by name-of (keys (:jobs eng)))
                "triggers" (sort-by name-of (keys (:triggers eng)))
                nil)]
    (when names
      (let [matches (filter #(or (empty? prefix) (.startsWith (name-of %) prefix)) names)
            total (count matches)
            items (->> matches (drop offset) (take limit) (mapv name-of))
            next-offset (when (< (+ offset (count items)) total) (+ offset (count items)))]
        {:ok true :kind (keyword kind) :prefix prefix :offset offset
         :items items :total total :next-offset next-offset}))))

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

(defn create
  "The API server for eng on socket-path: {:listen fn :close fn}, each returning a promise.
  Refuses to listen when a live process already holds the socket."
  [socket-path eng]
  (let [listening? (atom false)
        server (http/createServer
                (fn [req res]
                  (try
                    (let [url (js/URL. (.-url req) "http://engine")
                          pathname (.-pathname url)
                          method (.-method req)]
                      (cond
                        (and (= method "GET") (= pathname "/jobs"))
                        (let [params (.-searchParams url)
                              offset (number-param params "offset" 0 10000)
                              limit (number-param params "limit" 8 32)]
                          (if (and offset limit (pos? limit))
                            (respond! res 200 (job-api/list-jobs eng offset limit))
                            (bad! res 400 :bad-query)))

                        (and (= method "GET") (= pathname "/triggers"))
                        (respond! res 200 (trigger-api/triggers-view eng (.get (.-searchParams url) "id")))

                        (and (= method "POST") (contains? #{"/jobs" "/triggers"} pathname))
                        (if-not (.startsWith (or (aget (.-headers req) "content-type") "") "application/edn")
                          (bad! res 415 :content-type-must-be-application-edn)
                          (-> (read-body req)
                              (.then (fn [text]
                                       (let [{:keys [value error]} (parse-edn text)]
                                         (if error (bad! res 400 error)
                                           (try
                                             (let [result ((if (= pathname "/jobs") job-api/mutate! trigger-api/request!) eng value)]
                                               (respond! res (if (:ok result) 200 409) result))
                                             (catch :default e (engine-failed! eng res e)))))))
                              (.catch (fn [e] (bad! res (if (= "body too large" (.-message e)) 413 400)
                                                       (if (= "body too large" (.-message e)) :too-large :bad-request))))))

                        (and (= method "GET") (= pathname "/snapshot"))
                        (respond! res 200 (snapshot eng))

                        (and (= method "GET") (= pathname "/inventory"))
                        (let [view (inventory-view eng)]
                          (respond! res (if (:ok view) 200 503) view))

                        (and (= method "GET") (= pathname "/status"))
                        (let [limit (number-param (.-searchParams url) "limit" status-job-limit 32)]
                          (if (and limit (pos? limit))
                            (respond! res 200 (status eng limit))
                            (bad! res 400 :bad-query)))

                        (and (= method "GET") (= pathname "/job"))
                        (let [params (.-searchParams url)
                              id (.get params "id")
                              limit (number-param params "limit" attention-limit 32)]
                          (if (and id (re-matches #"j[0-9]+" id) limit (pos? limit))
                            (if-let [detail (job-detail eng id limit)]
                              (respond! res 200 detail)
                              (bad! res 404 :job-not-found))
                            (bad! res 400 :bad-query)))

                        (and (= method "GET") (= pathname "/catalog"))
                        (let [params (.-searchParams url)
                              kind (.get params "kind")
                              name (.get params "name")
                              prefix (or (.get params "prefix") "")
                              limit (number-param params "limit" catalog-page-limit max-catalog-page-limit)
                              offset (number-param params "offset" 0 10000)
                              exact? (contains? #{"job" "trigger"} kind)
                              list? (contains? #{"jobs" "triggers"} kind)
                              valid-name? (and (string? name)
                                               (<= (count name) 120)
                                               (case kind
                                                 "job" (boolean (re-matches #"jobs(?:\.[a-z][a-z0-9-]*)+" name))
                                                 "trigger" (boolean (re-matches #"[a-z][a-z0-9-]*" name))
                                                 false))
                              valid-prefix? (and (string? prefix)
                                                 (<= (count prefix) 120)
                                                 (case kind
                                                   "jobs" (or (empty? prefix)
                                                               (boolean (re-matches #"jobs(?:\.[a-z][a-z0-9-]*)*(?:\.)?" prefix)))
                                                   "triggers" (or (empty? prefix)
                                                                   (boolean (re-matches #"[a-z][a-z0-9-]*" prefix)))
                                                   false))]
                          (if (and (or (and exact? valid-name?)
                                       (and list? valid-prefix? limit (pos? limit) offset))
                                   (or (not exact?) (nil? (.get params "prefix"))))
                            (if exact?
                              (if-let [entry (catalog-entry eng kind name)]
                                (respond! res 200 entry)
                                (bad! res 404 :capability-not-found))
                              (respond! res 200 (catalog-list eng kind prefix offset limit)))
                            (bad! res 400 :bad-query)))

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
                                     (try
                                       (let [result (core/resolve-attention! eng (:request-id value) (:reason value))]
                                         (respond! res 200 {:ok true :request-id (:request-id value)
                                                            :resolved (= result :resolved)
                                                            :already-resolved (= result :already-resolved)}))
                                       (catch :default e (engine-failed! eng res e)))))))
                              (.catch (fn [e]
                                        (bad! res (if (= "body too large" (.-message e)) 413 400)
                                              (if (= "body too large" (.-message e)) :too-large :bad-edn))))))

                        (and (= method "POST") (= pathname "/chat"))
                        (if-not (and (aget (.-headers req) "content-type")
                                     (.startsWith (aget (.-headers req) "content-type") "application/edn"))
                          (bad! res 415 :content-type-must-be-application-edn)
                          (-> (read-body req)
                              (.then
                               (fn [text]
                                 (let [{:keys [value error]} (parse-edn text)
                                       message (:message value)
                                       to (:to value)
                                       to-valid? (or (nil? to)
                                                     (and (string? to) (re-matches #"[A-Za-z0-9_]{3,16}" to)))]
                                   (cond
                                     error (bad! res 400 error)
                                     (not (and (map? value) (string? message) (<= 1 (count message) 256) to-valid?))
                                     (bad! res 400 :bad-request)
                                     :else
                                     (-> (js/Promise.resolve) ; a sync throw of direct! is an engine failure too
                                         (.then #(chat/direct! eng message to))
                                         (.then (fn [result]
                                                  (respond! res 200 {:ok true :body (.-username (.self (:primitives eng)))
                                                                     :result (if (map? result) result
                                                                               (js->clj result :keywordize-keys true))}))
                                                (fn [e] (engine-failed! eng res e))))))))
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
