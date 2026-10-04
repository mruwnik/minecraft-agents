(ns dashboard.server
  "The dashboard server: plain node http on 127.0.0.1. Engine bodies are read from their local event service,
  with an events.jsonl/engine.edn fallback for legacy body directories."
  (:require ["fs" :as fs]
            ["http" :as http]
            ["path" :as path]
            ["url" :as url]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [dashboard.edn :as edn]
            [dashboard.chat :as chat]
            [dashboard.engine-edn :as engine-edn]
            [dashboard.chat-send :as chat-send]
            [dashboard.engine-events :as ee]
            [dashboard.entity-observations :as entities]
            [dashboard.guard :as guard]
            [dashboard.restart :as restart]
            [dashboard.items :as items]
            [dashboard.jobs-registry :as jobs-registry]
            [dashboard.blueprint-library :as blueprint-lib]
            [dashboard.village-data :as village-data]
            [dashboard.plan-api :as plan-api]
            [dashboard.rcon :as rcon]
            [dashboard.routes :as routes]
            [dashboard.thumbs :as thumbs]
            [dashboard.tiles :as tiles]
            [dashboard.view-info :as view-info]
            [dashboard.worlds :as worlds]
            [dashboard.shared-map :as shared-map]
            [engine.bodies :as bodies]))

(def repo-root (.resolve path js/__dirname ".." ".."))
(def root (or (.-DASHBOARD_ROOT js/process.env) repo-root))
(def dashboard-dir (.join path repo-root "dashboard"))
(def public-dir (.join path dashboard-dir "public"))
(def js-dir (.join path dashboard-dir "out" "public" "js"))
(def state-dir (.join path root "state"))
(def worlds-dir (.join path root "state" "worlds"))

(def first-read-bytes (* 4 1024 1024)) ; ~10 minutes of debug-heavy engine events
(def chat-tail-bytes (* 4 1024 1024))
(def read-chunk-bytes (* 256 1024)) ; a big first read is processed this much at a time
(def chat-keep 2000)
(def max-preview-bytes (* 2 1024 1024))

;; ---------------------------------------------------------------- files
(defn file-exists? [file] (.existsSync fs file))

(defn read-text [file]
  (try (.readFileSync fs file "utf8") (catch :default _ "")))

(defn read-json [file fallback]
  (let [parsed (worlds/parse-json (read-text file))]
    (if (nil? parsed) fallback parsed)))

(defn dir-names [dir]
  (try
    (->> (.readdirSync fs dir #js {:withFileTypes true})
         (filter #(.isDirectory %))
         (map #(.-name %))
         sort
         vec)
    (catch :default _ [])))

(defn read-range [file start end]
  (let [fd (.openSync fs file "r")]
    (try
      (let [buf (js/Buffer.alloc (- end start))]
        (.readSync fs fd buf 0 (.-length buf) start)
        buf)
      (finally (.closeSync fs fd)))))

;; Reads file[from, size) read-chunk-bytes at a time, calling (f acc text) with the whole lines of each chunk.
;; skip-head? is true when from is mid-file (the first line is torn). Returns {:acc :rest}: rest is a line
;; still being written. No more than one chunk of the range is in memory at once.
(defn reduce-lines [file from size skip-head? rest f init]
  (loop [pos from
         split {:rest rest :skipping? skip-head?}
         acc init]
    (if (>= pos size)
      {:acc acc :rest (:rest split)}
      (let [end (min size (+ pos read-chunk-bytes))
            next-split (ee/split-chunk split (read-range file pos end))]
        (recur end next-split (f acc (ee/decode-bytes (:complete next-split))))))))

;; ---------------------------------------------------------------- worlds
(defn world-names []
  (filterv #(file-exists? (.join path worlds-dir % "world.json")) (dir-names worlds-dir)))

(defn read-world-list []
  (worlds/parse-world-list
   (mapv (fn [n] {:name n :text (read-text (.join path worlds-dir n "world.json"))}) (world-names))))

(defn read-worlds []
  (mapv (fn [n]
          (let [dir (.join path worlds-dir n)
                overlays (shared-map/read-overlays dir (read-json (.join path dir "zones.json") []))]
            {:name n
             :places (worlds/readable-places (read-json (.join path dir "places.json") []))
             :zones (:zones overlays)
             :map-errors (:errors overlays)
             :clock (read-json (.join path dir "clock.json") nil)}))
        (world-names)))

;; the name only ever comes out of the directory listing, never from the query
(defn choose-world [query]
  (worlds/world-choice (world-names) (.get query "world")))

;; ---------------------------------------------------------------- engine bodies
;; A body is addressed by {:world :name} (a name is unique only within a world); agent entries carry both, and every
;; cache below is keyed by (body-key body).
(defn body-key [body] (select-keys body [:world :name]))
(defn body-dir [{:keys [world name]}] (bodies/body-dir state-dir world name))

;; Per body: how far into events.jsonl we have read, the folded state, and the bytes of a line still being written.
(def engines (atom {}))
(def edn-cache (atom {}))
(def tails (atom {}))

(defn engine-dir [body] (.join path (body-dir body) "engine"))
(defn events-file [body] (.join path (engine-dir body) "events.jsonl"))
(defn canonical-events-file [body] (.join path (engine-dir body) "events.edn"))
(defn events-socket [body] (.join path (engine-dir body) "events.sock"))
(defn engine-folder? [body]
  (or (file-exists? (.join path (engine-dir body) "engine.edn"))
      (file-exists? (canonical-events-file body))
      (file-exists? (events-socket body))
      (file-exists? (events-file body))))
(defn canonical-engine? [body]
  (or (file-exists? (canonical-events-file body)) (file-exists? (events-socket body))))

;; The engine owns event ordering, retention and the durable attention inbox. Dashboard caches only the last
;; serialized snapshot and its fold; every request refreshes from the socket before exposing live state.
(def live-engines (atom {}))
(def live-refreshing (atom {}))
(def live-errors (atom {}))
(def event-page-size 1000)
(def event-socket-timeout-ms 1500)
(declare agent-entries)

(defn edn-response [text]
  (try (reader/read-string text) (catch :default e (throw (js/Error. (str "bad EDN from engine: " (ex-message e)))))))

(defn event-socket-request! [target method request-path body]
  (js/Promise.
   (fn [resolve reject]
     (let [text (when (some? body) (pr-str body))
           headers (when text #js {"content-type" "application/edn" "content-length" (js/Buffer.byteLength text)})
           options #js {:socketPath (events-socket target) :method method :path request-path :headers (or headers #js {})}
           req (.request
                http options
                (fn [res]
                  (let [chunks (atom [])]
                    (.on res "data" #(swap! chunks conj %))
                    (.on res "end"
                         (fn []
                           (let [status (.-statusCode res)
                                 response-text (.toString (js/Buffer.concat (to-array @chunks)) "utf8")]
                             (if (and (>= status 200) (< status 300))
                               (try (resolve (edn-response response-text)) (catch :default e (reject e)))
                               (reject (js/Error. (str "engine event API HTTP " status))))))))))]
       (.on req "error" reject)
       (.setTimeout req event-socket-timeout-ms #(.destroy req (js/Error. "engine event API timed out")))
       (.end req text)))))

(defn event-page! [body stream-id after limit]
  (event-socket-request! body "GET"
                         (str "/events?stream-id=" (js/encodeURIComponent stream-id)
                              "&after=" after "&limit=" limit)
                         nil))

(defn state-cursor [snapshot]
  (let [cursor (:cursor snapshot)]
    {:stream-id (:stream-id cursor) :seq (or (:seq cursor) 0)}))

(defn read-live-tail! [body snapshot after]
  (let [{:keys [stream-id seq]} (state-cursor snapshot)
        after (max 0 after)]
    (-> (event-page! body stream-id after event-page-size)
        (.then (fn [page]
                 (if-not (:gap? page)
                   {:page page :after after :snapshot snapshot :reset? false}
                   ;; Retention or a replaced stream: take a fresh snapshot, reconcile its inbox, then read a
                   ;; retained tail from the same stream rather than pretending the missing range was delivered.
                   (-> (event-socket-request! body "GET" "/snapshot" nil)
                       (.then (fn [fresh]
                                (let [{new-stream :stream-id head :seq} (state-cursor fresh)
                                      oldest (or (:oldest-seq page) 1)
                                      newest (or (:latest-seq page) head)
                                      tail-after (max (dec oldest) (- newest (dec event-page-size)))]
                                  (-> (event-page! body new-stream tail-after event-page-size)
                                      (.then (fn [tail] {:page tail :after tail-after :snapshot fresh :reset? true})))))))))))))

(defn advance-engine
  "Reads only what was appended since; a file that shrank starts over from its tail."
  [cached file]
  (let [size (.-size (.statSync fs file))
        fresh? (or (nil? cached) (< size (:offset cached)))
        from (if fresh? (max 0 (- size first-read-bytes)) (:offset cached))
        previous (if fresh? {:state ee/empty-engine :rest (js/Uint8Array. 0)} cached)]
    (if (and (not fresh?) (= size (:offset cached)))
      cached
      (let [{:keys [acc rest]} (reduce-lines file from size (and fresh? (pos? from)) (:rest previous) ee/fold-text (:state previous))]
        {:offset size :state acc :rest rest}))))

(defn read-engine [body]
  (let [k (body-key body)
        entry (advance-engine (get @engines k) (events-file body))]
    (swap! engines assoc k entry)
    (:state entry)))

(defn last-seq [events fallback]
  (or (:seq (peek (vec events))) fallback 0))

(defn refresh-live-engine! [body]
  (or (get @live-refreshing (body-key body))
      (let [name (body-key body)
            promise
            (-> (event-socket-request! body "GET" "/snapshot" nil)
                (.then (fn [snapshot]
                         (let [previous (get @live-engines name)
                               cursor (state-cursor snapshot)
                               same-stream? (= (:stream-id cursor) (get-in previous [:cursor :stream-id]))
                               same-generation? (= (:generation-id snapshot) (:generation-id previous))
                               local-reset? (not (and same-stream? same-generation?))
                               after (if local-reset?
                                       (max 0 (- (:seq cursor) (dec event-page-size)))
                                       (get-in previous [:cursor :seq] 0))]
                           (-> (read-live-tail! body snapshot after)
                               (.then (fn [{:keys [page snapshot] gap-reset? :reset?}]
                                        (let [events (:events page)
                                              actual-reset? (or local-reset? gap-reset?)
                                              folded (ee/fold-engine (if actual-reset? ee/empty-engine (:folded previous)) events)
                                              combined (if actual-reset? events (into (vec (:events previous)) events))
                                              page-seq (last-seq events after)
                                              next-cursor {:stream-id (:stream-id (state-cursor snapshot))
                                                           :seq page-seq}
                                              cache {:snapshot snapshot :generation-id (:generation-id snapshot) :cursor next-cursor :folded folded
                                                     :events (vec (take-last event-page-size combined))
                                                     :reset? actual-reset?}]
                                          (swap! live-errors dissoc name)
                                          (swap! live-engines assoc name cache)
                                          cache)))))))
                (.catch (fn [e]
                          (swap! live-errors assoc name (ex-message e))
                          (get @live-engines name)))
                (.finally (fn [] (swap! live-refreshing dissoc name))))]
        (swap! live-refreshing assoc name promise)
        promise)))

(defn refresh-live-engines! []
  (let [targets (->> (agent-entries) (map body-key) (filter canonical-engine?) vec)]
    (js/Promise.all (clj->js (map refresh-live-engine! targets)))))

;; engine.edn is re-read when its mtime or size changed
(defn read-edn-text [body]
  (let [file (.join path (engine-dir body) "engine.edn")
        k (body-key body)]
    (try
      (let [st (.statSync fs file)
            stamp [(.-mtimeMs st) (.-size st)]
            cached (get @edn-cache k)]
        (if (= stamp (:stamp cached))
          (:text cached)
          (let [text (.readFileSync fs file "utf8")]
            (swap! edn-cache assoc k {:stamp stamp :text text})
            text)))
      (catch :default _ nil))))

(defn edn-fields [body now]
  (let [summary (engine-edn/summarize (read-edn-text body) now)]
    (if (:error summary)
      {:edn-error (:error summary)}
      summary)))

;; ---------------------------------------------------------------- views (pose.json, hud.json)
;; Each file is re-read only when its mtime changed; a missing or unreadable file is nil.
(def view-cache (atom {}))

(defn view-file [body file] (.join path (body-dir body) "view" file))

(defn parse-js [text] (try (js/JSON.parse text) (catch :default _ nil)))

;; pose.json carries every nearby entity and changes often: only the few fields we show are converted
(defn pose-fields [o]
  (when o
    {:t (.-t o) :status (.-status o) :dimension (.-dimension o) :pos (js->clj (.-pos o) :keywordize-keys true)
     :villagers (view-info/villagers (js->clj (.-entities o) :keywordize-keys true))}))

(defn read-view-file [body file convert]
  (let [full (view-file body file)
        k (body-key body)]
    (try
      (let [mtime (.-mtimeMs (.statSync fs full))
            cached (get-in @view-cache [k file])]
        (if (= mtime (:mtime cached))
          cached
          (let [entry {:mtime mtime :value (convert (parse-js (.readFileSync fs full "utf8")))}]
            (swap! view-cache assoc-in [k file] entry)
            entry)))
      (catch :default _ nil))))

(defn read-view [body]
  (let [pose (read-view-file body "pose.json" pose-fields)
        hud (read-view-file body "hud.json" #(some-> % (js->clj :keywordize-keys true)))]
    (view-info/summarize (:value pose) (:value hud) (:mtime pose))))

(defn engine-body [agent now]
  (let [body (body-key agent)
        live (get @live-engines body)
        canonical? (canonical-engine? body)
        live-error (get @live-errors body)
        persisted-state (when canonical?
                          (let [{:keys [value]} (engine-edn/read-edn (read-edn-text body))]
                            (when (map? value) value)))
        folded (if canonical?
                 (or (:folded live) ee/empty-engine)
                 (try (read-engine body) (catch :default _ ee/empty-engine)))
        engine-view (ee/engine-view folded now)
        pose-view (read-view body)
        snap (:snapshot live)
        authoritative-state (if (and snap (not live-error)) (:state snap) persisted-state)
        scheduler-summary (if (and canonical? (map? authoritative-state))
                            (engine-edn/summarize (pr-str authoritative-state) now)
                            (edn-fields body now))
        outstanding (if (and snap (not live-error))
                      (or (:outstanding snap) {})
                      (or (:attention persisted-state) {}))
        position (or (:position snap) (:pos engine-view))
        offline? (or (and canonical? live-error)
                     (true? (:offline snap))
                     (and canonical? (nil? snap)))
        view (ee/body-view engine-view {:position position
                                        :cursor (:cursor live)
                                        :generation-id (:generation-id snap)
                                        :outstanding outstanding
                                        :scheduler-summary scheduler-summary
                                        :offline? offline?
                                        :snap-present? (some? snap)
                                        :settling? (boolean (:settling snap))})]
    (assoc (ee/engine-body agent (ee/with-view-status view pose-view))
           :state (when position {:pos position})
           :outstanding outstanding
           :view pose-view)))

;; every body folder of every world: {:world :name :text raw config.json}
(defn agent-entries []
  (mapv (fn [{:keys [world name dir]}] {:world world :name name :text (read-text (.join path dir "config.json"))})
        (bodies/list-bodies state-dir)))

(defn bodies [now]
  (let [entries (agent-entries)
        engine? (filter engine-folder? entries)
        other (remove engine-folder? entries)]
    (vec (concat (map #(engine-body % now) (ee/parse-engine-agents engine?))
                 (map ee/unsupported-body (ee/parse-engine-agents other))))))

(defn agent-names [bodies]
  (vec (distinct (mapcat (juxt :name :username) bodies))))

;; Entity socket refreshes run in the background when dashboard requests arrive. Socket failures
;; preserve only the remaining lifetime of the previously observed entities.
(def entity-refresh-ms 2000)
(def entity-request-ms 1000)
(def entity-response-bytes (* 4 1024 1024))
(def entity-source-cap 128)
(def entity-request-cap 4)
(defonce entity-cache (atom {}))
(defonce entity-in-flight (atom #{}))

(defn entity-socket [body] (.join path (engine-dir body) "control.sock"))

(defn entity-request! [body]
  (js/Promise.
   (fn [resolve reject]
     (let [req (.request http #js {:socketPath (entity-socket body) :method "GET" :path "/entities"}
                         (fn [res]
                           (let [chunks (atom []) size (atom 0)]
                             (.on res "error" reject)
                             (.on res "data"
                                  (fn [chunk]
                                    (swap! size + (.-length chunk))
                                    (if (> @size entity-response-bytes)
                                      (.destroy res (js/Error. "engine entity response exceeds 4 MiB"))
                                      (swap! chunks conj chunk))))
                             (.on res "end"
                                  (fn []
                                    (if (= 200 (.-statusCode res))
                                      (try (resolve (edn/one-form (.toString (js/Buffer.concat (to-array @chunks)) "utf8")))
                                           (catch :default e (reject e)))
                                      (reject (ex-info (str "engine entity API HTTP " (.-statusCode res))
                                                       {:status (.-statusCode res)}))))))))
           timer (js/setTimeout #(.destroy req (js/Error. "engine entity API timed out")) entity-request-ms)]
       (.once req "close" #(js/clearTimeout timer))
       (.on req "error" reject)
       (.end req)))))

(defn refresh-entities! [world-name]
  (let [now (js/Date.now)
        targets (->> (agent-entries) (filter #(and (= world-name (:world %)) (engine-folder? %)))
                     (sort-by (fn [b] [(get-in @entity-cache [(body-key b) :requested-at] 0) (:name b)])))
        room (max 0 (- entity-request-cap (count @entity-in-flight)))]
    (swap! entity-cache #(entities/bound (into {} (filter (fn [[key _]] (some (fn [b] (= key (body-key b))) (agent-entries))) %)) now))
    (doseq [body (take room (filter #(and (not (contains? @entity-in-flight (body-key %)))
                                         (>= (- now (get-in @entity-cache [(body-key %) :requested-at] 0)) entity-refresh-ms)) targets))
            :let [key (body-key body)]]
      (swap! entity-in-flight conj key)
      (swap! entity-cache update key #(assoc % :body key :requested-at now :status (or (:status %) :loading)))
      (-> (entity-request! body)
          (.then (fn [payload]
                   (swap! entity-cache assoc key (entities/body-snapshot body payload (js/Date.now)))
                   (swap! entity-cache #(entities/bound % (js/Date.now)))))
          (.catch (fn [e]
                    (swap! entity-cache update key
                           #(assoc % :online? nil :status (if (= 404 (:status (ex-data e))) :unsupported :unavailable)
                                     :error (if (= 404 (:status (ex-data e)))
                                              "This body has no /entities endpoint. Restart it with the current engine build to enable entity observations."
                                              (subs (str (ex-message e)) 0 (min 384 (count (str (ex-message e))))))))))
          (.finally #(swap! entity-in-flight disj key))))
    (when (> (count @entity-cache) entity-source-cap)
      (swap! entity-cache #(into {} (take entity-source-cap (sort-by (comp - :requested-at val) %)))))))

(defn entity-snapshot [world-name dimension]
  (let [targets (filter #(and (= world-name (:world %)) (engine-folder? %)) (agent-entries))
        scoped (into {} (for [body (take entity-source-cap (sort-by :name targets))
                              :let [key (body-key body)]]
                          [key (or (get @entity-cache key) {:body key :status :loading :entities []})]))
        snapshot (entities/merge-world scoped world-name dimension (js/Date.now))
        truncated? (> (count targets) entity-source-cap)]
    (assoc snapshot :source-count (count targets) :source-cap entity-source-cap
           :sources-truncated? truncated? :truncated? (or truncated? (:truncated? snapshot)))))

;; ---------------------------------------------------------------- saved village observations
(defn to-js [x] (clj->js x :keyword-fn #(subs (str %) 1)))

(defn village-snapshot [worlds]
  (village-data/snapshot root
                         (vec (mapcat (fn [world]
                                        (map #(assoc % :world (:name world)) (:places world)))
                                      worlds))
                         {:worlds (mapv :name worlds) :blueprint-dir (.join path repo-root "blueprints")}))

;; ---------------------------------------------------------------- state
(defn world-entry [bodies agent-names world]
  (let [own (filterv #(= (:name world) (:world %)) bodies)
        observations (entity-snapshot (:name world) "overworld")]
    (assoc world :bodies own :humans []
           :entities (:entities observations) :entity-sources (:sources observations)
           :entity-truncated? (:truncated? observations))))

(defn attach-villages [worlds villages]
  (mapv (fn [world]
          (assoc world :places
                 (worlds/readable-places
                  (village-data/attach-status (mapv #(assoc % :world (:name world)) (:places world))
                                              (filterv #(= (:name world) (:world %)) villages)))))
        worlds))

(declare send-edn!)

(defn snapshot [world-name]
  (let [now (js/Date.now)
        all-bodies (bodies now)
        names (agent-names all-bodies)
        all-worlds (read-worlds)
        {:keys [villages error]} (village-snapshot all-worlds)
        with-villages (attach-villages all-worlds villages)
        full {:at now
              :agents names
              :bodies all-bodies
              :worlds (mapv #(world-entry all-bodies names %) with-villages)
              :villageError error}]
    (assoc (worlds/scope-snapshot full world-name)
           :worldList (read-world-list)
           :selected world-name)))

(defn send-state! [res world-name]
  (refresh-entities! world-name)
  (-> (refresh-live-engines!)
      (.then (fn [_] (send-edn! res 200 (assoc (snapshot world-name) :build-id restart/build-id))))
      (.catch (fn [e]
                (when-not (.-headersSent res)
                  (send-edn! res 500 {:error (str (ex-message e))}))))))

;; ---------------------------------------------------------------- chat
(defn talk-lines-of [text]
  (filterv chat/talk? (ee/parse-event-lines text chat/maybe-talk-line?)))

(defn edn-talk-lines-of [text]
  (into [] (comp (filter chat/maybe-edn-talk-line?) (keep chat/edn-talk-line)) (.split text "\n")))

;; First sight of a file: its last chat-tail-bytes. After that only the bytes appended since (a line still being
;; written is carried to the next read); a file that shrank starts over. Position events flood the file, so a small
;; tail would forget a chat line within minutes. Each chunk is reduced to its chat lines at once.
(defn read-chat-tail [file lines-of cached]
  (let [size (.-size (.statSync fs file))
        fresh? (or (nil? cached) (< size (:size cached)))
        start (if fresh? (max 0 (- size chat-tail-bytes)) (:size cached))
        keep-last (fn [lines text] (vec (take-last chat-keep (into lines (lines-of text)))))
        {:keys [acc rest]} (reduce-lines file start size (and fresh? (pos? start))
                                         (if fresh? (js/Uint8Array. 0) (:rest cached))
                                         keep-last (if fresh? [] (:lines cached)))]
    {:size size :rest rest :lines acc}))

(defn chat-lines [body]
  (let [canonical? (canonical-engine? body)
        file (if canonical? (canonical-events-file body) (events-file body))
        size (.-size (.statSync fs file))
        k (body-key body)
        cached (get @tails k)]
    (if (= size (:size cached))
      (:lines cached)
      (let [tail (read-chat-tail file (if canonical? edn-talk-lines-of talk-lines-of) cached)]
        (swap! tails assoc k tail)
        (:lines tail)))))

(def chat-sender (chat-send/configured-sender (.-DASHBOARD_CHAT_AS js/process.env)))

(defn chat-log [limit world-name]
  (let [agents (ee/parse-engine-agents (filter #(and (= world-name (:world %)) (engine-folder? %)) (agent-entries)))]
    {:at (js/Date.now)
     :sender chat-sender
     :agents (agent-names agents)
     :messages (chat/merge-chat (mapv (fn [a] {:agent (:name a) :lines (try (chat-lines a) (catch :default _ []))}) agents) limit)}))

;; ---------------------------------------------------------------- responses
(def content-types
  {".html" "text/html; charset=utf-8" ".js" "text/javascript; charset=utf-8" ".css" "text/css; charset=utf-8"
   ".json" "application/json" ".map" "application/json" ".png" "image/png" ".svg" "image/svg+xml"
   ".ico" "image/x-icon" ".txt" "text/plain; charset=utf-8" ".woff" "font/woff" ".woff2" "font/woff2"})

(defn send! [res code type payload]
  (.writeHead res code #js {"content-type" type "cache-control" "no-store"})
  (.end res payload))

(defn send-json-js! [res code value]
  (send! res code "application/json" (js/JSON.stringify value)))

(defn send-json! [res code value]
  (send-json-js! res code (to-js value)))

(defn send-edn! [res code value]
  (send! res code "application/edn; charset=utf-8" (pr-str value)))

(defn send-file! [res file]
  (let [type (get content-types (str/lower-case (.extname path file)) "application/octet-stream")]
    (if (file-exists? file)
      (send! res 200 type (.readFileSync fs file))
      (send-json! res 404 {:error (str "no such file: " (.basename path file))}))))

;; a file under dir only when the normalised path stays inside it
(defn safe-join [dir relative]
  (let [full (.resolve path dir (str/replace relative #"^/+" ""))]
    (when (str/starts-with? full (str dir (.-sep path))) full)))

(defn serve-static! [res request-path]
  (let [js? (str/starts-with? request-path "/js/")
        file (if js?
               (safe-join js-dir (subs request-path 4))
               (safe-join public-dir request-path))]
    (if file
      (send-file! res file)
      (send-json! res 404 {:error "not found"}))))

;; ---------------------------------------------------------------- thumbnails
;; The fallback for browsers without WebGL2: dashboard.thumbs decides, js/thumbs.mjs renders one still on request.
;; thumbs.mjs is ESM and this build is CJS. A literal import() in the bundle does not work (Closure; and
;; new Function("return import(..)") fails in the bundle: "A dynamic import callback was not specified"), so js/import-esm.cjs,
;; a native CommonJS file, makes the call. THUMBS_MODULE overrides the module path (tests).
(def thumbs-module (or (.-THUMBS_MODULE js/process.env) (.join path dashboard-dir "js" "thumbs.mjs")))

(def import-esm (js/require (.join path dashboard-dir "js" "import-esm.cjs")))

(def no-thumbnails
  {:get (fn [_] (js/Promise.resolve nil))
   :stats (fn [] {:error "thumbnailer unavailable"})
   :close (fn [])})

(defn pose-mtime [body]
  (try (.-mtimeMs (.statSync fs (view-file body "pose.json")))
       (catch :default _ nil)))

(defn load-thumbnailer []
  (-> (import-esm (.-href (.pathToFileURL url thumbs-module)))
      (.then (fn [m]
               (let [r ((.-createRenderer m) #js {:stateDir state-dir})
                     t (thumbs/make {:render (fn [{:keys [world name]}] (-> (.render r world name)
                                                            (.then (fn [x] (when x {:png (.-png x) :ms (.-ms x) :loaded (.-loaded x)})))))
                                     :pose-mtime pose-mtime
                                     :recycle #(.recycle r)
                                     :now #(js/Date.now)
                                     :min-interval-ms thumbs/min-interval-ms
                                     :column-cap thumbs/column-cap})]
                 (assoc t :close #(.close r)))))
      (.catch (fn [e]
                (js/console.error (str "thumbnails disabled: " (ex-message e)))
                no-thumbnails))))

;; one thumbnailer for the process, created on the first request
(defonce thumbnailer (delay (load-thumbnailer)))

(defn send-thumb! [res body]
  (-> @thumbnailer
      (.then (fn [t] ((:get t) (body-key body))))
      (.then (fn [thumb]
               (if-not thumb
                 (send-json! res 404 {:error (str "no view for " (:name body) " in world " (:world body))})
                 (do (.writeHead res 200 #js {"content-type" "image/png" "cache-control" "no-store"
                                              "x-pose-mtime" (str (:pose-mtime-ms thumb))})
                     (.end res (:png thumb))))))
      (.catch (fn [e] (when-not (.-headersSent res) (send-json! res 500 {:error (str (ex-message e))}))))))

(defn send-thumbs-stats! [res]
  (-> @thumbnailer
      (.then (fn [t] (send-json! res 200 ((:stats t)))))))

;; ---------------------------------------------------------------- the live view, on this origin
;; js/viewmount.mjs builds tools/view/serve.mjs's request handler without listening; its paths (/view, /pose/, /hud/,
;; /drive/, /web/ ...) are forwarded to it. Loaded once at startup, like the thumbnailer; absent when it fails to load.
(def viewmount-module (or (.-VIEWMOUNT_MODULE js/process.env) (.join path dashboard-dir "js" "viewmount.mjs")))

(defonce view-mount (atom nil))

(defn load-view-mount []
  (-> (import-esm (.-href (.pathToFileURL url viewmount-module)))
      (.then (fn [m] (reset! view-mount ((.-mountView m) #js {:repo repo-root :stateDir state-dir}))))
      (.catch (fn [e] (js/console.error (str "live view disabled: " (ex-message e)))))))

(defn view-request? [req]
  (when-let [m @view-mount]
    (.handles m (.-pathname (js/URL. (.-url req) "http://dashboard")))))

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

(declare port read-body)

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
                         (-> (event-socket-request! body "POST" "/attention/resolve" request)
                             (.then #(send-edn! res 200 %))
                             (.catch (fn [e] (when-not (.-headersSent res)
                                               (send-edn! res 503 {:error (ee/socket-failure-text e)})))))))))))))

;; ---------------------------------------------------------------- item pictures
;; The textures the view uses (repo textures/: blocks at the top, items under item/): the first candidate that exists.
(def textures-dir (.join path repo-root "textures"))

(defn send-item-icon! [res name]
  (let [file (->> (items/icon-candidates name)
                  (map #(.join path textures-dir %))
                  (filter file-exists?)
                  first)]
    (if-not file
      (send-json! res 404 {:error (str "no picture for " name)})
      (do (.writeHead res 200 #js {"content-type" "image/png" "cache-control" "public, max-age=3600"})
          (.end res (.readFileSync fs file))))))

(def route-list
  "try /, /villagers, /villages, /blueprints, /api/worlds, /api/state, /api/villagers, /api/villages, /api/chat?limit=200, POST /api/chat/send, /api/jobs, /api/plans, /api/plan/<name>, /api/blueprints, /api/blueprint/<name>, POST /api/blueprint-preview (state, chat, world and villages take ?world=<name>, default the first world)")

(defn read-body [req limit on-done]
  (let [chunks (atom []) size (atom 0)]
    (.on req "data" (fn [chunk]
                      (swap! size + (.-length chunk))
                      (when (<= @size limit) (swap! chunks conj chunk))))
    (.on req "end" #(on-done (when (<= @size limit) (.toString (js/Buffer.concat (to-array @chunks)) "utf8"))))))

;; ---------------------------------------------------------------- state-changing routes
;; Every POST route goes through dashboard.guard (Host, Origin, Content-Type, method) and a body limit.
(def port (js/Number (or (.-PORT js/process.env) 3701)))

(defn guarded-post!
  "Refuses with the guard's status, else reads the bounded body and calls on-body."
  ([req res limit-bytes on-body]
   (guarded-post! req res limit-bytes on-body {}))
  ([req res limit-bytes on-body {:keys [content-types send-error] :or {send-error send-json!}}]
   (let [headers (.-headers req)
         refused (or (guard/method-refusal (.-method req))
                     (guard/refusal {:host (.-host headers) :origin (.-origin headers)
                                     :content-type (aget headers "content-type") :port port
                                     :content-types content-types}))]
     (if refused
       (send-error res (:status refused) {:error (:error refused)})
       (read-body req limit-bytes on-body)))))

;; ---------------------------------------------------------------- chat send (POST /api/chat/send)
;; dashboard.chat-send validates the body and builds the fixed tellraw command; the runner is RCON (dashboard.rcon),
;; or, with DASHBOARD_CHAT_DRY=1, one that only logs the command. The sender is DASHBOARD_CHAT_AS (default "dashboard").
(def chat-dry? (= "1" (.-DASHBOARD_CHAT_DRY js/process.env)))
(def chat-stamps (atom []))

(defn run-chat-command! [command]
  (if chat-dry?
    (do (println (str "chat send (dry run): " command)) (js/Promise.resolve "dry"))
    (rcon/send-command! command)))

(defn send-chat! [req res]
  (guarded-post!
   req res guard/max-body-bytes
   (fn [text]
     (let [{:keys [status json command stamps]} (chat-send/plan text {:sender chat-sender :stamps @chat-stamps :now (js/Date.now)})]
       (reset! chat-stamps stamps)
       (if status
         (send-json! res status json)
         (-> (run-chat-command! command)
             (.then (fn [_] (send-json! res 200 {:ok true :command command})))
             (.catch (fn [e] (send-json! res 502 {:error (str "RCON failed: " (ex-message e))})))))))))

(defn send-whisper! [req res {target :name world :world}]
  (guarded-post!
   req res guard/max-body-bytes
   (fn [text]
     (let [engine-bodies (filter #(and (:engine %) (= world (:world %))) (bodies (js/Date.now)))
           {:keys [status json command stamps]} (chat-send/plan-whisper
                                                 target text
                                                 {:sender chat-sender :stamps @chat-stamps :now (js/Date.now)
                                                  :known (set (map :name engine-bodies))
                                                  :online (set (map :name (filter :up engine-bodies)))})]
       (reset! chat-stamps stamps)
       (if status
         (send-json! res status json)
         (-> (run-chat-command! command)
             (.then (fn [_] (send-json! res 200 {:ok true :command command})))
             (.catch (fn [e] (send-json! res 502 {:error (str "RCON failed: " (ex-message e))})))))))))

;; ---------------------------------------------------------------- jobs (GET /api/jobs)
;; The job and trigger namespaces as compiled into this build (dashboard.jobs-registry), joined with usage.
(defn read-jobs [now]
  (jobs-registry/attach-usage jobs-registry/entries (jobs-registry/usage (bodies now))))

;; ---------------------------------------------------------------- restart (POST /api/restart)
;; Asks the launcher (start.mjs) to rebuild and replace this server; 202 at once, the build happens there.
(defn request-restart! [req res]
  (if-not (restart/loopback-address? (some-> req .-socket .-remoteAddress))
    (send-json! res 403 {:error "restart is for loopback clients only"})
    (guarded-post!
     req res guard/max-body-bytes
     (fn [_]
       (if (restart/ask-launcher!)
         (send-json! res 202 {:ok true :message "restart requested; the launcher builds first and keeps this server if the build fails"})
         (send-json! res 409 {:error "no launcher: this server was not started by npm start"}))))))

(defn preview! [req res]
  (guarded-post!
   req res max-preview-bytes
   (fn [text]
     (if (nil? text)
       (send-edn! res 413 {:error "preview body exceeds 2 MiB"})
       (try
         (let [input (edn/one-form text)]
           (if-not (and (map? input) (string? (:source input))
                        (or (nil? (:stock input)) (string? (:stock input))))
             (send-edn! res 400 {:error "preview requires {:source <EDN text> :stock <optional EDN text>}"})
             (let [detail (blueprint-lib/preview (:source input) (:stock input))]
               (send-edn! res 200 detail))))
         (catch :default e (send-edn! res 400 {:error (ex-message e)})))))
   {:content-types ["application/edn"] :send-error send-edn!}))

(defn blueprint-library []
  (blueprint-lib/library (.join path repo-root "blueprints")))

;; ---------------------------------------------------------------- plans (dashboard.plan-api)
;; Plans are state/worlds/<world>/plans/<id>.edn, the blueprints they place blueprints/<id>.edn at the repo root (next to
;; the legacy .blueprint.json library). The block lookup over the dumped chunk columns is the JS glue js/worldblocks.mjs
;; (the view's column decoders); everything else is ClojureScript.
(def worldblocks-module (or (.-WORLDBLOCKS_MODULE js/process.env) (.join path dashboard-dir "js" "worldblocks.mjs")))
(def plan-blueprints-dir (.join path repo-root "blueprints"))
(def plans-ttl-ms 10000)

(defonce worldblocks-loaded (delay (import-esm (.-href (.pathToFileURL url worldblocks-module)))))
(defonce blocks-by-world (atom {}))
(defonce plan-summaries (atom {}))

(defn blocks-for [module world-name]
  (or (get @blocks-by-world world-name)
      (let [blocks ((.-createWorldBlocks module) #js {:stateDir (.join path root "state") :world world-name})]
        (swap! blocks-by-world assoc world-name blocks)
        blocks)))

(declare column-mtime)

(defn plan-opts [module world-name]
  (let [blocks (blocks-for module world-name)]
    {:dir (.join path worlds-dir world-name "plans")
     :blueprint-dir plan-blueprints-dir
     :block-at (fn [x y z] (.blockAt blocks x y z))
     :column-mtime (fn [cx cz] (column-mtime world-name cx cz))}))

(defn plan-list [module world-name]
  (let [cached (get @plan-summaries world-name)]
    (if (and cached (< (- (js/Date.now) (:at cached)) plans-ttl-ms))
      (:value cached)
      (let [value (assoc (plan-api/summaries (plan-opts module world-name)) :world world-name)]
        (swap! plan-summaries assoc world-name {:at (js/Date.now) :value value})
        value))))

(defn send-plans! [res world-name plan-id]
  (-> @worldblocks-loaded
      (.then (fn [module]
               (if-not plan-id
                 (send-json! res 200 (plan-list module world-name))
                 (if-let [found (plan-api/detail (plan-opts module world-name) plan-id)]
                   (send-json! res 200 found)
                   (send-json! res 404 {:error (str "no plan called " plan-id " in " world-name)})))))
      (.catch (fn [e] (when-not (.-headersSent res) (send-json! res 500 {:error (str (ex-message e))}))))))

;; ---------------------------------------------------------------- terrain tiles (dashboard.tiles)
;; GET /api/tile/<world>/<cx>.<cz>.png: the top-down picture of a dumped column. The JS glue decodes the column (a few ms),
;; dashboard.tiles colours it, the PNG is cached by the column file's mtime in a bounded LRU. A tile is ~100-600 bytes.
(def tile-cache-cap 2048)
(def tile-list-ttl-ms 5000)
(defonce tile-cache (tiles/lru))
(defonce tile-lists (atom {}))
(defonce world-tiles (atom {}))
(defonce tile-stats (atom {:renders 0 :render-ms 0 :hits 0}))

(defn tiles-for [module world-name]
  (or (get @world-tiles world-name)
      (let [made ((.-createWorldTiles module) #js {:stateDir (.join path root "state") :world world-name})]
        (swap! world-tiles assoc world-name made)
        made)))

(defn column-mtime [world-name cx cz]
  (try (js/Math.floor (.-mtimeMs (.statSync fs (.join path worlds-dir world-name "chunks" (str cx "." cz ".bin")))))
       (catch :default _ nil)))

(defn render-tile
  "The PNG of the column, or nil when it is not dumped."
  [module world-name cx cz]
  (let [started (js/performance.now)]
    (when-let [col (.column (tiles-for module world-name) cx cz (to-js tiles/skipped-blocks))]
      (let [png ((.-encodeTile module) tiles/size tiles/size
                 (tiles/tile-rgba {:palette (vec (.-palette col)) :top (.-top col) :floor (.-floor col) :y (.-y col) :depth (.-depth col)}))]
        (swap! tile-stats #(-> % (update :renders inc) (update :render-ms + (- (js/performance.now) started))))
        png))))

(defn cached-tile [module world-name cx cz mtime]
  (let [k (str world-name "/" cx "." cz)
        held (tiles/lru-get! tile-cache k)]
    (if (and held (= mtime (:mtime held)))
      (do (swap! tile-stats update :hits inc) (:png held))
      (when-let [png (render-tile module world-name cx cz)]
        (tiles/lru-put! tile-cache tile-cache-cap k {:mtime mtime :png png})
        png))))

(defn send-tile! [res world-name cx cz]
  (let [mtime (when (some #{world-name} (world-names)) (column-mtime world-name cx cz))]
    (if-not mtime
      (send-json! res 404 {:error (str "column " cx "." cz " of " world-name " is not dumped")})
      (-> @worldblocks-loaded
          (.then (fn [module]
                   (if-let [png (cached-tile module world-name cx cz mtime)]
                     (do (.writeHead res 200 #js {"content-type" "image/png" "cache-control" "public, max-age=3600"
                                                  "x-column-mtime" (str mtime)})
                         (.end res png))
                     (send-json! res 404 {:error (str "column " cx "." cz " of " world-name " is not dumped")}))))
          (.catch (fn [e] (when-not (.-headersSent res) (send-json! res 500 {:error (str (ex-message e))}))))))))

(defn column-entries [world-name]
  (let [dir (.join path worlds-dir world-name "chunks")]
    (vec (for [file (try (.readdirSync fs dir) (catch :default _ #js []))
               :let [mtime (try (.-mtimeMs (.statSync fs (.join path dir file))) (catch :default _ nil))]
               :when mtime]
           [file mtime]))))

(defn tile-entries
  "[[cx cz mtime]] of the world's dumped columns, re-read from the directory at most every 5 s."
  [world-name]
  (let [{:keys [at value]} (get @tile-lists world-name)]
    (if (and at (< (- (js/Date.now) at) tile-list-ttl-ms))
      value
      (let [value (tiles/index-entries (column-entries world-name))]
        (swap! tile-lists assoc world-name {:at (js/Date.now) :value value})
        value))))

(defn send-tiles! [res world-name query]
  (if-not (some #{world-name} (world-names))
    (send-json! res 404 {:error (str "no world called " world-name)})
    (let [since (some-> (.get query "since") (js/parseInt 10))]
      (send-json! res 200 {:world world-name :at (js/Date.now)
                           :tiles (tiles/newer-than (tile-entries world-name) (when-not (js/isNaN since) since))}))))

(defn tile-stats-json []
  (let [{:keys [renders render-ms hits]} @tile-stats]
    {:renders renders :renders-ms-avg (when (pos? renders) (/ render-ms renders)) :hits hits :cached (.-size tile-cache)}))

(defn handle-world-scoped! [res kind world-name query plan-name]
  (case kind
    :state (send-state! res world-name)
    :plans-api (send-plans! res world-name nil)
    :plan-api (send-plans! res world-name plan-name)
    :chat (send-json! res 200 (chat-log (chat/chat-limit (.get query "limit")) world-name))
    :world (send-json! res 501 {:error "unsupported for engine bodies: world scan (needs a body's HTTP API)"})
    :entities-api (do (refresh-entities! world-name)
                      (send-edn! res 200 (entity-snapshot world-name (.get query "dimension"))))
    :villagers-api (do (refresh-entities! world-name)
                       (send-edn! res 200 (entities/villagers (entity-snapshot world-name (.get query "dimension")))))
    :villages-api (let [snap (village-snapshot (filterv #(= world-name (:name %)) (read-worlds)))]
                    (send-edn! res 200 (assoc snap :readOnly true)))))

(def world-kinds #{:state :chat :world :villages-api :villagers-api :entities-api :plans-api :plan-api})
(def edn-library-kinds #{:villages-api :villagers-api :entities-api :blueprints :blueprint :blueprint-preview})

(defn handle! [req res]
  (let [{:keys [kind] blueprint-name :name request-path :path :as route} (routes/route (.-url req))
        query (.-searchParams (js/URL. (.-url req) "http://dashboard"))
        choice (when (world-kinds kind) (choose-world query))]
    (cond
      (:error choice) ((if (edn-library-kinds kind) send-edn! send-json!) res 400 choice)
      (world-kinds kind) (handle-world-scoped! res kind (:name choice) query blueprint-name)
      :else
      (case kind
        :page (send-file! res (.join path public-dir "index.html"))
        :static (serve-static! res request-path)
        :thumb (send-thumb! res (body-key route))
        :item-icon (send-item-icon! res blueprint-name)
        :events (send-events! res (body-key route) query)
        :attention-resolve (resolve-attention! req res (body-key route))
        :chat-send (send-chat! req res)
        :whisper-send (send-whisper! req res (body-key route))
        :restart (request-restart! req res)
        :jobs-api (send-json! res 200 {:at (js/Date.now) :jobs (read-jobs (js/Date.now))})
        :thumbs-stats (send-thumbs-stats! res)
        :tile (send-tile! res (:world route) (:cx route) (:cz route))
        :tiles (send-tiles! res (:world route) query)
        :tile-stats (send-json! res 200 (tile-stats-json))
        :worlds (send-json! res 200 {:worlds (read-world-list)})
        :blueprints (send-edn! res 200 (blueprint-library))
        :blueprint (let [found (first (filter #(= blueprint-name (:name %)) (:blueprints (blueprint-library))))]
                     (if found
                       (send-edn! res 200 found)
                       (send-edn! res 404 {:error (str "no blueprint called " blueprint-name ": /api/blueprints lists them")})))
        :blueprint-preview (preview! req res)
        :unsupported (send-json! res 404 {:error "unsupported for engine bodies"})
        (send-json! res 404 {:error route-list})))))

(defn handler [req res]
  (try
    (if (view-request? req)
      (.handle @view-mount req res)
      (handle! req res))
    (catch :default e
      (if (.-headersSent res)
        (.end res)
        ((if (edn-library-kinds (:kind (routes/route (.-url req)))) send-edn! send-json!)
         res 500 {:error (str (ex-message e))})))))

(defn close-all!
  "Ends the thumbnail worker and the view server's scan worker (those that were started); resolves when done."
  []
  (js/Promise.all
   #js [(if (realized? thumbnailer)
          (-> @thumbnailer (.then (fn [t] ((:close t)))) (.catch (fn [_])))
          (js/Promise.resolve))
        (if-let [m @view-mount]
          (-> (js/Promise.resolve (.close m)) (.catch (fn [_])))
          (js/Promise.resolve))]))

(defn shutdown-on-signals!
  "SIGTERM and SIGINT close the http server and the workers, then exit (after at most 3 s)."
  [server]
  (let [done (atom false)
        stop (fn [_]
               (when-not @done
                 (reset! done true)
                 (.unref (js/setTimeout #(.exit js/process 1) 3000))
                 (.close server)
                 (-> (close-all!) (.then #(.exit js/process 0)))))]
    (.on js/process "SIGTERM" stop)
    (.on js/process "SIGINT" stop)))

(defn main []
  (when-not (chat-send/valid-sender? chat-sender)
    (js/console.error (str "DASHBOARD_CHAT_AS must match " chat-send/sender-re ", got " (pr-str chat-sender)))
    (.exit js/process 1))
  (let [server (.createServer http handler)]
    (shutdown-on-signals! server)
    (-> (load-view-mount)
        (.then (fn [_]
                 (.listen server port "127.0.0.1"
                          #(println (str "dashboard on http://127.0.0.1:" port " (root " root ")"))))))))
