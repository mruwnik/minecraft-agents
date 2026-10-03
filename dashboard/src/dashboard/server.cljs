(ns dashboard.server
  "The dashboard server: plain node http on 127.0.0.1. Engine bodies are read from state/agents/<name>/engine/
  (events.jsonl tail and engine.edn); no body is ever contacted."
  (:require ["fs" :as fs]
            ["http" :as http]
            ["path" :as path]
            [clojure.string :as str]
            [dashboard.chat :as chat]
            [dashboard.engine-edn :as engine-edn]
            [dashboard.engine-events :as ee]
            [dashboard.legacy :as legacy]
            [dashboard.mapview :as mapview]
            [dashboard.routes :as routes]
            [dashboard.worlds :as worlds]))

(def repo-root (.resolve path js/__dirname ".." ".."))
(def root (or (.-DASHBOARD_ROOT js/process.env) repo-root))
(def dashboard-dir (.join path repo-root "dashboard"))
(def public-dir (.join path dashboard-dir "public"))
(def js-dir (.join path dashboard-dir "out" "public" "js"))
(def agents-dir (.join path root "state" "agents"))
(def worlds-dir (.join path root "state" "worlds"))

(def first-read-bytes (* 4 1024 1024)) ; ~10 minutes of debug-heavy engine events
(def chat-tail-bytes (* 64 1024))
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

;; ---------------------------------------------------------------- worlds
(defn world-names []
  (filterv #(file-exists? (.join path worlds-dir % "world.json")) (dir-names worlds-dir)))

(defn read-world-list []
  (worlds/parse-world-list
   (mapv (fn [n] {:name n :text (read-text (.join path worlds-dir n "world.json"))}) (world-names))))

(defn read-worlds []
  (mapv (fn [n]
          (let [dir (.join path worlds-dir n)]
            {:name n
             :places (read-json (.join path dir "places.json") [])
             :zones (read-json (.join path dir "zones.json") [])
             :clock (read-json (.join path dir "clock.json") nil)}))
        (world-names)))

;; the name only ever comes out of the directory listing, never from the query
(defn choose-world [query]
  (worlds/world-choice (world-names) (.get query "world")))

;; ---------------------------------------------------------------- engine bodies
;; Per body: how far into events.jsonl we have read, the folded state, and the bytes of a line still being written.
(def engines (atom {}))
(def edn-cache (atom {}))
(def tails (atom {}))

(defn events-file [name] (.join path agents-dir name "engine" "events.jsonl"))
(defn engine-folder? [name] (file-exists? (events-file name)))

(defn advance-engine
  "Reads only what was appended since; a file that shrank starts over from its tail."
  [cached file]
  (let [size (.-size (.statSync fs file))
        fresh? (or (nil? cached) (< size (:offset cached)))
        from (if fresh? (max 0 (- size first-read-bytes)) (:offset cached))
        previous (if fresh? {:state ee/empty-engine :rest (js/Uint8Array. 0)} cached)]
    (if (and (not fresh?) (= size (:offset cached)))
      cached
      (let [chunk (read-range file from size)
            {:keys [complete rest]} (ee/complete-lines (:rest previous) (if (and fresh? (pos? from)) (ee/drop-torn-head chunk) chunk))]
        {:offset size
         :state (ee/fold-engine (:state previous) (ee/parse-event-lines (ee/decode-bytes complete)))
         :rest rest}))))

(defn read-engine [name]
  (let [entry (advance-engine (get @engines name) (events-file name))]
    (swap! engines assoc name entry)
    (:state entry)))

;; engine.edn is re-read when its mtime or size changed
(defn read-edn-text [name]
  (let [file (.join path agents-dir name "engine" "engine.edn")]
    (try
      (let [st (.statSync fs file)
            stamp [(.-mtimeMs st) (.-size st)]
            cached (get @edn-cache name)]
        (if (= stamp (:stamp cached))
          (:text cached)
          (let [text (.readFileSync fs file "utf8")]
            (swap! edn-cache assoc name {:stamp stamp :text text})
            text)))
      (catch :default _ nil))))

(defn edn-fields [name now]
  (let [summary (engine-edn/summarize (read-edn-text name) now)]
    (if (:error summary)
      {:edn-error (:error summary)}
      summary)))

(defn engine-body [agent now]
  (let [view (try
               (ee/engine-view (read-engine (:name agent)) now)
               (catch :default e
                 (assoc (ee/engine-view ee/empty-engine now) :error (str "events unreadable: " (ex-message e)))))]
    (ee/engine-body agent (merge view (edn-fields (:name agent) now)))))

(defn agent-entries []
  (mapv (fn [n] {:name n :text (read-text (.join path agents-dir n "config.json"))}) (dir-names agents-dir)))

(defn bodies [now]
  (let [entries (agent-entries)
        engine? (filter #(engine-folder? (:name %)) entries)
        other (remove #(engine-folder? (:name %)) entries)]
    (vec (concat (map #(engine-body % now) (ee/parse-engine-agents engine?))
                 (map ee/unsupported-body (ee/parse-engine-agents other))))))

(defn agent-names [bodies]
  (vec (distinct (mapcat (juxt :name :username) bodies))))

;; ---------------------------------------------------------------- legacy (villages, villagers, blueprints)
(defn from-js [x] (js->clj x :keywordize-keys true))
(defn to-js [x] (legacy/to-js x))

(defn all-places-js [worlds]
  (to-js (vec (mapcat :places worlds))))

(defn village-snapshot [worlds]
  (let [snap (legacy/village-snapshot repo-root root (all-places-js worlds))]
    {:villages (.-villages snap) :error (.-error snap)}))

;; ---------------------------------------------------------------- state
(defn world-entry [bodies agent-names world]
  (let [own (filterv #(= (:name world) (:world %)) bodies)]
    (assoc world :bodies own :humans (mapview/human-sightings own agent-names))))

(defn attach-villages [worlds villages]
  (mapv (fn [w]
          (assoc w :places (from-js (legacy/attach-village-status repo-root (to-js (:places w)) villages))))
        worlds))

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

;; ---------------------------------------------------------------- chat
(defn read-chat-tail [file]
  (let [size (.-size (.statSync fs file))
        start (max 0 (- size chat-tail-bytes))
        bytes (read-range file start size)
        whole (if (pos? start) (ee/drop-torn-head bytes) bytes)]
    {:size size :lines (filterv chat/talk? (ee/parse-event-lines (ee/decode-bytes whole)))}))

;; the tail is kept per folder and re-read only when the file's size changed
(defn chat-lines [name]
  (let [file (events-file name)
        size (.-size (.statSync fs file))
        cached (get @tails name)]
    (if (= size (:size cached))
      (:lines cached)
      (let [tail (read-chat-tail file)]
        (swap! tails assoc name tail)
        (:lines tail)))))

(defn chat-log [limit world-name]
  (let [agents (filterv #(= world-name (:world %)) (ee/parse-engine-agents (filter #(engine-folder? (:name %)) (agent-entries))))]
    {:at (js/Date.now)
     :agents (agent-names agents)
     :messages (chat/merge-chat (mapv (fn [a] {:agent (:name a) :lines (try (chat-lines (:name a)) (catch :default _ []))}) agents) limit)}))

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

(def route-list
  "try /, /villagers, /villages, /blueprints, /api/worlds, /api/state, /api/villagers, /api/villages, /api/chat?limit=200, /api/blueprints, /api/blueprint/<name>, POST /api/blueprint-preview (state, chat, world and villages take ?world=<name>, default the first world)")

(defn read-body [req limit on-done]
  (let [chunks (atom []) size (atom 0)]
    (.on req "data" (fn [chunk]
                      (swap! size + (.-length chunk))
                      (when (<= @size limit) (swap! chunks conj chunk))))
    (.on req "end" #(on-done (when (<= @size limit) (.toString (js/Buffer.concat (to-array @chunks)) "utf8"))))))

(defn preview! [req res]
  (if-not (= "POST" (.-method req))
    (send-json! res 405 {:error "POST a structured plan to preview"})
    (read-body req max-preview-bytes
               (fn [text]
                 (if (nil? text)
                   (send-json! res 413 {:error "preview body exceeds 2 MiB"})
                   (try
                     (let [input (js/JSON.parse text)
                           detail (legacy/preview repo-root (.-plan input) (.-stock input))]
                       (send-json-js! res (if (pos? (.-length (.-errors detail))) 400 200) detail))
                     (catch :default e (send-json! res 400 {:error (ex-message e)}))))))))

(defn blueprint-library []
  (legacy/library repo-root (all-places-js (read-worlds))))

(defn handle-world-scoped! [res kind world-name query]
  (case kind
    :state (send-json! res 200 (snapshot world-name))
    :chat (send-json! res 200 (chat-log (chat/chat-limit (.get query "limit")) world-name))
    :world (send-json! res 501 {:error "unsupported for engine bodies: world scan (needs a body's HTTP API)"})
    :villages-api (let [snap (village-snapshot (filterv #(= world-name (:name %)) (read-worlds)))]
                    (send-json-js! res 200 (doto (to-js (select-keys snap [:error])) (aset "villages" (:villages snap)) (aset "readOnly" true))))))

(def world-kinds #{:state :chat :world :villages-api})

(defn handle! [req res]
  (let [{:keys [kind] blueprint-name :name request-path :path} (routes/route (.-url req))
        query (.-searchParams (js/URL. (.-url req) "http://dashboard"))
        choice (when (world-kinds kind) (choose-world query))]
    (cond
      (:error choice) (send-json! res 400 choice)
      (world-kinds kind) (handle-world-scoped! res kind (:name choice) query)
      :else
      (case kind
        :page (send-file! res (.join path public-dir "index.html"))
        :static (serve-static! res request-path)
        :worlds (send-json! res 200 {:worlds (read-world-list)})
        :villagers-api (send-json-js! res 200 (legacy/villagers repo-root root))
        :blueprints (send-json-js! res 200 (blueprint-library))
        :blueprint (let [found (.find (.-blueprints (blueprint-library)) #(= blueprint-name (.-name %)))]
                     (if found
                       (send-json-js! res 200 found)
                       (send-json! res 404 {:error (str "no blueprint called " blueprint-name ": /api/blueprints lists them")})))
        :blueprint-preview (preview! req res)
        :unsupported (send-json! res 404 {:error "unsupported for engine bodies"})
        (send-json! res 404 {:error route-list})))))

(defn handler [req res]
  (try
    (handle! req res)
    (catch :default e
      (if (.-headersSent res)
        (.end res)
        (send-json! res 500 {:error (str (ex-message e))})))))

(defn main []
  (let [port (js/Number (or (.-PORT js/process.env) 3701))
        server (.createServer http handler)]
    (.listen server port "127.0.0.1"
             #(println (str "dashboard on http://127.0.0.1:" port " (root " root ")")))))
