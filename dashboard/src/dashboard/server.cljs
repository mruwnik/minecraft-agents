(ns dashboard.server
  "The dashboard server: plain node http on 127.0.0.1. Engine bodies are read from state/agents/<name>/engine/
  (events.jsonl tail and engine.edn); no body is ever contacted."
  (:require ["fs" :as fs]
            ["http" :as http]
            ["path" :as path]
            ["url" :as url]
            [clojure.string :as str]
            [dashboard.chat :as chat]
            [dashboard.engine-edn :as engine-edn]
            [dashboard.chat-send :as chat-send]
            [dashboard.engine-events :as ee]
            [dashboard.guard :as guard]
            [dashboard.items :as items]
            [dashboard.jobs-source :as jobs-source]
            [dashboard.legacy :as legacy]
            [dashboard.mapview :as mapview]
            [dashboard.rcon :as rcon]
            [dashboard.routes :as routes]
            [dashboard.view-info :as view-info]
            [dashboard.worlds :as worlds]))

(def repo-root (.resolve path js/__dirname ".." ".."))
(def root (or (.-DASHBOARD_ROOT js/process.env) repo-root))
(def dashboard-dir (.join path repo-root "dashboard"))
(def public-dir (.join path dashboard-dir "public"))
(def js-dir (.join path dashboard-dir "out" "public" "js"))
(def agents-dir (.join path root "state" "agents"))
(def worlds-dir (.join path root "state" "worlds"))

(def first-read-bytes (* 4 1024 1024)) ; ~10 minutes of debug-heavy engine events
(def chat-tail-bytes (* 4 1024 1024))
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

;; ---------------------------------------------------------------- views (pose.json, hud.json)
;; Each file is re-read only when its mtime changed; a missing or unreadable file is nil.
(def view-cache (atom {}))

(defn view-file [name file] (.join path agents-dir name "view" file))

(defn parse-js [text] (try (js/JSON.parse text) (catch :default _ nil)))

;; pose.json carries every nearby entity and changes often: only the few fields we show are converted
(defn pose-fields [o]
  (when o
    {:status (.-status o) :dimension (.-dimension o) :pos (js->clj (.-pos o) :keywordize-keys true)}))

(defn read-view-file [name file convert]
  (let [full (view-file name file)]
    (try
      (let [mtime (.-mtimeMs (.statSync fs full))
            cached (get-in @view-cache [name file])]
        (if (= mtime (:mtime cached))
          cached
          (let [entry {:mtime mtime :value (convert (parse-js (.readFileSync fs full "utf8")))}]
            (swap! view-cache assoc-in [name file] entry)
            entry)))
      (catch :default _ nil))))

(defn read-view [name]
  (let [pose (read-view-file name "pose.json" pose-fields)
        hud (read-view-file name "hud.json" #(some-> % (js->clj :keywordize-keys true)))]
    (view-info/summarize (:value pose) (:value hud) (:mtime pose))))

(defn engine-body [agent now]
  (let [view (try
               (ee/engine-view (read-engine (:name agent)) now)
               (catch :default e
                 (assoc (ee/engine-view ee/empty-engine now) :error (str "events unreadable: " (ex-message e)))))]
    (assoc (ee/engine-body agent (merge view (edn-fields (:name agent) now)))
           :view (read-view (:name agent)))))

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
(defn talk-lines-of [bytes]
  (filterv chat/talk? (ee/parse-event-lines (ee/decode-bytes bytes))))

;; First sight of a file: its last chat-tail-bytes. After that only the bytes appended since (a line still being
;; written is carried to the next read); a file that shrank starts over. Position events flood the file, so a small
;; tail would forget a chat line within minutes.
(defn read-chat-tail [file cached]
  (let [size (.-size (.statSync fs file))
        fresh? (or (nil? cached) (< size (:size cached)))
        start (if fresh? (max 0 (- size chat-tail-bytes)) (:size cached))
        raw (read-range file start size)
        bytes (if (and fresh? (pos? start)) (ee/drop-torn-head raw) raw)
        {:keys [complete rest]} (ee/complete-lines (if fresh? (js/Uint8Array. 0) (:rest cached)) bytes)]
    {:size size
     :rest rest
     :lines (vec (take-last chat-keep (into (if fresh? [] (:lines cached)) (talk-lines-of complete))))}))

(defn chat-lines [name]
  (let [file (events-file name)
        size (.-size (.statSync fs file))
        cached (get @tails name)]
    (if (= size (:size cached))
      (:lines cached)
      (let [tail (read-chat-tail file cached)]
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

;; ---------------------------------------------------------------- thumbnails
;; js/thumbs.mjs is ESM and this build is CJS. A literal import() in the bundle does not work (Closure; and
;; new Function("return import(..)") fails in the bundle: "A dynamic import callback was not specified"), so js/import-esm.cjs,
;; a native CommonJS file, makes the call. THUMBS_MODULE overrides the module path (tests).
(def thumbs-module (or (.-THUMBS_MODULE js/process.env) (.join path dashboard-dir "js" "thumbs.mjs")))

(def import-esm (js/require (.join path dashboard-dir "js" "import-esm.cjs")))

(def no-thumbnails
  {:get (fn [_] (js/Promise.resolve nil))
   :stats (fn [] #js {:error "thumbnailer unavailable"})
   :close (fn [])})

(defn load-thumbnailer []
  (-> (import-esm (.-href (.pathToFileURL url thumbs-module)))
      (.then (fn [m] (let [t ((.-createThumbnailer m) #js {:stateDir (.join path root "state")})]
                       {:get #(.get t %) :stats #(.stats t) :close #(.close t)})))
      (.catch (fn [e]
                (js/console.error (str "thumbnails disabled: " (ex-message e)))
                no-thumbnails))))

;; one thumbnailer for the process, created on the first request
(defonce thumbnailer (delay (load-thumbnailer)))

(defn send-thumb! [res name]
  (-> @thumbnailer
      (.then (fn [t] ((:get t) name)))
      (.then (fn [thumb]
               (if-not thumb
                 (send-json! res 404 {:error (str "no view for " name)})
                 (do (.writeHead res 200 #js {"content-type" "image/png" "cache-control" "no-store"
                                              "x-pose-mtime" (str (.-poseMtimeMs thumb))})
                     (.end res (.-png thumb))))))
      (.catch (fn [e] (when-not (.-headersSent res) (send-json! res 500 {:error (str (ex-message e))}))))))

(defn send-thumbs-stats! [res]
  (-> @thumbnailer
      (.then (fn [t] (send-json-js! res 200 ((:stats t)))))))

;; ---------------------------------------------------------------- the live view, on this origin
;; js/viewmount.mjs builds tools/view/serve.mjs's request handler without listening; its paths (/view, /pose/, /hud/,
;; /drive/, /web/ ...) are forwarded to it. Loaded once at startup, like the thumbnailer; absent when it fails to load.
(def viewmount-module (or (.-VIEWMOUNT_MODULE js/process.env) (.join path dashboard-dir "js" "viewmount.mjs")))

(defonce view-mount (atom nil))

(defn load-view-mount []
  (-> (import-esm (.-href (.pathToFileURL url viewmount-module)))
      (.then (fn [m] (reset! view-mount ((.-mountView m) #js {:repo repo-root :stateDir (.join path root "state")}))))
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

(defn read-log [name]
  (let [file (events-file name)
        size (.-size (.statSync fs file))
        cached (get @log-cache name)]
    (if (= size (:size cached))
      (:events cached)
      (let [start (max 0 (- size log-tail-bytes))
            bytes (read-range file start size)
            whole (if (pos? start) (ee/drop-torn-head bytes) bytes)
            events (mapv ee/log-entry (filter ee/log-worthy? (ee/parse-event-lines (ee/decode-bytes whole))))]
        (swap! log-cache assoc name {:size size :events events})
        events))))

(defn send-events! [res name query]
  (if-not (engine-folder? name)
    (send-json! res 404 {:error (str "no engine body called " name)})
    (send-json! res 200 {:at (js/Date.now) :body name
                         :events (vec (take-last (log-limit (.get query "limit")) (read-log name)))})))

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
  "Refuses with the guard's status, else reads the body (nil when over limit-bytes) and calls (on-body text)."
  [req res limit-bytes on-body]
  (let [headers (.-headers req)
        refused (or (guard/method-refusal (.-method req))
                    (guard/refusal {:host (.-host headers) :origin (.-origin headers)
                                    :content-type (aget headers "content-type") :port port}))]
    (if refused
      (send-json! res (:status refused) {:error (:error refused)})
      (read-body req limit-bytes on-body))))

;; ---------------------------------------------------------------- chat send (POST /api/chat/send)
;; dashboard.chat-send validates the body and builds the fixed tellraw command; the runner is RCON (dashboard.rcon),
;; or, with DASHBOARD_CHAT_DRY=1, one that only logs the command. The sender is DASHBOARD_CHAT_AS (default Dan).
(def chat-dry? (= "1" (.-DASHBOARD_CHAT_DRY js/process.env)))
(def chat-sender (or (.-DASHBOARD_CHAT_AS js/process.env) "Dan"))
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

;; ---------------------------------------------------------------- jobs (GET /api/jobs)
;; The job namespaces themselves, engine/src/jobs/**/*.cljs, parsed per file and cached until the file's mtime changes.
(def jobs-dir (.join path repo-root "engine" "src" "jobs"))
(def job-cache (atom {}))

(defn job-files
  "Relative paths (from the repo root) of every job source file, sorted."
  []
  (vec (for [dir (dir-names jobs-dir)
             file (try (sort (.readdirSync fs (.join path jobs-dir dir))) (catch :default _ []))
             :when (re-find #"\.clj[sc]$" file)]
         (str "engine/src/jobs/" dir "/" file))))

(defn read-job [file]
  (let [full (.join path repo-root file)
        mtime (.-mtimeMs (.statSync fs full))
        cached (get @job-cache file)]
    (if (= mtime (:mtime cached))
      (:job cached)
      (let [job (jobs-source/parse-job file (read-text full))]
        (swap! job-cache assoc file {:mtime mtime :job job})
        job))))

(defn read-jobs [now]
  (jobs-source/attach-usage (mapv read-job (job-files)) (jobs-source/usage (bodies now))))

(defn preview! [req res]
  (guarded-post!
   req res max-preview-bytes
   (fn [text]
     (if (nil? text)
       (send-json! res 413 {:error "preview body exceeds 2 MiB"})
       (try
         (let [input (js/JSON.parse text)
               detail (legacy/preview repo-root (.-plan input) (.-stock input))]
           (send-json-js! res (if (pos? (.-length (.-errors detail))) 400 200) detail))
         (catch :default e (send-json! res 400 {:error (ex-message e)})))))))

(defn blueprint-library []
  (legacy/library repo-root (all-places-js (read-worlds))))

;; ---------------------------------------------------------------- plans (js/plans-service.mjs)
(def plans-module (or (.-PLANS_MODULE js/process.env) (.join path dashboard-dir "js" "plans-service.mjs")))

(defn load-plans-service []
  (-> (import-esm (.-href (.pathToFileURL url plans-module)))
      (.then (fn [m] ((.-createPlansService m) #js {:stateDir (.join path root "state")})))))

(defonce plans-service (delay (load-plans-service)))

(defn send-plans! [res world-name plan-name]
  (-> @plans-service
      (.then (fn [svc]
               (if-not plan-name
                 (send-json-js! res 200 (.list svc world-name))
                 (if-let [plan (.get svc world-name plan-name)]
                   (send-json-js! res 200 plan)
                   (send-json! res 404 {:error (str "no plan called " plan-name " in " world-name)})))))
      (.catch (fn [e] (when-not (.-headersSent res) (send-json! res 500 {:error (str (ex-message e))}))))))

(defn handle-world-scoped! [res kind world-name query plan-name]
  (case kind
    :state (send-json! res 200 (snapshot world-name))
    :plans-api (send-plans! res world-name nil)
    :plan-api (send-plans! res world-name plan-name)
    :chat (send-json! res 200 (chat-log (chat/chat-limit (.get query "limit")) world-name))
    :world (send-json! res 501 {:error "unsupported for engine bodies: world scan (needs a body's HTTP API)"})
    :villages-api (let [snap (village-snapshot (filterv #(= world-name (:name %)) (read-worlds)))]
                    (send-json-js! res 200 (doto (to-js (select-keys snap [:error])) (aset "villages" (:villages snap)) (aset "readOnly" true))))))

(def world-kinds #{:state :chat :world :villages-api :plans-api :plan-api})

(defn handle! [req res]
  (let [{:keys [kind] blueprint-name :name request-path :path} (routes/route (.-url req))
        query (.-searchParams (js/URL. (.-url req) "http://dashboard"))
        choice (when (world-kinds kind) (choose-world query))]
    (cond
      (:error choice) (send-json! res 400 choice)
      (world-kinds kind) (handle-world-scoped! res kind (:name choice) query blueprint-name)
      :else
      (case kind
        :page (send-file! res (.join path public-dir "index.html"))
        :static (serve-static! res request-path)
        :thumb (send-thumb! res blueprint-name)
        :item-icon (send-item-icon! res blueprint-name)
        :events (send-events! res blueprint-name query)
        :chat-send (send-chat! req res)
        :jobs-api (send-json! res 200 {:at (js/Date.now) :jobs (read-jobs (js/Date.now))})
        :thumbs-stats (send-thumbs-stats! res)
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
    (if (view-request? req)
      (.handle @view-mount req res)
      (handle! req res))
    (catch :default e
      (if (.-headersSent res)
        (.end res)
        (send-json! res 500 {:error (str (ex-message e))})))))

(defn main []
  (when-not (chat-send/valid-sender? chat-sender)
    (js/console.error (str "DASHBOARD_CHAT_AS must match " chat-send/sender-re ", got " (pr-str chat-sender)))
    (.exit js/process 1))
  (let [server (.createServer http handler)]
    (-> (load-view-mount)
        (.then (fn [_]
                 (.listen server port "127.0.0.1"
                          #(println (str "dashboard on http://127.0.0.1:" port " (root " root ")"))))))))
