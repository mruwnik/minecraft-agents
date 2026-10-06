(ns engine.single
  "One process per body, guarded by a socket.
  - A body binds <engine dir>/body.sock when it starts, before it opens any file in its folder or logs in,
    and holds it until it stops.
  - Every connection is answered with one JSON line {pid world body}, then closed.
  - A second start connects instead of binding. An answer means a live body, so it refuses.
  - A socket left by a crashed body answers ECONNREFUSED (so does a plain file in its place, on Linux).
    That counts as 'not running' and the next start replaces it.
  - Any other error while probing refuses the start, because unknown is not 'not running'."
  (:require ["fs" :as fs]
            ["net" :as net]
            ["path" :as path]
            [clojure.string :as str]))

(def exit-code 3)

(def answer-wait-ms 1000)

(defn socket-path [engine-dir] (path/join engine-dir "body.sock"))

(def stale-codes #{"ECONNREFUSED" "ENOENT"})

(defn parse-answer [text]
  (try (js->clj (js/JSON.parse text) :keywordize-keys true)
       (catch :default _ {})))

(defn probe
  "Connect to sock. Resolves to {:running info} when something listens (info {} when it gave no readable answer),
  to :free when the file is absent or stale; rejects on any other error."
  [sock]
  (js/Promise.
   (fn [resolve reject]
     (let [chunks (atom "")
           client (net/createConnection #js {:path sock})
           settled? (atom false)
           finish! (fn [v] (when-not @settled? (reset! settled? true) (.destroy client) (resolve v)))]
       (.setTimeout client answer-wait-ms)
       (.on client "data" (fn [d] (swap! chunks str d)))
       (.on client "end" (fn [] (finish! {:running (parse-answer @chunks)})))
       (.on client "timeout" (fn [] (finish! {:running (parse-answer @chunks)})))
       (.on client "error" (fn [e]
                             (cond
                               @settled? nil
                               (stale-codes (.-code e)) (finish! :free)
                               :else (do (reset! settled? true) (reject e)))))))))

(defn listen!
  "Bind sock and answer every connection with info. Resolves to the server, or :in-use when someone else bound first."
  [sock info]
  (js/Promise.
   (fn [resolve reject]
     (fs/mkdirSync (path/dirname sock) #js {:recursive true})
     (let [server (net/createServer (fn [conn]
                                      (.on conn "error" (fn [_])) ; a client that resets early must not crash the body
                                      (.end conn (str (js/JSON.stringify (clj->js info)) "\n"))))]
       (.once server "error" (fn [e] (if (= "EADDRINUSE" (.-code e)) (resolve :in-use) (reject e))))
       (.listen server sock (fn []
                              (try (fs/chmodSync sock 384)
                                   (resolve server)
                                   (catch :default e (reject e)))))))))

(defn releaser [server sock]
  (let [done? (atom false)]
    (fn []
      (js/Promise.
       (fn [resolve _]
         (if @done?
           (resolve true)
           (do (reset! done? true)
               (.close server (fn [] (fs/rmSync sock #js {:force true}) (resolve true))))))))))

(def lock-stale-ms 5000)

(defn start-time
  "Start time of pid (field 22 of /proc/<pid>/stat), or nil where /proc is unavailable. Guards against pid reuse."
  [pid]
  (try (let [stat (fs/readFileSync (str "/proc/" pid "/stat") "utf8")]
         (nth (str/split (subs stat (inc (str/last-index-of stat ")"))) #"\s+") 20 nil))
       (catch :default _ nil)))

(defn holder-file [lock] (path/join lock "holder"))

(defn write-holder!
  "Record this process (pid and start time) in lock."
  [lock]
  (fs/writeFileSync (holder-file lock)
                    (str/join " " (remove nil? [js/process.pid (start-time js/process.pid)]))))

(defn pid-alive? [pid]
  (try (.kill js/process pid 0) true
       (catch :default e (= "EPERM" (.-code e)))))

(defn lock-stale?
  "A lock is stale when the process recorded in it is gone (dead pid, or a pid now running since another start
  time). A lock with no readable holder file is a starter that crashed between mkdir and the write, or is about
  to write: stale only once older than lock-stale-ms."
  [lock]
  (let [holder (try (fs/readFileSync (holder-file lock) "utf8") (catch :default _ nil))]
    (if-let [[pid started] (some-> holder str/trim not-empty (str/split #"\s+"))]
      (let [pid (js/parseInt pid 10)]
        (or (not (pid-alive? pid))
            (boolean (and started (not= started (start-time pid))))))
      (try (> (- (js/Date.now) (.-mtimeMs (fs/statSync lock))) lock-stale-ms)
           (catch :default _ false)))))

(defn reclaim-stale!
  "Remove lock if it is stale: rename it to a unique name (atomic: one of several waiters wins), then delete that.
  Never put it back; a live holder's lock is never judged stale, so only a crashed holder's is renamed."
  [lock]
  (when (lock-stale? lock)
    (let [tomb (str lock ".dead-" js/process.pid "-" (js/Date.now) "-" (rand-int 1000000))]
      (when (try (fs/renameSync lock tomb) true (catch :default _ false))
        (fs/rmSync tomb #js {:recursive true :force true})))))

(defn ^:async with-replace-lock
  "Run (f) while holding <sock>.lock, a directory made atomically: replacing a stale socket is probe, rm, listen,
  and two starts doing that at once would both bind. The holder's pid is written into the lock; a lock is
  reclaimed only when that process is dead (see lock-stale?)."
  [sock f]
  (let [lock (str sock ".lock")]
    (loop []
      (let [made? (try (fs/mkdirSync lock) true
                       (catch :default e (if (= "EEXIST" (.-code e)) false (throw e))))]
        (cond
          made? (try (write-holder! lock) (catch :default _ nil))
          (lock-stale? lock) (do (reclaim-stale! lock) (recur))
          :else (do (await (js/Promise. (fn [r] (js/setTimeout r 20)))) (recur)))))
    (try (await (f))
         (finally (fs/rmSync lock #js {:recursive true :force true})))))

(defn ^:async claim!
  "Take the body's place at sock, naming info to anyone who asks. Resolves to {:held release} (release is a thunk
  returning a promise) or {:running info} when a live process already holds it."
  [sock info]
  (let [seen (if (fs/existsSync sock) (await (probe sock)) :free)]
    (if (not= :free seen)
      seen
      (await
       (with-replace-lock
         sock
         (fn ^:async replace! []
           (let [again (if (fs/existsSync sock) (await (probe sock)) :free)]
             (if (not= :free again)
               again
               (do
                 (fs/rmSync sock #js {:force true})
                 (let [server (await (listen! sock info))]
                   (if (= :in-use server)
                     (let [again (await (probe sock))]
                       (if (= :free again) (throw (js/Error. (str "cannot take " sock))) again))
                     {:held (releaser server sock)})))))))))))

(defn refusal
  "The one line printed when a start is refused."
  [world body sock {:keys [pid]}]
  (str "engine: body " body " in world " world " is already running ("
       (if pid (str "pid " pid ", it answered") "something answered")
       " on " sock "); not starting a second one. Stop that process first; --fresh does not override this."))
