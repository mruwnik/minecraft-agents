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
            ["path" :as path]))

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

(defn lock-stale? [lock]
  (try (> (- (js/Date.now) (.-mtimeMs (fs/statSync lock))) lock-stale-ms)
       (catch :default _ false)))

(defn reclaim-stale!
  "Remove lock if it is stale. Renames it to a unique name first (atomic: one of several waiters wins), then
  re-checks the moved dir: a waiter that judged the lock stale just before another reclaimed and re-made it
  would otherwise remove that fresh lock; a fresh one is put back."
  [lock]
  (let [tomb (str lock ".dead-" js/process.pid "-" (js/Date.now) "-" (rand-int 1000000))]
    (when (try (fs/renameSync lock tomb) true (catch :default _ false))
      (when-not (lock-stale? tomb)
        (try (fs/renameSync tomb lock) (catch :default _ nil)))
      (fs/rmSync tomb #js {:recursive true :force true}))))

(defn ^:async with-replace-lock
  "Run (f) while holding <sock>.lock, a directory made atomically: replacing a stale socket is probe, rm, listen,
  and two starts doing that at once would both bind. A lock older than lock-stale-ms is a crashed starter's; the holder touches it while f runs."
  [sock f]
  (let [lock (str sock ".lock")]
    (loop [tries 0]
      (let [made? (try (fs/mkdirSync lock) true
                       (catch :default e (if (= "EEXIST" (.-code e)) false (throw e))))]
        (cond
          made? nil
          (lock-stale? lock)
          (do (reclaim-stale! lock) (recur tries))
          :else (do (await (js/Promise. (fn [r] (js/setTimeout r 20)))) (recur (inc tries))))))
    (let [beat (js/setInterval #(try (let [t (/ (js/Date.now) 1000)] (fs/utimesSync lock t t)) (catch :default _ nil))
                               (/ lock-stale-ms 5))]
      (try (await (f))
           (finally (js/clearInterval beat)
                    (fs/rmSync lock #js {:recursive true :force true}))))))

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
