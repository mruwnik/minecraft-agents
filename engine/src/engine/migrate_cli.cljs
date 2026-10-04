(ns engine.migrate-cli
  "Command line for engine.migrate: node out/migrate.cjs --world <world> [--dry-run]
  [--state-dir <dir>] Name...

  Reads state/worlds/<world>/agents/<Name>/config.json and events.jsonl and the world's
  places.json, converts, and prints one line per body (--dry-run) or writes
  engine/memory.edn and view/pose.json. It writes only where engine/ is
  absent, never overwrites a file, never moves or deletes, and refuses a body
  that looks connected (engine/control.sock, a live body.pid, or something
  listening on 127.0.0.1:<apiPort>). The IO lives here, the logic in
  engine.migrate."
  (:require [clojure.string :as str]
            [engine.bodies :as bodies]
            [engine.fsutil :as fsu]
            [engine.memory :as mem]
            [engine.migrate :as migrate]
            ["fs" :as fs]
            ["net" :as net]
            ["path" :as path]))

;; ------------------------------------------------------------------ pure

(defn parse-args [args]
  (loop [[a & more] args, opts {:dry-run false :state-dir nil :world nil :names []}]
    (cond
      (and (nil? a) (nil? (:world opts))) {:error (bodies/missing-world-error "--world")}
      (nil? a) opts
      (= a "--dry-run") (recur more (assoc opts :dry-run true))
      (= a "--world") (if (empty? more)
                        {:error (bodies/missing-world-error "--world")}
                        (recur (rest more) (assoc opts :world (first more))))
      (= a "--state-dir") (if (empty? more)
                            {:error "--state-dir needs a directory"}
                            (recur (rest more) (assoc opts :state-dir (first more))))
      (str/starts-with? a "--") {:error (str "unknown option " a)}
      :else (recur more (update opts :names conj a)))))

(defn fmt-pos [{:keys [x y z]}] (str x " " y " " z))

(defn summary-line
  "One line for a convert result: files it would write, entries, the bed
  chosen and the beds dropped, the pose, skipped files."
  [{:keys [name memory pose beds chests beds-dropped chests-dropped skipped]}]
  (let [files (cond-> []
                memory (conj "engine/memory.edn")
                pose (conj "view/pose.json"))
        bed (get-in memory [:entries :bed 0 :data :pos])]
    (str name ": "
         (if (seq files) (str "writes " (str/join ", " files)) "writes nothing")
         "; beds " beds ", chests " chests
         (when bed (str "; bed " (fmt-pos bed)))
         (when (seq beds-dropped) (str ", dropped " (count beds-dropped) " (" (str/join "; " (map fmt-pos beds-dropped)) ")"))
         (when (seq chests-dropped) (str "; chests dropped " (count chests-dropped) " (" (str/join "; " (map fmt-pos chests-dropped)) ")"))
         "; " (if pose (str "pose " (fmt-pos (:pos pose))) "no pose")
         (when (seq skipped)
           (str "; skipped " (str/join ", " (map #(str (:file %) " (" (:error %) ")") skipped)))))))

;; ------------------------------------------------------------------ io

(defn read-text [file]
  (when (fs/existsSync file) (fs/readFileSync file "utf8")))

(defn parse-json-file [file]
  (when-let [text (read-text file)]
    (try (js->clj (js/JSON.parse text) :keywordize-keys true)
         (catch :default e {:parse-error (ex-message e)}))))

(defn parse-jsonl-file [file]
  (when-let [text (read-text file)]
    (let [lines (remove (comp str/blank? second) (map-indexed (fn [i l] [(inc i) l]) (str/split-lines text)))
          parsed (map (fn [[n l]]
                        (try {:ok (js->clj (js/JSON.parse l) :keywordize-keys true)}
                             (catch :default e {:error (str "line " n ": " (ex-message e))})))
                      lines)]
      (if-let [bad (first (filter :error parsed))]
        {:parse-error (:error bad)}
        (mapv :ok parsed)))))

(defn mtime-ms [file] (when (fs/existsSync file) (.-mtimeMs (fs/statSync file))))

(defn pid-alive? [pid]
  (try (js/process.kill pid 0) true
       (catch :default e (= "EPERM" (.-code e)))))

(defn live-pid-file? [file]
  (when-let [pid (some-> (read-text file) str/trim js/parseInt)]
    (and (js/Number.isInteger pid) (pos? pid) (pid-alive? pid))))

(defn listening? [port]
  (js/Promise.
   (fn [resolve _]
     (let [sock (net/connect #js {:host "127.0.0.1" :port port})
           done (fn [v] (.destroy sock) (resolve v))]
       (.setTimeout sock 500)
       (.on sock "connect" #(done true))
       (.on sock "timeout" #(done false))
       (.on sock "error" #(done false))))))

(defn connected-reason
  "A promise of why the body looks connected, or nil."
  [agent-dir port]
  (let [sock (path/join agent-dir "engine" "control.sock")]
    (cond
      (fs/existsSync sock) (js/Promise.resolve "engine/control.sock exists")
      (or (live-pid-file? (path/join agent-dir "body.pid")) (live-pid-file? (path/join agent-dir "engine" "body.pid")))
      (js/Promise.resolve "body.pid names a live process")
      (not (number? port)) (js/Promise.resolve nil)
      :else (.then (listening? port) #(when % (str "something listens on 127.0.0.1:" port))))))

(defn write-new! [file write!]
  (if (fs/existsSync file)
    (str "kept existing " file)
    (do (write!) (str "wrote " file))))

(defn load-body [state-dir world name]
  (let [agent-dir (bodies/body-dir state-dir world name)
        raw (parse-json-file (path/join agent-dir "config.json"))
        ;; the world is where the folder is, not a field of the config
        config (if (and (map? raw) (not (:parse-error raw))) (assoc raw :world world) raw)
        places-file (path/join (bodies/worlds-dir state-dir) world "places.json")]
    {:agent-dir agent-dir
     :input {:name name
             :config config
             :places (some-> places-file parse-json-file)
             :events (parse-jsonl-file (path/join agent-dir "events.jsonl"))
             :places-mtime (some-> places-file mtime-ms)
             :now (js/Date.now)}}))

(defn write-body!
  "Write what convert produced under agent-dir; returns lines describing it."
  [agent-dir {:keys [memory pose]}]
  (cond-> []
    memory (conj (write-new! (path/join agent-dir "engine" "memory.edn")
                             #(fsu/write-edn! (path/join agent-dir "engine" "memory.edn") memory)))
    pose (conj (write-new! (path/join agent-dir "view" "pose.json")
                           #(fsu/write-atomic! (path/join agent-dir "view" "pose.json")
                                               (js/JSON.stringify (clj->js pose)))))))

(defn migrate-body
  "A promise of the output lines for one body."
  [state-dir world dry-run name]
  (let [{:keys [agent-dir input]} (load-body state-dir world name)
        result (migrate/convert input)
        port (get-in input [:config :apiPort])]
    (cond
      (not (fs/existsSync agent-dir)) (js/Promise.resolve [(str name ": no such agent folder")])
      dry-run (js/Promise.resolve [(if (fs/existsSync (path/join agent-dir "engine"))
                                     (str name ": has engine/, left alone")
                                     (summary-line result))])
      :else (.then (connected-reason agent-dir port)
                   (fn [why]
                     (cond
                       why [(str name ": refused, " why "; nothing written")]
                       (fs/existsSync (path/join agent-dir "engine")) [(str name ": has engine/, left alone")]
                       :else (into [(summary-line result)] (map #(str "  " %) (write-body! agent-dir result)))))))))

(defn default-names [state-dir world]
  (let [agents (path/join (bodies/worlds-dir state-dir) world "agents")]
    (->> (fs/readdirSync agents)
         sort
         (filter #(and (.isDirectory (fs/statSync (path/join agents %)))
                       (not (fs/existsSync (path/join agents % "engine"))))))))

(defn migrate-all
  "A promise of the output lines for names, one body after another (a refused
  body does not stop the rest)."
  [state-dir world names dry-run]
  (reduce (fn [p name]
            (.then p (fn [lines]
                       (-> (migrate-body state-dir world dry-run name)
                           (.then #(into lines %))))))
          (js/Promise.resolve [])
          names))

(defn main [& args]
  (let [{:keys [error dry-run state-dir world names]} (parse-args args)]
    (when error
      (.write js/process.stderr (str "migrate: " error "\nusage: node out/migrate.cjs --world <world> [--dry-run] [--state-dir <dir>] Name...\n"))
      (js/process.exit 2))
    (let [state-dir (path/resolve (or state-dir (path/join js/__dirname ".." ".." "state")))
          names (if (seq names) names (default-names state-dir world))]
      (-> (migrate-all state-dir world names dry-run)
          (.then (fn [lines] (doseq [l lines] (println l))))
          (.catch (fn [e] (.write js/process.stderr (str "migrate: " (.-stack e) "\n")) (js/process.exit 1)))))))
