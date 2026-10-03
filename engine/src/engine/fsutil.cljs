(ns engine.fsutil
  "Small synchronous file helpers: JSON reads (agent config) and atomic EDN writes."
  (:require ["fs" :as fs]
            ["path" :as path]
            [cljs.reader :as reader]))

(defn write-atomic! [file text]
  (fs/mkdirSync (path/dirname file) #js {:recursive true})
  (let [tmp (str file ".tmp")]
    (fs/writeFileSync tmp text)
    (fs/renameSync tmp file)))

(defn read-json [file]
  (when (fs/existsSync file)
    (js->clj (js/JSON.parse (fs/readFileSync file "utf8")) :keywordize-keys true)))

(defn write-edn!
  "Write data to file atomically as EDN. Returns {:bytes n :ms elapsed}, the
  size written and the wall time the write took."
  [file data]
  (let [text (pr-str data)
        start (js/performance.now)]
    (write-atomic! file text)
    {:bytes (js/Buffer.byteLength text "utf8") :ms (- (js/performance.now) start)}))

(defn read-edn [file]
  (when (fs/existsSync file)
    (reader/read-string (fs/readFileSync file "utf8"))))
