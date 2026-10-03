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

(defn write-edn! [file data]
  (write-atomic! file (pr-str data)))

(defn read-edn [file]
  (when (fs/existsSync file)
    (reader/read-string (fs/readFileSync file "utf8"))))
