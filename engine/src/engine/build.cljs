(ns engine.build
  "Which engine build this body runs: the commit build-id.edn (written beside the bundle by tools/compile)
  names, and when this process started."
  (:require [cljs.reader :as reader]
            ["fs" :as fs]
            ["path" :as path]))

(defn read-commit
  "The :commit in dir/build-id.edn, or nil when the build wrote none."
  [dir]
  (let [file (path/join dir "build-id.edn")]
    (when (fs/existsSync file)
      (:commit (reader/read-string (fs/readFileSync file "utf8"))))))

(def started (js/Date.now))

(def commit (delay (read-commit js/__dirname)))

(defn info [] {:commit @commit :started-at started})

(defn status-extra
  "A status extra (event-api/status): the build under :build."
  [_view]
  (info))
