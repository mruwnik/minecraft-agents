(ns dashboard.server.files
  "Paths, file reading and the world list the dashboard server reads from."
  (:require [engine.bodies :as bodies]
            [dashboard.engine-events :as ee]
            ["fs" :as fs]
            ["path" :as path]
            [dashboard.shared-map :as shared-map]
            [dashboard.worlds :as worlds]))

(def repo-root (.resolve path js/__dirname ".." ".."))
(def root (or (.-DASHBOARD_ROOT js/process.env) repo-root))
(def dashboard-dir (.join path repo-root "dashboard"))
(def public-dir (.join path dashboard-dir "public"))
(def js-dir (.join path dashboard-dir "out" "public" "js"))
(def state-dir (clj->js (bodies/storage-root {} root)))
(def worlds-dir (bodies/worlds-dir state-dir))

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


(def port (js/Number (or (.-PORT js/process.env) 3701)))
