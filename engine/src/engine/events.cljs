(ns engine.events
  "Canonical EDN event stream, bounded rolling file appender, and local cursor reader."
  (:require [engine.args :as a]
            [clojure.string :as str]
            [cljs.reader :as reader]
            [engine.edn-fast :as fast]
            [engine.settings :as settings]
            ["crypto" :as crypto]
            ["fs" :as fs]
            ["path" :as path]))

(a/defargs settings
  {:engine.events/max-bytes {:default (* 64 1024 1024) :spec (a/int-in 1024 nil)
                             :doc "The event log's size cap over the active and rotated segments, bytes."}
   :engine.events/debug-max-bytes {:default (* 128 1024 1024) :spec (a/int-in 1024 nil)
                                   :doc "Size cap of the separate chain that holds :debug events, bytes; the main chain keeps everything else."}
   :engine.events/debug-max-age-ms {:default (* 3 60 60 1000) :spec (a/int-in 1000 nil)
                                    :doc "How long :debug events are kept, ms."}
   :engine.events/recent-count {:default 256 :spec (a/int-in 1 nil) :doc "Events kept in memory for the live reader; an older cursor reads from disk."}
   :engine.events/recent-bytes {:default (* 256 1024) :spec (a/int-in 1 nil) :doc "Bytes of events kept in memory for the live reader."}
   :engine.events/default-page-size {:default 256 :spec (a/int-in 1 nil) :doc "Events a read returns when it names no limit."}
   :engine.events/max-page-size {:default 2000 :spec (a/int-in 1 nil) :doc "The most events one read returns."}
   :engine.events/max-page-bytes {:default (* 1024 1024) :spec (a/int-in 1 nil) :doc "Bytes of disk events one read parses before it stops (at least one event is always returned)."}
   :engine.events/max-line-bytes {:default (* 16 1024) :spec (a/int-in 1 nil) :doc "An event line longer than this many characters is not parsed on read; the reader returns a stub with its seq, time, source and kind."}
   :engine.events/max-id-chars {:default 256 :spec (a/int-in 8 nil) :doc "Longest job id or chain element an event keeps; the middle of a longer one is cut."}
   :engine.events/max-chain {:default 8 :spec (a/int-in 3 nil) :doc "Most chain elements an event keeps; the middle of a longer chain is cut."}})

(defn setting [k] (settings/get settings (keyword "engine.events" k)))

(def min-max-bytes (get-in settings [:engine.events/max-bytes :min]))
(def segment-count 4) ; active + three rotated segments

(defn random-id [] (.randomUUID crypto))

(defn metadata-file [file]
  (str file ".meta.edn"))

(defn segment-file [file n]
  (if (zero? n) file (str file "." n)))

(defn debug-file [file]
  (str file ".debug"))

(defn segment-files [file]
  (mapv #(segment-file file %) (range segment-count)))

(defn read-edn-file [file]
  (when (fs/existsSync file)
    (reader/read-string (fs/readFileSync file "utf8"))))

(defn write-atomic! [file text]
  (fs/mkdirSync (path/dirname file) #js {:recursive true})
  (let [tmp (str file ".tmp")]
    (fs/writeFileSync tmp text)
    (fs/renameSync tmp file)))

(defn write-meta! [file meta]
  (write-atomic! (metadata-file file) (str (pr-str meta) "\n")))

(defn top-level
  "The text of a record line outside its nested collections and strings, one pass: enough to find the scalar fields
  without building the line's data."
  [line]
  (let [n (count line)]
    (loop [i 0 depth 0 in-str? false out (array)]
      (if (>= i n)
        (.join out "")
        (let [ch (.charAt line i)]
          (cond
            in-str? (recur (if (= ch "\\") (+ i 2) (inc i)) depth (not= ch "\"") out)
            (= ch "\"") (recur (inc i) depth true out)
            (contains? #{"{" "[" "("} ch) (recur (inc i) (inc depth) false (do (when (= depth 0) (.push out ch)) out))
            (contains? #{"}" "]" ")"} ch) (recur (inc i) (dec depth) false (do (when (= depth 1) (.push out ch)) out))
            :else (recur (inc i) depth false (do (when (= depth 1) (.push out ch)) out))))))))

(defn stub-record
  "A record for a line too long to parse: the scalar :seq, :time-ms, :source, :kind and :level read off the line, and
  :data {:oversize-chars n}. nil when the line has no :seq."
  [line]
  (let [flat (top-level line)
        field (fn [k re f] (some->> (re-find re flat) second f (vector k)))
        kw #(keyword %)
        n (second (re-find #":seq (\d+)" flat))]
    (when n
      (into {:seq (js/parseInt n) :truncated true :data {:oversize-chars (count line)}}
            (keep identity)
            [(field :time-ms #":time-ms (\d+)" js/parseInt)
             (field :source #":source :([^\s,{}\[\]()]+)" kw)
             (field :kind #":kind :([^\s,{}\[\]()]+)" kw)
             (field :level #":level :([^\s,{}\[\]()]+)" kw)]))))

(defn seq-of-record [line file line-no]
  (let [record (try
                 (if (> (count line) (setting "max-line-bytes"))
                   (or (stub-record line) (reader/read-string line))
                   (try (fast/read-string line) (catch :default _ (reader/read-string line))))
                 (catch :default e
                   (throw (ex-info (str "malformed complete event record in " file
                                        " at line " line-no ": " (.-message e))
                                   {:file file :line line-no}))))
        n (:seq record)]
    (when-not (and (map? record) (integer? n) (pos? n))
      (throw (ex-info (str "invalid event record in " file " at line " line-no
                           ": expected a map with a positive integer :seq")
                      {:file file :line line-no})))
    record))

(def read-chunk-bytes (* 64 1024))

(defn read-bytes
  "Up to len bytes of the file behind fd from position pos; empty at or past the end."
  [fd pos len]
  (let [buf (js/Buffer.alloc len)
        n (fs/readSync fd buf 0 len pos)]
    (.subarray buf 0 n)))

(def line-probe-bytes 512)

(defn line-end
  "Byte offset of the newline ending the line that holds position pos, or size.
  Reads small first, doubling up to a chunk, so a probe stays cheap."
  [fd size pos]
  (loop [at pos len line-probe-bytes]
    (if (>= at size)
      size
      (let [buf (read-bytes fd at len)
            i (.indexOf buf 10)]
        (cond
          (zero? (.-length buf)) size
          (neg? i) (recur (+ at (.-length buf)) (min read-chunk-bytes (* 2 len)))
          :else (+ at i))))))

(defn record-at
  "The first non-blank line starting at or after byte offset start:
  {:start :end :record}, or nil at the end of the file."
  [fd size file start]
  (loop [at start]
    (when (< at size)
      (let [end (line-end fd size at)
            line (.toString (read-bytes fd at (- end at)) "utf8")]
        (if (str/blank? line)
          (recur (inc end))
          {:start at :end end :record (seq-of-record line file (str "(byte " at ")"))})))))

(defn line-start
  "Byte offset of the start of the line that ends just before byte offset end.
  Reads backwards, small first, doubling up to a chunk."
  [fd end]
  (loop [at end len line-probe-bytes]
    (let [from (max 0 (- at len))
          i (.lastIndexOf (read-bytes fd from (- at from)) 10)]
      (cond
        (not (neg? i)) (+ from i 1)
        (zero? from) 0
        :else (recur from (min read-chunk-bytes (* 2 len)))))))

(defn last-record
  "The last non-blank line of the file behind fd: {:start :end :record}, or nil."
  [fd size file]
  (loop [end size]
    (when (pos? end)
      (let [content-end (if (= 10 (aget (read-bytes fd (dec end) 1) 0)) (dec end) end)
            start (line-start fd content-end)
            line (.toString (read-bytes fd start (- content-end start)) "utf8")]
        (if (str/blank? line)
          (recur start)
          {:start start :end content-end :record (seq-of-record line file (str "(byte " start ")"))})))))

(defn with-fd [file f]
  (let [fd (fs/openSync file "r")]
    (try (f fd (.-size (fs/fstatSync fd)))
         (finally (fs/closeSync fd)))))

(defn repair-active-tail!
  "Cut an incomplete last line (no newline) off file. Reads only the tail."
  [file]
  (when (fs/existsSync file)
    (let [keep (with-fd file (fn [fd size]
                               (when (and (pos? size) (not= 10 (aget (read-bytes fd (dec size) 1) 0)))
                                 (line-start fd size))))]
      (when keep (fs/truncateSync file keep)))))

(defn scan-segment
  "{:file :bytes :first-seq :last-seq} of a segment. Parses only its first and last record."
  [file]
  (if-not (fs/existsSync file)
    {:file file :bytes 0 :first-seq nil :last-seq nil}
    (with-fd file
      (fn [fd size]
        (let [first-seq (some-> (record-at fd size file 0) :record :seq)
              last-seq (some-> (last-record fd size file) :record :seq)]
          (when (and first-seq (< last-seq first-seq))
            (throw (ex-info (str "event sequence is not increasing in " file)
                            {:file file :first-seq first-seq :last-seq last-seq})))
          {:file file :bytes size :first-seq first-seq :last-seq last-seq})))))

(defn scan-segments [file]
  (mapv scan-segment (reverse (segment-files file)))) ; oldest to active

(defn copy-tail!
  "Replace file by its bytes from offset start on, copied a chunk at a time."
  [file fd size start]
  (let [tmp (str file ".tmp")
        out (fs/openSync tmp "w")]
    (try
      (loop [at start]
        (when (< at size)
          (let [buf (read-bytes fd at read-chunk-bytes)]
            (fs/writeSync out buf)
            (recur (+ at (.-length buf))))))
      (finally (fs/closeSync out)))
    (fs/renameSync tmp file)))

(defn trim-active!
  "Keep the newest whole lines of file within max-bytes. A file under the cap is not read."
  [file max-bytes]
  (when (> (.-size (fs/statSync file)) max-bytes)
    (with-fd file
      (fn [fd size]
        (let [cut (inc (line-end fd size (dec (- size max-bytes))))
              first-dropped (some-> (record-at fd size file 0) :record :seq)
              first-kept (some-> (record-at fd size file cut) :record :seq)]
          (.write js/process.stderr
                  (str "event retention: dropped complete records seq " first-dropped " to "
                       (if first-kept (dec first-kept) "the end") " (" cut " bytes) while enforcing "
                       max-bytes " byte cap\n"))
          (copy-tail! file fd size cut)))))
  (scan-segment file))

(defn ensure-valid-max-bytes! [n]
  (when-not (and (integer? n) (<= min-max-bytes n))
    (throw (ex-info (str ":max-bytes must be an integer at least " min-max-bytes)
                    {:max-bytes n})))
  n)

(defn age-out!
  "Drop the segments of a chain whose newest line is older than max-age-ms (file mtime); an old active one is emptied."
  [file max-age-ms]
  (let [cutoff (- (js/Date.now) max-age-ms)]
    (doseq [f (segment-files file)
            :when (and (fs/existsSync f) (< (.-mtimeMs (fs/statSync f)) cutoff))]
      (if (= f file)
        (fs/writeFileSync f "")
        (fs/rmSync f #js {:force true})))))

(defn initial-meta [file]
  (let [mf (metadata-file file)
        m (read-edn-file mf)]
    (when (and m (not (and (string? (:stream-id m))
                           (integer? (:last-seq m))
                           (not (neg? (:last-seq m))))))
      (throw (ex-info (str "invalid event metadata " mf) {:file mf})))
    (or m {:stream-id (random-id) :last-seq 0})))

(defn bounded-page-size [n]
  (if (and (integer? n) (pos? n)) (min n (setting "max-page-size")) (setting "default-page-size")))

(defn bytes-of [text]
  (js/Buffer.byteLength text "utf8"))

(defn cap-id
  "An id of at most m (default: the max-id-chars setting) characters: a longer one keeps its head and its tail around \"...\"."
  ([id] (cap-id id (setting "max-id-chars")))
  ([id m]
   (if (and (string? id) (> (count id) m))
     (str (subs id 0 (quot m 2)) "..." (subs id (- (count id) (quot m 2))))
     id)))

(defn cap-chain
  "A chain of at most max-chain elements, each capped: a longer one keeps its first two and its last elements around \"...\"."
  [chain]
  (let [m (setting "max-chain")
        id-chars (setting "max-id-chars")
        cap #(cap-id % id-chars)]
    (cond
      (not (sequential? chain)) (cap chain)
      (> (count chain) m) (mapv cap (concat (take 2 chain) ["..."] (take-last (- m 3) chain)))
      :else (mapv cap chain))))

(def known-keys
  #{:seq :generation-id :run-id :time-ms :t :body :source :kind :level
    :context :data :message :text :attention :request-id :job :round
    :chain :reflex :action-id :cause :cause-seq :pos})

(defn canonical-event
  "The canonical event for internal event map event, numbered n. :body is dropped,
  :level is kept. Other fields outside the known set fold into :data."
  [{:keys [generation-id run-id now pos-fn]} n event]
  (let [source (:source event)
        kind (:kind event)
        old-context (or (:context event) {})
        context (cond-> old-context
                  (some? (:job event)) (assoc :job-id (cap-id (:job event)))
                  (some? (:round event)) (assoc :round (:round event))
                  (some? (:chain event)) (assoc :chain (cap-chain (:chain event)))
                  (some? (:reflex event)) (assoc :reflex-id (:reflex event))
                  (some? (:action-id event)) (assoc :action-id (:action-id event))
                  (number? (:cause event)) (assoc :cause-seq (:cause event))
                  (some? (:cause-seq event)) (assoc :cause-seq (:cause-seq event)))
        ;; Emitters pass absent optional fields as nil: drop those.
        ;; A non-numeric :cause is the body's own datum (died/hurt: "lava"), not a cause sequence.
        extra (cond-> (reduce-kv (fn [m k v] (if (or (nil? v) (contains? known-keys k)) m (assoc m k v))) {} event)
                (and (some? (:cause event)) (not (number? (:cause event)))) (assoc :cause (:cause event)))
        ;; Job debug events (state writes, child starts) carry the job id; their position doubled the debug log's size.
        ;; A heard line's place is the speaker's (given by the emitter when seen), never the listener's.
        pos (or (:pos event) (when (and pos-fn
                                        (not (and (= :job source) (= :debug (:level event))))
                                        (not (#{:chat :whisper} kind)))
                               (pos-fn)))
        data (cond-> (merge (or (:data event) {}) extra)
               (and pos (nil? (get (or (:data event) {}) :pos)))
               (assoc :pos (select-keys pos [:x :y :z])))
        started? (and (= source :system) (#{:started :restored} kind))]
    (when-not (and (keyword? source) (keyword? kind))
      (throw (ex-info "event requires keyword :source and :kind" {:source source :kind kind})))
    (cond-> {:seq n
             :generation-id generation-id
             :time-ms (or (:time-ms event) (:t event) (now))
             :source source
             :kind kind}
      (seq context) (assoc :context context)
      (seq data) (assoc :data data)
      (or (:message event) (:text event)) (assoc :message (or (:message event) (:text event)))
      (:level event) (assoc :level (:level event))
      (:attention event) (assoc :attention (:attention event))
      (:request-id event) (assoc :request-id (:request-id event))
      (and started? run-id) (assoc :run-id run-id))))

(defn segment-target [max-bytes]
  (max 1 (js/Math.floor (/ max-bytes segment-count))))

(defn rotate! [file]
  (let [active (segment-file file 0)]
    (when (and (fs/existsSync active) (pos? (.-size (fs/statSync active))))
      (let [oldest (segment-file file (dec segment-count))]
        (fs/rmSync oldest #js {:force true})
        (doseq [n (range (- segment-count 2) 0 -1)]
          (let [from (segment-file file n)
                to (segment-file file (inc n))]
            (when (fs/existsSync from) (fs/renameSync from to))))
        (fs/renameSync active (segment-file file 1))
        (fs/writeFileSync active "")))))

(defn prune-segments! [file max-bytes]
  (let [paths (segment-files file)
        total (fn [] (reduce + 0 (map #(if (fs/existsSync %) (.-size (fs/statSync %)) 0) paths)))]
    (loop [n (dec segment-count)]
      (when (and (> (total) max-bytes) (pos? n))
        (fs/rmSync (segment-file file n) #js {:force true})
        (recur (dec n))))))

(defn existing-segment-index [segments]
  (->> segments
       (filter #(fs/existsSync (:file %)))
       (mapv #(assoc % :bytes (.-size (fs/statSync (:file %)))))))

(defn update-active-segment [segments file n line-bytes]
  (let [active (segment-file file 0)
        prior (last segments)
        first-seq (or (:first-seq prior) n)]
    (conj (vec (butlast segments))
          {:file active :bytes (+ (or (:bytes prior) 0) line-bytes)
           :first-seq first-seq :last-seq n :born (or (:born prior) (js/Date.now))})))

(defn rotate-segment-index [segments file]
  (let [empty-segment {:file (segment-file file 0) :bytes 0 :first-seq nil :last-seq nil}
        by-file (into {} (map (juxt :file identity) segments))
        rotated (keep (fn [n]
                        (when-let [prior (get by-file (segment-file file (dec n)))]
                          (assoc prior :file (segment-file file n))))
                      (range (dec segment-count) 0 -1))]
    (conj (vec rotated) empty-segment)))

(defn append-line!
  "Append line to file; a failed append (disk full) cuts the partial line off again (back to before bytes), so the
  next append cannot glue onto it and the log stays readable."
  [file line before]
  (try
    (fs/appendFileSync file line)
    (catch :default e
      (try (fs/truncateSync file before) (catch :default _ nil))
      (throw e))))

(def age-check-divisor 60)

(defn aged-out-lately?
  "True when the chain was age-checked less than max-age-ms/age-check-divisor ago: ageing out stats every segment, so
  appends do it at that pace instead of per line."
  [stream k max-age-ms]
  (let [now (js/Date.now)
        stale? (> (- now (get-in @stream [:aged-at k] 0)) (/ max-age-ms age-check-divisor))]
    (when stale? (swap! stream assoc-in [:aged-at k] now))
    (not stale?)))

(defn append-chain!
  "Append line to the chain at chain-file (segments under key k of the stream), rotating by size and, with max-age-ms,
  by the age of the active segment, and pruning by size and age. Sizes come from the stream's segment index, which the
  chain's own appends keep exact; files are only stat-ed when a prune or age-out may have changed them."
  [stream chain-file k cap max-age-ms n line line-bytes]
  (let [active (segment-file chain-file 0)
        prior (last (k @stream))
        active-bytes (or (:bytes prior) 0)
        born (:born prior)]
    (when (and (pos? active-bytes)
               (or (> (+ active-bytes line-bytes) (segment-target cap))
                   (and max-age-ms born (> (- (js/Date.now) born) (/ max-age-ms 3)))))
      (rotate! chain-file)
      (swap! stream update k rotate-segment-index chain-file))
    (append-line! active line (or (:bytes (last (k @stream))) 0))
    (swap! stream update k update-active-segment chain-file n line-bytes)
    (let [pruned? (when (> (reduce + 0 (map :bytes (k @stream))) cap)
                    (prune-segments! chain-file cap)
                    true)
          aged? (when (and max-age-ms (not (aged-out-lately? stream k max-age-ms)))
                  (age-out! chain-file max-age-ms)
                  true)]
      (when (or pruned? aged?)
        (swap! stream update k existing-segment-index)))))

(defn append-record! [stream event]
  (let [{:keys [file max-bytes debug-max-bytes debug-max-age-ms last-seq debug-seq stream-id generation-id run-id now pos-fn]} @stream
        n (inc last-seq)
        full (canonical-event {:generation-id generation-id :run-id run-id :now now :pos-fn pos-fn} n event)
        line (str (pr-str full) "\n")
        line-bytes (bytes-of line)
        debug? (= :debug (:level full))
        cap (if debug? debug-max-bytes max-bytes)]
    (if (> line-bytes cap)
      (do
        (.write js/process.stderr (str "event rejected: serialized record is " line-bytes
                                       " bytes, above the " cap " byte log cap\n"))
        {:error :event-too-large :bytes line-bytes :max-bytes cap})
      (do
        ;; The in-memory counter moves on even if the append fails.
        (swap! stream assoc :last-seq n :debug-seq (if debug? n debug-seq))
        (when file
          (let [layout (fn [] (mapv #(mapv :first-seq (% @stream)) [:segments :debug-segments]))
                before (layout)]
            (if debug?
              (append-chain! stream (debug-file file) :debug-segments cap debug-max-age-ms n line line-bytes)
              (append-chain! stream file :segments cap nil n line line-bytes))
            ;; Records a rotation or prune dropped may have held the high-water mark, so only then is the metadata rewritten.
            (when (not= before (layout))
              (write-meta! file {:stream-id stream-id :last-seq n :debug-seq (if debug? n debug-seq)}))))
        (let [max-count (setting "recent-count")
              byte-limit (setting "recent-bytes")
              s @stream
              recent (conj (:recent s) full)
              sizes (conj (:recent-sizes s) line-bytes)
              total (+ (:recent-byte-count s) line-bytes)
              over? (or (> (count recent) max-count) (> total byte-limit))
              ;; Evicting down to 3/4 copies the buffer once per quarter-buffer of events, not per event.
              keep-count (if over? (quot (* 3 max-count) 4) max-count)
              keep-bytes (if over? (quot (* 3 byte-limit) 4) byte-limit)
              [drop-n kept-bytes]
              (loop [i 0 total total]
                (if (and (< i (count recent))
                         (or (> (- (count recent) i) keep-count) (> total keep-bytes)))
                  (recur (inc i) (- total (nth sizes i)))
                  [i total]))]
          (swap! stream assoc
                 :recent (if (zero? drop-n) recent (into [] (subvec recent drop-n)))
                 :recent-sizes (if (zero? drop-n) sizes (into [] (subvec sizes drop-n)))
                 :recent-byte-count kept-bytes))
        (doseq [sink (:sinks @stream)]
          (try (sink full)
               (catch :default e
                 (.write js/process.stderr (str "event sink failed: " (.-message e) "\n")))))
        n))))

(defn open-chain!
  "Prepare the chain at file: repaired, aged out when max-age-ms is given. Returns its scanned segments, oldest first."
  [file max-age-ms]
  (fs/mkdirSync (path/dirname file) #js {:recursive true})
  (when-not (fs/existsSync file) (fs/writeFileSync file ""))
  (repair-active-tail! file)
  (when max-age-ms (age-out! file max-age-ms))
  (scan-segments file))

(defn enforce-chain!
  "Cut the chain at file to max-bytes; returns its segment index."
  [file scanned max-bytes]
  (prune-segments! file max-bytes)
  (let [active-info (trim-active! file max-bytes)
        base (existing-segment-index scanned)
        segments (assoc (vec base) (dec (count base)) active-info)
        retained-bytes (reduce + 0 (map :bytes segments))]
    (when (> retained-bytes max-bytes)
      (throw (ex-info (str "could not enforce configured :max-bytes " max-bytes)
                      {:file file :bytes retained-bytes :max-bytes max-bytes})))
    segments))

(defn make
  "Create a canonical EDN event stream. Options: :file, :generation-id,
  :max-bytes (aggregate active+rotated cap), :debug-max-bytes and :debug-max-age-ms (the separate :debug chain in
  <file>.debug), :stdout?, :sinks, :pos-fn, :now, and :stream-id for tests. File-backed streams use events.edn plus
  atomically replaced events.edn.meta.edn metadata. The metadata is the high-water mark: written at start and whenever an append rotates or drops records, not per event (a restart takes the larger of it and the numbers on disk)."
  [{:keys [file generation-id max-bytes debug-max-bytes debug-max-age-ms stdout? sinks pos-fn now stream-id]
    :or {max-bytes (setting "max-bytes") debug-max-bytes (setting "debug-max-bytes")
         debug-max-age-ms (setting "debug-max-age-ms") now js/Date.now pos-fn (constantly nil)
         sinks []}}]
  (ensure-valid-max-bytes! max-bytes)
  (ensure-valid-max-bytes! debug-max-bytes)
  (let [dfile (when file (debug-file file))
        scanned-segments (if file (open-chain! file nil) [])
        ;; Scanned before the age-out drops old debug records, so their numbers still count towards the high-water mark.
        debug-before-age (if file (open-chain! dfile nil) [])
        scanned-debug (if file (open-chain! dfile debug-max-age-ms) [])
        meta (if file (initial-meta file) {:stream-id (or stream-id (random-id)) :last-seq 0})
        debug-seq (max (or (:debug-seq meta) 0) (reduce max 0 (keep :last-seq debug-before-age)))
        scanned-seq (reduce max 0 (keep :last-seq (concat scanned-segments debug-before-age)))
        last-seq (max (:last-seq meta) scanned-seq)
        sid (or stream-id (:stream-id meta) (random-id))
        _reserved (when file (write-meta! file {:stream-id sid :last-seq last-seq :debug-seq debug-seq}))
        segments (if file (enforce-chain! file scanned-segments max-bytes) [])
        debug-segments (if file (enforce-chain! dfile scanned-debug debug-max-bytes) [])
        run-id (random-id)
        sinks (cond-> (vec sinks)
                stdout? (conj #(when-not (= :debug (:level %)) (.write js/process.stdout (str (pr-str %) "\n")))))]
    (atom {:file file :metadata-file (when file (metadata-file file))
           :stream-id sid :generation-id (or generation-id (random-id))
           :run-id run-id :max-bytes max-bytes :last-seq last-seq
           :debug-max-bytes debug-max-bytes :debug-max-age-ms debug-max-age-ms
           :debug-seq debug-seq :debug-segments debug-segments
           :oldest-seq (some :first-seq segments) :segments segments
           :recent [] :recent-sizes [] :recent-byte-count 0 :sinks sinks :now now :pos-fn pos-fn})))

(defn cursor
  "The append cursor of the stream: {:stream-id :seq}."
  [stream]
  (let [{:keys [stream-id last-seq]} @stream]
    {:stream-id stream-id :seq last-seq}))

(defn first-start-after
  "Byte offset of the first line whose :seq exceeds after, or size when there is
  none. Binary search over byte offsets; reads one line per probe."
  [fd size file after]
  (loop [lo 0 hi size best size]
    (if (>= lo hi)
      best
      (let [mid (quot (+ lo hi) 2)
            from (if (zero? mid) 0 (inc (line-end fd size (dec mid))))
            found (when (< from hi) (record-at fd size file from))]
        (cond
          (nil? found) (recur lo mid best)
          (> (:seq (:record found)) after) (recur lo mid (:start found))
          :else (recur (inc (:start found)) hi best))))))

(defn records-from
  "Up to n records of file from byte offset start (a line start) on, read forward a chunk at a time, stopping once their
  lines hold budget bytes (the first record is always taken): {:records :bytes :capped?}, capped? when the budget cut it short."
  [fd size file start n budget]
  (loop [at start out [] bytes 0 chunk read-chunk-bytes]
    (if (or (>= at size) (>= (count out) n))
      {:records out :bytes bytes :capped? false}
      (let [buf (read-bytes fd at chunk)
            last-nl (.lastIndexOf buf 10)
            complete? (or (>= (+ at (.-length buf)) size) (not (neg? last-nl)))]
        (if-not complete?
          (recur at out bytes (* 2 chunk)) ; one line longer than the chunk
          (let [used (if (neg? last-nl) (.-length buf) (inc last-nl))
                lines (->> (str/split-lines (.toString (.subarray buf 0 used) "utf8"))
                           (remove str/blank?))
                [out bytes capped?]
                (reduce (fn [[out bytes _] line]
                          (cond
                            (>= (count out) n) (reduced [out bytes false])
                            (and (seq out) (>= bytes budget)) (reduced [out bytes true])
                            :else [(conj out (seq-of-record line file (str "(byte " at ")"))) (+ bytes (bytes-of line)) false]))
                        [out bytes false] lines)]
            (if capped?
              {:records out :bytes bytes :capped? true}
              (recur (+ at used) out bytes read-chunk-bytes))))))))

(defn segment-records-after
  "{:records :bytes :capped?} of up to n records of file with :seq above after, within budget bytes (see records-from)."
  [file after n budget]
  (let [fd (fs/openSync file "r")]
    (try
      (let [size (.-size (fs/fstatSync fd))
            start (first-start-after fd size file after)
            r (records-from fd size file start n budget)]
        (update r :records (fn [records] (filterv #(> (:seq %) after) records))))
      (finally (fs/closeSync fd)))))

(defn disk-events-after
  "{:events :capped?}: up to limit events with :seq above after, oldest first, parsing about budget bytes at most (the
  first event is always taken). Cost follows the events returned, not the segment sizes: the first wanted line of a
  segment is found by binary search on byte offsets, so earlier lines are never parsed."
  [segments after limit budget]
  (loop [remaining segments out [] left budget]
    (cond
      (and (seq out) (<= left 0)) {:events out :capped? true}
      (or (empty? remaining) (>= (count out) limit)) {:events out :capped? false}
      :else
      (let [{:keys [file bytes last-seq]} (first remaining)]
        (if (or (zero? bytes) (nil? last-seq) (<= last-seq after))
          (recur (rest remaining) out left)
          (let [r (segment-records-after file after (- limit (count out)) left)]
            (if (:capped? r)
              {:events (into out (:records r)) :capped? true}
              (recur (rest remaining) (into out (:records r)) (- left (:bytes r))))))))))

(defn disk-events-merged
  "disk-events-after over several chains, merged by seq. A chain that filled the page bounds the result at its last
  event, so no event of another chain is skipped."
  [chains after limit budget]
  (let [rs (mapv #(disk-events-after % after limit budget) chains)
        bound (->> rs
                   (keep (fn [{:keys [events capped?]}]
                           (when (or capped? (>= (count events) limit)) (:seq (last events)))))
                   (reduce min js/Infinity))]
    {:events (->> rs (mapcat :events) (filter #(<= (:seq %) bound)) (sort-by :seq) (take limit) vec)
     :capped? (boolean (some :capped? rs))}))

(defn read-after
  "Read a bounded page after a cursor. A stream replacement, retention loss,
  or reserved-but-missing sequence is returned as :gap? true with no events;
  the caller must take a fresh state snapshot and use its cursor."
  [stream {:keys [stream-id after limit] :or {after 0}}]
  (let [{sid :stream-id latest :last-seq segments :segments recent :recent
         debug-segments :debug-segments horizon :debug-seq} @stream
        limit (bounded-page-size limit)
        oldest (some :first-seq segments)
        cursor-now {:stream-id sid :seq latest}
        base {:stream-id sid :oldest-seq oldest :latest-seq latest}]
    (cond
      (not= sid stream-id) (assoc base :gap? true :events [] :cursor cursor-now)
      (not (and (integer? after) (not (neg? after))))
      {:error :bad-cursor :stream-id sid :oldest-seq oldest :latest-seq latest
       :gap? true :events [] :cursor cursor-now}
      (> after latest) (assoc base :gap? true :events [] :cursor cursor-now)
      (and oldest (< after (dec oldest)) (> (dec oldest) (or horizon 0))) (assoc base :gap? true :events [] :cursor cursor-now)
      (>= after latest) (assoc base :gap? false :events [] :cursor {:stream-id sid :seq after})
      :else
      (let [recent-first (some-> recent first :seq)
            {:keys [events capped?]}
            (if (and recent-first (>= after (dec recent-first)))
              {:events (->> recent (filter #(> (:seq %) after)) (take limit) vec)}
              (disk-events-merged [segments debug-segments] after limit (setting "max-page-bytes")))
            seqs (map :seq events)
            complete? (and (not capped?) (< (count seqs) limit))
            ;; Missing seqs at or below the newest :debug event may be expired :debug events: no gap.
            missing (concat (when (and (seq seqs) (> (first seqs) (inc after))) [[(inc after) (dec (first seqs))]])
                            (for [[a b] (partition 2 1 seqs) :when (> b (inc a))] [(inc a) (dec b)])
                            (when (and complete? (< (or (last seqs) after) latest)) [[(inc (or (last seqs) after)) latest]]))]
        (if (some (fn [[_ to]] (> to (or horizon 0))) missing)
          (assoc base :gap? true :events [] :cursor cursor-now)
          (assoc base :gap? false :events events
                      :cursor {:stream-id sid :seq (if complete? latest (or (last seqs) after))}))))))

(defn emit!
  "Append one normalized EDN event and return its sequence. Oversized events
  return an explicit error map and are reported to stderr without losing their
  data to truncation."
  [stream event]
  (append-record! stream event))
