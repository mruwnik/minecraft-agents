(ns engine.events
  "Canonical EDN event stream, bounded rolling file appender, and local cursor reader."
  (:require [clojure.string :as str]
            [cljs.reader :as reader]
            ["crypto" :as crypto]
            ["fs" :as fs]
            ["path" :as path]))

(def default-max-bytes (* 64 1024 1024))
(def min-max-bytes 1024)
(def segment-count 4) ; active + three rotated segments
(def recent-count 2048)
(def recent-bytes (* 4 1024 1024))
(def default-page-size 256)
(def max-page-size 2000)

(defn random-id [] (.randomUUID crypto))

(defn metadata-file [file]
  (str file ".meta.edn"))

(defn segment-file [file n]
  (if (zero? n) file (str file "." n)))

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

(defn seq-of-record [line file line-no]
  (let [record (try
                 (reader/read-string line)
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

(defn repair-active-tail! [file]
  (when (fs/existsSync file)
    (let [buf (fs/readFileSync file)
          len (.-length buf)]
      (when (and (pos? len) (not= 10 (.readUInt8 buf (dec len))))
        (let [newline (.lastIndexOf buf 10)
              keep (if (neg? newline) 0 (inc newline))]
          (fs/truncateSync file keep))))))

(defn scan-segment [file]
  (if-not (fs/existsSync file)
    {:file file :bytes 0 :first-seq nil :last-seq nil}
    (let [text (fs/readFileSync file "utf8")
          lines (str/split-lines text)
          seqs (loop [remaining lines line-no 1 prior 0 found []]
                 (if-let [line (first remaining)]
                   (if (str/blank? line)
                     (recur (rest remaining) (inc line-no) prior found)
                     (let [n (:seq (seq-of-record line file line-no))]
                       (when (<= n prior)
                         (throw (ex-info (str "event sequence is not increasing in " file
                                              " at line " line-no)
                                         {:file file :line line-no :seq n :prior prior})))
                       (recur (rest remaining) (inc line-no) n (conj found n))))
                   found))]
      {:file file
       :bytes (.-size (fs/statSync file))
       :first-seq (first seqs)
       :last-seq (last seqs)})))

(defn scan-segments [file]
  (mapv scan-segment (reverse (segment-files file)))) ; oldest to active

(defn trim-active! [file max-bytes]
  (let [text (fs/readFileSync file "utf8")
        lines (->> (str/split-lines text) (remove str/blank?) vec)
        {:keys [kept dropped]} (loop [remaining (reverse lines) used 0 kept [] dropped [] stopped? false]
                                 (if-let [line (first remaining)]
                                   (let [line-size (inc (js/Buffer.byteLength line "utf8"))]
                                     (if (and (not stopped?) (<= (+ used line-size) max-bytes))
                                       (recur (rest remaining) (+ used line-size)
                                              (conj kept line) dropped false)
                                       (recur (rest remaining) used kept (conj dropped line) true)))
                                   {:kept (reverse kept) :dropped (reverse dropped)}))]
    (when (seq dropped)
      (doseq [line dropped]
        (let [event (seq-of-record line file 1)]
          (.write js/process.stderr
                  (str "event retention: dropped complete record seq " (:seq event)
                       " while enforcing " max-bytes " byte cap\n"))))
      (write-atomic! file (if (seq kept) (str (str/join "\n" kept) "\n") "")))
    (scan-segment file)))

(defn ensure-valid-max-bytes! [n]
  (when-not (and (integer? n) (<= min-max-bytes n))
    (throw (ex-info (str ":max-bytes must be an integer at least " min-max-bytes)
                    {:max-bytes n})))
  n)

(defn initial-meta [file]
  (let [mf (metadata-file file)
        m (read-edn-file mf)]
    (when (and m (not (and (string? (:stream-id m))
                           (integer? (:last-seq m))
                           (not (neg? (:last-seq m))))))
      (throw (ex-info (str "invalid event metadata " mf) {:file mf})))
    (or m {:stream-id (random-id) :last-seq 0})))

(defn all-records [segments]
  (into []
        (mapcat (fn [{:keys [file bytes]}]
                  (if (zero? bytes)
                    []
                    (->> (str/split-lines (fs/readFileSync file "utf8"))
                         (remove str/blank?)
                         (map-indexed (fn [i line] (seq-of-record line file (inc i))))))))
        segments))

(defn bounded-page-size [n]
  (if (and (integer? n) (pos? n)) (min n max-page-size) default-page-size))

(defn bytes-of [text]
  (js/Buffer.byteLength text "utf8"))

(defn canonical-event
  "The canonical event for internal event map event, numbered n. :body is dropped,
  :level is kept. Other fields outside the known set fold into :data."
  [{:keys [generation-id run-id now pos-fn]} n event]
  (let [source (:source event)
        kind (:kind event)
        old-context (or (:context event) {})
        context (cond-> old-context
                  (some? (:job event)) (assoc :job-id (:job event))
                  (some? (:round event)) (assoc :round (:round event))
                  (some? (:chain event)) (assoc :chain (:chain event))
                  (some? (:reflex event)) (assoc :reflex-id (:reflex event))
                  (some? (:action-id event)) (assoc :action-id (:action-id event))
                  (number? (:cause event)) (assoc :cause-seq (:cause event))
                  (some? (:cause-seq event)) (assoc :cause-seq (:cause-seq event)))
        known #{:seq :generation-id :run-id :time-ms :t :body :source :kind :level
                :context :data :message :text :attention :request-id :job :round
                :chain :reflex :action-id :cause :cause-seq :pos}
        ;; Emitters pass absent optional fields as nil: drop those.
        ;; A non-numeric :cause is the body's own datum (died/hurt: "lava"), not a cause sequence.
        extra (into {} (remove (comp nil? val))
                    (cond-> (apply dissoc event known)
                      (not (number? (:cause event))) (assoc :cause (:cause event))))
        pos (or (:pos event) (when pos-fn (pos-fn)))
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
           :first-seq first-seq :last-seq n})))

(defn rotate-segment-index [segments file]
  (let [empty-segment {:file (segment-file file 0) :bytes 0 :first-seq nil :last-seq nil}
        by-file (into {} (map (juxt :file identity) segments))
        rotated (keep (fn [n]
                        (when-let [prior (get by-file (segment-file file (dec n)))]
                          (assoc prior :file (segment-file file n))))
                      (range (dec segment-count) 0 -1))]
    (conj (vec rotated) empty-segment)))

(defn append-record! [stream event]
  (let [{:keys [file max-bytes last-seq stream-id generation-id run-id now pos-fn]} @stream
        n (inc last-seq)
        full (canonical-event {:generation-id generation-id :run-id run-id :now now :pos-fn pos-fn} n event)
        line (str (pr-str full) "\n")
        line-bytes (bytes-of line)]
    (if (> line-bytes max-bytes)
      (do
        (.write js/process.stderr (str "event rejected: serialized record is " line-bytes
                                       " bytes, above the " max-bytes " byte log cap\n"))
        {:error :event-too-large :bytes line-bytes :max-bytes max-bytes})
      (do
        ;; Reserve the sequence before appending: a crash leaves a gap, never a reused number.
        (when file
          (write-meta! file {:stream-id stream-id :last-seq n}))
        ;; The in-memory counter moves on even if the append fails.
        (swap! stream assoc :last-seq n)
        (when file
          (let [active (segment-file file 0)
                active-bytes (.-size (fs/statSync active))]
            (when (and (pos? active-bytes)
                       (> (+ active-bytes line-bytes) (segment-target max-bytes)))
              (rotate! file)
              (swap! stream update :segments rotate-segment-index file))
            (fs/appendFileSync active line)
            (prune-segments! file max-bytes)
            (swap! stream update :segments update-active-segment file n line-bytes)
            (swap! stream update :segments existing-segment-index)))
        (let [r (assoc @stream :recent (conj (:recent @stream) full)
                               :recent-byte-count (+ (:recent-byte-count @stream) line-bytes))]
          (let [r (loop [s r]
                    (if (or (> (count (:recent s)) recent-count)
                            (> (:recent-byte-count s) recent-bytes))
                      (let [first-event (first (:recent s))
                            first-bytes (bytes-of (str (pr-str first-event) "\n"))]
                        (recur (-> s
                                   (update :recent subvec 1)
                                   (update :recent-byte-count - first-bytes))))
                      s))]
            (swap! stream assoc :recent (into [] (:recent r))
                   :recent-byte-count (:recent-byte-count r))))
        (doseq [sink (:sinks @stream)]
          (try (sink full)
               (catch :default e
                 (.write js/process.stderr (str "event sink failed: " (.-message e) "\n")))))
        n))))

(def read-chunk-bytes (* 64 1024))

(defn read-bytes
  "Up to len bytes of the file behind fd from position pos; empty at or past the end."
  [fd pos len]
  (let [buf (js/Buffer.alloc len)
        n (fs/readSync fd buf 0 len pos)]
    (.subarray buf 0 n)))

(defn tail-records
  "The last events of file, at most max-count and within max-bytes: {:records :bytes}.
  Reads only the file's tail."
  [file max-bytes max-count]
  (let [fd (fs/openSync file "r")]
    (try
      (let [size (.-size (fs/fstatSync fd))
            start (max 0 (- size max-bytes))
            text (.toString (read-bytes fd start (- size start)) "utf8")
            lines (cond->> (str/split-lines text)
                    (pos? start) rest) ; the first line may be cut
            lines (->> lines (remove str/blank?) (take-last max-count) vec)]
        {:records (mapv #(seq-of-record % file (str "(tail of " size " bytes)")) lines)
         :bytes (reduce + 0 (map #(inc (bytes-of %)) lines))})
      (finally (fs/closeSync fd)))))

(defn make
  "Create a canonical EDN event stream. Options: :file, :generation-id,
  :max-bytes (aggregate active+rotated cap), :stdout?, :sinks, :pos-fn,
  :now, and :stream-id for tests. File-backed streams use events.edn plus
  atomically replaced events.edn.meta.edn metadata."
  [{:keys [file generation-id max-bytes stdout? sinks pos-fn now stream-id]
    :or {max-bytes default-max-bytes now js/Date.now pos-fn (constantly nil)
         sinks []}}]
  (ensure-valid-max-bytes! max-bytes)
  (when file
    (fs/mkdirSync (path/dirname file) #js {:recursive true})
    (when-not (fs/existsSync file) (fs/writeFileSync file ""))
    (repair-active-tail! file))
  (let [meta (if file (initial-meta file) {:stream-id (or stream-id (random-id)) :last-seq 0})
        scanned-segments (if file (scan-segments file) [])
        scanned-seq (reduce max 0 (keep :last-seq scanned-segments))
        last-seq (max (:last-seq meta) scanned-seq)
        sid (or stream-id (:stream-id meta) (random-id))
        _reserved (when file (write-meta! file {:stream-id sid :last-seq last-seq}))
        _pruned (when file (prune-segments! file max-bytes))
        active-info (when file (trim-active! file max-bytes))
        base-segments (if file (existing-segment-index scanned-segments) [])
        segments (if file (assoc (vec base-segments) (dec (count base-segments)) active-info) [])
        retained-bytes (reduce + 0 (map :bytes segments))
        _valid-cap (when (> retained-bytes max-bytes)
                     (throw (ex-info (str "could not enforce configured :max-bytes " max-bytes)
                                     {:file file :bytes retained-bytes :max-bytes max-bytes})))
        run-id (random-id)
        {recent :records recent-byte-count :bytes}
        (if file (tail-records file recent-bytes recent-count) {:records [] :bytes 0})
        sinks (cond-> (vec sinks)
                stdout? (conj #(.write js/process.stdout (str (pr-str %) "\n"))))]
    (atom {:file file :metadata-file (when file (metadata-file file))
           :stream-id sid :generation-id (or generation-id (random-id))
           :run-id run-id :max-bytes max-bytes :last-seq last-seq
           :oldest-seq (some :first-seq segments) :segments segments
           :recent recent :recent-byte-count recent-byte-count :sinks sinks :now now :pos-fn pos-fn})))

(defn cursor
  "The append cursor of the stream: {:stream-id :seq}."
  [stream]
  (let [{:keys [stream-id last-seq]} @stream]
    {:stream-id stream-id :seq last-seq}))

(defn cursor-map [stream]
  (let [{:keys [stream-id last-seq]} @stream]
    {:stream-id stream-id :seq last-seq}))

(defn line-end
  "Byte offset of the newline ending the line that holds position pos, or size."
  [fd size pos]
  (loop [at pos]
    (if (>= at size)
      size
      (let [buf (read-bytes fd at read-chunk-bytes)
            i (.indexOf buf 10)]
        (cond
          (zero? (.-length buf)) size
          (neg? i) (recur (+ at (.-length buf)))
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
  "Up to n records of file from byte offset start (a line start) on, read
  forward a chunk at a time."
  [fd size file start n]
  (loop [at start out [] chunk read-chunk-bytes]
    (if (or (>= at size) (>= (count out) n))
      out
      (let [buf (read-bytes fd at chunk)
            last-nl (.lastIndexOf buf 10)
            complete? (or (>= (+ at (.-length buf)) size) (not (neg? last-nl)))]
        (if-not complete?
          (recur at out (* 2 chunk)) ; one line longer than the chunk
          (let [used (if (neg? last-nl) (.-length buf) (inc last-nl))
                lines (->> (str/split-lines (.toString (.subarray buf 0 used) "utf8"))
                           (remove str/blank?))
                records (map #(seq-of-record % file (str "(byte " at ")")) lines)]
            (recur (+ at used) (into out (take (- n (count out)) records)) read-chunk-bytes)))))))

(defn segment-records-after [file after n]
  (let [fd (fs/openSync file "r")]
    (try
      (let [size (.-size (fs/fstatSync fd))
            start (first-start-after fd size file after)]
        (->> (records-from fd size file start n)
             (filter #(> (:seq %) after))
             vec))
      (finally (fs/closeSync fd)))))

(defn disk-events-after
  "Up to limit events with :seq above after, oldest first. Cost follows the
  events returned, not the segment sizes: the first wanted line of a segment
  is found by binary search on byte offsets, so earlier lines are never parsed."
  [segments after limit]
  (loop [remaining segments out []]
    (if (or (empty? remaining) (>= (count out) limit))
      out
      (let [{:keys [file bytes last-seq]} (first remaining)]
        (if (or (zero? bytes) (nil? last-seq) (<= last-seq after))
          (recur (rest remaining) out)
          (recur (rest remaining)
                 (into out (segment-records-after file after (- limit (count out))))))))))

(defn read-after
  "Read a bounded page after a cursor. A stream replacement, retention loss,
  or reserved-but-missing sequence is returned as :gap? true with no events;
  the caller must take a fresh state snapshot and use its cursor."
  [stream {:keys [stream-id after limit] :or {after 0}}]
  (let [{sid :stream-id latest :last-seq segments :segments recent :recent} @stream
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
      (and oldest (< after (dec oldest))) (assoc base :gap? true :events [] :cursor cursor-now)
      (>= after latest) (assoc base :gap? false :events [] :cursor {:stream-id sid :seq after})
      :else
      (let [recent-first (some-> recent first :seq)
            events (if (and recent-first (>= after (dec recent-first)))
                     (->> recent (filter #(> (:seq %) after)) (take limit) vec)
                     (disk-events-after segments after limit))
            seqs (map :seq events)
            expected (inc after)
            hole? (or (and (seq seqs) (> (first seqs) expected))
                      (and (empty? seqs) (< after latest))
                      (and (seq seqs) (< (count seqs) limit) (< (last seqs) latest)))
            internal-hole? (some true? (map (fn [[a b]] (> b (inc a)))
                                              (partition 2 1 seqs)))]
        (if (or hole? internal-hole?)
          (assoc base :gap? true :events [] :cursor cursor-now)
          (assoc base :gap? false :events events
                      :cursor {:stream-id sid :seq (or (last seqs) after)}))))))

(defn emit!
  "Append one normalized EDN event and return its sequence. Oversized events
  return an explicit error map and are reported to stderr without losing their
  data to truncation."
  [stream event]
  (append-record! stream event))
