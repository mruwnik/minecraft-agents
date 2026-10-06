(ns engine.notes
  "Notes: what bodies saw, written by the bodies themselves into the world's shared knowledge. (Plans, by
  contrast, are only read.)

  A note is {:kind :what :pos [x y z] :by body :t ms :until ms} plus what its kind needs:
  - {:kind :seen :what \"oak_log\" :pos ..}: a block seen there
  - {:kind :seen :what \"cow\" :id uuid :pos ..}: an entity, keyed by its uuid because animals walk
  - {:kind :searched :what [names] :pos .. :r n}: looked for those names within r blocks (XZ) of pos
  :t and :until are wall-clock ms, a clock all bodies share. There is one note per key (note-key: kind,
  what, and the uuid or position). The newest :t wins, a tie goes to the lowest :by.

  Files: each body writes only its own worlds/<world>/notes/<body>.edn (see paths), whole, to a temp
  file that is renamed, so a reader never sees half a file. Readers merge every body's file.

  Reading follows engine.world. The folder is stat-ed at most every :every-ms, lazily, and only files whose
  stamp changed are read again. A broken file keeps its last good copy and warns once
  (world.notes-unreadable). The body's own notes are kept in memory, read from its file at first use, so
  its writes show at once.

  A store is {:state atom :opts {...}}. :state is {:files {body entry} :checked-at ms :own [note]
  :own-loaded? bool :broken-own? bool}, an entry as in engine.world."
  (:require ["fs" :as fs]
            ["path" :as path]
            [cljs.reader :as reader]
            [engine.ctx :as ctx]
            [engine.file-sync :as fsync]))

(def default-every-ms 3000)
(def default-cap 2000)

;; ------------------------------------------------------------------ pure

(defn note-key [{:keys [kind what id pos]}]
  [kind what (or id pos)])

(defn newer?
  "Whether note a wins over b for one key: the later :t, on a tie the lower :by."
  [a b]
  (or (> (:t a) (:t b))
      (and (= (:t a) (:t b)) (neg? (compare (:by a) (:by b))))))

(defn merge-notes
  "One note per key from several bodies' note lists, the newest kept, in the order the keys first appear."
  [lists]
  (let [[order m] (reduce (fn [[order m] n]
                            (let [k (note-key n)
                                  old (get m k)]
                              (cond
                                (nil? old) [(conj order k) (assoc m k n)]
                                (newer? n old) [order (assoc m k n)]
                                :else [order m])))
                          [[] {}]
                          (apply concat lists))]
    (mapv m order)))

(defn live
  "The notes still in force at now."
  [ns now]
  (filterv #(> (:until %) now) ns))

(defn bound
  "The live notes of ns in their order, at most cap of them, the oldest :t dropped first."
  [ns now cap]
  (let [l (live ns now)]
    (if (<= (count l) cap) l (filterv (set (take-last cap (sort-by :t l))) l))))

(defn covers?
  "Whether note is a :searched note listing every name of targets whose centre lies within half its radius of
  the XZ point [x z]: that ground needs no look for these targets."
  [{:keys [kind what pos r]} [x z] targets]
  (and (= :searched kind)
       (every? (set what) targets)
       (<= (js/Math.hypot (- x (pos 0)) (- z (pos 2))) (/ r 2))))

(defn note-error
  "Why n is not a note, or nil."
  [n]
  (cond
    (not (map? n)) "not a map"
    (not (keyword? (:kind n))) ":kind is not a keyword"
    (not (and (vector? (:pos n)) (= 3 (count (:pos n))) (every? number? (:pos n)))) ":pos is not [x y z]"
    (not (and (number? (:t n)) (number? (:until n)))) ":t or :until is not a number"
    (not (or (string? (:what n)) (and (vector? (:what n)) (every? string? (:what n))))) ":what is not a name or names"
    (and (= :searched (:kind n)) (not (number? (:r n)))) "a :searched note without a number :r"))

(defn parse-file
  "A notes file's text as {:value [note]} or {:errors [text]}; one bad note makes the whole file bad."
  [text]
  (let [data (try (reader/read-string text) (catch :default e {::error (ex-message e)}))]
    (cond
      (::error data) {:errors [(str "unreadable EDN: " (::error data))]}
      (not (and (map? data) (vector? (:notes data)))) {:errors ["not a map with a :notes vector"]}
      :else (if-let [[i e] (first (keep-indexed (fn [i n] (when-let [e (note-error n)] [i e])) (:notes data)))]
              {:errors [(str "note " i ": " e)]}
              {:value (:notes data)}))))

(defn render [body ns]
  (pr-str {:body body :notes (vec ns)}))

;; ------------------------------------------------------------------ files

(defn paths
  "The one place that says where notes live: {:dir :file} for body in the world folder world-dir."
  [world-dir body]
  (let [dir (path/join world-dir "notes")]
    {:dir dir :file (path/join dir (str body ".edn"))}))

(defn read-file [file]
  (let [text (try (.readFileSync fs file "utf8") (catch :default e {:error (ex-message e)}))]
    (if (map? text)
      {:errors [(str "unreadable file: " (:error text))]}
      (parse-file text))))

(defn warn-event [body {:keys [error kept]}]
  {:source :system :kind :world.notes-unreadable :level :warn :body body :error error :kept kept
   :text (str "notes of " body " cannot be read (" error ")"
              (if kept "; the last good copy is still used" "; nothing of them is used"))})

(defn load-own!
  "Read the body's own file once, at the first use; a broken one warns and starts empty."
  [{:keys [state opts]}]
  (when-not (:own-loaded? @state)
    (let [{:keys [file]} (paths (:world-dir opts) (:body opts))
          exists? (.existsSync fs file)
          {:keys [value errors]} (when exists? (read-file file))]
      (swap! state assoc :own-loaded? true :own (or value []) :broken-own? (boolean errors))
      (when errors ((:emit opts) (warn-event (:body opts) {:error (first errors) :kept false}))))))

(defn refresh!
  "Stat the other bodies' files when due and fold in what changed, a warn per newly broken file."
  [{:keys [state opts]} now]
  (when (fsync/due? (assoc @state :every-ms (:every-ms opts)) now)
    (let [{:keys [dir]} (paths (:world-dir opts) (:body opts))
          stamps (dissoc (fsync/stamps dir) (:body opts))
          old (:files @state)
          [files warns] (reduce (fn [[entries warns] id]
                                  (let [[entries warn] (fsync/absorb entries id (get stamps id)
                                                                     (read-file (path/join dir (str id ".edn"))))]
                                    [entries (cond-> warns warn (conj warn))]))
                                [(fsync/drop-gone old stamps) []]
                                (fsync/stale-ids old stamps))]
      (swap! state assoc :files files :checked-at now)
      (doseq [w warns] ((:emit opts) (warn-event (:id w) w))))))

;; ------------------------------------------------------------------ stores

(defn open
  "A store for body's notes in the world folder :world-dir. opts: :body, :emit (an event fn), :every-ms, :cap."
  [{:keys [every-ms cap] :as opts}]
  {:state (atom {:files {} :own [] :own-loaded? false})
   :opts (assoc opts :every-ms (or every-ms default-every-ms) :cap (or cap default-cap))})

(defn body [s] (:body (:opts s)))

(defn all
  "Every live note of the world at now, merged (one per item)."
  [s now]
  (load-own! s)
  (refresh! s now)
  (let [{:keys [files own]} @(:state s)]
    (live (merge-notes (cons own (keep :value (vals files)))) now)))

(defn write-file!
  "Write the body's file whole (temp file, then rename); the broken own file is first moved aside."
  [{:keys [state opts]} ns]
  (let [{:keys [dir file]} (paths (:world-dir opts) (:body opts))
        tmp (str file ".tmp")]
    (.mkdirSync fs dir #js {:recursive true})
    (when (:broken-own? @state)
      (.renameSync fs file (str file ".broken"))
      (swap! state assoc :broken-own? false))
    (.writeFileSync fs tmp (render (:body opts) ns))
    (.renameSync fs tmp file)))

(defn add!
  "Add notes to the body's own notes (same key: the newer wins), bounded, and write the file at now:
  {:status :ok} or {:status :error :error text} (one world.notes-unwritable warn per error text; the notes
  stay in memory and the next write tries again)."
  [{:keys [state opts] :as s} now ns]
  (load-own! s)
  (let [own (bound (merge-notes [(:own @state) ns]) now (:cap opts))]
    (swap! state assoc :own own)
    (try
      (write-file! s own)
      {:status :ok}
      (catch :default e
        (let [error (ex-message e)]
          (when-not (contains? (:unwritable @state) error)
            (swap! state update :unwritable (fnil conj #{}) error)
            ((:emit opts) {:source :system :kind :world.notes-unwritable :level :warn :body (:body opts) :error error
                           :text (str "notes of " (:body opts) " cannot be written (" error ")")}))
          {:status :error :error error})))))

;; ------------------------------------------------------------------ through a job's ctx

(defn store-of [c] (:notes (:world (:engine c))))

(defn notes
  "The world's live notes (merged, see all) for a job's check or round; [] when the body has no notes store."
  [c]
  (if-let [s (store-of c)] (all s (ctx/now c)) []))

(defn note!
  "Write notes [{:kind :what :pos :ttl-ms ...}] as this body's, stamped :by (the body), :t (now) and :until
  (:t plus :ttl-ms): {:status ...} as add!, nil when the body has no notes store."
  [c ns]
  (when-let [s (store-of c)]
    (let [t (ctx/now c)]
      (add! s t (mapv #(-> % (dissoc :ttl-ms) (assoc :by (body s) :t t :until (+ t (:ttl-ms %)))) ns)))))
