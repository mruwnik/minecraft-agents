(ns agent-tools.world-data
  (:refer-clojure :exclude [name])
  (:require [engine.bodies :as bodies]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure.set :as set]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:crypto" :as crypto]
            ["node:timers/promises" :refer [setTimeout]]))

(def max-file-bytes 8388608)
(def max-objects 10000)
(def max-events 2048)
(def id-pattern #"^[A-Za-z0-9_-]{1,100}$")
(defn name [v] (if (keyword? v) (subs (str v) 1) v))
(defn fail
  ([reason message] (fail reason message {}))
  ([reason message extra]
   (let [e (js/Error. message)]
     (set! (.-reason e) (name reason))
     (doseq [[k v] extra] (aset e (name k) v))
     (set! (.-data e) extra)
     e)))
(defn revision [text]
  (when (some? text) (subs (.digest (.update (.createHash crypto "sha256") text) "hex") 0 24)))

;; Collections are written in a fixed EDN format (the revision is a hash of the text).
;; Source text for standalone documents is kept verbatim.
(declare write-edn)
(defn- ordered-entries [m]
  (let [prior (:json-keys (meta m))]
    (if prior
      (map (fn [k] [k (get m k)]) (concat (filter #(contains? m %) prior) (remove (set prior) (keys m))))
      (seq m))))
(defn ordered-set-items [values]
  (if-let [prior (:edn-items (meta values))]
    (concat (filter #(contains? values %) prior) (remove (set prior) values))
    (seq values)))
(defn write-edn [v]
  (cond
    (map? v) (str "{" (str/join " " (mapcat (fn [[k x]] [(write-edn k) (write-edn x)]) (ordered-entries v))) "}")
    (vector? v) (str "[" (str/join " " (map write-edn v)) "]")
    (set? v) (str "#{" (str/join " " (map write-edn (ordered-set-items v))) "}")
    (seq? v) (str "(" (str/join " " (map write-edn v)) ")")
    :else (pr-str v)))

(defn read-edn [text]
  ;; cljs.reader's hash maps lose source key order above eight entries. The revision covers that order,
  ;; so it is kept as map metadata; the values themselves are ordinary EDN.
  (let [size (count text) cursor (atom 0)]
    (letfn [(peek-char [] (when (< @cursor size) (.charAt text @cursor)))
            (skip! []
              (loop []
                (let [c (peek-char)]
                  (cond
                    (and c (re-matches #"[\s,]" c)) (do (swap! cursor inc) (recur))
                    (= c ";") (do (loop [] (when (and (peek-char) (not= "\n" (peek-char))) (swap! cursor inc) (recur))) (recur))
                    :else nil))))
            (string! []
              (swap! cursor inc)
              (loop [escape? false]
                (let [c (peek-char)]
                  (when-not c (throw (js/Error. "unterminated EDN string")))
                  (swap! cursor inc)
                  (cond escape? (recur false) (= c "\\") (recur true) (= c "\"") nil :else (recur false)))))
            (token! []
              (let [start @cursor]
                (loop [] (when (and (peek-char) (not (re-matches #"[\s,\[\]{}();]" (peek-char)))) (swap! cursor inc) (recur)))
                (when (= start @cursor) (throw (js/Error. "invalid EDN token")))))
            (items! [close]
              (loop [items []]
                (skip!)
                (cond
                  (= close (peek-char)) (do (swap! cursor inc) items)
                  (nil? (peek-char)) (throw (js/Error. "unterminated EDN collection"))
                  (and (= "#" (peek-char)) (= "_" (.charAt text (inc @cursor))))
                  (do (swap! cursor + 2) (form!) (recur items))
                  :else (recur (conj items (form!))))))
            (form! []
              (skip!)
              (let [start @cursor c (peek-char)]
                (case c
                  "{" (do (swap! cursor inc)
                          (let [items (items! "}")]
                            (when (odd? (count items)) (throw (js/Error. "odd EDN map")))
                            (with-meta (into {} (map vec (partition 2 items))) {:json-keys (vec (distinct (take-nth 2 items)))})))
                  "[" (do (swap! cursor inc) (vec (items! "]")))
                  "(" (do (swap! cursor inc) (apply list (items! ")")))
                  "\"" (do (string!) (reader/read-string (subs text start @cursor)))
                  "#" (if (= "{" (.charAt text (inc @cursor)))
                        (do (swap! cursor + 2)
                            (let [items (items! "}")]
                              (with-meta (set items) {:edn-items (vec (distinct items))})))
                        (do (token!) (form!) (reader/read-string (subs text start @cursor))))
                  (do (token!) (reader/read-string (subs text start @cursor))))))]
      ;; the reader does the full EDN validation (tagged literals included)
      (let [decoded (reader/read-string text)]
        (try (form!) (catch :default _ decoded))))))

(defn from-json [v]
  (cond
    (array? v) (mapv from-json (array-seq v))
    (and v (= "object" (goog/typeOf v)))
    (let [ks (mapv keyword (js/Object.keys v))]
      (with-meta (into {} (map (fn [k] [k (from-json (aget v (name k)))]) ks)) {:json-keys ks}))
    :else v))
(defn to-json [v]
  (cond
    (map? v) (let [o (js-obj)] (doseq [[k x] (ordered-entries v)] (aset o (name k) (to-json x))) o)
    (set? v) (into-array (map to-json (ordered-set-items v)))
    (coll? v) (into-array (map to-json v))
    (keyword? v) (name v)
    :else v))
(defn json-text
  ([v] (js/JSON.stringify (to-json v)))
  ([v indent] (js/JSON.stringify (to-json v) nil indent)))

(defn context [{:keys [state worlds world repo-root]}]
  (when-not (and (string? world) (re-matches #"[A-Za-z0-9_-]{1,64}" world))
    (throw (fail :invalid-world "give an explicit valid --world")))
  (let [repo-root (.resolve path (or repo-root (.resolve path js/__dirname "../..")))
        state (if (map? state) state (bodies/storage-root {:state state :worlds worlds} repo-root))
        world-dir (.resolve path (bodies/worlds-dir state) world)]
    (when-not (.existsSync fs (.join path world-dir "world.json"))
      (throw (fail :world-not-found (str "no world " world))))
    {:state state :world world :repo-root repo-root :world-dir world-dir
     :plans-dir (.join path world-dir "plans") :blueprint-dir (.join path repo-root "blueprints")
     :columns-dir (.join path world-dir "chunks") :metadata-dir (.join path world-dir ".agent-data")}))

(defn- checked-id [id]
  (when-not (and (string? id) (re-matches id-pattern id))
    (throw (fail :invalid-id "ID must use letters, digits, _ or - (up to100)"))) id)
(def collection-kinds #{:marker :zone :claim})
(defn document-path [ctx kind id]
  (let [kind (keyword kind)]
    (case kind
      :plan (.join path (:plans-dir ctx) (str (checked-id id) ".edn"))
      :blueprint (.join path (:blueprint-dir ctx) (str (checked-id id) ".edn"))
      (:marker :zone :claim)
      (do (when-not (and (string? id) (not (str/blank? id)) (<= (count id) 100) (not (str/includes? id "\u0000")))
            (throw (fail :invalid-id "map names need1..100 characters")))
          (.join path (:world-dir ctx) ({:marker "places.json" :zone "zones.edn" :claim "claims.edn"} kind)))
      (throw (fail :invalid-kind "kind must be marker, zone, claim, plan or blueprint")))))

(defonce ^:private scan-budget (atom nil))
(defn read-text
  ([file] (read-text file max-file-bytes))
  ([file max-size]
   (try
     (let [stat (.statSync fs file)]
       (when (or (not (.isFile stat)) (> (.-size stat) max-size))
         (throw (fail :file-too-large (str "file exceeds " max-size " bytes") {:file file})))
       (when (some? @scan-budget)
         (swap! scan-budget - (.-size stat))
         (when (neg? @scan-budget) (throw (fail :scan-too-large "shared world scan exceeds64MiB; archive unused documents"))))
       (.readFileSync fs file "utf8"))
     (catch :default e (if (= "ENOENT" (.-code e)) nil (throw e))))))
(defn- parse-collection [kind text]
  (if (nil? text) []
      (let [values (try (if (= kind :marker) (from-json (js/JSON.parse text)) (read-edn text))
                        (catch :default _ (throw (fail :invalid-file (str "unreadable " (name kind) " collection")))))
            id-key (if (= kind :claim) :id :name)
            ids (map id-key values)]
        (when-not (and (vector? values) (<= (count values) max-objects))
          (throw (fail :invalid-file (str "expected a bounded " (name kind) " vector"))))
        (when-not (and (every? string? ids) (= (count ids) (count (set ids))))
          (throw (fail :invalid-file "missing or duplicate object ID")))
        values)))
(defn- record [kind id value text file]
  {:kind kind :id id :value value :text text :path file :revision (revision text)
   :scope (if (= kind :blueprint) :global :world)})
(defn- object-text [kind value] (if (= kind :marker) (json-text value) (write-edn value)))
(defn read-document [ctx kind id]
  (let [kind (keyword kind) file (document-path ctx kind id) text (read-text file)]
    (when (some? text)
      (if (collection-kinds kind)
        (when-let [value (some #(when (= id ((if (= kind :claim) :id :name) %)) %) (parse-collection kind text))]
          (record kind id value (object-text kind value) file))
        (try (record kind id (read-edn text) text file)
             (catch :default _ (throw (fail :invalid-file (str "unreadable " (name kind) " " id)))))))))
(defn list-documents [ctx kind]
  (let [kind (keyword kind)]
    (if (collection-kinds kind)
      (let [file (document-path ctx kind "collection")]
        (mapv #(record kind ((if (= kind :claim) :id :name) %) % (object-text kind %) file)
              (parse-collection kind (read-text file))))
      (let [dir (case kind :plan (:plans-dir ctx) :blueprint (:blueprint-dir ctx)
                      (throw (fail :invalid-kind "unknown kind")))
            files (try (sort (filter #(str/ends-with? % ".edn") (array-seq (.readdirSync fs dir))))
                       (catch :default e (if (= "ENOENT" (.-code e)) [] (throw e))))
            prior @scan-budget]
        (when (> (count files) 1000) (throw (fail :object-limit "at most1000 documents per directory")))
        (reset! scan-budget (min (or prior 67108864) 67108864))
        (try (mapv #(read-document ctx kind (subs % 0 (- (count %) 4))) files)
             (finally (when (nil? prior) (reset! scan-budget nil))))))))

(defn atomic-text [file text]
  (.mkdirSync fs (.dirname path file) #js {:recursive true})
  (let [temp (str file "." (.-pid js/process) "." (.randomUUID crypto) ".tmp")]
    (try
      (let [fd (.openSync fs temp "wx" 384)]
        (try (.writeFileSync fs fd text) (.fsyncSync fs fd)
             (finally (.closeSync fs fd))))
      (.renameSync fs temp file)
      (finally (when (.existsSync fs temp) (.unlinkSync fs temp))))))
(defn- reap-lock! [lock]
  (let [guard (str lock ".reaper") fd (atom nil)]
    (try
      (reset! fd (.openSync fs guard "wx" 384))
      (let [owner (read-text lock 200) pid (js/Number (first (str/split (or owner "") #"\n")))
            dead? (when (and (js/Number.isInteger pid) (pos? pid))
                    (try (.kill js/process pid 0) false
                         (catch :default e (= "ESRCH" (.-code e)))))]
        (when (and dead? (= owner (read-text lock 200))) (.unlinkSync fs lock)))
      (catch :default e (when-not (#{"EEXIST" "ENOENT"} (.-code e)) (throw e)))
      (finally (when (some? @fd) (.closeSync fs @fd) (.unlinkSync fs guard))))))
(defn with-file-lock
  ([file run] (with-file-lock file run {}))
  ([file run {:keys [timeout-ms] :or {timeout-ms 1500}}]
   (.mkdirSync fs (.dirname path file) #js {:recursive true})
   (let [lock (str file ".lock") deadline (+ (js/Date.now) timeout-ms)
         token (str (.-pid js/process) "\n" (.randomUUID crypto))]
     (letfn [(acquire []
               (js/Promise.resolve
                (try
                  (let [fd (.openSync fs lock "wx" 384)] (.writeFileSync fs fd token) fd)
                  (catch :default e
                    (when-not (= "EEXIST" (.-code e)) (throw e))
                    (reap-lock! lock)
                    (cond
                      (not (.existsSync fs lock)) (acquire)
                      (>= (js/Date.now) deadline) (throw (fail :busy "shared data is busy; a crashed reaper guard requires inspection before removal"))
                      :else (.then (setTimeout 20) acquire))))))]
       ;; Defer acquisition so every failure, including a busy lock, rejects.
       (.then (js/Promise.resolve)
              (fn [] (.then (acquire)
                            (fn [fd]
                              (.finally (.then (js/Promise.resolve) (fn [] (run)))
                                        (fn [] (.closeSync fs fd)
                                          (when (= token (read-text lock 200)) (.unlinkSync fs lock))))))))))))

(defn- ledger-file [ctx] (.join path (:metadata-dir ctx) "changes.edn"))
(defn- load-ledger [ctx]
  (if-let [text (read-text (ledger-file ctx) (* 8 max-file-bytes))]
    (let [l (read-edn text)
          l (if (map? (:index l))
              (update l :index (fn [index]
                                (let [entries (mapv (fn [[k v]] [(name k) v]) (ordered-entries index))]
                                  (with-meta (into {} entries) {:json-keys (mapv first entries)})))) l)]
      (when-not (and (string? (:stream l)) (js/Number.isSafeInteger (:seq l))
                     (vector? (:events l)) (<= (count (:events l)) max-events)
                     (map? (:index l)) (<= (count (:index l)) max-objects))
        (throw (fail :invalid-ledger "invalid shared change ledger")))
      l)
    {:stream (.randomUUID crypto) :seq 0 :index {} :events []}))
(defn- object-key [kind id] (str (name kind) "/" id))
(defn box-of [v]
  (cond (and (vector? (:min v)) (vector? (:max v))) [(:min v) (:max v)]
        (every? #(js/Number.isFinite (get v %)) [:x :y :z]) (let [p (mapv v [:x :y :z])] [p p])
        :else nil))
(defn- object-meta [d]
  (let [v (:value d)]
    (cond-> {:kind (:kind d) :id (:id d) :revision (:revision d) :scope (:scope d)
             :box (box-of v)}
      (some? (or (:owner v) (:by v))) (assoc :owner (or (:owner v) (:by v)))
      (some? (:status v)) (assoc :status (name (:status v)))
      (some? (or (:kind v) (:type v))) (assoc :type (name (or (:kind v) (:type v))))
      (some? (:until v)) (assoc :until (:until v)))))
(defn- add-event [l obj op by source previous]
  (let [e (cond-> (assoc obj :seq (inc (:seq l)) :time-ms (js/Date.now) :op op :by by :source source)
            previous (assoc :previous-revision previous))]
    [(assoc l :seq (:seq e) :events (vec (take-last max-events (conj (:events l) e)))) e]))
(defn- scan [ctx l]
  (reset! scan-budget 67108864)
  (let [entries (try (vec (for [kind [:marker :zone :claim :plan :blueprint]
                               d (list-documents ctx kind)] [(object-key kind (:id d)) (object-meta d)]))
                    (finally (reset! scan-budget nil)))
        now (with-meta (into {} entries) {:json-keys (mapv first entries)})]
    (when (> (count now) max-objects) (throw (fail :object-limit "too many shared objects")))
    (-> (reduce (fn [l [key obj]]
                  (let [old (get-in l [:index key])]
                    (if (= (:revision old) (:revision obj)) l
                        (first (add-event l obj (if old :edit :add) nil :external (:revision old)))))) l entries)
        ((fn [l] (reduce (fn [l [key obj]]
                           (if (contains? now key) l
                               (first (add-event l (assoc obj :revision nil) :remove nil :external (:revision obj)))))
                         l (ordered-entries (:index l)))))
        (assoc :index now))))
(defn- normalize-context [ctx] (cond-> ctx (:repoRoot ctx) (assoc :repo-root (:repoRoot ctx))))
(defn- recover [ctx global?]
  (let [file (if global? (.join path (:blueprint-dir ctx) ".agent-pending.edn") (.join path (:metadata-dir ctx) "pending.edn"))
        text (read-text file (* 10 max-file-bytes))]
    (if (nil? text) (js/Promise.resolve)
        (let [tx (read-edn text) kind (keyword (:kind tx))
              ctx (if global?
                    (do (when-not (and (= kind :blueprint) (:context tx)
                                       (= (.resolve path (or (:repo-root (:context tx)) (:repoRoot (:context tx)))) (:repo-root ctx)))
                          (throw (fail :invalid-journal "invalid global blueprint journal")))
                        (context (normalize-context (:context tx)))) ctx)]
          (when-not (and (string? (:file tx)) (:ledger tx) (#{:plan :blueprint :marker :zone :claim} kind)
                         (contains? tx :after) (or (nil? (:after tx)) (string? (:after tx)))
                         (= (document-path ctx kind (:id tx)) (:file tx)))
            (throw (fail :invalid-journal "invalid pending shared write")))
          (with-file-lock (:file tx)
            (fn []
              (let [current (revision (read-text (:file tx)))]
                (when-not (or (= current (:before tx)) (= current (revision (:after tx))))
                  (throw (fail :recovery-conflict "a pending write conflicts with a later external edit; inspect .agent-data/pending.edn"))))
              (if (nil? (:after tx)) (when (.existsSync fs (:file tx)) (.unlinkSync fs (:file tx))) (atomic-text (:file tx) (:after tx)))
              (atomic-text (ledger-file ctx) (str (write-edn (:ledger tx)) "\n"))
              (.unlinkSync fs file)))))))
(defn- transaction [ctx run]
  (with-file-lock (.join path (:blueprint-dir ctx) ".agent-transactions")
    (fn [] (.then (recover ctx true)
                  (fn [] (with-file-lock (ledger-file ctx)
                           (fn [] (.then (recover ctx false) (fn [] (run (load-ledger ctx)))))))))))
(defn synchronize [ctx]
  (transaction ctx (fn [l] (let [l (scan ctx l)]
                            (atomic-text (ledger-file ctx) (str (write-edn l) "\n"))
                            (select-keys l [:stream :seq])))))

(defn mutate-document [ctx opts]
  (.then (js/Promise.resolve)
    (fn []
      (let [{:keys [id expected-revision by validate dry-run] :as opts} opts
            kind (keyword (:kind opts)) source (keyword (or (:source opts) :intent))
            exact? (contains? opts :text) text (:text opts)
            value (if exact?
                    (do (when-not (and (#{:plan :blueprint} kind) (string? text) (<= (js/Buffer.byteLength text) max-file-bytes))
                          (throw (fail :invalid-text "exact text is only supported for bounded plan/blueprint documents")))
                        (let [decoded (try (read-edn text) (catch :default _ (throw (fail :invalid-text "unreadable document text"))))]
                          (when (and (contains? opts :value) (not= decoded (:value opts)))
                            (throw (fail :text-mismatch "text and decoded value disagree"))) decoded)) (:value opts))
            file (document-path ctx kind id)]
        (when-not (and (string? by) (not (str/blank? by)) (<= (count by) 80))
          (throw (fail :actor-required "mutations need explicit --by (up to80 characters)")))
        (when-not (contains? opts :expected-revision) (throw (fail :revision-required "mutation needs expected revision; null means create only")))
        (when-not (#{:intent :observation} source) (throw (fail :invalid-source "source must be intent or observation")))
        (transaction ctx
          (fn [l]
            (with-file-lock file
              (fn []
                (let [l (scan ctx l) previous (read-document ctx kind id)]
                  (when-not (= (:revision previous) expected-revision)
                    (throw (fail :revision-conflict "object changed; read it before editing" {:current (:revision previous) :id id :kind kind})))
                  (.then (js/Promise.resolve (when validate (validate value ctx)))
                    (fn [validation]
                      (when (seq (:errors validation)) (throw (fail :validation (str/join "; " (:errors validation)) {:errors (:errors validation)})))
                      (let [before (read-text file)
                            after (if (collection-kinds kind)
                                    (let [values (filterv #(not= id ((if (= kind :claim) :id :name) %)) (parse-collection kind before))
                                          values (if (nil? value) values (conj values value))]
                                      (str (if (= kind :marker) (json-text values 1) (write-edn values)) "\n"))
                                    (when (some? value) (if exact? text (str (write-edn value) "\n"))))
                            next-revision (when (some? value) (revision (if (collection-kinds kind) (object-text kind value) after)))
                            obj (object-meta {:kind kind :id id :value (or value (:value previous) {}) :revision next-revision :scope (if (= kind :blueprint) :global :world)})
                            op (if (nil? value) :remove (if previous :edit :add))]
                        (when (and after (> (js/Buffer.byteLength after) max-file-bytes))
                          (throw (fail :file-too-large "updated file would exceed8MiB")))
                        (if dry-run
                          {:ok true :preview true :kind kind :id id :scope (:scope obj) :revision (:revision previous)
                           :next-revision next-revision :op op
                           :fields (mapv name (filter #(not= (get (:value previous) %) (get value %))
                                                    (distinct (concat (keys (:value previous)) (keys value)))))}
                          (let [[l e] (add-event l obj op by source (:revision previous))
                                l (update l :index (if (nil? value) dissoc assoc) (object-key kind id) obj)
                                pending (if (= kind :blueprint) (.join path (:blueprint-dir ctx) ".agent-pending.edn") (.join path (:metadata-dir ctx) "pending.edn"))]
                            (atomic-text pending (str (write-edn {:context (select-keys ctx [:state :worlds :world :repo-root]) :file file :kind kind :id id
                                                                 :before (revision before) :after after :ledger l}) "\n"))
                            (if (nil? after) (when (.existsSync fs file) (.unlinkSync fs file)) (atomic-text file after))
                            (atomic-text (ledger-file ctx) (str (write-edn l) "\n"))
                            (.unlinkSync fs pending)
                            {:ok true :kind kind :id id :scope (:scope obj) :revision next-revision :op op :cursor {:stream (:stream l) :seq (:seq e)}}))))))))))))))

(defn- distance [box center]
  (if box (js/Math.hypot
            (max (- (get-in box [0 0]) (center 0)) 0 (- (center 0) (get-in box [1 0])))
            (max (- (get-in box [0 1]) (center 1)) 0 (- (center 1) (get-in box [1 1])))
            (max (- (get-in box [0 2]) (center 2)) 0 (- (center 2) (get-in box [1 2])))) js/Infinity))
(defn query
  ([records] (query records {}))
  ([records {:keys [center type owner status text limit offset now radius]
             :or {limit 10 offset 0 now (js/Date.now) radius 128}}]
   (when-not (and (js/Number.isInteger limit) (<= 1 limit 100) (js/Number.isInteger offset) (<= 0 offset))
     (throw (fail :invalid-page "limit1..100 and nonnegative offset required")))
   (when (and center (not (and (vector? center) (= 3 (count center)) (every? js/Number.isFinite center) (js/Number.isFinite radius) (<= 0 radius))))
     (throw (fail :invalid-center "center needs [x y z], radius nonnegative")))
   (let [lower #(str/lower-case (str (or % "")))
         rows (->> records
                   (map (fn [d] (let [m (if (:value d) (cond-> (object-meta d) (:box d) (assoc :box (:box d))) d)]
                                  (cond-> (merge d m)
                                    (and (= "active" (name (:status m))) (:until m) (<= (:until m) now)) (assoc :status "expired")
                                    center (assoc :distance (distance (:box m) center))))))
                   (filter #(and (or (nil? type) (= (name (:kind %)) (name type)) (= (name (:type %)) (name type)))
                                 (or (nil? owner) (= (lower (:owner %)) (lower owner)))
                                 (or (nil? status) (= (name (:status %)) (name status)))
                                 (or (nil? text) (str/includes? (lower (str (:id %) " " (get-in % [:value :note] "") " " (get-in % [:value :name] ""))) (lower text)))
                                 (or (nil? center) (<= (:distance %) radius))))
                   (sort (fn [a b] (or (let [n (if center (- (:distance a) (:distance b)) 0)] (when-not (zero? n) n))
                                       (let [n (.localeCompare (name (:kind a)) (name (:kind b)))] (when-not (zero? n) n))
                                       (.localeCompare (:id a) (:id b))))) vec)
         items (vec (take limit (drop offset rows)))]
     (cond-> {:total (count rows) :items items}
       (< (+ offset (count items)) (count rows)) (assoc :next-offset (+ offset (count items)))))))
(defn read-changes
  ([ctx] (read-changes ctx {}))
  ([ctx {:keys [cursor limit] :or {limit 10} :as opts}]
   (.then (js/Promise.resolve)
     (fn []
       (when-not (and (js/Number.isInteger limit) (<= 1 limit 100)) (throw (fail :invalid-page "limit must be1..100")))
       (let [filters (dissoc opts :cursor :limit)]
         (query [] (assoc filters :limit limit))
         (transaction ctx
           (fn [l]
             (let [l (scan ctx l) _ (atomic-text (ledger-file ctx) (str (write-edn l) "\n"))
                   oldest (or (:seq (first (:events l))) (inc (:seq l))) end (select-keys l [:stream :seq])]
               (cond
                 (nil? cursor) {:cursor end :total 0 :items []}
                 (or (not= (:stream cursor) (:stream l)) (not (js/Number.isSafeInteger (:seq cursor)))
                     (< (:seq cursor) (dec oldest)) (> (:seq cursor) (:seq l)))
                 {:ok false :reason :cursor-gap :cursor end :oldest-seq oldest}
                 :else
                 (let [available (filterv #(> (:seq %) (:seq cursor)) (:events l))
                       matching? #(pos? (:total (query [%] (assoc filters :limit 1))))
                       [through more?] (loop [events available selected #{}]
                                         (if-let [e (first events)]
                                           (if-not (matching? e) (recur (next events) selected)
                                             (let [key (object-key (:kind e) (:id e))]
                                               (if (and (not (selected key)) (>= (count selected) limit))
                                                 [(dec (:seq e)) true] (recur (next events) (conj selected key)))))
                                           [(:seq l) false]))
                       grouped (reduce (fn [{:keys [order values]} e]
                                         (if (or (> (:seq e) through) (not (matching? e))) {:order order :values values}
                                           (let [key (object-key (:kind e) (:id e)) prior (get values key)]
                                             {:order (if prior order (conj order key))
                                              :values (assoc values key (assoc e :changes (inc (or (:changes prior) 0))))})))
                                       {:order [] :values {}} available)
                       items (mapv (fn [key] (update (update (update (get (:values grouped) key) :kind keyword) :op keyword) :source keyword)) (:order grouped))]
                   (cond-> {:cursor {:stream (:stream l) :seq through}
                            :total (count (set (map #(object-key (:kind %) (:id %)) (filter matching? available)))) :items items}
                     more? (assoc :more? true))))))))))))
(defn raw-bound
  ([value] (raw-bound value 65536))
  ([value max-size]
   (when (> (js/Buffer.byteLength (write-edn value)) max-size)
     (throw (fail :output-too-large (str "raw result exceeds " max-size " bytes; scope or page the query")))) value))
