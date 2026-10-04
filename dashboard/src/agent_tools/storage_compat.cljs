(ns agent-tools.storage-compat
  (:require [agent-tools.world-data :as data]))

;; Legacy JavaScript callers use keyword wrappers and camel-case option names.
;; Conversion belongs solely at this exported boundary; the implementation and
;; other CLJS tools use native EDN values.
(def aliases {"repoRoot" :repo-root "worldDir" :world-dir "plansDir" :plans-dir
              "blueprintDir" :blueprint-dir "columnsDir" :columns-dir "metadataDir" :metadata-dir
              "expectedRevision" :expected-revision "dryRun" :dry-run
              "timeoutMs" :timeout-ms "pollMs" :poll-ms "explicitCursor" :explicit-cursor})
(def context-aliases {:repo-root "repoRoot" :world-dir "worldDir" :plans-dir "plansDir"
                      :blueprint-dir "blueprintDir" :columns-dir "columnsDir" :metadata-dir "metadataDir"})
(declare from-js to-js)
(defn from-js [v]
  (cond
    (array? v) (mapv from-js (array-seq v))
    (and v (= "object" (goog/typeOf v)))
    (let [keys (vec (js/Object.keys v))]
      (cond
        (= keys ["key"]) (keyword (aget v "key"))
        (= keys ["sym"]) (symbol (aget v "sym"))
        (= keys ["set"]) (let [items (mapv from-js (array-seq (aget v "set")))]
                           (with-meta (set items) {:edn-items (vec (distinct items))}))
        (= keys ["list"]) (apply list (map from-js (array-seq (aget v "list"))))
        :else (let [keys (filterv #(not (undefined? (aget v %))) keys)
                    native-keys (mapv #(or (aliases %) (keyword %)) keys)]
                (with-meta (into {} (map (fn [k nk] [nk (from-js (aget v k))]) keys native-keys))
                  {:json-keys native-keys}))))
    :else v))
(defn- map-entries [m]
  (if-let [ks (:json-keys (meta m))]
    (map (fn [k] [k (get m k)]) (concat (filter #(contains? m %) ks) (remove (set ks) (keys m))))
    (seq m)))
(defn to-js [v]
  (cond
    (keyword? v) #js {:key (subs (str v) 1)}
    (symbol? v) #js {:sym (str v)}
    (map? v) (let [o (js-obj)]
               (doseq [[k x] (map-entries v)]
                 (aset o (if (keyword? k) (subs (str k) 1) (str k)) (to-js x))) o)
    (set? v) #js {:set (into-array (map to-js (data/ordered-set-items v)))}
    (vector? v) (into-array (map to-js v))
    (seq? v) #js {:list (into-array (map to-js v))}
    :else v))
(defn- context-js [ctx]
  (let [o (js-obj)] (doseq [[k x] ctx] (aset o (or (context-aliases k) (name k)) (if (= k :state) (clj->js x) x))) o))
(defn- legacy-record [d]
  (when d (-> d (update :kind data/name) (update :scope data/name))))
(defn- legacy-query [result] (update result :items #(mapv legacy-record %)))
(defn convert-error [e]
  (when-let [extra (.-data e)]
    (doseq [[k v] extra] (aset e (name k) (to-js v)))) e)
(defn- promise-js [p] (.catch (.then p to-js) (fn [e] (throw (convert-error e)))))
(defn- sync-js [run]
  (try (to-js (run)) (catch :default e (throw (convert-error e)))))
(defn js-context [opts] (try (context-js (data/context (from-js opts))) (catch :default e (throw (convert-error e)))))
(defn js-document-path [ctx kind id] (data/document-path (from-js ctx) kind id))
(defn js-read-document [ctx kind id] (sync-js #(legacy-record (data/read-document (from-js ctx) kind id))))
(defn js-list-documents [ctx kind] (sync-js #(mapv legacy-record (data/list-documents (from-js ctx) kind))))
(defn js-mutate-document [ctx opts]
  (let [native (from-js opts)
        native (if-let [validate (.-validate opts)]
                 (assoc native :validate (fn [value ctx]
                                           (.then (js/Promise.resolve (validate (to-js value) (context-js ctx))) from-js))) native)]
    (promise-js (data/mutate-document (from-js ctx) native))))
(defn js-query
  ([records] (js-query records #js {}))
  ([records opts] (sync-js #(legacy-query (data/query (from-js records) (from-js opts))))))
(defn js-read-changes
  ([ctx] (js-read-changes ctx #js {}))
  ([ctx opts] (promise-js (data/read-changes (from-js ctx) (from-js opts)))))
(defn js-synchronize [ctx] (promise-js (data/synchronize (from-js ctx))))
(defn js-lock
  ([file run] (data/with-file-lock file run))
  ([file run opts] (data/with-file-lock file run (from-js opts))))
(defn js-raw-bound
  ([value] (js-raw-bound value 65536))
  ([value max-size] (data/raw-bound (from-js value) max-size) value))
(defn js-fail
  ([reason message] (data/fail reason message))
  ([reason message extra] (convert-error (data/fail reason message (from-js extra)))))
(def api
  #js {:MAX_FILE_BYTES data/max-file-bytes :MAX_OBJECTS data/max-objects :MAX_EVENTS data/max-events
       :ID data/id-pattern :name (fn [v] (data/name (from-js v))) :fail js-fail :revision data/revision
       :context js-context :documentPath js-document-path :readDocument js-read-document
       :listDocuments js-list-documents :mutateDocument js-mutate-document :query js-query
       :readChanges js-read-changes :synchronize js-synchronize :withFileLock js-lock
       :rawBound js-raw-bound :boxOf (fn [v] (to-js (data/box-of (from-js v))))})
