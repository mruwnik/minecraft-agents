(ns dashboard.migrate-data
  "Explicit village plan migration, with create-only additions and guarded enrichment of existing native plans."
  (:require ["fs" :as fs]
            ["path" :as path]
            ["crypto" :as crypto]
            [clojure.string :as str]
            [dashboard.edn-data :as data]
            [plan.shape :as shape]))

(def usage "node dashboard/out/migrate-data.cjs --root REPO --world claude [--state-dir STATE] (--dry-run | --apply)")

(defn options [argv]
  (loop [args argv opts {:root (.cwd js/process)}]
    (if (empty? args)
      (do
        (when-not (and (:world opts) (some? (:apply? opts))) (throw (js/Error. usage)))
        (when-not (re-matches #"[A-Za-z0-9_-]{1,64}" (:world opts)) (throw (js/Error. "invalid world")))
        (update opts :root #(.resolve path %)))
      (let [[flag value] args]
        (case flag
          ("--dry-run" "--apply")
          (do (when (contains? opts :apply?) (throw (js/Error. "choose exactly one of --dry-run or --apply")))
              (recur (rest args) (assoc opts :apply? (= flag "--apply"))))
          ("--root" "--world" "--state-dir")
          (do (when (or (nil? value) (str/starts-with? value "--")) (throw (js/Error. usage)))
              (recur (nnext args) (assoc opts ({"--root" :root "--world" :world "--state-dir" :state-dir} flag) value)))
          (throw (js/Error. (str "unknown argument " flag "; " usage))))))))

(defn json->edn [v]
  (cond
    (vector? v) (mapv json->edn v)
    (map? v) (into {} (map (fn [[k value]]
                            [(if (and (re-matches #"[A-Za-z_][A-Za-z0-9_-]*" k)
                                      (not (re-matches #"[0-9a-fA-F]{8}-[0-9a-fA-F-]{27,}" k))) (keyword k) k)
                             (json->edn value)])) v)
    :else v))

(defn read-json [file]
  (json->edn (js->clj (js/JSON.parse (data/read-text file)))))

(defn source-ref [state file]
  {:file (.relative path state file) :sha256 (.digest (.update (.createHash crypto "sha256") (data/read-text file)) "hex")})

(defn anchor [record]
  (let [xyz (mapv #(get record %) [:x :y :z])]
    (when (every? #(and (number? %) (js/Number.isFinite %)) xyz) xyz)))

(defn safe-id [place]
  (when-not (and (string? place) (re-matches #"[A-Za-z0-9_-]{1,100}" place))
    (throw (js/Error. (str "place needs a deliberate native plan ID: " (pr-str place)))))
  place)

(defn intent-plan [world marker inspection refs]
  (doseq [record [marker inspection]
          :when (and (:world record) (not= world (:world record)))]
    (throw (js/Error. (str "record explicitly belongs to another world: " (:world record)))))
  (let [id (safe-id (or (:name marker) (:place inspection)))
        at (or (when inspection (anchor (:at inspection))) (anchor marker))
        population (or (:population inspection) (get-in inspection [:source :population]))
        metadata (cond-> {:geometry :incomplete
                          :migration {:world world :sources (vec refs) :world-basis :explicit-migration-world}}
                   marker (assoc :legacy-place marker)
                   inspection (assoc :legacy-inspection inspection :source (:source inspection))
                   population (assoc :population population))
        value (cond-> {:id id :kind :village :status :proposed :parts [] :metadata metadata
                        :note (or (:note marker) "Migrated village intention; geometry is incomplete.")}
                at (assoc :at at))
        errors (shape/plan-errors value id)]
    (when-not at (throw (js/Error. (str "village " id " has no recorded finite anchor"))))
    (when (seq errors) (throw (js/Error. (str "invalid migrated plan " id ": " (str/join "; " (map :error errors))))))
    value))

(defn inspection-inputs [state]
  (let [dir (.join path state "village-inspections")]
    (mapv (fn [file]
            (let [file (.join path dir file) value (read-json file)]
              (when-not (and (= 1 (:version value)) (string? (:place value)) (anchor (:at value)))
                (throw (js/Error. (str "invalid legacy village inspection: " file))))
              {:value value :source (source-ref state file)}))
          (data/files dir #"[a-f0-9]{32}\.json"))))

(defn village-intents [state world]
  (let [places-file (.join path state "worlds" world "places.json")
        places (if (.existsSync fs places-file) (read-json places-file) [])
        _ (when-not (and (vector? places) (<= (count places) 10000)) (throw (js/Error. "places must be a bounded vector")))
        markers (filter #(= "village" (:kind %)) places)
        all-by-name (into {} (map (juxt :name identity)) places)
        inspections (inspection-inputs state)
        duplicates (->> inspections (group-by #(get-in % [:value :place])) (filter #(> (count (val %)) 1)))]
    (when (seq duplicates) (throw (js/Error. "multiple inspections for one place need deliberate merge; no migration writes")))
    (when-not (= (count (map :name markers)) (count (set (map :name markers))))
      (throw (js/Error. "duplicate village marker names")))
    (let [inspection-by-name (into {} (map (fn [i] [(get-in i [:value :place]) i])) inspections)
          names (distinct (concat (map :name markers) (map #(get-in % [:value :place]) inspections)))
          places-ref (when (.existsSync fs places-file) (source-ref state places-file))]
      (mapv (fn [name]
              (let [{:keys [value source]} (get inspection-by-name name)
                    marker (get all-by-name name)]
                (when (and marker value (not= (anchor marker) (anchor (:at value))))
                  (throw (js/Error. (str "place/inspection anchors disagree for " name "; no migration writes"))))
                (intent-plan world marker value (concat (when marker [places-ref]) (when source [source]))))) names))))

(defn digest [text] (.digest (.update (.createHash crypto "sha256") text) "hex"))

(defn merge-metadata [prior incoming context]
  (reduce-kv (fn [result k value]
               (if-not (contains? result k) (assoc result k value)
                 (let [existing (get result k)]
                   (cond (= existing value) result
                         (and (map? existing) (map? value))
                         (assoc result k (merge-metadata existing value (str context "/" (name k))))
                         :else (throw (js/Error. (str "existing metadata conflicts at " context "/" (name k))))))))
             prior incoming))

(defn enrich [prior incoming]
  (doseq [k [:kind :at]
          :when (and (contains? prior k) (not= (get prior k) (get incoming k)))]
    (throw (js/Error. (str "existing plan conflicts at " (:id prior) "/" (name k)))))
  (let [metadata (assoc (:metadata incoming) :geometry (if (seq (:parts prior)) :planned :incomplete))]
    (-> prior (assoc :kind :village)
        (cond-> (not (contains? prior :at)) (assoc :at (:at incoming)))
        (assoc :metadata (merge-metadata (get prior :metadata {}) metadata (:id prior))))))

(defn planned-write [file value backup-dir]
  (let [before (when (.existsSync fs file) (data/read-text file))
        prior (when before (data/one-form before))
        errors (when prior (shape/plan-errors prior (:id value)))
        _ (when (seq errors) (throw (js/Error. (str "existing plan invalid: " file))))
        value (if prior (enrich prior value) value)
        text (str (pr-str value) "\n")]
    (when (> (js/Buffer.byteLength text) data/max-bytes) (throw (js/Error. (str "migration result exceeds 8 MiB: " file))))
    (when-not (= value (data/one-form text)) (throw (js/Error. (str "EDN round-trip changed migration result: " file))))
    (cond
      (= prior value) {:file file :status :unchanged :value value :text text}
      prior (let [hash (digest before) backup (.join path backup-dir (str (:id value) "." hash ".edn"))]
              (when (and (.existsSync fs backup) (not= before (data/read-text backup)))
                (throw (js/Error. (str "original plan backup conflicts: " backup))))
              {:file file :status :enrich :value value :text text :before before :before-sha256 hash
               :backup {:file backup :text before :value prior :exact? true}})
      :else {:file file :status :create :value value :text text})))

(defn prepare [{:keys [root state-dir world]}]
  (when-not (and (string? world) (re-matches #"[A-Za-z0-9_-]{1,64}" world)) (throw (js/Error. "give an explicit valid world")))
  (let [state (.resolve path (or state-dir (.join path root "state")))
        dir (.join path state "worlds" world)
        _ (when-not (.existsSync fs (.join path dir "world.json")) (throw (js/Error. (str "target world is missing: " world))))
        plans (village-intents state world)
        backup-dir (.join path dir ".migration-backups" "village-plans")
        writes (mapv #(planned-write (.join path dir "plans" (str (:id %) ".edn")) % backup-dir) plans)]
    {:world world :state-dir state :writes (vec writes)
     :warnings ["Existing native plan layout, status, assignments and plan notes are preserved. New plans are proposed with incomplete geometry."
                "Legacy inspection reports are historical evidence; no villagers are enrolled as plan members."]
     :village-count (count plans)}))

(defn create-only! [{:keys [file text value exact?]}]
  (.mkdirSync fs (.dirname path file) #js {:recursive true})
  (let [temp (str file "." (.randomUUID crypto) ".tmp")]
    (try
      (let [fd (.openSync fs temp "wx" 384)]
        (try (.writeFileSync fs fd text) (.fsyncSync fs fd) (finally (.closeSync fs fd))))
      ;; link creates the complete destination atomically and refuses overwrite,
      ;; unlike rename. A racing writer's different data is never replaced.
      (try (.linkSync fs temp file) :created
           (catch :default e
             (if (and (= "EEXIST" (.-code e))
                      (if exact? (= text (data/read-text file)) (= value (data/read-file file)))) :unchanged (throw e))))
      (finally (when (.existsSync fs temp) (.unlinkSync fs temp))))))

(defn enrich! [{:keys [file text before value backup]}]
  ;; Use the same per-file lock as native agent-tools writers. Compare exact
  ;; source bytes under that lock, then replace only the verified revision.
  (let [lock (str file ".lock") token (str (.-pid js/process) "\n" (.randomUUID crypto))
        fd (.openSync fs lock "wx" 384)
        temp (str file "." (.randomUUID crypto) ".tmp")]
    (try
      (.writeFileSync fs fd token)
      (let [current (data/read-text file)]
        (cond
          (= value (data/one-form current)) :unchanged
          (not= before current) (throw (js/Error. (str "plan changed after migration preflight: " file)))
          :else
          (do
            (create-only! backup)
            (let [out (.openSync fs temp "wx" 384)]
              (try (.writeFileSync fs out text) (.fsyncSync fs out) (finally (.closeSync fs out))))
            (when-not (= before (data/read-text file))
              (throw (js/Error. (str "plan changed during migration: " file))))
            (.renameSync fs temp file)
            :enriched)))
      (finally
        (.closeSync fs fd)
        (when (.existsSync fs temp) (.unlinkSync fs temp))
        (when (= token (data/read-text lock)) (.unlinkSync fs lock))))))

(defn execute [opts]
  (let [report (prepare opts)
        writes (:writes report)]
    ;; Preparing every file first means a known syntax/conflict error prevents
    ;; all writes. A race can leave earlier complete files; rerunning is safe.
    (assoc (dissoc report :writes) :mode (if (:apply? opts) :apply :dry-run)
           :files (mapv (fn [write]
                          (cond-> {:file (:file write)
                                   :status (if (:apply? opts)
                                             (case (:status write) :create (create-only! write) :enrich (enrich! write) (:status write))
                                             (:status write))}
                            (:backup write) (assoc :backup (get-in write [:backup :file])
                                                  :previous-sha256 (:before-sha256 write)))) writes))))

(defn main [& _]
  (try
    (println (pr-str (execute (options (vec (.slice (.-argv js/process) 2))))))
    (catch :default e
      (binding [*print-fn* #(.write (.-stderr js/process) (str % "\n"))]
        (println (pr-str {:error (ex-message e)})))
      (set! (.-exitCode js/process) 1))))
