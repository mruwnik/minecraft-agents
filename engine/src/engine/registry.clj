(ns engine.registry
  "Compile time: find every job namespace under the jobs/ source directory, check its exports, and emit the job
  registry; read the default trigger set (triggers/defaults.edn) and emit the trigger registry, and the engine
  hooks (hooks.edn). See README.md, Jobs and Triggers."
  (:require [engine.args :as args]
            [clojure.edn :as edn]
            [clojure.walk]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as rt]))

(defn jobs-dir
  "The jobs/ directory on the classpath (engine/src/jobs), or nil."
  []
  (some-> (io/resource "jobs") io/file))

(defn job-files [dir]
  (->> (file-seq dir)
       (filter #(and (.isFile %) (re-find #"\.clj[sc]$" (.getName %))))
       (sort-by #(.getPath %))))

(defn read-forms
  "Every top-level form of a cljs source file, read leniently: tagged
  literals and auto-resolved keywords are read as plain data."
  [file]
  (binding [reader/*read-eval* false
            reader/*default-data-reader-fn* (fn [_tag v] v)
            reader/*alias-map* (fn [_] 'alias)]
    (let [r (rt/indexing-push-back-reader (slurp file))
          eof (Object.)]
      (loop [forms []]
        (let [form (reader/read {:eof eof :read-cond :allow :features #{:cljs}} r)]
          (if (identical? eof form) forms (recur (conj forms form))))))))

(defn def-head?
  "Is sym a defining head: def, defn, defn-, or defargs under any alias (engine.args)?"
  [sym]
  (or (#{'def 'defn 'defn-} sym) (and (symbol? sym) (= "defargs" (name sym)))))

(defn defined-names
  "Names defined at top level with def, defn or defargs (any metadata)."
  [forms]
  (set (keep (fn [f] (when (and (seq? f) (def-head? (first f)) (symbol? (second f)))
                       (second f)))
             forms)))

(defn expected-ns
  "The namespace a file under dir must declare: jobs.<path with _ as ->."
  [dir file]
  (let [rel (-> (.relativize (.toPath (.getParentFile dir)) (.toPath file)) str)]
    (-> rel
        (str/replace #"\.clj[sc]$" "")
        (str/replace #"[/\\]" ".")
        (str/replace "_" "-")
        symbol)))

(defn job?
  "A job file defines round; any other file under jobs/ is a helper."
  [defines]
  (boolean (defines 'round)))

(declare job-meta)

(defn describe
  "{:ns sym :defines #{...} :meta (job-meta)} for a job file, nil for a helper file (job?), or throws when a job file's ns does not match
  its path, a job defines check (the old start check; a job declares can-start? or can-run?), or a job lacks safe? or fit?."
  [dir file]
  (let [forms (read-forms file)
        ns-form (first forms)
        ns-sym (when (and (seq? ns-form) (= 'ns (first ns-form))) (second ns-form))
        want (expected-ns dir file)
        defines (defined-names forms)]
    (cond
      (and (defines 'round) (defines 'check))
      (throw (ex-info (str "job file " (.getPath file) " defines check; a job declares can-start? or can-run?")
                      {:file (.getPath file)}))
      (not (job? defines)) nil
      (not= want ns-sym)
      (throw (ex-info (str "job file " (.getPath file) " must declare (ns " want " ...), found " (pr-str ns-sym))
                      {:file (.getPath file)}))
      (not (and (defines 'safe?) (defines 'fit?)))
      (throw (ex-info (str "job " want " must export " (str/join " and " (map str (remove defines '[safe? fit?]))) " (ready/ok for a job with nothing to check)")
                      {:file (.getPath file) :job want}))
      :else {:ns want :defines defines :meta (job-meta want forms)})))

(defn job-namespaces
  "Every job namespace under jobs/, checked, in path order. Helper files are left out."
  []
  (if-let [dir (jobs-dir)]
    (vec (keep #(describe dir %) (job-files dir)))
    []))

;; Metadata for tooling (the dashboard): what a file declares, without the export checks.

(defn defining?
  "Is the form a def/defn/defn-/defargs of the symbol named n?"
  [n form]
  (and (seq? form) (def-head? (first form)) (symbol? (second form)) (= n (name (second form)))))

(defn def-value
  "The value of (def n value) or (defargs n doc? value), or nil."
  [n forms]
  (some->> forms (filter #(and (defining? n %) (not (#{'defn 'defn-} (first %))))) first (drop 2) last))

(defn unquote-form
  "The form with every (quote x) replaced by x: a declared meta is read as source, not evaluated."
  [form]
  (clojure.walk/postwalk #(if (and (seq? %) (= 'quote (first %))) (second %) %) form))

(defn default-category
  "The category of a job that declares none, from its namespace group: :debug for jobs.debug.*, else :goal."
  [ns-sym]
  (if (= "debug" (second (str/split (str ns-sym) #"\."))) :debug :goal))

(defn job-meta
  "{:category :goal|:phase|:reflex|:debug :part-of [job-sym ...]} of job ns-sym from the (def meta {...}) in forms:
  what it declares over the default category, :part-of [] when absent."
  [ns-sym forms]
  (let [declared (some-> (def-value "meta" forms) unquote-form)]
    {:category (or (:category declared) (default-category ns-sym))
     :part-of (vec (:part-of declared))}))

(defn ns-doc
  "The docstring of the ns form (the first form), or nil."
  [forms]
  (let [form (first forms)
        doc (when (and (seq? form) (= 'ns (first form))) (nth form 2 nil))]
    (when (string? doc) doc)))

(defn repo-path
  "engine/src/... from a file under engine/src."
  [file]
  (str "engine/src/" (second (re-find #"/engine/src/(.+)$" (str/replace (.getPath file) "\\" "/")))))

(defn file-metadata
  "{:id :file :ns-doc :doc :args :category :part-of}, with :args the (def args ...) value itself, for the source file of
  namespace id. A file that cannot be read gets {:id :file :error \"...\"} instead of throwing."
  [id file]
  (let [base {:id (str id) :file (repo-path file)}]
    (try
      (let [forms (read-forms file)
            doc (def-value "doc" forms)]
        (merge base
               (job-meta id forms)
               {:ns-doc (ns-doc forms)
                :doc (when (string? doc) doc)
                :args (some-> (def-value "args" forms) args/expand)}))
      (catch Exception e
        (assoc base :error (str "unreadable: " (ex-message e)))))))

(defn primitive-job-name
  "jobs.primitive.<kebab-case> for the primitive keyword k (:moveTo is jobs.primitive.move-to)."
  [k]
  (symbol (str "jobs.primitive." (str/replace (name k) #"[A-Z]" #(str "-" (str/lower-case %))))))

(defn primitive-file "engine/primitive.cljs on the classpath, or nil." []
  (some-> (io/resource "engine/primitive.cljs") io/file))

(defn primitive-keys
  "The primitives of the table in engine/primitive.cljs (its keys), in order, or []."
  []
  (if-let [file (primitive-file)]
    (vec (keys (def-value "table" (read-forms file))))
    []))

(defn job-metadata
  "file-metadata of every job file under jobs/ (helpers left out), in path order."
  []
  (if-let [dir (jobs-dir)]
    (vec (concat (keep (fn [f] (let [id (expected-ns dir f)]
                                 (when (job? (try (defined-names (read-forms f)) (catch Exception _ #{'round})))
                                   (file-metadata id f))))
                       (job-files dir))
                 (let [file (primitive-file)]
                   (for [k (primitive-keys)]
                     (assoc (file-metadata (primitive-job-name k) file) :doc (str "The primitive " k " as a job: one act."))))))
    []))

(defmacro job-registry
  "{ns-symbol {:round :doc :args :idle-ok :category :part-of}} for every job namespace under jobs/; a contract job's entry also
  holds :can-start? :can-run? :safe? :fit? (a checker fn or a quoted condition form) :target :ways :defaults :done? :quote-of :cleanup :cleanup-defaults  and its namespace's
  :settings. Each
  primitive of the engine/primitive.cljs table is a job too, as jobs.primitive.<kebab-case> (jobs.primitive/job)."
  []
  (into (into {} (map (fn [k] [(list 'quote (primitive-job-name k)) (list 'jobs.primitive/job k (list 'quote (primitive-job-name k)))])) (primitive-keys))
        (map (fn [{:keys [ns defines] declared :meta}]
               (let [ref (fn [k] (when (defines k) (symbol (str ns) (str k))))]
                 [(list 'quote ns) (cond-> (merge {:round (ref 'round)
                                                  :doc (ref 'doc) :args (ref 'args) :idle-ok (ref 'idle-ok)}
                                                 (update declared :part-of #(list 'quote %)))
                                     (job? defines)
                                     (merge {:can-start? (ref 'can-start?) :can-run? (ref 'can-run?) :safe? (ref 'safe?) :fit? (ref 'fit?)
                                             :target (ref 'target) :ways (ref 'ways)
                                             :defaults (ref 'defaults) :done? (ref 'done?) :quote-of (ref 'quote-of)
                                             :cleanup (ref 'cleanup) :cleanup-defaults (ref 'cleanup-defaults)
                                             :settings (ref 'settings)}))])))
        (job-namespaces)))

;; ------------------------------------------------------------------ triggers and hooks

(defn read-edn-resource
  "The EDN data of classpath resource path, or nil when there is none."
  [path]
  (when-let [r (io/resource path)]
    (edn/read-string (slurp r))))

(defn ns-file
  "The source file of namespace ns-sym on the classpath, or nil."
  [ns-sym]
  (let [base (-> (str ns-sym) (str/replace "-" "_") (str/replace "." "/"))]
    (some-> (some #(io/resource (str base %)) [".cljs" ".cljc"]) io/file)))

(defn check-ref
  "The qualified symbol sym, checked to name a def or defn of a namespace on the classpath; what is the key for
  the message."
  [what sym]
  (let [file (when (qualified-symbol? sym) (ns-file (symbol (namespace sym))))]
    (cond
      (not (qualified-symbol? sym))
      (throw (ex-info (str what " must be a qualified symbol ns/name, found " (pr-str sym)) {:what what :sym sym}))
      (nil? file)
      (throw (ex-info (str what " " sym ": no source file for namespace " (namespace sym)) {:what what :sym sym}))
      (not ((defined-names (read-forms file)) (symbol (name sym))))
      (throw (ex-info (str what " " sym ": " (namespace sym) " defines no " (name sym)) {:what what :sym sym}))
      :else sym)))

(defn triggers-dir
  "The triggers/ directory on the classpath (engine/src/triggers), or nil."
  []
  (some-> (io/resource "triggers") io/file))

(defn settings-in
  "The namespaces under dir (helper files included) that define a top-level `settings`, in path order, minus skip."
  [dir skip]
  (vec (for [f (job-files dir)
             :when (re-find #"\.cljs$" (.getName f))
             :let [ns (expected-ns dir f)]
             :when (and (contains? (defined-names (read-forms f)) 'settings) (not (skip ns)))]
         ns)))

(defn settings-namespaces
  "Every namespace under the given directories (default: jobs/ and triggers/; helper files included) that defines
  a top-level `settings`, in path order."
  ([] (settings-namespaces [(jobs-dir) (triggers-dir)]))
  ([dirs] (vec (mapcat #(when % (settings-in % #{})) dirs))))

(defn engine-dir
  "The engine/ source directory on the classpath (engine/src/engine), or nil."
  []
  (some-> (io/resource "engine/registry.clj") io/file .getParentFile))

(def engine-ungathered
  "Engine namespaces that declare `settings` but cannot be gathered: the ones that hold the gathering (the registry,
  engine.settings-registry) and main, which requires it (main adds its own map)."
  '#{engine.registry engine.settings-registry engine.main})

(defn engine-settings-namespaces
  "Every engine namespace that defines a top-level `settings`, except `engine-ungathered`."
  []
  (if-let [dir (engine-dir)] (settings-in dir engine-ungathered) []))

(defn settings-form [nss]
  (into {} (map (fn [ns] [(list 'quote ns) (symbol (str ns) "settings")])) nss))

(defmacro settings-registry
  "{ns-symbol ns/settings} for every namespace that declares a top-level `settings` map of its keys. With a
  classpath directory name (a test fixture), reads that directory instead of jobs/ and triggers/."
  ([] (settings-form (settings-namespaces)))
  ([resource] (settings-form (settings-namespaces [(some-> (io/resource resource) io/file)]))))

(defn trigger-files-namespaces
  "The namespace of each source file under triggers/."
  []
  (if-let [dir (triggers-dir)]
    (mapv #(expected-ns dir %) (job-files dir))
    []))

(defn trigger-defaults
  "The default trigger set, resource triggers/defaults.edn: {:library sym :triggers [{:id :when :job :args
  :persistence :cooldown-s :level :backoff} ...]}, checked: every :when names a fn that exists, in a namespace under
  triggers/ once that folder holds any; every :level is :danger, :urgent or :comfort; ids unique. {} when absent."
  []
  (let [{:keys [library triggers] :as data} (or (read-edn-resource "triggers/defaults.edn") {})
        repeated (keep (fn [[id n]] (when (> n 1) id)) (frequencies (map :id triggers)))
        in-dir (set (trigger-files-namespaces))]
    (when (seq repeated)
      (throw (ex-info (str "triggers/defaults.edn: trigger id repeated: " (str/join ", " repeated)) {:ids repeated})))
    (doseq [{id :id when-fn :when level :level} triggers]
      (when-not (keyword? id)
        (throw (ex-info (str "triggers/defaults.edn: :id must be a keyword, found " (pr-str id)) {:id id})))
      (when-not (#{:danger :urgent :comfort} level)
        (throw (ex-info (str "trigger " id ": :level must be :danger, :urgent or :comfort, found " (pr-str level)) {:id id})))
      (check-ref (str "trigger " id " :when") when-fn)
      (when (and (seq in-dir) (not (in-dir (symbol (namespace when-fn)))))
        (throw (ex-info (str "trigger " id " :when " when-fn ": not a namespace under triggers/") {:id id}))))
    (when library (check-ref "triggers/defaults.edn :library" library))
    data))

(defn trigger-namespaces
  "Every namespace the trigger registry needs loaded: each file under triggers/, and the namespaces the default
  trigger set names (:when fns and :library)."
  []
  (let [{:keys [library triggers]} (trigger-defaults)
        named (map (comp symbol namespace) (cond-> (map :when triggers) library (conj library)))]
    (vec (distinct (concat (trigger-files-namespaces) named)))))

(defmacro trigger-registry
  "{id {:name id :when fn :job '(spec) :args ... :persistence ... :cooldown-s ...}} for every trigger of the
  default trigger set; only the keys a line gives. :args, :persistence and :cooldown-s are emitted as code (a
  qualified symbol reads that var)."
  []
  (into {}
        (map (fn [{:keys [id job] :as line}]
               [id (cond-> (-> (dissoc line :id) (assoc :name id))
                     (contains? line :job) (assoc :job (list 'quote job)))]))
        (:triggers (trigger-defaults))))

(defmacro trigger-order
  "The ids of the default trigger set, in its listed order (the default register's priority)."
  []
  (mapv :id (:triggers (trigger-defaults))))

(defmacro library-table
  "The condition library the default trigger set names (:library), or {}."
  []
  (or (:library (trigger-defaults)) {}))

(defn trigger-metadata
  "For tooling (the dashboard): one entry per default trigger {:id :fn :file :ns-doc :doc :job :args}, :doc the
  fn's docstring."
  []
  (for [{id :id when-fn :when job :job args :args} (:triggers (trigger-defaults))
        :let [file (ns-file (symbol (namespace when-fn)))
              forms (read-forms file)
              form (first (filter #(defining? (name when-fn) %) forms))
              doc (when (seq? form) (let [d (nth form 2 nil)] (when (string? d) d)))]]
    {:id (name id) :fn (str when-fn) :file (repo-path file) :ns-doc (ns-doc forms) :doc doc
     :job (pr-str job) :args args}))

(def hook-keys
  "Every hook the engine calls, and what for."
  {:world/open "the world store over a world folder's files (engine.main)"
   :world/blank "an empty world store (engine.core, when none is given)"
   :use/default "the list a call's :use gets for a purpose its job's defaults name a chooser for (engine.core.ending)"
   :ready/vocabulary "{sym entry} the condition entries only a job's :checks forms may use, (job-safe) (exposed) ... (engine.condition/compile extras)"
   :look/around "one look round from where the body stands, aimed first at the cells a :look status names, for a condition that asks for one (engine.core.ending)"})

(defn hook-defs
  "The hooks, resource jobs/hooks.edn: {hook-key qualified-fn-symbol}, checked: every key of hook-keys given, no
  other, each naming a def that exists."
  []
  (let [data (or (read-edn-resource "jobs/hooks.edn") {})
        missing (remove (set (keys data)) (keys hook-keys))
        unknown (remove hook-keys (keys data))]
    (when (seq missing)
      (throw (ex-info (str "jobs/hooks.edn lacks " (str/join ", " missing)) {:missing missing})))
    (when (seq unknown)
      (throw (ex-info (str "jobs/hooks.edn: unknown hook " (str/join ", " unknown)) {:unknown unknown})))
    (doseq [[k sym] data] (check-ref (str "hook " k) sym))
    data))

(defn hook-namespaces
  "The namespaces the hooks name."
  []
  (vec (distinct (map (comp symbol namespace) (vals (hook-defs))))))

(defmacro hook-table
  "{hook-key fn} of jobs/hooks.edn."
  []
  (hook-defs))

(defmacro engine-settings-registry
  "{ns-symbol ns/settings} for every engine namespace that declares a top-level `settings` map of its keys."
  []
  (settings-form (engine-settings-namespaces)))
