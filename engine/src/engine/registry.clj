(ns engine.registry
  "Compile time: find every job namespace under the jobs/ source directory, check its exports, and emit the job
  registry; read the default trigger set (triggers/defaults.edn) and emit the trigger registry, and the engine
  hooks (hooks.edn). See README.md, Jobs and Triggers."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as rt]))

(def exports
  "What a job namespace may define, and whether it must."
  {'check true 'round true 'doc false 'args false})

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

(defn defined-names
  "Names defined at top level with def or defn (any metadata)."
  [forms]
  (set (keep (fn [f] (when (and (seq? f) (#{'def 'defn 'defn-} (first f)) (symbol? (second f)))
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
  "A job file defines check and round (the required exports); any other file under jobs/ is a helper."
  [defines]
  (every? (fn [[k required?]] (or (not required?) (defines k))) exports))

(defn describe
  "{:ns sym :defines #{...}} for a job file, nil for a helper file (no check or no round), or throws when a job
  file's ns does not match its path."
  [dir file]
  (let [forms (read-forms file)
        ns-form (first forms)
        ns-sym (when (and (seq? ns-form) (= 'ns (first ns-form))) (second ns-form))
        want (expected-ns dir file)
        defines (defined-names forms)]
    (cond
      (not (job? defines)) nil
      (not= want ns-sym)
      (throw (ex-info (str "job file " (.getPath file) " must declare (ns " want " ...), found " (pr-str ns-sym))
                      {:file (.getPath file)}))
      :else {:ns want :defines defines})))

(defn job-namespaces
  "Every job namespace under jobs/, checked, in path order. Helper files are left out."
  []
  (if-let [dir (jobs-dir)]
    (vec (keep #(describe dir %) (job-files dir)))
    []))

;; Metadata for tooling (the dashboard): what a file declares, without the export checks.

(defn defining?
  "Is the form a def/defn/defn- of the symbol named n?"
  [n form]
  (and (seq? form) (#{'def 'defn 'defn-} (first form)) (symbol? (second form)) (= n (name (second form)))))

(defn def-value
  "The value of (def n value), or nil."
  [n forms]
  (some->> forms (filter #(and (defining? n %) (= 'def (first %)))) first (drop 2) first))

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
  "{:id :file :ns-doc :doc :args}, with :args the (def args ...) value itself, for the source file of
  namespace id. A file that cannot be read gets {:id :file :error \"...\"} instead of throwing."
  [id file]
  (let [base {:id (str id) :file (repo-path file)}]
    (try
      (let [forms (read-forms file)
            doc (def-value "doc" forms)]
        (assoc base
               :ns-doc (ns-doc forms)
               :doc (when (string? doc) doc)
               :args (def-value "args" forms)))
      (catch Exception e
        (assoc base :error (str "unreadable: " (ex-message e)))))))

(defn job-metadata
  "file-metadata of every job file under jobs/ (helpers left out), in path order."
  []
  (if-let [dir (jobs-dir)]
    (vec (keep (fn [f] (let [id (expected-ns dir f)]
                         (when (job? (try (defined-names (read-forms f)) (catch Exception _ #{'check 'round})))
                           (file-metadata id f))))
               (job-files dir)))
    []))

(defn settings-namespaces
  "Every namespace under jobs/ and triggers/ (helper files included) that defines a top-level `settings`, in
  path order."
  []
  (vec (for [dir-fn [jobs-dir triggers-dir]
             :let [dir (dir-fn)]
             :when dir
             f (job-files dir)
             :let [forms (read-forms f)]
             :when (contains? (defined-names forms) 'settings)]
         (expected-ns dir f))))

(defmacro settings-registry
  "{ns-symbol ns/settings} for every namespace that declares a top-level `settings` map of its keys."
  []
  (into {} (map (fn [ns] [(list 'quote ns) (symbol (str ns) "settings")])) (settings-namespaces)))

(defmacro job-registry
  "{ns-symbol {:check :round :doc :args}} for every job namespace under jobs/."
  []
  (into {}
        (map (fn [{:keys [ns defines]}]
               (let [ref (fn [k] (when (defines k) (symbol (str ns) (str k))))]
                 [(list 'quote ns) {:check (ref 'check) :round (ref 'round)
                                    :doc (ref 'doc) :args (ref 'args)}])))
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

(defn trigger-files-namespaces
  "The namespace of each source file under triggers/."
  []
  (if-let [dir (triggers-dir)]
    (mapv #(expected-ns dir %) (job-files dir))
    []))

(defn trigger-defaults
  "The default trigger set, resource triggers/defaults.edn: {:facts sym :triggers [{:id :when :job :args
  :persistence :cooldown-s :backoff} ...]}, checked: every :when names a fn that exists, in a namespace under triggers/ once
  that folder holds any; ids unique. {} when absent."
  []
  (let [{:keys [facts triggers] :as data} (or (read-edn-resource "triggers/defaults.edn") {})
        repeated (keep (fn [[id n]] (when (> n 1) id)) (frequencies (map :id triggers)))
        in-dir (set (trigger-files-namespaces))]
    (when (seq repeated)
      (throw (ex-info (str "triggers/defaults.edn: trigger id repeated: " (str/join ", " repeated)) {:ids repeated})))
    (doseq [{id :id when-fn :when} triggers]
      (when-not (keyword? id)
        (throw (ex-info (str "triggers/defaults.edn: :id must be a keyword, found " (pr-str id)) {:id id})))
      (check-ref (str "trigger " id " :when") when-fn)
      (when (and (seq in-dir) (not (in-dir (symbol (namespace when-fn)))))
        (throw (ex-info (str "trigger " id " :when " when-fn ": not a namespace under triggers/") {:id id}))))
    (when facts (check-ref "triggers/defaults.edn :facts" facts))
    data))

(defn trigger-namespaces
  "Every namespace the trigger registry needs loaded: each file under triggers/, and the namespaces the default
  trigger set names (:when fns and :facts)."
  []
  (let [{:keys [facts triggers]} (trigger-defaults)
        named (map (comp symbol namespace) (cond-> (map :when triggers) facts (conj facts)))]
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

(defmacro facts-table
  "The facts table the default trigger set names (:facts), or {}."
  []
  (or (:facts (trigger-defaults)) {}))

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
   :world/blank "an empty world store (engine.core, when none is given)"})

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
