(ns engine.registry
  "Compile time: find every job namespace under the jobs/ source directory,
  check its exports, and emit the job registry. See README.md, Jobs."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as rt]))

(def exports
  "What a job namespace may define, and whether it must."
  {'check true 'round true 'doc false 'args false 'backoff false})

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

(defn describe
  "{:ns sym :defines #{...}} for a job file, or throws with a message naming it."
  [dir file]
  (let [forms (read-forms file)
        ns-form (first forms)
        ns-sym (when (and (seq? ns-form) (= 'ns (first ns-form))) (second ns-form))
        want (expected-ns dir file)
        defines (defined-names forms)
        missing (sort (keep (fn [[k required?]] (when (and required? (not (defines k))) k)) exports))]
    (cond
      (not= want ns-sym)
      (throw (ex-info (str "job file " (.getPath file) " must declare (ns " want " ...), found " (pr-str ns-sym))
                      {:file (.getPath file)}))
      (seq missing)
      (throw (ex-info (str "job namespace " want " must define " (str/join " and " missing)
                           " (a job namespace exports check and round)")
                      {:ns want :missing missing}))
      :else {:ns want :defines defines})))

(defn job-namespaces
  "Every job namespace under jobs/, checked, in path order."
  []
  (if-let [dir (jobs-dir)]
    (mapv #(describe dir %) (job-files dir))
    []))

(defmacro job-registry
  "{ns-symbol {:check :round :doc :args :backoff}} for every job namespace under jobs/."
  []
  (into {}
        (map (fn [{:keys [ns defines]}]
               (let [ref (fn [k] (when (defines k) (symbol (str ns) (str k))))]
                 [(list 'quote ns) {:check (ref 'check) :round (ref 'round)
                                    :doc (ref 'doc) :args (ref 'args)
                                    :backoff (ref 'backoff)}])))
        (job-namespaces)))
