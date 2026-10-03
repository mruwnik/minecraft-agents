(ns dashboard.jobs-registry
  "Compile time: the engine's job and trigger namespaces as data. The files are found and read by
  engine.registry (engine/src is on the source path, only its .clj is loaded); this ns only picks the doc and
  args values out of the forms it returns."
  (:require [clojure.string :as str]
            [engine.registry :as registry]))

(defn defining?
  "Is the form a def/defn/defn- of the symbol named n?"
  [n form]
  (and (seq? form) (#{'def 'defn 'defn-} (first form)) (symbol? (second form)) (= n (name (second form)))))

(defn def-value
  "The value of (def n value), or nil."
  [n forms]
  (some->> forms (filter #(and (defining? n %) (= 'def (first %)))) first (drop 2) first))

(defn ns-doc [forms]
  (let [form (first forms)
        doc (when (and (seq? form) (= 'ns (first form))) (nth form 2 nil))]
    (when (string? doc) doc)))

(defn pretty
  "A map one entry per line (an args map's entries are long), anything else as pr-str."
  [value]
  (if-not (map? value)
    (pr-str value)
    (str "{" (str/join "\n " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) value)) "}")))

(defn repo-path
  "engine/src/... from an absolute file path."
  [file]
  (str "engine/src/" (second (re-find #"/engine/src/(.+)$" (str/replace (.getPath file) "\\" "/")))))

(defn entry-of-forms [kind category ns-sym file forms]
  (let [args (def-value "args" forms)
        doc (def-value "doc" forms)]
    {:kind kind
     :id (str ns-sym)
     :category category
     :name (str/join "." (drop 2 (str/split (str ns-sym) #"\.")))
     :file (repo-path file)
     :ns-doc (ns-doc forms)
     :doc (when (string? doc) doc)
     :args (when (some? args) (pretty args))
     :backoff (boolean (some #(defining? "backoff" %) forms))}))

(defn entry
  "The registry entry of one source file; ns-sym is the namespace it must define. A file engine.registry cannot
  read still gets an entry, with :error (the page shows it)."
  [kind category ns-sym file]
  (try (entry-of-forms kind category ns-sym file (registry/read-forms file))
       (catch Exception e
         {:kind kind :id (str ns-sym) :category category
          :name (str/join "." (drop 2 (str/split (str ns-sym) #"\.")))
          :file (repo-path file) :error (str "unreadable: " (ex-message e))})))

(defn job-entries []
  (when-let [dir (registry/jobs-dir)]
    (for [file (registry/job-files dir)
          :let [ns-sym (registry/expected-ns dir file)]]
      (entry :job (second (str/split (str ns-sym) #"\.")) ns-sym file))))

(defn trigger-entries []
  (when-let [dir (some-> (clojure.java.io/resource "engine/triggers") clojure.java.io/file)]
    (for [file (registry/job-files dir)
          :let [ns-sym (symbol (str "engine.triggers." (-> (.getName file)
                                                           (str/replace #"\.clj[sc]$" "")
                                                           (str/replace "_" "-"))))]]
      (entry :trigger "triggers" ns-sym file))))

(defmacro compile-entries
  "A literal vector of every job and trigger entry, read when the build runs."
  []
  (vec (concat (job-entries) (trigger-entries))))
