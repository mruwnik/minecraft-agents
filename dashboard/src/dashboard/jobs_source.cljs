(ns dashboard.jobs-source
  "A job namespace's source file as data: id, docs, default args, whether it defines backoff. Read leniently like
  engine.registry does: tagged literals and auto-resolved keywords become plain data. Also the usage join (which
  bodies run a job)."
  (:require [cljs.pprint :as pprint]
            [cljs.tools.reader :as reader]
            [cljs.tools.reader.reader-types :as rt]
            [clojure.string :as str]))

(def any-alias
  "An alias map that resolves every alias, so ::alias/kw reads."
  (reify ILookup
    (-lookup [_ _] 'alias)
    (-lookup [_ _ _] 'alias)))

(defn read-forms
  "Every top-level form of a cljs source text."
  [text]
  (binding [*ns* 'user
            reader/*default-data-reader-fn* (fn [_tag v] v)
            reader/*alias-map* any-alias]
    (let [r (rt/indexing-push-back-reader text)
          eof (js-obj)]
      (loop [forms []]
        (let [form (reader/read {:eof eof :read-cond :allow :features #{:cljs}} r)]
          (if (identical? eof form) forms (recur (conj forms form))))))))

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

(defn id-from-file
  "jobs.<dir>.<name> from .../jobs/<dir>/<name>.cljs, with _ as -."
  [file]
  (let [[_ rel] (re-find #"(?:^|/)jobs/(.+)\.clj[sc]$" file)]
    (when rel (str "jobs." (str/replace (str/replace rel "/" ".") "_" "-")))))

(defn pretty
  "A map one entry per line (an args map's entries are long), anything else as pr-str."
  [value]
  (if-not (map? value)
    (pr-str value)
    (str "{" (str/join "\n " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) value)) "}")))

(defn parse-job
  "{:id :category :name :file :ns-doc :doc :args :backoff} or, when the file cannot be read, the same with :error."
  [file text]
  (let [id (id-from-file file)
        parts (str/split id #"\.")
        base {:id id :category (second parts) :name (str/join "." (drop 2 parts)) :file file}]
    (try
      (let [forms (read-forms text)
            args (def-value "args" forms)]
        (merge base
               {:ns-doc (ns-doc forms)
                :doc (let [d (def-value "doc" forms)] (when (string? d) d))
                :args (when (some? args) (pretty args))
                :backoff (boolean (some #(defining? "backoff" %) forms))}))
      (catch :default e
        (assoc base :error (str "unreadable: " (ex-message e)))))))

(defn job-names
  "The job names a spec label mentions: jobs.dir.name occurrences."
  [label]
  (re-seq #"jobs\.[a-z0-9-]+\.[a-z0-9.-]*[a-z0-9]" (or label "")))

(defn usage
  "{job-id {:running [body ...] :reflex [body ...]}} over the engine bodies' job lists and reflex registers."
  [bodies]
  (let [blank {:running [] :reflex []}]
    (->> (for [b bodies
               :let [{:keys [jobs reflexes]} (:engine b)]
               [kind names] [[:running (distinct (mapcat (comp job-names :label) jobs))]
                             [:reflex (distinct (mapcat (comp job-names :job) reflexes))]]
               job names]
           [job kind b])
         (reduce (fn [m [job kind body]] (update-in (update m job #(merge blank %)) [job kind] conj (:name body))) {}))))

(defn attach-usage [jobs by-job]
  (mapv #(merge % {:running [] :reflex []} (get by-job (:id %))) jobs))
