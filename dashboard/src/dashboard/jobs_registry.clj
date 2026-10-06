(ns dashboard.jobs-registry
  "Compile time: the engine's job and trigger namespaces as data. The files are found and read by
  engine.registry (engine/src is on the source path, only its .clj is loaded); this ns adds kind, category and
  name and prints the args."
  (:require [clojure.string :as str]
            [engine.registry :as registry]))

(defn pretty
  "A map one entry per line (an args map's entries are long), anything else as pr-str."
  [value]
  (if-not (map? value)
    (pr-str value)
    (str "{" (str/join "\n " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) value)) "}")))

(defn entry
  "A registry-metadata map as a page entry: kind, category and name added, args printed. :error entries stay
  (the page shows them)."
  [kind category m]
  (merge {:kind kind :category category :name (str/join "." (drop 2 (str/split (:id m) #"\.")))}
         (update m :args #(when (some? %) (pretty %)))))

(defn job-entries []
  (for [m (registry/job-metadata)]
    (entry :job (second (str/split (:id m) #"\.")) m)))

(defn trigger-entries
  "One entry per trigger of the engine's default trigger set (triggers/defaults.edn): the trigger fn's doc and file."
  []
  (for [m (registry/trigger-metadata)]
    (merge {:kind :trigger :category "triggers" :name (:id m)}
           (update m :args #(when (some? %) (pretty %))))))

(defmacro compile-entries
  "A literal vector of every job and trigger entry, read when the build runs."
  []
  (vec (concat (job-entries) (trigger-entries))))
