(ns ^:dev/always dashboard.jobs-registry
  "The engine's jobs and triggers, fixed at build time (see jobs_registry.clj), and the usage join (which bodies
  run a job)."
  (:require-macros [dashboard.jobs-registry :refer [compile-entries]]))

(def entries
  "[{:kind :job|:trigger :id :category :name :file :ns-doc :doc :args}] in path order."
  (compile-entries))

(defn pretty
  "A map one entry per line (an args map's entries are long), anything else as pr-str."
  [value]
  (if-not (map? value)
    (pr-str value)
    (str "{" (apply str (interpose "\n " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) value))) "}")))

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
