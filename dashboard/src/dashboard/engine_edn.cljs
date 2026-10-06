(ns dashboard.engine-edn
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(defn read-edn [text]
  (try
    {:value (reader/read-string text)}
    (catch :default e {:error (str "unreadable engine.edn: " (ex-message e))})))

(defn job-name [sym]
  (if (keyword? sym) (name sym) (str sym)))

(defn label [spec]
  (case (:op spec)
    :leaf (job-name (:job spec))
    :repeat (str "repeat " (label (:child spec)))
    (:seq :any) (str (name (:op spec)) "(" (str/join ", " (map label (:children spec))) ")")
    (str (:op spec))))

(defn summarize-job [current instance]
  {:id (:id instance)
   :label (label (:spec instance))
   :current? (= current (:id instance))
   :hold? (boolean (:hold? instance))
   :round (:round instance)})

(defn summarize-reflex [reflex-state now entry]
  {:id (:id entry)
   :trigger (:trigger entry)
   :job (job-name (if (sequential? (:job entry)) (first (:job entry)) (:job entry)))
   :persistence (:persistence entry)
   :cooldown-s (:cooldown-s entry)
   :cooling? (> (or (get-in reflex-state [(:id entry) :cooldown-until]) 0) now)})

(defn ordered-instances [{:keys [instances list]}]
  (let [listed (keep instances list)
        listed-ids (set (map :id listed))]
    (concat listed (remove #(listed-ids (:id %)) (vals instances)))))

(defn read-text
  "read-edn of a file's text; nil or blank text is an error result."
  [edn-text]
  (if (str/blank? edn-text) {:error "engine.edn is empty"} (read-edn edn-text)))

(defn summarize-read
  "summarize over an already parsed read-text result (the parse is the costly part and can be cached)."
  [{:keys [value error]} now]
  (cond
    error {:error error}
    (not (map? value)) {:error "engine.edn is not a map"}
    :else
    {:jobs (mapv #(summarize-job (:current value) %) (ordered-instances value))
     :reflexes (mapv #(summarize-reflex (:reflex-state value) now %) (:register value))
     :current (:current value)
     :failed-count (count (:failed value))}))

(defn summarize [edn-text now]
  (summarize-read (read-text edn-text) now))
