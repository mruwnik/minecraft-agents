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

(defn summarize [edn-text now]
  (let [{:keys [value error]} (if (str/blank? edn-text) {:error "engine.edn is empty"} (read-edn edn-text))]
    (cond
      error {:error error}
      (not (map? value)) {:error "engine.edn is not a map"}
      :else
      {:jobs (mapv #(summarize-job (:current value) %) (ordered-instances value))
       :reflexes (mapv #(summarize-reflex (:reflex-state value) now %) (:register value))
       :current (:current value)
       :failed-count (count (:failed value))
       :pending-reflex (:pending-reflex value)})))
