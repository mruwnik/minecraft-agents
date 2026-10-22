(ns agent-tools.observe.catalog
  "observe.mjs catalog jobs (no prefix or filter): the jobs in one answer, by level. Goal jobs (tagged :goal in their
  meta) come with the first sentence of their doc; the other jobs are names by family; phase, reflex and primitive jobs
  are counted, named with --all."
  (:require [agent-tools.world-data :as data]
            [clojure.string :as str]))

(def page-limit 64)
(def doc-chars 160)

(defn names!
  "A promise of every job name the catalog lists for params from offset on (pages of page-limit); rejects on a failed page."
  ([get! socket-path params] (names! get! socket-path params 0))
  ([get! socket-path params offset]
   (let [query (.toString (js/URLSearchParams. (clj->js (merge {:kind "jobs" :prefix "" :limit page-limit :offset offset} params))))]
     (-> (get! socket-path (str "/catalog?" query) {})
         (.then (fn [{:keys [status text]}]
                  (let [page (when (= 200 status) (data/read-edn text))
                        items (mapv symbol (:items page))]
                    (cond
                      (not (:ok page)) (throw (ex-info "catalog list failed" {:status status}))
                      (:next-offset page) (.then (names! get! socket-path params (:next-offset page)) #(into items %))
                      :else items))))))))

(defn first-sentence [doc]
  (let [text (str/trim (str/replace (or doc "") #"\s+" " "))
        sentence (first (str/split text #"(?<=\.) "))]
    (if (> (count sentence) doc-chars) (str (subs sentence 0 (- doc-chars 3)) "...") sentence)))

(defn doc! [get! socket-path sym]
  (-> (get! socket-path (str "/catalog?kind=job&name=" sym) {})
      (.then (fn [{:keys [status text]}] [sym (when (= 200 status) (first-sentence (:doc (data/read-edn text))))]))))

(defn levels
  "The answer from the name lists: every name, those of the phase, reflex and debug categories, and the :goal tag."
  [{:keys [all phase reflex debug goal]} docs all?]
  (let [primitive? #(str/starts-with? (str %) "jobs.primitive.")
        goal-set (set goal)
        other (into goal-set (concat phase reflex debug))
        step (remove #(or (other %) (primitive? %)) all)
        group (fn [names] (if all? (vec (sort names)) (count names)))]
    {:ok true :kind :jobs
     :goal (vec (sort-by first docs))
     :step (into (sorted-map) (map (fn [[family names]] [(keyword family) (mapv #(symbol (last (str/split (str %) #"\."))) names)]))
                 (group-by #(second (str/split (str %) #"\.")) (sort step)))
     :phase (group (remove (some-fn goal-set primitive?) phase))
     :reflex (group (remove goal-set reflex))
     :primitive (group (filter primitive? all))
     :next (str "Submit a goal job; it fetches what it needs. Use a step (jobs.<family>.<name>) only to unblock a goal job, then submit the goal job again. "
                "`catalog job <name>` shows a job's args and doc; `catalog jobs <prefix>` and `--category` page names"
                (when-not all? "; --all names the phase, reflex and primitive jobs") ".")}))

(defn read!
  "A promise of the catalog by level, or of {:ok false :reason :catalog-failed}."
  [get! socket-path {:keys [all]}]
  (-> (js/Promise.all (into-array (map #(names! get! socket-path %)
                                       [{} {:category "phase"} {:category "reflex"} {:category "debug"} {:tag "goal"}])))
      (.then (fn [[all-names phase reflex debug goal]]
               (.then (js/Promise.all (into-array (map #(doc! get! socket-path %) goal)))
                      #(levels {:all all-names :phase phase :reflex reflex :debug debug :goal goal} (vec %) all))))
      (.catch (fn [e] {:ok false :reason :catalog-failed :status (:status (ex-data e))}))))
