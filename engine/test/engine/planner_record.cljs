(ns engine.planner-record
  "Re-records test/planner-bench.json from the ClojureScript planner (for an intended planner change) and prints what
  changed against the file it replaces. Run by `npm run record:planner-bench`. The world answers need the frozen bench
  (engine.planner-bench-test/bench-dir); without it they are kept as recorded and the summary says so."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [engine.planner-bench-test :as bench :refer [recorded record changes]]
            [engine.planner-fixture :as pf]
            [engine.planner-options-test :as options-test]))

(defn rounded [x] (if (number? x) (/ (js/Math.round (* x 1000)) 1000) x))

(defn recorded-answer
  "a planner result as the file stores it: seconds and risk to 3 decimals"
  [r]
  (let [[status reason seconds risk end expanded] (record r)]
    [status reason (rounded seconds) (rounded risk) end expanded]))

(defn ordered
  "a JS object with the pairs in order (a cljs map keeps no order past 8 keys, and the file is diffed as text)"
  [pairs]
  (let [o (js-obj)]
    (doseq [[k v] pairs] (aset o (name k) (clj->js v)))
    o))

(defn by-key
  "{key (recorded-answer (plan key))} over keys, as an ordered JS object"
  [ks plan]
  (ordered (map (fn [k] [k (recorded-answer (plan k))]) ks)))

(defn file-order
  "pairs [[key answer]] with the keys of the section in the order the file has them, new keys last"
  [section pairs]
  (let [raw (js/JSON.parse (fs/readFileSync (path/join (js/process.cwd) "test/planner-bench.json") "utf8"))
        known (js->clj (js/Object.keys (aget raw (name section))))
        m (into {} (map (fn [[k v]] [(name k) v])) pairs)]
    (ordered (concat (map (fn [k] [k (m k)]) (filter m known)) (remove (comp (set known) name first) pairs)))))

(defn world-answers []
  (let [qs (bench/world-queries)
        by-id (into {} (map (juxt :id identity)) qs)]
    (when (seq qs)
      (for [id (map :id qs)]
        [id (recorded-answer (let [{:keys [snapshot query]} (by-id id)] (pf/plan snapshot query)))]))))

(defn new-recording []
  (let [old @recorded
        samples (:samples old)
        world (world-answers)]
    {:recording (assoc old
                       :world (file-order :world (or world (map (fn [[k v]] [(name k) v]) (:world old))))
                       :course (file-order :course (map (fn [n] [n (recorded-answer (pf/course-plan n))]) (bench/course-names)))
                       :options (ordered (for [[what opts] (:optionSets old)
                                               :let [opts (js->clj (clj->js opts) :keywordize-keys true)]]
                                           [what (by-key samples #(options-test/plan-with % opts))]))
                       :goals (file-order :goals
                                          (for [name samples [variant make] options-test/goal-variants
                                                :let [{:keys [snapshot query]} (make (options-test/course-query name))]]
                                            [(str name "|" variant) (recorded-answer (pf/plan snapshot query))])))
     :world-kept? (nil? world)}))

(defn summary
  "lines naming, per section, how many answers changed, were added or removed, and which"
  [before after]
  (mapcat (fn [section]
            (let [flat (fn [m] (into {} (for [[k v] (get m section) [k2 v2] (if (map? v) v {nil v})]
                                          [(if k2 [k k2] k) v2])))
                  {:keys [added removed changed]} (changes (flat before) (flat after))]
              (cons (str (name section) ": " (count changed) " changed, " (count added) " added, " (count removed) " removed")
                    (concat (for [[id o n] changed] (str "  " id "  " (pr-str o) " -> " (pr-str n)))
                            (for [id added] (str "  + " id))
                            (for [id removed] (str "  - " id))))))
          [:world :course :options :goals]))

(defn main [& _]
  (let [file (path/join (js/process.cwd) "test/planner-bench.json")
        {:keys [recording world-kept?]} (new-recording)]
    (when world-kept? (println "no frozen bench at" (bench/bench-dir) ": the world answers are kept as recorded"))
    (println (str/join "\n" (summary @recorded (js->clj (clj->js recording) :keywordize-keys true))))
    (fs/writeFileSync file (js/JSON.stringify (clj->js recording)))
    (println "wrote" file)))
