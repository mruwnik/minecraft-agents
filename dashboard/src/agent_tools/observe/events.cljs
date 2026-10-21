(ns agent-tools.observe.events
  "observe.mjs events: a bounded, paged look at the retained past events (the post-mortem of a death, a job, a reflex)."
  (:require [agent-tools.job-results :as job-results]))

(def default-limit 20)
(def max-limit 64)
(def levels {:debug 0 :info 1 :warn 2 :error 3})

(defn death-time
  "The :time-ms of the newest :died event among events, else nil."
  [events]
  (some->> events (filter #(and (= :body (:source %)) (= :died (:kind %)))) (sort-by :seq) last :time-ms))

(defn view
  "One event as the tool prints it."
  [e]
  (let [{:keys [value truncated?]} (job-results/bounded (:data e))]
    (cond-> {:seq (:seq e) :time-ms (:time-ms e) :source (:source e) :kind (:kind e)}
      (not= :info (:level e)) (assoc :level (:level e))
      (get-in e [:context :job-id]) (assoc :job (get-in e [:context :job-id]))
      (:message e) (assoc :message (:message e))
      (seq value) (assoc :data value)
      truncated? (assoc :truncated? true))))

(defn matcher
  "A predicate of one event for the query {:kinds :job :level :since-ms :until-ms :before}."
  [{:keys [kinds job level since-ms until-ms before]}]
  (let [min-level (get levels level 1)]
    (fn [e]
      (and (>= (get levels (:level e :info) 1) min-level)
           (or (empty? kinds) (contains? kinds (name (:kind e))))
           (or (nil? job) (= job (get-in e [:context :job-id])) (= job (first (get-in e [:context :chain]))))
           (or (nil? since-ms) (>= (:time-ms e 0) since-ms))
           (or (nil? until-ms) (<= (:time-ms e 0) until-ms))
           (or (nil? before) (< (:seq e) before))))))

(defn select
  "{:events [..] :next-before seq} for the retained events and the query: the newest :limit matches (default
  default-limit), oldest first; :next-before is the seq to pass as --before for the older ones, absent when none remain."
  [events {:keys [limit] :as query}]
  (let [matching (->> events (filter (matcher query)) (sort-by :seq) vec)
        shown (vec (take-last (or limit default-limit) matching))]
    (cond-> {:events (mapv view shown)}
      (< (count shown) (count matching)) (assoc :next-before (:seq (first shown))))))

(defn resolve-time
  "The epoch ms a --since/--until value means: \"death\" is the newest death in events, a duration is that long ago
  from now-ms, a number is itself. nil when it is \"death\" and none is retained."
  [value events now-ms]
  (let [[_ amount unit] (re-matches #"(\d+)(ms|s|m|h)" (str value))]
    (cond
      (= "death" value) (death-time events)
      amount (- now-ms (* (js/Number amount) ({"ms" 1 "s" 1000 "m" 60000 "h" 3600000} unit)))
      :else (js/Number value))))

(defn window
  "The answer's :window: how many events were scanned, from which seq, and (when the log held more) how many older ones were not searched."
  [history events]
  (let [latest (get-in history [:snapshot :cursor :seq])
        unsearched (when (and (:truncated? history) (number? latest) (seq events)) (max 0 (dec (:seq (first events)))))]
    (cond-> {:scanned (count events)}
      (seq events) (assoc :oldest-seq (:seq (first events)))
      (:partial? history) (assoc :partial? true)
      (pos? (or unsearched 0)) (assoc :older-not-searched unsearched
                                      :note (str "only the newest " (count events) " events were searched; " unsearched " older ones were not")))))

(defn ^:async read!
  "The query's answer from the retained history (job-results/history! with the result window; a --job's recorded end stands in when its stopped event has aged out): {:ok true :events .. :next-before ..
  :window {:scanned n :oldest-seq s :partial? bool}}, or {:ok false :reason :no-death-retained}."
  [get! socket-path {:keys [since until job] :as query} now-ms]
  (let [history (await (job-results/history! get! socket-path (cond-> {:limit job-results/result-limit}
                                                                 job (assoc :record-ids [job]))))
        events (cond-> (:events history)
                 job (job-results/with-recorded (get-in history [:snapshot :generation-id]) (:recorded history) [job]))
        since-ms (when since (resolve-time since events now-ms))
        until-ms (when until (resolve-time until events now-ms))]
    (if (or (and since (nil? since-ms)) (and until (nil? until-ms)))
      {:ok false :reason :no-death-retained :window {:scanned (count events)}}
      (merge {:ok true}
             (select events (assoc query :since-ms since-ms :until-ms until-ms))
             {:window (window history events)}))))
