(ns agent-tools.observe.events
  "observe.mjs events: a bounded, paged look at the retained past events (the post-mortem of a death, a job, a reflex)."
  (:require [agent-tools.job-results :as job-results]
            [cljs.reader :as reader]
            [clojure.string :as str]
            ["node:fs/promises" :as fsp]
            ["node:path" :as path]))

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
           (or (empty? kinds) (let [n (name (:kind e))] (some #(or (= % n) (str/ends-with? n (str "." %))) kinds)))
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

(defn log-files
  "The kept log files of the events.sock in dir, oldest first: events.edn.N ... events.edn.1, events.edn."
  [names]
  (let [rank #(if-let [n (second (re-matches #"events\.edn\.(\d+)" %))] (- (js/parseInt n)) 1)]
    (->> names (filter #(re-matches #"events\.edn(\.\d+)?" %)) (sort-by rank))))

(defn line-events
  "The events of the lines of text that hold one of needles; a line that does not read as a map (a cut one) is left out."
  [text needles]
  (->> (str/split-lines text)
       (filter (fn [line] (some #(str/includes? line %) needles)))
       (keep (fn [line] (let [e (try (reader/read-string line) (catch :default _ nil))] (when (map? e) e))))))

(defn ^:async kept-events!
  "The events of the given kinds (and deaths, for --since/--until death) in every kept log file next to the socket,
  or nil when there is no log there. A substring test on each line skips the parse of the rest."
  [socket-path kinds]
  (let [dir (.dirname path socket-path)
        names (try (log-files (await (.readdir fsp dir))) (catch :default _ nil))
        needles (conj (set kinds) ":kind :died")]
    (when (seq names)
      (loop [[f & more] names out []]
        (if-not f
          out
          (let [text (try (await (.readFile fsp (.join path dir f) "utf8")) (catch :default _ ""))]
            (recur more (into out (line-events text needles)))))))))

(def hurt-tail-bytes 262144)
(def hurt-window-ms 120000)
(def hurt-shown 3)

(defn hurt-text
  "The newest hurt-shown :hurt events within hurt-window-ms of now-ms as \"cause amount Ns ago, ...\", newest first, else nil."
  [events now-ms]
  (let [hits (->> events
                  (filter #(and (= :body (:source %)) (= :hurt (:kind %)) (<= 0 (- now-ms (:time-ms %)) hurt-window-ms)))
                  (sort-by :seq) (take-last hurt-shown) reverse)]
    (when (seq hits)
      (str/join ", " (map (fn [{:keys [data time-ms]}]
                            (str (str/join "+" (:causes data)) " " (:amount data) " "
                                 (js/Math.round (/ (- now-ms time-ms) 1000)) "s ago"))
                          hits)))))

(defn ^:async recent-hurt!
  "hurt-text of the hurt events in the last hurt-tail-bytes of the current log next to the socket (a rotated events.edn.1 is not read), else nil."
  [socket-path now-ms]
  (let [file (.join path (.dirname path socket-path) "events.edn")
        text (try
               (let [h (await (.open fsp file "r"))]
                 (try
                   (let [size (.-size (await (.stat h)))
                         start (max 0 (- size hurt-tail-bytes))
                         buf (js/Buffer.alloc (- size start))]
                     (await (.read h buf 0 (.-length buf) start))
                     (.toString buf "utf8"))
                   (finally (await (.close h)))))
               (catch :default _ nil))]
    (when text
      (hurt-text (line-events text [":kind :hurt"]) now-ms))))

(defn ^:async read!
  "The query's answer from the retained history (job-results/history! with the result window; a --job's recorded end stands in when its stopped event has aged out): {:ok true :events .. :next-before ..
  :window {:scanned n :oldest-seq s :partial? bool}}, or {:ok false :reason :no-death-retained}."
  [get! socket-path {:keys [since until job] :as query} now-ms]
  (let [kept (when (and (seq (:kinds query)) (nil? job)) (await (kept-events! socket-path (:kinds query))))
        history (if kept
                  {:events kept}
                  (await (job-results/history! get! socket-path (cond-> {:limit job-results/result-limit}
                                                                  job (assoc :record-ids [job])))))
        events (cond-> (:events history)
                 job (job-results/with-recorded (get-in history [:snapshot :generation-id]) (:recorded history) [job]))
        since-ms (when since (resolve-time since events now-ms))
        until-ms (when until (resolve-time until events now-ms))]
    (if (or (and since (nil? since-ms)) (and until (nil? until-ms)))
      {:ok false :reason :no-death-retained :window {:scanned (count events)}}
      (merge {:ok true}
             (select events (assoc query :since-ms since-ms :until-ms until-ms))
             {:window (window history events)}))))
