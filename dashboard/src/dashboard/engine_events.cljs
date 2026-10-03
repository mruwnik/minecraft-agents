(ns dashboard.engine-events
  "What the dashboard knows about an ENGINE body: everything comes from the events it appends to
  state/agents/<name>/engine/events.jsonl. Pure; fs access stays in dashboard.server.")

;; The longest gap between two events inside one run was ~10 s (a restart gap is 60 s+), so 30 s is 3x margin.
(def engine-up-ms 30000)
(def warn-window-ms 600000)
(def recent-max 10)

;; job.round_started / job.yielded arrive at info every 0.25-10 s per running job: noise in a "what happened" list
(def heartbeats #{"round_started" "yielded"})
(def job-ends #{"completed" "failed" "cancelled"})
(def levels-shown #{"info" "warn" "error"})

(def empty-engine {:last nil :pos nil :job nil :reflex nil :recent [] :warns []})

(defn describe [e]
  (or (:text e) (:error e)
      (str (:source e) "." (:kind e) (when (:name e) (str " " (:name e))))))

(defn system-started? [e]
  (and (= "system" (:source e)) (= "started" (:kind e))))

;; only a top-level job (its own id is the head of its chain) is "the current job"; a queued job is not running yet
(defn next-job [job e]
  (cond
    (system-started? e) nil
    (or (not= "job" (:source e)) (= "queued" (:kind e)) (not (:job e)) (not= (first (:chain e)) (:job e))) job
    (job-ends (:kind e)) (when-not (= (:id job) (:job e)) job)
    :else {:id (:job e)
           :name (or (:name e) (when (= (:id job) (:job e)) (:name job)))}))

(defn next-reflex [reflex e]
  (cond
    (system-started? e) nil
    (not= "reflex" (:source e)) reflex
    (= "fired" (:kind e)) (or (:reflex e) reflex)
    (= "ended" (:kind e)) (when-not (= (:reflex e) reflex) reflex)
    :else reflex))

(defn noteworthy? [e]
  (and (levels-shown (:level e))
       (not (and (= "job" (:source e)) (heartbeats (:kind e))))))

(defn fold-one [state e]
  (-> state
      (assoc :last {:t (:t e) :seq (:seq e)}
             :pos (or (:pos e) (:pos state))
             :job (next-job (:job state) e)
             :reflex (next-reflex (:reflex state) e))
      (update :recent #(if (noteworthy? e)
                         (vec (take-last recent-max (conj % {:t (:t e) :level (:level e) :source (:source e) :kind (:kind e) :text (describe e)})))
                         %))
      (update :warns #(if (#{"warn" "error"} (:level e)) (conj % {:t (:t e) :level (:level e)}) %))))

;; events: oldest first. Returns a new state.
(defn fold-engine [state events]
  (let [next (reduce fold-one state events)
        from (- (get-in next [:last :t] 0) warn-window-ms)]
    (if (identical? next state)
      state
      (update next :warns #(filterv (fn [w] (> (:t w) from)) %)))))

(defn engine-view [state now]
  (if-not (:last state)
    {:up false :error "no events yet" :at nil :age-ms nil :job nil :reflex nil :pos nil :recent [] :warn10m 0 :error10m 0}
    (let [age-ms (max 0 (- now (get-in state [:last :t])))
          up (< age-ms engine-up-ms)
          warns (filter #(> (:t %) (- now warn-window-ms)) (:warns state))
          count-level (fn [level] (count (filter #(= level (:level %)) warns)))]
      {:up up
       :error (when-not up (str "last event " (js/Math.round (/ age-ms 1000)) "s ago"))
       :at (get-in state [:last :t])
       :age-ms age-ms
       :job (:job state)
       :reflex (:reflex state)
       :pos (:pos state)
       :recent (:recent state)
       :warn10m (count-level "warn")
       :error10m (count-level "error")})))

;; ---------------------------------------------------------------- reading the file in pieces
(def newline-byte 10)

(defn concat-bytes [a b]
  (let [out (js/Uint8Array. (+ (.-length a) (.-length b)))]
    (.set out a 0)
    (.set out b (.-length a))
    out))

;; the bytes up to and including the last newline are whole lines; the rest is a line still being written
(defn complete-lines [carry chunk]
  (let [bytes (concat-bytes carry chunk)
        cut (inc (.lastIndexOf bytes newline-byte))]
    {:complete (.subarray bytes 0 cut) :rest (.subarray bytes cut)}))

;; a read that starts mid-file begins inside some line: drop it
(defn drop-torn-head [bytes]
  (let [i (.indexOf bytes newline-byte)]
    (.subarray bytes (if (neg? i) (.-length bytes) (inc i)))))

(defn decode-bytes [bytes]
  (.decode (js/TextDecoder.) bytes))

(defn parse-line [line]
  (let [parsed (try (js/JSON.parse line) (catch :default _ nil))]
    (when (and (some? parsed) (identical? "object" (goog/typeOf parsed)) (not (array? parsed)))
      [(js->clj parsed :keywordize-keys true)])))

(defn parse-event-lines [text]
  (into [] (mapcat parse-line) (remove empty? (.split text "\n"))))

;; ---------------------------------------------------------------- agents and bodies
;; entries: [{:name :text raw config.json}]. No apiPort is needed (the engine serves none).
(defn parse-engine-agents [entries]
  (->> entries
       (map (fn [{:keys [name text]}]
              (let [cfg (let [c (try (js->clj (js/JSON.parse text) :keywordize-keys true) (catch :default _ nil))]
                          (when (map? c) c))]
                {:name name :username (or (:username cfg) name) :world (:world cfg)})))
       (sort-by :name)
       vec))

;; the shape every body in /api/state has, so scoping, grouping and the map treat all alike
(defn engine-body [agent view]
  (assoc agent
         :up (:up view)
         :error (:error view)
         :at (:at view)
         :state (when (:pos view) {:pos (:pos view)})
         :engine view))

;; an agent folder with no engine/events.jsonl is an old HTTP-API body: listed, never contacted
(defn unsupported-body [agent]
  (assoc agent :up false :error "not an engine body (unsupported)" :at nil :state nil :engine nil))
