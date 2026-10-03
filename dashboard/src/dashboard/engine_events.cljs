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

(def empty-engine {:last nil :pos nil :job nil :reflex nil :recent [] :warns [] :signals {}})

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

;; The last lifecycle event that says the body is gone: a stop (system.stopping), a disconnect or kick of the session,
;; a failed reconnect. A later body.spawned / body.online or system.started ends it.
(defn offline-event? [{:keys [source kind]}]
  (or (and (= "system" source) (= "stopping" kind))
      (and (= "body" source) (#{"disconnected" "kicked"} kind))
      (= "reconnect-failed" kind)))

;; What the trouble rules read, kept even for debug events that never reach :recent:
;; {:hurt-t :died-t :backoffs {key t} :stuck-t :stuck-open? :takeover? :takeover-who :takeover-t :offline? :online-t}
(defn next-signals [signals e]
  (let [{:keys [source kind t]} e
        backoff-key (or (:name e) (:reflex e) (:job e))]
    (cond
      (system-started? e) {:online-t t}
      (and (= "body" source) (#{"spawned" "online"} kind)) (assoc signals :offline? false :online-t t)
      (offline-event? e) (assoc signals :offline? true)
      (and (= "body" source) (= "hurt" kind)) (assoc signals :hurt-t t)
      (and (= "body" source) (= "died" kind)) (assoc signals :died-t t)
      (and (#{"job" "reflex"} source) (= "backoff" kind)) (assoc-in signals [:backoffs backoff-key] t)
      (and (#{"job" "reflex"} source) (= "recovered" kind)) (update signals :backoffs dissoc backoff-key)
      (and (= "reflex" source) (= "stuck" (:reflex e)) (= "fired" kind)) (assoc signals :stuck-t t :stuck-open? true)
      (and (= "reflex" source) (= "stuck" (:reflex e)) (= "ended" kind)) (assoc signals :stuck-open? false)
      (= "unstick.failed" kind) (assoc signals :stuck-t t)
      (and (= "system" source) (= "takeover_started" kind)) (assoc signals :takeover? true :takeover-who (:who e) :takeover-t t)
      (and (= "system" source) (= "takeover_ended" kind)) (-> signals (assoc :takeover? false) (dissoc :takeover-who :takeover-t))
      :else signals)))

(defn noteworthy? [e]
  (and (levels-shown (:level e))
       (not (and (= "job" (:source e)) (heartbeats (:kind e))))))

(defn fold-one [state e]
  (-> state
      (assoc :last {:t (:t e) :seq (:seq e)}
             :pos (or (:pos e) (:pos state))
             :job (next-job (:job state) e)
             :reflex (next-reflex (:reflex state) e)
             :signals (next-signals (:signals state) e))
      (update :recent #(if (noteworthy? e)
                         (vec (take-last recent-max (conj % {:t (:t e) :level (:level e) :source (:source e) :kind (:kind e) :text (describe e)})))
                         %))
      (update :warns #(if (#{"warn" "error"} (:level e)) (conj % {:t (:t e) :level (:level e)}) %))))

;; Warnings older than the window are dropped; a state that gained nothing is returned as it was.
(defn trim-warns [state next]
  (let [from (- (get-in next [:last :t] 0) warn-window-ms)]
    (if (identical? next state)
      state
      (update next :warns #(filterv (fn [w] (> (:t w) from)) %)))))

;; events: oldest first. Returns a new state.
(defn fold-engine [state events]
  (trim-warns state (reduce fold-one state events)))

(defn engine-view [state now]
  (if-not (:last state)
    {:up false :error "no events yet" :at nil :age-ms nil :job nil :reflex nil :pos nil :recent [] :warn10m 0 :error10m 0 :signals {}}
    (let [age-ms (max 0 (- now (get-in state [:last :t])))
          offline? (boolean (:offline? (:signals state)))
          up (and (< age-ms engine-up-ms) (not offline?))
          warns (filter #(> (:t %) (- now warn-window-ms)) (:warns state))
          count-level (fn [level] (count (filter #(= level (:level %)) warns)))]
      {:up up
       :error (cond up nil
                    offline? "disconnected"
                    :else (str "last event " (js/Math.round (/ age-ms 1000)) "s ago"))
       :at (get-in state [:last :t])
       :age-ms age-ms
       :job (:job state)
       :reflex (:reflex state)
       :pos (:pos state)
       :recent (:recent state)
       :warn10m (count-level "warn")
       :error10m (count-level "error")
       :signals (:signals state)})))

;; pose.json's status "offline", written after the connect, also takes a body down (the pose is newer than the last spawn).
(defn with-view-status [engine pose]
  (if (and (:up engine)
           (= "offline" (:status pose))
           (number? (:poseMtimeMs pose))
           (> (:poseMtimeMs pose) (or (get-in engine [:signals :online-t]) 0)))
    (assoc engine :up false :error "view offline")
    engine))

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

;; a read that starts mid-file begins inside a line that may be longer than one chunk: skip to its newline.
;; state {:rest bytes of a line still being written, :skipping? still inside the torn head}
(defn split-chunk [{:keys [rest skipping?]} chunk]
  (let [i (if skipping? (.indexOf chunk newline-byte) 0)
        starts-whole? (not (neg? i))
        usable (cond (not skipping?) chunk starts-whole? (.subarray chunk (inc i)) :else (js/Uint8Array. 0))]
    (assoc (complete-lines rest usable) :skipping? (not starts-whole?))))

(defn parse-line [line]
  (let [parsed (try (js/JSON.parse line) (catch :default _ nil))]
    (when (and (some? parsed) (identical? "object" (goog/typeOf parsed)) (not (array? parsed)))
      [(js->clj parsed :keywordize-keys true)])))

;; line-test: a cheap string test run before parsing; lines it rejects are never JSON-parsed
(defn parse-event-lines
  ([text] (parse-event-lines text any?))
  ([text line-test]
   (into [] (comp (remove empty?) (filter line-test) (mapcat parse-line)) (.split text "\n"))))

;; What fold-one reads, and nothing else: inventories, args and path dumps are never converted.
(def fold-fields ["t" "seq" "who" "pos" "job" "chain" "source" "kind" "name" "reflex" "level" "text" "error"])

(defn fold-event [o]
  (reduce (fn [m k]
            (let [v (aget o k)]
              (if (undefined? v) m (assoc m (keyword k) (js->clj v :keywordize-keys true)))))
          {}
          fold-fields))

(defn parse-fold-line [line]
  (let [parsed (try (js/JSON.parse line) (catch :default _ nil))]
    (when (and (some? parsed) (identical? "object" (goog/typeOf parsed)) (not (array? parsed)))
      [(fold-event parsed)])))

;; fold-engine over the lines of a text, one line at a time: no vector of events
(defn fold-text [state text]
  (trim-warns state (transduce (comp (remove empty?) (mapcat parse-fold-line)) (completing fold-one) state (.split text "\n"))))

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

;; ---------------------------------------------------------------- the action log
;; What a body's popup lists: the events people read, not memory saves, heartbeats and path debug.
(defn log-worthy? [e]
  (let [{:keys [source kind level]} e]
    (cond
      (= "memory" source) false
      (= "memory_written" kind) false
      (and (= "job" source) (heartbeats kind)) false
      (and (= "body" source) (= "view.stats" kind)) false
      (= "action" source) true
      :else (boolean (levels-shown level)))))

;; only the fields the log shows: inventories and path dumps in other events never leave the server
(defn log-entry [e]
  {:t (:t e) :seq (:seq e) :level (:level e) :source (:source e) :kind (:kind e)
   :name (:name e) :text (:text e) :error (:error e) :args (:args e) :reflex (:reflex e) :ms (:ms e)})

(defn log-tail
  "The last n log-worthy events of a list, oldest first."
  [events n]
  (vec (take-last n (filter log-worthy? events))))
