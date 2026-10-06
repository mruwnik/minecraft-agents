(ns engine.core.base
  "The engine's shared plumbing: the empty state, cuts as errors, accessors of the engine map, events,
  state and memory saves, ids and instances, and small predicates (paused?, stopped-result?, wait-reason)."
  (:require [engine.composite :as composite]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.memory :as mem]
            ["path" :as path]))

(def default-idle-s
  "Seconds a round may hold the body with no act in flight and no declared hold before a job.idle warn."
  10)

(def default-stats-ms
  "How often tick! emits the memory.save-stats summary."
  60000)

(def default-sweep-ms
  "How often tick! sweeps and saves memory."
  60000)

(def empty-state
  {:list [] :instances {} :register [] :changes {} :reflex-state {}
   :deferred-ends [] :cursor 0 :resume nil :current nil :failed {}
   :attention {} :next-id 1})

(defn cut-error []
  (doto (js/Error. "cut: the ownership token changed") (aset "code" "cut")))

(defn offline-cut-error
  "The cut an act raises when its primitive answers offline (the connection dropped): the round ends, the job stays
  listed and resumes once the body is back."
  []
  (doto (js/Error. "cut: the body lost its connection; the job resumes once it is back") (aset "code" "cut")))

(defn cut? [e]
  (and (instance? js/Error e) (= "cut" (.-code e))))

(defn remove-listed [state id]
  (let [idx (.indexOf (:list state) id)]
    (cond-> (-> state
                (update :list #(filterv (fn [x] (not= id x)) %))
                (update :instances dissoc id)
                (update :failed dissoc id))
      (and (>= idx 0) (< idx (:cursor state))) (update :cursor dec)
      (= id (:resume state)) (assoc :resume nil)
      (= id (:current state)) (assoc :current nil))))

(defn normalize-result [r]
  (if (#{:done :continue :declined} r)
    {:status r}
    {:status :error :error (js/Error. (str "a round returned " (pr-str r) ", not :done, :continue or :declined"))}))

(defn state [eng] @(:state eng))

(defn running [eng] @(:running eng))

(defn now [eng] ((:now eng)))

(defn job-of
  "[def args] of instance inst, from its parsed spec."
  [eng inst]
  (composite/job (:jobs eng) (:spec inst)))

(defn trigger-def [eng trigger]
  (or (get-in eng [:triggers trigger])
      (throw (ex-info (str "unknown trigger " trigger) {:trigger trigger}))))

(defn emit! [eng event]
  (events/emit! (:events eng) event))

(defn job-fields [eng id]
  (let [inst (get-in (state eng) [:instances id])]
    {:job id :chain [id] :round (:round inst) :reflex (:reflex inst)
     :name (some-> (:spec inst) expr/label)}))

(defn set-owner! [eng token]
  (.setOwner (:primitives eng) token))

(defn new-id! [eng]
  (let [n (:next-id (state eng))]
    (swap! (:state eng) update :next-id inc)
    (str "j" n)))

(defn add-instance [state id node opts]
  (assoc-in state [:instances id] (merge {:id id :spec node :round 0} opts)))

(def empty-save-stats {:count 0 :bytes 0 :ms 0 :max-ms 0})

(defn record-save!
  "Measure a state or memory save: add it to the running summary that
  flush-save-stats! reports. saved is the {:bytes :ms} fsutil/write-edn!
  returned for file."
  [eng file {:keys [bytes ms]}]
  (swap! (:save-stats eng) #(-> %
                                (update :count inc)
                                (update :bytes + bytes)
                                (update :ms + ms)
                                (update :max-ms max ms))))

(defn save-file!
  "Run write! (a fsutil/write-edn! of file) and record the save. A failed write
  emits a warn memory.save-failed event with the error, then rethrows."
  [eng file write!]
  (let [saved (try
                (write!)
                (catch :default e
                  (emit! eng {:source :memory :kind :save-failed :level :warn
                              :file (path/basename file) :error (or (.-message e) (str e))})
                  (throw e)))]
    (record-save! eng file saved)))

(defn flush-save-stats!
  "Emit the info memory.save-stats summary of the saves since the last one and
  start a new window."
  [eng]
  (let [stats @(:save-stats eng)]
    (reset! (:save-stats eng) empty-save-stats)
    (emit! eng (merge {:source :memory :kind :save-stats :level :info} stats))))

(defn save-memory! [eng]
  (save-file! eng (mem/file (:dir eng)) #(mem/save! (:store eng))))

(defn job-memory [eng id]
  (mem/job-mem (mem/view (:store eng)) id []))

(defn drop-instance! [eng id]
  (swap! (:state eng) update :instances dissoc id)
  (mem/delete-job! (:store eng) id)
  (save-memory! eng))

(defn distance
  "Straight-line distance between positions a and b ({:x :y :z}); nil when either is."
  [a b]
  (when (and a b)
    (Math/hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b)))))

(defn owner? [eng token]
  (and (some? token) (.isOwner (:primitives eng) token)))

(def guard-warned
  "[dir what] -> {:msg :t} of the last call-guarded warn, so one broken predicate warns once, not every tick."
  (atom {}))

(def guard-repeat-ms 60000)

(defn call-guarded
  "Call f; on a throw emit a warn and return fallback. For user predicates. The same error from the same
  caller warns again only after a minute."
  [eng what fallback f]
  (try
    (f)
    (catch :default e
      (let [k [(:dir eng) what]
            msg (str e)
            t (now eng)
            {last-msg :msg last-t :t} (get @guard-warned k)]
        (when (or (not= msg last-msg) (>= (- t last-t) guard-repeat-ms))
          (swap! guard-warned assoc k {:msg msg :t t})
          (emit! eng {:source :system :kind :error :level :warn :text (str what " threw: " msg)})))
      fallback)))

(defn wait-reason
  "A check's reason as a map with :reason: a keyword r is {:reason r}; no reason (a plain false) is :not-ready."
  [r]
  (cond (map? r) r
        (some? r) {:reason r}
        :else {:reason :not-ready}))

(defn waiting-text [{:keys [reason] :as r}]
  (let [more (dissoc r :reason)]
    (str "waiting: " (if (keyword? reason) (name reason) reason) (when (seq more) (str " " (pr-str more))))))

(defn stopped-result?
  "True when a job that ended :done handed over a result of status :stopped (it gave up; not a success)."
  [result]
  (and (map? result) (= :stopped (:status result))))

(defn reflex-text
  "\"burning → jobs.survival.extinguish\": the reflex id and its job's label."
  [reflex node]
  (str (name reflex) " → " (expr/label node)))

(defn offline?
  "Whether the body is away from the server (the offline primitive). The register
  and the list are paused meanwhile: sensing would only say offline."
  [eng]
  (true? (.isOffline (:primitives eng))))

(defn settling?
  "Whether the body is connected but its senses are not trustworthy yet (just
  after a login, reconnect, respawn or teleport). Treated like offline: nothing
  is evaluated, and reflex ends are not judged."
  [eng]
  (true? (.isSettling (:primitives eng))))

(defn manual?
  "Whether someone drives the body by hand (see engine.takeover). Not persisted."
  [eng]
  (some? @(:manual eng)))

(defn paused?
  "Whether the scheduler must stand still: offline, settling or under manual control."
  [eng]
  (or (offline? eng) (settling? eng) (manual? eng)))

(defn self-pos
  "The body's position, or nil when self() has none (the offline record is just {status: 'offline'})."
  [p]
  (when-let [pos (.-pos (.self p))]
    {:x (.-x pos) :y (.-y pos) :z (.-z pos)}))
