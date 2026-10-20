(ns engine.core.base
  "The engine's shared plumbing: the empty state, cuts as errors, accessors of the engine map, events,
  state and memory saves, ids and instances, and small predicates (paused?, stopped-result?, wait-reason)."
  (:require [engine.args :as a]
            [engine.composite :as composite]
            [engine.contract :as contract]
            [engine.core.tally :as tally]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.fsutil :as fsu]
            [engine.memory :as mem]
            [engine.settings :as settings]
            ["path" :as path]))

(a/defargs settings
  {:engine.core.base/recent-results-limit
   {:default 100 :spec (a/int-in 1 nil)
    :doc "How many ended listed jobs' outcomes the engine keeps (results.edn, served on /job and /results)."}
   :engine.core.base/reflex-results-limit
   {:default 20 :spec (a/int-in 1 nil)
    :doc "How many ended reflex jobs' outcomes the engine keeps, apart from the listed jobs'."}
   :engine.core.base/stats-ms
   {:default 60000 :spec (a/int-in 1 nil)
    :doc "How often the engine emits memory.save-stats and the body.heap sample, ms."}
   :engine.core.base/memory-save-ms
   {:default 2000 :spec (a/int-in 0 nil)
    :doc "Least gap between two memory.edn writes caused by acts, ms. Job start/end, stop and the housekeeping flush write at once, so a crash loses at most this much act progress."}})

(def default-idle-s
  "Seconds a round may hold the body with no act in flight and no declared hold before a job.idle warn."
  10)

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

(defn normalize-result
  "A round's return as {:status}: :done, :continue or :declined, or :ended with the :ending a contract job returned."
  [r]
  (cond
    (#{:done :continue :declined} r) {:status r}
    (contract/ending? r) {:status :ended :ending r}
    :else {:status :error
           :error (js/Error. (str "a round returned " (pr-str r) ", not :done, :continue, :declined or an ending"))}))

(defn state [eng] @(:state eng))

(defn running [eng] @(:running eng))

(defn now [eng] ((:now eng)))

(def suspend-gap-ms
  "Wall-clock time beyond the monotonic clock's that counts as a host suspend, not jitter."
  5000)

(defn ttl-now
  "The clock of TTLs (mutes, moves, entry :ttl-s): wall time minus the host suspends seen so far, so a suspend
  or clock jump cannot end a TTL early. The suspended total is state :clock-lag, saved with the state, so
  :until values stay comparable after a restart; the monotonic clock (:mono) is read at each call and the
  excess of wall time over it since the last call is added to the lag."
  [eng]
  (let [wall (now eng)]
    (when-let [mono-fn (:mono eng)]
      (let [mono (mono-fn)
            [wall0 mono0] (first (reset-vals! (:clock-mark eng) [wall mono]))
            excess (- (- wall wall0) (- mono mono0))]
        (when (> excess suspend-gap-ms)
          (swap! (:state eng) update :clock-lag (fnil + 0) excess))))
    (- wall (:clock-lag @(:state eng) 0))))

(defn job-of
  "[def args] of instance inst, from its parsed spec."
  [eng inst]
  (composite/job (:jobs eng) (:spec inst)))

(defn trigger-def [eng trigger]
  (or (get-in eng [:triggers trigger])
      (throw (ex-info (str "unknown trigger " trigger) {:trigger trigger}))))

(def terminal-kinds #{:completed :failed :cancelled :stopped})

(def max-result-chars 2000)

(defn results
  "The recorded outcomes of ended jobs {id {:name :status :result :ended-at :reflex? :n}}; :n orders them."
  [eng]
  (some-> (:results eng) deref))

(defn result-entry
  "The recent-results entry of a job's terminal event: {:name :status :result :ended-at :reflex? :n}, :result the event's data (dropped with :result-clipped true when it prints past max-result-chars). name and reflex come from
  the event, else from the job's instance."
  [eng event ended-at]
  (let [data (:data event)
        inst (get-in (state eng) [:instances (:job event)])
        clipped? (> (count (pr-str data)) max-result-chars)
        reflex (or (:reflex event) (:reflex inst))
        name (or (:name event) (some-> (:spec inst) expr/label))]
    (cond-> {:name name :status (:kind event) :ended-at ended-at}
      reflex (assoc :reflex? true)
      (and (seq data) (not clipped?)) (assoc :result data)
      clipped? (assoc :result-clipped true)
      (string? (:text event)) (assoc :text (subs (:text event) 0 (min 200 (count (:text event))))))))

(defn trim-results
  "kept without the oldest (lowest :n) entries of each kind past its limit: listed jobs and reflexes count apart."
  [kept limit reflex-limit]
  (let [drop-over (fn [kept reflex? limit]
                    (let [group (filter #(= reflex? (boolean (:reflex? (val %)))) kept)
                          over (- (count group) limit)]
                      (if (pos? over)
                        (apply dissoc kept (map key (take over (sort-by (comp :n val) group))))
                        kept)))]
    (-> kept (drop-over false limit) (drop-over true reflex-limit))))

(defn record-result!
  "Keep a root job's terminal event in the results record (own file, not the state): the newest
  recent-results-limit listed jobs and reflex-results-limit reflex jobs."
  [eng event]
  (let [id (:job event)
        entry (result-entry eng event (now eng))
        limit (settings/get settings :engine.core.base/recent-results-limit)
        reflex-limit (settings/get settings :engine.core.base/reflex-results-limit)
        next-n (inc (reduce max 0 (map :n (vals (results eng)))))]
    (swap! (:results eng)
           (fn [m] (trim-results (assoc (dissoc m id) id (assoc entry :n next-n)) limit reflex-limit)))
    (try (fsu/write-edn! (path/join (:dir eng) "results.edn") @(:results eng))
         (catch :default _ nil))))

(defn emit! [eng event]
  (when (and (= :job (:source event)) (terminal-kinds (:kind event)) (string? (:job event))
             (= [(:job event)] (or (:chain event) [(:job event)])))
    (record-result! eng event))
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
  (when-let [saved (try
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

(defn save-memory!
  "Write the memory when it changed since the last write; force? (on stop) writes anyway."
  ([eng] (save-memory! eng false))
  ([eng force?]
   (reset! (:memory-dirty eng) false)
   (reset! (:memory-saved-at eng) (now eng))
   (save-file! eng (mem/file (:dir eng)) #(mem/save! (:store eng) force?))))

(defn memory-flush-due?
  "True when an act skipped a save and the debounce gap has passed."
  [eng]
  (and @(:memory-dirty eng) (>= (- (now eng) @(:memory-saved-at eng)) (:memory-save-ms eng))))

(defn save-memory-soon!
  "save-memory! for the per-act saves: skipped within :memory-save-ms of the last write (housekeep flushes what was skipped)."
  [eng]
  (if (< (- (now eng) @(:memory-saved-at eng)) (:memory-save-ms eng))
    (reset! (:memory-dirty eng) true)
    (save-memory! eng)))

(defn job-memory
  "Job id's memory, without the engine's :contract tally, :queued-t and :job."
  [eng id]
  (dissoc (mem/job-mem (mem/view (:store eng)) id []) :contract :queued-t :job))

(defn cut-off!
  "Emit job.child_ended :cut-off for every started contract frame among the children of the frame m (job memory)
  at slots of root, deepest last; cause says why (the frame ended, the job was cancelled)."
  [eng root slots m cause]
  (doseq [[slot child] (:children m)
          :let [slots (conj slots slot)
                id (mem/path->id root slots)]]
    (when (get-in child [:contract :call])
      (emit! eng {:source :job :kind :child_ended :level :debug :job id :slot slot
                  :chain (mapv #(mem/path->id root (subvec slots 0 %)) (range (inc (count slots))))
                  :status :cut-off :ending {:kind :cut-off :cause cause}
                  :report (contract/report (:contract child) (now eng))}))
    (cut-off! eng root slots child cause)))

(defn cut-off-dropped!
  "Cut off, with cause, the frames below slots of root that the frame's memory had (before) and no longer has (after)."
  [eng root slots before after cause]
  (doseq [[slot child] (:children before)
          :let [kept (get-in after [:children slot])]]
    (if kept
      (cut-off-dropped! eng root (conj slots slot) child kept cause)
      (cut-off! eng root slots {:children {slot child}} cause))))

(defn delete-root!
  "Delete root job id's memory, its started contract frames cut off by cause first."
  [eng id cause]
  (swap! (:seen-done eng) dissoc (mem/path->id id []))
  (swap! (:looks eng) #(into {} (remove (fn [[[root]]] (= root id))) %))
  (cut-off! eng id [] (mem/job-mem (mem/view (:store eng)) id []) cause)
  (mem/delete-job! (:store eng) id))

(defn drop-instance! [eng id cause]
  (swap! (:state eng) update :instances dissoc id)
  (swap! (:rounds eng) dissoc id)
  (delete-root! eng id cause)
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

(defn waiting-text [{:keys [reason text] :as r}]
  (if text
    (str "waiting: " text)
    (let [more (dissoc r :reason)]
      (str "waiting: " (if (keyword? reason) (name reason) reason) (when (seq more) (str " " (pr-str more)))))))

(defn stopped-result?
  "True when a job that ended :done handed over a result of status :stopped (it gave up; not a success)."
  [result]
  (and (map? result) (= :stopped (:status result))))

(defn reflex-text
  "\"burning → jobs.survival.extinguish\": the reflex id and its job's label."
  [reflex node]
  (str (name reflex) " → " (expr/label node)))

(defn drop-reflex-job!
  "Drop reflex job id (instance and memory) and emit its one reflex.ended with
  outcome (:done :stopped :declined :cut :dropped :failed), its report, and any extra fields; a :dropped or
  :cancelled one is cut off by (:by extra)."
  [eng id reflex outcome extra]
  (let [text (reflex-text reflex (get-in (state eng) [:instances id :spec]))
        report (tally/report eng id [])]
    (drop-instance! eng id (if (#{:dropped :cancelled} outcome) (:by extra) :ended))
    (emit! eng (merge {:source :reflex :kind :ended :level :info :reflex reflex :job id
                       :outcome outcome :text (str text ": " (name outcome)) :report report}
                      (when (#{:dropped :cancelled} outcome) {:ending {:kind :cut-off :cause (:by extra)}})
                      extra))))

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

(defn dead?
  "Whether the body has died and not come back: the latest :died entry is newer than any :respawned or :spawned one."
  [eng]
  (let [view (mem/view (:store eng))
        t #(:t (mem/latest view %) -1)]
    (> (t :died) (max (t :respawned) (t :spawned)))))

(defn paused?
  "Whether the scheduler must stand still: offline or settling. An exclusive run does not pause it: the
  scheduler then runs only that job (see schedule/tick-exclusive!)."
  [eng]
  (or (offline? eng) (settling? eng)))

(defn free-owner!
  "Nobody's round holds the body."
  [eng]
  (set-owner! eng nil))

(defn exclusive-job
  "The id of the exclusive run's job while it is listed, else nil."
  [eng]
  (let [id (some-> (:exclusive eng) deref)]
    (when (and id (some #{id} (:list (state eng)))) id)))

(defn self-pos
  "The body's position, or nil when self() has none (the offline record is just {status: 'offline'})."
  [p]
  (when-let [pos (.-pos (.self p))]
    {:x (.-x pos) :y (.-y pos) :z (.-z pos)}))
