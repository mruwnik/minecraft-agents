(ns engine.core
  "The scheduler: the job list (round-robin over checks, with holding),
  the reflex register (ordered triggers with TTL mutes and moves), cuts by
  ownership-token rotation, and persistence of both to engine.edn.

  The state atom holds plain EDN, written on every change. Memory (engine.memory)
  is saved at every round end and whenever the engine itself writes to it.
    :list [id]              listed instance ids, in cycle order
    :instances {id inst}    {:id :spec :round :hold? :reflex :backoff}; :spec is a parsed
                            job expression node (engine.expr); :backoff is the
                            job's own backoff config (a map, or false), when given
    :register [entry]       {:id :trigger :job :args :persistence :cooldown-s :builtin?}
    :changes {rid {prop {:value v :until ms-or-nil}}}  prop is :mute or :position
    :reflex-state {rid {:cooldown-until ms :stopped? bool}}
    :cursor n               where the next round-robin scan starts
    :resume id              a cut listed job, next once the body is free
    :current id             the listed job whose round is in flight
    :pending-reflex id      a reflex job between its rounds; it keeps the body
    :failed {id {:error text :t ms}}  listed jobs whose round threw; they keep
                            their place and memory, the scheduler skips them
                            until retry! or cancel!
    :deferred-ends [{:reflex :job :outcome :ended-at ms :text}]  reflex jobs that ended while
                            the body was settling or offline, judged on the first ready tick
    :next-id n
  Not persisted:
    the in-flight round ({:id :token :reflex :round})
    the backoff: the :backoffs atom {key entry} (see engine.backoff). The key is a
      listed job's instance id or a reflex's id (a keyword). The entry is
      {:fruitless n :last {:act :status :reason}}, plus :delay-ms :until :since
      :alerted (ms of the last warn) once backing off. An entry goes on progress
      or when the job goes, and all go when the engine leaves a pause."
  (:require [engine.composite :as composite]
            [engine.hurt :as hurt]
            [engine.chat :as chat]
            [engine.backoff :as backoff]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.fsutil :as fsu]
            [engine.memory :as mem]
            [engine.hooks :as hooks]
            [clojure.string :as str]
            ["crypto" :as crypto]
            ["path" :as path]
            [engine.game :as game]))

(def default-stall-rounds
  "Rounds of a holding job with no act call and no change to its memory
  before a job.stalled warn."
  20)

(def default-stats-ms
  "How often tick! emits the memory.save-stats summary."
  60000)

(def default-sweep-ms
  "How often tick! sweeps and saves memory."
  60000)

(def empty-state
  {:list [] :instances {} :register [] :changes {} :reflex-state {}
   :deferred-ends [] :cursor 0 :resume nil :current nil :pending-reflex nil :failed {}
   :attention {} :next-id 1})

;; ------------------------------------------------------------------ errors

(defn cut-error []
  (doto (js/Error. "cut: the ownership token changed") (aset "code" "cut")))

(defn offline-cut-error
  "The cut an act raises when its primitive answers offline (the connection dropped): the round ends, the job stays
  listed and resumes once the body is back."
  []
  (doto (js/Error. "cut: the body lost its connection; the job resumes once it is back") (aset "code" "cut")))

(defn cut? [e]
  (and (instance? js/Error e) (= "cut" (.-code e))))

;; ------------------------------------------------------------------ pure register

(defn live? [change now]
  (and (some? change) (or (nil? (:until change)) (< now (:until change)))))

(defn index-of-id [order id]
  (first (keep-indexed (fn [i e] (when (= id (:id e)) i)) order)))

(defn insert-relative
  "Insert entry above or below its anchor; at the bottom when the anchor is gone."
  [order entry {:keys [above below]}]
  (let [idx (index-of-id order (or above below))]
    (cond
      (nil? idx) (conj order entry)
      above (-> (subvec order 0 idx) (conj entry) (into (subvec order idx)))
      :else (-> (subvec order 0 (inc idx)) (conj entry) (into (subvec order (inc idx)))))))

(defn effective-register
  "The register in firing order at time now: live moves applied, muted entries removed."
  [{:keys [register changes]} now]
  (let [change (fn [e prop] (get-in changes [(:id e) prop]))
        moved? (fn [e] (live? (change e :position) now))
        muted? (fn [e] (live? (change e :mute) now))
        order (reduce (fn [order e] (insert-relative order e (:value (change e :position))))
                      (filterv (complement moved?) register)
                      (filter moved? register))]
    (filterv (complement muted?) order)))

(defn expired-changes [state now]
  (for [[rid props] (:changes state)
        [prop change] props
        :when (not (live? change now))]
    [rid prop change]))

(defn preempts?
  "Whether a firing entry takes the body from its holder ({:id :reflex} or nil)."
  [order holder entry]
  (let [position #(or (index-of-id order %) ##Inf)]
    (cond
      (nil? holder) true
      (nil? (:reflex holder)) true
      (= (:reflex holder) (:id entry)) false
      :else (< (position (:id entry)) (position (:reflex holder))))))

(defn remove-listed [state id]
  (let [idx (.indexOf (:list state) id)]
    (cond-> (-> state
                (update :list #(filterv (fn [x] (not= id x)) %))
                (update :instances dissoc id)
                (update :failed dissoc id))
      (and (>= idx 0) (< idx (:cursor state))) (update :cursor dec)
      (= id (:resume state)) (assoc :resume nil)
      (= id (:pending-reflex state)) (assoc :pending-reflex nil)
      (= id (:current state)) (assoc :current nil))))

(defn restore
  "Saved state after a restart: the in-flight round is lost (its job resumes
  first) and reflex jobs are dropped."
  [saved]
  (let [s (merge empty-state saved)
        reflex-ids (keep (fn [[id inst]] (when (:reflex inst) id)) (:instances s))
        current (:current s)]
    (-> s
        (update :failed select-keys (:list s))
        (dissoc :backoff)
        (assoc :resume (if (some #{current} (:list s)) current (:resume s))
               :current nil
               :pending-reflex nil)
        (update :instances #(apply dissoc % reflex-ids)))))

(defn normalize-result [r]
  (if (#{:done :continue :declined} r)
    {:status r}
    {:status :error :error (js/Error. (str "a round returned " (pr-str r) ", not :done, :continue or :declined"))}))

;; ------------------------------------------------------------------ engine plumbing

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

(defn outstanding [eng] (:attention (state eng)))

(defn attention-event [request]
  (-> (:event request)
      (assoc :attention :required :request-id (:request-id request))))

(defn same-request? [request job-id reason]
  (and (= job-id (:job-id request)) (= reason (:reason request))))

(defn request-attention!
  "Persist and emit a required request. Re-observations for one job/reason reuse
  the stable request ID; only changed data or message emits an update."
  [eng {:keys [job-id reason kind data message context state-update]}]
  (when-not (and job-id reason kind)
    (throw (ex-info "required attention needs job-id, reason and kind" {:job-id job-id :reason reason :kind kind})))
  (let [existing (some (fn [[_ request]] (when (same-request? request job-id reason) request))
                       (:attention (state eng)))
        request-id (or (:request-id existing) (.randomUUID crypto))
        event {:source :job :kind kind
               :context (merge {:job-id job-id} context)
               :data (assoc (or data {}) :reason reason)
               :message message}
        request (merge existing {:request-id request-id :job-id job-id :reason reason
                                 :event event :updated-at (now eng)})
        changed? (or (nil? existing)
                     (not= (select-keys (:event existing) [:data :message :kind])
                           (select-keys event [:data :message :kind])))]
    (swap! (:state eng) (fn [s]
                          (-> (if state-update (state-update s) s)
                              (assoc-in [:attention request-id] request))))
    (when changed? (emit! eng (attention-event request)))
    request-id))

(defn resolve-attention!
  "Close a required request after persisting its removal. Missing IDs are safe
  to acknowledge repeatedly; callers get :already-resolved."
  [eng request-id reason]
  (if-let [request (get-in (state eng) [:attention request-id])]
    (do
      (swap! (:state eng)
             (fn [s]
               (cond-> (update s :attention dissoc request-id)
                 (and (:job-id request) (contains? (:failed s) (:job-id request)))
                 (assoc-in [:failed (:job-id request) :attention-closed?] true))))
      (emit! eng {:source :attention :kind :resolved
                  :context (cond-> {} (:job-id request) (assoc :job-id (:job-id request)))
                  :request-id request-id
                  :data {:handled (not (#{:job-cancelled :job-dropped} reason)) :reason reason}})
      :resolved)
    :already-resolved))

(defn resolve-job-attention!
  ([eng job-id reason] (resolve-job-attention! eng job-id reason identity))
  ([eng job-id reason state-update]
   (let [requests (->> (:attention (state eng))
                       (keep (fn [[request-id request]]
                               (when (= job-id (:job-id request)) [request-id request])))
                       vec)
         ids (mapv first requests)]
     (swap! (:state eng) (fn [s]
                           (-> (state-update s)
                               (update :attention #(apply dissoc % ids)))))
     (doseq [[request-id request] requests]
       (emit! eng {:source :attention :kind :resolved
                   :context (cond-> {} (:job-id request) (assoc :job-id (:job-id request)))
                   :request-id request-id
                   :data {:handled (not (#{:job-cancelled :job-dropped} reason)) :reason reason}}))
     (count requests))))

(defn replay-attention! [eng]
  (doseq [[_ request] (:attention (state eng))]
    (emit! eng (attention-event request))))

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
  (swap! (:state eng) #(cond-> (update % :instances dissoc id)
                         (= id (:pending-reflex %)) (assoc :pending-reflex nil)))
  (mem/delete-job! (:store eng) id)
  (save-memory! eng))

;; ------------------------------------------------------------------ backoff

(def default-backoff-alert-ms
  "Least gap between two job.backoff warns of one job."
  300000)

(defn backoff-key
  "What a job's backoff is counted under: its reflex id for a reflex job (each
  firing is a new instance), else its instance id."
  [inst]
  (or (:reflex inst) (:id inst)))

(defn backoff-config
  "The backoff config of instance inst (false: off): the engine's, then the job's
  own backoff var (a leaf spec only), then the register entry's or the instance's."
  [eng inst]
  (let [[def _] (job-of eng inst)
        entry (when (:reflex inst)
                (some #(when (= (:reflex inst) (:id %)) %) (:register (state eng))))]
    (backoff/config (:backoff eng) (:backoff def) (if (:reflex inst) (:backoff entry) (:backoff inst)))))

(defn backoff-fields
  "The event fields naming the job (or reflex) behind backoff key k."
  [eng k]
  (if (keyword? k)
    {:source :reflex :reflex k}
    (assoc (job-fields eng k) :source :job)))

(defn backoff-entries [eng] @(:backoffs eng))

(defn backoff-entry [eng k] (get @(:backoffs eng) k))

(defn reset-backoff!
  "Forget every backoff count and delay."
  [eng]
  (reset! (:backoffs eng) {})
  (reset! (:passes eng) {}))

(defn forget-backoff! [eng k]
  (swap! (:backoffs eng) dissoc k)
  (swap! (:passes eng) dissoc k))

(defn backing-off? [eng k]
  (backoff/backing-off? (backoff-entry eng k) (now eng)))

(defn pass-over?
  "Whether key k is backing off now; counts each pass that skips it."
  [eng k]
  (when (backing-off? eng k)
    (swap! (:passes eng) update k (fnil inc 0))
    true))

(defn alert-backoff! [eng k entry]
  (let [{:keys [act status reason]} (:last entry)]
    (emit! eng (merge (backoff-fields eng k)
                      {:kind :backoff :level :warn :act act :status status :reason reason
                       :delay-ms (:delay-ms entry) :fruitless (:fruitless entry)
                       :passes (get @(:passes eng) k 0) :since (:since entry)
                       :text (str (if (keyword? k) (name k) k) " backs off for "
                                  (/ (:delay-ms entry) 1000) " s after " (:fruitless entry)
                                  " fruitless rounds; last act " (some-> act name) ": " status
                                  (when reason (str " (" reason ")")))}))))

(defn fruitless!
  "Count one more fruitless round for instance inst; start the backoff or double
  its delay, warning when one is due. Returns the entry."
  [eng inst cfg last]
  (let [k (backoff-key inst)
        t (now eng)
        entry (backoff/fruitless (backoff-entry eng k) cfg t last)
        due? (backoff/alert-due? entry t (:backoff-alert-ms eng))
        entry (cond-> entry due? (assoc :alerted t))]
    (swap! (:backoffs eng) assoc k entry)
    (when due? (alert-backoff! eng k entry))
    entry))

(defn book-round!
  "After a round of run ended with status: a fruitless round counts toward its
  job's backoff (a cut or failed round, a declined round, and a listed job that
  is done, do not: :declined is the job saying not now, on purpose).
  True when the job is now backing off."
  [eng {:keys [id reflex]} status]
  (let [round (get @(:rounds eng) id)
        inst (get-in (state eng) [:instances id])
        cfg (when inst (backoff-config eng inst))]
    (swap! (:rounds eng) dissoc id)
    (boolean
     (when (and cfg
                (backoff/fruitless-round? round)
                (contains? (if reflex #{:done :continue} #{:continue}) status))
       (backoff/backing-off? (fruitless! eng inst cfg (:last round)) (now eng))))))

(defn recovered!
  "A progress act of key k: forget its backoff, and say so when it was backing off."
  [eng k]
  (let [entry (backoff-entry eng k)
        passes (get @(:passes eng) k 0)]
    (when entry
      (forget-backoff! eng k)
      (when (:since entry)
        (emit! eng (merge (backoff-fields eng k)
                          {:kind :recovered :level :info :fruitless (:fruitless entry)
                           :passes passes :since (:since entry)
                           :text (str (if (keyword? k) (name k) k) " made progress after "
                                      (:fruitless entry) " fruitless rounds; backoff over")}))))))

(defn record-act!
  "Book the result r of act k of the round of root (moved: blocks a moveTo moved
  the body): a failure status counts toward a fruitless round, any other status
  is progress and resets the backoff at once; a neutral act is neither."
  [eng {:keys [root reflex]} k r moved]
  (let [status (.-status r)]
    (swap! (:rounds eng) #(cond-> % (contains? % root) (update root backoff/note-act k status (.-reason r) moved)))
    (when-not (or (backoff/failure? status) (backoff/neutral? k status moved))
      (recovered! eng (or reflex root)))))

(defn distance
  "Straight-line distance between positions a and b ({:x :y :z}); nil when either is."
  [a b]
  (when (and a b)
    (Math/hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b)))))

;; ------------------------------------------------------------------ ctx and rounds

(declare submit! call-child self-pos wait-reason)

(defn owner? [eng token]
  (and (some? token) (.isOwner (:primitives eng) token)))

(defn away
  "Why and until when the body is away, or nil while it is online: {:by :job :why :back-at} when a job logged it out
  (the offline act: :by the root job's name, :job the instance, :why from the act's :why, :back-at epoch ms), and
  {:by :connection :why :connection-lost} when it is offline with no log-out on record (a kick or crash; the body
  reconnects by itself)."
  [eng]
  (when (true? (.isOffline (:primitives eng)))
    (or (some-> (:away eng) deref)
        {:by :connection :why :connection-lost})))

(defn log-out-record
  "The away record of an offline act by instance id of root job root, starting at now."
  [eng root id args now]
  (let [ms (.-ms args)]
    (cond-> {:by (keyword (some-> (get-in (state eng) [:instances root :spec]) expr/label))
             :job id
             :why (keyword (or (.-why args) "away"))}
      (number? ms) (assoc :back-at (+ now (min ms 600000))))))

(defn ^:async act!
  "Every acting primitive call from a job: check the ownership token, save
  memory, emit action.started, call, save memory again, emit action.done.
  A cut rejects here (stale token) or from the primitive."
  [eng {:keys [root token chain round reflex]} id k args]
  (when-not (owner? eng token) (throw (cut-error)))
  (swap! (:acts eng) update root (fnil inc 0))
  (save-memory! eng)
  (let [action-id (.randomUUID crypto)
        fields {:level :debug :job id :chain chain :round round :reflex reflex :name (name k)
                :action-id action-id}
        p (:primitives eng)
        _ (emit! eng (merge fields {:source :action :kind :started :args (js->clj args)}))
        from (when (= :moveTo k) (self-pos p))
        away-record (when (= :offline k) (log-out-record eng root id args ((:now eng))))
        _ (when away-record
            (reset! (:away eng) away-record)
            (emit! eng (merge fields {:source :action :kind :logged-out :level :info} away-record)))]
    (try
      (let [r (await (if (= :chat k) (chat/gate! eng p token args) (.call (aget p (name k)) p token args)))
            _ (when (= "offline" (some-> r .-status)) (throw (offline-cut-error)))
            to (when (= :moveTo k) (self-pos p))]
        (when (= :moveTo k)
          (mem/write! (:store eng) :moved (cond-> {:from from :to to :status (.-status r)
                                                   :target (js->clj (.-pos args) :keywordize-keys true)}
                                            (= "noPath" (.-reason r)) (assoc :no-path true))
                      mem/moved-policy))
        (save-memory! eng)
        (record-act! eng {:root root :reflex reflex} k r (distance from to))
        (emit! eng (cond-> (merge fields {:source :action :kind :done :status (.-status r)})
                     (.-reason r) (assoc :reason (.-reason r))
                     (number? (.-distance r)) (assoc :distance (/ (js/Math.round (* 100 (.-distance r))) 100))))
        r)
      (catch :default e
        (emit! eng (cond-> (merge fields {:source :action :kind :done
                                          :status (if (cut? e) :cut :failed)})
                     (not (cut? e)) (assoc :error (str e))))
        (throw e))
      (finally
        (when away-record (reset! (:away eng) nil))))))

(defn make-ctx
  "The ctx a round or a check receives. base is {:root :slots :chain :token
  :args :round :reflex}; :root is the top-level instance id and :slots the
  child slots below it ([] for the instance itself). A check gets :token nil,
  so it cannot write or act. :results holds the results children handed over
  with result! during this round of the top-level job; a fresh one is made
  when base has none, so results never outlive the round."
  [eng {:keys [root slots chain token args round reflex] :as base}]
  (let [store (:store eng)
        results (or (:results base) (atom {}))
        child-wait (or (:child-wait base) (atom nil))
        base (assoc base :results results :child-wait child-wait)
        id (mem/path->id root slots)
        check! #(when-not (owner? eng token) (throw (cut-error)))
        wrote! (fn [kind] (emit! eng {:source :job :kind :memory_written :level :debug :job id
                                      :chain chain :round round :reflex reflex :memory kind}))]
    {:engine eng :primitives (:primitives eng) :token token :args args :id id
     :root root :slots slots :chain chain :round round :reflex reflex :wait (:wait base)
     :view #(mem/view store)
     :update-mem (fn [f more]
                   (check!)
                   (apply mem/update-job! store root slots f more)
                   (wrote! (mem/job-kind root)))
     :remember (fn [kind data policy]
                 (check!)
                 (mem/write! store kind data policy)
                 (wrote! kind))
     :forget (fn [kind pred]
               (check!)
               (mem/forget-where! store kind pred)
               (wrote! kind))
     :forget-until (fn [kind t]
                     (check!)
                     (mem/forget-until! store kind t)
                     (wrote! kind))
     :act (fn [k act-args] (act! eng base id k act-args))
     :note-walk (fn [status moved] (record-act! eng base :walk #js {:status status} moved))
     :call-child (fn [slot def child-args] (call-child eng base slot def child-args))
     :result (fn [data] (check!) (swap! results assoc id data))
     :child-result (fn [slot] (get @results (mem/path->id root (conj slots slot))))
     :submit (fn [spec opts] (check!) (submit! eng spec (assoc opts :by id)))
     :request-attention (fn [kind reason data message]
                          (check!)
                          (request-attention! eng {:job-id root :reason reason :kind kind
                                                   :context (cond-> {:round round :chain chain}
                                                              reflex (assoc :reflex-id reflex))
                                                   :data data :message message}))
     :resolve-attention (fn [request-id reason]
                          (check!)
                          (resolve-attention! eng request-id reason))
     :emit (fn [kind level fields]
             (let [notice? (and (#{:warn :error} level) (not (contains? fields :attention)))]
               (emit! eng (cond-> (merge fields {:source :job :kind kind :level level :job id
                                                 :chain chain :round round :reflex reflex})
                            notice? (assoc :attention :notice)))))}))

(defn child-ctx
  "The ctx of the child in slot under parent base, with args."
  [eng base slot args]
  (let [slots (conj (:slots base) slot)]
    (make-ctx eng (assoc base :slots slots :args args
                         :chain (conj (:chain base) (mem/path->id (:root base) slots))))))

(defn ^:async call-child
  "One round of the child job def in slot under parent base (see README.md).
  The child's memory is the parent's [:children slot] sub-map. It is created
  with the args when missing and cleared only when the child is :done.
  Resolves to :declined when the child's check or round declines, else :done or :continue.
  A :done child's result! data stays readable with child-result for the rest of the parent's round.
  The child shares the parent's token, so a cut anywhere ends the whole chain's round."
  [eng base slot def args]
  (let [store (:store eng)
        slots (conj (:slots base) slot)
        child-id (mem/path->id (:root base) slots)
        clear! #(mem/update-job! store (:root base) (:slots base) update :children dissoc slot)]
    (when-not (owner? eng (:token base)) (throw (cut-error)))
    (swap! (:results base) dissoc child-id)
    (when (empty? (mem/job-mem (mem/view store) (:root base) slots))
      (mem/update-job! store (:root base) slots assoc :args args :children {}))
    (let [c (child-ctx eng base slot args)
          wait (atom nil)
          child-wait (:child-wait base)]
      (reset! child-wait nil)
      (if-not ((:check def) (assoc c :wait wait))
        (do (reset! child-wait (when @wait (wait-reason @wait)))
            :declined)
        (let [{:keys [status error]} (normalize-result (await ((:round def) c)))]
          (when-not (owner? eng (:token base)) (throw (cut-error)))
          (when (= status :error) (throw error))
          (when-not (= status :declined) (reset! child-wait nil))
          (if (= status :done)
            (clear!)
            (swap! (:results base) dissoc child-id))
          status)))))

(defn ^:async run-round [eng run inst]
  (let [[def args] (job-of eng inst)
        results (atom {})
        child-wait (atom nil)
        c (make-ctx eng {:root (:id run) :slots [] :chain [(:id run)] :token (:token run)
                         :args args :round (:round run) :reflex (:reflex run) :results results
                         :child-wait child-wait})]
    (try
      (assoc (normalize-result (await ((:round def) c))) :result (get @results (:id run))
             :child-wait @child-wait)
      (catch :default e
        (if (cut? e) {:status :cut :error e} {:status :error :error e})))))

;; ------------------------------------------------------------------ readiness

(defn call-guarded
  "Call f; on a throw emit a warn and return fallback. For user predicates."
  [eng what fallback f]
  (try
    (f)
    (catch :default e
      (emit! eng {:source :system :kind :error :level :warn :text (str what " threw: " e)})
      fallback)))

(defn check-ctx
  "The ctx a listed job's check receives: memory and sensing, no token. wait is the atom ctx/wait notes a reason in."
  [eng inst args wait]
  (make-ctx eng {:root (:id inst) :slots [] :chain [(:id inst)] :token nil
                 :args args :round (:round inst) :reflex (:reflex inst) :wait wait}))

(defn wait-reason
  "A check's reason as a map with :reason: a keyword r is {:reason r}; no reason (a plain false) is :not-ready."
  [r]
  (cond (map? r) r
        (some? r) {:reason r}
        :else {:reason :not-ready}))

(defn waiting-text [{:keys [reason] :as r}]
  (let [more (dissoc r :reason)]
    (str "waiting: " (if (keyword? reason) (name reason) reason) (when (seq more) (str " " (pr-str more))))))

(defn note-waiting!
  "Listed job id's check declined with reason (or none): emit job.waiting when the reason is new for this wait, and
  keep it for jobs show and observe. A passing check (reason ::passed) ends the wait."
  [eng id reason]
  (let [before (get @(:waiting eng) id)]
    (if (= ::passed reason)
      (when (and before (not (:child (meta before)))) (swap! (:waiting eng) dissoc id))
      (let [r (wait-reason reason)]
        (when (not= before r)
          (swap! (:waiting eng) #(assoc (select-keys % (:list (state eng))) id r))
          (emit! eng (merge (job-fields eng id)
                            {:source :job :kind :waiting :level :info :data r :text (waiting-text r)})))))))

(defn waiting
  "Why listed job id waits: the reason map of its last declining check, or nil while its check passes."
  [eng id]
  (some-> (:waiting eng) deref (get id)))

(defn check-passes?
  "Whether listed instance id's check passes now. A throwing check declines. A declining check's reason (ctx/wait)
  is told once with note-waiting!."
  [eng id]
  (let [inst (get-in (state eng) [:instances id])
        wait (atom nil)
        ok? (boolean (call-guarded eng (str "check of " id) false
                                   #(let [[def args] (job-of eng inst)]
                                      ((:check def) (check-ctx eng inst args wait)))))]
    (note-waiting! eng id (if ok? ::passed @wait))
    ok?))

(defn choose-listed
  "The listed job to run next, never a failed one: a holder (or nothing, while its check declines),
  else the cut job, else round-robin from the cursor over passing checks."
  [eng]
  (let [{:keys [list instances resume cursor]} (state eng)
        n (count list)
        failed? (fn [id] (contains? (:failed (state eng)) id))
        runnable? (fn [id] (and (not (failed? id)) (not (pass-over? eng id)) (check-passes? eng id)))
        holder (some #(when (and (:hold? (instances %)) (not (failed? %))) %) list)]
    (cond
      holder (when (runnable? holder) holder)
      (and resume (some #{resume} list) (runnable? resume)) resume
      (zero? n) nil
      :else (some (fn [i] (let [id (nth list (mod (+ cursor i) n))] (when (runnable? id) id)))
                  (range n)))))

;; ------------------------------------------------------------------ settling a round

(defn stopped-result?
  "True when a job that ended :done handed over a result of status :stopped (it gave up; not a success)."
  [result]
  (and (map? result) (= :stopped (:status result))))

(defn note-child-wait!
  "A listed job's round returned :declined because its child's check waits (reason, or nil when the child gave none):
  the job waits with that reason, told once. Any other round result drops a wait taken from a child."
  [eng id status reason]
  (let [before (get @(:waiting eng) id)]
    (cond
      (and (= :declined status) reason)
      (let [r (with-meta (wait-reason reason) {:child true})]
        (when (not= before r)
          (swap! (:waiting eng) #(assoc (select-keys % (:list (state eng))) id r))
          (emit! eng (merge (job-fields eng id)
                            {:source :job :kind :waiting :level :info :data r :text (waiting-text r)}))))
      (:child (meta before))
      (swap! (:waiting eng) dissoc id))))

(defn settle-listed! [eng {:keys [id]} {:keys [status error result child-wait]}]
  (note-child-wait! eng id status child-wait)
  (let [idx (.indexOf (:list (state eng)) id)
        fields (job-fields eng id)]
    (case status
      :done
      (do (resolve-job-attention! eng id :job-completed
                                  #(assoc (remove-listed % id) :cursor (max idx 0)))
          (forget-backoff! eng id)
          (mem/delete-job! (:store eng) id)
          (emit! eng (if (stopped-result? result)
                       (merge fields {:source :job :kind :stopped :level :warn :attention :notice
                                      :data (dissoc result :dug)
                                      :text (str "stopped: " (name (:reason result))
                                                 (some->> (:cell result) pr-str (str " at ")))})
                       (merge fields {:source :job :kind :completed :level :info
                                      :attention :notice
                                      :data {:status :completed}}))))

      :cut
      (do (swap! (:state eng) assoc :resume id :current nil)
          (emit! eng (merge (job-fields eng id)
                            {:source :job :kind :cut :level :info :by :primitives
                             :text "a primitive rejected with cut; the job stays listed"})))

      :error
      (request-attention! eng {:job-id id :reason :round-failed :kind :failed
                               :context (select-keys fields [:round :chain])
                               :data {:error (str error)}
                               :message (str "Job failed and is parked until retried or cancelled: " error)
                               :state-update #(-> %
                                                  (assoc-in [:failed id] {:error (str error) :t (now eng)})
                                                  (assoc :cursor (inc idx) :current nil))})

      (do (swap! (:state eng) assoc :cursor (inc idx) :current nil)
          (emit! eng (merge (job-fields eng id)
                            {:source :job :kind :yielded :level :info :status status}))))))

(defn trigger-holds?
  "Whether entry's trigger holds: (:when world view args plans live), where view is a
  memory view {:data :now} (see engine.memory), args are the entry's args, plans is the
  engine's engine.world (a trigger may ignore it) and live is the set of ids of
  the jobs still listed (running, queued or paused; not the parked failed ones)."
  [eng entry world view]
  (let [t (trigger-def eng (:trigger entry))
        st (state eng)
        live (into #{} (remove #(contains? (:failed st) %)) (:list st))]
    (boolean (call-guarded eng (str "trigger " (:trigger entry)) false
                           #((:when t) world view (:args entry) (:world eng) live)))))

(defn reflex-text
  "\"burning → jobs.survival.extinguish\": the reflex id and its job's label."
  [reflex node]
  (str (name reflex) " → " (expr/label node)))

(defn drop-reflex-job!
  "Drop reflex job id (instance and memory) and emit its one reflex.ended with
  outcome (:done :declined :cut :dropped :failed) and any extra fields."
  [eng id reflex outcome extra]
  (let [text (reflex-text reflex (get-in (state eng) [:instances id :spec]))]
    (drop-instance! eng id)
    (emit! eng (merge {:source :reflex :kind :ended :level :info :reflex reflex :job id
                       :outcome outcome :text (str text ": " (name outcome))}
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

(defn manual?
  "Whether someone drives the body by hand (see engine.takeover). Not persisted."
  [eng]
  (some? @(:manual eng)))

(defn paused?
  "Whether the scheduler must stand still: offline, settling or under manual control."
  [eng]
  (or (offline? eng) (settling? eng) (manual? eng)))

(defn judge-end!
  "Classify a reflex end now and apply the entry's persistence when its trigger
  still holds, the cooldown counted from the end's :ended-at; emit its one
  reflex.ended with extra fields."
  [eng {:keys [reflex job outcome ended-at text]} extra]
  (let [entry (some #(when (= reflex (:id %)) %) (:register (state eng)))
        still? (and entry (trigger-holds? eng entry (:primitives eng) (mem/view (:store eng))))]
    (when still?
      (case (:persistence entry)
        :cooldown (swap! (:state eng) assoc-in [:reflex-state reflex :cooldown-until]
                         (+ ended-at (* 1000 (:cooldown-s entry 0))))
        :stop (swap! (:state eng) assoc-in [:reflex-state reflex :stopped?] true)
        nil))
    (emit! eng (merge {:source :reflex :kind :ended :level :info :reflex reflex :job job
                       :outcome outcome :text (str text ": " (name outcome))
                       :how (if still? :completed_not_cleared :cleared)}
                      extra))))

(defn end-reflex!
  "A reflex job ended on its own with outcome; classify and apply the entry's
  persistence. While the body is settling, offline or under manual control the
  senses cannot say whether the trigger still holds: the end is deferred to the first ready tick."
  [eng {:keys [id reflex]} outcome]
  (let [end {:reflex reflex :job id :outcome outcome :ended-at (now eng)
             :text (reflex-text reflex (get-in (state eng) [:instances id :spec]))}]
    (drop-instance! eng id)
    (if (paused? eng)
      (swap! (:state eng) update :deferred-ends conj end)
      (judge-end! eng end {}))))

(defn judge-deferred-ends!
  "On a ready tick: judge the reflex ends deferred while settling, in order."
  [eng]
  (let [ends (:deferred-ends (state eng))]
    (swap! (:state eng) assoc :deferred-ends [])
    (doseq [end ends]
      (judge-end! eng end {:deferred-ms (- (now eng) (:ended-at end))}))))

(defn settle-reflex! [eng run {:keys [status error]}]
  (case status
    :continue (swap! (:state eng) assoc :pending-reflex (:id run))
    :declined
    (do (emit! eng {:source :reflex :kind :declined :level :info :reflex (:reflex run) :job (:id run)
                    :text (str "reflex " (name (:reflex run)) ": job " (:id run)
                               " declined; dropped, it may fire again after the trigger's cooldown")})
        (end-reflex! eng run :declined))
    (:error :cut)
    (do (emit! eng {:source :job :kind :failed :level :warn :job (:id run) :reflex (:reflex run)
                    :error (str error)})
        (end-reflex! eng run (if (= :cut status) :cut :failed)))
    (end-reflex! eng run :done)))

(defn watch-progress!
  "After a round of a holding listed job: count rounds with no act call and
  no change to its memory, and warn once when the count reaches the limit.
  Any progress resets the count."
  [eng {:keys [id acts-before mem-before]}]
  (let [inst (get-in (state eng) [:instances id])
        moved? (or (not= acts-before (get @(:acts eng) id 0))
                   (not= mem-before (job-memory eng id)))
        n (if moved? 0 (inc (get @(:stalls eng) id 0)))]
    (when (:hold? inst)
      (swap! (:stalls eng) assoc id n)
      (when (= n (:stall-rounds eng))
        (emit! eng (merge (job-fields eng id)
                          {:source :job :kind :stalled :level :warn :rounds n
                           :text (str "no act call and no memory change for " n " rounds")}))))))

(defn settle!
  "Book a finished round, unless it was cut (its token is no longer current)."
  [eng run outcome]
  (when (= (:token run) (:token (running eng)))
    (reset! (:running eng) nil)
    (set-owner! eng nil)
    (if (:reflex run)
      (if (and (book-round! eng run (:status outcome)) (= :continue (:status outcome)))
        (end-reflex! eng run :backoff)
        (settle-reflex! eng run outcome))
      (do (book-round! eng run (:status outcome))
          (settle-listed! eng run outcome)
          (watch-progress! eng run)))
    (save-memory! eng))
  nil)

(defn start-round!
  "Give the body to instance id for one round; returns the round's promise."
  [eng id]
  (let [token (str "t" (swap! (:tokens eng) inc))
        _ (set-owner! eng token)
        _ (swap! (:rounds eng) assoc id backoff/empty-round)
        s (swap! (:state eng)
                 (fn [s] (let [inst (get-in s [:instances id])]
                           (cond-> (update-in s [:instances id :round] inc)
                             (not (:reflex inst)) (assoc :current id)
                             (= id (:resume s)) (assoc :resume nil)
                             (= id (:pending-reflex s)) (assoc :pending-reflex nil)))))
        inst (get-in s [:instances id])
        run {:id id :token token :reflex (:reflex inst) :round (:round inst)
             :acts-before (get @(:acts eng) id 0)
             :mem-before (job-memory eng id)}]
    (reset! (:running eng) run)
    (emit! eng (merge (job-fields eng id) {:source :job :kind :round_started :level :info}))
    (-> (run-round eng run inst)
        (.then #(settle! eng run %)))))

;; ------------------------------------------------------------------ cuts and firing

(defn holder
  "Who has the body: the running round, or a reflex job between its rounds."
  [eng]
  (or (running eng)
      (when-let [id (:pending-reflex (state eng))]
        {:id id :reflex (get-in (state eng) [:instances id :reflex])})))

(defn cut!
  "Take the body from holder h for entry; cause is the seq of the firing."
  [eng h by cause]
  (set-owner! eng nil)
  (reset! (:running eng) nil)
  (if (:reflex h)
    (drop-reflex-job! eng (:id h) (:reflex h) :dropped {:how :dropped :by by :cause cause})
    (do (swap! (:state eng) assoc :resume (:id h) :current nil)
        (emit! eng (merge (job-fields eng (:id h))
                          {:source :job :kind :cut :level :info :by by :cause cause})))))

(defn eligible?
  "Whether entry may fire at t: not stopped, past its cooldown, not backing off
  (backoff-entry is its entry in the engine's backoffs, or nil)."
  [s entry backoff-entry t]
  (let [{:keys [cooldown-until stopped?]} (get-in s [:reflex-state (:id entry)])]
    (and (not stopped?)
         (or (nil? cooldown-until) (>= t cooldown-until))
         (not (backoff/backing-off? backoff-entry t)))))

(defn evaluate-register!
  "Evaluate every trigger in order; clear :stop latches whose condition is
  false; return the first entry that holds and is eligible."
  [eng order]
  (let [p (:primitives eng)
        view (mem/view (:store eng))
        results (mapv (fn [e] [e (trigger-holds? eng e p view)]) order)
        t (now eng)]
    (doseq [[e holds] results
            :when (and (not holds) (get-in (state eng) [:reflex-state (:id e) :stopped?]))]
      (swap! (:state eng) update-in [:reflex-state (:id e)] dissoc :stopped?))
    (doseq [[e holds] results :when holds]
      (pass-over? eng (:id e)))
    (some (fn [[e holds]] (when (and holds (eligible? (state eng) e (backoff-entry eng (:id e)) t)) e)) results)))

(defn fire! [eng entry h]
  (let [id (new-id! eng)
        node (expr/parse (:jobs eng) (:job entry))
        fired (emit! eng {:source :reflex :kind :fired :level :info :reflex (:id entry)
                          :job id :interrupted (:id h) :text (reflex-text (:id entry) node)})]
    (when h (cut! eng h (:id entry) fired))
    (swap! (:state eng) add-instance id node {:reflex (:id entry)})
    (mem/create-job! (:store eng) id (second (job-of eng {:spec node})))
    (start-round! eng id)))

(defn expire-changes! [eng]
  (doseq [[rid prop change] (expired-changes (state eng) (now eng))]
    (swap! (:state eng) (fn [s] (let [s (update-in s [:changes rid] dissoc prop)]
                                  (if (empty? (get-in s [:changes rid]))
                                    (update s :changes dissoc rid)
                                    s))))
    (emit! eng {:source :reflex :kind :reverted :level :info :reflex rid :property prop
                :from (:value change) :to :default})))

(defn tick-online!
  "One scheduling pass for a body that is on the server."
  [eng]
  (expire-changes! eng)
  (when (>= (- (now eng) @(:last-stats eng)) (:stats-ms eng))
    (reset! (:last-stats eng) (now eng))
    (flush-save-stats! eng))
  (when (>= (- (now eng) @(:last-sweep eng)) (:sweep-ms eng))
    (reset! (:last-sweep eng) (now eng))
    (save-memory! eng))
  (let [order (effective-register (state eng) (now eng))
        firing (evaluate-register! eng order)
        h (holder eng)]
    (cond
      (and firing (preempts? order h firing)) (fire! eng firing h)
      (running eng) nil
      (:pending-reflex (state eng)) (start-round! eng (:pending-reflex (state eng)))
      :else (when-let [id (choose-listed eng)] (start-round! eng id)))))

(defn tick!
  "One scheduling pass. Synchronous; returns the promise of a round it
  started (resolving once that round is settled), or nil. Does nothing while
  the body is offline, settling or under manual control: no trigger is evaluated and no round starts.
  The first ready tick after a pause clears the backoff and judges the reflex
  ends deferred meanwhile."
  [eng]
  (if (paused? eng)
    (do (reset! (:was-paused eng) true) nil)
    (do (when @(:was-paused eng)
          (reset! (:was-paused eng) false)
          (reset-backoff! eng))
        (judge-deferred-ends! eng)
        (tick-online! eng))))

;; ------------------------------------------------------------------ list edits (agents, and submit from rounds)

(defn front-position
  "Index in the list right after the current job: the one running (or cut and
  waiting to resume), else where the next scan starts, which is just after the
  job that ran last."
  [{:keys [list current resume cursor]}]
  (let [idx (.indexOf list (or current resume))]
    (if (neg? idx)
      (min cursor (count list))
      (inc idx))))

(defn insert-front
  "State with id listed directly after the current job, so it gets the next round."
  [state id]
  (let [pos (front-position state)]
    (update state :list #(into (conj (subvec % 0 pos) id) (subvec % pos)))))

(defn insert-now
  "State with id listed directly before the current job (the one running, or cut and waiting to resume), else where
  the next scan starts. When the new job ends, the scan restarts at its index, which is then the cut job's.
  So the cut job runs next even if a later cut took its :resume mark."
  [{:keys [list current resume cursor] :as state} id]
  (let [idx (.indexOf list (or current resume))
        pos (if (neg? idx) (min cursor (count list)) idx)]
    (cond-> (update state :list #(into (conj (subvec % 0 pos) id) (subvec % pos)))
      (< pos cursor) (update :cursor inc))))

(defn submit!
  "Put a job spec (an expression, see engine.expr) on the list. Returns the instance id; throws on a bad spec.
  opts:
    :hold?     hold the body (same as wrapping the spec in (hold e))
    :backoff   backoff config, a map or false (wins over a (backoff cfg e) wrapper)
    :front?    list it directly after the current job, so it gets the next round
    :now?      list it directly before the current job (do-now!)
    :by        who asked, for the event"
  [eng spec {:keys [front? now? by] :as opts}]
  (let [{:keys [node hold?] :as parsed} (expr/parse-spec (:jobs eng) spec)
        hold? (boolean (or hold? (:hold? opts)))
        bo (if (contains? opts :backoff) (:backoff opts) (:backoff parsed))
        _ (backoff/validate! bo)
        args (second (job-of eng {:spec node}))
        id (new-id! eng)]
    (swap! (:state eng) #(let [s (add-instance % id node (cond-> {:hold? hold?} (some? bo) (assoc :backoff bo)))]
                           (cond
                             now? (insert-now s id)
                             front? (insert-front s id)
                             :else (update s :list conj id))))
    (mem/create-job! (:store eng) id args)
    (save-memory! eng)
    (emit! eng {:source :job :kind :queued :level :info :job id :chain [id] :name (expr/label node)
                :spec (pr-str spec) :hold hold? :by by})
    id))

(defn cancel!
  "Remove listed job id (cutting its round if it is the one running). by names who asked."
  ([eng id] (cancel! eng id :agent))
  ([eng id by]
    (when (= id (:id (running eng)))
      (set-owner! eng nil)
      (reset! (:running eng) nil))
    (resolve-job-attention! eng id :job-cancelled #(remove-listed % id))
    (forget-backoff! eng id)
    (mem/delete-job! (:store eng) id)
    (save-memory! eng)
    (emit! eng {:source :job :kind :cancelled :level :info :job id :chain [id] :by by})))

(defn retry!
  "Clear the failed mark of listed job id so the scheduler runs it again, memory
  as it was. False when the job is not marked failed."
  [eng id]
  (if-not (contains? (:failed (state eng)) id)
    false
    (do (resolve-job-attention! eng id :job-retried #(update % :failed dissoc id))
        (emit! eng {:source :job :kind :retried :level :info :job id :chain [id] :by :agent})
        true)))

(defn do-now!
  "Cut a running listed job and list the job spec directly before it, holding the body: the new job runs now and the
  cut one, memory kept, continues once it ends. A running reflex is not cut; the new job waits behind it."
  [eng spec]
  (expr/parse-spec (:jobs eng) spec)
  (let [r (running eng)]
    (when (and r (not (:reflex r)))
      (cut! eng r :do-now nil))
    (submit! eng spec {:hold? true :now? true :by :agent})))

;; ------------------------------------------------------------------ register edits (agents only)

(defn entry-from
  "A register entry from a spec {:trigger ...overrides}, filled from the
  trigger. :job is a job spec (an expression without hold); :args are the
  trigger's, merged over its defaults."
  [eng spec]
  (let [t (trigger-def eng (:trigger spec))]
    (merge {:id (:trigger spec) :trigger (:trigger spec) :job (:job t)
            :persistence (:persistence t :retry) :cooldown-s (:cooldown-s t 0) :builtin? false}
           (select-keys spec [:id :job :persistence :cooldown-s :builtin? :backoff])
           {:args (merge (:args t {}) (:args spec))})))

(defn register-reflex!
  "Append a reflex to the register. Returns its id."
  [eng spec]
  (let [entry (entry-from eng spec)]
    (expr/parse (:jobs eng) (:job entry))
    (backoff/validate! (:backoff entry))
    (when (index-of-id (:register (state eng)) (:id entry))
      (throw (ex-info (str "reflex already registered: " (:id entry)) {:id (:id entry)})))
    (swap! (:state eng) update :register conj entry)
    (emit! eng {:source :reflex :kind :changed :level :info :reflex (:id entry)
                :property :registered :value (pr-str (:job entry)) :by :agent})
    (:id entry)))

(defn remove-reflex!
  "Remove a reflex; refused (false) for built-ins."
  [eng id]
  (let [entry (some #(when (= id (:id %)) %) (:register (state eng)))]
    (cond
      (nil? entry) false
      (:builtin? entry)
      (do (emit! eng {:source :reflex :kind :refused :level :warn :reflex id
                      :text "built-in reflexes can be muted or moved, not removed"})
          false)
      :else
      (do (swap! (:state eng) #(-> %
                                   (update :register (fn [r] (filterv (fn [e] (not= id (:id e))) r)))
                                   (update :changes dissoc id)
                                   (update :reflex-state dissoc id)))
          (emit! eng {:source :reflex :kind :changed :level :info :reflex id
                      :property :registered :value nil :by :agent})
          true))))

(defn change!
  "Put property prop of reflex id under one change, replacing any earlier one."
  [eng id prop value ttl-s]
  (when-not (index-of-id (:register (state eng)) id)
    (throw (ex-info (str "no such reflex " id) {:id id})))
  (swap! (:state eng) assoc-in [:changes id prop]
         {:value value :until (when ttl-s (+ (now eng) (* 1000 ttl-s)))})
  (emit! eng {:source :reflex :kind :changed :level :info :reflex id :property prop
              :value value :ttl ttl-s :by :agent}))

(defn mute! [eng id ttl-s] (change! eng id :mute true ttl-s))

(defn move!
  "where is {:above other-id} or {:below other-id}."
  [eng id where ttl-s]
  (change! eng id :position where ttl-s))

(defn clear-change! [eng id prop]
  (swap! (:state eng) update-in [:changes id] dissoc prop)
  (emit! eng {:source :reflex :kind :reverted :level :info :reflex id :property prop
              :to :default :by :agent}))

;; ------------------------------------------------------------------ scenario, lifecycle

(defn load-scenario!
  "Register a scenario's reflexes and queue its job specs, in order."
  [eng {:keys [register queue]}]
  (doseq [spec register] (register-reflex! eng spec))
  (doseq [spec queue]
    (submit! eng spec {:by :scenario})))

(defn self-pos
  "The body's position, or nil when self() has none (the offline record is just {status: 'offline'})."
  [p]
  (when-let [pos (.-pos (.self p))]
    {:x (.-x pos) :y (.-y pos) :z (.-z pos)}))

(def error-kinds #{"died" "error" "reconnect-failed" "dependency-patches-missing"})
(def warn-kinds #{"world-not-loaded" "physics-stalled"})
(def debug-kinds #{"picked-up"})

(defn body-event-level [kind]
  (cond (contains? error-kinds kind) :error
        (contains? warn-kinds kind) :warn
        (contains? debug-kinds kind) :debug
        :else :info))

(defn drop-jobs-on-death!
  "A job does not survive its body's death: cancel every listed job (queued, held,
  cut or running) and drop every reflex job. Register entries stay, so a trigger
  that still holds after the respawn starts its job again."
  [eng]
  (doseq [id (:list (state eng))]
    (cancel! eng id :death))
  (doseq [[id inst] (:instances (state eng))
          :when (:reflex inst)]
    (when (= id (:id (running eng)))
      (set-owner! eng nil)
      (reset! (:running eng) nil))
    (drop-reflex-job! eng id (:reflex inst) :dropped {:how :dropped :by :death})))

(defn record-body-event!
  "A momentary body event becomes an entry of its kind (:hurt, :died, ...).
  A death also drops every job."
  [eng e]
  (let [m (js->clj e :keywordize-keys true)]
    (when (= "died" (:kind m))
      (hurt/flush! eng #(emit! eng %))
      (drop-jobs-on-death! eng))
    (mem/write! (:store eng) (keyword (:kind m)) (dissoc m :kind))
    (save-memory! eng)
    (if (= "hurt" (:kind m))
      (hurt/record! eng #(emit! eng %) m)
      (emit! eng (merge (dissoc m :kind)
                        {:source :body :kind (keyword (:kind m))
                         :level (body-event-level (:kind m))})))))

(defn unknown-job
  "The message why inst's spec no longer resolves against the registry, or nil."
  [eng inst]
  (try (job-of eng inst) nil
       (catch :default e (ex-message e))))

(defn stale-keys
  "The arg keys of job sym that its registry entry no longer declares, sorted; nil when none."
  [registry sym args]
  (let [entry (get registry sym)]
    (when (contains? entry :args)
      (seq (sort-by str (remove (set (keys (:args entry))) (keys args)))))))

(defn strip-form
  "[form' stale] for a job spec form: every leaf's args without the keys its job no longer declares;
  stale is [[sym [key ...]] ...]. Forms of an unexpected shape pass through (parse refuses them)."
  [registry form]
  (let [head (when (and (seq? form) (seq form)) (first form))
        parts (rest form)
        kids (fn [kids] (reduce (fn [[out stale] k] (let [[k2 s2] (strip-form registry k)] [(conj out k2) (into stale s2)]))
                                [[] []] kids))]
    (cond
      (not (symbol? head)) [form []]
      (#{'seq 'any 'repeat 'hold} head) (let [[out stale] (kids parts)] [(apply list head out) stale])
      (= 'backoff head) (let [[out stale] (kids (rest parts))] [(apply list head (first parts) out) stale])
      (not (map? (first parts))) [form []]
      :else (let [args (first parts)
                  ks (stale-keys registry head args)]
              (if ks
                [(list head (apply dissoc args ks)) [[head (vec ks)]]]
                [form []])))))

(defn strip-node
  "[node' stale] for a parsed node, as strip-form does for a form."
  [registry node]
  (case (:op node)
    :leaf (let [ks (stale-keys registry (:job node) (:args node))]
            [(cond-> node ks (update :args #(apply dissoc % ks))) (if ks [[(:job node) (vec ks)]] [])])
    :repeat (let [[c stale] (strip-node registry (:child node))] [(assoc node :child c) stale])
    (let [rs (mapv #(strip-node registry %) (:children node))]
      [(assoc node :children (mapv first rs)) (into [] (mapcat second) rs)])))

(defn stale-text [stale]
  (str/join "; " (for [[sym ks] stale] (str sym " " (str/join " " ks)))))

(defn drop-unknown-jobs!
  "After a restore: a listed job whose job namespace is gone is dropped with a warn; one whose saved args
  hold keys its job no longer declares keeps running without them (defaults apply) and is returned in a
  vector of notices {:job-id :reason :message :data} to raise once restore is done."
  [eng]
  (vec
   (for [[id inst] (:instances (state eng))
         :let [problem (unknown-job eng inst)]
         :let [[node stale] (when-not problem (strip-node (:jobs eng) (:spec inst)))]
         :when (or problem (seq stale))
         :let [notice (when-not problem
                        (swap! (:state eng) assoc-in [:instances id :spec] node)
                        {:job-id id :reason :stale-args :kind :stale-args
                         :data {:dropped (mapv (fn [[sym ks]] {:job (str sym) :args (mapv str ks)}) stale)}
                         :message (str "job " id " restored without args its job no longer accepts (its defaults apply): " (stale-text stale))})]]
     (do (when problem
           (swap! (:state eng) remove-listed id)
           (mem/delete-job! (:store eng) id)
           (emit! eng {:source :job :kind :failed :level :warn :job id :chain [id]
                       :error problem :text (str "dropped on restore: " problem)}))
         notice))))

(defn drop-leftover-reflex-jobs!
  "After a restore: restore dropped the reflex jobs a crash left in saved;
  say so with one reflex.ended each (a clean shutdown already did)."
  [eng saved]
  (doseq [[id inst] (:instances saved)
          :when (:reflex inst)]
    (emit! eng {:source :reflex :kind :ended :level :info :reflex (:reflex inst) :job id
                :outcome :dropped :how :dropped :by :restart
                :text (str (reflex-text (:reflex inst) (:spec inst)) ": dropped")})))

(defn unresolved-entry
  "The message why register entry e no longer resolves (trigger or job spec), or nil."
  [eng e]
  (try (trigger-def eng (:trigger e))
       (expr/parse (:jobs eng) (:job e))
       nil
       (catch :default err (ex-message err))))

(defn remove-entry [s id]
  (-> s
      (update :register #(filterv (fn [x] (not= id (:id x))) %))
      (update :changes dissoc id)
      (update :reflex-state dissoc id)))

(defn repair-entries!
  "After a restore, fix register entries that no longer resolve.
  An entry whose trigger is gone, or whose job does not parse even without its stale args, is dropped.
  An entry with only stale args stays, minus those args (the job's defaults apply).
  Each case is warned and returned as a notice {:job-id :reason :kind :message :data}
  for the caller to raise as a required attention request. Never silent."
  [eng]
  (vec
   (for [e (:register (state eng))
         :let [problem (unresolved-entry eng e)]
         :when problem
         :let [id (:id e)
               [form stale] (strip-form (:jobs eng) (:job e))
               kept? (and (seq stale)
                          (some? (get-in eng [:triggers (:trigger e)]))
                          (nil? (expr/problem (:jobs eng) form)))]]
     (if kept?
       (let [text (str "reflex " id " restored without args its job no longer accepts (its defaults apply): " (stale-text stale))]
         (swap! (:state eng) update :register (fn [r] (mapv #(if (= id (:id %)) (assoc % :job form) %) r)))
         (emit! eng {:source :system :kind :reflex-repaired :level :warn :reflex id
                     :dropped (mapv (fn [[sym ks]] {:job (str sym) :args (mapv str ks)}) stale) :text text})
         {:job-id (str "reflex:" (name id)) :reason :reflex-repaired :kind :reflex-repaired
          :data {:reflex id :dropped (mapv (fn [[sym ks]] {:job (str sym) :args (mapv str ks)}) stale)}
          :message text})
       (let [text (str "reflex " id " DROPPED on restore, the body no longer has it: " problem)]
         (swap! (:state eng) remove-entry id)
         (emit! eng {:source :system :kind :dropped :level :warn :reflex id :error problem :text text})
         {:job-id (str "reflex:" (name id)) :reason :reflex-dropped :kind :reflex-dropped
          :data {:reflex id :error problem} :message text})))))

(defn create
  "An engine over primitives with state under dir. Restores engine.edn and
  memory.edn when present, sweeps memory and appends a :restart entry.
  Options:
    :primitives, :dir, :now, :events, :body
    :jobs        the registry {sym {:check :round :doc :args}}
    :triggers    {name trigger}
    :world       the body's world store (hooks :world/open; :world/blank when not given)
    :backoff     engine-wide backoff config, a map or false (see engine.backoff)
    :backoff-alert-ms  least gap between two job.backoff warns (300000)
    :stall-rounds      rounds before job.stalled (20)
    :sweep-ms          memory sweep interval (60000)
    :stats-ms          memory.save-stats interval (60000)
    :max-event-bytes   event log size cap (64 MiB)"
  [{:keys [primitives jobs triggers dir now events body stall-rounds sweep-ms stats-ms world
           max-event-bytes
           backoff backoff-alert-ms]
    :or {now js/Date.now stall-rounds default-stall-rounds sweep-ms default-sweep-ms
         stats-ms default-stats-ms backoff-alert-ms default-backoff-alert-ms}}]
  (let [_ (game/select! primitives)
        file (path/join dir "engine.edn")
        saved (fsu/read-edn file)
        username (or body (.-username (.self primitives)))
        initial-state (if saved (restore saved) empty-state)
        initial-state (cond-> initial-state
                        (nil? (:generation-id initial-state)) (assoc :generation-id (.randomUUID crypto)))
        st (atom initial-state)
        ev (or events (events/make {:file (path/join dir "events.edn")
                                    :generation-id (:generation-id initial-state)
                                    :max-bytes (or max-event-bytes 67108864)
                                    :stdout? true :now now :pos-fn #(self-pos primitives)}))
        store (mem/open dir {:now now
                             :world-time #(or (.-timeOfDay (.self primitives)) nil)
                             :live-jobs #(set (keys (:instances @st)))})
        eng {:primitives primitives :jobs jobs :triggers triggers :dir dir :now now :events ev
             :store store
             :hurt (hurt/new-state)
             :world (or world ((:world/blank hooks/all)))
             :warned (atom #{})
             :state st
             :running (atom nil)
             :tokens (atom 0)
             :manual (atom nil)
             :away (atom nil)
             :waiting (atom {})
             :world-ops (atom {:active nil :records {} :order [] :queue []})
             :acts (atom {})
             :said (atom [])
             :stalls (atom {})
             :rounds (atom {})
             :passes (atom {})
             :backoffs (atom {})
             :was-paused (atom false)
             :backoff backoff
             :backoff-alert-ms backoff-alert-ms
             :stall-rounds stall-rounds
             :sweep-ms sweep-ms
             :last-sweep (atom (now))
             :stats-ms stats-ms
             :last-stats (atom (now))
             :save-stats (atom empty-save-stats)}]
    (add-watch st ::persist (fn [_ _ old new]
                              (when (not= old new) (save-file! eng file #(fsu/write-edn! file new)))))
    (save-file! eng file #(fsu/write-edn! file (state eng)))
    (set-owner! eng nil)
    (.onBodyEvent primitives #(record-body-event! eng %))
    (drop-leftover-reflex-jobs! eng saved)
    (let [notices (into (filterv some? (drop-unknown-jobs! eng)) (repair-entries! eng))]
      (doseq [[request-id request] (:attention (state eng))
              :when (and (:job-id request) (not (some #{(:job-id request)} (:list (state eng)))))]
        (resolve-attention! eng request-id :job-dropped))
      (doseq [n notices] (request-attention! eng n)))
    ;; A parked failed job must have a required request before the replay below
    ;; (covers older snapshots and a crash between the two writes).
    (doseq [[id failure] (:failed (state eng))
            :when (and (not (:attention-closed? failure))
                       (not-any? #(same-request? % id :round-failed) (vals (:attention (state eng))))) ]
      (request-attention! eng {:job-id id :reason :round-failed :kind :failed
                               :data {:error (:error failure)}
                               :message (str "Job failed and is parked until retried or cancelled: " (:error failure))}))
    (replay-attention! eng)
    (mem/write! store :restart {})
    (save-memory! eng)
    (emit! eng {:source :system :kind (if saved :restored :started) :level :info
                :list (:list (state eng)) :register (mapv :id (:register (state eng)))})
    eng))

(defn shutdown!
  "Release the body for a shutdown. The in-flight round is cut by token
  rotation and its outcome is never booked, so its job stays on the list
  (persisted as :current, resumed first after a restart) with its memory."
  [eng]
  (let [h (holder eng)]
    (set-owner! eng nil)
    (reset! (:running eng) nil)
    (when (:reflex h)
      (drop-reflex-job! eng (:id h) (:reflex h) :dropped {:how :dropped :by :shutdown})))
  (emit! eng {:source :system :kind :stopping :level :info :job (:current (state eng))}))

(defn report-tick-failure! [eng e]
  (emit! eng {:source :system :kind :error :level :error :text (str "tick failed: " e)}))

(defn start!
  "Tick every tick-ms until the returned stop fn is called. A tick that throws,
  or whose round fails to settle, is reported as an error event; the loop goes on.
  :before-tick, when given, runs at the top of each step (also while paused), before tick!."
  [eng {:keys [tick-ms before-tick] :or {tick-ms 250}}]
  (let [stopped (atom false)]
    (letfn [(step []
              (when-not @stopped
                (try
                  (when before-tick (before-tick))
                  (some-> (tick! eng) (.catch #(report-tick-failure! eng %)))
                  (catch :default e
                    (report-tick-failure! eng e)))
                (js/setTimeout step tick-ms)))]
      (step))
    #(reset! stopped true)))
