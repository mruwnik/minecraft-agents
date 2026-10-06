(ns engine.core.round
  "One round: the ctx a round or check receives, the act wrapper (token check, memory saves, action
  events) and call-child."
  (:require [engine.core.base :refer [cut-error cut? distance emit! job-of normalize-result offline-cut-error owner? save-memory! self-pos stopped-result? wait-reason]]
            [engine.core.attention :refer [request-attention! resolve-attention!]]
            [engine.core.fruitless :refer [record-act!]]
            [engine.core.activity :refer [act-ended! act-started! hold-still! log-out-record]]
            [engine.core.list-edits :refer [submit!]]
            [engine.chat :as chat]
            [engine.memory :as mem]
            ["crypto" :as crypto]))

(declare call-child)

(defn ^:async act!
  "Every acting primitive call from a job: check the ownership token, save
  memory, emit action.started, call, save memory again, emit action.done.
  A cut rejects here (stale token) or from the primitive."
  [eng {:keys [root token chain round reflex]} id k args]
  (when-not (owner? eng token) (throw (cut-error)))
  (act-started! eng token root k args)
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
        (act-ended! eng token)
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
     :alive? #(or (nil? token) (owner? eng token))
     :hold-still (fn [reason] (check!) (hold-still! eng token root reason))
     :emit (fn [kind level fields]
             (let [notice? (and (#{:warn :error} level) (not (contains? fields :attention)))]
               (when (or (nil? token) (owner? eng token))
                 (emit! eng (cond-> (merge fields {:source :job :kind kind :level level :job id
                                                   :chain chain :round round :reflex reflex})
                              notice? (assoc :attention :notice))))))}))

(defn child-ctx
  "The ctx of the child in slot under parent base, with args."
  [eng base slot args]
  (let [slots (conj (:slots base) slot)]
    (make-ctx eng (assoc base :slots slots :args args
                         :chain (conj (:chain base) (mem/path->id (:root base) slots))))))

(defn child-end-fields
  "The :status and :reason of a child call's job.child_ended: a :done child whose result is :stopped is :stopped with
  the result's reason; a declining one carries its check's wait reason."
  [status result wait]
  (cond
    (and (= :done status) (stopped-result? result)) (cond-> {:status :stopped} (:reason result) (assoc :reason (:reason result)))
    (and (= :declined status) wait) {:status :declined :reason (:reason wait)}
    :else {:status status}))

(defn ^:async call-child
  "One round of the child job def in slot under parent base (see README.md).
  The child's memory is the parent's [:children slot] sub-map. It is created
  with the args when missing and cleared only when the child is :done.
  Resolves to :declined when the child's check or round declines, else :done or :continue.
  A :done child's result! data stays readable with child-result for the rest of the parent's round.
  The child shares the parent's token, so a cut anywhere ends the whole chain's round.
  Each call emits a debug job.child_started and, unless cut, job.child_ended."
  [eng base slot def args]
  (let [store (:store eng)
        slots (conj (:slots base) slot)
        child-id (mem/path->id (:root base) slots)
        chain (conj (:chain base) child-id)
        fields {:source :job :level :debug :job child-id :slot slot :chain chain :round (:round base)
                :reflex (:reflex base)}
        ended! (fn [status result wait]
                 (emit! eng (merge fields {:kind :child_ended} (child-end-fields status result wait))))
        clear! #(mem/update-job! store (:root base) (:slots base) update :children dissoc slot)]
    (when-not (owner? eng (:token base)) (throw (cut-error)))
    (emit! eng (assoc fields :kind :child_started))
    (swap! (:results base) dissoc child-id)
    (when (empty? (mem/job-mem (mem/view store) (:root base) slots))
      (mem/update-job! store (:root base) slots assoc :args args :children {}))
    (let [c (child-ctx eng base slot args)
          wait (atom nil)
          child-wait (:child-wait base)]
      (reset! child-wait nil)
      (if-not ((:check def) (assoc c :wait wait))
        (let [w (when @wait (wait-reason @wait))]
          (reset! child-wait w)
          (ended! :declined nil w)
          :declined)
        (let [{:keys [status error]} (normalize-result (await ((:round def) c)))]
          (when-not (owner? eng (:token base)) (throw (cut-error)))
          (when (= status :error) (ended! :error nil nil) (throw error))
          (when-not (= status :declined) (reset! child-wait nil))
          (ended! status (get @(:results base) child-id) @child-wait)
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

(defn check-ctx
  "The ctx a listed job's check receives: memory and sensing, no token. wait is the atom ctx/wait notes a reason in."
  [eng inst args wait]
  (make-ctx eng {:root (:id inst) :slots [] :chain [(:id inst)] :token nil
                 :args args :round (:round inst) :reflex (:reflex inst) :wait wait}))
