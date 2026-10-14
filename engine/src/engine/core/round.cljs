(ns engine.core.round
  "One round: the ctx a round or check receives, the act wrapper (token check, memory saves, action
  events) and call-child."
  (:require [engine.core.base :refer [cut-error cut? cut-off! cut-off-dropped! distance emit! job-of normalize-result now offline-cut-error owner? save-memory! self-pos stopped-result? wait-reason]]
            [engine.core.attention :refer [request-attention! resolve-attention!]]
            [engine.core.fruitless :refer [record-act!]]
            [engine.core.activity :refer [act-ended! act-started! body-act! close-section! hold-still! log-out-record open-section! useful-act?]]
            [engine.core.list-edits :refer [submit!]]
            [engine.core.ending :as ending]
            [engine.core.legacy :as legacy]
            [engine.core.limits :as limits]
            [engine.core.quote :as quote]
            [engine.core.tally :as tally]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.chat :as chat]
            [engine.memory :as mem]
            [engine.settings :as settings]
            ["crypto" :as crypto]))

(declare call call-child quote-ctx)

(defn own!
  "Throw unless the token of base is current: a cut; a quote ctx (no token) gets a plain error."
  [eng {:keys [token quote-deadline]}]
  (when quote-deadline (throw (js/Error. "a quote cannot act, call or write")))
  (when-not (owner? eng token) (throw (cut-error))))

(defn ^:async act!
  "Every acting primitive call from a job: check the ownership token, save
  memory, emit action.started, call, count what it used and yielded on every frame of the chain (engine.core.tally),
  save memory again, emit action.done. A cut rejects here (stale token) or from the primitive."
  [eng {:keys [root slots token chain round reflex] :as base} id k args]
  (own! eng base)
  (limits/check-act! eng base k args)
  (act-started! eng token root k args)
  (save-memory! eng)
  (let [useful? (useful-act? eng root k)
        action-id (.randomUUID crypto)
        fields {:level :debug :job id :chain chain :round round :reflex reflex :name (name k)
                :action-id action-id}
        p (:primitives eng)
        _ (emit! eng (merge fields {:source :action :kind :started :args (js->clj args)}))
        from (when (= :moveTo k) (self-pos p))
        before (tally/snapshot eng)
        away-record (when (= :offline k) (log-out-record eng root id args ((:now eng))))
        _ (when away-record
            (reset! (:away eng) away-record)
            (emit! eng (merge fields {:source :action :kind :logged-out :level :info} away-record)))]
    (try
      (when useful? (body-act! eng 1))
      (let [r (await (if (= :chat k) (chat/gate! eng p token args) (.call (aget p (name k)) p token args)))
            _ (when (= "offline" (some-> r .-status)) (throw (offline-cut-error)))
            to (when (= :moveTo k) (self-pos p))]
        (when (= :moveTo k)
          (mem/write! (:store eng) :moved (cond-> {:from from :to to :status (.-status r)
                                                   :target (js->clj (.-pos args) :keywordize-keys true)}
                                            (= "noPath" (.-reason r)) (assoc :no-path true))
                      mem/moved-policy))
        (tally/count-act! eng root slots before)
        (save-memory! eng)
        (record-act! eng {:root root :reflex reflex} k r (distance from to))
        (emit! eng (cond-> (merge fields {:source :action :kind :done :status (.-status r)})
                     (.-reason r) (assoc :reason (.-reason r))
                     (number? (.-distance r)) (assoc :distance (/ (js/Math.round (* 100 (.-distance r))) 100))))
        r)
      (catch :default e
        (tally/count-act! eng root slots before)
        (save-memory! eng)
        (emit! eng (cond-> (merge fields {:source :action :kind :done
                                          :status (if (cut? e) :cut :failed)})
                     (not (cut? e)) (assoc :error (str e))))
        (throw e))
      (finally
        (act-ended! eng token)
        (when useful? (body-act! eng -1))
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
        child-ending (atom nil)
        base (assoc base :results results :child-wait child-wait
                    :section-depth (or (:section-depth base) (atom 0)) :held (or (:held base) (atom nil)))
        id (mem/path->id root slots)
        check! #(own! eng base)
        wrote! (fn [kind] (emit! eng {:source :job :kind :memory_written :level :debug :job id
                                      :chain chain :round round :reflex reflex :memory kind}))]
    {:engine eng :primitives (:primitives eng) :token token :args args :id id
     :root root :slots slots :chain chain :round round :reflex reflex :wait (:wait base) :t0s (:t0s base)
     :quote-deadline (:quote-deadline base)
     :left #(let [t (now eng)]
              (contract/left (:contract (mem/job-mem (mem/view store) root slots)) (- t (nth (:t0s base) (count slots) t))))
     :section-depth (:section-depth base) :held (:held base) :section-closed #(limits/section-closed! eng base)
     :view #(mem/view store)
     :update-mem (fn [f more]
                   (check!)
                   (let [before (mem/job-mem (mem/view store) root slots)]
                     (apply mem/update-job! store root slots
                            (fn [m & more]
                              (cond-> (apply f (dissoc m :contract) more) (:contract m) (assoc :contract (:contract m))))
                            more)
                     (cut-off-dropped! eng root slots before (mem/job-mem (mem/view store) root slots) :abandoned))
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
     :note-walk (fn [status moved] (check!) (record-act! eng base :walk #js {:status status} moved))
     :call-child (fn [slot def child-args] (call-child eng base slot def child-args))
     :call (fn [slot def child-args] (call eng base child-ending slot def child-args))
     :quote (fn [def quote-args deadline self?]
              (quote/quote! eng base def quote-args deadline self? #(quote-ctx eng base def quote-args % self?)))
     :next-way quote/next-way
     :child-ending child-ending
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
     :open-section (fn [section level]
                     (check!)
                     (let [opened? (open-section! eng token section level (boolean (some-> (:deferred eng) deref seq)))]
                       (when opened?
                         (emit! eng {:source :job :kind :section :level :debug :job id :chain chain :round round
                                     :reflex reflex :section section :holds level :open true}))
                       opened?))
     :close-section (fn [section]
                      (close-section! eng token)
                      (when (owner? eng token)
                        (emit! eng {:source :job :kind :section :level :debug :job id :chain chain :round round
                                    :reflex reflex :section section :open false})))
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
    (make-ctx eng (assoc base :slots slots :args args :t0s (conj (or (:t0s base) []) (now eng))
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
  A contract child answers as an old one (engine.core.legacy): any ending is :done, with result-of it as the result.
  The child shares the parent's token, so a cut anywhere ends the whole chain's round.
  Each call emits a debug job.child_started and, unless cut, job.child_ended."
  [eng base slot def args]
  (let [def (ending/wrap def)
        store (:store eng)
        slots (conj (:slots base) slot)
        child-id (mem/path->id (:root base) slots)
        chain (conj (:chain base) child-id)
        fields {:source :job :level :debug :job child-id :slot slot :chain chain :round (:round base)
                :reflex (:reflex base)}
        ended! (fn [status result wait report]
                 (emit! eng (cond-> (merge fields {:kind :child_ended} (child-end-fields status result wait))
                              report (assoc :report report)
                              (:ending result)
                              (assoc :ending (dissoc (contract/for-event (:ending result)) :report)))))
        clear! #(mem/update-job! store (:root base) (:slots base) update :children dissoc slot)]
    (own! eng base)
    (limits/refuse! eng base)
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
          (ended! :declined nil w nil)
          :declined)
        (let [t0 (now eng)
              {:keys [status error ending]} (try (limits/check! eng c)
                                                 (normalize-result (or (limits/ending eng (:root base) slots nil)
                                                                       (await ((:round def) c))))
                                                 (catch :default e
                                                   (if-let [l (limits/ending eng (:root base) slots e)] {:status :ended :ending l} (throw e)))
                                                 (finally (tally/add-time! eng (:root base) slots (- (now eng) t0))))
              report (merge (tally/report eng (:root base) slots) (:report ending))
              status (if ending :done status)]
          (when-not (owner? eng (:token base)) (throw (cut-error)))
          (when (= status :error) (ended! :error nil nil report) (throw error))
          (when-not (= status :declined) (reset! child-wait nil))
          (when ending (swap! (:results base) assoc child-id (legacy/result-of (assoc ending :report report))))
          (when (= status :done) (cut-off! eng (:root base) slots (mem/job-mem (mem/view store) (:root base) slots) :ended))
          (ended! status (get @(:results base) child-id) @child-wait report)
          (if (= status :done)
            (do (tally/remember! eng (:sym def) (some->> (:target def) (select-keys args)) report)
                (clear!))
            (swap! (:results base) dissoc child-id))
          (limits/check! eng base)
          status)))))

(defn quote-ctx
  "The ctx def's quote-of gets for a call with args under base, qd the quote deadline: token-less, so it cannot act,
  call or write. self?: base's own frame. Otherwise a child's in a slot of its own, carrying the would-be call keys
  (:quote-call) and what is left of them, and the call in :quoting for the duplicate check of quotes below it."
  [eng base def args qd self?]
  (let [b (assoc base :token nil :quote-deadline qd)]
    (if self?
      (assoc (make-ctx eng b) :job-ways (:ways def))
      (let [slot ::quote
            ident (ending/frame-identity def args)
            id (mem/path->id (:root base) (conj (:slots base) slot))
            c (child-ctx eng (cond-> b ident (update :quoting (fnil conj []) [ident id])) slot args)
            call (first (ending/call-of c def))]
        (assoc c :quote-call call :left (constantly (contract/left {:call call} 0)) :job-ways (:ways def))))))

(a/defargs settings
  {:engine.core.round/short-wait-rounds
   {:default 600 :spec (a/int-in 1 nil)
    :doc "Consecutive rounds an old child under ctx/call may yield on a short wait (legacy/short-waits) before its caller sees condition-failed :wait-too-long."}})

(defn ^:async old-round
  "One round of old job def as the ctx/call child c (its id child-id) at slots under parent base: :continue or its
  ending (engine.core.legacy). Its check runs every round, as under call-child. A decline before the child's first
  round (counted in its frame) ends it condition-failed; so does one after it unless its wait reason is a short
  wait (legacy/short-waits): then :continue, the child and its memory kept, the wait put in waited, up to
  short-wait-rounds in a row."
  [eng base c child-id def slots waited]
  (let [wait (atom nil)
        child-wait (:child-wait base)
        results (:results base)
        store (:store eng)
        frame (fn [] (mem/job-mem (mem/view store) (:root base) slots))
        rounds (:call/rounds (frame) 0)
        note! (fn [rounds* waits*] (mem/update-job! store (:root base) slots assoc :call/rounds rounds* :call/waits waits*))
        failed (fn [w] (legacy/ending-of :declined nil w))
        short-wait (fn [w rounds]
                     (cond
                       (not (legacy/short-waits (:reason w))) (failed w)
                       (>= (:call/waits (frame) 0) (settings/get settings :engine.core.round/short-wait-rounds))
                       (failed (assoc w :why :wait-too-long))
                       :else (do (reset! waited w) (note! rounds (inc (:call/waits (frame) 0))) :continue)))]
    (swap! results dissoc child-id)
    (reset! child-wait nil)
    (if-not ((:check def) (assoc c :wait wait))
      (if (zero? rounds)
        (failed (wait-reason @wait))
        (short-wait (wait-reason @wait) rounds))
      (let [{:keys [status error]} (normalize-result (await ((:round def) c)))]
        (when (= status :error) (throw error))
        (let [e (if (= :declined status)
                  (short-wait @child-wait (inc rounds))
                  (do (note! (inc rounds) 0)
                      (legacy/ending-of status (get @results child-id) nil)))]
          (swap! results dissoc child-id)
          (reset! child-wait nil)
          e)))))

(defn ^:async call
  "One round of job def as the child in slot under parent base (ctx/call): :continue, or its ending with the :report
  (an old job's from engine.core.legacy); the child's memory goes with any ending, and an ending other than :done is
  kept in child-ending, the parent round's. Shares the parent's token, like call-child; each call emits
  job.child_started and job.child_ended (with :ending)."
  [eng base child-ending slot def args]
  (let [def (ending/wrap def)
        store (:store eng)
        slots (conj (:slots base) slot)
        child-id (mem/path->id (:root base) slots)
        fields {:source :job :level :debug :job child-id :slot slot :chain (conj (:chain base) child-id)
                :round (:round base) :reflex (:reflex base)}]
    (own! eng base)
    (limits/refuse! eng base)
    (if-let [dup (ending/duplicate-ending eng base def args)]
      (do (emit! eng (assoc fields :kind :duplicate_call :ending dup))
          (reset! child-ending dup)
          (assoc dup :report {}))
    (do
    (emit! eng (assoc fields :kind :child_started))
    (let [fresh? (empty? (mem/job-mem (mem/view store) (:root base) slots))
          _ (when fresh?
              (mem/update-job! store (:root base) slots (fn [m] (let [ident (ending/frame-identity def args)] (cond-> (assoc m :args args :children {}) ident (assoc :contract ident))))))
          waited (atom nil)
          t0 (now eng)
          c (child-ctx eng base slot args)
          own-limit #(limits/ending eng (:root base) slots %)
          r (try (limits/check! eng c)
                 (or (own-limit nil)
                     (let [r (await (if (ending/contract-job? def) ((:round def) c) (old-round eng base c child-id def slots waited)))]
                       (if (= :done (:kind r)) r (or (own-limit nil) r))))
                 (catch :default e
                   (or (own-limit e)
                       (do (when-not (contract/passes-through? e) (emit! eng (assoc fields :kind :child_ended :status :error)))
                           (throw e))))
                 (finally (tally/add-time! eng (:root base) slots (- (now eng) t0))))]
      (when-not (owner? eng (:token base)) (throw (cut-error)))
      (if (= :continue r)
        (do (emit! eng (cond-> (assoc fields :kind :child_ended :status :continue) @waited (assoc :reason (:reason @waited))))
            (limits/check! eng base)
            :continue)
        (let [e (assoc r :report (merge (tally/report eng (:root base) slots) (:report r)))
              done? (= :done (:kind e))]
          (emit! eng (merge fields {:kind :child_ended :status (if done? :done :stopped) :ending (dissoc (contract/for-event e) :report)
                                    :report (:report e)}
                            (when-not done? {:reason (contract/cause-key e)})))
          (tally/remember! eng (:sym def) (some->> (:target def) (select-keys args)) (:report e))
          (cut-off! eng (:root base) slots (mem/job-mem (mem/view store) (:root base) slots) :ended)
          (mem/update-job! store (:root base) (:slots base) update :children dissoc slot)
          (if done? (quote/clear-way! eng base) (do (reset! child-ending e) (quote/book-failure! eng base e)))
          (limits/check! eng base)
          e)))))))

(defn ^:async run-round [eng run inst]
  (let [[def args] (job-of eng inst)
        results (atom {})
        child-wait (atom nil)
        t0 (now eng)
        c (make-ctx eng {:root (:id run) :slots [] :chain [(:id run)] :token (:token run)
                         :args args :round (:round run) :reflex (:reflex run) :results results
                         :child-wait child-wait :t0s [t0]})
        own-limit #(limits/ending eng (:id run) [] %)
        ;; flat: a nested let here compiles to an awaited async fn, a tick before the round starts, in which a reflex could cut it
        {:keys [op job]} (:spec inst)
        ident (when (and (= :leaf op) (ending/contract-job? def)) (ending/frame-identity (assoc def :sym job) args))]
    (try
      (when ident
        (mem/update-job! (:store eng) (:id run) [] update :contract merge ident))
      (limits/check! eng c)
      (let [r (or (own-limit nil) (await ((:round def) c)))]
        (assoc (normalize-result (if (or (= :done r) (= :done (:kind r))) r (or (own-limit nil) r)))
               :result (get @results (:id run)) :child-wait @child-wait))
      (catch :default e
        (cond
          (own-limit e) {:status :ended :ending (own-limit e)}
          (cut? e) {:status :cut :error e}
          :else {:status :error :error e}))
      (finally
        (tally/add-time! eng (:id run) [] (- (now eng) t0))))))

(defn check-ctx
  "The ctx a listed job's check receives: memory and sensing, no token. wait is the atom ctx/wait notes a reason in."
  [eng inst args wait]
  (make-ctx eng {:root (:id inst) :slots [] :chain [(:id inst)] :token nil
                 :args args :round (:round inst) :reflex (:reflex inst) :wait wait}))
