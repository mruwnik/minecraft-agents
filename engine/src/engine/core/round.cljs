(ns engine.core.round
  "One round: the ctx a round or check receives, the act wrapper (token check, memory saves, action
  events) and ctx/call."
  (:require [engine.core.base :refer [cut-error cut? cut-off! cut-off-dropped! distance emit! job-of exclusive-job normalize-result now offline-cut-error owner? save-memory-soon! self-pos]]
            [engine.core.attention :refer [request-attention! resolve-attention!]]
            [engine.core.fruitless :refer [record-act!]]
            [engine.core.activity :refer [act-ended! act-started! body-act! close-section! hold-still! log-out-record open-section! useful-act?]]
            [engine.core.list-edits :refer [submit!]]
            [engine.core.cleanup :as cleanup]
            [engine.core.ending :as ending]
            [engine.core.limits :as limits]
            [engine.core.notices :as notices]
            [engine.core.quote :as quote]
            [engine.core.tally :as tally]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.chat :as chat]
            [engine.settings :as settings]
            [engine.memory :as mem]
            ["crypto" :as crypto]))

(declare call quote-ctx)

(a/defargs settings
  {:engine.core.round/spin-calls
   {:default 100000 :spec (a/int-in 1000 nil)
    :doc "Engine calls (checks, writes, acts, alive? tests) one job may make inside a single event-loop turn before it is failed as spinning: a loop that never awaits starves every other body and trigger."}
   :engine.core.round/act-max-s
   {:default 300 :spec (a/num-in 1 nil)
    :doc "Seconds one act (a primitive call) may stay in flight before it is aborted as stuck: the job fails with the act named, the body is free. :offline is allowed this plus its own time."}})

(def ^:private spin-count (volatile! 0))

(defn spin-guard!
  "Count one engine call of a job. The count restarts every event-loop turn (one setImmediate per turn, no allocation
  per call), so only code that never yields reaches the setting; it throws a plain error naming the job, which ends it failed."
  [root slots]
  (let [n (vswap! spin-count inc)]
    (when (== 1 n) (js/setImmediate #(vreset! spin-count 0)))
    (when (and (> n 1000) (> n (settings/get settings :engine.core.round/spin-calls)))
      (vreset! spin-count 0)
      (throw (js/Error. (str "spinning: job " (mem/path->id root slots) " made " n " engine calls without yielding the event loop"))))))

(defn own!
  "Throw unless the token of base is current: a cut; a quote ctx (no token) gets a plain error."
  [eng {:keys [token quote-deadline root slots]}]
  (spin-guard! root slots)
  (when quote-deadline (throw (js/Error. "a quote cannot act, call or write")))
  (when-not (owner? eng token) (throw (cut-error))))

(defn stuck-after
  "A promise that rejects with an act-stuck error after ms; on-stuck gets the timer id so the caller can clear it."
  [ms k on-stuck]
  (js/Promise. (fn [_ reject] (on-stuck (js/setTimeout #(reject (js/Error. (str "act-stuck: " (name k) " in flight over " (js/Math.round (/ ms 1000)) " s"))) ms)))))

(defn ^:async act!
  "Every acting primitive call from a job: check the ownership token, save
  memory, emit action.started, call, count what it used and yielded on every frame of the chain (engine.core.tally),
  save memory again, emit action.done. A cut rejects here (stale token) or from the primitive. A :wait with a :why
  (a pause; job.holding names it) writes no started or done event unless it fails."
  [eng {:keys [root slots token chain round reflex] :as base} id k args]
  (own! eng base)
  (limits/check-act! eng base k args)
  (act-started! eng token root k args)
  (save-memory-soon! eng)
  (let [timer (volatile! nil)
        useful? (useful-act? eng root k)
        action-id (.randomUUID crypto)
        fields {:level :debug :job id :chain chain :round round :reflex reflex :name (name k)
                :action-id action-id}
        p (:primitives eng)
        quiet? (and (= :wait k) (some? (some-> args .-why)))
        _ (when-not quiet? (emit! eng (merge fields {:source :action :kind :started :args (js->clj args)})))
        from (when (= :moveTo k) (self-pos p))
        before (tally/snapshot eng)
        away-record (when (= :offline k) (log-out-record eng root id args ((:now eng))))
        _ (when away-record
            (reset! (:away eng) away-record)
            (emit! eng (merge fields {:source :action :kind :logged-out :level :info} away-record)))]
    (try
      (when useful? (body-act! eng 1))
      (let [limit-ms (+ (* 1000 (settings/get settings :engine.core.round/act-max-s))
                        (if (= :offline k) (+ 600000 (or (some-> args .-ms) 0)) 0))
            r (await (js/Promise.race #js [(if (= :chat k) (chat/gate! eng p token args) (.call (aget p (name k)) p token args))
                                           (stuck-after limit-ms k #(vreset! timer %))]))
            _ (when (= "offline" (some-> r .-status)) (throw (offline-cut-error)))
            to (when (= :moveTo k) (self-pos p))]
        (when (= :moveTo k)
          (mem/write! (:store eng) :moved (cond-> {:from from :to to :status (.-status r)
                                                   :target (js->clj (.-pos args) :keywordize-keys true)}
                                            (= "noPath" (.-reason r)) (assoc :no-path true))
                      mem/moved-policy))
        (tally/count-act! eng root slots before)
        (save-memory-soon! eng)
        (record-act! eng {:root root :reflex reflex} k r (distance from to))
        (when-not quiet?
          (emit! eng (cond-> (merge fields {:source :action :kind :done :status (.-status r)})
                       (.-reason r) (assoc :reason (.-reason r))
                       (number? (.-distance r)) (assoc :distance (/ (js/Math.round (* 100 (.-distance r))) 100)))))
        r)
      (catch :default e
        (when (re-find #"^act-stuck" (str (.-message e)))
          (emit! eng (merge fields {:source :action :kind :stuck :level :warn :text (.-message e) :args (js->clj args)})))
        (tally/count-act! eng root slots before)
        (save-memory-soon! eng)
        (emit! eng (cond-> (merge fields {:source :action :kind :done
                                          :status (if (cut? e) :cut :failed)})
                     (not (cut? e)) (assoc :error (str e))))
        (throw e))
      (finally
        (js/clearTimeout @timer)
        (act-ended! eng token)
        (when useful? (body-act! eng -1))
        (when away-record (reset! (:away eng) nil))))))

(defn make-ctx
  "The ctx a round or a check receives. base is {:root :slots :chain :token
  :args :round :reflex}; :root is the top-level instance id and :slots the
  child slots below it ([] for the instance itself). A check gets :token nil,
  so it cannot write or act."
  [eng {:keys [root slots chain token args round reflex] :as base}]
  (let [store (:store eng)
        child-wait (or (:child-wait base) (atom nil))
        child-ending (atom nil)
        base (assoc base :child-wait child-wait
                    :section-depth (or (:section-depth base) (atom 0)) :held (or (:held base) (atom nil)))
        id (mem/path->id root slots)
        check! #(own! eng base)
        wrote! (fn [kind] (emit! eng {:source :job :kind :memory_written :level :debug :job id
                                      :chain chain :round round :reflex reflex :memory kind}))
        notice! (fn [[kind fields]]
                  (when kind
                    (emit! eng (merge fields {:source :job :kind kind :level :info :job id
                                              :chain chain :round round :reflex reflex}))))
        remember! (fn [kind data policy asked?]
                    (check!)
                    (let [was (:data (mem/latest (mem/view store) kind))]
                      (mem/write! store kind data policy)
                      (wrote! kind)
                      (notice! (notices/written (mem/view store) kind data policy was asked?))))]
    {:engine eng :primitives (:primitives eng) :token token :args args :id id
     :root root :slots slots :chain chain :round round :reflex reflex :wait (:wait base) :t0s (:t0s base) :child-wait child-wait
     :quote-deadline (:quote-deadline base)
     :left #(let [t (now eng)]
              (contract/least-left (for [[s state] (limits/frames eng root slots)]
                                     (contract/left state t (- t (nth (:t0s base) (count s) t))))))
     :book-try (fn [job reason] (check!) (when (seq slots) (tally/count-try! eng root (pop slots) job reason)))
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
     :remember #(remember! %1 %2 %3 false)
     :remember-asked #(remember! %1 %2 %3 true)
     :forget (fn [kind pred]
               (check!)
               (let [dropped (filterv #(pred (:data %)) (mem/entries (mem/view store) kind))
                     view (mem/view store)]
                 (mem/forget-where! store kind pred)
                 (wrote! kind)
                 (notice! (notices/forgotten view kind (mapv :data dropped)))))
     :forget-until (fn [kind t]
                     (check!)
                     (mem/forget-until! store kind t)
                     (wrote! kind))
     :act (fn [k act-args] (act! eng base id k act-args))
     :note-walk (fn [status moved] (check!) (record-act! eng base :walk #js {:status status} moved))
     :call (fn [slot def child-args] (call eng base child-ending slot def child-args))
     :quote (fn [def quote-args deadline self?]
              (quote/quote! eng base def quote-args deadline self? #(quote-ctx eng base def quote-args % self?)))
     :next-way quote/next-way
     :child-ending child-ending
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
     :alive? #(do (spin-guard! root slots) (or (nil? token) (owner? eng token)))
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

(defn ctx-below
  "The ctx of the frame at rel (slots relative to c's frame) below c, each with its args from memory."
  [eng c rel]
  (reduce (fn [c slot]
            (child-ctx eng c slot (:args (mem/job-mem (mem/view (:store eng)) (:root c) (conj (:slots c) slot)))))
          c rel))

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
        (assoc c :quote-call call :left (constantly (contract/left {:call call} (now eng) 0)) :job-ways (:ways def))))))

(defn ^:async call
  "One round of job def as the child in slot under parent base (ctx/call): :continue, or its ending with the :report;
  the child's memory goes with any ending, and an ending other than :done is
  kept in child-ending, the parent round's. Shares the parent's token; each call emits
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
    (reset! (:child-wait base) nil)
    (if-let [dup (ending/duplicate-ending eng base def args)]
      (do (emit! eng (assoc fields :kind :duplicate_call :ending dup))
          (reset! child-ending dup)
          (assoc dup :report {}))
    (do
    (emit! eng (assoc fields :kind :child_started))
    (let [fresh? (empty? (mem/job-mem (mem/view store) (:root base) slots))
          _ (when fresh?
              (mem/update-job! store (:root base) slots (fn [m] (let [ident (ending/frame-identity def args)] (cond-> (assoc m :args args :children {}) ident (assoc :contract ident))))))
          t0 (now eng)
          c (child-ctx eng base slot args)
          own-limit #(ending/with-progress def c (limits/ending eng (:root base) slots %))
          below #(ctx-below eng %1 %2)
          r (try (if (cleanup/closing? eng (:root base) slots)
                   (await (cleanup/step eng def c below))
                   (let [r (try (limits/check! eng c)
                                (or (own-limit nil)
                                    (let [r (await ((:round def) c))]
                                      (if (= :done (:kind r)) r (or (own-limit nil) r))))
                                (catch :default e
                                  (or (own-limit e)
                                      (do (when-not (contract/passes-through? e) (emit! eng (assoc fields :kind :child_ended :status :error)))
                                          (throw e)))))]
                     (if (= :continue r) r (await (cleanup/close eng def c r below)))))
                 (finally (tally/add-time! eng (:root base) slots (- (now eng) t0))))]
      (when-not (owner? eng (:token base)) (throw (cut-error)))
      (if (= :continue r)
        (do (emit! eng (assoc fields :kind :child_ended :status :continue))
            (limits/check! eng base)
            :continue)
        (let [e (assoc r :report (merge (tally/report eng (:root base) slots) (:report r)))
              done? (= :done (:kind e))]
          (emit! eng (merge fields {:kind :child_ended :status (if done? :done :stopped) :ending (dissoc (contract/for-event e) :report)
                                    :report (:report e)}
                            (when-not done? {:reason (contract/cause-key e)})))
          (tally/remember! eng (:sym def) (some->> (:target def) (select-keys args)) (:report e) e)
          (cut-off! eng (:root base) slots (mem/job-mem (mem/view store) (:root base) slots) :ended)
          (mem/update-job! store (:root base) (:slots base) update :children dissoc slot)
          (if done? (quote/clear-way! eng base) (do (reset! child-ending e) (quote/book-failure! eng base e)))
          (when-not (= :cut-off (:kind e))
            (tally/count-try! eng (:root base) (:slots base) (:sym def) (contract/cause-key e)))
          (limits/check! eng base)
          e)))))))

(defn ^:async close-after-throw
  "The cleanups of the root frame of c (job def) whose round threw e, run out here: the job is parked as failed, so no
  later round runs them. A cleanup that fails only adds :cleanup to the ending, which nobody reads; the error stands."
  [eng def c e]
  (try (await (cleanup/close-now eng def c (contract/ending :nothing-possible {:reason :error :error (str e)} nil)
                                 #(ctx-below eng %1 %2)))
       (catch :default _ nil)))

(defn ^:async run-round [eng run inst]
  (let [[def args] (job-of eng inst)
        child-wait (atom nil)
        t0 (now eng)
        c (make-ctx eng {:root (:id run) :slots [] :chain [(:id run)] :token (:token run)
                         :args args :round (:round run) :reflex (:reflex run)
                         :child-wait child-wait :t0s [t0]})
        own-limit #(ending/with-progress def c (limits/ending eng (:id run) [] %))
        ;; flat: a nested let here compiles to an awaited async fn, a tick before the round starts, in which a reflex could cut it
        {:keys [op job]} (:spec inst)
        ident (when (and (= :leaf op) (ending/contract-job? def)) (ending/frame-identity (assoc def :sym job) args))]
    (try
      (when ident
        (mem/update-job! (:store eng) (:id run) [] update :contract merge ident))
      (let [below #(ctx-below eng %1 %2)
            r (if (cleanup/closing? eng (:id run) [])
                (await (cleanup/step eng def c below))
                (let [r (try (limits/check! eng c)
                             (let [r (or (own-limit nil) (await ((:round def) c)))]
                               (if (or (= :done r) (= :done (:kind r))) r (or (own-limit nil) r)))
                             (catch :default e (or (own-limit e) (throw e))))]
                  (if (contract/ending? r) (await (cleanup/close eng def c r below)) r)))
            ;; a reflex root and an exclusive run get this round only
            r (if (or (:reflex run) (= (:id run) (exclusive-job eng))) (await (cleanup/run-out eng def c r below)) r)]
        (assoc (normalize-result r) :child-wait @child-wait))
      (catch :default e
        (cond
          (cut? e) {:status :cut :error e}
          (and (not (contract/passes-through? e)) (owner? eng (:token run)))
          (do (await (close-after-throw eng def c e)) {:status :error :error e})
          :else {:status :error :error e}))
      (finally
        (tally/add-time! eng (:id run) [] (- (now eng) t0))))))

(defn check-ctx
  "The ctx a listed job's check receives: memory and sensing, no token."
  [eng inst args]
  (make-ctx eng {:root (:id inst) :slots [] :chain [(:id inst)] :token nil
                 :args args :round (:round inst) :reflex (:reflex inst)}))
