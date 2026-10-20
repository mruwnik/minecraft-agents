(ns engine.core.schedule
  "The scheduling pass: readiness of listed jobs (checks, job.waiting, the next one to run), who holds
  the body, cuts, firing reflexes and holding them off during a section (ctx/atomic), tick! and do-now!."
  (:require [engine.args :as a]
            [engine.core.base :refer [add-instance dead? drop-reflex-job! emit! flush-save-stats! free-owner! job-fields job-of exclusive-job memory-flush-due? new-id! now paused? reflex-text running save-memory! state ttl-now wait-reason waiting-text]]
            [engine.core.attention :refer [request-attention!]]
            [engine.core.fruitless :refer [reset-backoff!]]
            [engine.core.register :refer [effective-register evaluate-register! expire-changes! levels preempts?]]
            [engine.core.ending :refer [clear-asked! run-status]]
            [engine.core.cleanup :as cleanup]
            [engine.core.activity :refer [check-body-idle! check-idle! restart-body-idle! section-level sections]]
            [engine.core.list-edits :refer [submit!]]
            [engine.core.round :refer [check-ctx]]
            [engine.core.settle :refer [end-slot! judge-deferred-ends! start-round!]]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(a/defargs settings
  {::urgent-wait-s {:default 15 :spec (a/num-in 0 nil)
                    :doc "Seconds an urgent reflex (night, food near 0, low health) waits for the running job's urgent section (ctx/atomic) before it cuts it."}
   ::comfort-wait-s {:default 90 :spec (a/num-in 0 nil)
                     :doc "Seconds a comfort reflex (hunger, a full bag, tidying, doors, loose items) waits for the running job's section (ctx/atomic) before it cuts it."}
   ::held-recheck-ms {:default 2000 :spec (a/num-in 0 nil)
                      :doc "How long a held trigger's job checks are not asked again, in ms of body clock."}})

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

(defn held-trigger
  "Why trigger id is held: the reason map of the start check of its job that declined, or nil."
  [eng id]
  (some-> (:held-triggers eng) deref (get id)))

(defn check-passes?
  "Whether listed instance id would start now (ctx/check-status; a job closing for its cleanup always would). Its wait reason is told once with
  note-waiting!. A throwing check is a bug, not a world state: the job is parked like a round that threw."
  [eng id]
  (let [inst (get-in (state eng) [:instances id])
        wait (atom nil)
        ok? (try (let [[def args] (job-of eng inst)]
                   (or (cleanup/closing? eng id [])
                       (let [s (ctx/check-status (check-ctx eng inst args) def)]
                         (or (not= :later (first s)) (do (reset! wait (contract/status-cause s)) false)))))
                 (catch :default e
                   (let [error (str e)]
                     (emit! eng {:source :system :kind :error :level :warn :job id
                                 :text (str "check of " id " (" (expr/label (:spec inst)) ") threw: " error)})
                     (request-attention! eng {:job-id id :reason :round-failed :kind :failed
                                              :data {:error error}
                                              :message (str "Job failed and is parked until retried or cancelled: " error)
                                              :state-update #(assoc-in % [:failed id] {:error error :t (now eng)})}))
                   false))]
    (note-waiting! eng id (if ok? ::passed @wait))
    ok?))

(defn choose-listed
  "The listed job to run next, never a failed one nor, while the body is dead, one kept through the death: the cut
  job, else round-robin from the cursor over passing checks."
  [eng]
  (let [{:keys [list instances resume cursor]} (state eng)
        n (count list)
        dead (dead? eng)
        failed? (fn [id] (contains? (:failed (state eng)) id))
        runnable? (fn [id] (and (not (failed? id)) (not (and dead (:survive-death? (instances id)))) (check-passes? eng id)))]
    (cond
      (and resume (some #{resume} list) (runnable? resume)) resume
      (zero? n) nil
      :else (some (fn [i] (let [id (nth list (mod (+ cursor i) n))] (when (runnable? id) id)))
                  (range n)))))

(defn holder
  "Who has the body: the running round."
  [eng]
  (running eng))

(defn cut!
  "Take the body from holder h for entry; cause is the seq of the firing. A reflex job is dropped, unless it is in its
  cleanup: a pause is not an ending, so it stays (paused-reflex) and resumes."
  [eng h by cause]
  (free-owner! eng)
  (reset! (:running eng) nil)
  (let [paused? (or (not (:reflex h)) (cleanup/closing? eng (:id h) []))]
    (cond
      (not (:reflex h)) (swap! (:state eng) assoc :resume (:id h) :current nil)
      (not paused?) (drop-reflex-job! eng (:id h) (:reflex h) :dropped {:how :dropped :by by :cause cause}))
    (when paused?
      (emit! eng (merge (job-fields eng (:id h)) {:source :job :kind :cut :level :info :by by :cause cause})))))

(defn paused-reflexes
  "The reflex jobs cut in their cleanup (cut!) as {entry id job id}."
  [eng]
  (into {} (keep (fn [[id inst]] (when (and (:reflex inst) (cleanup/closing? eng id [])) [(:reflex inst) id])))
        (:instances (state eng))))

(defn paused-reflex
  "The reflex job cut in its cleanup (cut!) whose entry comes first in the register, else any (its entry may have left
  the register): it gets the body before the list does."
  [eng order]
  (let [paused (paused-reflexes eng)]
    (or (some #(paused (:id %)) order) (first (vals paused)))))

(defn trigger-status
  "The status entry's job would start with now: its start and run checks as one (contract/all), over a ctx of the job
  as it would be fired. A check that throws holds the trigger (:check-threw, told by startable-trigger?)."
  [eng entry]
  (try (let [node (expr/parse (:jobs eng) (:job entry))
             inst {:id (str "trigger:" (name (:id entry))) :spec node :reflex (:id entry)}
             [def args] (job-of eng inst)
             c (check-ctx eng inst args)]
         (contract/all [(ctx/check-status c def) (run-status def c)]))
       (catch :default e
         (contract/later :check-threw (str e)))))

(defn startable-trigger?
  "Whether the job of firing entry would start (trigger-status says neither :later nor :never). A held trigger is kept
  for /triggers and told once per reason as reflex.held (a warn when the check threw); its checks are not asked again
  for held-recheck-ms."
  [eng entry]
  (let [id (:id entry)
        t (now eng)
        before (get @(:held-triggers eng) id)]
    (if (and before (< (- t (:t (meta before))) (settings/get settings ::held-recheck-ms)))
      false
      (let [[kind :as s] (trigger-status eng entry)
            ok? (not (#{:later :never} kind))
            r (when-not ok? (contract/status-cause s))]
        (if ok?
          (swap! (:held-triggers eng) dissoc id)
          (do (swap! (:held-triggers eng) assoc id (with-meta r {:t t}))
              (when (not= before r)
                (emit! eng {:source :reflex :kind :held :level (if (= :check-threw (:reason r)) :warn :info) :reflex id :data r
                            :text (str "trigger " (name id) " held: " (:text r))}))))
        ok?))))

(defn fire! [eng entry h]
  (let [id (new-id! eng)
        node (expr/parse (:jobs eng) (:job entry))
        fired (emit! eng {:source :reflex :kind :fired :level :info :reflex (:id entry)
                          :job id :interrupted (:id h) :text (reflex-text (:id entry) node)})]
    (when h (cut! eng h (:id entry) fired))
    (swap! (:state eng) add-instance id node {:reflex (:id entry)})
    (mem/create-job! (:store eng) id (expr/label node) (second (job-of eng {:spec node})))
    (start-round! eng id)))

(defn wait-cap-ms
  "How long a reflex at level waits for a section, in ms: danger never waits."
  [level]
  (* 1000 (case level
            :urgent (settings/get settings ::urgent-wait-s)
            :comfort (settings/get settings ::comfort-wait-s)
            0)))

(defn under-section?
  "Whether firing entry's level is at or below the highest level of the sections open in holder h's round."
  [eng h entry]
  (let [held (section-level eng (:token h))]
    (boolean (and held (<= (levels (:level entry)) (levels held))))))

(defn choose-firing!
  "The entry to fire now among the firing ones (in register order), or nil: the first that preempts holder h and is
  not held off. An entry under h's open sections is held off until it has waited its level's cap since it was first
  held off. :deferred keeps {id since} of the entries held off ahead of the chosen one: reflex.deferred when one is
  new, job.section_overran when the chosen one cuts a section past its cap."
  [eng order firing h]
  (let [t (now eng)
        before @(:deferred eng)
        since #(get before (:id %) t)
        held-off? #(and (under-section? eng h %) (< (- t (since %)) (wait-cap-ms (:level %))))
        [held entry] (loop [[e & more] firing held []]
                       (cond (nil? e) [held nil]
                             (not (and (preempts? order h e) (startable-trigger? eng e))) (recur more held)
                             (held-off? e) (recur more (conj held e))
                             :else [held e]))
        section (some-> (sections eng (:token h)) last :section)]
    (swap! (:held-triggers eng) select-keys (map :id firing))
    (reset! (:deferred eng) (into {} (map (fn [e] [(:id e) (since e)])) held))
    (doseq [e held :when (not (contains? before (:id e)))]
      (emit! eng {:source :reflex :kind :deferred :level :info :reflex (:id e) :urgency (:level e) :job (:id h)
                  :section section :wait-s (/ (wait-cap-ms (:level e)) 1000)
                  :text (str (name (:id e)) " (" (name (:level e)) ") waits for " (:id h) " to end section "
                             (name section))}))
    (when (and entry (contains? before (:id entry)) (under-section? eng h entry))
      (emit! eng (merge (job-fields eng (:id h))
                        {:source :job :kind :section_overran :level :warn :reflex (:id entry) :section section
                         :held-ms (- t (since entry))
                         :text (str "section " (name section) " ran past the " (/ (wait-cap-ms (:level entry)) 1000)
                                    " s " (name (:level entry)) " reflex " (name (:id entry)) " may wait: cut")})))
    entry))

(defn housekeep!
  "Expire register changes, and emit the save stats and sweep memory when due."
  [eng]
  (expire-changes! eng)
  (when (>= (- (now eng) @(:last-stats eng)) (:stats-ms eng))
    (reset! (:last-stats eng) (now eng))
    (flush-save-stats! eng))
  (when (>= (- (now eng) @(:last-sweep eng)) (:sweep-ms eng))
    (reset! (:last-sweep eng) (now eng))
    (save-memory! eng))
  (when (memory-flush-due? eng)
    (save-memory! eng)))

(defn tick-exclusive!
  "One pass under an exclusive run: no trigger, no loop. A listed job still in its cleanup (a cancelled run) runs first. Then the exclusive job gets one call (see
  settle/end-slot!): a failing check ends it at once with the check's reason. With it gone the body idles."
  [eng]
  (housekeep! eng)
  (reset! (:deferred eng) {})
  (if (running eng)
    (check-idle! eng)
    (when-let [id (or (some #(when (cleanup/closing? eng % []) %) (:list (state eng))) (exclusive-job eng))]
      (if (check-passes? eng id)
        (start-round! eng id)
        (do (end-slot! eng id (:reason (waiting eng id) :not-ready) (waiting eng id))
            nil)))))

(defonce rechecked (js/WeakMap.))

(defn recheck-waits!
  "While a reflex holds the body, the listed jobs' wait reasons are asked again every held-recheck-ms: a reason told
  before it started (the dark of a night gone) would else stay on the list."
  [eng]
  (let [t (now eng)
        key (:waiting eng)]
    (when (>= (- t (or (.get rechecked key) 0)) (settings/get settings ::held-recheck-ms))
      (.set rechecked key t)
      (doseq [id (:list (state eng))] (check-passes? eng id)))))

(defn tick-online!
  "One scheduling pass for a body that is on the server."
  [eng]
  (housekeep! eng)
  (check-body-idle! eng)
  (let [order (effective-register (state eng) (ttl-now eng))
        h (holder eng)
        paused (paused-reflexes eng)
        firing (choose-firing! eng order (remove #(contains? paused (:id %)) (evaluate-register! eng order)) h)]
    (cond
      firing (fire! eng firing h)
      (running eng) (do (when (:reflex h) (recheck-waits! eng)) (check-idle! eng))
      :else (when-let [id (or (paused-reflex eng order) (choose-listed eng))] (start-round! eng id)))))

(defn tick!
  "One scheduling pass. Synchronous; returns the promise of a round it
  started (resolving once that round is settled), or nil. Does nothing while
  the body is offline or settling. Under an exclusive run only that one job runs (tick-exclusive!).
  The first ready tick after a pause clears the backoff and judges the reflex
  ends deferred meanwhile."
  [eng]
  (clear-asked! eng)
  (cond
    (paused? eng) (do (reset! (:was-paused eng) true) (restart-body-idle! eng) nil)
    (exclusive-job eng) (do (reset! (:was-paused eng) true) (restart-body-idle! eng) (tick-exclusive! eng))
    :else
    (do (when @(:was-paused eng)
          (reset! (:was-paused eng) false)
          (reset-backoff! eng))
        (judge-deferred-ends! eng)
        (tick-online! eng))))

(defn exclusive-run!
  "Pause the whole engine and run the job spec alone: whatever holds the body is cut at once (a listed job continues
  after the run, a reflex job is dropped unless it is in its cleanup), no trigger or other job runs until the job ends.
  The caller has checked that the body is online and no run is in flight."
  [eng spec]
  (expr/parse (:jobs eng) spec)
  (when-let [h (holder eng)] (cut! eng h :exclusive nil))
  (submit! eng spec {:exclusive? true :by :agent}))

(defn do-now!
  "Cut a running listed job, open sections or not, and list the job spec directly before it as a normal job: it runs
  next, and the cut one, memory kept, continues right after it yields or ends. A running reflex is not cut; the new job
  waits behind it, as does a running exclusive run."
  [eng spec]
  (expr/parse (:jobs eng) spec)
  (let [r (running eng)]
    (when (and r (not (:reflex r)) (not (exclusive-job eng)))
      (cut! eng r :do-now nil))
    (let [id (submit! eng spec {:now? true :by :agent})]
      (swap! (:state eng) assoc :resume id)
      id)))
