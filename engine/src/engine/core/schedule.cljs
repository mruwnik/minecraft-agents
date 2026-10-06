(ns engine.core.schedule
  "The scheduling pass: readiness of listed jobs (checks, job.waiting, the next one to run), who holds
  the body, cuts, firing reflexes, tick! and do-now!."
  (:require [engine.core.base :refer [add-instance call-guarded emit! flush-save-stats! job-fields job-of new-id! now paused? reflex-text running save-memory! set-owner! state wait-reason waiting-text]]
            [engine.core.fruitless :refer [reset-backoff!]]
            [engine.core.register :refer [effective-register evaluate-register! expire-changes! preempts?]]
            [engine.core.activity :refer [check-idle!]]
            [engine.core.list-edits :refer [submit!]]
            [engine.core.round :refer [check-ctx]]
            [engine.core.settle :refer [drop-reflex-job! judge-deferred-ends! start-round!]]
            [engine.expr :as expr]
            [engine.memory :as mem]))

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
        runnable? (fn [id] (and (not (failed? id)) (check-passes? eng id)))
        holder (some #(when (and (:hold? (instances %)) (not (failed? %))) %) list)]
    (cond
      holder (when (runnable? holder) holder)
      (and resume (some #{resume} list) (runnable? resume)) resume
      (zero? n) nil
      :else (some (fn [i] (let [id (nth list (mod (+ cursor i) n))] (when (runnable? id) id)))
                  (range n)))))

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

(defn fire! [eng entry h]
  (let [id (new-id! eng)
        node (expr/parse (:jobs eng) (:job entry))
        fired (emit! eng {:source :reflex :kind :fired :level :info :reflex (:id entry)
                          :job id :interrupted (:id h) :text (reflex-text (:id entry) node)})]
    (when h (cut! eng h (:id entry) fired))
    (swap! (:state eng) add-instance id node {:reflex (:id entry)})
    (mem/create-job! (:store eng) id (second (job-of eng {:spec node})))
    (start-round! eng id)))

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
      (running eng) (check-idle! eng)
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

(defn do-now!
  "Cut a running listed job and list the job spec directly before it, holding the body: the new job runs now and the
  cut one, memory kept, continues once it ends. A running reflex is not cut; the new job waits behind it."
  [eng spec]
  (expr/parse-spec (:jobs eng) spec)
  (let [r (running eng)]
    (when (and r (not (:reflex r)))
      (cut! eng r :do-now nil))
    (submit! eng spec {:hold? true :now? true :by :agent})))
