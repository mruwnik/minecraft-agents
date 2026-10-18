(ns engine.core.list-edits
  "Edits of the job list: submit! (at the end, next or now), cancel! and retry!."
  (:require [engine.core.base :refer [add-instance delete-root! drop-reflex-job! emit! free-owner! job-fields job-of new-id! remove-listed running save-memory! state]]
            [engine.core.attention :refer [resolve-job-attention!]]
            [engine.core.cleanup :as cleanup]
            [engine.core.tally :as tally]
            [engine.expr :as expr]
            [engine.memory :as mem]))

(defn next-position
  "Index in the list right after the current job: the one running (or cut and
  waiting to resume), else where the next scan starts, which is just after the
  job that ran last."
  [{:keys [list current resume cursor]}]
  (let [idx (.indexOf list (or current resume))]
    (if (neg? idx)
      (min cursor (count list))
      (inc idx))))

(defn insert-next
  "State with id listed directly after the current job, so it gets the next round."
  [state id]
  (let [pos (next-position state)]
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

(defn drop-cancelled!
  "Remove listed job id cancelled by by, its memory cut off; job.cancelled with its report and data (by default the
  :cut-off ending's)."
  [eng id by report data]
  (let [fields (job-fields eng id)]
    (resolve-job-attention! eng id :job-cancelled #(remove-listed % id))
    (swap! (:fruitless eng) dissoc id)
    (swap! (:rounds eng) dissoc id)
    (delete-root! eng id by)
    (save-memory! eng)
    (emit! eng (merge (into {} (filter val) (select-keys fields [:name :reflex]))
                      {:source :job :kind :cancelled :level :info :job id :chain [id] :by by :report report
                       :data (merge data {:kind :cut-off :cause by})}))))

(defn cancel!
  "End job id (cutting its round if it is the one running): a listed job is removed, cut off by by (job.cancelled
  :data, ending e's data and report over it); a reflex job ends with reflex.ended :cancelled and its trigger may fire
  again. by names who asked. A listed job that owes a cleanup (engine.core.cleanup) stays listed until it has run
  (job.cleaning_up now, job.cancelled then, e its ending); a cancel while it runs does not cut it. A death (by :death)
  runs none."
  ([eng id] (cancel! eng id :agent))
  ([eng id by] (cancel! eng id by nil))
  ([eng id by e]
   (let [fields (job-fields eng id)
         report (merge (tally/report eng id []) (:report e))
         inst (get-in (state eng) [:instances id])
         m (mem/job-mem (mem/view (:store eng)) id [])
         cleans? (and inst (not (:reflex inst)) (not= :death by)
                      (or (cleanup/closing? eng id []) (cleanup/owes? eng (first (job-of eng inst)) m)))]
     (when (= id (:id (running eng)))
       (free-owner! eng)
       (reset! (:running eng) nil)
       (when cleans? (swap! (:state eng) assoc :resume id :current nil)))
     (cond
       cleans?
       (do (when-not (cleanup/closing? eng id [])
             (cleanup/mark-closing! eng id [] (or (dissoc e :report) {:kind :cut-off :cause by})))
           (emit! eng (merge (into {} (filter val) (select-keys fields [:name]))
                             {:source :job :kind :cleaning_up :level :info :job id :chain [id] :by by
                              :text "cancelled: the job's cleanup runs first"})))

       (:reflex inst)
       (do (drop-reflex-job! eng id (:reflex inst) :cancelled {:how :cancelled :by by})
           (emit! eng (merge (into {} (filter val) (select-keys fields [:name :reflex]))
                             {:source :job :kind :cancelled :level :info :job id :chain [id] :by by :report report
                              :data (merge (:data e) {:kind :cut-off :cause by})})))

       :else (drop-cancelled! eng id by report (:data e))))))

(defn submit!
  "Put a job spec (an expression, see engine.expr) on the list. Returns the instance id; throws on a bad spec.
  opts:
    :next?    list it directly after the current job, so it gets the next round
    :now?      list it directly before the current job (do-now!)
    :exclusive? the job is an exclusive run (schedule/exclusive-run!): listed at the end, alone on the body
    :by        who asked, for the event
    :survive-death?  the job stays listed through a death and runs again after the respawn (see core/drop-jobs-on-death!)
  An exclusive run is listed at the end (:next? and :now? ignored)."
  [eng spec {:keys [next? now? by] :as opts}]
  (let [node (expr/parse (:jobs eng) spec)
        run? (boolean (:exclusive? opts))
        args (second (job-of eng {:spec node}))
        next? (and next? (not run?))
        now? (and now? (not run?))
        id (new-id! eng)]
    (when run? (reset! (:exclusive eng) id))
    (swap! (:state eng) #(let [s (add-instance % id node (cond-> {} run? (assoc :exclusive? true) (:survive-death? opts) (assoc :survive-death? true)))]
                           (cond
                             now? (insert-now s id)
                             next? (insert-next s id)
                             :else (update s :list conj id))))
    (mem/create-job! (:store eng) id args)
    (save-memory! eng)
    (emit! eng (cond-> {:source :job :kind :queued :level :info :job id :chain [id] :name (expr/label node)
                        :spec (pr-str spec) :by by}
                 run? (assoc :exclusive true)))
    id))

(defn retry!
  "Clear the failed mark of listed job id so the scheduler runs it again, memory
  as it was. False when the job is not marked failed."
  [eng id]
  (if-not (contains? (:failed (state eng)) id)
    false
    (do (resolve-job-attention! eng id :job-retried #(update % :failed dissoc id))
        (emit! eng {:source :job :kind :retried :level :info :job id :chain [id] :by :agent})
        true)))
