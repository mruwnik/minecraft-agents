(ns engine.core.list-edits
  "Edits of the job list: submit! (at the end, front or now), cancel! and retry!."
  (:require [engine.core.base :refer [add-instance driver? emit! free-owner! job-of manual-job new-id! remove-listed running save-memory! state]]
            [engine.core.attention :refer [resolve-job-attention!]]
            [engine.expr :as expr]
            [engine.memory :as mem]))

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

(defn cancel!
  "Remove listed job id (cutting its round if it is the one running). by names who asked."
  ([eng id] (cancel! eng id :agent))
  ([eng id by]
    (when (= id (:id (running eng)))
      (free-owner! eng)
      (reset! (:running eng) nil))
    (resolve-job-attention! eng id :job-cancelled #(remove-listed % id))
    (swap! (:fruitless eng) dissoc id)
    (swap! (:rounds eng) dissoc id)
    (mem/delete-job! (:store eng) id)
    (save-memory! eng)
    (emit! eng {:source :job :kind :cancelled :level :info :job id :chain [id] :by by})))

(defn submit!
  "Put a job spec (an expression, see engine.expr) on the list. Returns the instance id; throws on a bad spec.
  opts:
    :hold?     hold the body (same as wrapping the spec in (hold e))
    :front?    list it directly after the current job, so it gets the next round
    :now?      list it directly before the current job (do-now!)
    :by        who asked, for the event
  While someone holds the manual lease, a job submitted by them is the slot job: it is listed at the end (:front? and
  :now? ignored) and replaces (cancels) the previous slot job. Jobs from others are listed as usual and wait."
  [eng spec {:keys [front? now? by] :as opts}]
  (let [{:keys [node hold?]} (expr/parse-spec (:jobs eng) spec)
        hold? (boolean (or hold? (:hold? opts)))
        args (second (job-of eng {:spec node}))
        slot? (driver? eng by)
        front? (and front? (not slot?))
        now? (and now? (not slot?))
        _ (when-let [old (and slot? (manual-job eng))] (cancel! eng old by))
        id (new-id! eng)]
    (when slot? (reset! (:manual-job eng) id))
    (swap! (:state eng) #(let [s (add-instance % id node {:hold? hold?})]
                           (cond
                             now? (insert-now s id)
                             front? (insert-front s id)
                             :else (update s :list conj id))))
    (mem/create-job! (:store eng) id args)
    (save-memory! eng)
    (emit! eng {:source :job :kind :queued :level :info :job id :chain [id] :name (expr/label node)
                :spec (pr-str spec) :hold hold? :by by})
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
