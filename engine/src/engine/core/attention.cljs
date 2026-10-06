(ns engine.core.attention
  "Required attention requests: persisted in the state's :attention, emitted once per change, resolved
  by the job's end or an agent."
  (:require [engine.core.base :refer [emit! now state]]
            ["crypto" :as crypto]))

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
