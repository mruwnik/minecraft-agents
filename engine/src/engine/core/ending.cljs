(ns engine.core.ending
  "Contract jobs (a job that exports conditions; engine.contract) as the engine runs them: wrap gives the job a check
  (a listed job waits while its conditions fail, until it starts) and a round that checks the conditions, freezes the
  call keys at the first round, asks done? and adds :because to the ending the job's round returns."
  (:require [engine.contract :as contract]
            [engine.core.base :refer [cut-error emit! owner?]]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(defn contract-job? [def] (some? (:conditions def)))

(defn started? [c] (some? (ctx/frozen-call c)))

(defn conditions-cause
  "The cause of the first of conditions ([:fn f], (f c) a cause or nil) that fails now, or nil."
  [c conditions]
  (some (fn [[_ f]] (f c)) conditions))

(defn wait-reason
  "A failing condition's cause as a check's wait reason: {:reason (cause key)}, with :cause when it is a map."
  [cause]
  (cond-> {:reason (contract/cause-key {:kind :condition-failed :cause cause})} (map? cause) (assoc :cause cause)))

(defn freeze!
  "Write the call keys of c's call to its frame's :contract :call, from its args, def's defaults (settings keys read)
  and the caller's call; the call has started."
  [c def]
  (let [eng (:engine c)
        view ((:view c))
        caller (when (seq (:slots c)) (get-in (mem/job-mem view (:root c) (pop (:slots c))) [:contract :call]))
        defaults (contract/resolve-settings (:defaults def) #(settings/get (:settings def) %))
        call (contract/freeze (assoc def :defaults defaults) (:args c) caller)]
    (when-not (owner? eng (:token c)) (throw (cut-error)))
    (mem/update-job! (:store eng) (:root c) (:slots c) assoc-in [:contract :call] call)))

(defn finish
  "The round's ending e with the last ending of a child that did not end :done as its :because, and done?'s count."
  [def c e]
  (let [d (contract/done? def c)]
    (cond-> (contract/with-because e @(:child-ending c))
      d (update :report merge (contract/progress d)))))

(defn ^:async run
  "One round of contract job def: an ending, or :continue."
  [def c]
  (if-let [cause (conditions-cause c (:conditions def))]
    (contract/ending :condition-failed cause nil)
    (do (when-not (started? c) (freeze! c def))
        (let [d (contract/done? def c)]
          (if (:done? d)
            (assoc (contract/ending :done nil nil) :report (contract/progress d))
            (let [r (await ((:round def) c))]
              (cond
                (= :continue r) :continue
                (contract/ending? r) (finish def c r)
                :else (throw (js/Error. (str "a contract job's round returned " (pr-str r)
                                             ", not :continue or an ending (ctx/end)"))))))))))

(defn check
  "A started contract job runs on (any ending ends it); one not started waits while a condition fails."
  [def c]
  (or (started? c)
      (if-let [cause (conditions-cause c (:conditions def))]
        (ctx/wait c (wait-reason cause))
        true)))

(defn wrap
  "Job def with the engine's check and round when it is a contract job; any other def as it is."
  [def]
  (if (or (not (contract-job? def)) (::wrapped def))
    def
    (do (doseq [entry (:conditions def)]
          (when-not (and (vector? entry) (= :fn (first entry)) (fn? (second entry)))
            (throw (ex-info (str "a condition is [:fn f], not " (pr-str entry)) {:condition entry}))))
        (assoc def ::wrapped true :check #(check def %) :round #(run def %)))))

(defn registry "jobs with every contract job wrapped." [jobs] (some-> jobs (update-vals wrap)))

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
                  :report (contract/report (:contract child))}))
    (cut-off! eng root slots child cause)))
