(ns engine.core.ending
  "Contract jobs (a job that exports conditions; engine.contract) as the engine runs them: wrap gives the job a check
  (a listed job waits while its conditions fail, until it starts) and a round that checks the conditions, freezes the
  call keys at the first round, asks done? and adds :because to the ending the job's round returns."
  (:require [engine.contract :as contract]
            [engine.core.base :refer [cut-error now owner?]]
            [engine.core.limits :as limits]
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

(defn call-of
  "The call keys c's call is frozen with: its args over def's defaults (settings keys read) and the caller's call,
  clamped to the caller's remainder (engine.contract/clamp). [call lines], lines the clamp changed."
  [c def]
  (let [view ((:view c))
        caller (when (seq (:slots c)) (:contract (mem/job-mem view (:root c) (pop (:slots c)))))
        defaults (contract/resolve-settings (:defaults def) #(settings/get (:settings def) %))
        call (contract/freeze (assoc def :defaults defaults) (:args c) (:call caller))]
    (if (:call caller)
      (let [t (now (:engine c))]
        (contract/clamp call (:call caller) (contract/remainder caller (- t (nth (:t0s c) (dec (count (:slots c))) t)))))
      [call []])))

(defn freeze!
  "Write the call keys of c's call (call-of) and def's symbol to its frame's :contract; the call has started. A clamped
  call is told with job.budget_clamped."
  [c def]
  (let [eng (:engine c)
        [call lines] (call-of c def)]
    (when-not (owner? eng (:token c)) (throw (cut-error)))
    (mem/update-job! (:store eng) (:root c) (:slots c) update :contract
                     #(cond-> (assoc % :call call) (:sym def) (assoc :job (:sym def))))
    (when (seq lines) (ctx/emit! c :budget_clamped :debug {:lines lines :call call}))))

(defn finish
  "The round's ending e with the last ending of a child that did not end :done as its :because, and done?'s count."
  [def c e]
  (let [d (contract/done? def c)]
    (cond-> (contract/with-because e @(:child-ending c))
      d (update :report merge (contract/progress d)))))

(defn ^:async run
  "One round of contract job def: an ending, or :continue."
  [def c]
  (if-let [cause (or (conditions-cause c (:conditions def))
                     (when-not (started? c) (limits/floor-cause c #(first (call-of c def)))))]
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
  "A contract job waits while the body is below a floor of its call (enforced limits); a started one runs on (any
  ending ends it); one not started waits while a condition fails."
  [def c]
  (if-let [cause (or (limits/floor-cause c #(or (ctx/frozen-call c) (first (call-of c def))))
                     (when-not (started? c) (conditions-cause c (:conditions def))))]
    (ctx/wait c (wait-reason cause))
    true))

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
