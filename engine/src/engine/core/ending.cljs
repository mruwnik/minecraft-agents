(ns engine.core.ending
  "Contract jobs (a job that exports conditions; engine.contract) as the engine runs them: wrap gives the job a check
  (a listed job waits while a condition fails, until it starts) and a round that checks the conditions (an impossible
  one ends the job, one that asks for a look gets a look round first), freezes the
  call keys at the first round, asks done? (and, at the first round of a call with a :least other than 1, its quote for the least check) and adds
  :because to the ending the job's round returns."
  (:require [engine.contract :as contract]
            [engine.core.base :refer [cut-error manual-job now owner?]]
            [engine.core.limits :as limits]
            [engine.ctx :as ctx]
            [engine.hooks :as hooks]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(defn contract-job? [def] (some? (:conditions def)))

(defn started? [c] (some? (ctx/frozen-call c)))

(defn verdict
  "What conditions ([:fn f], (f c) nil, a cause, contract/impossible or contract/look-first) say now: nil to start,
  [:impossible cause] when one is impossible, else [:skip cause] for the first that fails, else [:look cause] for the
  first that asks for a look."
  [c conditions]
  (let [rs (filter identity (map (fn [[_ f]] (f c)) conditions))]
    (or (some #(some->> (contract/impossible-cause %) (vector :impossible)) rs)
        (some #(when-not (contract/look-cause %) [:skip %]) rs)
        (some #(some->> (contract/look-cause %) (vector :look)) rs))))

(defn ^:async looked-verdict
  "verdict, after one look round (the :look/around hook) when a condition asks for a look; asking again after it skips."
  [c conditions]
  (let [v (verdict c conditions)]
    (if (= :look (first v))
      (do (await ((:look/around hooks/all) c))
          (let [[k cause :as v] (verdict c conditions)] (if (= :look k) [:skip cause] v)))
      v)))

(defn stays-listed?
  "Whether a skip leaves c's job queued: a listed root not yet started, not a reflex's nor the driver's slot job."
  [c]
  (and (empty? (:slots c)) (not (started? c)) (not (:reflex c)) (not= (:root c) (manual-job (:engine c)))))

(defn wait-reason
  "A failing condition's cause as a check's wait reason: {:reason (cause key)}, with :cause when it is a map."
  [cause]
  (cond-> {:reason (contract/cause-key {:kind :condition-failed :cause cause})} (map? cause) (assoc :cause cause)))

(defn fill-use
  "call with a list in :use for each purpose def's defaults name a chooser for and the call gives none, from the
  :use/default hook."
  [c def call]
  (reduce (fn [call [purpose spec]]
            (if (contains? (:use call) purpose)
              call
              (if-let [items ((:use/default hooks/all) c purpose spec)]
                (assoc-in call [:use purpose] items)
                call)))
          call (:use (:defaults def))))

(defn call-of
  "The call keys c's call is frozen with: its args over def's defaults (settings keys read) and the caller's call,
  clamped to the caller's remainder (engine.contract/clamp). [call lines], lines the clamp changed."
  [c def]
  (let [view ((:view c))
        caller (when (seq (:slots c)) (:contract (mem/job-mem view (:root c) (pop (:slots c)))))
        defaults (contract/resolve-settings (:defaults def) #(settings/get (:settings def) %))
        call (fill-use c def (contract/freeze (assoc def :defaults defaults) (:args c) (:call caller)))]
    (if (:call caller)
      (let [t (now (:engine c))]
        (contract/clamp call (:call caller) (contract/remainder caller (- t (nth (:t0s c) (dec (count (:slots c))) t)))))
      [call []])))

(defn frame-identity
  "{:job sym :target args} a frame of def with args is known by: the job and the args its :target names. nil for a
  def with no symbol."
  [def args]
  (when (:sym def) {:job (:sym def) :target (select-keys args (:target def))}))

(defn duplicate-ending
  "The impossible ending of a call or quote of def with args from base, when a frame on base's chain (base
  itself down to the root) or a quote base's quote is asked under ((:quoting base), [[ident id] ...]) has the same
  job and target, else nil. Booked nowhere: it is no failure of the way."
  [eng base def args]
  (when-let [ident (frame-identity def args)]
    (let [view (mem/view (:store eng))
          m (mem/job-mem view (:root base) [])
          dup #(contract/ending :impossible {:reason :duplicate-call :of %} nil)]
      (or (some (fn [[q id]] (when (= ident q) (dup id))) (rseq (or (:quoting base) [])))
          (some (fn [i]
                  (let [s (subvec (:slots base) 0 i)]
                    (when (= ident (select-keys (:contract (get-in m (mem/slot-path s))) [:job :target]))
                      (dup (mem/path->id (:root base) s)))))
                (range (count (:slots base)) -1 -1))))))

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

(defn ^:async started-round
  "One round of contract job def once its conditions pass: freeze the call at the first, then done?, the least
  check and the job's round."
  [def c]
  (let [fresh? (not (started? c))
        c (assoc c :job-ways (:ways def))]
    (when fresh? (freeze! c def))
    (let [d (contract/done? def c)]
      (if (:done? d)
        (assoc (contract/ending :done nil nil) :report (contract/progress d))
        (if-let [cause (when (and fresh? (:quote-of def) (not= 1 (:least (ctx/frozen-call c))))
                         (contract/least-cause (ctx/frozen-call c) (await ((:quote c) def (:args c) nil true)) d))]
          (contract/ending :nothing-possible cause nil)
          (let [r (await ((:round def) c))]
            (cond
              (= :continue r) :continue
              (contract/ending? r) (finish def c r)
              :else (throw (js/Error. (str "a contract job's round returned " (pr-str r)
                                           ", not :continue or an ending (ctx/end)"))))))))))

(defn ^:async run
  "One round of contract job def: an ending, or :continue (a listed job not started whose condition skips)."
  [def c]
  (let [[k cause] (await (looked-verdict c (:conditions def)))]
    (cond
      (= :impossible k) (contract/ending :impossible cause nil)
      (and k (stays-listed? c)) :continue
      k (contract/ending :condition-failed cause nil)
      :else (if-let [floor (when-not (started? c) (limits/floor-cause c #(first (call-of c def))))]
              (contract/ending :condition-failed floor nil)
              (await (started-round def c))))))

(defn check
  "A contract job waits while the body is below a floor of its call (enforced limits); a started one runs on (any
  ending ends it); one not started waits while a condition fails. An impossible condition, or one that asks for a
  look, passes: the round ends the job, or looks."
  [def c]
  (let [[k cause] (when-not (started? c) (verdict c (:conditions def)))]
    (if-let [cause (when-not (= :impossible k)
                     (or (limits/floor-cause c #(or (ctx/frozen-call c) (first (call-of c def))))
                         (when (= :skip k) cause)))]
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
