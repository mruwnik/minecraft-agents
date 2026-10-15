(ns engine.core.ending
  "Contract jobs (a job that exports conditions; engine.contract) as the engine runs them: wrap gives the job a check
  (a listed job waits while a condition fails, until it starts) and a round that checks the conditions (an impossible
  one ends the job, one that asks for a look gets a look round first), freezes the
  call keys at the first round, asks done? before the start (and, at the first round of a call with a :least other than 1, its quote for the
  least check) and adds :because to the ending the job's round returns. Once started, the job's round decides when it is done."
  (:require [engine.contract :as contract]
            [engine.core.base :refer [cut-error manual-job now owner?]]
            [engine.core.limits :as limits]
            [engine.ctx :as ctx]
            [engine.hooks :as hooks]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(defn contract-job? [def] (some some? ((juxt :conditions :can-start? :can-run?) def)))

(defn started? [c] (some? (ctx/frozen-call c)))

(defn run-status "def's can-run? status for c (contract/verdict)." [def c] (contract/verdict c (select-keys def [:can-run?])))

(defn statuses
  "[start run] for c: start the status of def's old conditions and, until it has started, its can-start?; run its
  can-run?'s (contract/verdict). asked: {:start s :run s} a check just computed for this round, used as it is."
  ([def c] (statuses def c nil))
  ([def c asked]
   [(or (:start asked) (contract/verdict c (cond-> (dissoc def :can-run?) (started? c) (dissoc :can-start?))))
    (or (:run asked) (run-status def c))]))

(defn asked-key "c's frame, as ctx/child-status makes a child ctx without a new :id." [c] (mem/path->id (:root c) (:slots c)))

(defn remember-asked!
  "Keep a check's statuses {:start :run} for the round that follows it, so can-run? is asked once per round."
  [c asked]
  (some-> (:engine c) :asked (swap! assoc (asked-key c) asked))
  asked)

(defn take-asked!
  "The statuses the last check of c left, once."
  [c]
  (when-let [a (some-> (:engine c) :asked)]
    (let [k (asked-key c) asked (get @a k)]
      (swap! a dissoc k)
      asked)))

(defn ^:async looked
  "Statuses [start run], or after one look round (the :look/around hook) when they come to :look (no :never or :later
  among them), the statuses then; a second :look counts as :later."
  [def c ss]
  (if (= :look (first (contract/all ss)))
    (do (await ((:look/around hooks/all) c))
        (mapv #(cond-> % (= :look (first %)) (assoc 0 :later)) (statuses def c)))
    ss))

(defn stays-listed?
  "Whether a skip leaves c's job queued: a listed root not yet started, not a reflex's nor the driver's slot job."
  [c]
  (and (empty? (:slots c)) (not (started? c)) (not (:reflex c)) (not= (:root c) (manual-job (:engine c)))))

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
        defaults (contract/resolve-settings (:defaults def) #(settings/get (:settings def) %) (:args c))
        call (fill-use c def (contract/freeze (assoc def :defaults defaults) (:args c) (:call caller)))]
    (if (:call caller)
      (let [t (now (:engine c))]
        (contract/clamp call (:call caller) (contract/remainder caller t (- t (nth (:t0s c) (dec (count (:slots c))) t)))))
      [call []])))

(defn frame-identity
  "{:job sym :target args} a frame of def with args is known by: the job and the args its :target names. nil for a
  def with no symbol."
  [def args]
  (when (:sym def) {:job (:sym def) :target (select-keys args (:target def))}))

(defn duplicate-ending
  "The condition-failed ending of a call or quote of def with args from base, when a frame on base's chain (base
  itself down to the root) or a quote base's quote is asked under ((:quoting base), [[ident id] ...]) has the same
  job and target, else nil: a loop, so the parent tries another way. Booked nowhere: it is no failure of the way."
  [eng base def args]
  (when-let [ident (frame-identity def args)]
    (let [view (mem/view (:store eng))
          m (mem/job-mem view (:root base) [])
          dup #(contract/ending :condition-failed {:reason :duplicate-call :of %} nil)]
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
                     #(cond-> (assoc % :call call :started-ms (now eng)) (:sym def) (assoc :job (:sym def))))
    (when (seq lines) (ctx/emit! c :budget_clamped :debug {:lines lines :call call}))))

(defn done-now
  "What def's done? says for c; before c's call starts, under the call it would start with."
  [def c]
  (when (:done? def)
    (contract/done? def (if (started? c) c (assoc c :quote-call (first (call-of c def)))))))

(defn done-or-error
  "[done-now's answer nil], or [nil error] when done? throws: a done? that throws counts as not done."
  [def c]
  (try [(done-now def c) nil]
       (catch :default e
         (if (contract/passes-through? e) (throw e) [nil e]))))

(defn least-met?
  "Whether done?'s answer d counts units at least the call's :least (default 1; :all: none left)."
  [c d]
  (let [least (:least (ctx/frozen-call c) 1)]
    (and (number? (:done d)) (:units d)
         (if (= :all least) (zero? (:left d 0)) (>= (:done d) least)))))

(defn with-progress
  "Limit ending e (or nil) with done?'s count in its report, and done?'s :data (the job's result so far) as the data of
  a :limit-reached ending without its own; a :limit-reached ending whose done? counts the call's :least units is :done
  (the limit in its report). A done? that throws leaves e as it is."
  [def c e]
  (if-let [d (when e (try (done-now def c) (catch :default _ nil)))]
    (let [e (update e :report merge (contract/progress d))]
      (cond
        (and (= :limit-reached (:kind e)) (least-met? c d))
        (-> (contract/ending :done nil (or (:data e) (:data d))) (assoc :report (assoc (:report e) :limit (:cause e))))

        (and (= :limit-reached (:kind e)) (not (:data e)) (seq (:data d))) (assoc e :data (:data d))
        :else e))
    e))

(defn finish
  "The round's ending e with the last ending of a child that did not end :done as its :because, and done?'s count."
  [def c e]
  (with-progress def c (contract/with-because e @(:child-ending c))))

(defn ^:async started-round
  "One round of contract job def once its goal does not hold (d, done?'s answer before the start) and its conditions pass: freeze the
  call at the first, then the least check and the job's round."
  [def c d]
  (let [fresh? (not (started? c))
        c (assoc c :job-ways (:ways def))]
    (when fresh? (freeze! c def))
    (if-let [cause (when (and fresh? (:quote-of def) (not= 1 (:least (ctx/frozen-call c))))
                     (contract/least-cause (ctx/frozen-call c) (await ((:quote c) def (:args c) nil true)) d))]
      (cond-> (contract/ending :nothing-possible cause nil) d (update :report merge (contract/progress d)))
      (let [r (await ((:round def) c))]
        (cond
          (= :continue r) :continue
          (contract/ending? r) (finish def c r)
          :else (throw (js/Error. (str "a contract job's round returned " (pr-str r)
                                       ", not :continue or an ending (ctx/end)"))))))))

(defn old-never?
  "Whether the statuses [start run] hold a :never an old condition gave (bad args, game data): it beats a goal that
  holds, which a can-start? :never does not."
  [ss]
  (let [[kind _ data] (contract/all ss)]
    (and (= :never kind) (contains? data ::contract/cause))))

(defn ^:async run
  "One round of contract job def: an ending, or :continue. Before it starts, a goal that holds ends it done :already,
  whatever its checks say (but an old condition's :never); else a :never ends it impossible (a done? that throws is told with job.done_failed and
  counts as not done). A :later from its old conditions or can-start? keeps a listed job not started queued, else ends
  it condition-failed; one from can-run? yields (a reflex ends condition-failed: it runs one round; a called child
  tells its caller's wait with the status). The round's ctx
  asks can-run? between steps (ctx/can-run)."
  [def c]
  (let [ss (statuses def c (take-asked! c))
        fresh? (not (started? c))
        [d err] (when (and fresh? (not (old-never? ss))) (done-or-error def c))
        _ (when err (ctx/emit! c :done_failed :warn {:error (str (or (.-message err) err))}))
        [start-s run-s] (if (:done? d) ss (await (looked def c ss)))
        s (contract/all [start-s run-s])
        counted #(if fresh? (cond-> % d (update :report merge (contract/progress d))) (with-progress def c %))
        failed #(counted (contract/ending :condition-failed (contract/status-cause %) nil))]
    (cond
      (:done? d) (counted (contract/ending :done :already nil))
      (= :never (first s)) (contract/ending :impossible (contract/status-cause s) nil)
      (= :later (first start-s)) (if (stays-listed? c) :continue (failed start-s))
      (= :later (first run-s)) (cond (:reflex c) (failed run-s)
                                     (seq (:slots c)) (do (some-> (:child-wait c) (reset! (contract/wait-reason run-s))) :continue)
                                     :else :continue)
      :else (if-let [floor (when-not (started? c) (limits/floor-cause c #(first (call-of c def))))]
              (counted (contract/ending :condition-failed floor nil))
              (await (started-round def (assoc c :can-run #(contract/verdict c (select-keys def [:can-run?]))) d))))))

(defn spent?
  "Whether a started job's spend or deadline line is crossed (a floor is not: below one it waits)."
  [c]
  (let [eng (:engine c)]
    (boolean (some #(not (:floor (:cause %))) (limits/crossings eng (limits/frames eng (:root c) (:slots c)) nil)))))

(defn check
  "A contract job waits while its checks say :later (old conditions and can-start? until it starts, can-run? always),
  then while the body is below a floor of its call. A :never, a :look or a goal that holds before the start passes: the
  round ends the job, or looks. A started job waiting on can-run? whose spend or deadline is crossed passes too, so its
  round ends it: waiting is bounded by the call's limits, :wall-time among them."
  [def c]
  (let [fresh? (not (started? c))
        {start-s :start run-s :run} (remember-asked! c (if fresh? (zipmap [:start :run] (statuses def c)) {:run (run-status def c)}))
        s (if fresh? (contract/all [start-s run-s]) run-s)
        later? (= :later (first s))
        w (or (when later? (contract/wait-reason s))
              (some-> (limits/floor-cause c #(or (ctx/frozen-call c) (first (call-of c def)))) contract/cause-wait))]
    (cond
      (or (nil? w) (= :never (first s))) true
      fresh? (or (boolean (:done? (first (done-or-error def c)))) (ctx/wait c w))
      (and later? (spent? c)) true
      :else (ctx/wait c w))))

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
