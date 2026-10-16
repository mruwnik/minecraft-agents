(ns engine.core.ending
  "Contract jobs (engine.contract) as the engine runs them: wrap gives the job a check (a listed job waits while
  can-start? says :later, until it starts, and while can-run? does) and a round that asks them (a :never ends the job,
  a :look gets a look round first), freezes the
  call keys at the first round, asks done? before every round and between steps (an :ok ends the job :done; at the first round of a
  call with a :least other than 1 it asks its quote for the least check) and adds :because to the ending the job's round returns.
  Every ending the engine makes carries done?'s :result and count."
  (:require [engine.condition :as condition]
            [engine.contract :as contract]
            [engine.core.base :refer [cut-error exclusive-job now owner?]]
            [engine.core.limits :as limits]
            [engine.ctx :as ctx]
            [engine.hooks :as hooks]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(defn contract-job?
  "A def with can-start? or can-run?, or a round and no old check: the checks default to :ok."
  [def]
  (boolean (or (some some? ((juxt :can-start? :can-run?) def)) (::wrapped def) (and (:round def) (nil? (:check def))))))

(defn started? [c] (some? (ctx/frozen-call c)))

(def compiled-forms "{form checker}: a condition form an export or a :checks line is, compiled when first asked." (atom {}))

(defn compiled
  "The checker fn of condition form, compiled with the job-only vocabulary and cached by form; a refusal throws naming what."
  [form what]
  (or (get @compiled-forms form)
      (let [r (condition/compile form (:ready/vocabulary hooks/all))]
        (when (condition/refusal? r)
          (throw (js/Error. (str what ": " (:message r)))))
        (swap! compiled-forms assoc form (:check r))
        (:check r))))

(defn export-check
  "The checker fn of def's export k (:safe? or :fit?: a checker fn or a quoted condition form), or nil when it has none."
  [def k]
  (let [x (get def k)]
    (if (seq? x) (compiled x (str (name k) " of " (:sym def))) x)))

(declare inherited-checks)

(defn checks-of
  "The :checks (form per key) c's frame runs under: its frozen call's once started, else what it would freeze with."
  [def c]
  (if-let [call (ctx/frozen-call c)]
    (:checks call)
    (merge (inherited-checks c) (:checks (:defaults def)) (:checks (:args c)))))

(defn run-status
  "def's can-run?, safe? and fit? statuses for c as one (contract/all); a def lacking :safe? or :fit? is an error. A :checks form of the
  frame's call (its own, else the nearest caller's, per key) replaces the export; c carries the def, with its exports
  as checker fns, for the forms' (job-safe) and (exposed)."
  [def c]
  (let [checks (checks-of def c)
        own (assoc def :safe? (export-check def :safe?) :fit? (export-check def :fit?))
        c (assoc c :def own)]
    (contract/all (for [k [:can-run? :safe? :fit?]
                        :let [form (get checks k)
                              f (cond form (compiled form (str (name k) " of " (:sym def) " :checks"))
                                      (= k :can-run?) (export-check def k)
                                      :else (or (get own k) (throw (js/Error. (str "job " (or (:sym def) (:label def)) " lacks " k " (ready/ok for a job with nothing to check)")))))]
                        :when f]
                    (contract/asked c def k f)))))

(defn statuses
  "[start run] for c: start def's can-start? status until it has started (:ok after); run its can-run?'s
  (contract/verdict). asked: {:start s :run s} a check just computed for this round, used as it is."
  ([def c] (statuses def c nil))
  ([def c asked]
   [(or (:start asked) (contract/verdict c (cond-> (dissoc def :can-run?) (started? c) (dissoc :can-start?))))
    (or (:run asked) (run-status def c))]))

(defn asked-key "c's frame, as ctx/child-status makes a child ctx without a new :id." [c] (mem/path->id (:root c) (:slots c)))

(defn remember-asked!
  "Keep a check's statuses {:start :run} with the args it was asked under for the round that follows it, so can-run? is
  asked once per round. The scheduler drops what a pass leaves unused (clear-asked!)."
  [c asked]
  (some-> (:engine c) :asked (swap! assoc (asked-key c) [(:args c) asked]))
  asked)

(defn take-asked!
  "The statuses the last check of c left, once, when it was asked under c's args."
  [c]
  (when-let [a (some-> (:engine c) :asked)]
    (let [k (asked-key c) [args asked] (get @a k)]
      (swap! a dissoc k)
      (when (= args (:args c)) asked))))

(defn clear-asked! "Drop every kept check: they last for the pass that asked them." [eng]
  (reset! (:asked eng) {}))

(defn later-looks
  "Statuses ss with each :look made :later."
  [ss]
  (mapv #(cond-> % (= :look (first %)) (assoc 0 :later)) ss))

(defn look-key
  "What a look of c's frame was about: the :look-at cells of the :look statuses in ss and the cell the body stood in."
  [c ss]
  [(into [] (comp (filter #(= :look (first %))) (mapcat #(:look-at (nth % 2 nil)))) ss)
   (some->> (some-> (:primitives c) .self .-pos) ((juxt #(.-x %) #(.-y %) #(.-z %))) (mapv js/Math.floor))])

(defn looked?
  "Whether a look round already followed a :look of c's frame about key, which no check since has passed without."
  [c key]
  (= key (some-> (:engine c) :looks deref (get [(:root c) (:slots c)]))))

(defn note-look!
  "Record the key of the look c's frame has looked on, or nil to clear it: after a look round that left a :look, the
  next :look about the same cells from the same spot counts as :later (across rounds)."
  [c key]
  (some-> (:engine c) :looks (swap! (if key assoc dissoc) [(:root c) (:slots c)] key))
  nil)

(defn ^:async looked
  "Statuses [start run], or after one look round (the :look/around hook, given the :look-at cells of the :look statuses)
  when they come to :look (no :never or :later among them), the statuses then; a second :look counts as :later."
  [def c ss]
  (if (= :look (first (contract/all ss)))
    (do (await ((:look/around hooks/all) c (into [] (comp (filter #(= :look (first %))) (mapcat #(:look-at (nth % 2 nil)))) ss)))
        (let [after (statuses def c)]
          (note-look! c (when (= :look (first (contract/all after))) (look-key c after)))
          (later-looks after)))
    ss))

(defn stays-listed?
  "Whether a skip leaves c's job queued: a listed root not yet started, not a reflex's nor the driver's slot job."
  [c]
  (and (empty? (:slots c)) (not (started? c)) (not (:reflex c)) (not= (:root c) (exclusive-job (:engine c)))))

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
        call (fill-use c def (contract/freeze (assoc def :defaults defaults) (:args c) (:call caller)))
        call (cond-> call (seq (inherited-checks c)) (update :checks #(merge (inherited-checks c) %)))]
    (if (:call caller)
      (let [t (now (:engine c))]
        (contract/clamp call (:call caller) (contract/remainder caller t (- t (nth (:t0s c) (dec (count (:slots c))) t)))))
      [call []])))

(defn inherited-checks
  "The :checks of c's callers, per key from the nearest one."
  [c]
  (when (seq (:slots c))
    (let [view ((:view c))]
      (reduce (fn [acc i] (merge acc (get-in (mem/job-mem view (:root c) (subvec (:slots c) 0 i)) [:contract :call :checks])))
              {} (range (count (:slots c)))))))

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
  "What def's done? says for c, as a status (contract/done?); before c's call starts, under the call it would start
  with."
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
  "Whether done?'s data d counts units at least the call's :least (default 1; :all: none left)."
  [c d]
  (let [least (:least (ctx/frozen-call c) 1)]
    (and (number? (:done d)) (:units d)
         (if (= :all least) (zero? (:left d 0)) (>= (:done d) least)))))

(defn ends? "Whether done? status s ends the job." [s] (= :ok (first s)))

(defn done-ending
  "The :done ending of done?'s status s: cause :already before the start; s's :result with its text as data, its count
  in the report."
  [s fresh?]
  (let [[_ text {:keys [result]}] s]
    (-> (contract/ending :done (when fresh? :already) (assoc result :text text))
        (assoc :report (contract/progress s)))))

(defn progress-ending
  "Ending e (or nil) with done? status s's count in its report, and s's :result (the job's result so far) as the data of
  a :limit-reached or :cut-off ending without its own; a :limit-reached ending whose s counts the call's :least units
  is :done (the limit in its report). c may be nil for a cut-off."
  [c s e]
  (if (and e s)
    (let [e (update e :report merge (contract/progress s))
          {:keys [result] :as d} (nth s 2)]
      (cond
        (and (= :limit-reached (:kind e)) (least-met? c d))
        (-> (contract/ending :done nil (or (:data e) result)) (assoc :report (assoc (:report e) :limit (:cause e))))

        (and (#{:limit-reached :cut-off} (:kind e)) (not (:data e)) (seq result)) (assoc e :data result)
        :else e))
    e))

(defn with-progress
  "Ending e (or nil) with done? asked afresh (progress-ending). A done? that throws leaves e as it is."
  [def c e]
  (progress-ending c (when e (try (done-now def c) (catch :default _ nil))) e))

(defn remember-done!
  "Keep done? status s of a root frame c for a cancel, which asks nothing."
  [c s]
  (when (and s (empty? (:slots c))) (some-> (:engine c) :seen-done (swap! assoc (asked-key c) s)))
  s)

(defn forget-done! [c] (some-> (:engine c) :seen-done (swap! dissoc (asked-key c))))

(defn progress-of
  "Cut-off ending e of root job id with the count of the last done? status asked for it (nothing asked: e as it is)."
  [eng id e]
  (let [seen (:seen-done eng) k (mem/path->id id [])
        s (get @seen k)]
    (swap! seen dissoc k)
    (progress-ending nil s e)))

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
                     (contract/least-cause (ctx/frozen-call c) (await ((:quote c) def (:args c) nil true)) (some-> d (nth 2))))]
      (cond-> (contract/ending :nothing-possible cause nil) d (update :report merge (contract/progress d)))
      (let [r (await ((:round def) c))]
        (cond
          (= :continue r) :continue
          (contract/ending? r) (finish def c r)
          :else (throw (js/Error. (str "a contract job's round returned " (pr-str r)
                                       ", not :continue or an ending (ctx/end)"))))))))

(defn bad-args?
  "Whether the statuses [start run] hold a :never that beats a goal that holds: before the start a can-start? :never
  :bad-args. Any other :never does not."
  [ss fresh?]
  (let [[kind _ data] (contract/all ss)]
    (and fresh? (= :never kind) (= :bad-args (:reason data)))))

(defn refused?
  "Whether the check just asked for fresh root job id left a can-start? :never :bad-args: the job is refused, its round starts nothing."
  [eng id]
  (let [[_ asked] (get @(:asked eng) id)]
    (bad-args? [(:start asked) (:run asked)] true)))

(defn done-between
  "The ending done? gives between steps of a started job (ctx/done): its :done ending when it ends the job, else nil."
  [def c]
  (let [s (remember-done! c (first (done-or-error def c)))]
    (when (and s (ends? s)) (done-ending s false))))

(defn ended!
  "r, the round's answer, after a root job's remembered done? status is dropped once r ends it."
  [c r]
  (when-not (= :continue r) (forget-done! c) (note-look! c nil))
  r)

(defn ^:async run
  "One round of contract job def: an ending, or :continue. A goal that holds ends it :done (:already before the start),
  whatever its checks say but a can-start? :never :bad-args (a done? that throws is told with job.done_failed and counts
  as not done); else a :never ends it impossible. A :later from its can-start? keeps a listed job
  not started queued, else ends it condition-failed; one from can-run? yields (a reflex ends condition-failed: it runs
  one round; a called child tells its caller's wait with the status). The round's ctx asks done? and can-run? between
  steps (ctx/done, ctx/can-run)."
  [def c]
  (let [asked (take-asked! c)
        ss (statuses def c asked)
        fresh? (not (started? c))
        [d err] (if-let [de (:done asked)] de (when-not (bad-args? ss fresh?) (done-or-error def c)))
        _ (remember-done! c d)
        _ (when err (ctx/emit! c :done_failed :warn {:error (str (or (.-message err) err))}))
        done? (and d (ends? d))
        [start-s run-s] (if done? ss (await (looked def c ss)))
        s (contract/all [start-s run-s])
        counted #(if fresh? (cond-> % d (update :report merge (contract/progress d))) (progress-ending c d %))
        failed #(counted (contract/ending :condition-failed (contract/status-cause %) nil))]
    (ended! c
     (cond
       done? (done-ending d fresh?)
       (= :never (first s)) (contract/ending :impossible (contract/status-cause s) nil)
       (= :later (first start-s)) (if (stays-listed? c) :continue (failed start-s))
       (= :later (first run-s)) (cond (:reflex c) (failed run-s)
                                      (seq (:slots c)) (do (some-> (:child-wait c) (reset! (contract/status-cause run-s))) :continue)
                                      :else :continue)
       :else (if-let [floor (when-not (started? c) (limits/floor-cause c #(first (call-of c def))))]
               (counted (contract/ending :condition-failed floor nil))
               (await (started-round def (assoc c :can-run #(contract/verdict c (select-keys def [:can-run?]))
                                                :done #(done-between def c))
                                     d)))))))

(defn spent?
  "Whether a started job's spend or deadline line is crossed (a floor is not: below one it waits)."
  [c]
  (let [eng (:engine c)]
    (boolean (some #(not (:floor (:cause %))) (limits/crossings eng (limits/frames eng (:root c) (:slots c)) nil)))))

(defn check
  "A contract job waits while its checks say :later (can-start? until it starts, can-run? always),
  then while the body is below a floor of its call. A :never, a :look or a goal that holds passes: the round ends the
  job, or looks. A started job waiting on can-run? whose spend or deadline is crossed passes too, so its round ends it:
  waiting is bounded by the call's limits, :wall-time among them."
  [def c]
  (let [fresh? (not (started? c))
        raw (if fresh? (zipmap [:start :run] (statuses def c)) {:run (run-status def c)})
        look? (= :look (first (contract/all (vals raw))))
        _ (when-not look? (note-look! c nil))
        asked (cond-> raw (and look? (looked? c (look-key c (vals raw)))) (update-vals #(first (later-looks [%]))))
        {start-s :start run-s :run} asked
        s (if fresh? (contract/all [start-s run-s]) run-s)
        later? (= :later (first s))
        w (or (when later? (contract/status-cause s))
              (some-> (limits/floor-cause c #(or (ctx/frozen-call c) (first (call-of c def)))) limits/floor-wait))
        de (when (and w (not= :never (first s))) (done-or-error def c))]
    (remember-asked! c (cond-> asked de (assoc :done de)))
    (remember-done! c (first de))
    (cond
      (or (nil? w) (= :never (first s))) true
      (some-> (first de) ends?) true
      fresh? (ctx/wait c w)
      (and later? (spent? c)) true
      :else (ctx/wait c w))))

(defn passed-status
  "The status a check that just passed for c left: its :never or :look, which the check lets through for the round to
  act on, else :ok (also for a goal that holds and a spent job)."
  [c]
  (let [{:keys [start run done]} (second (get @(:asked (:engine c)) (asked-key c)))]
    (let [s (contract/all (keep identity [start run]))]
      (if (and (not (some-> done ends?)) (#{:never :look} (first s)))
        s
        (contract/ok :ready "ready")))))

(defn wrap
  "Job def with the engine's check and round when it is a contract job; any other def as it is."
  [def]
  (if (or (not (contract-job? def)) (::wrapped def))
    def
    (assoc def ::wrapped true :check #(check def %) :passed-status passed-status :round #(run def %))))

(defn registry "jobs with every contract job wrapped." [jobs] (some-> jobs (update-vals wrap)))
