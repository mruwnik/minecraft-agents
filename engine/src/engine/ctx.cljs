(ns engine.ctx
  "Helpers a job's check and round call on their ctx. See README.md, section ctx."
  (:refer-clojure :exclude [quote use])
  (:require [engine.contract :as contract]
            [engine.expr :as expr]
            [engine.memory :as mem]))

;; ------------------------------------------------------------------ job memory

(defn without-tally
  "m, a job's memory map, without the engine's :contract tally, :queued-t and :job (children included)."
  [m]
  (cond-> (dissoc m :contract :queued-t :job)
    (:children m) (update :children update-vals without-tally)))

(defn mem
  "This job's memory map (its sub-map, for a child), without the engine's :contract tally, :queued-t and :job."
  [ctx]
  (without-tally (mem/job-mem ((:view ctx)) (:root ctx) (:slots ctx))))

(defn update-mem!
  "Apply (f current & args) to this job's memory map. Throws a cut error if
  this round was cut; a check cannot write."
  [ctx f & args]
  ((:update-mem ctx) f args))

;; ------------------------------------------------------------------ the contract (engine.contract)

(defn frozen-call
  "The call keys of this contract job's call, frozen at its first round (in a quote ctx, the would-be call's); nil
  before it starts."
  [ctx]
  (or (:quote-call ctx) (get-in (mem/job-mem ((:view ctx)) (:root ctx) (:slots ctx)) [:contract :call])))

(defn left
  "What is left of the call's limits and its callers', each line the least over the chain: {:spend {line remainder}
  :floor {line level} :deadline ms}."
  [ctx]
  ((:left ctx)))

(defn book-try!
  "Book a try of this job's own (job sym, why keyword) on every frame above its own: their :tries spend line, for a job
  that retries by itself (its own frame holds only its children's endings, so its own :tries line is the job's to read). Throws a cut error if this round was cut; a check cannot book."
  [ctx job reason]
  ((:book-try ctx) job reason))

(defn tries-left
  "How many more tries the :tries spend lines of the call's callers and its children's endings allow (least over the chain), ##Inf when none is set."
  [ctx]
  (get-in (left ctx) [:spend :tries] ##Inf))

(defn own-tries
  "The :tries spend line this call itself sets, ##Inf when none."
  [ctx]
  (get-in (frozen-call ctx) [:spend :tries] ##Inf))

(defn way?
  "Whether the call allows way k."
  [ctx k]
  (contains? (:ways (frozen-call ctx)) k))

(defn use
  "The list the call gives for purpose (:use), or nil."
  [ctx purpose]
  (get-in (frozen-call ctx) [:use purpose]))

(defn end
  "The ending a contract job's round returns: kind (:done :condition-failed :impossible :limit-reached
  :nothing-possible :cut-off) and cause (a keyword or a map), data the job's own result fields. The engine adds :report and :because."
  ([ctx kind cause] (end ctx kind cause nil))
  ([_ctx kind cause data] (contract/ending kind cause data)))

;; ------------------------------------------------------------------ body memory

(defn view
  "A memory view {:data :now} for engine.memory reads."
  [ctx]
  ((:view ctx)))

(defn now [ctx] (:now (view ctx)))

(defn entries [ctx kind] (mem/entries (view ctx) kind))
(defn latest [ctx kind] (mem/latest (view ctx) kind))
(defn since [ctx kind t] (mem/since (view ctx) kind t))
(defn count-in [ctx kind ms] (mem/count-in (view ctx) kind ms))

(defn remember!
  "Append an entry with data to kind, under policy {:cap n :ttl ms-or-:forever}
  (optional; see engine.memory). A place or reservation write earns its info notice (engine.core.notices).
  remember-asked! is for a write the agent asked for by name: its place.set has no :auto, and any other kind
  earns memory.remembered."
  ([ctx kind data] (remember! ctx kind data nil))
  ([ctx kind data policy] ((:remember ctx) kind data policy)))

(defn remember-asked!
  "remember! for a write the agent asked for by name."
  [ctx kind data policy]
  ((:remember-asked ctx) kind data policy))

(defn forget-where!
  "Drop the entries of kind whose data matches pred."
  [ctx kind pred]
  ((:forget ctx) kind pred))

(defn forget-until!
  "Drop the entries of kind written at or before t."
  [ctx kind t]
  ((:forget-until ctx) kind t))

;; ------------------------------------------------------------------ composition

(defn child-job
  "[def args] for a child: job is a definition map, or a job namespace
  symbol looked up in the registry with args merged over its defaults."
  [ctx job args]
  (if (symbol? job)
    (update (expr/leaf (:jobs (:engine ctx)) job args) 0 assoc :sym job)
    [job args]))

(defn call
  "Run one round of job (a definition map, or a job namespace symbol) as the
  child in slot, with args (its own and the call keys). A promise of :continue, or the child's ending with its
  :report; an ending other than :done becomes this round's :because unless the job says its own."
  [ctx slot job args]
  (let [[def args] (child-job ctx job args)]
    ((:call ctx) slot def args)))

(defn quote
  "A promise of what job (a definition map, or a job namespace symbol) expects to use and yield for a call with args:
  {:uses {:job-time [least mean most] :wall-time fig (seconds paused beyond :job-time; the :wall-time spend line counts both) :health .. :food .. :items {name fig} :wear {tool fig}} :yields {:items {name fig}}
  :done fig :moves-to [x y z]} (:done in the job's done? units), ##Inf allowed; or the condition-failed :duplicate-call
  ending when the call would be refused. Runs its quote-of export under a quote ctx (no acts, calls or writes), else
  time (0, the mean of its :job-report times or ::contract/default-time-s, ##Inf). quote-deadline: wall-clock ms,
  never past this ctx's :quote-deadline (default ::contract/quote-s from now); asked or answered past it, time is ##Inf."
  ([ctx job args] (engine.ctx/quote ctx job args nil))
  ([ctx job args quote-deadline]
   (let [[def args] (child-job ctx job args)]
     ((:quote ctx) def args quote-deadline false))))

(defn next-way
  "A promise of {:way k :dropped {way why}}: the first of this contract job's ways, in its ways order, that the call
  allows, has not failed on thing this call and whose quote crosses no line of left by its least; :way nil when none.
  (quote-of-way way) is a quote, a promise of one, an ending (dropped with its cause) or nil (passes). A child ending
  other than :done after next-way chose a way books that way as failed on thing. Quotes cost: ask when choosing."
  ([ctx quote-of-way] (next-way ctx quote-of-way nil))
  ([ctx quote-of-way thing] ((:next-way ctx) ctx quote-of-way thing)))

(defn wait
  "For a check that declines: returns false and notes why the job waits.
  reason is a keyword or a map with :reason. Keep its fields stable while the wait lasts:
  the scheduler tells a reason once, and again only when it changes.
  Outside a check the scheduler runs (a round, a test) it only returns false."
  [ctx reason]
  (some-> (:wait ctx) (reset! reason))
  false)

(defn check-status
  "The status def would start with now under ctx: :later with its check's wait reason, else contract/verdict of its
  checks (a registry job's check passes a :never or :look, so they are read from the checks), which a parent passes on."
  [ctx def]
  (if-let [check (:check def)]
    (let [a (atom nil)]
      (if (check (assoc ctx :wait a))
        (if-let [f (:passed-status def)] (f ctx) (contract/ok :ready "ready"))
        (let [w @a
              w (cond (map? w) w (some? w) {:reason w} :else {:reason :not-ready})
              reason (if (keyword? (:reason w)) (:reason w) :not-ready)]
          [:later (or (:text w) (name reason)) (assoc w :reason reason)])))
    (contract/verdict ctx def)))

(defn child-status
  "check-status of the child job (a registry symbol, or a definition) in slot with args, for a parent that asks before
  it calls."
  [ctx slot job args]
  (let [[def args] (child-job ctx job args)]
    (check-status (assoc ctx :slots (conj (:slots ctx) slot) :args args) def)))

(defn returns-on-later?
  "Whether a :later check ends this job condition-failed for its caller to decide: a reflex (one round) and a called child.
  Only a listed root job waits."
  [ctx]
  (boolean (or (:reflex ctx) (seq (:slots ctx)))))

(defn can-run
  "This job's can-run? status now, asked between steps (jobs.lib.pace/steps!); :ok when it has none."
  [ctx]
  (if-let [f (:can-run ctx)] (f) (contract/ok :holds "holds")))

(defn done
  "The :done ending this job's done? gives now, asked between steps (jobs.lib.pace/steps!), else nil."
  [ctx]
  (when-let [f (:done ctx)] (f)))

(defn submit!
  "Put a job spec (an expression, see engine.expr) at the end of the list;
  returns its instance id."
  ([ctx spec] (submit! ctx spec {}))
  ([ctx spec opts] ((:submit ctx) spec opts)))

(defn request-attention!
  "Open or update a durable request for this top-level job. `reason` is the
  stable deduplication key; `kind`, `data`, and `message` describe the current
  request. Repeated calls with the same job/reason reuse its request ID."
  [ctx kind reason data message]
  ((:request-attention ctx) kind reason data message))

(defn resolve-attention!
  "Resolve a request ID owned by this job, with a structured reason keyword."
  [ctx request-id reason]
  ((:resolve-attention ctx) request-id reason))

(defn alive?
  "Whether this round still owns the body (a check, or a ctx with no engine behind it, always does). A cut round's
  code runs on until its next act, write or emit: a search loop with no act checks this and stops."
  [ctx]
  (if-let [f (:alive? ctx)] (f) true))

(defn emit!
  "Emit an event with :source :job and this job's envelope fields. A cut round's events are dropped."
  ([ctx kind level] (emit! ctx kind level {}))
  ([ctx kind level fields] ((:emit ctx) kind level fields)))

(defn note-walk!
  "Book one walk round for the backoff (engine.backoff). status is its :moved status (\"arrived\", \"partial\",
  \"blocked\"), moved the blocks the body moved. The steer acts of the walk are neutral, so this is what counts.
  A ctx with no engine behind it books nothing."
  [ctx status moved]
  (when-let [f (:note-walk ctx)] (f status moved)))

(defn hold-still!
  "Declare that this round holds the body still on purpose, for reason (a keyword), until the round ends; nil
  clears it. Emits job.holding; agents see {:reason :since}; no job.idle warn while it lasts. A check cannot hold."
  [ctx reason]
  ((:hold-still ctx) reason))

(def section-levels #{:comfort :urgent})

(defn ^:async atomic
  "Run (f) as an atomic section named section (a keyword) at level :comfort or :urgent; a promise of f's result. While
  it runs the scheduler holds off a reflex at or below level, each up to its level's wait cap (settings
  :engine.core.schedule/urgent-wait-s, comfort-wait-s); a danger reflex and the agent's do-now cut at once. Sections
  nest (the highest level holds). The outermost section is not opened while a held-off reflex waits, so it fires in
  the gap. A limit crossed inside is thrown once the outermost section closes (engine.core.limits). In a check it only
  calls f."
  [ctx level section f]
  (when-not (section-levels level)
    (throw (ex-info (str "ctx/atomic level is :comfort or :urgent, not " (pr-str level)) {:level level})))
  (let [opened? (if-let [open (:open-section ctx)] (and (:token ctx) (open section level)) false)
        depth (:section-depth ctx)]
    (some-> depth (swap! inc))
    (let [r (try
              (await (f))
              (finally
                (some-> depth (swap! dec))
                (when opened? ((:close-section ctx) section))))]
      (some-> (:section-closed ctx) (apply []))
      r)))

(defn act
  "Call acting primitive k (a keyword such as :moveTo) with this round's
  token, through the engine's act wrapper: token check, memory saved before
  and after, debug events. A promise of the primitive's result."
  [ctx k args]
  ((:act ctx) k args))

;; ------------------------------------------------------------------ the body

(defn warn-once!
  "Emit warn event kind with fields the first time this job gives key in this body process; a check
  may call it (it writes no memory)."
  [ctx key kind fields]
  (let [k [(:id ctx) key]
        [old _] (swap-vals! (:warned (:engine ctx)) conj k)]
    (when-not (contains? old k)
      (emit! ctx kind :warn fields))))

(defn self-name
  "The body's own username, the owner name zones and claims are compared with."
  [ctx]
  (.-username (.self (:primitives ctx))))
