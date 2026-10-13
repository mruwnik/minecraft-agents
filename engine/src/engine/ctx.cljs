(ns engine.ctx
  "Helpers a job's check and round call on their ctx. See README.md, section ctx."
  (:refer-clojure :exclude [use])
  (:require [engine.contract :as contract]
            [engine.expr :as expr]
            [engine.memory :as mem]))

;; ------------------------------------------------------------------ job memory

(defn without-tally
  "m, a job's memory map, without the engine's :contract tally, its children's included."
  [m]
  (cond-> (dissoc m :contract)
    (:children m) (update :children update-vals without-tally)))

(defn mem
  "This job's memory map (its sub-map, for a child), without the engine's :contract tally."
  [ctx]
  (without-tally (mem/job-mem ((:view ctx)) (:root ctx) (:slots ctx))))

(defn update-mem!
  "Apply (f current & args) to this job's memory map. Throws a cut error if
  this round was cut; a check cannot write."
  [ctx f & args]
  ((:update-mem ctx) f args))

;; ------------------------------------------------------------------ the contract (engine.contract)

(defn frozen-call
  "The call keys of this contract job's call, frozen at its first round; nil before it starts."
  [ctx]
  (get-in (mem/job-mem ((:view ctx)) (:root ctx) (:slots ctx)) [:contract :call]))

(defn way?
  "Whether the call allows way k."
  [ctx k]
  (contains? (:ways (frozen-call ctx)) k))

(defn use
  "The list the call gives for purpose (:use), or nil."
  [ctx purpose]
  (get-in (frozen-call ctx) [:use purpose]))

(defn end
  "The ending a contract job's round returns: kind (:done :condition-failed :limit-reached :nothing-possible
  :cut-off) and cause (a keyword or a map), data the job's own result fields. The engine adds :report and :because."
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
  (optional; see engine.memory)."
  ([ctx kind data] (remember! ctx kind data nil))
  ([ctx kind data policy] ((:remember ctx) kind data policy)))

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

(defn call-child
  "Run one round of job (a definition map, or a job namespace symbol) as the
  child in slot, with args. A promise of :done, :continue or :declined (its
  check failed)."
  [ctx slot job args]
  (let [[def args] (child-job ctx job args)]
    ((:call-child ctx) slot def args)))

(defn call
  "Run one round of job (a definition map, or a job namespace symbol; an old job through engine.core.legacy) as the
  child in slot, with args (its own and the call keys). A promise of :continue, or the child's ending with its :report; an ending other than
  :done becomes this round's :because unless the job says its own."
  [ctx slot job args]
  (let [[def args] (child-job ctx job args)]
    ((:call ctx) slot def args)))

(defn result!
  "Hand data to the parent as this job's result. Only the round that ends
  :done hands it over; the parent reads it with child-result."
  [ctx data]
  ((:result ctx) data))

(defn child-result
  "The data the child in slot handed over with result! in the round it
  finished, read during that same round of the parent; nil otherwise."
  [ctx slot]
  ((:child-result ctx) slot))

(defn wait
  "For a check that declines: returns false and notes why the job waits.
  reason is a keyword or a map with :reason. Keep its fields stable while the wait lasts:
  the scheduler tells a reason once, and again only when it changes.
  Outside a check the scheduler runs (a round, a test) it only returns false."
  [ctx reason]
  (some-> (:wait ctx) (reset! reason))
  false)

(defn check-child
  "Run job's check as the child in slot would see it, for a parent's check. A reason the child's check gives with
  wait is the parent's too, when the parent declines."
  [ctx slot job args]
  (let [[def args] (child-job ctx job args)
        slots (conj (:slots ctx) slot)]
    ((:check def) (assoc ctx :slots slots :args args))))

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
  the gap. In a check it only calls f."
  [ctx level section f]
  (when-not (section-levels level)
    (throw (ex-info (str "ctx/atomic level is :comfort or :urgent, not " (pr-str level)) {:level level})))
  (let [opened? (if-let [open (:open-section ctx)] (and (:token ctx) (open section level)) false)]
    (try
      (await (f))
      (finally
        (when opened? ((:close-section ctx) section))))))

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
