(ns engine.ctx
  "Helpers a job's check and round call on their ctx. See README.md, section ctx."
  (:require [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.world :as world]))

;; ------------------------------------------------------------------ job memory

(defn mem
  "This job's memory map (its sub-map, for a child)."
  [ctx]
  (mem/job-mem ((:view ctx)) (:root ctx) (:slots ctx)))

(defn update-mem!
  "Apply (f current & args) to this job's memory map. Throws a cut error if
  this round was cut; a check cannot write."
  [ctx f & args]
  ((:update-mem ctx) f args))

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
    (expr/leaf (:jobs (:engine ctx)) job args)
    [job args]))

(defn call-child
  "Run one round of job (a definition map, or a job namespace symbol) as the
  child in slot, with args. A promise of :done, :continue or :declined (its
  check failed)."
  [ctx slot job args]
  (let [[def args] (child-job ctx job args)]
    ((:call-child ctx) slot def args)))

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
  "For a check that declines: false, noting why the job waits. reason is a keyword or a map with :reason (keep its
  fields stable while the wait lasts: the scheduler tells a reason once, and again only when it changes). Outside a
  check the scheduler runs (a round, a test) it only returns false."
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

(defn emit!
  "Emit an event with :source :job and this job's envelope fields."
  ([ctx kind level] (emit! ctx kind level {}))
  ([ctx kind level fields] ((:emit ctx) kind level fields)))

(defn note-walk!
  "Book one walk round for the backoff (engine.backoff): status is its :moved status (\"arrived\", \"partial\",
  \"blocked\"), moved the blocks the body moved. The steer acts of the walk are neutral; this is what counts. A ctx with
  no engine behind it books nothing."
  [ctx status moved]
  (when-let [f (:note-walk ctx)] (f status moved)))

(defn act
  "Call acting primitive k (a keyword such as :moveTo) with this round's
  token, through the engine's act wrapper: token check, memory saved before
  and after, debug events. A promise of the primitive's result."
  [ctx k args]
  ((:act ctx) k args))

;; ------------------------------------------------------------------ world knowledge

(defn plan
  "The body's world's plan id, from memory (engine.world/answer): nil when there is no such plan,
  {:id :broken text} when it was never readable, else {:id :plan :cells :errors}, with :error
  when the file is bad now and this is its last good copy."
  [ctx id]
  (world/plan (:world (:engine ctx)) id))

(defn warn-once!
  "Emit warn event kind with fields the first time this job gives key in this body process; a check
  may call it (it writes no memory)."
  [ctx key kind fields]
  (when (world/first-time! (:world (:engine ctx)) [(:id ctx) key])
    (emit! ctx kind :warn fields)))

(defn zones
  "The body's world's zone list [{:name :min [x y z] :max [x y z] :owner :allow #{..}} ...] (see engine.zones), or
  nil when the zone file is missing or was never readable. A dig or place job declines on nil (one warn); nil is
  never \"no zones\" ([] is)."
  [ctx]
  (world/zones (:world (:engine ctx))))

(defn claims
  "The active, unexpired area claims of the body's world (claims.edn) at the clock: [{:id :owner :min :max :until ..}].
  Zones and claims are a rule jobs consult (engine.access.zones); the engine enforces neither."
  [ctx]
  (world/live-claims (world/area-claims (:world (:engine ctx))) (now ctx)))

(defn self-name
  "The body's own username, the owner name zones and claims are compared with."
  [ctx]
  (.-username (.self (:primitives ctx))))

(defn plan-authors
  "{plan-id by}: the body each plan names as its maker (:by); a plan without :by is not listed."
  [ctx]
  (world/plan-authors (:world (:engine ctx))))

(defn footprints
  "{[x y z] plan-id}: the cells of every plan, as engine.access.rules takes :footprints (a refusal then names
  the :plan). A job working plan P passes {:except P} to leave P's own cells out."
  ([ctx] (footprints ctx {}))
  ([ctx {:keys [except]}]
   (world/footprints (:world (:engine ctx)) except)))
