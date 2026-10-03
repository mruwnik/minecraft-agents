(ns engine.ctx
  "Helpers a job's check and round call on their ctx. See README.md, section ctx."
  (:require [engine.expr :as expr]
            [engine.memory :as mem]))

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

(defn check-child
  "Run job's check as the child in slot would see it, for a parent's check."
  [ctx slot job args]
  (let [[def args] (child-job ctx job args)
        slots (conj (:slots ctx) slot)]
    ((:check def) (assoc ctx :slots slots :args args))))

(defn submit!
  "Put a job spec (an expression, see engine.expr) at the end of the list;
  returns its instance id."
  ([ctx spec] (submit! ctx spec {}))
  ([ctx spec opts] ((:submit ctx) spec opts)))

(defn emit!
  "Emit an event with :source :job and this job's envelope fields."
  ([ctx kind level] (emit! ctx kind level {}))
  ([ctx kind level fields] ((:emit ctx) kind level fields)))

(defn act
  "Call acting primitive k (a keyword such as :moveTo) with this round's
  token, through the engine's act wrapper: token check, memory saved before
  and after, debug events. A promise of the primitive's result."
  [ctx k args]
  ((:act ctx) k args))
