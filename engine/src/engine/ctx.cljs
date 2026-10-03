(ns engine.ctx
  "Helpers a job's check and round call on their ctx. See README.md, section ctx."
  (:require [engine.memory :as mem]))

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

(defn call-child
  "Run one round of job definition def as the child in slot, with args.
  A promise of :done, :continue or :declined (its check failed)."
  [ctx slot def args]
  ((:call-child ctx) slot def args))

(defn check-child
  "Run def's check as the child in slot would see it, for a parent's check."
  [ctx slot def args]
  (let [slots (conj (:slots ctx) slot)]
    ((:check def) (assoc ctx :slots slots :args args))))

(defn submit!
  "Put a new job at the end of the list; returns its instance id."
  ([ctx job args] (submit! ctx job args {}))
  ([ctx job args opts] ((:submit ctx) job args opts)))

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
