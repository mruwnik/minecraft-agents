(ns engine.ctx
  "Helpers a job round calls on its ctx. See README.md, section ctx.")

(defn mem
  "Read a memory scope now: the job's own by default, or :body / :common."
  ([ctx] ((get-in ctx [:memory :get])))
  ([ctx scope] ((get-in ctx [:memory :get]) scope)))

(defn commit!
  "Replace a scope (the job's own by default) with m, or with (f current).
  Written to disk at once. Throws a cut error if this round was cut."
  ([ctx m-or-f] ((get-in ctx [:memory :commit]) m-or-f))
  ([ctx scope m-or-f] ((get-in ctx [:memory :commit]) scope m-or-f)))

(defn step-child
  "Run one round of the child in slot; a promise of :done, :continue or :not-ready."
  [ctx slot job args]
  ((:step-child ctx) slot job args))

(defn submit!
  "Put a new job at the end of the list; returns its instance id."
  ([ctx job args] (submit! ctx job args {}))
  ([ctx job args opts] ((:submit ctx) job args opts)))

(defn emit!
  "Emit an event with :source :job and this job's envelope fields."
  ([ctx kind level] (emit! ctx kind level {}))
  ([ctx kind level fields] ((:emit ctx) kind level fields)))

(defn act
  "Call primitive method k (a keyword such as :moveTo) with this round's token."
  [ctx k args]
  (let [p (:primitives ctx)
        f (aget p (name k))]
    (.call f p (:token ctx) args)))
