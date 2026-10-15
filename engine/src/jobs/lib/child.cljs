(ns jobs.lib.child
  "Run a child job to its end inside the parent's round, for children that still answer :continue per step."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]))

(a/defargs settings
  {::default-max-calls {:default 400 :doc "Calls of one run!* before it gives the round back with :continue." :spec (a/int-in 1 nil)}})

(def pace-ms "The timer awaited between two calls, so a loop with no act never starves the event loop." 50)

(defn default-max-calls [] (settings/get settings ::default-max-calls))

(defn pace!
  "A promise that resolves after pace-ms (a timer, never a microtask)."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve pace-ms))))

(defn pause!
  "An intentional pause of seconds, holding still with why: a jobs.time.noop child in slot :pause, so reflexes still
  cut in and the time counts on the caller. Resolves to its ending."
  [c seconds why]
  (ctx/call c :pause 'jobs.time.noop {:seconds seconds :why why}))

(defn ^:async walk-reason
  "call-walk-reason with the child call fn (ctx/call-child, or ctx/call for the ending-returning versions): the reason
  is the :hold the go-to child keeps in its memory while it yields."
  [call c slot args]
  (let [r (await (call c slot 'jobs.movement.go-to args))
        reason (when (= :continue r) (get-in (ctx/mem c) [:children slot :hold]))]
    (when reason (ctx/hold-still! c reason))
    [r reason]))

(defn call-walk-reason
  "call-walk, resolving to [result reason]: reason is what go-to waits on (:night-sheltered) when it yielded :continue, else nil."
  [c slot args]
  (walk-reason ctx/call-child c slot args))

(defn call-walk-reason*
  "call-walk-reason through ctx/call: [r reason], r :continue or go-to's ending."
  [c slot args]
  (walk-reason ctx/call c slot args))

(defn ^:async call-walk*
  "call-walk through ctx/call: :continue while the walk goes on (holding still when go-to waits on the world), else
  go-to's ending."
  [c slot args]
  (first (await (call-walk-reason* c slot args))))

(defn ^:async run-with
  "run!* with the child call fn and walk-reason fn."
  [call walk c slot job args max-calls]
  (when (not= args (get-in (ctx/mem c) [:child-args slot]))
    (ctx/update-mem! c #(-> % (update :children dissoc slot) (assoc-in [:child-args slot] args))))
  (loop [n 1]
    (let [[r why] (if (= 'jobs.movement.go-to job)
                    (await (walk c slot args))
                    [(await (call c slot job args)) nil])]
      (if (and (= :continue r) (nil? why) (< n max-calls) (ctx/alive? c))
        (do (await (pace!)) (recur (inc n)))
        r))))

(defn run!*
  "run! through ctx/call: resolves to the child's ending, or :continue after :max-calls calls or while a go-to child
  waits on the world."
  ([c slot job args] (run!* c slot job args {}))
  ([c slot job args {:keys [max-calls] :or {max-calls (default-max-calls)}}]
   (run-with ctx/call call-walk-reason* c slot job args max-calls)))

(defn ^:async call-walk
  "Call a go-to child in slot with args (one round). While it yields :continue and its check would decline (go-to waiting
  for day in a shelter), the round holds still with that reason, so jobs show and body.idle name what the walk waits on."
  [c slot args]
  (first (await (call-walk-reason c slot args))))

(defn ^:async call-walk-leg
  "One leg of a walk: resolves to [:danger d] when (danger) answers d (the walk is not made, the caller yields so the
  reflex can act), else [r] of call-walk in slot with args and :spend {:job-time leg-s}. The leg ends {:arrived false :leg true} in the go-to
  result while the goal is still ahead; the caller weighs danger again by calling it again."
  [c slot args leg-s danger]
  (if-let [d (danger)]
    [:danger d]
    [(await (call-walk c slot (assoc-in args [:spend :job-time] leg-s)))]))

(def entry-depth "Blocks below both ends a walk to or from a line's entry may plan." 16)

(defn walk-into-args [slot cell]
  (cond-> {:pos cell :range 0 :ways #{:walk :clear-seal}}
    (#{:walk :back} slot) (assoc :at-night :go)
    (#{:in :out} slot) (assoc :y-margin entry-depth)))

(defn ^:async walk-into!
  "Walk the body into cell [x y z] with a go-to child in slot (no escalation: the dig is this job's own): :continue
  while it walks, else the child's result. The steps of the line (:walk, :back) go on at night: the cell is one this job
  cut, and the night shelter would hold the body between two steps for ever. The walks to and from the entry (:in, :out)
  plan no deeper than entry-depth below the lower of body and cell: a way through deep caves is no way, the job digs."
  [c slot cell]
  (if (= :done (await (call-walk c slot (walk-into-args slot cell))))
    (ctx/child-result c slot)
    :continue))

(defn ^:async walk-into!*
  "walk-into! through ctx/call: :continue while it walks, else go-to's ending (its :data has the old child-result)."
  [c slot cell]
  (await (call-walk* c slot (walk-into-args slot cell))))
