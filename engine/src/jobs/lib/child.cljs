(ns jobs.lib.child
  "Run a child job to its end inside the parent's round, for children that still answer :continue per step."
  (:refer-clojure :exclude [run!])
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]))

(a/defargs settings
  {::default-max-calls {:default 400 :doc "Calls of one run! before it gives the round back with :continue." :spec (a/int-in 1 nil)}})

(def pace-ms "The timer awaited between two calls, so a loop with no act never starves the event loop." 50)

(defn default-max-calls [] (settings/get settings ::default-max-calls))

(defn pace!
  "A promise that resolves after pace-ms (a timer, never a microtask)."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve pace-ms))))

(defn ^:async call-walk-reason
  "call-walk, resolving to [result reason]: reason is what go-to waits on (:night-sheltered) when it yielded :continue, else nil."
  [c slot args]
  (let [r (await (ctx/call-child c slot 'jobs.movement.go-to args))
        w (atom nil)]
    (when (= :continue r)
      (ctx/check-child (assoc c :wait w :report-wait true) slot 'jobs.movement.go-to args)
      (when-let [reason (some-> @w :reason)] (ctx/hold-still! c reason)))
    [r (some-> @w :reason)]))

(defn ^:async run!
  "Call job as the child in slot until it ends (:done or :declined), then resolve to that status. Between calls it
  awaits pace!. After :max-calls calls (opts) it resolves to :continue; the child keeps its memory, so the next run!
  with the same args resumes it. A slot last run with other args is cleared first: the parent changed its mind.
  A cut ends it at the next call (or at once, by ctx/alive?). A go-to child waiting for day in a shelter ends it
  at once with :continue and call-walk's hold."
  ([c slot job args] (run! c slot job args {}))
  ([c slot job args {:keys [max-calls] :or {max-calls (default-max-calls)}}]
   (when (not= args (get-in (ctx/mem c) [:child-args slot]))
     (ctx/update-mem! c #(-> % (update :children dissoc slot) (assoc-in [:child-args slot] args))))
   (loop [n 1]
     (let [[r why] (if (= 'jobs.movement.go-to job)
                     (await (call-walk-reason c slot args))
                     [(await (ctx/call-child c slot job args)) nil])]
       (if (and (= :continue r) (nil? why) (< n max-calls) (ctx/alive? c))
         (do (await (pace!)) (recur (inc n)))
         r)))))

(defn ^:async call-walk
  "Call a go-to child in slot with args (one round). While it yields :continue and its check would decline (go-to waiting
  for day in a shelter), the round holds still with that reason, so jobs show and body.idle name what the walk waits on."
  [c slot args]
  (first (await (call-walk-reason c slot args))))

(defn ^:async call-walk-leg
  "One leg of a walk: resolves to [:danger d] when (danger) answers d (the walk is not made, the caller yields so the
  reflex can act), else [r] of call-walk in slot with args and :leg-s leg-s. The leg ends {:arrived false :leg true} in the go-to
  result while the goal is still ahead; the caller weighs danger again by calling it again."
  [c slot args leg-s danger]
  (if-let [d (danger)]
    [:danger d]
    [(await (call-walk c slot (assoc args :leg-s leg-s)))]))
