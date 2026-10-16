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

(defn ^:async call-walk*
  "A go-to child in slot with args through ctx/call: :continue while the walk goes on (or waits: go-to's safe? says
  later, which makes this round wait with its reason), else go-to's ending."
  [c slot args]
  (await (ctx/call c slot 'jobs.movement.go-to args)))

(defn ^:async run-with
  "run!* with the child call fn. A child that waits on a check (child-wait) gives the round back at once."
  [call c slot job args max-calls]
  (when (not= args (get-in (ctx/mem c) [:child-args slot]))
    (ctx/update-mem! c #(-> % (update :children dissoc slot) (assoc-in [:child-args slot] args))))
  (loop [n 1]
    (let [r (await (call c slot job args))]
      (if (and (= :continue r) (nil? (some-> (:child-wait c) deref)) (< n max-calls) (ctx/alive? c))
        (do (await (pace!)) (recur (inc n)))
        r))))

(defn run!*
  "run! through ctx/call: resolves to the child's ending, or :continue after :max-calls calls."
  ([c slot job args] (run!* c slot job args {}))
  ([c slot job args {:keys [max-calls] :or {max-calls (default-max-calls)}}]
   (run-with ctx/call c slot job args max-calls)))

(defn ^:async call-walk
  "A go-to child in slot with args (one round, ctx/call-child): :continue while it walks or waits."
  [c slot args]
  (await (ctx/call-child c slot 'jobs.movement.go-to args)))

(def entry-depth "Blocks below both ends a walk to or from a line's entry may plan." 16)

(defn walk-into-args [slot cell]
  (cond-> {:pos cell :range 0 :ways #{:walk :clear-seal}}
    (#{:in :out} slot) (assoc :y-margin entry-depth)))

(defn ^:async walk-into!
  "Walk the body into cell [x y z] with a go-to child in slot (no escalation: the dig is this job's own): :continue
  while it walks, else the child's result. The walk's checks are the caller's (an escape's defaults flow down). The walks to and from the entry (:in, :out)
  plan no deeper than entry-depth below the lower of body and cell: a way through deep caves is no way, the job digs."
  [c slot cell]
  (if (= :done (await (call-walk c slot (walk-into-args slot cell))))
    (ctx/child-result c slot)
    :continue))

(defn ^:async walk-into!*
  "walk-into! through ctx/call: :continue while it walks, else go-to's ending (its :data has the old child-result)."
  [c slot cell]
  (await (call-walk* c slot (walk-into-args slot cell))))
