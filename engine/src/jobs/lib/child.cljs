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

(defn ^:async run!
  "Call job as the child in slot until it ends (:done or :declined), then resolve to that status. Between calls it
  awaits pace!. After :max-calls calls (opts) it resolves to :continue; the child keeps its memory, so the next run!
  with the same args resumes it. A slot last run with other args is cleared first: the parent changed its mind.
  A cut ends it at the next call (or at once, by ctx/alive?)."
  ([c slot job args] (run! c slot job args {}))
  ([c slot job args {:keys [max-calls] :or {max-calls (default-max-calls)}}]
   (when (not= args (get-in (ctx/mem c) [:child-args slot]))
     (ctx/update-mem! c #(-> % (update :children dissoc slot) (assoc-in [:child-args slot] args))))
   (loop [n 1]
     (let [r (await (ctx/call-child c slot job args))]
       (if (and (= :continue r) (< n max-calls) (ctx/alive? c))
         (do (await (pace!)) (recur (inc n)))
         r)))))
