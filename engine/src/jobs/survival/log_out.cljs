(ns jobs.survival.log-out
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.shelter :as sh]))

(def doc
  "Leave the server for a short stint, then come back: the night job's way to let another player's sleep skip the
  night. Declines unless it is night, :offline-allowed is true and the last log-out was not unsupported.
  Stays away :offline-ms, at most until morning. Back while it is still night, it waits :news-ms for the server's sleep
  count (sent on a join while anyone sleeps) before it ends.
  A listed job that finds another job logged the body out since it was queued ends at once without acting.
  Ends with the result {:status :ms}. Status is ok, cut, unsupported, closed or already (another job's log-out).
  Memory: writes :log-out {:ms :status} (cap 10, one in-game day).")

(a/defargs args
  {:offline-allowed {:doc "false forbids logging out" :spec boolean? :default true}
   :offline-ms {:doc "how long to stay away at most; the stint ends at morning" :spec (a/num-in 0 nil) :default 30000}
   :news-ms {:doc "back at night, how long to wait for the sleep count" :spec (a/num-in 0 nil) :default 3000}})

(def log-out-policy {:cap 10 :ttl sh/ms-per-day})

(defn logged-out-meanwhile
  "The latest ok or cut :log-out entry written after this listed job was queued (its memory entry is as old as the
  job until it first runs), or nil; a child or reflex job never asks."
  [c]
  (when (and (empty? (:slots c)) (nil? (:reflex c)))
    (let [v (ctx/view c)
          queued (:t (mem/latest v (mem/job-kind (:root c))))
          out (mem/latest v :log-out)]
      (when (and queued out (> (:t out) queued) (#{"ok" "cut"} (:status (:data out))))
        out))))

(defn check [c]
  (let [p (:primitives c)]
    (cond
      (logged-out-meanwhile c) true
      (= false (:offline-allowed (:args c))) (ctx/wait c {:reason :offline-forbidden})
      (not (sh/night? p)) (ctx/wait c {:reason :not-night})
      (sh/log-out-unsupported? (ctx/view c)) (ctx/wait c {:reason :log-out-unsupported})
      :else true)))

(defn away-ms [c]
  (let [until-morning (sh/ms-until-morning (.-timeOfDay (.self (:primitives c))))]
    (min (:offline-ms (:args c)) (if (pos? until-morning) until-morning js/Infinity))))

(defn ^:async log-out! [c]
  (let [r (await (ctx/act c :offline #js {:ms (away-ms c) :why "logged-out-for-sleeping-player"}))
        out {:ms (.-ms r) :status (.-status r)}]
    (when (and (= "ok" (.-status r)) (sh/night? (:primitives c)))
      (await (ctx/act c :wait #js {:ms (:news-ms (:args c))})))
    (ctx/remember! c :log-out out log-out-policy)
    (ctx/result! c out)
    :done))

(defn ^:async round [c]
  (if-let [out (logged-out-meanwhile c)]
    (do (ctx/result! c {:ms (:ms (:data out)) :status "already"})
        :done)
    (await (log-out! c))))
