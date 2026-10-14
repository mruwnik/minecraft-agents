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

(def conditions
  [[:fn #(when (logged-out-meanwhile %) :logged-out-meanwhile)]
   [:fn #(when (= false (:offline-allowed (:args %))) :offline-forbidden)]
   [:fn #(when-not (sh/night? (:primitives %)) :not-night)]
   [:fn #(when (sh/log-out-unsupported? (ctx/view %)) :log-out-unsupported)]])

(def target [])

(defn away-ms [c]
  (let [until-morning (sh/ms-until-morning (.-timeOfDay (.self (:primitives c))))]
    (min (:offline-ms (:args c)) (if (pos? until-morning) until-morning js/Infinity))))

(defn ^:async quote-of [c]
  (let [s (/ (+ (away-ms c) (:news-ms (:args c))) 1000)]
    {:uses {:time [0 s s]}}))

(defn ^:async log-out! [c]
  (let [r (await (ctx/act c :offline #js {:ms (away-ms c) :why "logged-out-for-sleeping-player"}))
        how (.-status r)
        out {:ms (.-ms r) :how how}]
    (when (and (= "ok" how) (sh/night? (:primitives c)))
      (await (ctx/act c :wait #js {:ms (:news-ms (:args c))})))
    (ctx/remember! c :log-out {:ms (.-ms r) :status how} log-out-policy)
    (case how
      ("ok" "cut") (ctx/end c :done nil out)
      "unsupported" (ctx/end c :condition-failed :unsupported out)
      (ctx/end c :nothing-possible (keyword how) out))))

(defn ^:async round [c] (await (log-out! c)))
