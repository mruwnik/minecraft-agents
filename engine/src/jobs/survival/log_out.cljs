(ns jobs.survival.log-out
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.shelter :as sh]))

(def doc
  "Leave the server for a short stint, then come back: the night job's way to let another player's sleep skip the
  night. Waits for night (:not-night); impossible with :offline-allowed false (:offline-forbidden) or once a log-out
  was unsupported (:log-out-unsupported).
  Stays away :offline-ms, at most until morning. Back while it is still night, it waits :news-ms for the server's sleep
  count (sent on a join while anyone sleeps) before it ends.
  A listed job that finds another job logged the body out since it was queued is done at once without acting.
  Ends with the result {:ms :how}. How is ok, cut, unsupported or closed.
  Memory: writes :log-out {:ms :status} (cap 10, one in-game day).")

(a/defargs args
  {:offline-allowed {:doc "false forbids logging out" :spec boolean? :default true}
   :offline-ms {:doc "how long to stay away at most; the stint ends at morning" :spec (a/num-in 0 nil) :default 30000}
   :news-ms {:doc "back at night, how long to wait for the sleep count" :spec (a/num-in 0 nil) :default 3000}})

(def log-out-policy {:cap 10 :ttl sh/ms-per-day})

(defn listed? [c] (and (empty? (:slots c)) (nil? (:reflex c))))

(defn logged-out-meanwhile
  "The latest ok or cut :log-out entry written after this listed job was queued, or nil; a child or reflex job never
  asks."
  [c]
  (when (listed? c)
    (let [v (ctx/view c)
          queued (:queued-t (mem/job-mem v (:root c) []))
          out (mem/latest v :log-out)]
      (when (and queued out (> (:t out) queued) (#{"ok" "cut"} (:status (:data out))))
        out))))

(defn allowed [c]
  (if (= false (:offline-allowed (:args c)))
    (contract/never :offline-forbidden ":offline-allowed false forbids logging out")
    (contract/ok :offline-allowed "logging out allowed")))

(defn supported [c]
  (if (sh/log-out-unsupported? (ctx/view c))
    (contract/never :log-out-unsupported "a log-out was unsupported")
    (contract/ok :log-out-supported "log-out not known unsupported")))

(def can-start? (cond/and allowed (checks/night :not-night) supported))

(a/defargs settings
  {::job-time-s {:default 900 :doc "Seconds a log-out stint and the wait for the sleep count may take (a stint lasts at most until morning)." :spec (a/num-in 0 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)

(def target [])

(defn done? [c]
  (let [out (logged-out-meanwhile c)]
    (checks/goal (some? out) [:logged-out "logged out meanwhile"] [:online "not logged out meanwhile"] {:result (when out {:ms (:ms (:data out)) :how (:status (:data out))})})))

(defn away-ms [c]
  (let [until-morning (sh/ms-until-morning (.-timeOfDay (.self (:primitives c))))]
    (min (:offline-ms (:args c)) (if (pos? until-morning) until-morning js/Infinity))))

(defn ^:async quote-of [c]
  (let [s (/ (+ (away-ms c) (:news-ms (:args c))) 1000)]
    {:uses {:job-time [0 s s]}}))

(defn ^:async log-out! [c]
  (let [r (await (ctx/act c :offline #js {:ms (away-ms c) :why "logged-out-for-sleeping-player"}))
        how (.-status r)
        out {:ms (.-ms r) :how how}]
    (when (and (= "ok" how) (sh/night? (:primitives c)))
      (await (child/pause! c (/ (:news-ms (:args c)) 1000) :sleep-news)))
    (ctx/remember! c :log-out {:ms (.-ms r) :status how} log-out-policy)
    (case how
      ("ok" "cut") (ctx/end c :done nil out)
      "unsupported" (ctx/end c :impossible :unsupported out)
      (ctx/end c :nothing-possible (keyword how) out))))

(defn ^:async round [c] (await (log-out! c)))
