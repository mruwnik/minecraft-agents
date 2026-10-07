(ns jobs.survival.log-out
  (:require [engine.ctx :as ctx]
            [jobs.lib.shelter :as sh]))

(def doc
  "Leave the server for a short stint, then come back: the night job's way to let another player's sleep skip the
  night. Declines unless it is night, :offline-allowed is true and the last log-out was not unsupported.
  Stays away :offline-ms, at most until morning. Back while it is still night, it waits :news-ms for the server's sleep
  count (sent on a join while anyone sleeps) before it ends.
  Ends with the result {:status :ms}. Status is ok, cut, unsupported or closed.
  Memory: writes :log-out {:ms :status} (cap 10, one in-game day).")

(def args
  {:offline-allowed {:doc "false forbids logging out" :default true}
   :offline-ms {:doc "how long to stay away at most; the stint ends at morning" :default 30000}
   :news-ms {:doc "back at night, how long to wait for the sleep count" :default 3000}})

(def log-out-policy {:cap 10 :ttl sh/ms-per-day})

(defn check [c]
  (let [p (:primitives c)]
    (cond
      (= false (:offline-allowed (:args c))) (ctx/wait c {:reason :offline-forbidden})
      (not (sh/night? p)) (ctx/wait c {:reason :not-night})
      (sh/log-out-unsupported? (ctx/view c)) (ctx/wait c {:reason :log-out-unsupported})
      :else true)))

(defn away-ms [c]
  (let [until-morning (sh/ms-until-morning (.-timeOfDay (.self (:primitives c))))]
    (min (:offline-ms (:args c)) (if (pos? until-morning) until-morning js/Infinity))))

(defn ^:async round [c]
  (let [r (await (ctx/act c :offline #js {:ms (away-ms c) :why "logged-out-for-sleeping-player"}))
        out {:ms (.-ms r) :status (.-status r)}]
    (when (and (= "ok" (.-status r)) (sh/night? (:primitives c)))
      (await (ctx/act c :wait #js {:ms (:news-ms (:args c))})))
    (ctx/remember! c :log-out out log-out-policy)
    (ctx/result! c out)
    :done))
