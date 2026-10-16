(ns jobs.survival.dig-in.site
  "What the dig-in modes share: the stop reason of a call, and the :dig-in-futile entries a failed site leaves."
  (:require [engine.ctx :as ctx]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(def futile-policy {:cap 5 :ttl 600000})

(defn stop-reason!
  "Note in job memory why this call ends without a roof: the call's ending."
  [c reason]
  (ctx/update-mem! c assoc :stop reason))

(defn remember-failed-site! [c reason]
  (stop-reason! c reason)
  (ctx/remember! c :dig-in-futile
                 {:pos (or (:roof (ctx/mem c)) (sh/feet (:primitives c))) :reason reason}
                 futile-policy))

(defn remember-material!
  "A lack of blocks (data {:pos}) or of a tool (data {:pos :needs}): a :dig-in-futile entry retried once that changes."
  [c data]
  (stop-reason! c (if (:needs data) :no-tool :no-blocks))
  (ctx/remember! c :dig-in-futile data futile-policy))

(defn fail-site! [c reason text]
  (let [result (u/fail! c :dig_in_failed text)]
    (when (= :done result) (remember-failed-site! c reason))
    result))

(defn open-text [cells]
  (str "open cells " (pr-str (mapv (juxt :x :y :z) cells))))
