(ns jobs.lib.check-cache
  "A short-lived cache for what a job's check reads off the world, so a check run every tick reads it once per
  window. A check cannot write job memory, so the values live here, per job instance, and are lost on restart."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]))

(a/defargs settings
  {::ttl-ms {:default 2000 :doc "How long a check reuses what it read off the world, in ms of body clock." :spec (a/int-in 0 nil)}
   ::keep {:default 4 :doc "How many sets of inputs a keyed check keeps a value for, per job and tag (the latest)." :spec (a/int-in 1 nil)}})
(defn ttl-ms [] (settings/get settings ::ttl-ms))

(defonce engines (js/WeakMap.))

(defn entries-of
  "The cache atom of the engine c runs in (job ids repeat from one engine to the next)."
  [c]
  (let [key (:warned (:engine c))]
    (or (.get engines key)
        (let [e (atom {})] (.set engines key e) e))))

(defn fresh
  "c whose cached reads read the world anew (and renew the entries): for a check asked again because the world changed."
  [c]
  (assoc c ::fresh true))

(defn reuse
  "(thunk) for key k of this job, its kept value while (fresh? now entry) and not under fresh."
  [c k thunk fresh?]
  (let [entries (entries-of c)
        id [(:id c) (:slots c) k]
        now (ctx/now c)
        hit (get @entries id)]
    (if (and hit (not (::fresh c)) (fresh? now hit))
      (:v hit)
      (let [v (thunk)]
        (swap! entries (fn [m] (assoc (into {} (remove (fn [[_ e]] (and (not (:recent e)) (> (- now (:t e)) (* 10 (ttl-ms)))))) m) id {:t now :v v})))
        v))))

(defn cached
  "The value of (thunk) for key k of this job, reused while it is younger than ttl-ms (never under fresh)."
  [c k thunk]
  (reuse c k thunk (fn [now hit] (<= 0 (- now (:t hit)) (ttl-ms)))))

(defn keyed
  "The value of (thunk) for the inputs of tag (the values the thunk reads, as gathered) of this job, whatever its age
  (never under fresh): the latest ::keep inputs of a tag are kept, with the settings they were read under."
  [c tag inputs thunk]
  (let [entries (entries-of c)
        id [(:id c) (:slots c) tag]
        now (ctx/now c)
        look {:in inputs :values (:values @settings/state) :scope (.getStore settings/call-scope)}
        same? (fn [e] (and (identical? (:values e) (:values look)) (= (:scope e) (:scope look)) (= (:in e) inputs)))
        hit (when-not (::fresh c) (first (filter same? (:recent (get @entries id)))))]
    (if hit
      (:v hit)
      (let [v (thunk)]
        (swap! entries (fn [m] (assoc m id {:t now :recent (into [] (take (settings/get settings ::keep)) (cons (assoc look :v v) (remove same? (:recent (get m id)))))})))
        v))))
