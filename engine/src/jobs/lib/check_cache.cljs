(ns jobs.lib.check-cache
  "A short-lived cache for what a job's check reads off the world, so a check run every tick reads it once per
  window. A check cannot write job memory, so the values live here, per job instance, and are lost on restart."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [clojure.string]))

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

(defn root-id
  "The instance id a (child) call id descends from: child call ids are the parent's id plus \"/name\"."
  [id]
  (first (clojure.string/split id #"/" 2)))

(defn without-ended
  "entries m without the ids of jobs that are no longer instances of c's engine (but c's own): the engine drops an instance
  at every kind of end, so this frees a job's entries however it ended. Child calls are not instances: they live with their root."
  [c m]
  (let [live (some-> (:state (:engine c)) deref :instances)]
    (if (nil? live)
      m
      (into {} (filter (fn [[[job]]] (or (= job (:id c)) (contains? live (root-id job))))) m))))

(defn store
  "m with entry e under id, without the entries unused for 10 windows (a child call that ended leaves its own: only its
  root's end frees them otherwise); an entry of a new job id also frees those of ended jobs."
  [c m id e]
  (let [now (ctx/now c)
        m (into {} (remove (fn [[_ x]] (> (- now (:t x)) (* 10 (ttl-ms))))) m)]
    (assoc (if (some (fn [[[job]]] (= job (first id))) m) m (without-ended c m)) id e)))

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
        (swap! entries (fn [m] (store c m id {:t now :v v})))
        v))))

(defn cached
  "The value of (thunk) for key k of this job, reused while it is younger than ttl-ms (never under fresh)."
  [c k thunk]
  (reuse c k thunk (fn [now hit] (<= 0 (- now (:t hit)) (ttl-ms)))))

(defn kept-value
  "The value kept! for key k of this job while younger than ttl-ms, nil when none (or under fresh)."
  [c k]
  (let [hit (get @(entries-of c) [(:id c) (:slots c) k])]
    (when (and hit (not (::fresh c)) (<= 0 (- (ctx/now c) (:t hit)) (ttl-ms))) (:v hit))))

(defn keep!
  "Keep v under key k of this job (kept-value): a search a call goes on with at the next."
  [c k v]
  (swap! (entries-of c) (fn [m] (store c m [(:id c) (:slots c) k] {:t (ctx/now c) :v v}))))

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
      (do (swap! entries (fn [m] (cond-> m (contains? m id) (assoc-in [id :t] now)))) (:v hit))
      (let [v (thunk)]
        (swap! entries (fn [m] (store c m id {:t now :recent (into [] (take (settings/get settings ::keep)) (cons (assoc look :v v) (remove same? (:recent (get m id)))))})))
        v))))

(defonce bodies (js/WeakMap.))

(defn body-keyed
  "keyed for a pure (thunk) of `inputs`, kept for every job of c's body (engine; none without one): a value no world
  read, only its inputs and the settings, decides."
  [c tag inputs thunk]
  (if-let [key (:warned (:engine c))]
    (let [entries (or (.get bodies key) (let [e (atom {})] (.set bodies key e) e))
          look {:in inputs :values (:values @settings/state) :scope (.getStore settings/call-scope)}
          same? (fn [e] (and (identical? (:values e) (:values look)) (= (:scope e) (:scope look)) (= (:in e) inputs)))
          hit (first (filter same? (get @entries tag)))]
      (if hit
        (:v hit)
        (let [v (thunk)]
          (swap! entries update tag #(into [] (take (settings/get settings ::keep)) (cons (assoc look :v v) (remove same? %))))
          v)))
    (thunk)))
