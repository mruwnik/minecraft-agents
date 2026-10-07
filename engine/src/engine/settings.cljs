(ns engine.settings
  "The global tuning numbers: layers (code default, world file, body file), validation and provenance.
  See README.md, Settings."
  (:refer-clojure :exclude [get])
  (:require [cljs.reader :as reader]
            [engine.expr :as expr]
            ["fs" :as fs]))

(def settings
  "The engine's own keys, :engine.<area>/<name>, each {:default :doc :type ...} (the job arg spec shape)."
  {})

(defonce state
  (atom {:body nil :specs {} :values {} :layers {}}))

(defn get
  "The value of key k: the layered override, else the :default of k in specs (the declaring namespace's own
  `settings` map). Throws on a key specs lacks."
  [specs k]
  (let [spec (clojure.core/get specs k)]
    (when-not spec
      (throw (ex-info (str "unknown setting " k) {:key k})))
    (let [values (:values @state)]
      (if (contains? values k) (clojure.core/get values k) (:default spec)))))

(defn spec-problem
  "Why v does not fit spec, or nil. An override is never nil."
  [spec v]
  (if (nil? v) "a value, not nil" (expr/type-problem spec v)))

(defn read-layer
  "[:missing], [:ok map] or [:bad why] for a settings file."
  [file]
  (if-not (and file (fs/existsSync file))
    [:missing]
    (try
      (let [data (reader/read-string (fs/readFileSync file "utf8"))]
        (if (map? data) [:ok data] [:bad (str "not a map, found " (pr-str data))]))
      (catch :default e [:bad (str "unreadable: " (.-message e))]))))

(defn bad-event [file k why]
  (cond-> {:source :system :kind (keyword "settings.bad") :level :info :file file
           :text (str "settings: " file (when k (str " " k)) ": " why "; ignored")}
    k (assoc :key k)))

(defn apply-layer
  "[values layers events] with the file's keys applied over them at layer, each key checked against specs."
  [specs [values layers events] layer file]
  (let [[kind data] (read-layer file)]
    (case kind
      :missing [values layers events]
      :bad [values layers (conj events (bad-event file nil data))]
      (reduce-kv (fn [[values layers events] k v]
                   (let [spec (clojure.core/get specs k)
                         why (if spec (spec-problem spec v) "an unknown key")]
                     (if why
                       [values layers (conj events (bad-event file k (str "must be " why (when spec (str ", got " (pr-str v))))))]
                       [(assoc values k v) (assoc layers k layer) events])))
                 [values layers events] data))))

(defn load!
  "Read the world file, then the body file, over the code defaults of specs, and keep the result. Each bad value,
  unknown key or unreadable file keeps the lower layer and is passed to emit as one settings.bad :info event.
  Callable again to reload. Throws when a different body already loaded in this process (one body per process)."
  [{:keys [specs world-file body-file body emit]}]
  (let [loaded (:body @state)]
    (when (and loaded (not= loaded body))
      (throw (ex-info (str "second body " body " in one process (settings are global, " loaded " loaded first)") {:body body}))))
  (let [[values layers events] (-> [{} {} []]
                                   (->> (#(apply-layer specs % :world world-file)))
                                   (->> (#(apply-layer specs % :body body-file))))]
    (reset! state {:body body :specs specs :values values :layers layers})
    (run! emit events)
    nil))

(defn resolved
  "{k {:value :layer :doc}} for every declared key: layer :code, :world or :body (or :test)."
  []
  (let [{:keys [specs values layers]} @state]
    (into {} (map (fn [[k spec]]
                    [k {:value (if (contains? values k) (clojure.core/get values k) (:default spec))
                        :layer (clojure.core/get layers k :code)
                        :doc (:doc spec)}]))
          specs)))

(defn with-settings
  "Run f with a fresh settings state holding the overrides {k v} (layer :test), then restore the state, also after a
  throw and, when f returns a promise, once it settles. For tests."
  [overrides f]
  (let [saved @state
        restore! #(reset! state saved)]
    (reset! state {:body nil :specs {} :values overrides :layers (zipmap (keys overrides) (repeat :test))})
    (let [result (try (f) (catch :default e (restore!) (throw e)))]
      (if (and result (fn? (.-then result)))
        (-> result
            (.then (fn [v] (restore!) v) (fn [e] (restore!) (throw e))))
        (do (restore!) result)))))
