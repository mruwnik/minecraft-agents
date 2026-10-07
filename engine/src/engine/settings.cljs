(ns engine.settings
  "The global tuning numbers: layers (code default, world file, body file), validation and provenance.
  See README.md, Settings."
  (:refer-clojure :exclude [get])
  (:require [engine.args :as a]
            [cljs.reader :as reader]
            ["fs" :as fs]))

(a/defargs settings
  "The engine's own keys, :engine.<area>/<name>, each {:default :doc :spec} (engine.args, as job args)."
  {:engine.perception/save-ms {:default 60000 :spec (a/int-in 1000 nil)
                               :doc "How often the body's seen-world file (seen.bin) is saved, in ms of wall clock."}})

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
                         why (if spec (a/problem k v) :unknown)]
                     (if why
                       [values layers (conj events (bad-event file k (if spec (str "must be " why ", got " (pr-str v)) "unknown key")))]
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

;; ---------------------------------------------------------------- the rates
;; Two rates: the game rate (ticks per second the server runs at, /tick rate; item despawn, smelting and the day
;; are counted in these ticks) and the physics step (physics-ms, how often the body's physics runs). Wall-clock
;; timers (the engine tick, JS time scale) are neither and are never converted.

(defonce clock
  (atom {:rate 20 :frozen false :source :assumed}))

(defn set-clock! [rate frozen source]
  (reset! clock {:rate rate :frozen (boolean frozen) :source source}))

(defn game-rate "Game ticks per second: the server's last set_ticking_state, else 20." [] (:rate @clock))
(defn frozen? "True while the server's tick is frozen." [] (:frozen @clock))
(defn rate-source "Where the game rate comes from: :packet or :assumed (no packet yet)." [] (:source @clock))

(defn ticks->ms "Wall-clock ms n game ticks take at the live game rate." [n] (/ (* n 1000) (game-rate)))
(defn ms->ticks "Game ticks in ms of wall clock at the live game rate." [ms] (/ (* ms (game-rate)) 1000))

(defn physics-ms
  "The body's physics step in ms: the primitives' physicsMs (the tick-rate shim's interval), else 50."
  [p]
  (if (and p (fn? (.-physicsMs p))) (.physicsMs p) 50))

(defn wire-game-clock!
  "Follow the JS game clock gc (engine/js/game-clock.mjs): its rate now and after every packet. With no packet yet
  the rate stays 20, assumed, and emit gets one :info event saying so."
  [gc emit]
  (let [sync! (fn [rate frozen source] (set-clock! rate frozen (keyword source)))]
    (sync! (.-rate gc) (.-frozen gc) (.-source gc))
    (.onChange gc sync!)
    (when (= :assumed (rate-source))
      (emit {:source :system :kind (keyword "game-rate.assumed") :level :info
             :text "no set_ticking_state packet came since the join; assuming 20 ticks per second"}))))
