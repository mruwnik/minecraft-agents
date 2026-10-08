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

(defn js-specs
  "{key spec} of the JavaScript primitives' tuning numbers, registered as settings: specs is the SPECS object of
  js/settings.mjs, whose entries {default, spec: [kind lo hi], doc} become the engine.args shape."
  [specs]
  (a/declare-settings!
   (into {} (map (fn [[k {:keys [default spec doc]}]]
                   (let [[kind lo hi] spec
                         [make sym] (case kind "int-in" [a/int-in 'int-in] "num-in" [a/num-in 'num-in])]
                     [(keyword k) {:default default :spec (make lo hi) :doc doc :form (list sym lo hi)}])))
         (js->clj specs :keywordize-keys true))))

(defn js-reader
  "The reader the primitives get (key string -> live value, nil for a key not declared): it reads the state at each
  call, so a reload reaches the JavaScript at its next use."
  []
  (fn [k]
    (let [specs (:specs @state)
          kw (keyword k)]
      (when (contains? specs kw) (get specs kw)))))

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
    (reset! state {:body body :specs specs :values values :layers layers
                   :load {:world-file world-file :body-file body-file :body body}})
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

(defn reload!
  "Load again from the files of the last load!, passing each settings.bad event to emit. Throws before any load."
  [emit]
  (let [{:keys [specs load]} @state]
    (when-not load (throw (ex-info "settings were never loaded" {})))
    (load! (assoc load :specs specs :emit emit))))

(defn write-body-key!
  "Change the body file by f (a map fn) and reload. {:ok true}, or {:ok false :reason} when the file is unreadable."
  [f emit]
  (let [file (:body-file (:load @state))
        [kind data] (read-layer file)]
    (if (= :bad kind)
      {:ok false :reason (str file " " data "; fix or delete it first")}
      (do (fs/writeFileSync file (str (pr-str (f (if (= :ok kind) data {}))) "\n"))
          (reload! emit)
          {:ok true}))))

(defn set-key!
  "Write k = v into the body's settings.edn after checking it against the key's spec, then reload (events to emit).
  {:ok true} or {:ok false :reason} with the file left alone."
  [k v emit]
  (let [{:keys [specs load]} @state
        spec (clojure.core/get specs k)
        why (when spec (a/problem k v))]
    (cond
      (not (:body-file load)) {:ok false :reason "no body settings file (settings were never loaded)"}
      (not spec) {:ok false :reason (str "unknown key " k)}
      why {:ok false :reason (str k " must be " why ", got " (pr-str v))}
      :else (write-body-key! #(assoc % k v) emit))))

(defn unset-key!
  "Remove k from the body's settings.edn and reload. {:ok true} or {:ok false :reason}."
  [k emit]
  (let [{:keys [specs load]} @state]
    (cond
      (not (:body-file load)) {:ok false :reason "no body settings file (settings were never loaded)"}
      (not (contains? specs k)) {:ok false :reason (str "unknown key " k)}
      :else (write-body-key! #(dissoc % k) emit))))

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

(defn view
  "What the tools read: every key's value, layer and doc, and the rates. p is the primitives (for physics-ms)."
  [p]
  {:settings (resolved) :game-rate (game-rate) :rate-source (rate-source) :frozen? (frozen?) :physics-ms (physics-ms p)})
