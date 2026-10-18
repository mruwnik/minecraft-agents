(ns engine.settings
  "The global tuning numbers: layers (code default, world file, body file), validation and provenance.
  See README.md, Settings."
  (:refer-clojure :exclude [get])
  (:require [engine.args :as a]
            [cljs.reader :as reader]
            ["fs" :as fs]
            ["async_hooks" :refer [AsyncLocalStorage]]))

(defonce call-scope
  ;; AsyncLocalStorage, not a dynamic var: a job's rounds await, and the scope must follow them
  (AsyncLocalStorage.))

(defn scoped
  "Run f with the overrides {k v} laid over those of the enclosing scope, for f and everything it awaits. A call's
  :settings key (engine.core.ending/wrap)."
  [overrides f]
  (if (empty? overrides)
    (f)
    (.run call-scope (merge (.getStore call-scope) overrides) f)))

(a/defargs settings
  "The engine's own keys, :engine.<area>/<name>, each {:default :doc :spec} (engine.args, as job args)."
  {:engine.perception/save-ms {:default 60000 :spec (a/int-in 1000 nil)
                               :doc "How often the body's seen-world file (seen.bin) is saved, in ms of wall clock."}
   :engine.settings/job-args {:default {} :spec (a/map-of symbol? map?)
                              :doc "Job arg defaults over the code's, {job-symbol {arg value}}; a per-call arg still wins. Frozen into a job's args when it is submitted."}})

(defonce state
  (atom {:body nil :specs {} :values {} :layers {}}))

(defn get
  "The value of key k: the enclosing call's :settings override (scoped), else the layered override, else the :default of k in specs (the declaring namespace's own
  `settings` map). Throws on a key specs lacks."
  [specs k]
  (let [spec (clojure.core/get specs k)]
    (when-not spec
      (throw (ex-info (str "unknown setting " k) {:key k})))
    (let [values (:values @state)
          scope (.getStore call-scope)]
      (cond (contains? scope k) (clojure.core/get scope k)
            (contains? values k) (clojure.core/get values k)
            :else (:default spec)))))

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

(def job-args-key :engine.settings/job-args)

(defn clean-job-args
  "[kept problems] for the overlay {job-sym {arg value}} against jobs ({sym entry}, the job registry): an unknown job or arg
  or a value its arg spec refuses is dropped, with one text in problems."
  [jobs overlay]
  (reduce-kv
   (fn [[kept problems] sym args]
     (let [data (:args (clojure.core/get jobs sym))]
       (if-not (contains? jobs sym)
         [kept (conj problems (str sym ": unknown job"))]
         (reduce-kv
          (fn [[kept problems] k v]
            (let [why (cond (not (contains? data k)) "unknown arg"
                            (nil? v) "a value, not nil"
                            :else (when-let [p (a/first-problem sym data {k v})]
                                    (str "must be " (:want p) ", got " (pr-str v))))]
              (if why
                [kept (conj problems (str sym " " k ": " why))]
                [(assoc-in kept [sym k] v) problems])))
          [kept problems] args))))
   [{} []] overlay))

(defn job-overlay
  "The settings' override of job sym's arg defaults, {arg value} (empty when none)."
  [sym]
  (clojure.core/get (clojure.core/get (:values @state) job-args-key) sym {}))

(defn job-defaults
  "The default args of job sym (data: its args map): the code's :default of each arg under the settings' overlay."
  [sym data]
  (a/merge-args data (a/defaults data) (job-overlay sym)))

(defn job-arg-layer
  "Where job sym's arg default comes from: :world or :body when the overlay sets it, else :code."
  [sym arg]
  (let [layers (:layers @state)]
    (if (contains? (job-overlay sym) arg)
      (clojure.core/get layers [sym arg] (clojure.core/get layers job-args-key))
      :code)))

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
  "[values layers events] with the file's keys applied over them at layer, each key checked against specs. The job-args
  of a layer are checked against jobs (when given) and merged over the lower layer's, job by job."
  [specs jobs [values layers events :as acc] layer file]
  (let [[kind data] (read-layer file)]
    (case kind
      :missing acc
      :bad [values layers (conj events (bad-event file nil data))]
      (reduce-kv (fn [[values layers events] k v]
                   (let [spec (clojure.core/get specs k)
                         why (if spec (a/problem k v) :unknown)]
                     (cond
                       why [values layers (conj events (bad-event file k (if spec (str "must be " why ", got " (pr-str v)) "unknown key")))]
                       (and jobs (= k job-args-key))
                       (let [[kept problems] (clean-job-args jobs v)]
                         [(assoc values k (merge-with merge (clojure.core/get values k) kept))
                          (into (assoc layers k layer) (for [[sym args] kept, arg (keys args)] [[sym arg] layer]))
                          (into events (map #(bad-event file k (str % "; ignored"))) problems)])
                       :else [(assoc values k v) (assoc layers k layer) events])))
                 acc data))))

(defn load!
  "Read the world file, then the body file, over the code defaults of specs, and keep the result. Each bad value,
  unknown key or unreadable file keeps the lower layer and is passed to emit as one settings.bad :info event.
  Callable again to reload. Throws when a different body already loaded in this process (one body per process)."
  [{:keys [specs jobs world-file body-file body emit]}]
  (let [loaded (:body @state)]
    (when (and loaded (not= loaded body))
      (throw (ex-info (str "second body " body " in one process (settings are global, " loaded " loaded first)") {:body body}))))
  (let [[values layers events] (-> [{} {} []]
                                   (->> (#(apply-layer specs jobs % :world world-file)))
                                   (->> (#(apply-layer specs jobs % :body body-file))))]
    (reset! state {:body body :specs specs :values values :layers layers
                   :load {:world-file world-file :body-file body-file :body body :jobs jobs}})
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

(declare form-end)

(defn- skip-blank
  "Index of the next form in text from i: past whitespace, commas, ; comments and #_ discarded forms."
  [text i]
  (let [n (count text)]
    (loop [i i]
      (let [c (when (< i n) (nth text i))]
        (cond
          (nil? c) i
          (or (contains? #{\space \tab \newline \return \,} c)) (recur (inc i))
          (= c \;) (recur (let [e (.indexOf text "\n" i)] (if (neg? e) n e)))
          (and (= c \#) (= \_ (nth text (inc i) nil))) (recur (form-end text (skip-blank text (+ i 2))))
          :else i)))))

(defn- form-end
  "Index just past the form that starts at i."
  [text i]
  (let [n (count text)
        c (nth text i)]
    (cond
      (= c \") (loop [j (inc i)]
                 (cond (>= j n) n
                       (= \\ (nth text j)) (recur (+ j 2))
                       (= \" (nth text j)) (inc j)
                       :else (recur (inc j))))
      (or (contains? #{\{ \[ \(} c) (and (= c \#) (contains? #{\{ \(} (nth text (inc i) nil))))
      (loop [j (inc (if (= c \#) (inc i) i))]
        (let [j (skip-blank text j)
              d (when (< j n) (nth text j))]
          (cond (nil? d) n
                (contains? #{\} \] \)} d) (inc j)
                :else (recur (form-end text j)))))
      :else (loop [j (inc i)]
              (if (and (< j n) (not (contains? #{\space \tab \newline \return \, \; \" \{ \} \[ \] \( \)} (nth text j))))
                (recur (inc j))
                j)))))

(defn- map-layout
  "{:open i :close i :forms [[start end] ...]} of the top-level map in text, or nil when it is not one."
  [text]
  (let [open (skip-blank text 0)]
    (when (= \{ (nth text open nil))
      (loop [i (inc open) forms []]
        (let [j (skip-blank text i)]
          (cond (>= j (count text)) nil
                (= \} (nth text j)) {:open open :close j :forms forms}
                :else (let [e (form-end text j)] (recur e (conj forms [j e])))))))))

(defn edit-text
  "The map text with key k set to v (op :set) or removed (op :unset), every other character kept (comments, layout).
  nil when the text is not a map the edit can follow."
  [text op k v]
  (when-let [{:keys [forms close open]} (map-layout text)]
    (let [kstr (str k)
          pairs (partition 2 forms)
          hit (first (filter (fn [[[ks ke]]] (= kstr (subs text ks ke))) pairs))
          splice (fn [s e r] (str (subs text 0 s) r (subs text e)))]
      (cond
        (odd? (count forms)) nil
        (and hit (= op :set)) (let [[_ [vs ve]] hit] (splice vs ve (pr-str v)))
        hit (let [[[ks] [_ ve]] hit
                  ve (loop [j ve] (if (contains? #{\space \tab \,} (nth text j nil)) (recur (inc j)) j))
                  line-start (inc (.lastIndexOf text "\n" (dec ks)))
                  alone? (and (= \newline (nth text ve nil)) (every? #{\space \tab} (subs text line-start ks)))]
              (if alone? (splice line-start (inc ve) "") (splice ks ve "")))
        (= op :unset) text
        (empty? forms) (splice (inc open) (inc open) (str kstr " " (pr-str v)))
        :else (let [at (second (last forms))] (splice at at (str " " kstr " " (pr-str v))))))))

(defn write-atomic!
  "Write text to file through a temp file and a rename, so a reader never sees half of it."
  [file text]
  (let [temp (str file "." (.-pid js/process) ".tmp")]
    (try (fs/writeFileSync temp text)
         (fs/renameSync temp file)
         (finally (when (fs/existsSync temp) (fs/unlinkSync temp))))))

(defn write-body-key!
  "Set (op :set) or remove (op :unset) k in the body file, keeping its comments and layout, then reload. {:ok true},
  or {:ok false :reason} when the file is unreadable."
  [op k v emit]
  (let [file (:body-file (:load @state))
        [kind data] (read-layer file)]
    (if (= :bad kind)
      {:ok false :reason (str file " " data "; fix or delete it first")}
      (let [data (if (= :ok kind) data {})
            wanted (if (= op :set) (assoc data k v) (dissoc data k))
            edited (when (= :ok kind) (edit-text (fs/readFileSync file "utf8") op k v))
            ok? (and edited (= wanted (reader/read-string edited)))]
        (write-atomic! file (if ok? edited (str (pr-str wanted) "\n")))
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
      :else (write-body-key! :set k v emit))))

(defn unset-key!
  "Remove k from the body's settings.edn and reload. {:ok true} or {:ok false :reason}."
  [k emit]
  (let [{:keys [specs load]} @state]
    (cond
      (not (:body-file load)) {:ok false :reason "no body settings file (settings were never loaded)"}
      (not (contains? specs k)) {:ok false :reason (str "unknown key " k)}
      :else (write-body-key! :unset k nil emit))))

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
