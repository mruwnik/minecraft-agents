(ns engine.test-util
  "Helpers shared by the cljs tests: temp dirs, the fake world, async tests."
  (:require [cljs.test :refer [is]]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            ["module" :refer [createRequire]]))

(def require-here (createRequire (str (js/process.cwd) "/")))

(defonce made-dirs (atom []))

(defonce remove-dirs-on-exit
  (.on js/process "exit"
       (fn [] (run! #(fs/rmSync % #js {:recursive true :force true}) @made-dirs))))

(defn tmp-dir
  "A fresh temp dir, removed when the test process exits."
  []
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "engine-test-"))]
    (swap! made-dirs conj dir)
    dir))

(defn fake
  "A fake primitives object from js/fake.mjs built from a cljs spec map."
  ([] (fake {}))
  ([spec] ((.-createFake (require-here "./js/fake.mjs")) (clj->js spec))))

(defn pos [x y z] #js {:x x :y y :z z})

(defn run-async
  "Run the promise-returning thunk f inside a cljs.test async block."
  [done f]
  (-> (js/Promise.resolve)
      (.then f)
      (.catch (fn [e] (is (nil? e) (str "async test threw: " e "\n" (.-stack e)))))
      (.finally done)))

(defn read-json [file]
  (js->clj (js/JSON.parse (fs/readFileSync file "utf8")) :keywordize-keys true))

(defn capture-sink
  "An events sink and the atom it collects into."
  []
  (let [seen (atom [])]
    [seen (fn [e] (swap! seen conj e))]))

(defn legacy-event
  "Test-only adapter for older scheduler/job assertions. Production consumers
  should inspect canonical :context and :data instead."
  [event]
  (let [context (:context event)
        projected (merge {:seq (:seq event) :t (:time-ms event) :body "Fake"}
                         (dissoc context :job-id :reflex-id :cause-seq)
                         (:data event)
                         (select-keys event [:source :kind :request-id :attention]))]
    (cond-> projected
      (:job-id context) (assoc :job (:job-id context))
      (:reflex-id context) (assoc :reflex (:reflex-id context))
      (:cause-seq context) (assoc :cause (:cause-seq context))
      (some? (:message event)) (assoc :text (:message event)))))

(defn legacy-capture-sink
  "Legacy-shaped test sink. Kept separate so direct appender tests exercise
  the canonical event API without a compatibility projection."
  []
  (let [seen (atom [])]
    [seen (fn [e] (swap! seen conj (legacy-event e)))]))

(defn kinds
  "The \"source.kind\" names of collected events, in order."
  [seen]
  (mapv #(str (name (:source %)) "." (name (:kind %))) @seen))
