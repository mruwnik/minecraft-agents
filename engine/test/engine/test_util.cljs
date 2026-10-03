(ns engine.test-util
  "Helpers shared by the cljs tests: temp dirs, the fake world, async tests."
  (:require [cljs.test :refer [is]]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            ["module" :refer [createRequire]]))

(def require-here (createRequire (str (js/process.cwd) "/")))

(defn tmp-dir []
  (fs/mkdtempSync (path/join (os/tmpdir) "engine-test-")))

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

(defn kinds
  "The \"source.kind\" names of collected events, in order."
  [seen]
  (mapv #(str (name (:source %)) "." (name (:kind %))) @seen))
