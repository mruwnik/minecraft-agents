(ns engine.fake.node
  "The Node require the fake world uses to load the JS modules it shares with the real primitives. Its own namespace so
  engine.test-util can require the fake without a cycle."
  (:require ["fs" :as fs]
            ["module" :refer [createRequire]]
            ["path" :as path]))

(defn engine-root
  "The nearest ancestor of dir that holds js/blocks.mjs, else fallback (test builds in a temp dir sit outside the repo)."
  [dir fallback]
  (let [parent (.dirname path dir)]
    (cond
      (.existsSync fs (str dir "/js/blocks.mjs")) dir
      (= parent dir) fallback
      :else (recur parent fallback))))

(def require-here (createRequire (str (engine-root js/__dirname (js/process.cwd)) "/")))
