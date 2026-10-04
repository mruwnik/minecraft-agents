(ns engine.fake.node
  "The Node require the fake world uses to load the JS modules it shares with the real primitives. Its own namespace so
  engine.test-util can require the fake without a cycle."
  (:require ["module" :refer [createRequire]]))

(def require-here (createRequire (str (js/process.cwd) "/")))
