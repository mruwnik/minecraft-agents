(ns settings-demo.declares
  "Test fixture for engine.settings-test: a helper namespace that declares settings."
  (:require [engine.args :as a]))

(a/defargs settings
  {:settings-demo.declares/size {:default 3 :doc "A size." :spec (a/int-in 0 nil)}})
