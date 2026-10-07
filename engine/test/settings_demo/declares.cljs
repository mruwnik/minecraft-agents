(ns settings-demo.declares
  "Test fixture for engine.settings-test: a helper namespace that declares settings.")

(def settings
  {:settings-demo.declares/size {:default 3 :doc "A size." :type :int :min 0}})
