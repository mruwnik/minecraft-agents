(ns ^:dev/always engine.settings-registry
  "The settings of the engine's own namespaces (those under engine/src/engine declaring a `settings` map), found at
  compile time; jobs and triggers are in engine.registry. See README.md, Settings."
  (:require-macros [engine.registry :refer [engine-settings-registry]]))

(def settings-by-ns
  "{ns-symbol settings-map} of every engine namespace that declares one (not main, which adds its own)."
  (engine-settings-registry))

(def settings
  "{key spec} of the engine's own settings."
  (into {} (mapcat val) settings-by-ns))
