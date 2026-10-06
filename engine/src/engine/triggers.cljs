(ns ^:dev/always engine.triggers
  "The trigger registry, built at compile time from the default trigger set (resource triggers/defaults.edn, see
  engine.registry): every trigger by id, {:name :when :job :args :persistence :cooldown-s}. :when is a plain fn
  (world view args world-knowledge live) -> truthy; :job the default job spec (an expression, see engine.expr);
  :args the trigger's own. Order means nothing: a body's register orders its triggers. See README.md, Triggers and
  the register."
  (:require-macros [engine.registry :refer [trigger-registry facts-table]]))

(def all
  "Every default trigger by id."
  (trigger-registry))

(def facts
  "The facts a trigger condition may name (engine.condition), the table the default trigger set names (:facts)."
  (facts-table))
