(ns ^:dev/always engine.triggers
  "The trigger registry, built at compile time from the default trigger set (resource triggers/defaults.edn, see
  engine.registry): every trigger by id, {:name :when :job :args :persistence :cooldown-s :backoff}. :when is a plain fn
  (world view args world-knowledge live) -> truthy; :job the default job spec (an expression, see engine.expr);
  :args the trigger's own. A body's register orders its triggers; `order` is the default register's. See README.md, Triggers and
  the register."
  (:require-macros [engine.registry :refer [trigger-registry trigger-order facts-table]]))

(def all
  "Every default trigger by id."
  (trigger-registry))

(def order
  "The ids of the default trigger set in the order the default register fires them."
  (trigger-order))

(def facts
  "The facts a trigger condition may name (engine.condition), the table the default trigger set names (:facts)."
  (facts-table))
