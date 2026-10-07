(ns engine.build-hooks
  "shadow-cljs build hooks. See README.md, Jobs."
  (:require [engine.registry :as registry]
            [shadow.build.modules :as modules]))

(defn add-job-namespaces
  "Put every job namespace under jobs/, every trigger namespace (registry/trigger-namespaces) and every hook
  namespace at the front of the :main module's entries, so they are compiled and loaded although nothing requires
  them, and make engine.registry wait for the jobs and the namespaces declaring settings, engine.settings-registry for the engine's own, engine.triggers for the triggers and engine.hooks for the hooks
  (:extra-requires, as shadow's own test runner does) so their macros see them analyzed. Runs at :compile-prepare, which comes every compile cycle
  (watch included), after the target set its entries; the modules are re-analyzed."
  {:shadow.build/stage :compile-prepare}
  [state]
  (let [jobs (mapv :ns (registry/job-namespaces))
        triggers (registry/trigger-namespaces)
        hooks (registry/hook-namespaces)
        settings (registry/settings-namespaces)
        engine-settings (registry/engine-settings-namespaces)
        nss (vec (distinct (concat jobs triggers hooks settings engine-settings)))]
    (if (empty? nss)
      state
      (let [state (-> state
                      (update-in [::modules/config :main :entries] #(vec (distinct (concat nss %))))
                      (modules/analyze))
            registry-id (get-in state [:sym->id 'engine.registry])
            triggers-id (get-in state [:sym->id 'engine.triggers])
            hooks-id (get-in state [:sym->id 'engine.hooks])
            engine-settings-id (get-in state [:sym->id 'engine.settings-registry])]
        (cond-> state
          (and registry-id (seq (concat jobs settings))) (update-in [:sources registry-id] assoc :extra-requires (set (concat jobs settings)))
          (and engine-settings-id (seq engine-settings)) (update-in [:sources engine-settings-id] assoc :extra-requires (set engine-settings))
          (and triggers-id (seq triggers)) (update-in [:sources triggers-id] assoc :extra-requires (set triggers))
          (and hooks-id (seq hooks)) (update-in [:sources hooks-id] assoc :extra-requires (set hooks)))))))
