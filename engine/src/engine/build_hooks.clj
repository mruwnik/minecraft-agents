(ns engine.build-hooks
  "shadow-cljs build hooks. See README.md, Jobs."
  (:require [engine.registry :as registry]
            [shadow.build.modules :as modules]))

(defn add-job-namespaces
  "Put every job namespace under jobs/ at the front of the :main module's
  entries, so they are compiled and loaded although nothing requires them,
  and make engine.registry wait for them (:extra-requires, as shadow's own
  test runner does) so its macro sees them analyzed. Runs at
  :compile-prepare, which comes every compile cycle (watch included), after
  the target set its entries; the modules are re-analyzed."
  {:shadow.build/stage :compile-prepare}
  [state]
  (let [nss (mapv :ns (registry/job-namespaces))]
    (if (empty? nss)
      state
      (let [state (-> state
                      (update-in [::modules/config :main :entries] #(vec (distinct (concat nss %))))
                      (modules/analyze))
            registry-id (get-in state [:sym->id 'engine.registry])]
        (cond-> state
          registry-id (update-in [:sources registry-id] assoc :extra-requires (set nss)))))))
