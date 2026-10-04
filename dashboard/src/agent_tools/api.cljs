(ns agent-tools.api
  "Compatibility boundary for the existing Node entry points and black-box tests.
   Tool implementations use native ClojureScript values internally."
  (:require [agent-tools.workspace]
            [agent-tools.changes :as changes]
            [agent-tools.drive :as drive]
            [agent-tools.entities :as entities]
            [agent-tools.inventory :as inventory]
            [agent-tools.jobs :as jobs]
            [agent-tools.map :as map-tool]
            [agent-tools.plans :as plans]
            [agent-tools.say :as say]
            [agent-tools.time :as time-tool]
            [agent-tools.triggers :as triggers]
            [agent-tools.world :as world]
            [agent-tools.storage-compat :as compat]))

(defn map-filters [ctx values]
  (compat/to-js (map-tool/filters (compat/from-js ctx) (compat/from-js values))))

(defn map-summary [doc]
  (compat/to-js (map-tool/summary (compat/from-js doc))))

(defn map-validate-zone [value]
  (map-tool/validate-zone (compat/from-js value)))

(def plan-usage (clj->js {:plans (:plan plans/usage) :blueprints (:blueprint plans/usage)}))
(defn plan-one-form [text] (compat/to-js (plans/one-form text)))
(defn blueprint-form [text] (compat/to-js (plans/blueprint-form text)))
(defn plan-request-for [kind argv]
  (compat/to-js (plans/request-for (if (= kind "plan") :plan :blueprint) argv)))

(defn map-options [argv]
  (compat/to-js (map-tool/options (vec argv))))

(defn map-execute [request]
  (-> (js/Promise.resolve nil)
      (.then (fn [] (map-tool/execute! (compat/from-js request))))
      (.then compat/to-js)
      (.catch (fn [error] (throw (compat/convert-error error))))))

(defn map-main [argv]
  (map-tool/main! (vec argv)))

(defn changes-options [argv]
  (compat/to-js (changes/options (vec argv))))

(defn changes-execute [request options]
  (let [output (when options (aget options "output"))
        signal (when options (aget options "signal"))
        opts (cond-> {}
               signal (assoc :signal signal)
               output (assoc :output (fn [value] (output (compat/to-js value)))))]
    (-> (js/Promise.resolve nil)
        (.then (fn [] (changes/execute! (compat/from-js request) opts)))
        (.then compat/to-js)
        (.catch (fn [error] (throw (compat/convert-error error)))))))

(defn changes-main [argv]
  (changes/main! (vec argv)))

(defn inventory-summary [value mode slots]
  (compat/to-js (inventory/compact (compat/from-js value) (keyword mode) slots)))

(defn time-options [argv] (compat/to-js (time-tool/options (vec argv))))
(defn time-clock [ctx now] (compat/to-js (time-tool/clock (compat/from-js ctx) now)))
(defn time-execute [request]
  (.then (time-tool/execute! (compat/from-js request)) compat/to-js))
(defn time-main [argv] (time-tool/main! (vec argv)))

(def jobs-usage jobs/usage)
(defn jobs-request-for [argv] (compat/to-js (jobs/request-for (vec argv))))
(defn jobs-main [argv] (jobs/main! (vec argv)))

(def triggers-usage triggers/usage)
(defn triggers-request-for [argv] (compat/to-js (triggers/request-for (vec argv))))
(defn triggers-main [argv] (triggers/main! (vec argv)))

(def entities-usage entities/usage)
(defn entities-options [argv]
  (compat/to-js (entities/options (vec argv))))
(defn entities-main [argv] (entities/main! (vec argv)))
(def say-usage (clj->js say/usage))
(defn say-request-for [argv] (compat/to-js (say/request-for (vec argv))))
(defn say-main [argv] (say/main! (vec argv)))

(def drive-usage (clj->js drive/usage))
(def drive-default-state-dir map-tool/default-state-dir)
(defn drive-main [argv] (drive/main! (vec argv)))
(defn drive-request-for [argv] (compat/to-js (drive/request-for (vec argv))))
(defn drive-socket-path-for [request] (drive/socket-path-for (compat/from-js request)))

(def world-usage (clj->js world/usage))
(defn world-main [argv] (world/main! (vec argv)))
(defn world-request-for [argv] (compat/to-js (world/request-for (vec argv))))
