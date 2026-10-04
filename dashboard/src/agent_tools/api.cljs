(ns agent-tools.api
  "Compatibility boundary for the existing Node entry points and black-box tests.
   Tool implementations use native ClojureScript values internally."
  (:require [agent-tools.changes :as changes]
            [agent-tools.inventory :as inventory]
            [agent-tools.jobs :as jobs]
            [agent-tools.map :as map-tool]
            [agent-tools.plans :as plans]
            [agent-tools.time :as time-tool]
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

(defn- edn-value-js [value]
  ;; edn-data requires nested maps in expression lists to stay EDN maps,
  ;; rather than the plain objects used by ordinary compatibility results.
  (cond
    (map? value) #js {:map (into-array (map (fn [[k v]] #js [(edn-value-js k) (edn-value-js v)]) value))}
    (vector? value) (into-array (map edn-value-js value))
    (seq? value) #js {:list (into-array (map edn-value-js value))}
    (set? value) #js {:set (into-array (map edn-value-js value))}
    :else (compat/to-js value)))

(defn jobs-spec-for [text] (edn-value-js (jobs/spec-for text)))
(defn jobs-request-for [argv]
  (let [request (jobs/request-for (vec argv)) converted (compat/to-js request)]
    (when-let [spec (get-in request [:request :spec])]
      (aset (.-request converted) "spec" (edn-value-js spec)))
    converted))
