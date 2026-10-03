(ns engine.catalog
  "Every job and trigger by name. Add new definitions here."
  (:require [engine.jobs.samples :as samples]
            [engine.triggers :as triggers]))

(defn by-name [defs]
  (into {} (map (juxt :name identity)) defs))

(def catalog
  {:jobs (by-name [samples/go-to samples/wait-for-day samples/eat])
   :triggers (by-name [triggers/health-low])})
