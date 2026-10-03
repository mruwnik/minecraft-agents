(ns engine.catalog
  "Every job and trigger by name. Add new definitions here."
  (:require [engine.jobs.forestry :as forestry]
            [engine.jobs.samples :as samples]
            [engine.jobs.storage :as storage]
            [engine.jobs.survival :as survival]
            [engine.triggers :as triggers]))

(defn by-name [defs]
  (into {} (map (juxt :name identity)) defs))

(def catalog
  {:jobs (by-name [samples/go-to samples/wait-for-day samples/eat samples/look-around samples/pace
                   forestry/fell-tree forestry/collect-drops forestry/plant-sapling forestry/harvest-wood
                   storage/deposit
                   survival/retreat survival/sleep])
   :triggers (by-name [triggers/health-low triggers/hostile-near
                       triggers/night-and-bed-known triggers/inventory-nearly-full triggers/every-interval])})
