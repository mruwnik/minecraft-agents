(ns jobs.village.offers
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.ready :as ready]
            [jobs.lib.trade-window :as tw]
            [jobs.lib.villager :as villager]))

(def meta {:tags [:goal]})

(def doc
  "Read the offers of the villager :villager (uuid or entity id; nil: the nearest adult in sight that has offers, a
  villager that shows none is skipped for the next) and end; nothing is bought or paid. It finds, walks within reach
  and opens the window exactly as jobs.village.trade does (same causes: :gone, :not-villager, :unreachable,
  :no-offers, :window), and emits the info `trade.offers` (profession, level, every offer with an enchanted book's
  enchantments and levels).
  Ends :done with the data {:villager id :profession p :level n :offers [{:cost [{:item :count}] :gives {:item :count
  :enchants [{:name :level}]} :index :disabled :uses :max-uses}]}. Reading again later gives the prices of then.
  What was read is kept across a cut and restart.")

(a/defargs args
  {:villager {:doc "the villager's entity uuid or numeric entity id; nil: the nearest adult villager in sight, the next one when it has no offers" :spec villager/id? :default nil}})

(def target [:villager])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds one reading may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(def can-start? (constantly (contract/ok :start "nothing needed")))

(defn done?
  "Ok once the offers are read (kept in memory), with them as its result."
  [c]
  (if-let [read (:read (ctx/mem c))]
    (contract/ok :read "offers read" :result read)
    (contract/later :reading "offers not read yet")))

(defn quote-of [_] {:uses {:job-time [0 5 ##Inf]}})

(defn finish!
  "End without offers: :condition-failed for a villager gone or no villager, else nothing-possible."
  [c reason _]
  (ctx/end c (if (#{:gone :not-villager} (:reason reason reason)) :condition-failed :nothing-possible) reason {}))

(defn ^:async keep!
  "Book the offers read and end with them."
  [c villager r offers]
  (let [read {:villager villager :profession (.-profession r) :level (.-level r) :offers offers}]
    (ctx/update-mem! c assoc :read read)
    (ctx/end c :done nil read)))

(defn ^:async round
  "One call is the whole reading."
  [c]
  (await (tw/run! c finish! #(tw/read! % finish! :trade.offers keep!) (fn [_]))))
