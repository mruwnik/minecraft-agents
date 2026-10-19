(ns jobs.memory.unreserve
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.ready :as ready]
            [jobs.lib.reserve :as reserve]))

(def doc
  "Drop entries from the body's reserve register, in one round: the entry named :id, or every entry of :item.
  Entries of other jobs are dropped too; they write them again if they still need them.
  Neither or both of :id and :item is refused before any round: ends :impossible {:reason :bad-args :text}.
  Success: ends :done with data {:removed n}; the memory layer emits info reserve.cleared with :removed. Nothing to drop is already done.")

(a/defargs args
  {:id {:doc "the entry's name" :spec (a/or-of keyword? string?) :default nil}
   :item {:doc "drop every entry of this item name" :spec a/name? :default nil}})

(def target [:id :item])

(a/defargs settings
  {::job-time-s {:default 5 :doc "Seconds one call may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(defn bad-args [{:keys [id item]}]
  (when (= (some? id) (some? item))
    {:reason :bad-args :text "name exactly one of :id and :item"}))

(defn matching
  "The live entries the call drops."
  [c]
  (let [{:keys [id item]} (:args c)]
    (filterv #(if id (= (name id) (:id %)) (= item (:item %))) (reserve/entries (ctx/view c)))))

(defn can-start? [c] (checks/cause-status :never (bad-args (:args c))))

(defn done? [c]
  (if (and (nil? (bad-args (:args c))) (empty? (matching c)))
    (contract/ok :cleared "nothing of that is reserved" :units :entries :done 1 :left 0 :result {:removed 0})
    (contract/later :reserved "still reserved" :units :entries :done 0 :left (count (matching c)) :result {:removed 0})))

(defn quote-of [_c] {:uses {:job-time [0 0 1]}})

(def safe? ready/ok)
(def fit? ready/ok)

(defn round [c]
  (let [{:keys [id item]} (:args c)
        n (count (matching c))]
    (ctx/forget-where! c reserve/kind #(if id (= (name id) (:id %)) (= item (:item %))))
    (ctx/end c :done nil {:removed n})))
