(ns jobs.survival.cure
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.util :as u]))

(def doc
  "Cure a poison: drink the carried milk bucket (it clears every effect), else eat a golden apple (its regeneration outweighs the poison;
  the effect itself runs out later), one item per call. Only a poisoned or withered body needs it.
  Done? means no hp-costing effect. Poisoned, it needs a cure carried, else condition-failed :no-cure (a listed job waits).
  Ends :done {:used item} after the item was taken; a failed attempt is nothing-possible :cure-failed.
  Quote: one drink or bite, the cure as the item used.")

(a/defargs settings
  {::job-time-s {:default 10 :doc "Seconds the drink or bite may take." :spec (a/num-in 0 nil)}})

(a/defargs args
  {:item {:doc "the cure to use; the best carried when nil" :spec a/item? :default nil}})

(def defaults {:spend {:job-time ::job-time-s}})

(def meta {:category :reflex})

(def safe? ready/ok)

(def fit? ready/ok)

(def target [:item])

(defn carried-cure [c]
  (body/cure-carried (u/inventory (:primitives c)) (:item (:args c))))

(defn done? [c]
  (let [d (not (body/poisoned? (:primitives c)))]
    (checks/goal d [:cured "no poison or wither effect"] [:poisoned "poisoned or withered"] {:units :effects :left (if d 0 1)})))

(defn no-cure [c] (if (carried-cure c) (contract/ok :cure "a cure carried") (contract/later :no-cure "no milk bucket or golden apple carried")))

(def can-start? no-cure)

(defn quote-of [c]
  (let [t (settings/get settings ::job-time-s)]
    (cond-> {:uses {:job-time [0 t t]}}
      (carried-cure c) (assoc-in [:uses :items (carried-cure c)] [1 1 1]))))

(defn ^:async round
  "Takes the best carried cure once."
  [c]
  (let [best (carried-cure c)]
    (cond
      (not (ctx/alive? c)) :continue
      (nil? best) (ctx/end c :condition-failed :no-cure)
      :else
      (let [_ (await (ctx/act c :equip #js {:item best}))
            r (await (ctx/act c :eat #js {:item best :always true}))]
        (if (= "ate" (.-status r))
          (ctx/end c :done nil {:used best})
          (ctx/end c :nothing-possible :cure-failed {:text (str "taking " best " failed: " (.-status r))}))))))
