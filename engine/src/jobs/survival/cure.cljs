(ns jobs.survival.cure
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.cost.health :as cost-health]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def doc
  "Cure a poison: drink the carried milk bucket (it clears every effect), else eat a golden apple (its regeneration outweighs the poison;
  the effect itself runs out later), one item per call. Only a poisoned or withered body needs it.
  Done? means no hp-costing effect. Poisoned, it needs a cure carried, else condition-failed :no-cure (a listed job waits), unless
  the call's :ways allow a fetch and the cheapest fetch of milk or a golden apple (jobs.lib.need/item-price) is cheaper than
  waiting the effect out (cost.health/hp-cost of ticking-hp): then it is fetched (need/item!) and taken.
  The trigger fires only with a cure carried, so it never fetches.
  Ends :done {:used item} after the item was taken; a failed attempt is nothing-possible :cure-failed.
  Quote: one drink or bite, the cure as the item used.")

(a/defargs settings
  {::job-time-s {:default 10 :doc "Seconds the drink or bite may take." :spec (a/num-in 0 nil)}})

(a/defargs args
  {:item {:doc "the cure to use; the best carried when nil" :spec a/item? :default nil}})

(def ways need/fetch-ways)

(def fetchable "The cures a fetch can bring: the enchanted golden apple is no one's find." ["milk_bucket" "golden_apple"])

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

(defn wait-cost
  "Seconds waiting the poison out costs: the hp it takes (cost.health/ticking-hp) at the price of an hp."
  [c]
  (let [self (.self (:primitives c))]
    (cost-health/hp-cost (.-health self)
                         (cost-health/ticking-hp (map (fn [e] {:name (.-name e) :amplifier (.-amplifier e) :duration (.-duration e)}) (array-seq (.-effects self)))
                                                 (.-health self)))))

(defn fetch-plan
  "{:item :price} of the fetchable cure the call's ways bring at the least price, when that is less than waiting the poison out; else nil."
  [c]
  (when (need/fetch-opts c)
    (let [wait (wait-cost c)
          opts (need/fetch-opts c)]
      (->> (if-let [i (:item (:args c))] [i] fetchable)
           (map (fn [i] {:item i :price (need/item-price c i 1 opts nil)}))
           (filter #(< (:price %) wait))
           (sort-by :price)
           first))))

(defn no-cure
  [c]
  (if (or (carried-cure c) (fetch-plan c))
    (contract/ok :cure "a cure carried or worth fetching")
    (contract/later :no-cure "no milk bucket or golden apple carried, and none worth fetching")))

(def can-start? no-cure)

(defn quote-of [c]
  (let [t (settings/get settings ::job-time-s)]
    (cond-> {:uses {:job-time [0 t t]}}
      (carried-cure c) (assoc-in [:uses :items (carried-cure c)] [1 1 1]))))

(defn ^:async round
  "Takes the best carried cure once."
  [c]
  (let [best (carried-cure c)
        plan (when-not best (fetch-plan c))
        fetched (when plan (await (need/item! c (:item plan) 1 (need/fetch-opts c))))]
    (cond
      (not (ctx/alive? c)) :continue
      (= :continue fetched) :continue
      (and fetched (not= :done (:kind fetched))) fetched
      (and (nil? best) (nil? plan)) (ctx/end c :condition-failed :no-cure)
      (nil? best) :continue
      :else
      (let [r (await (ctx/act c :eat #js {:item best :always true}))]
        (if (= "ate" (.-status r))
          (ctx/end c :done nil {:used best})
          (ctx/end c :nothing-possible :cure-failed {:text (str "taking " best " failed: " (.-status r))}))))))
