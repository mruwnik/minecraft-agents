(ns jobs.items.wear
  (:require [engine.args :as a]
            [jobs.lib.armour :as armour]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]))

(def doc
  "Put armour on. With :item, that carried piece goes into its slot, and a worn piece of the slot comes back to
  the pockets. Without it, the best carried piece goes into each slot that is empty or worn with a weaker one
  (leather < golden < turtle helmet < chainmail < iron < diamond < netherite).
  Result: {:worn [{:item :slot}]}, info wear.done. Nothing better than what is worn gives info wear.nothing.
  :reason \"not-armour\" (the item is no armour piece) or \"no-item\" (not carried) comes with warn wear.refused.
  A piece the server does not take gives :reason \"failed\" and :status, warn wear.failed.")

(a/defargs args
  {:item {:doc "the armour piece to wear; nil wears the best carried piece for each slot" :spec a/item? :default nil}})

(defn check
  "Always runnable; the item, when given, is checked by the round."
  [c]
  (let [item (:item (:args c))]
    (or (nil? item) (string? item)
        (ctx/wait c {:reason :bad-args :why "the item must be an armour piece name"}))))

(defn ^:async round
  [c]
  (let [p (:primitives c)
        item (:item (:args c))
        worn (armour/worn-of (.-equipment (.self p)))
        carried (map :name (u/inventory p))
        r (await (armour/wear! (fn [i slot] (ctx/act c :equip (clj->js {:item i :dest slot}))) worn carried item))
        result (cond-> {:worn (:worn r)} (:reason r) (assoc :reason (name (:reason r))) (:status r) (assoc :status (:status r)))]
    (cond
      (not (:ok r))
      (ctx/emit! c (if (= :failed (:reason r)) :wear.failed :wear.refused) :warn
                 (assoc (select-keys result [:reason :status]) :item (:item r) :text (str "cannot wear " (:item r) ": " (name (:reason r)))))
      (empty? (:worn r))
      (ctx/emit! c :wear.nothing :info {:text "nothing carried is better than what is worn"})
      :else
      (ctx/emit! c :wear.done :info {:worn (:worn r) :text (str "wore " (count (:worn r)) " armour piece(s)")}))
    (ctx/result! c result)
    :done))
