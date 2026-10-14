(ns jobs.items.wear
  (:require [engine.args :as a]
            [jobs.lib.armour :as armour]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(def doc
  "Put armour on. With :item, that carried piece goes into its slot, and a worn piece of the slot comes back to
  the pockets. Without it, the best carried piece goes into each slot that is empty or worn with a weaker one
  (leather < golden < turtle helmet < chainmail < iron < diamond < netherite).
  Needs nothing; done? holds when nothing carried is better than what is worn.
  Ends :done with {:worn [{:item :slot}]}, info wear.done. A named item that is no armour piece or not carried is a condition (a listed call waits); a started call ends
  condition-failed :not-armour or :no-item with warn wear.refused. A piece the server does not take ends
  nothing-possible :failed with the pieces worn so far and :server-status, warn wear.failed.
  Quote: time per piece to put on.")

(a/defargs args
  {:item {:doc "the armour piece to wear; nil wears the best carried piece for each slot" :spec a/item? :default nil}})

(a/defargs settings
  {::piece-s {:default 0.5 :doc "Seconds to put one armour piece on (the quote)." :spec (a/num-in 0 nil)}})

(def target [:item])

(defn plan-of
  "armour/plan for c: worn pieces and carried item names read from the body."
  [c]
  (let [p (:primitives c)]
    (armour/plan (armour/worn-of (.-equipment (.self p))) (map :name (u/inventory p)) (:item (:args c)))))

(defn refused
  "The cause :not-armour or :no-item when the named :item cannot be put on."
  [c]
  (let [{:keys [ok reason]} (plan-of c)]
    (when-not ok reason)))

(def conditions [[:fn refused]])

(defn done?
  "Nothing to put on (a refused item is not done)."
  [c]
  (let [{:keys [ok put]} (plan-of c)
        left (count put)]
    {:done? (and ok (zero? left)) :units :pieces :left left}))

(defn ^:async quote-of [c]
  (let [n (count (:put (plan-of c)))
        t (* n (settings/get settings ::piece-s))]
    {:uses {:time [(min t (settings/get settings ::piece-s)) t t]}}))

(defn ^:async round
  [c]
  (let [p (:primitives c)
        worn (armour/worn-of (.-equipment (.self p)))
        carried (map :name (u/inventory p))
        r (await (armour/wear! (fn [i slot] (ctx/act c :equip (clj->js {:item i :dest slot}))) worn carried (:item (:args c))))
        data {:worn (:worn r)}]
    (cond
      (:ok r)
      (do (ctx/emit! c :wear.done :info {:worn (:worn r) :text (str "wore " (count (:worn r)) " armour piece(s)")})
          (ctx/end c :done nil data))
      (= :failed (:reason r))
      (do (ctx/emit! c :wear.failed :warn {:reason "failed" :status (:status r) :item (:item r) :text (str "cannot wear " (:item r) ": failed")})
          (ctx/end c :nothing-possible :failed (assoc data :server-status (:status r))))
      :else
      (do (ctx/emit! c :wear.refused :warn {:reason (name (:reason r)) :item (:item r) :text (str "cannot wear " (:item r) ": " (name (:reason r)))})
          (ctx/end c :condition-failed (:reason r) data)))))
