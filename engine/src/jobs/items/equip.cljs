(ns jobs.items.equip
  (:require [engine.args :as a]
            ["minecraft-data" :as minecraft-data]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def doc
  "Hold a carried item in the main hand (:hand \"main\", default) or the off-hand (:hand \"off\"). Needs the item
  carried; a missing one is fetched (jobs.lib.need) unless :fetch is false. done? holds when it is held there.
  Ends :done {:item :hand}. Condition-failed: :bad-args (no :item), :unknown-item (no such item for the body's version),
  :no-item (not carried, :fetch false; both warn equip.refused), a failed fetch as {:reason :need} with its ending as the
  :because. Nothing-possible :failed (the server did not take it, data :equip the primitive's status; warn
  equip.refused). Quote: A, a fixed time.")

(a/defargs args
  {:item {:doc "the item to hold" :spec a/item? :default nil}
   :hand {:doc "\"main\" or \"off\"" :spec #{"main" "off"} :default "main"}
   :fetch {:doc "get a missing item (jobs.lib.need): true, a set of kinds or a map of limits; false stops :no-item instead" :spec fetch/option? :default true}})

(a/defargs settings
  {::equip-s {:default 0.5 :doc "Seconds to put one item in hand (the quote)." :spec (a/num-in 0 nil)}})

(def dests {"main" "hand" "off" "off-hand"})
(def slots {"main" :mainHand "off" :offHand})

(defn known-item? [p name]
  (.hasOwnProperty (.-itemsByName (minecraft-data (game/version-of p))) name))

(defn carried? [p item] (boolean (some #(= item (:name %)) (u/inventory p))))

(defn held-in [p hand]
  (some-> (.-equipment (.self p)) (aget (name (slots hand))) .-name))

(def target [:item :hand])

(defn problem
  "The cause when no item is given."
  [c]
  (when (nil? (:item (:args c)))
    {:reason :bad-args :why "equip needs :item"}))

(def conditions [[:fn problem]])

(defn done?
  "The item is in the hand asked for."
  [c]
  (let [{:keys [item hand]} (:args c)
        held (= item (held-in (:primitives c) hand))]
    {:done? held :units :items :left (if held 0 1)}))

(defn quote-of [_c]
  (let [t (settings/get settings ::equip-s)]
    {:uses {:time [0 t t]}}))

(defn fetch-opts [c] (fetch/opts c 'jobs.items.equip))

(defn refuse!
  [c kind reason text data]
  (ctx/emit! c :equip.refused :warn (assoc data :reason reason :text text))
  (ctx/end c kind reason data))

(defn ^:async round
  [c]
  (let [p (:primitives c)
        {:keys [item hand]} (:args c)
        opts (fetch-opts c)
        e (when (and (known-item? p item) (not (carried? p item)) opts) (await (need/item! c item 1 opts)))]
    (cond
      (not (known-item? p item)) (refuse! c :condition-failed :unknown-item (str "no such item " item) {:item item})
      (= :continue e) :continue
      (and e (not= :done (:kind e))) (ctx/end c (:kind e) (:cause e) {:item item})
      (not (carried? p item)) (refuse! c :condition-failed :no-item (str "cannot hold " item ": not carried") {:item item})
      :else
      (let [r (await (ctx/act c :equip (clj->js {:item item :dest (dests hand)})))
            status (.-status r)]
        (if (= "equipped" status)
          (do (ctx/emit! c :equip.done :info {:item item :hand hand :text (str "holding " item)})
              (ctx/end c :done nil {:item item :hand hand}))
          (refuse! c :nothing-possible :failed (str "cannot hold " item ": " status) {:item item :equip status}))))))
