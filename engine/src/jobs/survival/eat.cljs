(ns jobs.survival.eat
  (:require [engine.ctx :as ctx]))

(def doc "Eat the best food carried, once.")

(def args {:item {:doc "the food to eat; the best carried when nil" :default nil}})

(defn check [_c] true)

(defn ^:async round [c]
  (let [item (:item (:args c))
        r (await (ctx/act c :eat (if item #js {:item item} #js {})))]
    (when (= "no-food" (.-status r))
      (ctx/emit! c :no_food :info {:text "nothing to eat"}))
    :done))
