(ns jobs.survival.eat
  (:require [engine.ctx :as ctx]))

(def doc "Eat the best food carried, once.")

(def args {:item {:doc "the food to eat; the best carried when nil" :default nil}})

(defn check [_c] true)

(defn ^:async round [c]
  (let [r (await (ctx/act c :eat (clj->js (select-keys (:args c) [:item]))))]
    (when (= "no-food" (.-status r))
      (ctx/emit! c :no_food :info {:text "nothing to eat"}))
    :done))
