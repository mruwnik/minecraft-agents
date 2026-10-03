(ns jobs.forestry.plant-sapling
  (:require [engine.ctx :as ctx]
            [engine.jobs.forestry :refer [debts target-of sapling-for log-name? replant-kind]]
            [engine.jobs.util :as u]))

(def doc "Plant a sapling at :at, or at the oldest replant debt, and clear that debt.")

(def args
  {:at {:doc "where to plant; the oldest :forestry/replant debt when nil" :default nil}
   :species {:doc "sapling species; any when nil" :default nil}})

(defn check
  "Nothing to plant (the round finishes), or a matching sapling is carried
  and the spot no longer holds a log."
  [c]
  (let [p (:primitives c)
        t (target-of (debts c) (:args c))]
    (cond
      (nil? t) true
      (nil? (sapling-for (u/inventory p) (:species t))) false
      (log-name? (some-> (.blockAt p (clj->js (:pos t))) .-name)) false
      :else true)))

(defn ^:async round
  "args {:at pos-or-nil :species name-or-nil}. Without :at, plants at the
  oldest replant debt in body memory (of species, when given). Equips a
  sapling, places it and clears the debt. Done at once when there is nothing
  to plant."
  [c]
  (let [t (target-of (debts c) (:args c))
        sapling (when t (sapling-for (u/inventory (:primitives c)) (:species t)))]
    (cond
      (nil? t) :done
      (nil? sapling) :continue
      :else
      (let [w (await (u/walk-near! c (:pos t) 3))]
        (case w
          :partial :continue
          :blocked (u/fail! c :plant_blocked "cannot reach the planting spot")
          (do (await (ctx/act c :equip (clj->js {:item sapling})))
              (let [r (await (ctx/act c :place (clj->js {:pos (:pos t) :item sapling})))]
                (if (#{"placed" "occupied"} (.-status r))
                  (do (ctx/forget-where! c replant-kind #(= (:pos t) (:pos %)))
                      :done)
                  (u/fail! c :plant_blocked (str "cannot plant: " (.-status r)))))))))))
