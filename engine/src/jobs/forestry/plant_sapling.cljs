(ns jobs.forestry.plant-sapling
  (:require [engine.ctx :as ctx]
            [engine.jobs.gate :as gate]
            [engine.jobs.forestry :refer [debts target-of sapling-for log-name? replant-kind]]
            [engine.jobs.util :as u]
            [engine.path.near :as near]))

(def doc
  "Plant a sapling at :at, or at the oldest replant debt, and clear that debt.

  Zones and claims are a rule the job consults: a spot in a zone or claim of another owner, or in a plan's footprint
  (but :for-plan's own), is not planted, asked when chosen and again right before the place; the job is done at
  once, as with nothing to plant, and a debt stays owed. One plant-sapling.declined warn per job names the zones,
  claims and plans ({:reason :refused ...}); without a zone list it declines with {:reason :no-zones}.
  :ignore-zones? acts regardless.")

(def args
  {:at {:doc "where to plant; the oldest :forestry/replant debt when nil" :default nil}
   :species {:doc "sapling species; any when nil" :default nil}
   :bone-meal {:doc "bone meal uses after planting, 0 for none" :default 0}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(defn spot-allowed?
  "Whether the job may plant at pos (one warn per job when refused)."
  [c pos]
  (gate/allowed? c :plant-sapling.declined "plant-sapling" :place pos {:except (:for-plan (:args c))}))

(defn sapling-at?
  "True when the block at pos ends in _sapling."
  [p pos]
  (boolean (some-> (u/block-name p pos) (.endsWith "_sapling"))))

(defn has-meal? [p]
  (some #(= "bone_meal" (:name %)) (u/inventory p)))

(defn ^:async meal-round
  "One bone meal use on the planted sapling; done when it grew, the uses are
  spent or no bone meal is carried."
  [c {:keys [pos left]}]
  (let [p (:primitives c)]
    (if (or (zero? left) (not (sapling-at? p pos)) (not (has-meal? p)))
      (do (ctx/update-mem! c dissoc :meal) :done)
      (let [w (await (near/walk-near! c pos 3))]
        (case w
          :partial :continue
          (do (await (ctx/act c :useOn #js {:pos (clj->js pos) :item "bone_meal" :face "up"}))
              (ctx/update-mem! c update-in [:meal :left] dec)
              :continue))))))

(defn check
  "Nothing to plant (the round finishes), or a matching sapling is carried
  and the spot no longer holds a log."
  [c]
  (let [p (:primitives c)
        t (target-of (debts c) (:args c))]
    (cond
      (:meal (ctx/mem c)) true
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
        sapling (when t (sapling-for (u/inventory (:primitives c)) (:species t)))
        meal (:meal (ctx/mem c))]
    (cond
      meal (await (meal-round c meal))
      (nil? t) :done
      (not (spot-allowed? c (:pos t))) :done
      (nil? sapling) :continue
      :else
      (let [w (await (near/walk-near! c (:pos t) 3))]
        (case w
          :partial :continue
          :blocked (u/fail! c :plant_blocked "cannot reach the planting spot")
          (if-not (spot-allowed? c (:pos t))
            :done
            (do (await (ctx/act c :equip (clj->js {:item sapling})))
              (let [r (await (ctx/act c :place (clj->js {:pos (:pos t) :item sapling})))]
                (if (#{"placed" "occupied"} (.-status r))
                  (let [n (:bone-meal (:args c))]
                    (ctx/forget-where! c replant-kind #(= (:pos t) (:pos %)))
                    (if (and (pos? n) (has-meal? (:primitives c)))
                      (do (ctx/update-mem! c assoc :meal {:pos (:pos t) :left n}) :continue)
                      :done))
                  (u/fail! c :plant_blocked (str "cannot plant: " (.-status r))))))))))))
