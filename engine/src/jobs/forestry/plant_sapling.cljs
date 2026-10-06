(ns jobs.forestry.plant-sapling
  (:require [engine.ctx :as ctx]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.forestry.trees :refer [debts target-of sapling-for log-name? replant-kind]]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]))

(def doc
  "Plant a sapling at :at, or at the oldest replant debt, and clear that debt. With :bone-meal n it then uses up to
  n bone meal on it.
  With :near and :within only the debts within :within blocks of :near count; the others stay owed.
  A missing sapling is fetched (jobs.lib.fetch: a seen chest, a craft) unless :fetch is false; then, or when the
  fetch failed, it waits (check) with :reason :no-sapling (:species). It also waits :log-on-spot when the spot still
  holds a log. Ends at once when there is nothing to plant or the spot is refused (a debt stays owed).
  Zones: a spot in another owner's zone or claim, or in a plan's footprint (but :for-plan's own), is not planted.
  The job warns plant-sapling.declined once, with :reason :refused (or :no-zones when no zone list was read).
  :ignore-zones? true skips the check.")

(def args
  {:at {:doc "where to plant; the oldest :forestry/replant debt when nil" :type :pos :default nil}
   :near {:doc "{:x :z}: only replant debts within :within blocks of it are taken; nil: any" :type :pos :default nil}
   :within {:doc "radius for :near" :default nil}
   :species {:doc "sapling species; any when nil" :default nil}
   :bone-meal {:doc "bone meal uses after planting, 0 for none" :default 0}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :default nil}
   :fetch {:doc "get a missing sapling (jobs.lib.fetch): true, a set of kinds or a map of limits; false waits :no-sapling" :default true}
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

(defn problem
  "The wait {:reason :no-sapling :species} while there is a spot to plant and no matching sapling is carried; else nil.
  The :need fields (:item, or :any-of when no species) are what a fetch gets."
  [c]
  (let [t (when-not (:meal (ctx/mem c)) (target-of (debts c) (:args c)))
        species (:species t)]
    (when (and t (nil? (sapling-for (u/inventory (:primitives c)) species)))
      (cond-> {:reason :no-sapling :species species}
        species (assoc :item (str species "_sapling"))))))

(def saplings ["oak_sapling" "spruce_sapling" "birch_sapling" "jungle_sapling" "acacia_sapling" "dark_oak_sapling" "cherry_sapling"])

(defn fetch-wait
  "The :need wait a fetch of w's sapling answers."
  [w]
  (if (:item w)
    {:reason :need :item (:item w)}
    {:reason :need :any-of saplings}))

(defn shown-wait
  "w without :item, plus the :fetch field when a fetch for it failed lately."
  [c w]
  (let [o (fetch/opts c 'jobs.forestry.plant-sapling)
        f (when (and o (not (:error o)))
            (some->> (fetch/plan-for (fetch-wait w) o) :key (fetch/failure c)))]
    (cond-> (dissoc w :item)
      f (assoc :fetch (fetch/shown f)))))

(defn check
  "Nothing to plant (the round finishes), or a matching sapling is carried or will be fetched, and the spot no longer
  holds a log. Else it waits with reason :no-sapling (and :species) or :log-on-spot (and :pos)."
  [c]
  (let [p (:primitives c)
        t (target-of (debts c) (:args c))]
    (cond
      (:meal (ctx/mem c)) true
      (nil? t) true
      (problem c) (let [w (problem c)
                        r (fetch/check c 'jobs.forestry.plant-sapling (fetch-wait w))]
                    (or (true? r) (ctx/wait c (shown-wait c w))))
      (log-name? (some-> (u/block-at p (:pos t)) .-name)) (ctx/wait c {:reason :log-on-spot :pos (:pos t)})
      :else true)))

(defn ^:async round
  [c]
  (let [t (target-of (debts c) (:args c))
        sapling (when t (sapling-for (u/inventory (:primitives c)) (:species t)))
        meal (:meal (ctx/mem c))]
    (cond
      meal (await (meal-round c meal))
      (nil? t) :done
      (not (spot-allowed? c (:pos t))) :done
      (nil? sapling) (or (await (fetch/fetch! c 'jobs.forestry.plant-sapling #(some-> (problem %) fetch-wait))) :continue)
      :else
      (let [w (await (near/walk-near! c (:pos t) 3))]
        (case w
          :partial :continue
          :blocked (u/fail! c :plant_blocked "cannot reach the planting spot")
          (cond
            (not (spot-allowed? c (:pos t))) :done
            (nil? (u/block-name (:primitives c) (:pos t)))
            (do (ctx/warn-once! c [:unknown (:pos t)] :plant-sapling.unknown {:pos (:pos t)}) :done)
            :else
            (do (await (ctx/act c :equip (clj->js {:item sapling})))
              (let [r (await (ctx/act c :place (clj->js {:pos (:pos t) :item sapling})))]
                (if (#{"placed" "occupied"} (.-status r))
                  (let [n (:bone-meal (:args c))]
                    (ctx/forget-where! c replant-kind #(= (:pos t) (:pos %)))
                    (if (and (pos? n) (has-meal? (:primitives c)))
                      (do (ctx/update-mem! c assoc :meal {:pos (:pos t) :left n}) :continue)
                      :done))
                  (u/fail! c :plant_blocked (str "cannot plant: " (.-status r))))))))))))
