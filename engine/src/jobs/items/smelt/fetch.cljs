(ns jobs.items.smelt.fetch
  "What jobs.items.smelt gets before it loads: the missing input and fuel, and a furnace (jobs.lib.need children)."
  (:require [engine.ctx :as ctx]
            [jobs.items.smelt.plan :as sp]
            [jobs.lib.fuel :as fuel]
            [jobs.lib.furnace :as furnace]
            [jobs.lib.need :as need]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(defn carried [c] (u/inventory (:primitives c)))

(defn fetch-opts
  "The options of a need call for stage (a slot name) under the :fetch arg: nil when :fetch is off or leaves out items."
  [c stage]
  (let [o (need/parse-arg (:fetch (:args c)))]
    (when (and o (or (nil? (:what o)) (contains? (:what o) :item)))
      (-> (select-keys o [:how :explore])
          (assoc :slot (keyword (str "need-" (name stage))))))))

(defn want-count
  "How many items are to be smelted: :count, else what is carried of the input (at least 1 when nothing is)."
  [c item]
  (let [have (storage/carried (carried c) item)]
    (or (:count (:args c)) (max 1 have))))

(defn fuel-capacity
  "How many items the carried fuel (only :fuel when given) would smelt."
  [c item]
  (let [{:keys [fuel]} (:args c)]
    (transduce (comp (filter #(and (not= item (:name %)) (or (nil? fuel) (= fuel (:name %)))))
                     (keep #(some-> (fuel/fuel-per-unit "furnace" (:name %)) (* (:count %)))))
               + 0 (carried c))))

(defn stage-problem
  "The :need wait of the fetch stage in memory (:input or :fuel) while it is unmet, else nil."
  [c fuels wood-fuels]
  (let [item (sp/input-of (:item (:args c)) (carried c))
        n (when item (want-count c item))]
    (when item
      (case (:fetch-stage (ctx/mem c))
        :input (let [need (- n (storage/carried (carried c) item))]
                 (when (pos? need) {:reason :need :item item :count need}))
        (:fuel :wood-fuel)
        (let [fuel (:fuel (:args c))
              names (cond fuel [fuel] (= :fuel (:fetch-stage (ctx/mem c))) fuels :else wood-fuels)
              short (- n (fuel-capacity c item))
              need (js/Math.ceil (/ short (fuel/fuel-per-unit "furnace" (first names))))]
          (when (and (pos? short) (not (and fuel (= :wood-fuel (:fetch-stage (ctx/mem c))))))
            (if fuel
              {:reason :need :item fuel :count need}
              {:reason :need :any-of names :count need})))
        nil))))

(defn ^:async fetch-stages!
  "Before the first load: fetch the missing input, then the missing fuel, once each. A fetch that finds nothing is left
  to the load plan, which gives up naming the missing thing. nil when done with them, else :continue while a fetch waits."
  [c fuels wood-fuels]
  (when (nil? (:owed (ctx/mem c)))
    (loop [[stage & more] (remove #(contains? (:fetch-done (ctx/mem c)) %) [:input :fuel :wood-fuel])]
      (when stage
        (ctx/update-mem! c assoc :fetch-stage stage)
        (let [problem (stage-problem c fuels wood-fuels)
              opts (fetch-opts c stage)
              e (when (and problem opts)
                  (await (if (:item problem)
                           (need/item! c (:item problem) (:count problem) opts)
                           (need/blocks! c (:any-of problem) (:count problem) opts))))]
          (if (= :continue e)
            :continue
            (do (ctx/update-mem! c update :fetch-done (fnil conj #{}) stage)
                (recur more))))))))

(defn ^:async furnace-fetch!
  "With no furnace seen: put a carried one down, else fetch one and put it down. Once per smelt. :again / :continue,
  nil when there is nothing to do."
  [c]
  (when-let [opts (and (not (:furnace-tried (ctx/mem c))) (fetch-opts c :furnace))]
    (let [e (when-not (pos? (storage/carried (carried c) "furnace"))
              (await (need/item! c "furnace" 1 opts)))
          r (cond (= :continue e) :continue
                  (pos? (storage/carried (carried c) "furnace")) (await (furnace/place-furnace! c)))]
      (when-not r (ctx/update-mem! c assoc :furnace-tried true))
      r)))
