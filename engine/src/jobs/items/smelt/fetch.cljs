(ns jobs.items.smelt.fetch
  "What jobs.items.smelt gets before it loads: the missing input and fuel, and a furnace (jobs.lib.need children)."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.items.craft :as craft]
            [jobs.items.smelt.plan :as sp]
            [jobs.lib.need :as need]
            [jobs.lib.places :as places]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(defn carried [c] (u/inventory (:primitives c)))

(defn fetch-opts
  "The options of a need call for stage (a slot name) under the :fetch arg: nil when :fetch is off or leaves out items."
  [c stage]
  (let [o (need/parse-arg (:fetch (:args c)))]
    (when (and o (or (nil? (:what o)) (contains? (:what o) :item)))
      (-> (select-keys o [:how :depth :explore])
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
                     (keep #(some-> (sp/fuel-per-unit "furnace" (:name %)) (* (:count %)))))
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
              need (js/Math.ceil (/ short (sp/fuel-per-unit "furnace" (first names))))]
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

(defn ^:async place-furnace!
  "Put the carried furnace down beside the body (jobs.blocks.place child; craft/spot! finds the cell) and make it the
  furnace of this job. :again once it stands, :continue while a child waits, nil when it could not be put down (memory
  :furnace-refusal says why)."
  [c]
  (let [spot (or (:furnace-spot (ctx/mem c)) (await (craft/spot! c)))]
    (cond
      (= :continue spot) :continue
      (nil? spot) (do (ctx/update-mem! c assoc :furnace-refusal "no free cell beside the body to put it on")
                      nil)
      :else
      (do (ctx/update-mem! c assoc :furnace-spot spot)
          (let [e (await (ctx/call c :place 'jobs.blocks.place {:item "furnace" :pos spot :fetch false}))]
            (if (= :continue e)
              :continue
              (do (ctx/update-mem! c dissoc :furnace-spot)
                  (if (and (= :done (:kind e)) (get-in e [:data :placed]))
                    (do (ctx/update-mem! c assoc :furnace spot)
                        (places/offer! c :furnace spot)
                        (ctx/emit! c :smelt.furnace :info {:furnace spot :text (str "put a furnace down at " (:x spot) " " (:y spot) " " (:z spot))})
                        :again)
                    (do (ctx/update-mem! c assoc :furnace-refusal (str (or (get-in e [:data :why]) (some-> (contract/cause-key e) name) "it was not placed")))
                        nil)))))))))

(defn ^:async furnace-fetch!
  "With no furnace seen: put a carried one down, else fetch one and put it down. Once per smelt. :again / :continue,
  nil when there is nothing to do."
  [c]
  (when-let [opts (and (not (:furnace-tried (ctx/mem c))) (fetch-opts c :furnace))]
    (let [e (when-not (pos? (storage/carried (carried c) "furnace"))
              (await (need/item! c "furnace" 1 opts)))
          r (cond (= :continue e) :continue
                  (pos? (storage/carried (carried c) "furnace")) (await (place-furnace! c)))]
      (when-not r (ctx/update-mem! c assoc :furnace-tried true))
      r)))
