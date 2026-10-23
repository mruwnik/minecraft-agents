(ns jobs.items.smelt.fetch
  "What jobs.items.smelt gets before it loads: the missing input and fuel, and a furnace (jobs.lib.need children)."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.cost :as cost]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.items.smelt.plan :as sp]
            [jobs.lib.fuel :as fuel]
            [jobs.lib.furnace :as furnace]
            [jobs.lib.need :as need]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [clojure.string :as str]))

(defn carried [c] (u/inventory (:primitives c)))

(defn fetch-opts
  "The options of a need call for stage (a slot name) from the call's ways: nil when they allow no fetch."
  [c stage]
  (let [how (need/fetch-how c)]
    (when (seq how)
      {:how how :explore (:explore (:args c)) :search-worth (:search-worth (:args c)) :slot (keyword (str "need-" (name stage)))})))

(defn want-count
  "How many items are to be smelted: :count, else what is carried of the input (at least 1 when nothing is)."
  [c item]
  (let [have (storage/carried (carried c) item)]
    (or (:count (:args c)) (max 1 have))))

(defn fuel-capacity
  "How many items the carried fuel (only :fuel when given) would smelt, with a wood input's spare logs burnt as fuel
  beyond the n to smelt."
  [c item n]
  (let [{:keys [fuel]} (:args c)]
    (+ (transduce (comp (filter #(and (not= item (:name %)) (or (nil? fuel) (= fuel (:name %)))))
                        (keep #(some-> (fuel/fuel-per-unit "furnace" (:name %)) (* (:count %)))))
                  + 0 (carried c))
       (if (fuel/wood-fuel? item)
         (* (fuel/fuel-per-unit "furnace" item) (max 0 (- (storage/carried (carried c) item) n)))
         0))))

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
              short (- n (fuel-capacity c item n))
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

(defn own-possible?
  "Whether the body carries a furnace or the 8 cobblestone to make one."
  [c]
  (let [inv (carried c)]
    (or (pos? (storage/carried inv "furnace")) (>= (storage/carried inv "cobblestone") 8))))

(defn make-cost
  "What a furnace of its own costs, counting only what is missing: 0 with one carried, else the cobblestone short of 8 and,
  with no table carried or seen within 64, the table with the logs it is made of, a trip up to `surface-dy` blocks off when none are carried."
  [c surface-dy]
  (let [inv (carried c)
        have (fn [pred] (transduce (comp (filter #(pred (:name %))) (map :count)) + 0 inv))
        stone-short (max 0 (- 8 (have #{"cobblestone" "cobbled_deepslate"})))
        table? (or (pos? (storage/carried inv "crafting_table")) (crafting-table/nearest (:primitives c) 64))
        wood-short? (< (+ (have #(str/ends-with? % "_planks")) (* 4 (have #(str/ends-with? % "_log")))) 4)
        value (fn [name n] (if (pos? n) (:value (cost/item-value [{:name name :count n}])) 0))]
    (if (pos? (storage/carried inv "furnace"))
      0
      (+ (value "cobblestone" stone-short)
         (if (or table? (not wood-short?)) 0 (+ (value "crafting_table" 1) (:cost (cost/walk-cost {:distance surface-dy}))))))))

(defn walk-beats-own?
  "Whether walking to the furnace at pos costs no more than making one (make-cost) and putting it down here."
  [c pos]
  (cost/walk-beats-making {:distance (u/dist (u/self-pos c) pos)
                           :make (make-cost c (js/Math.abs (- (:y pos) (:y (u/self-pos c)))))}))

(defn cheaper-than-own
  "pos when the seen furnace there is walked to rather than putting a carried furnace (or cobblestone for one) down here, else nil."
  [c pos]
  (when (or (not (own-possible? c)) (walk-beats-own? c pos))
    pos))

(defn home-furnace
  "The furnace the body remembers (the :furnace place), not one a walk found unreachable: with anywhere? any distance,
  else only when walk-beats-own?. nil otherwise."
  [c anywhere?]
  (let [pos (mem/place (ctx/view c) :furnace)]
    (when (and pos (not (contains? (furnace/unreachable-furnaces c) pos))
               (or anywhere? (walk-beats-own? c pos)))
      pos)))

(defn ^:async furnace-fetch!
  "With no furnace seen: put a carried one down, else fetch one and put it down. Once per smelt. :again / :continue,
  nil when there is nothing to do (memory :furnace-why then says why the fetch failed)."
  [c]
  (when-let [opts (and (not (:furnace-tried (ctx/mem c))) (fetch-opts c :furnace))]
    (let [e (when-not (pos? (storage/carried (carried c) "furnace"))
              (await (need/item! c "furnace" 1 opts)))
          r (cond (= :continue e) :continue
                  (pos? (storage/carried (carried c) "furnace")) (await (furnace/place-furnace! c)))]
      (when-not r
        (ctx/update-mem! c assoc :furnace-tried true :furnace-why (when (map? e) (get-in e [:data :why]))))
      r)))
