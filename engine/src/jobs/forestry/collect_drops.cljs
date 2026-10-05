(ns jobs.forestry.collect-drops
  (:require [engine.ctx :as ctx]
            [engine.jobs.forestry :refer [default-radius]]))

(def doc
  "Collect the nearest matching dropped item, one per round, until none are
  left in radius. Hands over {:collected n}, the items that entered the
  inventory (the sum of the stack counts; ctx/result!).")

(def args
  {:radius {:doc "search radius in blocks" :default default-radius}
   :filter {:doc "item names to collect; everything when nil" :default nil}
   :visible-only {:doc "skip items the body has no line of sight to (a player cannot see through walls)" :default false}})

(defn check [_c] true)

(defn gained-count
  "The items an act result says entered the inventory."
  [r]
  (->> (array-seq (or (.-gained r) #js []))
       (map #(.-count %))
       (reduce + 0)))

(defn ^:async round
  "args {:radius 16 :filter [item names] or nil}. Collects the nearest
  matching dropped item, one per round. Items that could not be reached (or
  were in reach and not picked up) are remembered in job memory and skipped,
  and :collected counts the items gained, also on a round that gave up.
  Done when none are left in radius."
  [c]
  (let [{:keys [radius visible-only]} (:args c)
        wanted (some-> (:filter (:args c)) set)
        skipped (set (:skipped (ctx/mem c)))
        item (->> (array-seq (.entities (:primitives c) #js {:radius radius :kind "item" :max 32}))
                  (remove #(skipped (.-id %)))
                  (remove #(and visible-only (false? (.-visible %))))
                  (filter #(or (nil? wanted) (wanted (some-> (.-item %) .-name))))
                  first)]
    (if-not item
      (do (ctx/result! c {:collected (:collected (ctx/mem c) 0)})
          :done)
      (let [r (await (ctx/act c :collect #js {:id (.-id item)}))]
        (ctx/update-mem! c update :collected (fnil + 0) (gained-count r))
        (when (#{"unreachable" "timeout"} (.-status r))
          (ctx/update-mem! c update :skipped (fnil conj []) (.-id item)))
        :continue))))
