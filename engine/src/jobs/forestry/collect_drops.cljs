(ns jobs.forestry.collect-drops
  (:require [engine.ctx :as ctx]
            [engine.jobs.forestry :refer [default-radius]]))

(def doc "Collect the nearest matching dropped item, one per round, until none are left in radius.")

(def args
  {:radius {:doc "search radius in blocks" :default default-radius}
   :filter {:doc "item names to collect; everything when nil" :default nil}})

(defn check [_c] true)

(defn ^:async round
  "args {:radius 16 :filter [item names] or nil}. Collects the nearest
  matching dropped item, one per round. Items that could not be reached are
  remembered in job memory and skipped. Done when none are left in radius."
  [c]
  (let [{:keys [radius]} (:args c)
        wanted (some-> (:filter (:args c)) set)
        skipped (set (:skipped (ctx/mem c)))
        item (->> (array-seq (.entities (:primitives c) #js {:radius radius :kind "item" :max 32}))
                  (remove #(skipped (.-id %)))
                  (filter #(or (nil? wanted) (wanted (some-> (.-item %) .-name))))
                  first)]
    (if-not item
      :done
      (let [r (await (ctx/act c :collect #js {:id (.-id item)}))]
        (when (#{"unreachable" "timeout"} (.-status r))
          (ctx/update-mem! c update :skipped (fnil conj []) (.-id item)))
        :continue))))
