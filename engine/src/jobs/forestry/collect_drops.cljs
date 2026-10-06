(ns jobs.forestry.collect-drops
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.forestry.trees :refer [default-radius]]))

(def doc
  "Collect the nearest matching dropped item, in one call, until none is left within :radius of where the job began (the work area: a walk to an item
  does not move it, so drops far away, e.g. other bodies', are never chased).
  An item that cannot be reached or picked up is skipped.
  Result: {:collected n}, the number of items that entered the inventory.")

(def args
  {:radius {:doc "search radius in blocks" :default default-radius}
   :filter {:doc "item names to collect; everything when nil" :default nil}
   :near {:doc "{:x :y :z} the work area is centred on, instead of where the body stands when the job begins" :type :pos :default nil}
   :ids {:doc "entity ids to collect (only those); any item when nil" :default nil}
   :visible-only {:doc "skip items the body has no line of sight to (a player cannot see through walls)" :default false}})

(defn check [_c] true)

(defn gained-count
  "The items an act result says entered the inventory."
  [r]
  (->> (array-seq (or (.-gained r) #js []))
       (map #(.-count %))
       (reduce + 0)))

(defn nearest-item
  "The nearest wanted item within :radius of the anchor that is not skipped, or nil."
  [c anchor]
  (let [{:keys [radius visible-only ids]} (:args c)
        only-ids (some-> ids set)
        wanted (some-> (:filter (:args c)) set)
        skipped (set (:skipped (ctx/mem c)))]
    (->> (array-seq (.entities (:primitives c) #js {:radius (+ radius (u/dist anchor (u/self-pos c))) :kind "item" :max (if (or only-ids wanted) 1024 32)}))
         (filter #(if-let [p (.-pos %)] (<= (u/dist anchor (u/pos-of p)) radius) true))
         (remove #(skipped (.-id %)))
         (filter #(or (nil? only-ids) (only-ids (.-id %))))
         (remove #(and visible-only (false? (.-visible %))))
         (filter #(or (nil? wanted) (wanted (some-> (.-item %) .-name))))
         first)))

(defn ^:async round
  "One whole attempt: collects the nearest matching dropped item again and again until none is left in radius.
  Items that could not be reached (or were in reach and not picked up) are remembered in job memory and skipped,
  and :collected counts the items gained, also on a round that gave up.
  :ids limits it to those entity ids (a dig's own drops)."
  [c]
  (let [anchor (or (:anchor (ctx/mem c))
                   (let [a (or (:near (:args c)) (u/self-pos c))] (ctx/update-mem! c assoc :anchor a) a))]
    (loop []
      (let [item (when (ctx/alive? c) (nearest-item c anchor))]
        (if-not item
          (do (ctx/result! c {:collected (:collected (ctx/mem c) 0)})
              :done)
          (let [r (await (ctx/act c :collect #js {:id (.-id item)}))]
            (ctx/update-mem! c update :collected (fnil + 0) (gained-count r))
            (when (#{"unreachable" "timeout"} (.-status r))
              (ctx/update-mem! c update :skipped (fnil conj []) (.-id item)))
            (recur)))))))
