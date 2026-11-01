(ns jobs.village.roll-gap
  "The slab over a workstation cell (a lectern under a top slab keeps the villager in and the body out) lifted for a drop that
  slid into the villager's cell, out of pickup reach from both sides, and put back."
  (:require [engine.ctx :as ctx]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.placement :as placement]
            [jobs.lib.util :as u]))

(defn above [pos] (update pos :y inc))

(defn ^:async run!
  "A dig or place child in its own slot: ::ok when it is done, :continue, or the stop."
  [c slot job cargs]
  (let [r (await (ctx/call c slot job (cond-> cargs (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
    (cond (= :continue r) :continue
          (= :done (:kind r)) ::ok
          :else r)))

(defn ^:async lift!
  "Dig the slab above pos (once per call) so the body can stand in the cell and take a drop from anywhere in the villager's
  cell; the slab's name and type are booked in mem :slab first and the drop search starts afresh. :again, :continue, a stop,
  or nil when there is no slab or it was lifted already."
  [c pos n]
  (let [b (when-not (:slab (ctx/mem c)) (u/seen-block (:primitives c) (above pos)))]
    (when (and b (re-find #"_slab$" (.-name b)))
      (ctx/update-mem! c assoc :slab {:name (.-name b) :type (some-> b .-properties (aget "type"))} :reach-drops 0)
      (let [r (await (run! c (keyword (str "lift-slab-" n)) 'jobs.blocks.dig {:pos (above pos) :collect true}))]
        (if (= ::ok r) :again r)))))

(defn ^:async restore!
  "Place the lifted slab back in its cell with its type (from the stand the body has left the workstation cell for). :again
  when it is placed or can no longer be (the cell is filled, the slab is not carried), :continue, or the place child's stop."
  [c pos n]
  (let [{:keys [name type]} (:slab (ctx/mem c))
        p (:primitives c)
        up (above pos)
        here (u/seen-name p up)]
    (if (or (nil? here) (not (bs/air here)) (zero? (u/carried-count p name)))
      (do (ctx/update-mem! c dissoc :slab) :again)
      (let [click (:click (placement/click {:block name :type type} [(:x up) (:y up) (:z up)] (placement/eye (u/self-pos c)) (u/seen-block-at-fn p)))
            r (await (run! c (keyword (str "restore-slab-" n)) 'jobs.blocks.place
                           (cond-> {:pos up :item name :through false} click (assoc :click click))))]
        (if (= ::ok r)
          (do (ctx/update-mem! c dissoc :slab) :again)
          r)))))
