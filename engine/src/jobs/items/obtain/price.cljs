(ns jobs.items.obtain.price
  "What the call's shortfall costs the body to get at least: the cheapest way that would start, a chest's or loose item's walk with its risk (obtain.plan/walk-s), a gather's walks straight."
  (:require [engine.ctx :as ctx]
            [jobs.items.obtain.base :refer [gathers? names-of shortfall]]
            [jobs.items.obtain.need :refer [climbs-first? explorable-needs gather-viable?]]
            [jobs.items.obtain.plan :refer [chest-s craft-s gather-plan gathered-s loose-s]]
            [jobs.lib.cost :as cost]
            [jobs.lib.need :as need]
            [jobs.lib.surface-up :as surface]))

(defn gather-s
  "A gather of seen raw needs: obtain.plan/gathered-s."
  [c names n]
  (when (and (gathers? c) (gather-viable? c names n))
    (gathered-s c (gather-plan c names n))))

(defn search-s
  "A search for an unseen raw need: a trip (each walk of it is priced against the need's worth as it goes, go-to's quote with its risk), and the stair up first when the gather climbs before searching (need/climbs-first?: a tree for a body under rock, surface-up/climb-s)."
  [c names n]
  (when (gathers? c)
    (when-let [needs (seq (explorable-needs c names n))]
      (+ (cost/trip) (if (some #(climbs-first? c %) needs) (surface/climb-s c) 0)))))

(defn price-of
  "Seconds getting the call's shortfall of its :item or :any-of costs the body: 0 when carried, else the cheapest way that would
  start (a chest to try, a loose item, a craft from what is carried, a gather of seen needs, a search), ##Inf when none would."
  [c]
  (let [names (names-of (:args c))
        n (shortfall c names)]
    (if (zero? n)
      0
      (apply min ##Inf (keep identity [(chest-s c names) (loose-s c names) (craft-s c names n) (gather-s c names n) (search-s c names n)])))))

(defn item-price
  "price-of the jobs.items.obtain child need/item-status would ask for n more of item under options o, with the light
  judged under the sky's darkening sky (nil: now; 0: full day)."
  [c item n o sky]
  (let [[_ args] (ctx/child-job c 'jobs.items.obtain (cond-> (need/fetch-args 'jobs.items.obtain {:item item :count (+ n (get (need/counts c [item]) item))} o)
                                                        sky (assoc :sky sky)))]
    (price-of (assoc c :slots (conj (:slots c) (or (:slot o) :need)) :args args))))
