(ns jobs.items.obtain.price
  "What the call's shortfall costs the body to get: the cheapest way that would start, each with the death its walk risks."
  (:require [jobs.items.obtain.base :refer [gathers? names-of shortfall]]
            [jobs.items.obtain.need :refer [explorable? gather-viable?]]
            [jobs.items.obtain.plan :refer [chest-s craft-s death-s gather-plan gathered-s loose-s]]
            [jobs.lib.cost :as cost]
            [jobs.lib.util :as u]))

(defn gather-s
  "A gather of seen raw needs: obtain.plan/gathered-s."
  [c names n]
  (when (and (gathers? c) (gather-viable? c names n))
    (gathered-s c (gather-plan c names n))))

(defn search-s
  "A search for an unseen raw need: a trip (each walk of it is priced against the need's worth as it goes) and the death risked where the body stands."
  [c names n]
  (when (and (gathers? c) (explorable? c names n))
    (+ (cost/trip) (death-s c (u/self-pos c)))))

(defn price-of
  "Seconds getting the call's shortfall of its :item or :any-of costs the body: 0 when carried, else the cheapest way that would
  start (a chest to try, a loose item, a craft from what is carried, a gather of seen needs, a search), ##Inf when none would."
  [c]
  (let [names (names-of (:args c))
        n (shortfall c names)]
    (if (zero? n)
      0
      (apply min ##Inf (keep identity [(chest-s c names) (loose-s c names) (craft-s c names n) (gather-s c names n) (search-s c names n)])))))
