(ns jobs.items.obtain.price
  "What the call's shortfall costs the body to get at least: the cheapest way that would start, a chest's or loose item's walk with its risk (obtain.plan/walk-s), a gather's walks straight."
  (:require [engine.ctx :as ctx]
            [jobs.items.obtain.base :refer [gathers? names-of shortfall]]
            [jobs.items.obtain.need :refer [climbs-first? explorable-needs gather-viable?]]
            [jobs.items.obtain.plan :refer [chest-s craft-s gather-plan gathered-s loose-s seen-walk-s]]
            [jobs.lib.cost :as cost]
            [jobs.lib.game-facts :as facts]
            [jobs.lib.util :as u]
            [jobs.lib.need :as need]
            [jobs.lib.surface-up :as surface]))

(defn gather-s
  "A gather of seen raw needs: obtain.plan/gathered-s."
  [c names n]
  (when (and (gathers? c) (gather-viable? c names n))
    (gathered-s c (gather-plan c names n))))

(defn search-s
  "A search for unseen raw needs: what getting them usually costs, their goal worth (cost/worth-to: the measured gathers,
  search and walks in them; each walk is priced against the call's worth as it goes, go-to's quote with its risk), and
  the stair up first when the gather climbs before searching (need/climbs-first?: a tree for a body under rock,
  surface-up/climb-s)."
  [c names n]
  (when (gathers? c)
    (when-let [needs (seq (explorable-needs c names n))]
      (+ (reduce + (map (fn [{:keys [key count]}] (* count (cost/worth-to nil (if (= "log" key) facts/log-names [key]) (u/self-pos c))))
                        needs))
         (if (some #(climbs-first? c %) needs) (surface/climb-s c) 0)))))

(defn price-of
  "Seconds getting the call's shortfall of its :item or :any-of costs the body: 0 when carried, else the cheapest way that would
  start (a chest to try, a loose item, a craft from what is carried, a gather of seen needs, a search), ##Inf when none would."
  [c]
  (let [names (names-of (:args c))
        n (shortfall c names)]
    (if (zero? n)
      0
      (apply min ##Inf (keep identity [(chest-s c names) (loose-s c names) (craft-s c names n) (gather-s c names n) (search-s c names n)])))))

(defn gather-walk-s
  "The longest walk to a seen need when a gather of seen needs is the cheapest way (price-of), else nil."
  [c]
  (let [names (names-of (:args c))
        n (shortfall c names)]
    (when (and (pos? n) (gathers? c) (gather-viable? c names n)
               (= (gather-s c names n) (price-of c)))
      (seen-walk-s c (gather-plan c names n)))))
