(ns jobs.items.obtain.price
  "What the call's shortfall costs the body to get: the cheapest way that would start, each with the death its walk risks."
  (:require [engine.game :as game]
            [jobs.items.obtain.base :refer [craft-step-s gathers? makes? names-of shortfall smelt-item-s way?]]
            [jobs.items.obtain.chest :refer [candidates]]
            [jobs.items.obtain.loose :as loose-way]
            [jobs.items.obtain.need :refer [explorable? explore-opts gather-viable?]]
            [jobs.items.obtain.plan :refer [below? craft-plan gather-plan plan-cost]]
            [jobs.items.obtain.seen :refer [seen-of]]
            [jobs.items.obtain.table :refer [gather-needs]]
            [jobs.lib.cost :as cost]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]))

(defn steps-s
  "Seconds the craft and smelt steps of a plan take."
  [pl]
  (let [{smelts true crafts false} (group-by #(= :smelt (:op %)) (:steps pl))]
    (+ (* (craft-step-s) (count crafts)) (* (smelt-item-s) (reduce + 0 (map :count smelts))))))

(defn death-s
  "The death a walk to `at` risks past the known threats (jobs.lib.threats/route-death-cost): underground when under? (an ore below the feet), else over the
  surface at night while the open sky is dark."
  [c at under?]
  (threats/route-death-cost c {:underground? (boolean under?) :night? (sh-sense/dark-outside? (:primitives c))} at))

(defn walk-s
  "A trip, the walk to `at` and back, and the death it risks."
  [c at]
  (+ (cost/trip) (:cost (cost/walk-cost {:distance (* 2 (u/dist (u/self-pos c) at))})) (death-s c at false)))

(defn chest-s
  "The walk to the nearest chest the chest way would try: one booked as holding a name, or a seen one not looked into yet."
  [c names]
  (when (way? c :chest)
    (some->> (candidates c names) first :pos (walk-s c))))

(defn loose-s [c names]
  (when (way? c :loose)
    (some->> (loose-way/seen c names) first .-position u/pos-of (walk-s c))))

(defn craft-s [c names n]
  (when (makes? c)
    (some-> (craft-plan c names n) steps-s)))

(defn gather-s
  "A gather of seen raw needs: obtain's plan-cost, the craft steps and the death each walk risks."
  [c names n]
  (when (and (gathers? c) (gather-viable? c names n))
    (let [pl (gather-plan c names n)
          needs (gather-needs (:gather pl) (game/version-of (:primitives c)))]
      (+ (plan-cost c (game/version-of (:primitives c)) pl)
         (* (craft-step-s) (count (remove #(= :smelt (:op %)) (:steps pl))))
         (reduce + 0 (map #(death-s c (or (:pos (seen-of c %)) (u/self-pos c)) (below? c %)) needs))))))

(defn search-s
  "A search for an unseen raw need: the seconds it may spend and the death risked where the body stands."
  [c names n]
  (when (and (gathers? c) (explorable? c names n))
    (+ (get-in (explore-opts c) [:spend :job-time] 0) (death-s c (u/self-pos c) false))))

(defn price-of
  "Seconds getting the call's shortfall of its :item or :any-of costs the body: 0 when carried, else the cheapest way that would
  start (a chest to try, a loose item, a craft from what is carried, a gather of seen needs, a search), ##Inf when none would."
  [c]
  (let [names (names-of (:args c))
        n (shortfall c names)]
    (if (zero? n)
      0
      (apply min ##Inf (keep identity [(chest-s c names) (loose-s c names) (craft-s c names n) (gather-s c names n) (search-s c names n)])))))
