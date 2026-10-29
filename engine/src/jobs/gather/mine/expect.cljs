(ns jobs.gather.mine.expect
  "What a mine or dig-for call is expected to do from where the body stands, for its start check (safe?): the seconds it
  stays at its place (stay-s) and the torches it hangs (torch-way)."
  (:require [jobs.gather.mine.descent :as descent]
            [jobs.gather.mine.targets :as targets]
            [jobs.gather.mine.way-set :as mw]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look :refer [cell-of]]
            [jobs.lib.stair-time :as stair-time]
            [jobs.lib.torch-stock :as stock]
            [jobs.lib.util :as u]
            [jobs.lib.work-place :as work-place]))

(defn run
  "{:seen [pos] :left n :down n :level n}: the seen blocks of its kinds within :radius it digs first (:seen a way; nearest
  first, at most the count owed), the items :left after them (one a block), and for those the steps :down to the tunnel
  level (a descending way; through soil to stone, at most :descend-limit, for a stone-type block with no level) and the
  blocks of strip tunnel (:strip-tunnel) that lay that many bare (cost/tunnel-ore at the level), at most :tunnel-length."
  [c]
  (let [args (:args c)
        owed (targets/owed c)
        seen (if (and (mw/seen? args) (pos? owed))
               (mapv :pos (look/seen-blocks (:primitives c) {:names (vec (targets/kinds args)) :radius (or (:radius args) work-place/dig-radius) :max owed :live? true}))
               [])
        left (- owed (count seen))
        feet (:y (cell-of (u/self-pos c)))
        block (targets/main-block args)
        level (when block (descent/tunnel-level args))
        down (cond (not (and (pos? left) (mw/descends? args))) 0
                   (and level (mw/tunnel? args)) (max 0 (- feet level))
                   (and (nil? level) (contains? descent/stone-types block)) (or (:descend-limit args) 0)
                   :else 0)
        per-block (some-> block vector cost/ore-of (cost/tunnel-ore {:y (- feet down)} 1))]
    {:seen seen :left left :down down
     :level (if (and (pos? left) (mw/tunnel? args))
              (min (:tunnel-length args) (if (and per-block (pos? per-block)) (js/Math.ceil (/ left per-block)) ##Inf))
              0)}))

(defn stay-s
  "Seconds the call stays at its place (run): the walk from seen block to seen block and each one's dig (cost/dig-cost),
  the stair down at stair-time's seconds a step (a walk down a mineshaft without :descend-stair), the tunnel at
  stair-time's, and the dig of each item it lays bare."
  [c]
  (let [p (:primitives c)
        {:keys [seen left down level]} (run c)
        dig-s #(or (some-> % cost/dig-cost) 0)]
    (+ (* (cost/walk-step-s) (reduce + 0 (map u/dist (cons (u/self-pos c) seen) seen)))
       (reduce + 0 (map #(dig-s (u/seen-name p %)) seen))
       (if (mw/stair? (:args c)) (stair-time/steps-s :down down) (* down (cost/walk-step-s)))
       (stair-time/steps-s :level level)
       (if (pos? level) (* left (dig-s (targets/main-block (:args c)))) 0))))

(defn torch-way
  "The torch way (ready/dark-near-ok) of a call with :torches: one torch at the spot of its seen digs (torches/spot-torch!, priced as a stretch of its own: torch-gap blocks)
  and the blocks of the stair it cuts and of its tunnel (run), more torches by its own get; nil when it hangs none."
  [c]
  (let [{:keys [seen down level]} (run c)
        n (+ (if (seq seen) cost/torch-gap 0) (if (mw/stair? (:args c)) down 0) level)]
    (when (and (:torches (:args c)) (pos? n))
      (stock/torch-way c n (stock/fetch-allows? c)))))
