(ns jobs.movement.go-to.climb-route
  "A shut-in body's climb to a goal above it: the cheapest route of digs, laid floors and pillar blocks
  (escalate-choose/route-answer), run as its ways (route-ways/segments)."
  (:require [jobs.lib.cost.ways :as cw]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.escape :as escape]
            [jobs.movement.go-to.escalate-choose :as choose]
            [jobs.movement.go-to.route-ways :as route-ways]))

(defn climb-way
  "The way up for a shut-in body at feet to goal above it, its stair, pillar and dig route allowed (not in opts :skip):
  the first of the climb's segments, the rest its :then, its pillars and laid floors of the carried block (opts
  :pillar-item), as many as are carried (go-to's pillar fetches none); {:step :searching} while the search goes on; nil when none
  reaches the goal or its first way is one that failed (opts :skip)."
  [p feet goal may-dig? {:keys [own? skip fetch? pillar-item] :as opts}]
  (when (and (> (goal 1) (feet 1)) (not-any? skip [:stair :pillar :dig-route]))
    (let [n (:count pillar-item 0)
          a (choose/route-answer p feet goal may-dig? own? false fetch?
                                 (cond-> (choose/search-opts opts)
                                   (pos? n) (assoc :place-s (cw/fill-cost n) :pillar-s (cw/pillar-cost 1) :max-places n)))]
      (cond
        (= :searching a) {:step :searching}
        (seq (:steps a))
        (let [[e & then] (map #(cond-> % (= :pillar (:step %)) (assoc :item (:item pillar-item)))
                              (route-ways/segments (escape/block-at-of p) #(dig-look/unknown? p %) feet (:steps a)))]
          ;; a first way that failed here before (skip-key) would fail again: the other ways are tried instead
          (when (and e (not (skip (if (= :stair (:step e)) [:stair (:heading e)] (:step e)))))
            (assoc e :then (vec then))))))))
