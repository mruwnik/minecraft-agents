(ns jobs.lib.stair-price
  "The seconds of the ways up that go-to's escalations, a buried body's climb (surface-up) and a one-way drop's climb back
  out (walk.one-way) price: one composition of a stair step and of a pillar."
  (:require [jobs.lib.cost :as cost]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.escape :as escape]
            [jobs.lib.walk.world :as wworld]))

(defn unseen-step-s
  "Seconds a step into a cell not seen takes on top (cost/unseen-route-s at the share of the world the body has not seen)."
  [p]
  (cost/unseen-route-s (wworld/unseen-share p) (constantly :ground) [{:x 0 :y 0 :z 0}]))

(defn leg-pricer
  "A fn [cuts-s fill-s unseen?] of the seconds one stair step takes from p's body: cw/stair-step-cost of a walk step, the
  seconds of each cut (nil for an open cell), a fill of the floor when fill-s is given, and the unseen step price when unseen?."
  [p]
  (let [step-s (:cost (cost/walk-cost {:distance 1}))
        unseen-s (unseen-step-s p)]
    (fn [cuts-s fill-s unseen?]
      (cw/stair-step-cost step-s (cw/stair-cost 1) (cond-> (vec cuts-s) fill-s (conj fill-s)) (if unseen? unseen-s 0)))))

(defn pillar-s
  "Seconds the pillar of height blocks up from feet takes: cw/pillar-cost, and the unseen step price for each block whose
  cell or the two over it is unseen."
  [p feet height]
  (let [unseen? (escape/unseen-of p)
        unseen-s (unseen-step-s p)]
    (+ (cw/pillar-cost height)
       (* unseen-s (count (filter #(some unseen? (map (fn [dy] (escape/up feet (+ % dy))) [1 2])) (range height)))))))
