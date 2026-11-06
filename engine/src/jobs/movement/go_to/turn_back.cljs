(ns jobs.movement.go-to.turn-back
  "The unseen-floor turn-back gate, shared by go-to's walk (walk/turn-back-check) and its quote (preview): a plan whose
  unseen floors cost more to turn back from than the call's worth is refused by both."
  (:require [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.walk.sight :as wsight]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.cut :as cut :refer [tour]]
            [jobs.movement.go-to.detour :as detour]))

(defn turn-back
  "{:cost :worth :p :cells} of a plan's floors the body has not seen (steps with :unseen-floor; nil when none): each
  priced by cost/turn-back-cost, its look a peek over a drop or an aim (cost/walk-unknown-prices), its walk back the blocks
  from the plan's start to its takeoff (a step down whose peek sees a floor further under it, wworld/peek-depth-fn, may
  take that fall and climb back out instead, the cheaper: cost/unseen-fall-s with that walk back as :else-s, whatever the
  call's :ways), its chance of not being there cost/floor-absent-chance of its kind
  (wsight/prior-kind) over the call's floors of each kind found there or not (cut/floor-counts). :worth is cost/tour-worth
  of the goal's straight blocks at the call's start and the call's worth (ctx/worth). No second way is known when the plan is
  made: a floor not there costs, when no lower step has one either (cost/floor-dead-end-chance), the worse case of
  finding one, the tour that worth still pays for (less cost/tour-cost of the call's tour so far), whose end without one
  loses the goal; a lower step's replan adds nothing. A floor not there costs no more than that, so the worth is not
  weighed by its chance again. :p is the first floor's chance."
  [c steps]
  (when (some :unseen-floor steps)
    (let [steps (vec steps)
          counts (cut/floor-counts c)
          {:keys [peek-s aim-s]} (cost/walk-unknown-prices)
          p (:primitives c)
          peek-depth (wworld/peek-depth-fn p)
          self (.self p)
          equipment (cost/equipment-of (.-equipment self))
          fall {:health (.-health self) :equipment equipment :carried (:count (escape/carried-pillar-item p) 0)
                :share (wworld/unseen-share p)
                :max-drop (cost/drop-limit equipment (wworld/damage-body c) (wworld/walk-settings c))}
          worth (cost/tour-worth (:detour-from (ctx/mem c) 0) (ctx/worth c))
          left (max 0 (- worth (:cost (cost/tour-cost (tour c)))))
          backs (vec (reductions + 0 (map #(detour/length [%1 %2]) steps (rest steps))))
          floors (keep-indexed (fn [i s]
                                 (when (:unseen-floor s)
                                   (let [kind (wsight/floor-kind steps i)
                                         prior-kind (wsight/prior-kind (wsight/step-rise steps i))
                                         drop-by (js/Math.round (- (:y (steps (dec i))) (:y s)))
                                         back (get backs (max 0 (dec i)))
                                         extra-s (cost/chance-times (cost/floor-dead-end-chance kind counts) left)]
                                     {:absent-s (when (#{:down :drop} kind)
                                                  (cost/unseen-fall-s prior-kind drop-by (assoc fall :see-depth (peek-depth (js/Math.floor (:x s)) (js/Math.floor (:y s)) (js/Math.floor (:z s)))
                                                                                                :else-s (+ (:cost (cost/walk-cost {:distance back})) extra-s))))
                                      :look (if (#{:down :drop} kind) peek-s aim-s) :back back
                                      :p (cost/floor-absent-chance prior-kind (get counts prior-kind))
                                      :extra-s extra-s
                                      :cell [(js/Math.floor (:x s)) (dec (js/Math.floor (:y s))) (js/Math.floor (:z s))]})))
                               steps)]
      {:cost (:cost (cost/turn-back-cost floors))
       :worth worth
       :p (:p (first floors))
       :cells (vec (take 6 (map :cell floors)))})))

(defn exceeded
  "The turn-back (turn-back) of steps when its cost is more than its worth, else nil. Not cost/affords?: the cost is
  bounded by its worth and ##Inf at an arrive worth, which must still walk on."
  [c steps]
  (when-let [{:keys [cost worth] :as t} (turn-back c steps)]
    (when (> cost worth) t)))
