(ns jobs.movement.go-to.turn-back
  "The unseen-floor turn-back gate, shared by go-to's walk (walk/turn-back-check) and its quote (preview): a plan whose
  unseen floors cost more to turn back from than the call's worth is refused by both."
  (:require [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.escape :as escape]
            [jobs.lib.escape.way :as escape-way]
            [jobs.lib.look :as look]
            [jobs.lib.walk.one-way :as one-way]
            [jobs.lib.walk.sight :as wsight]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.cut :as cut :refer [tour]]
            [jobs.movement.go-to.detour :as detour]))

(defn cell-of [{:keys [x y z]}] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])

(defn climb-back
  "[i j] of steps: i its first drop the body cannot jump back up (2 blocks or more), j the first step after it back at
  the rim's height; nil when the plan has no such drop or stays down."
  [steps]
  (when-let [i (first (filter #(>= (- (:y (steps (dec %))) (:y (steps %))) 2) (range 1 (count steps))))]
    (some->> (range (inc i) (count steps)) (filter #(>= (:y (steps %)) (:y (steps (dec i))))) first (vector i))))

(defn shut-in
  "{:p :trap-s :cells} of steps' drop i (climb-back) onto a landing beside a seen solid side (a pit or trench, not open
  ground with other ways on) when the plan's own way back up from it (to step j) passes feet or head cells the body has
  not seen: :p the chance one of them is solid
  (look/unseen-solid-at-fn), which shuts the body in at the landing, no more than one-way/chance of a landing seen lit
  (or a body carrying a light), where the way on is seen on landing; :trap-s what that costs,
  the climb back out (one-way/climb-out-s) when escape-way/choose finds a way to make there and it is finite, else (nothing it may dig)
  the call's worth spent shut in and the goal lost (left). nil when there is no such drop or no unseen cell."
  [p steps [i j] worth left]
  (when i
    (let [rim (steps (dec i))
          landing (steps i)
          unseen? (escape/unseen-of p)
          cells (into [] (comp (mapcat (fn [s] (let [[x y z] (cell-of s)] [[x y z] [x (inc y) z]]))) (distinct) (filter unseen?))
                      (subvec steps (inc i) (inc j)))]
      (when (and (seq cells) (escape/walled-side? (let [block-at (escape/block-at-of p)] #(when-not (unseen? %) (block-at %))) (cell-of landing)))
        (let [solid-at (look/unseen-solid-at-fn p)
              way (escape-way/choose p (cell-of landing) (cell-of (peek steps)))]
          {:p (let [p-solid (- 1 (transduce (map #(- 1 (solid-at %))) * 1 cells))
                    landing-cell (zipmap [:x :y :z] (cell-of landing))]
                (if (one-way/lit-landing? p landing-cell) (min p-solid (one-way/chance p landing-cell false)) p-solid))
           :trap-s (let [out-s (when-not (= :none (:step way)) (one-way/climb-out-s p rim landing))]
                     (if (and out-s (js/isFinite out-s)) out-s (+ worth left)))
           :cells (vec (take 6 cells))})))))

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
  weighed by its chance again. :p is the first floor's chance. A drop whose way on rests on unseen cells (shut-in) adds
  its :trap-s at its :p, given as :shut."
  [c steps]
  (let [steps (vec steps)
        back-up (climb-back steps)]
    (when (or back-up (some :unseen-floor steps))
      (let [p (:primitives c)
            worth (cost/tour-worth (:detour-from (ctx/mem c) 0) (ctx/worth c))
            left (max 0 (- worth (:cost (cost/tour-cost (tour c)))))
            shut (shut-in p steps back-up worth left)]
        (when (or shut (some :unseen-floor steps))
          (let [counts (cut/floor-counts c)
                {:keys [peek-s aim-s]} (cost/walk-unknown-prices)
                peek-depth (wworld/peek-depth-fn p)
                self (.self p)
                equipment (cost/equipment-of (.-equipment self))
                fall {:equipment equipment :carried (:count (escape/carried-pillar-item p) 0)
                      :share (wworld/unseen-share p)
                      :max-drop (cost/drop-limit equipment (wworld/damage-body c) (wworld/walk-settings c))}
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
            (cond-> {:cost (+ (:cost (cost/turn-back-cost floors)) (if shut (cost/chance-times (:p shut) (:trap-s shut)) 0))
                     :worth worth
                     :p (:p (first floors))
                     :cells (vec (take 6 (or (seq (map :cell floors)) (:cells shut))))}
              shut (assoc :shut shut))))))))

(defn exceeded
  "The turn-back (turn-back) of steps when its cost is more than its worth, else nil."
  [c steps]
  (when-let [{:keys [cost worth] :as t} (turn-back c steps)]
    (when (> cost worth) t)))
