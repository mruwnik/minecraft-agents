(ns jobs.lib.step-off
  "Leaving a cell the body stands in so something can be put or dug there: one go-to to the nearest cell within two
  blocks that can be stood on and is no hazard, never the column of the cell itself."
  (:require [engine.ctx :as ctx]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

(def reach-blocks 2)

(defn hazard-at?
  "Whether the feet or head cell holds fire, lava or another block a body must not stand in."
  [p {:keys [x y z]}]
  (some #(contains? reach/hazard-blocks (u/block-name p {:x x :y % :z z})) [y (inc y)]))

(defn candidates
  "The cells {:x :y :z} to stand on within reach (default two) blocks (feet one down, level or one up) of the column of cell, nearest
  first, level before a step. Left out: standable-cell? says no, a hazard at feet or head, a member of avoid (a set of
  [x y z]), or one ok? (a fn of the cell, default all) refuses."
  [p {:keys [x y z]} {:keys [avoid ok? reach] :or {avoid #{} ok? (constantly true) reach reach-blocks}}]
  (let [r (range (- reach) (inc reach))]
    (->> (for [dx r dz r dy [0 1 -1]
               :when (not (and (zero? dx) (zero? dz)))]
           {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
         (sort-by (fn [c] [(+ (* (- (:x c) x) (- (:x c) x)) (* (- (:z c) z) (- (:z c) z))) (js/Math.abs (- (:y c) y))]))
         (filter #(and (reach/standable-cell? p %) (not (hazard-at? p %))
                       (not (contains? avoid [(:x %) (:y %) (:z %)])) (ok? %))))))

(defn ^:async step-off!
  "Walk off the column of cell (a go-to child :step-off, range 0, no escalation). opts {:avoid :ok? :reach} as candidates.
  Resolves to :arrived, :continue (go-to waits on the world) or {:unreachable why}: no cell to go to (:no-cell) or go-to's."
  [c cell opts]
  (if-let [to (first (candidates (:primitives c) cell opts))]
    (let [r (await (ctx/call-child c :step-off 'jobs.movement.go-to {:pos to :range 0 :escalate false}))
          res (ctx/child-result c :step-off)]
      (cond
        (= :continue r) :continue
        (and (= :done r) (:arrived res)) :arrived
        :else {:unreachable (if (= :done r) (:why res :unreachable) :declined)}))
    {:unreachable :no-cell}))
