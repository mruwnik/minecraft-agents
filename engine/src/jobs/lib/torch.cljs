(ns jobs.lib.torch
  "Hanging a torch in a straight 1-wide run, shared by jobs.access.tunnel and jobs.gather.mine."
  (:require [jobs.lib.util :as u]
            [jobs.lib.placement :as placement]))

(def torch-blocks #{"torch" "wall_torch"})

(defn torches-carried [p]
  (reduce + (map :count (filter #(= "torch" (:name %)) (u/inventory p)))))

(defn off-hand-torches
  "The torches in the off hand (the place act takes only the inventory's)."
  [p]
  (let [o (some-> (.self p) .-equipment .-offHand)]
    (if (= "torch" (some-> o .-name)) (or (.-count o) 1) 0)))

(defn torches-held [p] (+ (torches-carried p) (off-hand-torches p)))

(defn side-dirs
  "The unit steps to the left and to the right of a run going [dx dz]."
  [[dx dz]]
  [[dz 0 (- dx)] [(- dz) 0 dx]])

(defn facing-name [d] (some (fn [[n v]] (when (= v d) n)) placement/steps))

(defn torch-at
  "How to hang a torch for the cell feet ([x y z]) of a run going dir ([dx dz]) from the eye: a wall torch in the head
  cell on the first side wall (left, then right of the run) that takes it, else a floor torch in the feet cell:
  {:cell :block :click}, or {:refused :no-support}. ok? (default: any), a fn of a cell, leaves out the cells it refuses."
  ([dir feet eye block-at] (torch-at dir feet eye block-at (constantly true)))
  ([dir [x y z :as feet] eye block-at ok?]
   (let [head [x (inc y) z]
         world (fn [cell] (some->> (block-at cell) (hash-map :name)))
         wall (fn [d] (assoc (placement/click {:block "wall_torch" :facing (facing-name (mapv - d))} head eye world)
                             :cell head :block "wall_torch"))
         floor (assoc (placement/click "torch" feet eye world) :cell feet :block "torch")]
     (or (when (ok? head) (first (remove :refused (map wall (side-dirs dir)))))
         (when (and (ok? feet) (not (:refused floor))) floor)
         {:refused :no-support}))))
