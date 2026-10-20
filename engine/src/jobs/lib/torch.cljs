(ns jobs.lib.torch
  "Hanging a torch in a straight 1-wide run, shared by jobs.access.tunnel and jobs.gather.mine."
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.lib.placement :as placement]
            [jobs.lib.shelter.sense :as sense]))

(def torch-blocks #{"torch" "wall_torch"})

(defn torches-carried [p]
  (reduce + (map :count (filter #(= "torch" (:name %)) (u/inventory p)))))

(defn fuel-held
  "How many coal and charcoal are carried: each makes 4 torches with a stick."
  [p]
  (reduce + (map :count (filter #(#{"coal" "charcoal"} (:name %)) (u/inventory p)))))

(defn stick-supply
  "How many sticks the body has or can make from carried planks (2 planks make 4 sticks): each torch crafting needs one."
  [p]
  (reduce + (map (fn [{n :name c :count}]
                   (cond (= "stick" n) c
                         (re-find #"_planks$" n) (* 2 c)
                         :else 0))
                 (u/inventory p))))

(defn off-hand-torches
  "The torches in the off hand (the place act takes only the inventory's)."
  [p]
  (let [o (some-> (.self p) .-equipment .-offHand)]
    (if (= "torch" (some-> o .-name)) (or (.-count o) 1) 0)))

(defn torches-held [p] (+ (torches-carried p) (off-hand-torches p)))

(defn ^:async off-hand-to-inventory!
  "With no torch in the inventory but some in the off hand (the off-hand torch aid moves the whole stack), move them
  back so the place act finds them: true when the inventory holds a torch now."
  [c]
  (let [p (:primitives c)]
    (when (and (zero? (torches-carried p)) (pos? (off-hand-torches p)))
      (await (ctx/act c :unequip #js {:dest "off-hand"})))
    (pos? (torches-carried p))))

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

(defn ^:async place!
  "Place the torch of choice (torch-at's) from the body's place; true when the cell holds a torch after (placed, or
  already there). block-at: a fn of a cell, the block name seen there."
  [c choice block-at]
  (let [cell (:cell choice)]
    (await (off-hand-to-inventory! c))
    (let [r (await (ctx/act c :place (clj->js {:pos (zipmap [:x :y :z] cell) :item "torch"
                                               :click (placement/js-click (:click choice))})))]
      (boolean (or (= "placed" (.-status r)) (torch-blocks (block-at cell)))))))

(defn perceived-dark?
  "Whether the body counts the cell [x y z] dark: not sense/lit-here? (an unseen cell by the open-sky guess)."
  [p [x y z]]
  (not (sense/lit-here? p {:x x :y y :z z})))
