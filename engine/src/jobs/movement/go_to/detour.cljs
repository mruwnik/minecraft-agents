(ns jobs.movement.go-to.detour
  "go-to's detour bound: a found way under ground whose walk, over cells the body has never seen priced as the walk
  prices them, costs more past the straight line than its goal is worth, or more than a dug way to a sealed goal, is
  weighed against a dug way first; with none, it is walked only when that walk costs no more than the goal is worth."
  (:require [jobs.lib.cost :as cost]))

(defn length
  "Blocks along the steps' feet cells ({:x :y :z}), up and down counted."
  [steps]
  (reduce + (map (fn [a b] (js/Math.hypot (- (:x b) (:x a)) (- (:y b) (:y a)) (- (:z b) (:z a)))) steps (rest steps))))

(defn sealed-blocks
  "Walk blocks that take as long as digging straight blocks through a goal's sealed surroundings, clear-ms the ms each
  block of them takes to break (cost/dig-cells-s): a loop longer than that costs more than the dug way."
  [straight clear-ms]
  (/ (cost/dig-cells-s (repeat (js/Math.ceil straight) clear-ms)) (cost/walk-step-s)))

(defn priced-length
  "Blocks of walk a way's steps cost: their length and the unseen cells' toll (cost/unseen-route-s at share, the walk's
  seconds the unseen price adds) in walked blocks; [blocks unseen-blocks]."
  [steps share unseen?]
  (let [cells (map (fn [{:keys [x y z]}] {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)}) steps)
        toll (/ (cost/unseen-route-s share (fn [x y z] (if (unseen? x y z) :ground :seen)) cells) (cost/walk-step-s))]
    [(+ (length steps) toll) toll]))

(defn price
  "{:extra-s :worth-s} of a refused way (refusal's :route and :straight): the walk past the straight line (cost/walk-cost)
  and what the goal is worth (cost/tour-worth of straight and the call's `worth`), tenths of a second."
  [{:keys [route straight]} worth]
  (let [tenths #(/ (js/Math.round (* 10 %)) 10)]
    {:extra-s (tenths (:cost (cost/walk-cost {:distance (max 0 (- route straight))}))) :worth-s (tenths (cost/tour-worth straight worth))}))

(defn refusal
  "{:detour true :route :blocks :straight :allowed} (whole blocks) of a way, steps, to a goal straight blocks off, or nil.
  :route is its priced-length (blocks of walk, its unseen cells (unseen? (fn [x y z])) priced at share, walk/world
  unseen-share), :blocks its plain length. With a dug way (dug-blocks: sealed-blocks, then :allowed) it is refused when
  the priced length passes it; without, when its walk past the straight line (price) costs more than the goal is worth
  (worth, seconds)."
  [steps straight unseen? share dug-blocks worth]
  (let [[priced] (priced-length steps share unseen?)
        why {:detour true :route (js/Math.round priced) :blocks (js/Math.round (length steps)) :straight (js/Math.round straight)}
        {:keys [extra-s worth-s]} (price why worth)]
    (when (or (and dug-blocks (> priced dug-blocks)) (> extra-s worth-s))
      (cond-> why dug-blocks (assoc :allowed (js/Math.round dug-blocks))))))
