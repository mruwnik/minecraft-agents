(ns jobs.movement.go-to.detour
  "go-to's detour bound: a found way under ground many times longer than its goal is far, over cells the body has
  never seen (a cave the planner knows of and the body does not) priced as the walk prices them, is weighed against a dug way first; with none, it is
  walked only when its walk past the straight line costs no more than the goal is worth."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]))

(a/defargs settings
  {::ratio {:default 4 :doc "Times the distance to its goal at the call's start that a go-to's found way under ground may be long, with its unseen cells priced, before a dug way is weighed." :spec (a/num-in 1 nil)}
   ::floor {:default 48 :doc "Blocks a go-to's found way under ground may always be long, however near the goal, before a dug way is weighed." :spec (a/num-in 0 nil)}})

(defn ratio [] (settings/get settings ::ratio))
(defn floor [] (settings/get settings ::floor))

(defn length
  "Blocks along the steps' feet cells ({:x :y :z}), up and down counted."
  [steps]
  (reduce + (map (fn [a b] (js/Math.hypot (- (:x b) (:x a)) (- (:y b) (:y a)) (- (:z b) (:z a)))) steps (rest steps))))

(defn allowed
  "Blocks a way to a goal straight blocks off at the call's start may be long; dug-blocks (sealed-blocks) the walk
  blocks the dug way to a goal sealed by seen solid blocks costs, else nil."
  [straight dug-blocks]
  (or dug-blocks (max (floor) (* (ratio) straight))))

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

(defn refusal
  "{:detour true :route :straight :allowed} (whole blocks) when steps are longer than allowed for a goal straight
  blocks off, their unseen cells (unseen? (fn [x y z])) priced at share (walk/world unseen-share). With a dug way
  (dug-blocks: allowed's) it is refused when the priced-length passes it; without, when the steps' length
  passes the bound and the unseen cells' toll alone does too; else nil."
  ([steps straight unseen? share] (refusal steps straight unseen? share nil))
  ([steps straight unseen? share dug-blocks]
   (let [[priced toll] (priced-length steps share unseen?)
         route (length steps)
         ok (allowed straight dug-blocks)]
     (when (if dug-blocks (> priced ok) (and (> route ok) (> toll ok)))
       {:detour true :route (js/Math.round route) :straight (js/Math.round straight) :allowed (js/Math.round ok)}))))

(defn price
  "{:extra-s :worth-s} of a refused way (refusal's :route and :straight): the walk past the straight line (cost/walk-cost)
  and what the goal is worth (cost/tour-worth of straight and the call's `worth`), tenths of a second."
  [{:keys [route straight]} worth]
  (let [tenths #(/ (js/Math.round (* 10 %)) 10)]
    {:extra-s (tenths (:cost (cost/walk-cost {:distance (max 0 (- route straight))}))) :worth-s (tenths (cost/tour-worth straight worth))}))
