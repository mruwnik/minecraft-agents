(ns jobs.movement.go-to.detour
  "go-to's detour bound: a found way under ground many times longer than its goal is far, mostly over cells the body has
  never seen (a cave the planner knows of and the body does not), is weighed against a dug way first; with none, it is
  walked only when its walk past the straight line costs no more than the goal is worth."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]))

(a/defargs settings
  {::ratio {:default 4 :doc "Times the distance to its goal at the call's start that a go-to's found way under ground may be long, when most of it is unseen, before a dug way is weighed." :spec (a/num-in 1 nil)}
   ::unseen-share {:default 0.5 :doc "The share of a way's cells the body has never seen from which the way counts as unseen." :spec (a/num-in 0 1)}
   ::floor {:default 48 :doc "Blocks a go-to's found way under ground may always be long, however near the goal, before a dug way is weighed." :spec (a/num-in 0 nil)}})

(defn ratio [] (settings/get settings ::ratio))
(defn floor [] (settings/get settings ::floor))
(defn unseen-share [] (settings/get settings ::unseen-share))

(defn length
  "Blocks along the steps' feet cells ({:x :y :z}), up and down counted."
  [steps]
  (reduce + (map (fn [a b] (js/Math.hypot (- (:x b) (:x a)) (- (:y b) (:y a)) (- (:z b) (:z a)))) steps (rest steps))))

(defn allowed
  "Blocks a way to a goal straight blocks off at the call's start may be long."
  [straight]
  (max (floor) (* (ratio) straight)))

(defn refusal
  "{:detour true :route :straight :allowed} (whole blocks) when steps are longer than allowed for a goal straight blocks
  off and at least ::unseen-share of their cells answer unseen? (fn [x y z]); else nil."
  [steps straight unseen?]
  (let [route (length steps)
        unseen (count (filter (fn [{:keys [x y z]}] (unseen? (js/Math.floor x) (js/Math.floor y) (js/Math.floor z))) steps))]
    (when (and (> route (allowed straight)) (>= unseen (* (unseen-share) (count steps))))
      {:detour true :route (js/Math.round route) :straight (js/Math.round straight) :allowed (js/Math.round (allowed straight))})))

(defn price
  "{:extra-s :worth-s} of a refused way (refusal's :route and :straight): the walk past the straight line (cost/walk-cost)
  and what the goal is worth (cost/tour-worth of straight and the caller's `worth`), tenths of a second."
  [{:keys [route straight]} worth]
  (let [tenths #(/ (js/Math.round (* 10 %)) 10)]
    {:extra-s (tenths (:cost (cost/walk-cost {:distance (max 0 (- route straight))}))) :worth-s (tenths (cost/tour-worth straight worth))}))
