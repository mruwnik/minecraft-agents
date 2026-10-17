(ns jobs.movement.go-to.detour
  "go-to's detour bound: a body under ground does not walk a found way many times longer than its goal is far, mostly
  over cells it has never seen (a cave the planner knows of and the body does not), unless the caller says so."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::ratio {:default 4 :doc "Times the distance to its goal at the call's start that a go-to's found way under ground may be long, when most of it is unseen." :spec (a/num-in 1 nil)}
   ::unseen-share {:default 0.5 :doc "The share of a way's cells the body has never seen from which the way counts as unseen." :spec (a/num-in 0 1)}
   ::floor {:default 48 :doc "Blocks a go-to's found way under ground may always be long, however near the goal." :spec (a/num-in 0 nil)}})

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
