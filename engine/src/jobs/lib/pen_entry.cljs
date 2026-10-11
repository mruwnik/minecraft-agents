(ns jobs.lib.pen-entry
  "The gates of the pens that hold animals near the body: a walk made for a block job keeps out of them (go-to :keep-out),
  because a gate opened for a body lets the crowd out. A pen is the closed enclosure (jobs.lib.pen/check) round a passive
  animal in sight, or a pen an open gate leaks from; its gates are all of the pen's."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.combat :as combat]
            [jobs.lib.pen :as pen]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::radius {:default 24 :doc "Blocks round the body in which animals are looked for to find the pens whose gates a block job's walk keeps out of." :spec (a/int-in 1 nil)}
   ::max-animals {:default 12 :doc "The nearest animals checked for a pen." :spec (a/int-in 1 nil)}
   ::max-cells {:default 400 :doc "Most cells searched for one pen." :spec (a/int-in 1 nil)}})

(defn animal-cell [e]
  (let [{:keys [x y z]} (u/pos-of (.-pos e))]
    {:x (js/Math.floor x) :y (js/Math.floor (+ y 0.01)) :z (js/Math.floor z)}))

(defn pens
  "[{:gates [{:x :y :z}] :animals n}] of the pens round the passive animals in sight, one per pen."
  [c]
  (let [p (:primitives c)
        animals (->> (combat/sensed p {:radius (settings/get settings ::radius) :max 64})
                     (filter #(= "passive" (.-kind %)))
                     (take (settings/get settings ::max-animals)))]
    (loop [todo animals found []]
      (if-let [e (first todo)]
        (let [cell (animal-cell e)
              k [(:x cell) (:y cell) (:z cell)]]
          (if-let [i (first (keep-indexed (fn [i f] (when (contains? (:inside f) k) i)) found))]
            (recur (rest todo) (update-in found [i :animals] inc))
            (let [answer (pen/check {:block-at (apiary/seen-block-at-fn p) :at cell :max-cells (settings/get settings ::max-cells)})]
              (recur (rest todo)
                     (if (and (contains? (:inside answer) k) (seq (:gates answer)))
                       (conj found {:inside (:inside answer) :gates (mapv :pos (:gates answer)) :animals 1})
                       found)))))
        (mapv #(dissoc % :inside) found)))))
