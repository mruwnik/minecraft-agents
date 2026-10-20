(ns jobs.survival.pit-drops
  "Picking up the placeable blocks a shelter dig dropped (dig-pocket, the retreat pit)."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::collect-radius {:default 4 :doc "How far round the body an items.collect-drops child looks for the pit's drops, in blocks." :spec (a/num-in 0 nil)}
   ::pickup-reach {:default 1 :doc "How far from the body, along the ground, an item is picked up without a walk." :spec (a/int-in 0 nil)}})

(defn ^:async take-ids!
  "An items.collect-drops child picks up the item entities ids."
  [c ids]
  (when (seq ids)
    (await (child/run!* c :collect 'jobs.items.collect-drops {:ids (vec ids) :radius (settings/get settings ::collect-radius)}))))

(defn pickup-reach [] (settings/get settings ::pickup-reach))

(defn in-reach-ids
  "The ids of the item entities among ids that lie within pickup-reach of the body (a step or less above or below)."
  [c ids]
  (let [p (:primitives c)
        at (u/self-pos c)
        wanted (set ids)]
    (->> (look/seen-items p {:radius 4 :max 64})
         (filter #(wanted (.-id %)))
         (filter (fn [e] (let [q (u/pos-of (.-pos e))]
                           (and (<= (js/Math.hypot (- (:x q) (:x at)) (- (:z q) (:z at))) (pickup-reach))
                                (<= (js/Math.abs (- (:y q) (:y at))) 1)))))
         (mapv #(.-id %)))))

(defn ^:async collect-pit-drops!
  "Pick up the placeable drops of the pit's digs (memory :pit-drops) that fell into the body's column at or over its
  feet and lie within its pickup reach: a drop that drifted further is left, so taking drops never walks the body off
  the floor it stands on."
  [c {:keys [x y z]}]
  (let [here? (fn [{cell :cell}] (and (= x (:x cell)) (= z (:z cell)) (>= (:y cell) y)))
        drops (filter here? (:pit-drops (ctx/mem c)))]
    (await (take-ids! c (in-reach-ids c (map :id drops))))
    (ctx/update-mem! c update :pit-drops #(vec (remove here? %)))))

(defn ^:async collect-drops!
  "Pick up the placeable blocks a dig dropped."
  [c blocks drops]
  (await (take-ids! c (keep #(when (some #{(.-name %)} blocks) (.-id %)) (array-seq drops)))))
