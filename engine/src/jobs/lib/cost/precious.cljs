(ns jobs.lib.cost.precious
  "What precious food (golden apples) gives a body, apart from any danger: the one sum jobs.lib.cost.food/precious-worth
  adds a death averted to and the item table prices an unsourced one by. Pure."
  (:require [jobs.lib.cost.health :as health]
            [jobs.lib.foods :as foods]))

(defn fed
  "body once it has eaten precious food item: its regeneration on its health (at most 20), its absorption on top."
  [{:keys [health absorption] :as body} item]
  (let [{:keys [regen absorb]} (foods/healing item)]
    (assoc body :health (min 20 (+ (or health 20) regen)) :absorption (+ (or absorption 0) absorb))))

(def table-heals?
  "Whether the item table, and the eat decision against it (jobs.lib.meal), count the body as healing by waiting: no, so
  an irreplaceable food and the hp it gives are priced the same way."
  false)

(defn hp-worth
  "Seconds the hp precious food item adds to body are worth: its regeneration up to 20, each at most what waiting costs
  an hp while the body heals by waiting, heals?, then its absorption over that."
  [body item heals?]
  (let [h (or (:health body) 20)
        eaten (fed body item)]
    (+ (health/gain-worth h (js/Math.floor (- (:health eaten) h)) heals?)
       (health/gain-worth (:health eaten) (- (:absorption eaten) (or (:absorption body) 0)) false))))
