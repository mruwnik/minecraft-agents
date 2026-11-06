(ns jobs.lib.cost.mob-toll
  "What a second within a known mob's reach costs go-to's planner (jobs.lib.threats/threat-tolls): danger-rate, the mob's
  hurt over 1 s x its stance's factor (danger/danger-opts), at most max-rate; threat-factor prices it at the hp price of the
  body's health. The stance is the answer the reflex would take to the mob met beside the body (answer/decide), so the
  planner and the reflex price a mob by one price."
  (:require [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.health :as health]))

(defn stance
  "How the planner prices mob m (a name, or {:name :held}) alone for body {:health :food :equipment :weapon :pos} and its
  death (death/death-cost's :level :carried :respawn): :fight
  when a fight is the cheapest answer to it met beside the body (answer/decide over answer/threat-answers), else :flee."
  [body m]
  (if (= :fight (:answer (answer/decide body (answer/threat-answers body [m] nil) (:pos body)))) :fight :flee))

(defn danger-rate
  "hp a second a near mob m costs body (see the ns doc). opts: danger/danger-opts and :stance (default m's stance)."
  ([body m] (danger-rate body m nil))
  ([{:keys [equipment] :as body} m opts]
   (min (get opts :max-rate danger/max-rate)
        (* (danger/hurt-rate equipment m)
           (get (merge danger/stances (:stances opts)) (or (:stance opts) (stance body m)))))))

(defn threat-factor
  "Seconds more each second within a known mob's reach costs body: m's danger-rate at stance st (default its stance) x
  weight (0-1: a fading memory, the cell's place in its field) at the hp price of the body's health (the planner's
  options.tolls factor)."
  ([body m weight] (threat-factor body m weight (stance body m)))
  ([{:keys [health] :as body} m weight st]
   (* weight (danger-rate body m (assoc (danger/danger-opts) :stance st)) (health/hp-price (if (number? health) health 20)))))
