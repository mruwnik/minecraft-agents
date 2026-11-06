(ns jobs.lib.cost.mob-toll
  "What a second within a known mob's reach costs go-to's planner (jobs.lib.threats/threat-tolls), by the answer the reflex
  would take to the mob met beside the body (answer/decide), so the planner and the reflex price a mob by one price:
  a mob the body would flee costs its hurt over 1 s x the :flee factor (danger/danger-opts); one it would fight costs the
  fight's blows at their hp cost (its :damage, at most the body's health) over the exposure seconds (danger/exposure-s). Both at
  most max-rate hp at the hp price of the body's health (threat-factor)."
  (:require [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.health :as health]))

(defn answered-together
  "{:stance :rate} for mobs ms (names, or {:name :held}) met together by body {:health :food :equipment :weapon :pos} and its
  death (death/death-cost's :level :carried :respawn): :stance :fight when a fight is the cheapest answer to them met beside
  the body (answer/decide over answer/threat-answers), else :flee; :rate each mob's share of the seconds the fight's blows
  cost (hp-cost, at most the body's health) over the ::exposure-s a body is exposed to a mob; 0 for a flight."
  [body ms]
  (let [answers (answer/threat-answers body ms nil)
        {:keys [answer]} (answer/decide body answers (:pos body))
        fight (:fight answers)]
    (if (= :fight answer)
      {:stance :fight
       :rate (let [h (if (number? (:health body)) (:health body) 20)]
               (/ (health/hp-cost h (min h (:damage fight))) (max 1 (danger/exposure-s)) (count ms)))}
      {:stance :flee :rate 0})))

(defn answered
  "answered-together for mob m alone."
  [body m]
  (answered-together body [m]))

(defn stance
  "How the planner prices mob m for body: :fight or :flee (answered)."
  [body m]
  (:stance (answered body m)))

(defn threat-factor
  "Seconds more each second within a known mob's reach costs body: weight (0-1: a fading memory, the cell's place in its
  field) x, for the answer a ({:stance :rate}, default answered), a flight's hurt over 1 s x the :flee factor at the hp
  price of the body's health, or a fight's rate; at most max-rate hp at that price (the planner's options.tolls factor)."
  ([body m weight] (threat-factor body m weight (answered body m)))
  ([{:keys [health equipment]} m weight {:keys [stance rate]}]
   (let [{:keys [stances max-rate]} (danger/danger-opts)
         hp-s (health/hp-price (if (number? health) health 20))]
     (* weight (min (* max-rate hp-s)
                    (if (= :fight stance) rate (* (danger/hurt-rate equipment m) (:flee stances) hp-s)))))))
