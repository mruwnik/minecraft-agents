(ns jobs.lib.cost.mob-toll
  "What a second within a known mob's reach costs go-to's planner (jobs.lib.threats/threat-tolls): the chance a second the
  mob gets to the body there (its notice rate, death/notice-prior's mean, by the cell's place in its field) times the price
  of the answer the reflex would take to the mobs met with it (answer/decide), as answer/water-factors prices a drowned."
  (:require [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.threat :as threat]))

(defn answer-mob
  "Threat t {:mob :held :chaser} as the answers take a mob: {:name :held :speed} (its chase speed)."
  [{:keys [mob held chaser]}]
  {:name mob :held held :speed (danger/chase-speed (or chaser mob))})

(defn answered-together
  "{:stance :price :near} of mobs ms (names, or answer-mob's) met together by body {:health :absorption :food :equipment
  :weapon} and its death (death/death-cost's :level :carried :respawn) at `at`, as respond weighs them (hostile-fight/weigh: a
  death meets them again from a near respawn): answer/decide over answer/threat-answers; :stance :fight when that is a
  fight, else :flee; :price its seconds, :death the part of them its death is (answer/answer-death-chance x the death's
  price); :near the seconds it holds the body at the place (a flight's way out, else its seconds). nil when no answer is open."
  [body ms at]
  (let [body (assoc body :meet (mapv #(if (map? %) (:name %) %) ms))
        answers (answer/threat-answers body ms nil)]
    (when-let [{:keys [answer prices death]} (answer/decide body answers at)]
      (let [a (get answers answer)
            p (answer/answer-death-chance body a at death)]
        {:stance (if (= :fight answer) :fight :flee) :price (get prices answer) :death (if (pos? p) (* p death) 0)
         :near (or (:near a) (:seconds a) 0)}))))

(defn joins?
  "Whether mob u (answer-mob's) at u-pos comes within its blow (answer/hit-range) of `at` within seconds (danger/leg-s at
  its :speed): met in an answer that holds the body there that long."
  [u u-pos at seconds]
  (<= (danger/leg-s (:name u) (danger/way-blocks u-pos at nil (answer/hit-range u)) 0 (:speed u)) seconds))

(defn meet-rate
  "Meetings a second mob {:name :held} makes with a body in the full weight of its field: the mean of its notice prior
  (death/notice-prior), the rate at which met-stretch's chance of a meeting starts."
  [m]
  (let [{:keys [shape rate]} (death/notice-prior (threat/kind m))]
    (/ shape rate)))

(defn threat-parts
  "[rest death]: seconds more each second within mob m's reach costs at weight (0-1: a fading memory, the cell's place in
  its field), answered a (answered-together's; nil: no answer open): weight x meet-rate x the answer's price, the first
  term of met-stretch's chance of a meeting times what the meeting costs, its :death apart. The rest is at most the
  ::max-rate hp at hp-seconds (a mob hurts no faster); the death is not capped, so low health
  weighs through its death chance (Q142)."
  [m weight {:keys [price death]}]
  (let [cap (* (:max-rate (danger/danger-opts)) (health/hp-seconds))
        rate (meet-rate m)
        d (min (or death 0) (or price 0))]
    (if price
      [(* weight (min cap (* rate (- price d)))) (* weight rate d)]
      [(* weight cap) 0])))

(defn threat-factor "threat-parts summed." [m weight a] (reduce + (threat-parts m weight a)))
