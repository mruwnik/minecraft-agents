(ns jobs.survival.respond-to-hostile
  (:require [engine.ctx :as ctx]
            [engine.jobs.combat :as combat]
            [engine.jobs.reach :as reach]
            [engine.jobs.util :as u]))

(def doc
  "A hostile is near: fight it (jobs.survival.fight-back, the best weapon
  carried equipped) when the odds are fair, else retreat (jobs.survival.retreat).
  Decided afresh every round: never against a creeper; otherwise fight when the
  damage the fight is expected to cost (engine.jobs.combat/fight-damage: the
  weapon, the armour worn, each mob's kind and what is left of it after the
  hits landed, the dangers killed nearest first) leaves at least :reserve
  health. Writes one :hostile entry per encounter. Done the first round no real
  danger (as the hostile-near trigger: engine.jobs.reach, in sight) is within
  :radius (:ranged-radius for ranged mobs), unless the retreat is hiding (sealed
  in, up a pillar or down a pit): then the retreat says when the danger is gone.
  A danger reflex: never backed off.")

(def backoff
  "Off: a danger reflex reacts every round the danger is there; fruitless rounds (a blocked walk, a cornered body
  trying its escapes) must not mute it."
  false)

(def args
  {:radius {:doc "hostiles within this many blocks count" :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks count" :default 16}
   :reserve {:doc "health a fight must be expected to leave" :default 4}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}})

(def hostile-policy {:cap 50 :ttl (* 60 60 1000)})

(defn near
  "The hostiles that count: visible ones, melee within :radius and ranged
  within :ranged-radius. One behind a wall cannot reach or shoot the body, so
  it is left alone, as is one with no walkable way to the body (engine.jobs.reach),
  as the hostile-near trigger does."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (reach/dangers (:primitives c) radius {:ranged-radius ranged-radius} {})))


(defn decide
  "Pure: :flee from any creeper, else :fight when the expected damage leaves at least reserve health, else :flee."
  [{:keys [health damage creeper? reserve]}]
  (if (and (not creeper?) (<= damage (- health reserve)))
    :fight
    :flee))

(def other {:fight :flee :flee :fight})

(def child-jobs {:fight 'jobs.survival.fight-back :flee 'jobs.survival.retreat})

(defn ^:async run-child [c decision {:keys [radius ranged-radius weapons reserve]}]
  (let [child-args (case decision
                     :fight {:range radius :ranged-range ranged-radius :min-health 0 :weapons weapons}
                     :flee {:radius radius :ranged-radius ranged-radius :weapons weapons :reserve reserve})]
    (await (ctx/call-child c decision (child-jobs decision) child-args))))

(defn log-encounter! [c threat decision]
  (when-not (:logged (ctx/mem c))
    (ctx/remember! c :hostile {:mob (.-name threat) :pos (u/pos-of (.-pos threat)) :decision decision}
                   hostile-policy)
    (ctx/update-mem! c assoc :logged true)))

(defn mob-of
  "What fight-damage needs of hostile e: its name, distance and the hits fight-back has landed on it."
  [c e]
  (let [struck (get-in (ctx/mem c) [:children :fight :struck (.-id e)])]
    (cond-> {:name (.-name e) :distance (.-distance e) :hits (:hits struck 0)}
      (number? (:health struck)) (assoc :health (:health struck)))))

(defn ^:async respond [c near]
  (let [{:keys [reserve weapons] :as a} (:args c)
        p (:primitives c)
        self (.self p)
        decision (decide {:health (.-health self)
                          :creeper? (boolean (some combat/creeper? near))
                          :reserve reserve
                          :damage (combat/fight-damage {:weapon (combat/best-weapon p weapons)
                                                        :armour (combat/armour-points (.-equipment self))
                                                        :mobs (map #(mob-of c %) near)})})]
    (log-encounter! c (first near) decision)
    (ctx/update-mem! c assoc :decision decision)
    (let [result (await (run-child c decision a))]
      (if (= :declined result)
        (await (run-child c (other decision) a))
        result))))

(defn hiding?
  "Whether the retreat child holds a refuge (sealed in, a pillar or a pit): a hostile it hides from is out of sight
  and has no way to the body, yet is still there."
  [c]
  (some? (get-in (ctx/mem c) [:children :flee :refuge])))

(defn check
  "A real danger near, or the retreat hiding from one (it says when that is over)."
  [c]
  (or (hiding? c) (boolean (seq (near c)))))

(defn ^:async round [c]
  (let [hs (near c)]
    (cond
      (hiding? c) (await (run-child c :flee (:args c)))
      (empty? hs) :done
      :else (await (respond c hs)))))
