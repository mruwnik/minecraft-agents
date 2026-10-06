(ns jobs.survival.respond-to-hostile
  (:require [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.result :as r]
            [jobs.lib.util :as u]))

(def doc
  "A hostile is near: fight it (jobs.survival.fight-back, best weapon equipped) when the odds are fair,
  else retreat (jobs.survival.retreat).
  One whole attempt per round: decides afresh before each call of the child, while a danger is near.
  Never fights a creeper. Otherwise fights when the damage the fight is expected to cost
  leaves at least :reserve health (jobs.lib.cost/fight-damage: weapon, armour worn, each mob's kind
  and what is left of it after the hits landed, the dangers killed nearest first).
  If the chosen child declines, the other one runs.
  Done once no real danger (as the hostile-near trigger, jobs.lib.reach, in sight) is within :radius
  (:ranged-radius for ranged mobs) and the retreat is not hiding (sealed in, up a pillar or down a pit).
  A child that stops (a retreat still chased after its bound) stops it with that cause; a danger still near after
  :max-attempt-s stops it :still-near; never :continue.
  Memory: writes one :hostile entry {:mob :pos :decision} per encounter.
  A danger reflex: never backed off.")

(def args
  {:radius {:doc "hostiles within this many blocks count" :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks count" :default 16}
   :reserve {:doc "health a fight must be expected to leave" :default 4}
   :max-attempt-s {:doc "an attempt with a danger still near after this many seconds stops :still-near" :default 300}
   :quiet-s {:doc "passed to the retreat: a hidden body keeps its refuge this many seconds after the last danger" :default 30}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}})

(def hostile-policy {:cap 50 :ttl (* 60 60 1000)})

(defn near
  "The hostiles that count: ones the body has seen (jobs.lib.reach/known-hostiles), melee within :radius and ranged
  within :ranged-radius. One behind a wall cannot reach or shoot the body, so
  it is left alone, as is one with no walkable way to the body (jobs.lib.reach),
  as the hostile-near trigger does."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (reach/dangers (:primitives c) radius {:ranged-radius ranged-radius} {})))


(def other {:fight :flee :flee :fight})

(def child-jobs {:fight 'jobs.survival.fight-back :flee 'jobs.survival.retreat})

(defn ^:async run-child [c decision {:keys [radius ranged-radius weapons reserve quiet-s]}]
  (let [child-args (case decision
                     :fight {:range radius :ranged-range ranged-radius :min-health 0 :weapons weapons}
                     :flee {:radius radius :ranged-radius ranged-radius :weapons weapons :reserve reserve :quiet-s quiet-s})]
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
        decision (cost/decide {:health (.-health self)
                               :creeper? (boolean (some combat/creeper? near))
                               :reserve reserve
                               :damage (cost/fight-damage {:weapon (combat/best-weapon p weapons)
                                                           :equipment (cost/equipment-of (.-equipment self))
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

(defn stopped-child
  "[slot result] of the child that stopped in this round's last call, or nil."
  [c]
  (some #(let [res (ctx/child-result c %)] (when (= :stopped (:status res)) [% res])) [:flee :fight]))

(defn ^:async round
  "One whole attempt: respond (or let the retreat hide on) while a danger is near; stopped when a child stops or after
  :max-attempt-s with a danger still near."
  [c]
  (let [t0 (ctx/now c)]
    (loop []
      (let [hs (near c)]
        (cond
          (hiding? c) (await (run-child c :flee (:args c)))
          (empty? hs) nil
          :else (await (respond c hs)))
        (if-let [[slot res] (stopped-child c)]
          (r/stop! c (:reason res) (:text res) :cause (r/cause-of slot res))
          (cond
            (not (or (seq hs) (hiding? c))) :done
            (> (- (ctx/now c) t0) (* 1000 (:max-attempt-s (:args c))))
            (r/stop! c :still-near (str "a hostile is still near after " (:max-attempt-s (:args c)) " s"))
            :else (do (await (pace/pace!)) (recur))))))))
