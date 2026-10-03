(ns jobs.survival.respond-to-hostile
  (:require [engine.ctx :as ctx]
            [engine.jobs.combat :as combat]
            [engine.jobs.util :as u]))

(def doc
  "A hostile is near: fight it (jobs.survival.fight-back) when healthy, armed,
  and no creeper is near and at most :max-fight hostiles are; otherwise
  retreat (jobs.survival.retreat). A fight once chosen is kept until health
  drops below :min-health, and below that too while the target is nearly
  dead (one more hit of the weapon carried likely kills it, from the hits
  fight-back has landed). Writes one :hostile entry per encounter.")

(def args
  {:radius {:doc "hostiles within this many blocks count" :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks count" :default 16}
   :fight-health {:doc "least health to start a fight" :default 12}
   :min-health {:doc "a fight already chosen is kept down to this health" :default 8}
   :max-fight {:doc "most hostiles to fight at once" :default 2}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}})

(def hostile-policy {:cap 50 :ttl (* 60 60 1000)})

(defn near
  "The hostiles that count: visible ones, melee within :radius and ranged
  within :ranged-radius. One behind a wall cannot reach or shoot the body, so
  it is left alone, as the hostile-near trigger does."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (combat/hostiles (:primitives c) radius {:ranged-radius ranged-radius :sight :only})))

(defn check [c]
  (boolean (seq (near c))))

(defn decide
  "Pure: :fight or :flee from health, whether a weapon is carried, whether a
  creeper is near, the number of hostiles and the health threshold in force."
  [{:keys [health armed? creeper? hostile-count max-fight threshold]}]
  (if (and (>= health threshold) armed? (not creeper?) (<= hostile-count max-fight))
    :fight
    :flee))

(def other {:fight :flee :flee :fight})

(def child-jobs {:fight 'jobs.survival.fight-back :flee 'jobs.survival.retreat})

(defn ^:async run-child [c decision {:keys [radius ranged-radius min-health weapons]}]
  (let [child-args (case decision
                     :fight {:range radius :ranged-range ranged-radius :min-health min-health :weapons weapons}
                     :flee {:radius radius :ranged-radius ranged-radius})]
    (await (ctx/call-child c decision (child-jobs decision) child-args))))

(defn log-encounter! [c threat decision]
  (when-not (:logged (ctx/mem c))
    (ctx/remember! c :hostile {:mob (.-name threat) :pos (u/pos-of (.-pos threat)) :decision decision}
                   hostile-policy)
    (ctx/update-mem! c assoc :logged true)))

(defn finishing?
  "Whether a fight is under way and its target (the nearest hostile) is
  nearly dead by the hits fight-back has landed."
  [c near weapon]
  (let [target (first near)
        struck (when target (get-in (ctx/mem c) [:children :fight :struck (.-id target)]))]
    (boolean (and struck weapon (= :fight (:decision (ctx/mem c)))
                  (combat/nearly-dead? (assoc struck :damage (combat/weapon-damage weapon)))))))

(defn ^:async respond [c near]
  (let [{:keys [fight-health min-health max-fight weapons] :as a} (:args c)
        p (:primitives c)
        previous (:decision (ctx/mem c))
        weapon (combat/best-weapon p weapons)
        finish? (finishing? c near weapon)
        decision (cond
                   (empty? near) :flee ; retreat tracks its own cooldown after the hostile is out of sight
                   (and finish? (not (some combat/creeper? near))) :fight
                   :else (decide {:health (.-health (.self p))
                                  :armed? (some? weapon)
                                  :creeper? (boolean (some combat/creeper? near))
                                  :hostile-count (count near)
                                  :max-fight max-fight
                                  :threshold (if (= :fight previous) min-health fight-health)}))
        a (cond-> a finish? (assoc :min-health 0))]
    (when (seq near) (log-encounter! c (first near) decision))
    (ctx/update-mem! c assoc :decision decision)
    (let [result (await (run-child c decision a))]
      (if (= :declined result)
        (await (run-child c (other decision) a))
        result))))

(defn ^:async round [c]
  (let [hs (near c)]
    (if (and (empty? hs) (= :fight (:decision (ctx/mem c))))
      :done
      (await (respond c hs)))))
