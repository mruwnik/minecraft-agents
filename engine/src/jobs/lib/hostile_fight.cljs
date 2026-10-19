(ns jobs.lib.hostile-fight
  "The hostile mode of combat.attack (:targets :hostiles): which dangers near it fights, and when a held or leashed fight stops."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::hold-s {:default 15 :doc "Seconds a held fight waits without a swing for a mob to come within reach before it declines." :spec (a/num-in 0 nil)}
   ::ranged-chase {:default 6 :doc "Farthest a ranged mob may be for the body to cross open ground to it while another hostile is in range; beyond it the fight declines." :spec (a/num-in 0 nil)}
   ::chase-timeout-s {:default 10 :doc "Seconds one walk toward a hostile may take: it moves, so the walk aims again at where it is now this often." :spec (a/num-in 0 nil)}
   ::leash {:default 20 :doc "Blocks from where a hostile fight started beyond which a hostile is not chased, when the call gives no :leash." :spec (a/num-in 0 nil)}})

(defn hostiles? [c] (= :hostiles (:targets (:args c))))

(defn chase-timeout-s [] (settings/get settings ::chase-timeout-s))

(defn hold-ms [] (* 1000 (settings/get settings ::hold-s)))

(defn leash [c] (or (:leash (:args c)) (settings/get settings ::leash)))

(defn start-of
  "Where the fight started: the first step's position (the current one before it)."
  [c]
  (or (:start (ctx/mem c)) (u/self-pos c)))

(defn in-leash?
  "Whether hostile e stands within the leash of the start point."
  [c e]
  (<= (u/dist (start-of c) (u/pos-of (.-pos e))) (leash c)))

(defn in-range
  "The hostiles within :radius (ranged ones within :ranged-radius, a heard one by its band): the visible ones nearest
  first, then the hidden melee ones (a ranged mob without a line of fire, by jobs.lib.danger/danger? as the hostile-near
  trigger, is dropped). The killed and the :skip ids are left out."
  [c]
  (let [{:keys [radius ranged-radius skip]} (:args c)
        ranged-radius (or ranged-radius radius)
        dead (into (set skip) (:killed (ctx/mem c)))
        all (->> (danger-q/known-hostiles (:primitives c) radius {:ranged-radius (max radius ranged-radius)})
                 (remove #(contains? dead (.-id %)))
                 (remove #(and (combat/ranged? %) (not (danger-q/danger? (:primitives c) %)))))]
    (into (filterv #(.-visible %) all) (remove #(.-visible %)) all)))

(defn targets
  "The hostiles in range the body has seen, within the leash and not given up on, nearest first."
  [c]
  (let [given-up (:given-up (ctx/mem c) {})]
    (->> (in-range c) (filter danger-q/seen-only?) (filter #(in-leash? c %)) (remove #(contains? given-up (.-id %))))))

(defn heard
  "The first hostile in range the body only hears, or nil."
  [c]
  (first (remove danger-q/seen-only? (in-range c))))

(defn too-far-to-chase?
  "Whether target is a ranged mob beyond ::ranged-chase while another hostile is in range."
  [c target]
  (and (combat/ranged? target)
       (> (danger-q/mob-distance (:primitives c) target) (settings/get settings ::ranged-chase))
       (some #(not= (.-id %) (.-id target)) (in-range c))))

(defn held-idle?
  "Whether a held fight has waited ::hold-s since its last swing (or its start) with no mob coming within reach."
  [c]
  (let [m (ctx/mem c)]
    (and (:hold (:args c))
         (> (- (ctx/now c) (or (:last-attack m) (:started-at m))) (hold-ms)))))

(defn struck
  "The hits that landed, as {id {:name :hits :health}}, for a parent that weighs how near each mob is to dying."
  [m]
  (into {} (for [[id n] (:hits m)]
             [id (cond-> {:name (get-in m [:names id]) :hits n}
                   (number? (get-in m [:health id])) (assoc :health (get-in m [:health id])))])))
