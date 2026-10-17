(ns jobs.lib.body
  "What state the body is in, shared by the burning, died and hostile-near triggers and their jobs: on fire, dead,
  an unrecovered death. Memory entries :died, :recovered and :respawned are written by the engine and
  jobs.survival.recover-drops."
  (:require [engine.args :as a]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.cost.health :as cost-health]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::risky-min-health {:default 6 :spec (a/num-in 0 20)
                       :doc "Below this health (of 20) a risky job (mining) waits to heal before it starts or goes on."}
   ::recovery-holds {:default #{:hungry :door-left :inventory-nearly-full :scaffold-left :tidy-pending :pen-gate :loose-items}
                     :spec (a/set-of keyword?)
                     :doc "Trigger ids that do not hold while an unrecovered death is inside ::recovery-hold-s of its time, so a saved register listing one ahead of :died cannot run it on the pile's errand. The sheltering reflexes (:night :dusk-home :needs-bed) are left out on purpose; every covered id also waits while the body is dead."}
   ::recovery-hold-s {:default 180 :spec (a/num-in 0 nil)
                      :doc "Seconds after a death during which the ::recovery-holds reflexes stay back (at most the despawn window)."}})

(defn risky-min-health [] (settings/get settings ::risky-min-health))

(def fires #{"fire" "soul_fire"})

(def cures
  "Carried items that end or outweigh a poison, best first: milk clears every effect, a golden apple regenerates."
  ["milk_bucket" "golden_apple" "enchanted_golden_apple"])

(defn poisoned?
  "Whether the body (primitives p) has an effect that keeps costing hp (poison, wither)."
  [p]
  (boolean (some #(contains? cost-health/ticking-effects (.-name %)) (array-seq (.-effects (.self p))))))

(defn regenerating?
  "Whether the body (primitives p) has a regeneration effect running."
  [p]
  (boolean (some #(= "regeneration" (.-name %)) (array-seq (.-effects (.self p))))))

(defn cure-carried
  "The first of cures (or of those named in only) the inventory [{:name}] holds, else nil."
  [inventory only]
  (let [held (set (map :name inventory))]
    (first (filter #(and (contains? held %) (or (nil? only) (= only %))) cures))))

(defn fire-resistant? [p]
  (boolean (some #(= "fire_resistance" (.-name %)) (array-seq (.-effects (.self p))))))

(defn in-hazard?
  "True when the body is in lava or in a fire block at its feet or head, and not fire resistant: standing there hurts
  whatever the body does, so it has to move."
  [p]
  (let [self (.self p)
        pos (.-pos self)
        feet {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))}]
    (boolean (and (or (.-inLava self)
                      (contains? fires (u/feel-name p feet))
                      (contains? fires (u/feel-name p (update feet :y inc))))
                  (not (fire-resistant? p))))))

(defn burning?
  "True when the body (primitives p) is on fire, in lava or in a fire block at its feet or head (rain puts the on-fire
  flag out every tick, the fire block still hurts), and has no fire_resistance effect (which makes all three harmless)."
  [p]
  (boolean (or (in-hazard? p)
               (and (.-onFire (.self p)) (not (fire-resistant? p))))))

;; A flight from heard-only mobs that ended blocked mutes heard mobs for a growing time; a seen mob fires through it.
(def mute-policy {:cap 8 :ttl (* 60 60 1000)})

(def mute-base-ms 60000)

(def mute-max-ms (* 30 60 1000))

(defn heard-muted?
  "Whether heard-only mobs are muted now: the latest :heard-mute entry's :until is ahead of the view's now and the
  body was not hurt since it was written (an unseen mob may be the one hitting)."
  [view]
  (boolean (when-let [m (mem/latest view :heard-mute)]
             (and (> (:until (:data m)) (:now view))
                  (empty? (mem/since view :hurt (:t m)))))))

(defn next-mute-ms
  "How long the next mute lasts: mute-base-ms, doubled for each mute within the last hour, up to mute-max-ms."
  [view]
  (min mute-max-ms (* mute-base-ms (js/Math.pow 2 (count (mem/entries view :heard-mute))))))

(defn open-deaths
  "The :died entries (oldest first) no :recovered entry covers: one with the death's :death-t, or, from before
  :recovered carried it, any newer one."
  [view]
  (let [recovered (mem/entries view :recovered)
        covered? (fn [died] (some #(if-let [t (:death-t (:data %))] (= t (:t died)) (> (:t %) (:t died))) recovered))]
    (into [] (remove covered?) (mem/entries view :died))))

(defn danger-hold
  "The data of the :recover-danger entry ({:death-t :until :text}) while recover-drops' decline of death for a danger
  is remembered (view's :now before its :until), else nil."
  [view death]
  (some #(when (and (= (:t death) (:death-t (:data %))) (< (:now view) (:until (:data %)))) (:data %))
        (reverse (mem/entries view :recover-danger))))

(defn actionable-deaths
  "open-deaths without those whose danger decline is still remembered (danger-hold)."
  [view]
  (into [] (remove #(danger-hold view %)) (open-deaths view)))

(defn respawned-since?
  "True when a :respawned entry newer than death exists: the body is alive again."
  [view death]
  (let [respawned (mem/latest view :respawned)]
    (boolean (and respawned (> (:t respawned) (:t death))))))

(defn dead?
  "True between a :died entry and the :respawned entry that follows it."
  [view]
  (let [death (mem/latest view :died)]
    (boolean (and death (not (respawned-since? view death))))))

(defn recovery-pending?
  "True when the trigger id must not hold now for the pile: the body is dead (all covered reflexes), or it is alive
  with an unrecovered death younger than ::recovery-hold-s and id is in ::recovery-holds. The :died reflex has the
  body then, so a later reflex in a saved register that lists it ahead of :died must not start."
  [view id]
  (boolean (or (dead? view)
               (and (contains? (settings/get settings ::recovery-holds) id)
                    (let [ticks (min game/despawn-ticks (settings/ms->ticks (* 1000 (settings/get settings ::recovery-hold-s))))]
                      (some #(< (game/ticks-since % view) ticks) (open-deaths view)))))))

(defn too-hurt
  "The wait reason {:reason :low-health :health :min-health :text} of a risky job while the body's health (primitives
  p) is below risky-min-health, or {:reason :poisoned :health :text} while it is poisoned or withered, else nil.
  Healing is up to the body: carried food (the hungry trigger eats it), a cure (the poisoned trigger drinks it), or rest
  at food 18+."
  [p]
  (let [health (.-health (.self p))
        floor (risky-min-health)]
    (cond
      (< health floor)
      {:reason :low-health :health health :min-health floor
       :text (str "health " (.toFixed health 1) " of 20 is below " floor ": eat or rest to heal first (natural healing needs food 18+)")}

      (poisoned? p)
      {:reason :poisoned :health health
       :text "poisoned: drink milk or wait until the effect is gone before the risk (natural healing stops)"})))
