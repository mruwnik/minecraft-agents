(ns jobs.lib.body
  "What state the body is in, shared by the burning, died and hostile-near triggers and their jobs: on fire, dead,
  an unrecovered death. Memory entries :died, :recovered and :respawned are written by the engine and
  jobs.survival.recover-drops."
  (:require [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::risky-min-health {:default 6 :spec (a/num-in 0 20)
                       :doc "Below this health (of 20) a risky job (mining) waits to heal before it starts or goes on."}})

(defn risky-min-health [] (settings/get settings ::risky-min-health))

(def fires #{"fire" "soul_fire"})

(defn burning?
  "True when the body (primitives p) is on fire, in lava or in a fire block at its feet or head (rain puts the on-fire
  flag out every tick, the fire block still hurts), and has no fire_resistance effect (which makes all three harmless)."
  [p]
  (let [self (.self p)
        pos (.-pos self)
        feet {:x (js/Math.floor (.-x pos)) :y (js/Math.floor (.-y pos)) :z (js/Math.floor (.-z pos))}]
    (boolean (and (or (.-onFire self) (.-inLava self)
                      (contains? fires (u/feel-name p feet))
                      (contains? fires (u/feel-name p (update feet :y inc))))
                  (not-any? #(= "fire_resistance" (.-name %)) (array-seq (.-effects self)))))))

(defn open-deaths
  "The :died entries (oldest first) no :recovered entry covers: one with the death's :death-t, or, from before
  :recovered carried it, any newer one."
  [view]
  (let [recovered (mem/entries view :recovered)
        covered? (fn [died] (some #(if-let [t (:death-t (:data %))] (= t (:t died)) (> (:t %) (:t died))) recovered))]
    (into [] (remove covered?) (mem/entries view :died))))

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

(defn too-hurt
  "The wait reason {:reason :low-health :health :min-health :text} of a risky job while the body's health (primitives
  p) is below risky-min-health, else nil. Healing is up to the body: carried food (the hungry trigger eats it), or rest
  at food 18+."
  [p]
  (let [health (.-health (.self p))
        floor (risky-min-health)]
    (when (< health floor)
      {:reason :low-health :health health :min-health floor
       :text (str "health " (.toFixed health 1) " of 20 is below " floor ": eat or rest to heal first (natural healing needs food 18+)")})))
