(ns jobs.lib.hostile-loss
  "What doing nothing about the dangers near costs the body (cost/stand-loss over an encounter): the hostile-near
  trigger holds only while it is positive, and respond-to-hostile is worth it."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.shot :as shot]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::encounter-s {:default 20 :doc "Seconds an encounter with a hostile is guessed to take: the stay its loss is priced over, and respond's quote." :spec (a/num-in 0 nil)}})

(defn encounter-s [] (settings/get settings ::encounter-s))

(defn mob-near
  "Hostile e as the answers take a mob: {:name :held :distance :speed (its chase speed) :follow (its follow range)}; a
  seen mob's distance from a body afloat at the surface is from the surface cell's floor (breath/start-y), not the bob."
  [p e]
  {:name (.-name e) :held (.-held e)
   :distance (danger-q/mob-distance p e (when (and (breath/swimming? p) (not (breath/head-under? p)) (danger-q/seen-only? e))
                                        (assoc (u/pos-of (.-pos (.self p))) :y (breath/start-y p))))
   :speed (cost/chase-speed (cost/mob-kind e)) :follow (threat/follow-range (.-name e))})

(defn body-at
  "What the answers' prices read of the body of p (memory view): health, absorption, food and what a death costs it (cost/death-body)."
  [p view]
  (let [self (.self p)]
    (assoc (cost/death-body p view)
           :health (.-health self) :absorption (.-absorption self) :food (or (.-food self) 20))))

(defn loss
  "Seconds doing nothing about hostiles hs (JS entities) costs the body of p over encounter-s: cost/stand-loss of them and,
  while a shot from a shooter not seen has a line of fire (shot/open?), a skeleton where the shot came from."
  [p view hs]
  (let [mobs (cond-> (mapv #(mob-near p %) hs)
               (shot/open? p view) (conj {:name "skeleton" :distance (settings/get shot/settings ::shot/reach)}))]
    (if (empty? mobs)
      0
      (cost/stand-loss (body-at p view) (cost/equipment-of (.-equipment (.self p))) mobs (u/pos-of (.-pos (.self p))) (encounter-s)))))

(defn hostile-near?
  "Whether the hostile-near trigger holds (args :radius, :ranged-radius, :visible-only): the loss of doing nothing about
  its dangers (danger/hostile-dangers) is positive. A dead body sees no danger."
  [world memory args]
  (boolean (and (not (body/dead? memory))
                (pos? (loss world memory (danger-q/hostile-dangers world memory args))))))
