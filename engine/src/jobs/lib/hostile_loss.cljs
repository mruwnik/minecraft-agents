(ns jobs.lib.hostile-loss
  "What doing nothing about the dangers near costs the body (cost/stand-loss over a respond call): the hostile-near
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

(def call-deciles? (a/pred "10 seconds" #(and (vector? %) (= 10 (count %)) (every? (fn [x] (and (a/finite? x) (>= x 0))) %))))

(a/defargs settings
  {::call-s {:default {:n 1413 :deciles [0.47 0.82 1.94 3.06 4.47 6.15 9.36 15.11 15.62 44.75]}
             :doc "Seconds a respond-to-hostile call lasts, its deciles at (i + 0.5) / 10: the stays a call's loss is priced over (loss), the :setting of node tools/event-counts.mjs respond-calls (each hostile-near reflex, fired to ended, of the trial bodies)."
             :spec (a/pred "{:n :deciles}" #(and (map? %) (call-deciles? (:deciles %))))}})

(defn call-s
  "The ten equally likely seconds a respond call lasts (::call-s)."
  []
  (:deciles (settings/get settings ::call-s)))

(defn mean-call-s [] (let [s (call-s)] (/ (reduce + s) (count s))))

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

(defn mobs-of
  "Hostiles hs (JS entities) of p as the answers take them (mob-near) and, while a shot from a shooter not seen has a line
  of fire (shot/open?) and no shooter of hs is seen, a skeleton where the shot came from."
  [p view hs]
  (cond-> (mapv #(mob-near p %) hs)
    (and (not-any? #(and (danger-q/seen-only? %) (threat/ranged? {:name (.-name %) :held (.-held %)})) hs) (shot/open? p view))
    (conj {:name "skeleton" :distance (settings/get shot/settings ::shot/reach)})))

(defn loss-over
  "Seconds doing nothing about hostiles hs (JS entities) of p for seconds (a number, or a spread as cost/stand-loss
  takes) costs the body: cost/stand-loss of mobs-of them."
  [p view hs seconds]
  (let [mobs (mobs-of p view hs)]
    (if (empty? mobs)
      0
      (cost/stand-loss (body-at p view) (cost/equipment-of (.-equipment (.self p))) mobs (u/pos-of (.-pos (.self p))) seconds))))

(defn loss
  "Seconds doing nothing about hostiles hs costs the body over a respond call (loss-over the spread of call-s)."
  [p view hs]
  (loss-over p view hs (call-s)))

(defn hostile-near?
  "Whether the hostile-near trigger holds (args :radius, :ranged-radius, :visible-only): the loss of doing nothing about
  its dangers (danger/hostile-dangers) is positive. A dead body sees no danger."
  [world memory args]
  (boolean (and (not (body/dead? memory))
                (pos? (loss world memory (danger-q/hostile-dangers world memory args))))))
